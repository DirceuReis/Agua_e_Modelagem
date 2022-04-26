
get_stats <- function(dados){
  
  lm <- samlmu(dados$p.max) 
  
  out <- dados %>%
    summarise(n       = n(),
              med     = mean(p.max, na.rm = TRUE),
              dp      = sd(p.max, TRUE),
              skew    = e1071::skewness(p.max, na.rm = TRUE),
              ano_rec = hidro_ano[which.max(p.max)],
              p_rec   = max(p.max, na.rm = TRUE)) %>%
    mutate(l1 = lm[1],
           l2 = lm[2],
           t3 = lm[3],
           t4 = lm[4])
  
  return(out)
}

fit_gev <- function(dados){

  lmom <- samlmu(dados$p.max) 
  par  <- pelgev(lmom)
  
  out <- data.frame(par_posicao = par[1],
                    par_escala  = par[2],
                    par_forma   = par[3],
                    p.10anos    = quagev(1-1/10, par),
                    p.50anos    = quagev(1-1/50, par),
                    p.100anos   = quagev(1-1/100, par))
  return(out)
}

get_max <- function(sta){
  
  sta <- sta %>%
    mutate(codigo       = as.integer(EstacaoCodigo),
           consistencia = as.integer(NivelConsistencia),
           dt           = as_date(DataHora),
           across(D1:D31, ~ as.numeric(.))) %>%
    select(codigo, consistencia, dt, D1:D31)
  
  sta <- sta %>% 
    pivot_longer(cols = D1:D31,
                 names_to  = 'dia',
                 values_to = 'p')
  
  sta <- sta %>%
    filter(!is.na(p)) %>%
    mutate(dt = dmy(paste0(str_remove(dia,'D'),'/', month(dt),'/',year(dt)))) %>%
    select(-dia) %>%
    filter(!is.na(dt)) #datas impossiveis viram NA
  
  sta <- sta %>% 
    pivot_wider(names_from = consistencia,
                names_prefix = "p_",
                values_from = p)
  
  #caso nao tenha nenhum consistido
  if(!any(names(sta)=='p_2')){
    sta <- sta %>%
      mutate(p_2 = NA_real_,)
  }
  #caso nao tenha nenhum bruto
  if(!any(names(sta)=='p_1')){
    sta <- sta %>%
      mutate(p_1 = NA_real_,)
  }
  
  sta <- sta %>%
    mutate(p  = if_else(is.na(p_2), p_1, p_2)) %>%
    select(codigo, dt, p)
  
  #trata bordas do ano hidrologico inicial e final
  dt.min <- dmy(paste0('01/10/',year(min(sta$dt))))
  dt.max <- dmy(paste0('30/04/',year(max(sta$dt))))
  
  sta <- sta %>% 
    filter(dt >= dt.min & dt <= dt.max) %>% #elimina as pontas que não serão utlizadas
    complete(dt = seq.Date(dt.min, dt.max, by = "day")) %>% #completa as datas faltantes dos dados da ANA
    fill(codigo, .direction = "downup")
  
  sta <- sta %>% 
    filter(month(dt) %in% c(10,11,12,1,2,3,4)) %>% #elimina dados desnecessarios
    mutate(hidro_ano = if_else(month(dt) %in% c(10,11,12), year(dt)+1,year(dt))) %>% #cria a variavel ano hidrológico
    mutate(hidro_ano = as.integer(hidro_ano)) %>%
    arrange(dt)
  
  
  #criterios para eliminar o posto: 
  
  # 1 - menos de 15 valores unicos nos registros 
  if(length(unique(sta$p)) < 15){
    # 'eliminar o posto'
    return(NULL)
  }
  
  # 2 - % de dias secos (<0.5) >= 99.5
  if(sum(sta$p < 0.5, na.rm = TRUE)/nrow(sta) >= 0.995){
    # return('eliminar o posto')
    return(NULL)
  }
  
  
  #criterios para eliminar ano
  
  #1 10% de falhas
  criterio.1 <- sta %>%
    group_by(hidro_ano) %>%
    summarise(falhas = sum(is.na(p)),
              n = n()) %>%
    mutate(falhas = falhas/n,
           cr.1 = if_else(falhas > 0.1, 1, 0)) %>%
    select(hidro_ano, cr.1)
  
  #2 o maximo é repetido (pluviometro esgotado)
  criterio.2 <- sta %>%
    group_by(hidro_ano) %>%
    slice_max(p, n = 2) %>%
    summarise(unique.max = length(unique(p))) %>%
    mutate(cr.2 = if_else(unique.max == 2, 0, 1)) %>%
    select(hidro_ano, cr.2)
  
  #3 falha no entorno do maximo (1 semana)
  criterio.3 <- sta %>%
    group_by(hidro_ano) %>%
    filter(p == max(p, na.rm = TRUE)) %>%
    select(hidro_ano, dt) %>%
    mutate(dt.min = dt - days(3),
           dt.max = dt + days(3))
  
  criterio.3 <- criterio.3 %>%
    pivot_longer(cols = c(dt.min,dt.max), 
                 names_to = 'dt_tag', 
                 values_to = 'dt_value') %>%
    select(hidro_ano, dt_value) %>%
    group_by(hidro_ano) %>%
    complete(dt_value = seq.Date(min(dt_value), max(dt_value), by = "day")) %>%
    fill(hidro_ano, .direction = "downup") %>%
    left_join(sta, by = c('dt_value' = 'dt', 'hidro_ano' = 'hidro_ano')) %>%
    summarise(falhas = sum(is.na(p))) %>%
    mutate(cr.3 = if_else(falhas == 0, 0, 1)) %>%
    select(hidro_ano, cr.3)
  
  #4 max é menor que o recorde brasileiro  registrado
  # Saldanha et al (2012) - https://www.abrhidro.org.br/SGCv3/publicacao.php?PUB=1&ID=62&SUMARIO=1508&ST=o_evento_de_chuva_intensa_de_janeiro_de_2009_sobre_a_regiao_de_pelotas_rs 
  recorde <- 479 
  
  criterio.4 <- sta %>%
    group_by(hidro_ano) %>%
    summarise(p.max  = max(p, na.rm = TRUE)) %>%
    mutate(cr.4 = if_else(p.max < recorde, 0, 1)) %>%
    select(hidro_ano, cr.4)
  
  
  #agrega todos os critérios
  
  criterios.anuais <- data.frame(hidro_ano = seq(min(sta$hidro_ano), max(sta$hidro_ano), by = 1))
  
  criterios.anuais <- purrr::reduce(list(criterios.anuais, criterio.1, criterio.2, criterio.3, criterio.4), dplyr::left_join, by = 'hidro_ano')
  
  criterios.anuais <- criterios.anuais %>%
    mutate_at(vars(starts_with('cr.')), ~replace(., is.na(.), 0)) %>%
    mutate(elimina_ano = cr.1+cr.2+cr.3+cr.4) %>%
    select(hidro_ano, elimina_ano)
  
  #seleciona a série final
  filtro <- criterios.anuais %>% filter(elimina_ano == 0) %>% select(hidro_ano) %>% unlist() %>% as.vector()
  
  annual.max <- sta %>%
    group_by(hidro_ano) %>%
    summarise(p.max  = max(p, na.rm = TRUE)) %>%
    filter(hidro_ano %in% filtro) %>%
    mutate(codigo = unique(sta$codigo)) %>%
    select(codigo, hidro_ano, p.max)
  
  return(annual.max)
  
}

