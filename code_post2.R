library(dplyr)      # versão 1.0.8
library(tidyr)      # versão 1.2.0
library(stringr)    # versão 1.4.0
library(XML)        # versão 3.99-0.9
library(lubridate)  # versão 1.8.0
library(purrr)      # versão 0.3.4
library(lmom)       # versão 2.8
library(e1071)      # versão 1.7-9
library(ggplot2)    # versão 3.3.5
library(hrbrthemes) # Versão 0.8.0
library(patchwork)  # versão 1.1.1
library(sf)         # versão 1.0-7

cods <- readRDS('C:/inventario_all_plu.rds')

n <- length(cods$EstadoCodigo)

for(i in 1:n){
  sta <- getStationData(station_code = cods[i], tipo_dados = '2')
  if(is.null(sta$erro)){  #o download foi feito corretamente e o posto contém dados
    saveRDS(sta$data, file = paste0('C:/dados_plu_ana/', cod[i], '.rds'))
  }
}

sta <- readRDS('C:/dados_plu_ana/1944010.rds')

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
  # return(NULL)
}

# 2 - % de dias secos (<0.5) >= 99.5
if(sum(sta$p < 0.5, na.rm = TRUE)/nrow(sta) >= 0.995){
  # return('eliminar o posto')
  # return(NULL)
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

#4 max é menor que o recorde brasileiro registrado
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
filtro <- criterios.anuais %>% 
  filter(elimina_ano == 0) %>% 
  select(hidro_ano) %>% 
  unlist() %>% 
  as.vector()
  
annual.max <- sta %>%
  group_by(hidro_ano) %>%
  summarise(p.max  = max(p, na.rm = TRUE)) %>%
  filter(hidro_ano %in% filtro)

#### Analise dos dados

files <- list.files(path="C:/dados_plu_ana/raw_stations/", pattern="*.rds", full.names=TRUE, recursive=FALSE)

count.all <- 0
count.val <- 0
n <- length(files)

out <- data.frame(codigo    = as.integer(),
                  hidro_ano = as.integer(),
                  p.max     = as.double())


for(i in 1:n){
  
  sta <- readRDS(files[i])
  
  count.all <- count.all+1
  
  sta.max = tryCatch(
    {
      get_max(sta)
    },
    error = function(e){
      print(e)
      return(NULL)
    },
    finally={
      
    }
  )
  
  if(!is.null(sta.max)){
    count.val <- count.val+1
    out <- bind_rows(out,sta.max)
  }
  print(paste0(sta$EstacaoCodigo %>% unique(), ' total: ', count.all,'/', n, ' válidas: ', count.val))
  
}

saveRDS(out, 'C:/dados_plu_ana/max_plu_080422.rds')


df <- readRDS('C:/dados_plu_ana/max_plu_080422.rds')

output <- df %>%
  group_by(codigo) %>%
  nest() %>%
  mutate(map_dfr(.x = data, .f = ~ get_stats(dados = .x))) %>%
  select(-data)

num_sta <- nrow(output)

tmp <- output %>%
  group_by(n) %>%
  summarise(n = n()) %>%
  mutate(x = 1:nrow(.),
         n_acc = num_sta - cumsum(n) + 1)

g1 <- ggplot(output, aes(n)) +
  geom_histogram(color="darkblue", fill = "lightblue", alpha=0.8, size = 0.3) +
  theme_ipsum() +
  labs(x = 'número de anos', y = 'número de postos')

g2 <- ggplot(tmp, aes(x = x, y = n_acc)) +
  geom_line(size = .3) +
  theme_ipsum() +
  xlab('número de anos') +
  ylab('Total de postos com mais de n anos')

#Histograma + linha
g1 + g2 + 
  plot_layout(widths = c(1, 1))

#densidade do coef de assimetria
ggplot(output, aes(x = skew)) +
  geom_density(adjust = 2, 
               size = 0.2, 
               fill = "gray", 
               color = "black", 
               alpha = 0.4) +
  labs(x="Coeficiente de assimetria", y = "Densidade") +
  theme_ipsum() +
  theme(legend.title = element_blank()) 

#densidade do coef de assimetria por região

tmp <- output %>%
  left_join(cods %>% select(Codigo, nmEstado), by = c('codigo' = 'Codigo'))

df_estados <- data.frame(nmEstado = c('PARÁ', 'AMAPÁ', 'AMAZONAS', 'RORAIMA', 'ACRE', 'RONDÔNIA', 'MARANHÃO', 'CEARÁ', 'PIAUÍ', 'PERNAMBUCO', 'RIO GRANDE DO NORTE', 'TOCANTINS', 'PARAÍBA', 'ALAGOAS', 'BAHIA', 'SERGIPE', 'MATO GROSSO', 'GOIÁS', 'DISTRITO FEDERAL', 'MATO GROSSO DO SUL', 'MINAS GERAIS', 'ESPÍRITO SANTO', 'SÃO PAULO', 'RIO DE JANEIRO', 'SANTA CATARINA', 'PARANÁ', 'RIO GRANDE DO SUL', 'COLÔMBIA', 'EQUADOR', 'PERU', 'BOLÍVIA', 'CHILE', 'PARAGUAI', 'ARGENTINA', 'URUGUAI', 'VENEZUELA', 'GUIANA FRANCESA', 'SURINAME', 'GUIANA'),
                         regiao   = c('Norte', 'Norte', 'Norte', 'Norte', 'Norte', 'Norte', 'Nordeste', 'Nordeste', 'Nordeste', 'Nordeste', 'Nordeste', 'Nordeste', 'Nordeste', 'Nordeste', 'Nordeste', 'Nordeste', 'Centro oeste', 'Centro oeste', 'Centro oeste', 'Centro oeste', 'Sudeste', 'Sudeste', 'Sudeste', 'Sudeste', 'Sul', 'Sul', 'Sul', 'Exterior', 'Exterior', 'Exterior', 'Exterior', 'Exterior', 'Exterior', 'Exterior', 'Exterior', 'Exterior', 'Exterior', 'Exterior', 'Exterior'))

tmp <- tmp %>%
  left_join(df_estados, by = 'nmEstado')

ggplot(tmp, aes(x = skew, color = regiao, fill = regiao)) +
  geom_density(adjust = 2, 
               size = 0.3,  
               alpha = 0.2) +
  labs(x="Coeficiente de assimetria", y = "Densidade") +
  theme_ipsum() +
  theme(legend.title = element_blank()) 

#quantis da GEV
df30 <- df %>%
  group_by(codigo) %>%
  summarise(n = n()) %>%
  filter(n >= 30) %>%
  select(codigo)

out_AF <- df %>% filter(codigo %in% df30$codigo) %>%
  group_by(codigo) %>%
  nest() %>%
  mutate(map_dfr(.x = data, .f = ~ fit_gev(dados = .x))) %>%
  select(-data)

t <- out_AF %>% 
  select(codigo, p.10anos, p.50anos, p.100anos) %>%
  pivot_longer(cols = starts_with('p.'),
               names_to = 'grupo',
               values_to = 'quantil')

ggplot(t, aes(x = quantil, color = grupo, fill = grupo)) +
  geom_density(adjust = 2, 
               size = 0.3,  
               alpha = 0.2) +
  labs(x="Quantil", y = "Densidade") +
  theme_ipsum() +
  theme(legend.title = element_blank()) 


#Mapa de distribuição espacial de p

inventario <- cods %>% select(Codigo, Latitude, Longitude, nmEstado)

estados <- c('PARÁ', 'AMAPÁ', 'AMAZONAS', 'RORAIMA', 'ACRE', 'RONDÔNIA', 'MARANHÃO', 'CEARÁ', 'PIAUÍ', 'PERNAMBUCO', 'RIO GRANDE DO NORTE', 'TOCANTINS', 'PARAÍBA', 'ALAGOAS', 'BAHIA', 'SERGIPE', 'MATO GROSSO', 'GOIÁS', 'DISTRITO FEDERAL', 'MATO GROSSO DO SUL', 'MINAS GERAIS', 'ESPÍRITO SANTO', 'SÃO PAULO', 'RIO DE JANEIRO', 'SANTA CATARINA', 'PARANÁ', 'RIO GRANDE DO SUL')

data_map <- output %>% 
  left_join(inventario, by = c('codigo' = 'Codigo')) %>%
  filter(nmEstado %in% estados) %>%
  filter(n >= 25) %>%
  arrange(med) %>%
  tail(250) %>%
  select(codigo, med, n, p_rec, ano_rec, Latitude, Longitude) %>%
  mutate(periodo = case_when(ano_rec <= 1960                    ~ '1: < 1960',
                             ano_rec >  1960 && ano_rec <= 1980 ~ '2: 1960-1980',
                             ano_rec >  1980 && ano_rec <= 2000 ~ '3: 1980-2000',
                             ano_rec >  2000                    ~ '4: > 2000'))

base_shape <- st_read('C:/dados_plu_ana/brasil_contorno.shp')

breaks_rec <- seq(100, 300, by = 50)

ggplot(data = base_shape) +
  geom_sf( fill = 'darkgrey', alpha = 0.4) +
  geom_point(data = data_map, aes(x=Longitude, y = Latitude, size = med, color = ano_rec), alpha = 0.5) +
  scale_size_continuous(trans = "log", breaks = breaks_rec, range = c(0,10), name = "Precipitação (mm)") +
  scale_color_binned(type = "viridis", name = "Ocorrência do recorde") +
  ggtitle("250 maiores médias máximas anuais de precipitação")









