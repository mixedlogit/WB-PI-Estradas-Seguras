
rm(list = ls())

library(tidyverse)
install.packages(enderecob)

# Data -------------------------


data.escola <- read.csv(file = "datasets/intermediate/schools/Análise - Tabela da lista das escolas - Detalhado.csv")

names(data.escola)

names(data.escola) <- c("restricao_atendimento","escola","codigo_inep",
                        "abrev_uf","name_muni","urban","localidade_dif",
                        "categoria",
                        "endereco","telefone","nivel_governo","tipo_escola",
                        "conveniada_publico","regulamentacao","porte",
                        "etapa_modalidade","outras_ofertas","lat","long")

data.pi <- data.escola %>% 
  filter(abrev_uf=="PI") %>% 
  filter(categoria == "Pública") %>% 
  filter(restricao_atendimento != "ESCOLA PARALISADA") %>% 
  filter(!(etapa_modalidade %in% c("Educação de Jovens Adultos",
                                 "Ensino Médio, Educação Profissional, Educação de Jovens Adultos",
                                 "Ensino Médio",
                                 "Ensino Médio, Educação de Jovens Adultos",
                                 "",
                                 "Educação Profissional, Educação de Jovens Adultos",
                                 "Educação Profissional")))

data.pi %>% 
  group_by(porte) %>% 
  reframe(n = n())

data.pi %>% 
  group_by(restricao_atendimento) %>% 
  reframe(n =n())
  
data.pi.na <- data.pi %>% 
  filter(is.na(lat))

library(geocodebr)

teste <- enderecobr::padronizar_enderecos(enderecos = data.pi.na,
                                 campos_do_endereco = c(logradouro="endereco",
                                                        municipio="name_muni",
                                                        estado="abrev_uf"))
glimpse(data.pi.na)

campos <- definir_campos(estado = "abrev_uf",
                         municipio = "name_muni",
                         logradouro = "logradouro_padr")


teste <- geocode(enderecos = data.pi.na,
                            campos_endereco = campos,
                 resolver_empates = TRUE)
names(teste)

data.ms.na <- teste |> select(1:17,20:21)

data.ms <- rbind(data.ms |>filter(!is.na(lat)),data.ms.na)

data.ms.urban <- data.ms.na %>% 
  filter(urban == "Urbana")

data.ms.urban <- sf::st_as_sf(data.ms.urban,coords=c("long","lat"),
                              crs=4674)

mapview::mapview(data.ms.urban)
data.public <- data.ms %>% 
  filter(nivel_governo == "Estadual")
# Road network ----------------------------------------------------------

library(geobr)
library(osmdata)
library(sf)
library(dplyr)

# 1️⃣ Obter o limite do estado de Mato Grosso do Sul
ms <- read_state(code_state = "MS", year = 2020)

# 2️⃣ Definir bounding box a partir do limite
ms_bb <- st_bbox(ms)

# 3️⃣ Fazer consulta ao OSM para todas as vias (highways)
ms_osm <- opq(ms_bb) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

# 4️⃣ Extrair as linhas (vias)
vias <- ms_osm$osm_lines
