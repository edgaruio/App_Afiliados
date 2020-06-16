## ================== 0. Paquetes ------------------------------------------------
rm(list = ls())
# options(scipen = 999) 1018426823

library(shiny);library(ggplot2);library(dplyr)
library(maptools);library(shinydashboard);library(shinythemes)
library(DT);library(leaflet);library(shinyjs)
library(data.table); library(readxl); library(leaflet.extras)
library(geosphere); library(rgdal)
library(Hmisc); library(tidyr); library(tictoc)
library(shinycustomloader)
# Pir1, salario, sectorCiiu, estrato, estadoscivil, numero_beneficarios_cuota_monetaria, segmento_grupo_familair

# ================== 1. Datos -----------------------------------------------

empresa <- readRDS("Data/geo_empresas_may.rds") %>%
  mutate(id_empresa = as.character(id_empresa)) %>%
  filter(estado_empresa == "al día",
         !is.na(cx_empresa))
str(empresa)

persona <- readRDS("Data/geo_personas_may.rds") %>%
  mutate(id_empresa = as.character(id_empresa),
         Categoria = as.character(Categoria),
         Segmento_poblacional = as.character(Segmento_poblacional))
str(persona)

name_piramide1 <- c("1 Emp Grandes","2 Emp Medio","3 Empresas Pymes","4 Micro")
name_piramide2 <- c("Total","1.1 Platinum","1.2 Premium","2.1 Gold","2.2 Silver","3.1 VIP","3.2 VIP Estándar","4.1 Estándar")
name_segmento <- c("Alto","Joven","Medio","Básico")
name_segmento2 <- c("Total","Alto","Joven","Medio","Básico")
name_edad <- c("Total",names(table(persona$Edad_agru)))
name_localidad <- c(unique(empresa$LOCALIDAD))

# # Capas
cundi <- readRDS("Data/poligonos-localidades/Cundinamarca.rds")
localidad <- readOGR("Data/poligonos-localidades/poligonos-localidades.shp")
# departamento <- shapefile("Data/poligonos_dpto/depto.shp")

# Infraestructura Colsubsidio
infra <- read_excel("Data/INFRAESTRUCTURA_PROPIA_COLSUBSIDIO.xlsx") %>% data.frame()
str(infra)
AGENCIA  <- infra %>% filter(UES == "AGENCIA DE EMPLEO")
CSERVICIOS <- infra %>% filter(UES=="CENTROS DE SERVICIO")
EDUCACION <- infra %>% filter(UES=="EDUCACION")
MERCADEO_SOCIAL <- infra %>% filter(UES=="MERCADEO SOCIAL")
SUPERMERCADOS <- infra %>% filter(TIPO=="SUPERMERCADO")
MEDICAMENTOS <- infra %>% filter(TIPO=="DROGUERIA")
RYT <- infra %>% filter(UES=="RECREACION Y TURISMO")
SALUD <- infra %>% filter(UES=="SALUD")
VIVIENDA <- infra %>% filter(UES=="VIVIENDA")


# Infraestrutura LogColsubsidio
leafIconsAG <- icons(
  iconUrl = ifelse(AGENCIA$UES == "AGENCIA DE EMPLEO",
                   "Data/icons/ICONOS_ACT/LogColsubsidio.png","Data/icons/ICONOS_COLSUBSIDIO/LogColsubsidio.png"),
  iconWidth = 28, iconHeight = 45,
  iconAnchorX = 16, iconAnchorY = 40)


leafIconsCS <- icons(
  iconUrl = ifelse(CSERVICIOS$UES == "CENTROS DE SERVICIO",
                   "Data/icons/ICONOS_ACT/Colsubsidio.png","Data/icons/ICONOS_COLSUBSIDIO/Colsubsidio2.png"),
  iconWidth = 28, iconHeight = 45,
  iconAnchorX = 16, iconAnchorY = 40)

leafIconsED <- icons(
  iconUrl = ifelse(MERCADEO_SOCIAL$UES == "EDUCACION",
                   "Data/icons/ICONOS_ACT/Educacion.png","Data/icons/ICONOS_ACT/Educacion.png"),
  iconWidth = 28, iconHeight = 45,
  iconAnchorX = 16, iconAnchorY = 40)

leafIconsSP <- icons(
  iconUrl = ifelse(MERCADEO_SOCIAL$TIPO == "SUPERMERCADOS",
                   "Data/icons/ICONOS_ACT/Supermercados.png","Data/icons/ICONOS_ACT/Supermercados.png"),
  iconWidth = 28, iconHeight = 45,
  iconAnchorX = 16, iconAnchorY = 40)

leafIconsDR <- icons(
  iconUrl = ifelse(MERCADEO_SOCIAL$TIPO == "DROGUERIA",
                   "Data/icons/ICONOS_ACT/Farmacias.png","Data/icons/ICONOS_ACT/Farmacias.png"),
  iconWidth = 28, iconHeight = 45,
  iconAnchorX = 16, iconAnchorY = 40)

leafIconsRYT <- icons(
  iconUrl = ifelse(MERCADEO_SOCIAL$UES == "RECREACION Y TURISMO",
                   "Data/icons/ICONOS_ACT/Recreacion.png","Data/icons/ICONOS_ACT/Recreacion.png"),
  iconWidth = 28, iconHeight = 45,
  iconAnchorX = 16, iconAnchorY = 40)

leafIconsSL <- icons(
  iconUrl = ifelse(MERCADEO_SOCIAL$UES == "SALUD",
                   "Data/icons/ICONOS_ACT/Salud.png","Data/icons/ICONOS_ACT/Salud.png"),
  iconWidth = 28, iconHeight = 45,
  iconAnchorX = 16, iconAnchorY = 40)

leafIconsVV <- icons(
  iconUrl = ifelse(MERCADEO_SOCIAL$UES == "VIVIENDA",
                   "Data/icons/ICONOS_ACT/Vivienda.png","Data/icons/ICONOS_ACT/Vivienda.png"),
  iconWidth = 28, iconHeight = 45,
  iconAnchorX = 16, iconAnchorY = 40)




