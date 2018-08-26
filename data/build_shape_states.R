rm(list = ls())

library(tidyverse)
library(sf)
library(RCurl)

# 1. Download Shape -------------------------------------------------------

temp_file <- tempfile()

temp_dir <- tempdir()

url_use <- "ftp://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2015/Brasil/BR/BR.zip"

download.file(url_use, destfile = temp_file)

unzip(temp_file, exdir = temp_dir)

uf_shape <- sf::read_sf(paste0(temp_dir,"/BR/BRUFE250GC_SIR.shp"))

uf_shape <- rmapshaper::ms_simplify(uf_shape)

uf_shape <- rmapshaper::ms_filter_islands(uf_shape)

ufs <- list("Acre"                 = "AC",
            "Alagoas"              = "AL",
            "Amazonas"             = "AM",
            "Amapá"                = "AP",
            "Bahia"                = "BA",
            "Ceará"                = "CE",
            "Distrito Federal"     = "DF",
            "Espírito Santo"       = "ES",
            "Goiás"                = "GO",
            "Maranhão"             = "MA",
            "Mato Grosso"          = "MT",
            "Mato Grosso do Sul"   = "MS",
            "Minas Gerais"         = "MG",
            "Pará"                 = "PA",
            "Paraíba"              = "PB",
            "Paraná"               = "PR",
            "Pernambuco"           = "PE",
            "Piauí"                = "PI",
            "Rio de Janeiro"       = "RJ",
            "Rio Grande do Norte"  = "RN",
            "Rio Grande do Sul"    = "RS",
            "Rondônia"             = "RO",
            "Roraima"              = "RR",
            "Santa Catarina"       = "SC", 
            "São Paulo"            = "SP",
            "Sergipe"              = "SE",
            "Tocantins"            = "TO")

names(ufs) <- stringr::str_to_upper(names(ufs))

uf_shape$UF <- vector(mode = "character", length = nrow(uf_shape))

for(i in seq_along(uf_shape$NM_ESTADO)){
  uf_shape$UF[[i]] <- ufs[[stringr::str_to_upper(uf_shape$NM_ESTADO[[i]])]]
}

dplyr::glimpse(uf_shape)

# 2. Save -----------------------------------------------------------------

for(i in seq_along(uf_shape$UF)){
  uf_shape_use <- uf_shape[uf_shape$UF == uf_shape$UF[[i]],]
  readr::write_rds(uf_shape_use, paste0("data/shape_states/", uf_shape$UF[[i]],".rds"))
}

length(list.files("data/shape_states/"))

# 3. Shape Brasil ---------------------------------------------------------

temp_file <- tempfile()

temp_dir <- tempdir()

url_use = "ftp://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2015/Brasil/BR/br_unidades_da_federacao.zip"

download.file(url_use, destfile = temp_file)

unzip(temp_file, exdir = temp_dir)

br_shape <- sf::read_sf(paste0(temp_dir,"/BRUFE250GC_SIR.shp"))

br_shape <- rmapshaper::ms_simplify(br_shape)

br_shape <- rmapshaper::ms_filter_islands(br_shape)

readr::write_rds(br_shape, paste0("data/shape_states/br.rds"))




