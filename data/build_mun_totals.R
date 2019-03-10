rm(list = ls())

suppressWarnings(dir.create("data/mun_totals"))

# 1. Download -------------------------------------------------------------

anos <- seq(1998, 2018, by = 4)
cargos <- c(3,5,6,7)

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


args <- expand.grid(year = anos, position = cargos,state=ufs)

muns_ls <- purrr::pmap(args, cepespR::get_elections, 
                        regional_aggregation = "Municipality", 
                        political_aggregation="Consolidated",
                        dev=T)

muns_temp <- purrr::map(muns_ls, dplyr::select, COD_MUN_IBGE, NUM_TURNO,QTD_COMPARECIMENTO)

muns_temp <- purrr::map(muns_temp, dplyr::rename, Tot_Mun = QTD_COMPARECIMENTO)

# 2. Save -----------------------------------------------------------------

for(i in seq_along(muns_temp)){
  readr::write_rds(muns_temp[[i]], paste0("data/mun_totals/",args$year[[i]], "_", args$position[[i]], "_", args$state[[i]], ".rds"))
}
