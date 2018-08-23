rm(list = ls())

suppressWarnings(dir.create("data/mun_totals"))

# 1. Download -------------------------------------------------------------

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

anos <- seq(1998, 2014, by = 4)
cargos <- c(7)

args <- expand.grid(ano = anos, cargo = cargos, uf = ufs)

get_data <- function(ano, cargo, uf){
  url <- "http://api.cepesp.io/api/consulta/tse"
  
  ### Load municipal voting totals
  vars_mun <- list("UF","ANO_ELEICAO","COD_MUN_IBGE","QTDE_VOTOS")
  names(vars_mun) <- rep("selected_columns[]",length(vars_mun))
  filter <- list("columns[0][name]"="UF","columns[0][search][value]" = uf)
  consulta_mun <- append(append(list(cached = TRUE,
                                     anos   = ano,
                                     uf     = uf,
                                     agregacao_regional = 6,
                                     agregacao_politica = 2,
                                     cargo              = cargo),
                                vars_mun),
                         filter)
  mun_temp <- httr::content(httr::GET(url,query=consulta_mun),type="text/csv", encoding = "UTF-8")
  mun_temp <- mun_temp[,c("COD_MUN_IBGE","QTDE_VOTOS")]
  colnames(mun_temp)[colnames(mun_temp)=="QTDE_VOTOS"] <- "Tot_Mun"
  
  mun_totals <- mun_temp
  Sys.sleep(1)
  return(mun_totals)
}

data_ls <- purrr::pmap(args, get_data)

# 2. Save -----------------------------------------------------------------

for(i in seq_along(data_ls)){
  readr::write_rds(data_ls[[i]], paste0("data/mun_totals/",args$ano[[i]], "_", args$cargo[[i]], "_", args$uf[[i]], ".rds"))
}
