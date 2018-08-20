rm(list = ls())

suppressWarnings(dir.create("data/state_totals"))

# 1. Download -------------------------------------------------------------

cargos <- c(1,3,5,6)
anos <- seq(1998,2014, by = 4)
args <- expand.grid(ano = anos, cargo = cargos)

get_data <- function(ano, cargo){
  url <- "http://api.cepesp.io/api/consulta/tse"
  filter <- list()
  
  
  consulta_state <- append(append(list(cached = TRUE,
                                       anos   = ano,
                                       uf     = "all",
                                       agregacao_regional = 2,
                                       agregacao_politica = 2,
                                       cargo              = cargo),
                                  vars_state),
                           filter)
  
  state_temp <- httr::GET(url, query = consulta_state) %>% 
    httr::content(type="text/csv", encoding = "UTF-8")
  
  state_temp <- state_temp[,c("UF","QTDE_VOTOS")]
  
  colnames(state_temp)[colnames(state_temp)=="QTDE_VOTOS"] <- "Tot_State"
  
  state_totals <- state_temp
  
  return(state_totals)
}

data_ls <- purrr::pmap(args, get_data)

# 2. Save -----------------------------------------------------------------

for(i in seq_along(data_ls)){
  readr::write_rds(data_ls[[i]], paste0("data/state_totals/",args$ano[[i]], "_", args$cargo[[i]], ".rds"))
}
