rm(list = ls())

library(tidyverse)

get_candidatos <- function(ano, cargo){
  url_base <- "http://cepesp.io/api/consulta/candidatos"
  
  params <- list(
    `cargo` = cargo,
    `ano`   = ano
  )
  
  httr::GET(url_base, query = params) %>% 
    httr::content(type = "text/plain", encoding = "UTF-8") %>% 
    readr::read_csv(col_types = cols_only(ANO_ELEICAO                    = col_integer(),
                                          SIGLA_UF                       = col_character(),
                                          NUM_TURNO                      = col_integer(),
                                          CODIGO_CARGO                   = col_integer(),
                                          NUMERO_PARTIDO                 = col_integer(),
                                          SIGLA_PARTIDO                  = col_character()),
                    locale = locale(encoding = "UTF-8"))
}

# 1. Carregando os Bancos -------------------------------------------------

cargos <- c(1,3,5,6)

anos <- c(1998,2002,2006,2010,2014)

args <- expand.grid(ano = anos, cargo = cargos)

candidatos_ls <- pmap(args,get_candidatos)

candidatos_df <- bind_rows(candidatos_ls)

candidatos_df$PARTIDOS <- PARTIDOS

candidatos_df <- candidatos_df %>% 
  group_by(ANO_ELEICAO, NUM_TURNO, SIGLA_UF, CODIGO_CARGO) %>% 
  summarise(PARTIDOS = list(SIGLA_PARTIDO))

write_rds(candidatos_df, "data/party_template.rds")
