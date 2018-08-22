rm(list = ls())

library(tidyverse)

args <- list(position = c(1,3,5,6,7))

candidatos_ls <- pmap(args, 
                      cepespR::get_candidates,
                      columns_list = list("ANO_ELEICAO",
                                          "NUM_TURNO",
                                          'SIGLA_UF',
                                          'CODIGO_CARGO',
                                          'SIGLA_PARTIDO',
                                          'NOME_CANDIDATO',
                                          'NUMERO_CANDIDATO'))

candidatos_df <- bind_rows(candidatos_ls)

candidatos_df$PARTIDOS <- PARTIDOS

candidatos_df <- candidatos_df %>% 
  group_by(ANO_ELEICAO, NUM_TURNO, SIGLA_UF, CODIGO_CARGO) %>% 
  summarise(PARTIDOS = list(SIGLA_PARTIDO))

write_rds(candidatos_df, "data/party_template.rds")
