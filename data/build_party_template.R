rm(list = ls())

library(tidyverse)
 
 args <- expand.grid(position = c(1,3,5,6,7),
              year=c("1998","2002","2006","2010","2014","2018"))
 
 candidatos_ls <- pmap(args,
                       cepespR::get_candidates,
                       columns_list = list("ANO_ELEICAO",
                                           "NUM_TURNO",
                                           'SIGLA_UF',
                                           'CODIGO_CARGO',
                                           'SIGLA_PARTIDO',
                                           'NOME_CANDIDATO',
                                           'NOME_URNA_CANDIDATO',
                                           'NUMERO_CANDIDATO',
                                           'NUM_TITULO_ELEITORAL_CANDIDATO',
                                           'DATA_NASCIMENTO',
                                           'DES_SITUACAO_CANDIDATURA',
                                           'DESC_SIT_TOT_TURNO'))
 
 
 candidatos_ls <- map(candidatos_ls,
                      mutate,
                      NUM_TITULO_ELEITORAL_CANDIDATO = as.double(NUM_TITULO_ELEITORAL_CANDIDATO),
                      DATA_NASCIMENTO                = as.character(DATA_NASCIMENTO))
 
 candidatos_df <- bind_rows(candidatos_ls)
 
 write_rds(candidatos_df, "./data/candidatos.rds")

candidatos_df <- read_rds('./data/candidatos.rds')

template <- candidatos_df %>% 
  filter(DES_SITUACAO_CANDIDATURA %in% c('DEFERIDO',
                                         'DEFERIDO COM RECURSO',
                                         'APTO')) %>% 
  mutate(RESULTADO = ifelse(DESC_SIT_TOT_TURNO %in% c('ELEITO',
                                                      'ELEITO POR QP',
                                                      'ELEITO POR MEDIA',
                                                      'MEDIA'),
                            1,
                            2)) %>% 
  group_by(ANO_ELEICAO, NUM_TURNO, SIGLA_UF, CODIGO_CARGO, SIGLA_PARTIDO, RESULTADO) %>% 
  mutate(NOME_URNA_CANDIDATO = paste0(NOME_URNA_CANDIDATO, " (", SIGLA_PARTIDO,")")) %>% 
  summarise(LISTA_NOMES  = list(NOME_URNA_CANDIDATO),
            LISTA_NUMERO = list(NUMERO_CANDIDATO))

for(i in seq_along(template$LISTA_NUMERO)){
  names(template$LISTA_NUMERO[[i]]) <- template$LISTA_NOMES[[i]]
}

write_rds(template, "./data/party_template.rds")
