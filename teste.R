library(magrittr)

httr::GET('http://cepesp.io/api/consulta/candidatos?ano=2010&cargo=3&c[]="ANO_ELEICAO"&c[]="NUM_TURNO"&c[]="CODIGO_CARGO"&c[]="NOME_URNA_CANDIDATO"&c[]="NUMERO_CANDIDATO"') %>% 
  httr::content(type = "text/csv")
