#rm(list=ls())

library(tidyverse)
library(sf)
library(sp)
library(spdep)

#setwd("C:\\Users\\Jonny\\Google Drive\\Academic\\FGV-SP\\shiny\\New_apps_2018\\March 2019\\spatial2")

mun <- readRDS("data/mun_simple3.rds") %>% st_as_sf() %>% 
  group_by(UF,GEOCOD_CH) %>% 
  summarize() %>%
  mutate(COD_MUN_IBGE=as.numeric(as.character(GEOCOD_CH))) %>%
  as("Spatial")

args <- list(position = c(3,5,6,7))

vars <- list("CODIGO_CARGO","NUM_TURNO","UF","NUMERO_PARTIDO","ANO_ELEICAO","COD_MUN_IBGE",
             "QTDE_VOTOS","NUMERO_CANDIDATO","SIGLA_PARTIDO","NOME_URNA_CANDIDATO",
             "DESC_SIT_TOT_TURNO")

votos_ls <- pmap(args, 
                 cepespR::get_elections,
                 year = "1998,2002,2006,2010,2014,2018",
                 columns_list=vars)



#saveRDS(votos_ls,"temp.rds")
#votos_ls <- readRDS("temp.rds")

votos_df <- bind_rows(votos_ls)

votos_UF <- votos_df %>% split(.$UF)

votos_UF2 <- votos_UF %>% 
  map(~complete(.,COD_MUN_IBGE,nesting(ANO_ELEICAO,NUMERO_CANDIDATO,NOME_URNA_CANDIDATO,DESC_SIT_TOT_TURNO,CODIGO_CARGO,NUM_TURNO,UF), 
                fill=list(QTDE_VOTOS=0)))

votos_UF2 <- bind_rows(votos_UF2)

votos_UF2 <- votos_UF2 %>% 
  group_by(ANO_ELEICAO,CODIGO_CARGO,UF,NUM_TURNO) %>% 
  mutate(Tot_State=sum(QTDE_VOTOS,na.rm=T)) %>%
  group_by(ANO_ELEICAO,CODIGO_CARGO,UF,NUM_TURNO,COD_MUN_IBGE) %>%
  mutate(Tot_Mun=sum(QTDE_VOTOS,na.rm=T)) %>%
  group_by(ANO_ELEICAO,CODIGO_CARGO,UF,NUM_TURNO,NUMERO_CANDIDATO) %>%
  mutate(Tot_Deputado=sum(QTDE_VOTOS,na.rm=T),
         Mun_Vote_Share=(QTDE_VOTOS/Tot_Mun)*100,
         Cand_Vote_Share = (QTDE_VOTOS/Tot_Deputado)*100,
         G_temp = (QTDE_VOTOS/Tot_Deputado - Tot_Mun/Tot_State)^2,
         G_Index = sum(G_temp,na.rm=T),
         LQ = (QTDE_VOTOS/Tot_Deputado)/(Tot_Mun/Tot_State))
#saveRDS(votos_UF2,"temp2.rds")
#votos_UF2 <- readRDS("temp2.rds")

#Test: should have 22 rows:
#votos_UF2 %>% filter(UF=="AC" & ANO_ELEICAO==1998 & CODIGO_CARGO==6 & NUM_TURNO==1 & NUMERO_CANDIDATO==12)


#### Moran's I neighbourhood matrix by state. Only for contiguous.
states <- c("AC","AM","AL","AP","BA","GO","MA","MS","PI","RR","RS","SC","SE","SP","TO","CE","ES","MG","MT","PA","PB","PE","PR","RJ","RN","RO")

states_nb <- list()
states_nb_list <- list()

for (s in states){
  mun_state <- mun[mun$UF == s,]
  states_nb[[which(s==states)]] <- knn2nb(knearneigh(coordinates(mun_state), k = 6))
  states_nb_list[[which(s==states)]] <- nb2listw(states_nb[[which(s==states)]],zero.policy=TRUE)
}
names(states_nb) <- states
names(states_nb_list) <- states

#votos_sf <- mun %>% st_as_sf() %>% left_join(votos_df_stats,by="COD_MUN_IBGE")

votos_LQ <- votos_UF2 %>% mutate(case_when(is.na(LQ)~0,
                                                TRUE~LQ)) %>%
  group_by(ANO_ELEICAO,CODIGO_CARGO,UF,NUM_TURNO,NUMERO_CANDIDATO,NOME_URNA_CANDIDATO,DESC_SIT_TOT_TURNO,G_Index,Tot_Deputado) %>%
  dplyr::select(LQ) %>%
  nest(.key="LQ")

votos_LQ$LQ_vector <- votos_LQ$LQ %>% map("LQ")

states_nb_df <- tibble(UF=names(states_nb_list),
                       listw=states_nb_list,
                       n=states_nb %>% map_int(length),
                       S0=states_nb_list %>% map_dbl(Szero))

votos_LQ <- votos_LQ %>% left_join(states_nb_df,by="UF")

votos_LQ <- votos_LQ %>% filter(!UF=="DF")

#Then pmap the separated LQ data, the listw object, the length of the nb object and the Szero of the nb object. Repeating over states as necessary.
#saveRDS(votos_LQ,"temp3.rds")

#votos_LQ$LQ_vector_length <- votos_LQ$LQ_vector %>% map_int(length) 
#votos_LQ <- votos_LQ %>% mutate(Lengths_OK=LQ_vector_length==n)
#summary(votos_LQ$Lengths_OK)

#votos_LQ_problem <- votos_LQ %>% filter(is.na(Lengths_OK))
#votos_LQ_problem %>% select(ANO_ELEICAO,UF) %>% distinct()

#votos_LQ %>% select(ANO_ELEICAO,UF,NUMERO_CANDIDATO,NUMERO_PARTIDO,DESC_SIT_TOT_TURNO,NUM_TURNO,LQ_vector_length,n)

votos_LQ$Moran_I <- votos_LQ %>%
  dplyr::select(LQ_vector,listw,n,S0) %>%
  rename("x"="LQ_vector") %>%
  pmap(moran,zero.policy=TRUE,NAOK=TRUE) %>%
  map(~.x[["I"]]) %>% 
  unlist()

#Test comparisons:
#votos_LQ %>% filter(ANO_ELEICAO==2014 & CODIGO_CARGO==6 & NUM_TURNO==1 & NUMERO_CANDIDATO==1012 & UF=="AC") %>% select(-LQ,-LQ_vector)

#votos_LQ %>% filter(ANO_ELEICAO==2014 & CODIGO_CARGO==6 & NUM_TURNO==1 & NUMERO_CANDIDATO==1314 & UF=="AC") %>% select(-LQ,-LQ_vector)
#similar but not exact, due to zeros and different neighbourhood matrix

output <- votos_LQ %>% select(-LQ,-LQ_vector,-listw,-n,-S0)

saveRDS(output,"data/spatial_data.rds")

