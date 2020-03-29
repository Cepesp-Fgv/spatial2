library(plyr)
library(data.table)
library(sp)
library(sf)
library(spdep)
library(scales)
library(leaflet)
library(rgeos)
library(raster)
library(maptools)
library(ggplot2)
library(httr)
library(ape)
library(RCurl)
library(digest)
library(dplyr)
library(DT)
library(magrittr)
library(shinyalert)
library(tidyr)
library(shiny)

source("global.R")
source("database.R")

#input <- tibble::tibble(cargo = 7,
#                        Year = 2018,
#                        turno = 1,
#                        Party = "PSL",
#                        State  = "SP",
#                        candidato = "17079",
#                        eleito = 0,
#                        indicator="Medida QL"
#)
#turno_use <- turno <- input$turno

spatial2Server <- function(input, output, session) {
  
  ### Turno ###
  turno <- reactive({
    cargo <- as.numeric(input$cargo)
    if(cargo %in% c(1,3)){
      return(input$turno_value)
    } else {
      return(1)
    }
  })
  
  output$turno_UI <- renderUI({
    cargo <- as.numeric(input$cargo)
    if(cargo %in% c(1,3)){
      selectizeInput("turno_value", 
                     label = NULL,
                     selected = NULL,
                     choices = list("",
                                    "1º Turno" = 1,
                                    "2º Turno" = 2),
                     options = list(
                       placeholder = "Selecione um turno"
                     ))
    }
  })
  
  ### Partido ###
  
  partidos_escolhas <- reactive({
    cat("Parsing partidos_escolhas\n")
    cargo <- as.numeric(input$cargo)
    ano <- as.numeric(input$Year)
    turno_use <- turno()
    eleito <-  as.numeric(input$eleito)
    uf <- input$State
    
    print(cargo)
    
    
    if(uf == "" | is.na(ano) | is.na(cargo)){
      cat("Parsing partidos_escolhas. NULL\n")
      return(NULL)
    }    
    
    if(eleito == 1 & !(turno_use == 1 & cargo == 3)){
      party_template <- party_template[party_template$RESULTADO == eleito,]
    }
    
    choices <- (party_template$SIGLA_PARTIDO[party_template$CODIGO_CARGO == cargo &
                                               party_template$SIGLA_UF == uf &
                                               party_template$ANO_ELEICAO == ano &
                                               party_template$NUM_TURNO == turno_use])
    
    choices <- c("Todos os Partidos", unique(sort(choices)))
    cat("Parsing partidos_escolhas. CHECK!!!\n")
    return(choices)
  }) 
  
  output$party_UI <- renderUI({
    cat("Outputing party_UI.\n")
    partidos <- partidos_escolhas()
    if(is.null(partidos)){
      cat("Outputing party_UI. NULL\n")
      return(NULL)
    }
    
    UI <- selectizeInput("Party",
                         label = NULL,
                         choices = partidos,
                         selected = "Todos os Partidos",
                         options  = list(placeholder = 'Escolha um partido:',allowEmptyOption=TRUE))
    cat("Outputing party_UI. CHECK!!!\n")
    return(UI)
  })
  
  ### Candidato ###
  
  candidatos_value <- reactive({
    cat("Parsing candidates names.\n")
    
    
    if(is.null(partidos_escolhas())){
      cat("Parsing candidates names. NULL\n")
      return(NULL)
    } 
    
    cargo <- as.numeric(input$cargo)
    ano <- as.numeric(input$Year)
    partido <- input$Party
    eleito <- as.numeric(input$eleito)
    turno_use <- turno()
    uf <- input$State
    
    party_template <- party_template[party_template$CODIGO_CARGO == cargo,]
    party_template <- party_template[party_template$SIGLA_UF == uf,]
    party_template <- party_template[party_template$ANO_ELEICAO == ano,]
    party_template <- party_template[party_template$NUM_TURNO == turno_use,]
    
    if(partido != "Todos os Partidos"){
      party_template <- party_template[party_template$SIGLA_PARTIDO == partido,]
    }
    
    if(eleito == 1 & !(turno_use == 1 & cargo == 3)){
      party_template <- party_template[party_template$RESULTADO == eleito,]
    }
    
    choices <- unlist(party_template$LISTA_NUMERO)
    
    candidatos_value <- choices[sort(names(choices))]
    
    cat("Parsing candidates names. CHECK!!!\n")
    return(candidatos_value)
  })
  
  candidato_name <- function(cand_list, number) {

    for(key in names(cand_list)){
      value <- cand_list[key]
      if (value == number) {
        return(trim(gsub("\\([a-zA-Z ]+\\)", "", key)))
      }
    }
    
    return(NULL)
  }
  
    # query_observe <- reactive({
  #   ano <- isolate(input$Year)
  #   uf <- isolate(input$State)
  #   cargo <- isolate(as.numeric(input$cargo))
  #   party <- isolate(input$Party)
  #   
  #   cat(paste0("query_observe: ", paste0(ano,uf,cargo, party),"\n"))
  #   
  #   return(paste0(ano,uf,cargo, party))
  # })
  
  output$cand_UI <- renderUI({
    cat("Outputing cand_UI.\n")
    
    candidatos <- candidatos_value()
    
    if(is.null(candidatos)){
      cat("Outputing cand_UI. NULL\n")
      return(NULL)
    }
    
    print(paste0("candidato: ", candidatos[[1]]))
    
    cat("Outputing candidates UI.\n ")
    UI <- selectizeInput("candidato",
                         label = NULL,
                         choices = candidatos,
                         selected = NULL,
                         options  = list(placeholder = 'Escolha um candidato:',allowEmptyOption=TRUE))
    cat("Outputing cand_UI. CHECK!!!\n")
    return(UI)
  })
  
  ## Data Querys ##
  
  ### Query ###
  
  state_totals <- reactive({
    start <- Sys.time()
    cat("Downloading State Totals. ")
    
    ### Inputs ###
    ano <- input$Year
    cargo <- as.numeric(input$cargo)
    
    ### Loading State Totals
    state_totals <- readr::read_rds(paste0("data/state_totals/",ano,"_",cargo,".rds"))
    end_start <- difftime(Sys.time(), start, units = "secs")
    cat("CHECK!!! (",end_start, "seconds).\n", sep = "")
    return(state_totals)
  })
  
  mun_totals <- reactive({
    start <- Sys.time()
    cat("Downloading Municipal Totals. \n")
    
    ### Input ###
    ano <- input$Year
    uf <- input$State
    cargo <- as.numeric(input$cargo)
    
    ### Load municipal voting totals
    mun_totals <- readr::read_rds(paste0("data/mun_totals/", ano,"_", cargo,"_" , uf, ".rds"))
    
    end <- Sys.time()
    end_start <- round(difftime(end, start, units = "secs"),2)
    cat("Downloading Municipal Totals. CHECK!!! (",end_start, " seconds).\n", sep = "")
    
    return(mun_totals)
  })
  
  ### Test Query ###
  

  # url <- "http://api.cepesp.io/api/consulta/tse"
  
  banco <- eventReactive(input$button, {
    cat("Starting to download banco.\n")
    start <- Sys.time()
    withProgress(message="Por favor, espere...",
                 detail="Download dos dados",
                 value=0.3,{
                   
                   uf <- input$State
                   partido <- stringr::str_remove_all(input$Party, " ")
                   cargo <- as.numeric(input$cargo)
                   candidato <- as.numeric(input$candidato)
                   candidatos_list <- candidatos_value()
                   
                   if(is.null(partidos_escolhas()) | is.null(candidatos_list)){
                     cat("Starting to download banco. NULL\n")
                     return(1)
                   }
                  
                   cname <- candidato_name(candidatos_list, candidato)
                   print(cname)
                   
                   cat("Downloading main data (uf=", uf, "; partido=", partido, ";cargo=", cargo, ";candidato=",candidato,")\n", sep = "") 
                   
                   banco <- db_get_elections(year = input$Year,
                                             position = cargo,
                                             candidate_number = candidato, 
                                             state = uf,
                                             turn = turno(),
                                             name = cname)
                   
                   end_beginning <- round(difftime(Sys.time(), start, units = "secs"), 2)
                   cat("CHECK!!! (", end_beginning, "seconds)\n", sep = "")
                 })
    return(banco)
  })
  
  d <- reactive({
    cat("Calculating 'd' value. \n")
    
    banco_use <- banco()
    
    if(any(class(banco_use) == c("reactive"))){
      cat("Calculating 'd' value. NULL\n")
      return(NULL)
    }
    
    start <- Sys.time()
    
    withProgress(message="Por favor, espere...",detail="Download dos dados", value=0.3,{
      
      d <- data.table::as.data.table(banco_use)
      
      if(dim(d)[1] != 0){
        
        #Ideally will be faster when can request specific state
        setkeyv(d,c('ANO_ELEICAO','COD_MUN_IBGE','NUMERO_CANDIDATO'))
        
        #### Aggregations
        d <- merge(d,isolate(mun_totals()), by=c("COD_MUN_IBGE","NUM_TURNO"))
        #d <- merge(d,isolate(mun_totals), by=c("COD_MUN_IBGE","NUM_TURNO"))
        d <- merge(d,isolate(state_totals()), by=c("UF","NUM_TURNO"))
        #d <- merge(d,isolate(state_totals), by=c("UF","NUM_TURNO"))
        
        d[,Tot_Deputado := sum(QTDE_VOTOS), by=.(ANO_ELEICAO,UF,NUMERO_CANDIDATO)]
        d[,Mun_Vote_Share := (QTDE_VOTOS/Tot_Mun)*100]
        d[,Cand_Vote_Share := (QTDE_VOTOS/Tot_Deputado)*100]
        
        incProgress(amount = 0.7)
        
        #### G-Index Calcs
        d[,G_temp := (QTDE_VOTOS/Tot_Deputado - Tot_Mun/Tot_State)^2]
        d[,G_Index := sum(G_temp),by=.(ANO_ELEICAO,UF,NUMERO_CANDIDATO)] #Correct? CHECK
 
        #### LQ Calcs
        d[,LQ := (QTDE_VOTOS/Tot_Deputado)/(Tot_Mun/Tot_State),by=.(ANO_ELEICAO,UF,NUMERO_CANDIDATO)] #Correct?
      
        #Remove NULO line from selectable candidates, though is included in calculations of total statewide and municipal votes above
        d <- d[NOME_URNA_CANDIDATO!="#NULO#" | is.na(NOME_URNA_CANDIDATO)]
      } else {
        d <- data.table("UF"                   = character(),
                        "NUMERO_PARTIDO"       = integer(),
                        "ANO_ELEICAO"          = integer(),
                        "COD_MUN_IBGE"         = integer(),
                        "QTDE_VOTOS"           = integer(),
                        "NUMERO_CANDIDATO"     = integer(),
                        "SIGLA_PARTIDO"        = character(),
                        "NOME_URNA_CANDIDATO"  = character(),
                        "DESC_SIT_TOT_TURNO"   = character())
      }
      end <- Sys.time()
      end_beginning <- round(difftime(end,start, units = "secs"), 2)
      cat("Calculating 'd' value. CHECK!!! (", end_beginning, "seconds)\n")
      return(d)
    })
  })
  
  mun_state_contig <- reactive({
    uf <- input$State
    
    ## Break
    if(uf == ""){
      return(NULL)
    }
    
    beginning <- Sys.time()
    names(mun)[which(names(mun)=="UF")] <- "UF_shape"
    mun_state <- mun[mun$UF_shape == uf,]
    #   state_nb <- poly2nb(mun_state)
    #    if (any(card(state_nb)==0)){
    #      mun_state_contig <- mun_state[-which(card(state_nb)==0),]  
    #    } else {
    mun_state_contig <- mun_state
    #    }
    end <- Sys.time()
    cat("Time for trimming shapefile to state and first screening for neighbours:",end-beginning,".\n")
    return(mun_state_contig)
  })
  
  dz3 <- reactive({
    
    beginning <- Sys.time()
    
    dz2 <- d()
    
    if(is.null(dz2)){
      return(NULL)
    }
    
    candidato <- isolate(input$candidato)
    year <- isolate(input$Year)
    
    dz3_temp <- merge(isolate(mun_state_contig()),dz2, by.x="GEOCOD",by.y="COD_MUN_IBGE",all.x=TRUE,all.y=FALSE)
    
    #dz3_temp <- merge(isolate(mun_state_contig),dz2, by.x="GEOCOD",by.y="COD_MUN_IBGE",all.x=TRUE,all.y=FALSE)
    dz3_temp@data[is.na(dz3_temp@data[,"LQ"])==TRUE,"LQ"] <- 0
    dz3_temp@data[is.na(dz3_temp@data[,"QTDE_VOTOS"])==TRUE,"Mun_Vote_Share"] <- 0
    dz3_temp@data[is.na(dz3_temp@data[,"QTDE_VOTOS"])==TRUE,"Tot_State"] <- mean(dz3_temp@data[,"Tot_State"],na.rm=TRUE)
    dz3_temp@data[is.na(dz3_temp@data[,"QTDE_VOTOS"])==TRUE,"Tot_Deputado"] <- mean(dz3_temp@data[,"Tot_Deputado"],na.rm=TRUE)
    dz3_temp@data[is.na(dz3_temp@data[,"QTDE_VOTOS"])==TRUE,"NOME_URNA_CANDIDATO"] <- as.character(na.exclude(unique(dz3_temp@data[,"NOME_URNA_CANDIDATO"])))
    dz3_temp$Tot_Mun <- NULL
    
    # Removes duplicates from mun_totals()
    mun_t <- isolate(mun_totals())
    #mun_t <- mun_totals
    mun_t <- mun_t[!duplicated(mun_t$COD_MUN_IBGE), ]
    
    dz3_temp <- merge(dz3_temp, mun_t, by.x="GEOCOD", by.y="COD_MUN_IBGE")
    
    #dz3_temp <- merge(dz3_temp,isolate(mun_totals),by.x="GEOCOD",by.y="COD_MUN_IBGE")
    dz3_temp@data[is.na(dz3_temp@data[,"QTDE_VOTOS"])==TRUE,"QTDE_VOTOS"] <- 0
    end <- Sys.time()
    cat("Time for merging candidate data with shapefile:",end-beginning,".\n")
    dz3 <- dz3_temp
    return(dz3)
  })
  
  state_nb2 <- reactive({
    if(is.null(mun_state_contig())){
      return(NULL)
    }
    state_nb2 <- knn2nb(knearneigh(coordinates(mun_state_contig()), k = 6))
    #state_nb2 <- knn2nb(knearneigh(coordinates(mun_state_contig), k = 6))
    #state_nb2 <- poly2nb(mun_state_contig()) #Necessary to remove 'islands' as causes problems
    return(state_nb2)
  })
  
  state_nb2listw <- reactive({
    beginning <- Sys.time()
    if(is.null(state_nb2())){
      return(NULL)
    }
    state_nb2listw <- nb2listw(state_nb2(),zero.policy=TRUE)
    #state_nb2listw <- nb2listw(state_nb2,zero.policy=TRUE)
    end <- Sys.time()
    cat("Time for identifying neightbours list: ",end-beginning,".\n")
    return(state_nb2listw)
  })
  
  dz5 <- reactive({
    beginning <- Sys.time()
    
    ## Reactive events
    
    dz4 <- dz3()
    
    ## Break
    if(is.null(dz4)){
      return(NULL)
    }
    state_nb2listw <- isolate(state_nb2listw())
    
    #dz4 <- dz3
    
    lisa <- as.data.frame(localmoran(dz4$LQ,state_nb2listw))
    
    dz4$LISA_I <- lisa[,"Ii"]
    dz4$LISA_p <- lisa[,"Pr(z > 0)"]
    dz4$LQ_stdzd <- as.vector(scale(dz4$LQ))
    dz4$LQ_stdzd_lag <- lag.listw(state_nb2listw,dz4$LQ_stdzd, NAOK=TRUE) #NAOK here helps or hinders?
    
    dz4$category <- "Insignificant"
    dz4$category[dz4$LISA_p<0.05 & dz4$LQ_stdzd>=0 & dz4$LQ_stdzd_lag>=0] <- "High-High"
    dz4$category[dz4$LISA_p<0.05 & dz4$LQ_stdzd>=0 & dz4$LQ_stdzd_lag<=0] <- "High-Low"
    dz4$category[dz4$LISA_p<0.05 & dz4$LQ_stdzd<=0 & dz4$LQ_stdzd_lag>=0] <- "Low-High"
    dz4$category[dz4$LISA_p<0.05 & dz4$LQ_stdzd<=0 & dz4$LQ_stdzd_lag<=0] <- "Low-Low"
    dz4$category <- as.factor(dz4$category)
    end <- Sys.time()
    print(c("Time to calculate Moran's I and LISA: ",end-beginning))
    dz5 <- dz4
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Positron)
  })
  
  state_shp <- reactive({
    uf <- input$State
    if(uf == "")
      uf <- "br"
    state_shp <- readr::read_rds(paste0("data/shape_states/", uf,".rds"))
  })
  
  observe({
    uf <- input$State
    
    geo <- as.numeric(st_bbox(state_shp()))
    
    ### Base Map ###
    
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>% 
      addPolygons(data = state_shp(),
                  fillOpacity  = 0,
                  weight       = 3,
                  color        = "black",
                  fillColor    = NULL) %>% 
      flyToBounds(geo[3], geo[4], geo[1], geo[2])
  })
  
  observe({
    dz5_use <- dz5()
    
    if(is.null(dz5_use)){
      return(NULL)
    }
    
    proxy <- leafletProxy("map")
    
    proxy %>% 
      clearShapes() %>% 
      addPolygons(data  = state_shp(),
                  color = "black",
                  fillColor = NULL,
                  fillOpacity = 0)
    
    if (input$Indicator == "Medida QL"){
      pal <- colorBin(palette  = c("white","light blue","#fcbba1","#fb6a4a","#ef3b2c","#cb181d"),
                      domain   = c(0,1000),
                      bins     = c(0,0.01,1,5,10,50,1000),
                      na.color = "#cb181d")
    } else if(input$Indicator == "1") {
      pal <- colorNumeric(palette  = c("white","red"),
                          domain   = c(0,max(dz5_use@data[["Cand_Vote_Share"]],na.rm=TRUE)),
                          na.color = "white")
    } else {
      pal <- colorNumeric(palette  = c("white","red"),
                          domain   = c(0,max(dz5_use@data[["Mun_Vote_Share"]],na.rm=TRUE)),
                          na.color = "white")
    }
    
    popup_text <- paste0("<h4>", dz5_use@data[,"NOME"], "</h2>",
                         "</br>",
                         dz5_use@data[,"NOME_URNA_CANDIDATO"],
                         " recebeu ",
                         "<strong>", dz5_use@data[,"QTDE_VOTOS"], "</strong>",
                         " votos (",
                         round((dz5_use@data[,"QTDE_VOTOS"] / dz5_use@data[,"Tot_Deputado"])*100,1),
                         "% do total recebido pelo candidato(a) no estado). </br>",
                         "</br> Votos válidos no município: ",
                         dz5_use@data[,"Tot_Mun"],
                         " (",
                         round((dz5_use@data[,"Tot_Mun"] / dz5_use@data[,"Tot_State"])*100,1),
                         "% do total do Estado).",
                         "<br>",
                         "<br> Medida QL: ", round(dz5_use@data[,"LQ"],3))
    
    popup_text_hihi <- paste0("<h4>", dz5_use@data[dz5_use@data$category=="High-High","NOME"], "</h4>",
                              dz5_use@data[,"NOME_URNA_CANDIDATO"],
                              " recebeu ",
                              dz5_use@data[dz5_use@data$category=="High-High","QTDE_VOTOS"],
                              " votos (",
                              round((dz5_use@data[dz5_use@data$category=="High-High","QTDE_VOTOS"]/dz5_use@data[dz5_use@data$category=="High-High","Tot_Deputado"])*100,1),
                              "% do total recebido pelo candidato(a) no estado)",
                              "</br> </br> Votos válidos no município: ",
                              dz5_use@data[dz5_use@data$category=="High-High","Tot_Mun"],
                              " (",
                              round((dz5_use@data[dz5_use@data$category=="High-High","Tot_Mun"]/dz5_use@data[dz5_use@data$category=="High-High","Tot_State"])*100,1),
                              "% do total do Estado)",
                              "<br>",
                              "<br> Medida QL: ",
                              round(dz5_use@data[dz5_use@data$category=="High-High","LQ"],3))
    
    proxy %>% 
      clearControls() %>% 
      addPolygons(data         = dz5_use,
                  fillOpacity  = 0.8,
                  weight       = 0.1,
                  color        = "black",
                  fillColor    = pal(dz5_use@data[[switch(input$Indicator,"2"="Mun_Vote_Share",
                                                          "Medida QL" = "LQ",
                                                          "1"         = "Cand_Vote_Share")]]),
                  popup        = popup_text) %>%
      addLegend(title          = switch(input$Indicator,
                                        "Medida QL" = "Medida QL",
                                        "2" = "% Votos no <br>Município",
                                        "1"                  = "% Votos do(a)<br>Candidato(a)"),
                pal            = pal,
                values         = dz5_use@data[[switch(input$Indicator,"2"="Mun_Vote_Share",
                                                      "Medida QL"="LQ",
                                                      "1"         = "Cand_Vote_Share")]],
                opacity        = 0.8,
                labFormat      = labelFormat(suffix = ifelse(input$indicator=="Medida QL","","%")))  %>%
      addPolygons(data         = dz5_use[dz5_use@data$category=="High-High",],
                  fillOpacity  = 0,
                  weight       = 2,
                  color        = "green",
                  stroke       = TRUE,
                  popup        = popup_text_hihi)
  })
  
  ### Map for Download ###
  
  map_reactive <- eventReactive(input$button, {
    dz5_use <- dz5()
    
    if(is.null(dz5_use)){
      return(NULL)
    }
    
    geo <- as.numeric(st_bbox(state_shp()))
    
    if (input$Indicator == "Medida QL"){
      pal <- colorBin(palette  = c("white","light blue","#fcbba1","#fb6a4a","#ef3b2c","#cb181d"),
                      domain   = c(0,1000),
                      bins     = c(0,0.01,1,5,10,50,1000),
                      na.color = "#cb181d")
    } else if(input$Indicator == "1") {
      pal <- colorNumeric(palette  = c("white","red"),
                          domain   = c(0,max(dz5_use@data[["Cand_Vote_Share"]],na.rm=TRUE)),
                          na.color = "white")
      
      pal <- colorBin(palette  = c("white","#fcbba1","#fc9272","#fb6a4a","#ef3b2c"),
                      domain   = quantile(dz5_use@data[["Cand_Vote_Share"]],probs=c(0,0.2,0.4,0.6,0.8,1),na.rm=T),
                      na.color = "white")
    } else {
      pal <- colorNumeric(palette  = c("white","red"),
                          domain   = c(0,max(dz5_use@data[["Mun_Vote_Share"]],na.rm=TRUE)),
                          na.color = "white")
      
      pal <- colorBin(palette  = c("white","#fcbba1","#fc9272","#fb6a4a","#ef3b2c"),
                      domain   = quantile(dz5_use@data[["Mun_Vote_Share"]],probs=c(0,0.2,0.4,0.6,0.8,1),na.rm=T),
                      na.color = "white")
    }
    
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(data = state_shp(),
                  fillOpacity  = 0,
                  weight       = 3,
                  color        = "black",
                  fillColor    = NULL) %>% 
      flyToBounds(geo[3], geo[4], geo[1], geo[2]) %>% 
      addPolygons(data         = dz5_use,
                  fillOpacity  = 0.8,
                  weight       = 0.1,
                  color        = "black",
                  fillColor    = pal(dz5_use@data[[switch(input$Indicator,"2"="Mun_Vote_Share",
                                                          "Medida QL" = "LQ",
                                                          "1"         = "Cand_Vote_Share")]])) %>%
      addLegend(title          = switch(input$Indicator,
                                        "Medida QL" = "Medida QL",
                                        "2" = "% Votos no <br>Município",
                                        "1"                  = "% Votos do(a)<br>Candidato(a)"),
                pal            = pal,
                values         = dz5_use@data[[switch(input$Indicator,"2"="Mun_Vote_Share",
                                                      "Medida QL"="LQ",
                                                      "1"         = "Cand_Vote_Share")]],
                opacity        = 0.8,
                labFormat      = labelFormat(suffix = ifelse(input$indicator=="Medida QL","","%")))  %>%
      addPolygons(data         = dz5_use[dz5_use@data$category=="High-High",],
                  fillOpacity  = 0,
                  weight       = 2,
                  color        = "green",
                  stroke       = TRUE)
  })
  
  output$downloadMap <- downloadHandler(
    filename = function () {
      return(paste0(paste("CepespData",
                    input$Year, 
                    input$State, 
                    input$cargo,
                    "Turno", 
                    input$turno_value, 
                    input$Party, 
                    unique(dz5()@data[,"NOME_URNA_CANDIDATO"]), 
                    sep="_"),
              ".png"))
    },
    content = function(file) {
      mapshot( x = map_reactive()
               , file = file
               , selfcontained = FALSE # when this was not specified, the function for produced a PDF of two pages: one of the leaflet map, the other a blank page.
      )
    }
  )
  
  ### End ###
  
  clusters <- reactive({
    dz5_HH <- dz5()[dz5()$category=="High-High",]
    #dz5_HH <- dz5[dz5$category=="High-High",]
    if (dim(dz5_HH)[1]!=0){
      clusters <- gUnion(dz5_HH,dz5_HH)  
    } else {
      clusters <- NULL
    }
  })
  
  clusters_sp <- reactive({
    if (!(is.null(clusters()))){
      clusters_sep <- slot(clusters()@polygons[[1]],"Polygons")
      #clusters_sep <- slot(clusters@polygons[[1]],"Polygons")
      
      clusters_sep <- clusters_sep[unlist(lapply(clusters_sep, function(x) x@hole==FALSE))] #Have to make sure aren't picking up holes too!
      polygons_list <- list()
      for (i in 1:length(clusters_sep)){
        polygons_list[[i]] <- Polygons(list(clusters_sep[[i]]),"test")  
        polygons_list[[i]]@ID <- paste0(i)
      }
      clusters_sp <- SpatialPolygons(polygons_list)
    }
  })
  
  clusters_sp_cent_table <- reactive({
    if (!(is.null(clusters()))){
      clusters_sp_cent <- gCentroid(clusters_sp(),byid=TRUE)
      #clusters_sp_cent <- gCentroid(clusters_sp,byid=TRUE)
      clusters_sp_cent_table_temp <- as.data.frame(clusters_sp_cent@coords)
      clusters_sp_cent_table_temp$Cluster_num <- rownames(clusters_sp_cent_table_temp)
      clusters_sp_cent_table <- clusters_sp_cent_table_temp
    }
  })
  
  clusters_list <- reactive({
    if (!(is.null(clusters()))){
      clusters_list_temp <- list()
      num_clust <- length(clusters_sp())
      #num_clust <- length(clusters_sp)
      
      for (i in 1:num_clust){
        clusters_list_temp[[i]] <- raster::intersect(dz5()[dz5()$category=="High-High",],clusters_sp()[i])
        #clusters_list_temp[[i]] <- raster::intersect(dz5[dz5$category=="High-High",],clusters_sp[i])
        clusters_list_temp[[i]]@data$Cluster_Num <- i
      }
      clusters_list <- clusters_list_temp
    }
  })
  
  output$Num_clusters <- renderUI({
    if (!(is.null(clusters()))){
      Num_clusters <- paste0("<b> Number of High-High Clusters: ",length(clusters_list()),"<b>")
      #Num_clusters <- paste0("<b> Number of High-High Clusters: ",length(clusters_list),"<b>")
      HTML(Num_clusters)
    } else {
      HTML(paste0("<b> No clusters <b>"))
    }
  })
  
  cluster_table <- reactive({
    if (!(is.null(clusters()))){
      clusters_table_temp <- rbind.fill(lapply(clusters_list(),slot,'data'))
      #clusters_table_temp <- rbind.fill(lapply(clusters_list,slot,'data'))
      clusters_table_temp$pct_votes_from_mun <- clusters_table_temp$QTDE_VOTOS/clusters_table_temp$Tot_Deputado
      clusters_table_temp <- clusters_table_temp[,c("Cluster_Num","NOME","QTDE_VOTOS","pct_votes_from_mun","LQ")]
      clusters_table_temp$pct_votes_from_mun <- round(clusters_table_temp$pct_votes_from_mun*100,1)
      clusters_table_temp$LQ <- round(clusters_table_temp$LQ,1)
      cluster_table <- clusters_table_temp
    }
  })
  
  output$Clusters <- renderDataTable({
    if (!(is.null(clusters()))){
      table_temp <- cluster_table()
      colnames(table_temp) <- c("Cluster Number","Municipality","Votes","% Candidate Votes","LQ")
      table_temp[,"Votes"] <- round(table_temp[,"Votes"],0)
      table_temp[,"% Candidate Votes"] <- round(table_temp[,"% Candidate Votes"],1)
      Clusters <- as.data.table(table_temp)
      datatable(Clusters, rownames=TRUE, options=list(dom = 't'), selection='single', style = 'bootstrap', class = 'table-bordered')
    }
  })
  
  output$Clusters_agg <- renderDataTable({
    if (!(is.null(clusters()))){
      agg <- as.data.frame(as.data.table(cluster_table())[,.(sum(QTDE_VOTOS), sum(pct_votes_from_mun)),by=Cluster_Num])
      #agg <- as.data.frame(as.data.table(cluster_table)[,.(sum(QTDE_VOTOS), sum(pct_votes_from_mun)),by=Cluster_Num])
      colnames(agg) <- c("Cluster Number","Total Votes Received","Total % Candidate Votes")
      agg[,"Total Votes Received"] <- round(agg[,"Total Votes Received"],0)
      agg[,"Total % Candidate Votes"] <- round(agg[,"Total % Candidate Votes"],1)
      Clusters_agg <- as.data.table(agg)
      datatable(Clusters_agg, rownames=TRUE, options=list(dom = 't'), selection='single', style = 'bootstrap', class = 'table-bordered')
    }
  })
  
  output$map_clusters <- renderLeaflet({
    pal <- colorBin(palette=c("white","light blue","#fcbba1","#fb6a4a","#ef3b2c","#cb181d"),domain=c(0,1000), bins=c(0,0.01,1,5,10,50,1000), na.color="white")
    
    if (!(is.null(clusters()))){
      leaflet() %>% 
        addProviderTiles(providers$CartoDB.Positron) %>% 
        clearBounds() %>% 
        addPolygons(data=state_shp(),fillOpacity=0,weight=3,color="black",fillColor=NULL) %>% 
        addPolygons(data=dz5()[dz5()@data$category=="High-High",], layerId=dz5()@data[dz5()@data$category=="High-High",],fillOpacity=0,weight=3,color="green",stroke=TRUE) %>% 
        addMarkers(data=clusters_sp_cent_table(),~x,~y,label = ~Cluster_num,labelOptions = labelOptions(noHide = T, textOnly = FALSE,textsize="25px"))
    } else {
      leaflet() %>% 
        addProviderTiles(providers$CartoDB.Positron) %>% 
        clearBounds() %>% 
        addPolygons(data=state_shp(),fillOpacity=0,weight=3,color="black",fillColor=NULL)
    }
  })
  
  d_G <- reactive({
    d_local <- d()
    d_G <- d_local[,unique(G_Index),by=.(UF,NUMERO_CANDIDATO,NOME_URNA_CANDIDATO,NUMERO_PARTIDO,DESC_SIT_TOT_TURNO)]
  })
  
  output$Result <- renderUI({
    if(is.null(dz3())){
      output_error <- "Por favor, informe os parâmetros <b>estado</b>, <b>cargo</b>, <b>ano</b>, <b>partido</b> e <b>candidato</b> antes atualizar o mapa."
      return(HTML(output_error))
    }
    
    str_Result <- paste0("<b>Resultado: </b>: ",
                         unique(dz3()@data$DESC_SIT_TOT_TURNO[is.na(dz3()@data$DESC_SIT_TOT_TURNO)==FALSE]),
                         "<br><b>Votos: </b>",unique(dz3()@data$Tot_Deputado[is.na(dz3()@data$Tot_Deputado)==FALSE]),
                         "<br><b>Porcentagem dos votos válidos: </b>",round((unique(dz3()@data$Tot_Deputado[is.na(dz3()@data$Tot_Deputado)==FALSE])/unique(dz3()@data$Tot_State[is.na(dz3()@data$Tot_State)==FALSE]))*100,1), "%")
    HTML(str_Result)
  })
  
  output$G_Index <- renderUI({
    
    str_G_Index <- paste0("<h4>Estatísticas Geoespaciais: </h4><b>Índice G:</b> ",round(unique(dz3()@data$G_Index[is.na(dz3()@data$G_Index)==FALSE]),3))
    HTML(str_G_Index)
  })
  
  moran_I <- reactive({
    dz3_local <- dz3()
    state_nb2_local <- state_nb2()
    state_nb2listw_local <- state_nb2listw()
    moran_I <- moran(dz3_local$LQ,state_nb2listw_local,n=length(state_nb2_local),Szero(state_nb2listw_local),zero.policy=TRUE,NAOK=TRUE)$I  
  })
  
  output$chart_LQ <- renderPlot({
    dz3_local <- dz3()
    ggplot() +
      geom_density(data=dz3_local@data,aes(x=LQ),fill="light blue",colour=NA,alpha=0.5) +
      xlab("Log of Medida QL") +
      theme_classic() +
      ylab("Density") +
      scale_x_log10()
  })
  
  output$chart_scatter <- renderPlot({
    dz3_local <- dz3()
    ggplot() +
      geom_point(aes(x=dz3_local@data$Tot_Mun,y=dz3()@data$LQ),color="dark green") +
      xlab("Log of Municipal Voting Population") +
      ylab("Medida QL") +
      theme_classic() +
      scale_x_log10()
  })
  
  d_stats_cut <- reactive({
    if (input$Cut=="All"){
      d_stats_cut <- d_stats
    } else if (input$Cut=="Selected Year") {
      d_stats_cut <- d_stats[d_stats$ANO_ELEICAO==input$Year,]
    } else if (input$Cut=="Selected State") {
      d_stats_cut <- d_stats[d_stats$UF==input$State,]
    } else if (input$Cut=="Selected Party") {
      d_stats_cut <- d_stats[as.numeric(substr(d_stats$NUMERO_CANDIDATO,1,2))==input$Party,]
    }
    d_stats_cut <-  d_stats_cut %>% mutate(Eleito=ifelse(DESC_SIT_TOT_TURNO %in% c("ELEITO","ELEITO POR MEDIA","ELEITO POR QP"),"Eleito","Nao Eleito"))
    d_stats_cut
  })
  
  output$G_cand <- renderPlot({
    d_stats_cut_local <- d_stats_cut()
    
    
    dz3_local <- dz3()
    ##Check categories for winner here
    ggplot() + geom_density(data=d_stats_cut_local,aes(x=G_Index,fill=Eleito),colour=NA, alpha=0.5) +
      xlab("G Index") + 
      theme_classic() + 
      ylab("Density") +
      geom_vline(xintercept=unique(dz3_local@data$G_Index[is.na(dz3_local@data$G_Index)==FALSE]),lty=2) + 
      theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),legend.text=element_text(size=))
  })
  
  output$I_cand <- renderPlot({
    d_stats_cut_local <- d_stats_cut()
    moran_I_local <- moran_I()
    ggplot() +
      geom_density(data=d_stats_cut_local,aes(x=Moran_I,fill=Eleito),colour=NA,alpha=0.5) +
      xlab("Moran's I") +
      ylab("Density") +
      theme_classic() +
      geom_vline(xintercept=moran_I_local,lty=2) +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"),
            legend.text=element_text(size=12))
  })
  
  output$QL_dist <- renderPlot({
    d_local <- d()
    
    ggplot() + geom_density(data=as.data.frame(d_local),aes(x=LQ), color=NA, fill="#2ca25f", alpha=0.5, na.rm=T) +
      geom_vline(xintercept=1,lty=2) +
      xlim(0,2) +
      xlab("QL") + 
      theme_classic() + 
      ylab("Density") +
      theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),legend.text=element_text(size=)) +
      ggtitle("Distribuição do QL do candidato selecionado")
  })
  
  output$Note <- renderUI({
    note <- paste0("<font size='3'> Os mapas eleitorais foram desenvolvidos no âmbito do projeto temático da FAPESP (processo # 2013/15658-1), sob coordenação do Prof. George Avelino. Eles utilizam os dados do TSE coletados e limpos pela equipe do <a href='http://cepesp.io/'> CEPESPData</a>. Desenvolvido por Jonathan Phillips e Rafael de Castro Coelho Silva com apoio dos pesquisadores do CEPESP. </font>")
    HTML(note)
  })
  
  output$moran <- renderUI({
    str_moran <- paste0("<b> Moran's I: </b>", round(moran_I(),3))
    HTML(str_moran)
  })
  
  output$quadrant <- renderPlot({
    
    ggplot() + 
      geom_point(data=d_stats[d_stats$CODIGO_CARGO==input$cargo & d_stats$ANO_ELEICAO==input$Year & d_stats$UF==input$State,],aes(x=G_Index,y=Moran_I,size=Tot_Deputado),color="blue",alpha=0.2) + 
      geom_point(data=d_stats[d_stats$CODIGO_CARGO==input$cargo & d_stats$NUMERO_CANDIDATO!=input$candidato & d_stats$ANO_ELEICAO==input$Year & d_stats$UF==input$State & as.numeric(substr(d_stats$NUMERO_CANDIDATO,1,2))==as.numeric(substr(input$candidato,1,2)),],aes(x=G_Index,y=Moran_I,size=Tot_Deputado),color="red",alpha=0.8) + 
      geom_point(data=d_stats[d_stats$CODIGO_CARGO==input$cargo & d_stats$NUMERO_CANDIDATO==input$candidato & d_stats$ANO_ELEICAO==input$Year & d_stats$UF==input$State,],aes(x=G_Index,y=Moran_I,size=Tot_Deputado),color="dark green",alpha=1) + 
      theme_classic() + 
      geom_vline(xintercept=median(d_stats[d_stats$CODIGO_CARGO==input$cargo & d_stats$ANO_ELEICAO==input$Year & d_stats$UF==input$State,"G_Index"][[1]],na.rm=TRUE),lty=2) +
      geom_hline(yintercept=median(d_stats[d_stats$CODIGO_CARGO==input$cargo & d_stats$ANO_ELEICAO==input$Year & d_stats$UF==input$State,"Moran_I"][[1]],na.rm=TRUE),lty=2) +
      theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),legend.text=element_text(size=12)) + 
      xlab("G Index") + 
      ylab("Moran's I")
  })
  
  mouse <- reactive({
    if (is.null(input$plot_click)){
      mouse_temp  <- d_stats[d_stats$ANO_ELEICAO==input$Year & d_stats$UF==input$State & d_stats$NUMERO_CANDIDATO==input$candidato,][1,]
    } else {
      mouse_temp <- nearPoints(d_stats[d_stats$ANO_ELEICAO==input$Year & d_stats$UF==input$State,],input$plot_click,threshold=20,maxpoints=1)
    }
    mouse <- mouse_temp
  })
  
  G_Quadrant <- reactive({
    mouse_local <- mouse()
    if(d_stats[d_stats$NUMERO_CANDIDATO==mouse$NUMERO_CANDIDATO & d_stats$ANO_ELEICAO==input$Year & d_stats$UF==input$State & as.numeric(substr(d_stats$NUMERO_CANDIDATO,1,2))==as.numeric(substr(mouse$NUMERO_CANDIDATO,1,2)),"G_Index"][[1]]>
       median(d_stats[d_stats$ANO_ELEICAO==input$Year & d_stats$UF==input$State,"G_Index"][[1]],na.rm=TRUE)){
      G_Quadrant_temp <-"above"
    } else {
      G_Quadrant_temp <- "below"
    }
    G_Quadrant <- G_Quadrant_temp
  })
  
  G_desc <- reactive({
    mouse_local <- mouse()
    if(d_stats[d_stats$NUMERO_CANDIDATO==mouse$NUMERO_CANDIDATO & d_stats$ANO_ELEICAO==input$Year & d_stats$UF==input$State & as.numeric(substr(d_stats$NUMERO_CANDIDATO,1,2))==as.numeric(substr(mouse$NUMERO_CANDIDATO,1,2)),"G_Index"][[1]]>
       median(d_stats[d_stats$ANO_ELEICAO==input$Year & d_stats$UF==input$State,"G_Index"][[1]],na.rm=TRUE)){
      G_desc_temp <-"concentrated"
    } else {
      G_desc_temp <- "diffuse"
    }
    G_desc <- G_desc_temp
  })
  
  Moran_Quadrant <- reactive({
    mouse_local <- mouse()
    if(d_stats[d_stats$NUMERO_CANDIDATO==mouse$NUMERO_CANDIDATO & d_stats$ANO_ELEICAO==input$Year & d_stats$UF==input$State & as.numeric(substr(d_stats$NUMERO_CANDIDATO,1,2))==as.numeric(substr(mouse$NUMERO_CANDIDATO,1,2)),"Moran_I"][[1]]>
       median(d_stats[d_stats$ANO_ELEICAO==input$Year & d_stats$UF==input$State,"Moran_I"][[1]],na.rm=TRUE)){
      Moran_Quadrant_temp <-"above"
    } else {
      Moran_Quadrant_temp <- "below"
    }
    Moran_Quadrant <- Moran_Quadrant_temp
  })
  
  Moran_desc <- reactive({
    mouse_local <- mouse()
    if(d_stats[d_stats$NUMERO_CANDIDATO==mouse$NUMERO_CANDIDATO & d_stats$ANO_ELEICAO==input$Year & d_stats$UF==input$State & as.numeric(substr(d_stats$NUMERO_CANDIDATO,1,2))==as.numeric(substr(mouse$NUMERO_CANDIDATO,1,2)),"Moran_I"][[1]]>
       median(d_stats[d_stats$ANO_ELEICAO==input$Year & d_stats$UF==input$State,"Moran_I"][[1]],na.rm=TRUE)){
      Moran_desc_temp <-"contiguous"
    } else {
      Moran_desc_temp <- "dispersed"
    }
    Moran_desc <- Moran_desc_temp
    
  })
  
  mouse_cand <- reactive({
    mouse_local <- mouse()
    party_template_2 <- readr::read_rds("data/party_template.rds") %>% 
      ungroup()
    #Lacking NOME_URNA_CANDIDATO
    mouse_cand <- party_template_2 %>% filter(ANO_ELEICAO==input$Year & 
                                                CODIGO_CARGO==input$cargo & 
                                                NUM_TURNO==turno) %>%
      unnest() %>%
      filter(LISTA_NUMERO==input$candidato) %>%
      pull(LISTA_NOMES) %>%
      unique()
    
    #mouse_cand <- d_stats[d_stats$NUMERO_CANDIDATO==d_stats$NUMERO_CANDIDATO & d_stats$ANO_ELEICAO==input$Year & d_stats$UF==input$State,"NOME_URNA_CANDIDATO"][[1]]
  })
  
  classify_note_text <- reactive({
    classify_note_text <- paste0("Each point represents a <font color=\"blue\"> Candidate </font> in this election, with the size proportionate to their total number of votes received. <br> <font color=\"red\"> Red </font> points indicate votes for the selected party. <br> The <font color=\"green\"> Green </font> point is the selected candidate. <br> <br>")
  })
  
  classify_note_text_2 <- reactive({
    classify_note_text_2 <- paste0("The currently selected candidate (on the chart above), ", mouse_cand() ," has a G-Index <b>",  G_Quadrant(),"</b> the median and a Moran's I <b>", Moran_Quadrant(), "</b> the median, indicating that the candidate's support is more <b>",G_desc(),"</b> and <b>",Moran_desc(),"</b> than average.")
  })
  
  output$Classify_Note <- renderUI({
    Classify_Note <- HTML(classify_note_text())
  })
  
  output$Classify_Note2 <- renderUI({
    Classify_Note2 <- HTML(classify_note_text_2())
  })
  
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(d_stats[d_stats$ANO_ELEICAO==input$Year & d_stats$UF==input$State,], hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(255, 255, 255, 0); border-color: rgba(255, 255, 255, 0); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    Cand_name <- party_template %>% unnest() %>% 
      filter(LISTA_NUMERO==point$NUMERO_CANDIDATO & ANO_ELEICAO==input$Year & SIGLA_UF==input$State & CODIGO_CARGO==input$cargo) %>%
      pull(LISTA_NOMES)
    
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0(Cand_name)))
    )
  })
  
  filtered <- reactive({
    filtered <- d_stats %>% 
      filter(ANO_ELEICAO==input$Year,UF==input$State & CODIGO_CARGO==input$cargo) %>% 
      distinct(G_Index,Moran_I,NUMERO_CANDIDATO,NOME_URNA_CANDIDATO) %>% 
      top_n(5,G_Index)
    
    filtered_low <- d_stats %>% 
      filter(ANO_ELEICAO==input$Year,UF==input$State & CODIGO_CARGO==input$cargo) %>% 
      distinct(G_Index,Moran_I,NUMERO_CANDIDATO,NOME_URNA_CANDIDATO) %>% 
      top_n(-5,G_Index)
    
    filtered <- rbind(filtered,filtered_low) %>%
      mutate(NUMERO_PARTIDO=as.numeric(substr(NUMERO_CANDIDATO,1,2)))
    
    parties <- data.frame(Party=c("PRB","PP","PDT","PT","PTB","PMDB","PSTU","PSL","REDE","PTN","PSC","PCB","PR","PPS","DEM","PSDC","PRTB","PCO","NOVO","PHS","PMN","PMB","PTC","PSB","PV","PRP","PSDB","PSOL","PEN","PPL","PSD","PCdoB","PTdoB","SD","PROS"),
                          NUMERO_PARTIDO=c(10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,27,28,29,30,31,33,35,36,40,43,44,45,50,51,54,55,65,70,77,90))
      
    filtered <- merge(filtered,parties,by="NUMERO_PARTIDO",all.x=TRUE,all.y=FALSE)
    
    filtered <- as.data.table(filtered)
    filtered <- filtered  %>% arrange(desc(G_Index)) %>% dplyr::select(NUMERO_CANDIDATO,NOME_URNA_CANDIDATO,Party,G_Index,Moran_I)
    setnames(filtered,c("NUMERO_CANDIDATO","NOME_URNA_CANDIDATO","Party","G_Index","Moran_I"),c("NUMERO_CANDIDATO","Candidate Name","Party","G Index","Moran's I"))
    setcolorder(filtered,c("NUMERO_CANDIDATO","Candidate Name","Party","G Index","Moran's I"))
    filtered
  })
  
  output$Extremes <- renderDataTable({
    filtered_local <- filtered()
    #filtered2 <- filtered_local[,!names(filtered_local)=="Candidate Number"]
    
    datatable(filtered_local, 
              rownames=TRUE, 
              options=list(dom = 't',
                           columnDefs = list(list(visible=FALSE, targets=c(1)))),
              selection='single', 
              style = 'bootstrap', 
              class = 'table-bordered') %>% 
      formatRound(c("G Index","Moran's I"),3) %>% 
      formatStyle('G Index',target='row',
                  backgroundColor=styleInterval(median(filtered_local[['G Index']],na.rm=TRUE),
                                                c('lightblue','lightgreen')))
  })
  
  candidato_hi <- eventReactive(input$Extremes_row_last_clicked,{
    filtered_local <- filtered()
    candidato_hi <- filtered_local[input$Extremes_row_last_clicked,"NUMERO_CANDIDATO"]
    candidato_hi
  })
  
  d_hi <- eventReactive(input$Extremes_row_last_clicked,{
    beginning <- Sys.time()
    candidato_hi_local <- candidato_hi()
    candidatos_list <- candidatos_values()
    
    cname <- candidato_name(candidatos_list, candidato)
    print(cname)
    
    d <- db_get_elections(year = as.numeric(input$Year), position = as.numeric(input$cargo), candidate_number = as.numeric(candidato_hi_local), 
                          state = input$State, turn = turno(), name = cname)
    #print(candidato_hi_local)
    print(d)
    d <- data.table(d)
    
    
    if (dim(d)[1]!=0){
      
      #Ideally will be faster when can request specific state
      setkeyv(d,c('ANO_ELEICAO','COD_MUN_IBGE','NUMERO_CANDIDATO'))
      
      #### Aggregations
      d <- merge(d,mun_totals(),by="COD_MUN_IBGE")
      #d <- merge(d,mun_totals,by="COD_MUN_IBGE")
      d <- merge(d,state_totals(),by="UF")
      #d <- merge(d,state_totals,by="UF")
      
      d[,Tot_Deputado := sum(QTDE_VOTOS),by=.(ANO_ELEICAO,UF,NUMERO_CANDIDATO)]
      d[,Mun_Vote_Share := QTDE_VOTOS/Tot_Mun]
      
      #### G-Index Calcs
      d[,G_temp := (QTDE_VOTOS/Tot_Deputado - Tot_Mun/Tot_State)^2]
      d[,G_Index := sum(G_temp),by=.(ANO_ELEICAO,UF,NUMERO_CANDIDATO)] #Correct? CHECK
      
      #### LQ Calcs
      d[,LQ := (QTDE_VOTOS/Tot_Deputado)/(Tot_Mun/Tot_State),by=.(ANO_ELEICAO,UF,NUMERO_CANDIDATO)] #Correct?
      
      #Remove NULO line from selectable candidates, though is included in calculations of total statewide and municipal votes above
      d <- d[NOME_URNA_CANDIDATO!="#NULO#"]
    } else {
      d <- data.table("UF"=character(),"NUMERO_PARTIDO"=integer(),"ANO_ELEICAO"=integer(),"COD_MUN_IBGE"=integer(),"QTDE_VOTOS"=integer(),"NUMERO_CANDIDATO"=integer(),"SIGLA_PARTIDO"=character(),"NOME_URNA_CANDIDATO"=character(),"DESC_SIT_TOT_TURNO"=character())
    }
    end <- Sys.time()
    print(c("Load Candidate Data Time: ",end-beginning))
    d_hi <- d
    print(d_hi)
    d_hi
  })
  
  extreme_d <- reactive({
    d_hi_local <- d_hi()
    candidato_hi_local <- candidato_hi()
    mun_state_contig_local <- mun_state_contig()
    #candidato_hi_local <- 1010
    
    #extreme_d_temp <- d_hi_local[NUMERO_CANDIDATO==candidato_hi_local]
    extreme_d_temp <- d_hi_local %>% as.data.frame() %>% filter(NUMERO_CANDIDATO==candidato_hi_local)
    
    print(extreme_d_temp)
    extreme_d_temp <- merge(mun_state_contig_local,
                            extreme_d_temp, 
                            by.x="GEOCOD",
                            by.y="COD_MUN_IBGE",
                            all.x=TRUE)
    
    extreme_d <- extreme_d_temp
  })
  
  output$map_selected_hi <- renderLeaflet({
    extreme_d_local <- extreme_d()
    state_shp_local <- state_shp()
    
    pal <- colorBin(palette=c("white","light blue","#fcbba1","#fb6a4a","#ef3b2c","#cb181d"),domain=c(0,1000), bins=c(1000,50,10,5,1,0.01,0), na.color="white")
    popup_text <- paste0(extreme_d_local@data[,"NOME"],
                         "<br> Valid Votes: ",
                         extreme_d_local@data[,"Tot_Mun"],
                         " (",
                         round((extreme_d_local@data[,"Tot_Mun"]/extreme_d_local@data[,"Tot_State"])*100,1),
                         "% of State Total)","<br>",
                         extreme_d_local@data[,"NOME_URNA_CANDIDATO"],
                         " received ",
                         extreme_d_local@data[,"QTDE_VOTOS"],
                         " votes (",
                         round((extreme_d_local@data[,"QTDE_VOTOS"]/extreme_d_local@data[,"Tot_Deputado"])*100,1),
                         "% of their Statewide Total)",
                         "<br> Medida QL: ",
                         round(extreme_d_local@data[,"LQ"],3))
    
    leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
      clearBounds() %>% 
      addPolygons(data=state_shp_local,fillOpacity=0,weight=3,color="black",fillColor=NULL) %>% 
      addPolygons(data=extreme_d_local, layerId=extreme_d_local@data[,"LQ"],
                  fillOpacity=0.8,weight=0.1,color=NA,
                  fillColor=pal(extreme_d_local@data[,"LQ"]), 
                  popup=popup_text) %>% 
      addLegend(position="bottomleft", pal=pal,
                values=extreme_d_local@data[,"LQ"],opacity=0.8) 
  })
  
  observeEvent(input$map_zoom_out ,{
    leafletProxy("map") %>% 
      setView(lat  = (input$map_bounds$north + input$map_bounds$south) / 2,
              lng  = (input$map_bounds$east + input$map_bounds$west) / 2,
              zoom = input$map_zoom - 1)
  })
  # Zoom control - zoom in
  observeEvent(input$map_zoom_in ,{
    leafletProxy("map") %>% 
      setView(lat  = (input$map_bounds$north + input$map_bounds$south) / 2,
              lng  = (input$map_bounds$east + input$map_bounds$west) / 2,
              zoom = input$map_zoom + 1)
  })
  
  output$Indicators1 <- renderUI({
    str_Result <- HTML(paste0("<b>Resultado: </b>: ",
                              unique(dz3()@data$DESC_SIT_TOT_TURNO[is.na(dz3()@data$DESC_SIT_TOT_TURNO)==FALSE]),
                              "<br><b>Votos válidos: </b>",format(unique(dz3()@data$Tot_Deputado[is.na(dz3()@data$Tot_Deputado)==FALSE]),big.mark=" "),
                              "<br><b>% dos votos: </b>",round((unique(dz3()@data$Tot_Deputado[is.na(dz3()@data$Tot_Deputado)==FALSE])/unique(dz3()@data$Tot_State[is.na(dz3()@data$Tot_State)==FALSE]))*100,1), "%",
                              "<h4>Estatísticas Geoespaciais: </h4>"))
  })
  
  output$Indicators2 <- renderUI({
    str_G_Index <- HTML(paste0("<b>Índice G:</b> ",round(unique(dz3()@data$G_Index[is.na(dz3()@data$G_Index)==FALSE]),3),"<br> </br>"))
  })
  
  output$Indicators3 <- renderUI({
    str_moran <- HTML(paste0("<b> Moran's I: </b>", round(moran_I(),3),"<br> </br>"))
    #str_moran <- HTML(paste0("<b> Moran's I: </b>", round(moran_I,3),"<br> </br>"))
  })
  
  output$Indicators4 <- renderUI({
    str_moran <- HTML(paste0("<b> Número de Clusters: ",length(clusters_list()),"<b>"))
  })
  
}