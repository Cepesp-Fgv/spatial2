#Governador not working
#ES, Estadual, 2006, PAN, votos no mun

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

#setwd("C:/Users/jonny/Google Drive/Academic/FGV-SP/CEPESPData/Spatial2 Shiny App/New_apps_2018/Spatial2 Oct19/spatial_parties")

source("global.R")
source("database.R")

#input <- c()
#input$State <- "ES"
#input$cargo <- 7
#input$turno_value <- turno <- 1
#input$Party <- "PAN"
#input$Year <- 2006
#input$Indicator <- 2

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
    uf <- input$State
    
    print(cargo)
    
    
    if(uf == "" | is.na(ano) | is.na(cargo)){
      cat("Parsing partidos_escolhas. NULL\n")
      return(NULL)
    }    
    
    choices <- (party_template$SIGLA_PARTIDO[party_template$CODIGO_CARGO == cargo &
                                               party_template$SIGLA_UF == uf &
                                               party_template$ANO_ELEICAO == ano &
                                               party_template$NUM_TURNO == turno_use])
    
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
                         selected = 1,
                         options  = list(placeholder = 'Escolha um partido:',allowEmptyOption=F))
    cat("Outputing party_UI. CHECK!!!\n")
    return(UI)
  })
  
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
  
  banco <- eventReactive(input$button, {
    cat("Starting to download banco.\n")
    start <- Sys.time()
    withProgress(message="Por favor, espere...",
                 detail="Download dos dados",
                 value=0.3,{
                   
                   uf <- input$State
                   
                   partido <- switch(input$Party,"PRB"=10,"PP"=11,"PDT"=12,"PT"=13,"PTB"=14,
                                     "PMDB"=15,"PSTU"=16,"PSL"=17,"REDE"=18,"PTN"=19,
                                     "PSC"=20,"PCB"=21,"PR"=22,"PPS"=23,"DEM"=25,
                                     "PSDC"=27,"PRTB"=28,"PCO"=29,"NOVO"=30,"PHS"=31,
                                     "PMN"=33,"PMB"=35,"PTC"=36,"PSB"=40,"PV"=43,
                                     "PRP"=44,"PSDB"=45,"PSOL"=50,"PEN"=51,"PPL"=54,
                                     "PSD"=55,"PC do B"=65,"PT do B"=70,"SD"=77,"PROS"=90,
                                     "PAN"=26,"PGT"=30,"PST"=18,"PL"=22,"PRONA"=56,"PRP"=44,
                                     "PEN"=44,"PPL"=54,"PHS"=31)
                   
                   #Adjustment because old PSD (up to 2002) is 41 but clashes with new PSD
                   partido <- case_when(input$Party=="PSD" & input$Year<=2002~41,
                                        input$Party=="PSD" & input$Year>=2010~55,
                                        TRUE~partido)
                   
                   cargo <- as.numeric(input$cargo)
                   
                   if(is.null(partidos_escolhas())){
                     cat("Starting to download banco. NULL\n")
                     return(1)
                   }
                  
                   cat("Downloading main data (uf=", uf, "; partido=", partido, "; cargo=", cargo,"; ano=",input$Year,"; position=",cargo,"; turno=",turno(),")\n", sep = "") 
                   
                   banco <- db_get_party_elections(year = input$Year,
                                             position = cargo,
                                             candidate_or_party_number = partido,
                                             state = uf,
                                             turn = turno())
                   
                   end_beginning <- round(difftime(Sys.time(), start, units = "secs"), 2)
                   cat("CHECK!!! (Num rows: ",dim(banco)[1],", ", end_beginning, "seconds)\n", sep = "")
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
      
      cat("Calculating 'd' value (pre), ",dim(d)[1]," rows. CHECK!!! (")
      
      if(dim(d)[1] != 0){
        
        #Ideally will be faster when can request specific state
        setkeyv(d,c('ANO_ELEICAO','COD_MUN_IBGE','NUMERO_PARTIDO'))
        
        #### Aggregations
        d <- merge(d,isolate(mun_totals()), by=c("COD_MUN_IBGE","NUM_TURNO"))
        #d <- merge(d,isolate(mun_totals), by=c("COD_MUN_IBGE","NUM_TURNO"))
        d <- merge(d,isolate(state_totals()), by=c("UF","NUM_TURNO"))
        #d <- merge(d,isolate(state_totals), by=c("UF","NUM_TURNO"))
        
        d[,Tot_Partido := sum(QTDE_VOTOS), by=.(ANO_ELEICAO,UF,NUMERO_PARTIDO)]
        d[,Mun_Vote_Share := (QTDE_VOTOS/Tot_Mun)*100]
        d[,Party_Vote_Share := (QTDE_VOTOS/Tot_Partido)*100]
        
        incProgress(amount = 0.7)
        
        #### G-Index Calcs
        d[,G_temp := (QTDE_VOTOS/Tot_Partido - Tot_Mun/Tot_State)^2]
        d[,G_Index := sum(G_temp),by=.(ANO_ELEICAO,UF,NUMERO_PARTIDO)] #Correct? CHECK
        
        #### LQ Calcs
        d[,LQ := (QTDE_VOTOS/Tot_Partido)/(Tot_Mun/Tot_State),by=.(ANO_ELEICAO,UF,NUMERO_PARTIDO)] #Correct?
        
      } else {
        d <- data.table("UF"                   = character(),
                        "NUMERO_PARTIDO"       = integer(),
                        "ANO_ELEICAO"          = integer(),
                        "COD_MUN_IBGE"         = integer(),
                        "QTDE_VOTOS"           = integer())
      }
      end <- Sys.time()
      end_beginning <- round(difftime(end,start, units = "secs"), 2)
      cat("Calculating 'd' value, ",dim(d)[1]," rows. CHECK!!! (", end_beginning, "seconds)\n")
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
    mun_state_contig <- mun_state
    
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
    
    year <- isolate(input$Year)
    
    dz3_temp <- merge(isolate(mun_state_contig()),dz2, by.x="GEOCOD",by.y="COD_MUN_IBGE",all.x=TRUE,all.y=FALSE)
    #dz3_temp <- merge(isolate(mun_state_contig),dz2, by.x="GEOCOD",by.y="COD_MUN_IBGE",all.x=TRUE,all.y=FALSE)
    
    dz3_temp@data[is.na(dz3_temp@data[,"LQ"])==TRUE,"LQ"] <- 0
    dz3_temp@data[is.na(dz3_temp@data[,"QTDE_VOTOS"])==TRUE,"Mun_Vote_Share"] <- 0
    dz3_temp@data[is.na(dz3_temp@data[,"QTDE_VOTOS"])==TRUE,"Tot_State"] <- mean(dz3_temp@data[,"Tot_State"],na.rm=TRUE)
    dz3_temp@data[is.na(dz3_temp@data[,"QTDE_VOTOS"])==TRUE,"Tot_Partido"] <- mean(dz3_temp@data[,"Tot_Partido"],na.rm=TRUE)
    dz3_temp$Tot_Mun <- NULL
    dz3_temp <- merge(dz3_temp,isolate(mun_totals()),by.x="GEOCOD",by.y="COD_MUN_IBGE")
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
    #geo <- as.numeric(st_bbox(state_shp))
    
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
                      na.color = "white")
    } else if(input$Indicator == "1") {
      pal <- colorNumeric(palette  = c("white","red"),
                          domain   = c(0,max(dz5_use@data[["Party_Vote_Share"]],na.rm=TRUE)),
                          na.color = "white")
    } else {
      pal <- colorNumeric(palette  = c("white","red"),
                          domain   = c(0,max(dz5_use@data[["Mun_Vote_Share"]],na.rm=TRUE)),
                          na.color = "white")
    }
    
    popup_text <- paste0("<h4>", dz5_use@data[,"NOME"], "</h2>",
                         "</br>",
                         dz5_use@data[,"NUMERO_PARTIDO"],
                         " recebeu ",
                         "<strong>", dz5_use@data[,"QTDE_VOTOS"], "</strong>",
                         " votos (",
                         round((dz5_use@data[,"QTDE_VOTOS"] / dz5_use@data[,"Tot_Partido"])*100,1),
                         "% do total recebido pelo partido no estado). </br>",
                         "</br> Votos váliados no município: ",
                         dz5_use@data[,"Tot_Mun"],
                         " (",
                         round((dz5_use@data[,"Tot_Mun"] / dz5_use@data[,"Tot_State"])*100,1),
                         "% do total do Estado).",
                         "<br>",
                         "<br> Medida QL: ", round(dz5_use@data[,"LQ"],3))
    
    popup_text_hihi <- paste0("<h4>", dz5_use@data[dz5_use@data$category=="High-High","NOME"], "</h4>",
                              dz5_use@data[,"NUMERO_PARTIDO"],
                              " recebeu ",
                              dz5_use@data[dz5_use@data$category=="High-High","QTDE_VOTOS"],
                              " votos (",
                              round((dz5_use@data[dz5_use@data$category=="High-High","QTDE_VOTOS"]/dz5_use@data[dz5_use@data$category=="High-High","Tot_Deputado"])*100,1),
                              "% do total recebido pelo partido no estado)",
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
                                                          "1"         = "Party_Vote_Share")]]),
                  popup        = popup_text) %>%
      addLegend(title          = switch(input$Indicator,
                                        "Medida QL" = "Medida QL",
                                        "2" = "% Votos no <br>Município",
                                        "1"                  = "% Votos do(a)<br>Partido"),
                pal            = pal,
                values         = dz5_use@data[[switch(input$Indicator,"2"="Mun_Vote_Share",
                                                      "Medida QL"="LQ",
                                                      "1"         = "Party_Vote_Share")]],
                opacity        = 0.8,
                labFormat      = labelFormat(suffix = "%"))  %>%
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
                      na.color = "white")
    } else if(input$Indicator == "1") {
      pal <- colorNumeric(palette  = c("white","red"),
                          domain   = c(0,max(dz5_use@data[["Party_Vote_Share"]],na.rm=TRUE)),
                          na.color = "white")
      
      pal <- colorBin(palette  = c("white","#fcbba1","#fc9272","#fb6a4a","#ef3b2c"),
                      domain   = quantile(dz5_use@data[["Party_Vote_Share"]],probs=c(0,0.2,0.4,0.6,0.8,1),na.rm=T),
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
                                                          "1"         = "Party_Vote_Share")]])) %>%
      addLegend(title          = switch(input$Indicator,
                                        "Medida QL" = "Medida QL",
                                        "2" = "% Votos no <br>Município",
                                        "1"                  = "% Votos do(a)<br>Partido"),
                pal            = pal,
                values         = dz5_use@data[[switch(input$Indicator,"2"="Mun_Vote_Share",
                                                      "Medida QL"="LQ",
                                                      "1"         = "Party_Vote_Share")]],
                opacity        = 0.8,
                labFormat      = labelFormat(suffix = "%"))  %>%
      addPolygons(data         = dz5_use[dz5_use@data$category=="High-High",],
                  fillOpacity  = 0,
                  weight       = 2,
                  color        = "green",
                  stroke       = TRUE)
  })
  
  output$map_down <- downloadHandler(
    filename = paste0(paste("CepespData",
                            input$Year, 
                            input$State, 
                            input$cargo,
                            "Turno", 
                            input$turno_value, 
                            input$Party,
                            sep="_"),
                      ".png")
    
    , content = function(file) {
      mapshot( x = map_reactive()
               , file = file
               , selfcontained = FALSE # when this was not specified, the function for produced a PDF of two pages: one of the leaflet map, the other a blank page.
      )
    } # end of content() function
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
  
  
  output$Result <- renderUI({
    if(is.null(dz3())){
      output_error <- "Por favor, informe os parâmetros <b>estado</b>, <b>cargo</b>, <b>ano</b> e <b>partido</b> antes atualizar o mapa."
      return(HTML(output_error))
    }
    
    str_Result <- paste0("<br><b>Votos: </b>",unique(dz3()@data$Tot_Partido[is.na(dz3()@data$Tot_Partido)==FALSE]),
                         "<br><b>Porcentagem dos votos válidos: </b>",round((unique(dz3()@data$Tot_Partido[is.na(dz3()@data$Tot_Partido)==FALSE])/unique(dz3()@data$Tot_State[is.na(dz3()@data$Tot_State)==FALSE]))*100,1), "%")
    #str_Result <- paste0("<br><b>Votos: </b>",unique(dz3@data$Tot_Partido[is.na(dz3@data$Tot_Partido)==FALSE]),
    #                     "<br><b>Porcentagem dos votos válidos: </b>",round((unique(dz3@data$Tot_Partido[is.na(dz3@data$Tot_Partido)==FALSE])/unique(dz3@data$Tot_State[is.na(dz3@data$Tot_State)==FALSE]))*100,1), "%")
    
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
  
  
  output$Note <- renderUI({
    note <- paste0("<font size='3'> As mapas eleitorais foram desenvolvidos utilizando os dados coletados e limpos pelo <a href='http://cepesp.io/'> CepespData </a>. Desenvolvido por Jonathan Phillips e Rafael de Castro Coelho Silva com apoio do equipe CEPESP. </font>")
    HTML(note)
  })
  
  output$moran <- renderUI({
    str_moran <- paste0("<b> Moran's I: </b>", round(moran_I(),3))
    HTML(str_moran)
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
                              "<br><b>Votos válidos: </b>",format(unique(dz3()@data$Tot_Partido[is.na(dz3()@data$Tot_Partido)==FALSE]),big.mark=" "),
                              "<br><b>% dos votos: </b>",round((unique(dz3()@data$Tot_Partido[is.na(dz3()@data$Tot_Partido)==FALSE])/unique(dz3()@data$Tot_State[is.na(dz3()@data$Tot_State)==FALSE]))*100,1), "%",
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