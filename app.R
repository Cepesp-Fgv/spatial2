# rm(list=ls())
# options(shiny.reactlog=TRUE)

library(plyr)
library(data.table)
library(shiny)
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
library(shinythemes)
library(dplyr)
library(DT)
library(magrittr)
source("global.R")

ui <- navbarPage("Spatial Voting",id="nav",theme=shinytheme("flatly"),
                 tabPanel("Mapa",div(class="outer",
                                    tags$head(
                                      includeCSS("styles.css")
                                    ),
                                    
                                    tags$style(type="text/css",
                                               ".shiny-output-error { visibility: hidden; }",
                                               ".shiny-output-error:before { visibility: hidden; }"
                                    ),
                                    leafletOutput("map",width="100%",height="100%")),
                           bootstrapPage(absolutePanel(id = "note", class = "panel panel-default", fixed = TRUE,
                                                      draggable = TRUE, top = 60, left = "auto", right = 30, bottom = "auto",
                                                      width = 330, height = "auto",
                                                      HTML('<button data-toggle="collapse" data-target="#demo">Info</button>'),
                                                      tags$div(id = 'demo',  class="collapse in",htmlOutput("Note"))
                          ))
                 ),
                 tabPanel("Charts",
                          fluidRow(column(width=4,""),column(width=4,plotOutput("G_cand")),column(width=4,plotOutput("I_cand"))),
                          bootstrapPage(absolutePanel(id = "cuts", class = "panel panel-default", fixed = TRUE,
                                                      draggable = TRUE, top = "auto", left = "auto", right = 30, bottom = 60,
                                                      width = 700, height = "auto",
                                                      h4("Winning candidates tend to have more diffuse (low G) and contiguous (high I) support"),
                                                      radioButtons("Cut",
                                                                   label = "Data:",
                                                                   choices = list("All","Selected Year","Selected State","Selected Party"),
                                                                   selected = "All")
                 ))
                 ),
                 tabPanel("Classify",
                          column(width=4,""),
                          column(width=4,plotOutput("quadrant",click="plot_click",hover="plot_hover"),
                                 uiOutput("hover_info"),
                                 htmlOutput("Classify_Note"),
                                 htmlOutput("Classify_Note2")),
                          column(width=4,leafletOutput("map_selected",width="500px",height="400px"))
                 ),
                 tabPanel("Clusters",
                          column(width=4,""),
                          column(width=4,htmlOutput("Num_clusters"),h4("Cluster Summary"),dataTableOutput("Clusters_agg"),h4("Municipalities by Cluster"),dataTableOutput("Clusters")),
                          column(width=4,leafletOutput("map_clusters",width="500px",height="400px"))
                 ),
                 tabPanel("Extremes",
                          column(width=4,""),
                          column(width=4,h4("Top and Bottom 5 G Index in this State and Year"),dataTableOutput("Extremes")),
                          column(width=4,leafletOutput("map_selected_hi",width="500px",height="400px"))
                 ),
                 absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                               draggable = FALSE, top = 120, left = 10, right = "auto", bottom = "auto",
                               width = 330, height = "auto",
                               fluidPage(h4("Resultados Eleitorais"),
                                         selectInput("cargo", 
                                                     label = "Escolha um cargo",
                                                     choices = list("Presidente" = 1,
                                                                    "Governador" = 3, 
                                                                    "Senador"    = 5,
                                                                    "Deputado Federal" = 6),
                                                     selected = 6),
                                         selectInput("Year", 
                                                     label = "Escolha um ano:",
                                                     choices = c(1998,2002,2006,2010,2014),
                                                     selected = 2014),
                                         uiOutput("turno_UI"),
                                                                                  selectInput("State", 
                                                     label = "Escolha um estado:",
                                                     choices = c("AC","AM","AL","AP","BA","CE","DF","ES","GO","MA","MS","MG","MT","PA",
                                                                 "PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO"),
                                                     selected = NULL),
                                         uiOutput("party_UI"),
                                         uiOutput("cand_UI"),
                                         radioButtons("Indicator",
                                                      label = "Indicador:",
                                                      choices = list("Proporção de Votos no Município" = "Proporção de Votos", "Medida QL"),
                                                      selected = "Proporção de Votos"),
                                         actionButton("button", label = strong("Atualizar"), width = "95%"),
                                         HTML(paste0("<hr> </hr>")),
                                         htmlOutput("Result"),
                                         htmlOutput("G_Index"),
                                         htmlOutput("moran"))
                               
                 )
)

server <- function(input, output, session) {

  ### Turno ###
  turno <- reactive({
    cargo <- as.numeric(input$cargo)
    if(cargo%in% c(1,3)){
      return(input$turno_value)
    } else {
      return(1)
    }
  })
  
  output$turno_UI <- renderUI({
    cargo <- as.numeric(input$cargo)
    if(cargo %in% c(1,3)){
      selectInput("turno_value", 
                  label = "Escolha um turno:",
                  choices = list(`1º Turno` = 1,
                                 `2º Turno` = 2),
                  selected = 1)
    }
  })
  
  ### Partido ###
  
  partidos_escolhas <- reactive({
    cargo <- as.numeric(input$cargo)
    ano <- as.numeric(input$Year)
    turno <- turno()
    
    if(cargo == 1){
      uf <- "BR"
    } else {
      uf <- input$State
    }
    choices <- (party_template$PARTIDOS[party_template$CODIGO_CARGO == cargo &
                                          party_template$SIGLA_UF == uf &
                                          party_template$ANO_ELEICAO == ano &
                                          party_template$NUM_TURNO == turno])[[1]]
    choices <- unique(sort(choices))
    return(choices)
  }) 
  
  output$party_UI <- renderUI({
    partidos <- partidos_escolhas()
    selectInput("Party",
                label = "Escolha um partido:",
                choices = partidos,
                selected = partidos[[1]])
  })
  
  ### Candidato ###

  candidatos_value <- reactive({
    cargo <- isolate(as.numeric(input$cargo))
    nomes <- banco()[["NOME_URNA_CANDIDATO"]]
    print(banco())
    cat("Parsing candidates names. ")
    candidatos_value <- sort(unique(nomes))
    cat("CHECK!!!\n")
    print(paste0("candidatos_value: ", candidatos_value))
    return(candidatos_value)
  })
  
  query_observe <- eventReactive(input$button, {
    ano <- isolate(input$Year)
    uf <- isolate(input$State)
    cargo <- isolate(as.numeric(input$cargo))
    party <- isolate(input$Party)
    
    cat(paste0("query_observe: ", paste0(ano,uf,cargo, party),"\n"))
    
    return(paste0(ano,uf,cargo, party))
  })
  
  output$cand_UI <- renderUI({
    ano <- input$Year
    uf <- input$State
    cargo <- as.numeric(input$cargo)
    party <- input$Party
    candidatos <- candidatos_value()
    
    if(paste0(ano,uf,cargo, party) == query_observe()){
      cat("Outputing candidates UI. ")
      if(cargo %in% c(5,6)){
        UI <- selectInput("candidato",
                          label = "Escolha um candidato:",
                          choices = candidatos,
                          selected = candidatos[1])
      } else {
        UI <- NULL
      }
      cat("CHECK!!!\n")
    } else {
      UI <- NULL
    }
    return(UI)
    })
  
  Candidate <- reactive({
    cargo <- isolate(as.numeric(input$cargo))
    partido <- isolate(input$Party)
    banco <- banco()
    cat("Candidate variable\n")
    if(cargo %in% c(1,3)){
      candidato_uso <- banco$NOME_URNA_CANDIDATO[banco$SIGLA_PARTIDO == partido]
      Candidate <- unique(candidato_uso)
    } else {
      Candidate <- input$candidato
    }
    print(paste0("Candidate: ", Candidate))
    cat("Candidate variable. CHECK!!!\n")
    return(Candidate)
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

  # input <- tibble::tibble(cargo = 1,
  #                         Year = 2014,
  #                         turno = 2,
  #                         Party = "PT",
  #                         State  = "SP")
  # url <- "http://api.cepesp.io/api/consulta/tse"
  
  banco <- eventReactive(input$button, {
    start <- Sys.time()
    withProgress(message="Por favor, espere...",
                 detail="Download dos dados",
                 value=0.3,{
                   
    uf <- input$State
    partido <- stringr::str_remove_all(input$Party, " ")
    cargo <- as.numeric(input$cargo)
    
    cat("Downloading main data (uf=", uf, "; partido=", partido, ";cargo=", cargo, ")", sep = "") 
    
    vars <- list("NUM_TURNO","UF","NUMERO_PARTIDO","ANO_ELEICAO","COD_MUN_IBGE",
                 "QTDE_VOTOS","NUMERO_CANDIDATO","SIGLA_PARTIDO","NOME_URNA_CANDIDATO",
                 "DESC_SIT_TOT_TURNO")
    
    names(vars) <- rep("selected_columns[]",length(vars))
    
    filter <- list("columns[0][name]"          = "UF",
                   "columns[0][search][value]" = uf,
                   "columns[1][name]"          = "NUMERO_PARTIDO",
                   "columns[1][search][value]" = sigla_partidos[partido])
    consulta <- append(append(list(cached=TRUE,anos = input$Year,uf=input$State,agregacao_regional=6,
                                   agregacao_politica=2, cargo= cargo),vars),filter)
    banco <- content(GET(url,query=consulta),type="text/plain", encoding = "UTF-8")
    banco <- readr::read_csv(banco, col_types = list(NUMERO_PARTIDO      = readr::col_integer(),
                                                     NOME_URNA_CANDIDATO = readr::col_character(),
                                                     COD_MUN_IBGE        = readr::col_integer(),
                                                     NUMERO_CANDIDATO    = readr::col_integer(),
                                                     SIGLA_PARTIDO       = readr::col_character(),
                                                     DESC_SIT_TOT_TURNO  = readr::col_character(),
                                                     UF                  = readr::col_character(),
                                                     NUM_TURNO           = readr::col_integer(),
                                                     ANO_ELEICAO         = readr::col_integer(),
                                                     QTDE_VOTOS          = readr::col_integer()))
    banco <- banco[banco$NUM_TURNO == turno(),]
    end_beginning <- round(difftime(Sys.time(), start, units = "secs"), 2)
    cat("CHECK!!! (", end_beginning, "seconds)\n", sep = "")
    })
    return(banco)
  })
  
  d <- eventReactive(Candidate(), {
    start <- Sys.time()
    
    cat("Calculating 'd' value. \n")
    
    withProgress(message="Por favor, espere...",detail="Download dos dados", value=0.3,{
    
    d <- data.table::as.data.table(banco())

    if(dim(d)[1] != 0){
    
    #Ideally will be faster when can request specific state
    setkeyv(d,c('ANO_ELEICAO','COD_MUN_IBGE','NUMERO_CANDIDATO'))
    
    #### Aggregations
    d <- merge(d,mun_totals(), by="COD_MUN_IBGE")
    d <- merge(d,state_totals(), by="UF")

    d[,Tot_Deputado := sum(QTDE_VOTOS),by=.(ANO_ELEICAO,UF,NUMERO_CANDIDATO)]
    d[,Mun_Vote_Share := (QTDE_VOTOS/Tot_Mun)*100]
    
    incProgress(amount = 0.7)
    
    #### G-Index Calcs
    d[,G_temp := (QTDE_VOTOS/Tot_Deputado - Tot_Mun/Tot_State)^2]
    d[,G_Index := sum(G_temp),by=.(ANO_ELEICAO,UF,NUMERO_CANDIDATO)] #Correct? CHECK
    
    #### LQ Calcs
    d[,LQ := (QTDE_VOTOS/Tot_Deputado)/(Tot_Mun/Tot_State),by=.(ANO_ELEICAO,UF,NUMERO_CANDIDATO)] #Correct?
    
    #Remove NULO line from selectable candidates, though is included in calculations of total statewide and municipal votes above
    d <- d[NOME_URNA_CANDIDATO!="#NULO#"]
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
    beginning <- Sys.time()
    names(mun)[which(names(mun)=="UF")] <- "UF_shape"
    mun_state <- mun[mun$UF_shape==input$State,]
    state_nb <- poly2nb(mun_state)
    if (any(card(state_nb)==0)){
      mun_state_contig <- mun_state[-which(card(state_nb)==0),]  
    } else {
      mun_state_contig <- mun_state
    }
    end <- Sys.time()
    cat("Time for trimming shapefile to state and first screening for neighbours:",end-beginning,".\n")
    return(mun_state_contig)
  })
  
  dz3 <- reactive({
    candidato <- Candidate()
    beginning <- Sys.time()
    
    dz2 <- d()[NOME_URNA_CANDIDATO == candidato]
    
    dz3_temp <- merge(mun_state_contig(),dz2, by.x="GEOCOD",by.y="COD_MUN_IBGE",all.x=TRUE,all.y=FALSE)
    dz3_temp@data[is.na(dz3_temp@data[,"LQ"])==TRUE,"LQ"] <- 0
    dz3_temp@data[is.na(dz3_temp@data[,"QTDE_VOTOS"])==TRUE,"Mun_Vote_Share"] <- 0
    dz3_temp@data[is.na(dz3_temp@data[,"QTDE_VOTOS"])==TRUE,"Tot_State"] <- mean(dz3_temp@data[,"Tot_State"],na.rm=TRUE)
    dz3_temp@data[is.na(dz3_temp@data[,"QTDE_VOTOS"])==TRUE,"Tot_Deputado"] <- mean(dz3_temp@data[,"Tot_Deputado"],na.rm=TRUE)
    dz3_temp@data[is.na(dz3_temp@data[,"QTDE_VOTOS"])==TRUE,"NOME_URNA_CANDIDATO"] <- candidato
    dz3_temp$Tot_Mun <- NULL
    dz3_temp <- merge(dz3_temp,mun_totals(),by.x="GEOCOD",by.y="COD_MUN_IBGE")
    dz3_temp@data[is.na(dz3_temp@data[,"QTDE_VOTOS"])==TRUE,"QTDE_VOTOS"] <- 0
    end <- Sys.time()
    cat("Time for merging candidate data with shapefile:",end-beginning,".\n")
    dz3 <- dz3_temp
    return(dz3)
  })
  
  state_nb2 <- reactive({
    state_nb2 <- poly2nb(mun_state_contig()) #Necessary to remove 'islands' as causes problems
    return(state_nb2)
  })
  
  state_nb2listw <- reactive({
    beginning <- Sys.time()
    state_nb2listw <- nb2listw(state_nb2(),zero.policy=TRUE)
    end <- Sys.time()
    cat("Time for identifying neightbours list: ",end-beginning,".\n")
    return(state_nb2listw)
  })
  
  dz5 <- reactive({
    beginning <- Sys.time()
    dz4 <- dz3()
    #dz4 <- dz3
    
    lisa <- as.data.frame(localmoran(dz4$LQ,state_nb2listw()))

    dz4$LISA_I <- lisa[,"Ii"]
    dz4$LISA_p <- lisa[,"Pr(z > 0)"]
    dz4$LQ_stdzd <- as.vector(scale(dz4$LQ))
    dz4$LQ_stdzd_lag <- lag.listw(state_nb2listw(),dz4$LQ_stdzd, NAOK=TRUE) #NAOK here helps or hinders?

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
  
  state_shp <- reactive({
    state_shp <- unionSpatialPolygons(dz5(),IDs=dz5()@data[,"UF_shape"])
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles()
  })
  
  shape_estado <- reactive({
    ### Input ###
    uf <- input$State
    
    ### Data ###
    shape_estado <- readr::read_rds(paste0("data/shape_states/", uf,".rds"))
    
    return(shape_estado)
  })
  
  observe({
    geo <- as.numeric(st_bbox(shape_estado()))
    
    cargo <- input$cargo
    candi <- input$candidato
    uf <- input$State
    party <- input$Party
    
    ### Base Map ###
    
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>% 
      addPolygons(data = shape_estado(),
                  fillOpacity  = 0,
                  weight       = 3,
                  color        = "black",
                  fillColor    = NULL) %>% 
      flyToBounds(geo[3], geo[4], geo[1], geo[2])
  })
  
  observe({
    dz5_use <- dz5()
    
    if (input$Indicator=="Medida QL"){
          pal <- colorBin(palette  = c("white","light blue","#fcbba1","#fb6a4a","#ef3b2c","#cb181d"),
                          domain   = c(0,1000),
                          bins     = c(0,0.01,1,5,10,50,1000),
                          na.color = "white")
    } else {
      pal <- colorNumeric(palette  = c("white","red"),
                          domain   = c(0,max(dz5_use@data[,"Mun_Vote_Share"],na.rm=TRUE)),
                          na.color = "white")
    }

    popup_text <- paste0("<h4>", dz5_use@data[,"NOME"], "</h2>",
                         "</br>",
                         dz5_use@data[,"NOME_URNA_CANDIDATO"],
                         " recebeu ",
                         "<strong>", dz5_use@data[,"QTDE_VOTOS"], "</strong>",
                         " votos (",
                         round((dz5_use@data[,"QTDE_VOTOS"] / dz5_use@data[,"Tot_Deputado"])*100,1),
                         "% do total recebeido pelo candidato(a) no estado). </br>",
                         "</br> Votos váliados no município: ",
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
                              "% do total recebeido pelo candidato(a) no estado)",
                              "</br> </br> Votos válidos no município: ",
                              dz5_use@data[dz5_use@data$category=="High-High","Tot_Mun"],
                              " (",
                              round((dz5_use@data[dz5_use@data$category=="High-High","Tot_Mun"]/dz5_use@data[dz5_use@data$category=="High-High","Tot_State"])*100,1),
                              "% do total do Estado)",
                              "<br>",
                              "<br> Medida QL: ",
                              round(dz5_use@data[dz5_use@data$category=="High-High","LQ"],3))

    leafletProxy("map") %>%
      clearControls() %>% 
      addPolygons(data         = dz5_use,
                  layerId      = dz5_use@data[,switch(input$Indicator,"Proporção de Votos"="Mun_Vote_Share","Medida QL"="LQ")],
                  fillOpacity  = 0.8,
                  weight       = 0.1,
                  color        = "black",
                  fillColor    = pal(dz5_use@data[,switch(input$Indicator,"Proporção de Votos"="Mun_Vote_Share","Medida QL"="LQ")]),
                  popup        = popup_text) %>%
      addLegend(position       = "bottomright",
                title          = ifelse(input$Indicator=="Medida QL","Medida QL","% Votos no Município"),
                pal            = pal,
                values         = dz5_use@data[,switch(input$Indicator,"Proporção de Votos"="Mun_Vote_Share","Medida QL"="LQ")],
                opacity        = 0.8,
                labFormat      = labelFormat(suffix = "%"))  %>%
      addPolygons(data         = dz5_use[dz5_use@data$category=="High-High",],
                  layerId      = dz5_use@data[dz5_use@data$category=="High-High",],
                  fillOpacity  = 0,
                  weight       = 2,
                  color        = "green",
                  stroke       = TRUE,
                  popup        = popup_text_hihi)
  })
  
  clusters <- reactive({
    dz5_HH <- dz5()[dz5()$category=="High-High",]
    if (dim(dz5_HH)[1]!=0){
      clusters <- gUnion(dz5_HH,dz5_HH)  
    } else {
      clusters <- NULL
    }
  })

  clusters_sp <- reactive({
    if (!(is.null(clusters()))){
      clusters_sep <- slot(clusters()@polygons[[1]],"Polygons")

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
      clusters_sp_cent_table_temp <- as.data.frame(clusters_sp_cent@coords)
      clusters_sp_cent_table_temp$Cluster_num <- rownames(clusters_sp_cent_table_temp)
      clusters_sp_cent_table <- clusters_sp_cent_table_temp
    }
  })

  clusters_list <- reactive({
    if (!(is.null(clusters()))){
      clusters_list_temp <- list()
      num_clust <- length(clusters_sp())

      for (i in 1:num_clust){
        clusters_list_temp[[i]] <- raster::intersect(dz5()[dz5()$category=="High-High",],clusters_sp()[i])
        clusters_list_temp[[i]]@data$Cluster_Num <- i
      }
      clusters_list <- clusters_list_temp
    }
  })
  
  output$Num_clusters <- renderUI({
    if (!(is.null(clusters()))){
      Num_clusters <- paste0("<b> Number of High-High Clusters: ",length(clusters_list()),"<b>")
      HTML(Num_clusters)
    } else {
      HTML(paste0("<b> No clusters <b>"))
    }
  })
  
  cluster_table <- reactive({
    if (!(is.null(clusters()))){
      clusters_table_temp <- rbind.fill(lapply(clusters_list(),slot,'data'))
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
    d_G <- d()[,unique(G_Index),by=.(UF,NUMERO_CANDIDATO,NOME_URNA_CANDIDATO,NUMERO_PARTIDO,DESC_SIT_TOT_TURNO)]
  })
  
  output$Result <- renderUI({
    str_Result <- paste0("Resultado: ",
                         unique(dz3()@data$DESC_SIT_TOT_TURNO[is.na(dz3()@data$DESC_SIT_TOT_TURNO)==FALSE]),
                         " <br> Total de votos do candidato: ",unique(dz3()@data$Tot_Deputado[is.na(dz3()@data$Tot_Deputado)==FALSE]),
                         " <br> % dos votos váliados: ",round((unique(dz3()@data$Tot_Deputado[is.na(dz3()@data$Tot_Deputado)==FALSE])/unique(dz3()@data$Tot_State[is.na(dz3()@data$Tot_State)==FALSE]))*100,1),"%")
    HTML(str_Result)
  })
  
  output$G_Index <- renderUI({
    str_G_Index <- paste0("<b> G Index: ",round(unique(dz3()@data$G_Index[is.na(dz3()@data$G_Index)==FALSE]),3),"<b>")
    HTML(str_G_Index)
  })
  
  moran_I <- reactive({
    moran_I <- moran(dz3()$LQ,state_nb2listw(),n=length(state_nb2()),Szero(state_nb2listw()),zero.policy=TRUE,NAOK=TRUE)$I  
  })
  
  output$chart_LQ <- renderPlot({
    ggplot() +
      geom_density(data=dz3()@data,aes(x=LQ),fill="light blue",colour=NA,alpha=0.5) +
      xlab("Log of Medida QL") +
      theme_classic() +
      ylab("Density") +
      scale_x_log10()
  })
  
  output$chart_scatter <- renderPlot({
    ggplot() +
      geom_point(aes(x=dz3()@data$Tot_Mun,y=dz3()@data$LQ),color="dark green") +
      xlab("Log of Municipal Voting Population") +
      ylab("Medida QL") +
      theme_classic() +
      scale_x_log10()
  })

  d_uniq_cut <- reactive({
    if (input$Cut=="All"){
      d_uniq_cut <- d_uniq
    } else if (input$Cut=="Selected Year") {
      d_uniq_cut <- d_uniq[d_uniq$anoEleicao==input$Year,]
    } else if (input$Cut=="Selected State") {
      d_uniq_cut <- d_uniq[d_uniq$sigla_UF==input$State,]
    } else if (input$Cut=="Selected Party") {
      d_uniq_cut <- d_uniq[d_uniq$NUMERO_PARTIDO==sigla_partidos[input$Party],]
    }
    d_uniq_cut
  })
  
  output$G_cand <- renderPlot({
    ##Check categories for winner here
    ggplot() + geom_density(data=d_uniq_cut(),aes(x=G_Index,fill=Result),colour=NA,alpha=0.5) +
      xlab("G Index") + 
      theme_classic() + 
      ylab("Density") +
      geom_vline(xintercept=unique(dz3()@data$G_Index[is.na(dz3()@data$G_Index)==FALSE]),lty=2) + 
      theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),legend.text=element_text(size=))
  })
  
  output$I_cand <- renderPlot({
    ggplot() +
      geom_density(data=d_uniq_cut(),aes(x=MoranI,fill=Result),colour=NA,alpha=0.5) +
      xlab("Moran's I") +
      ylab("Density") +
      theme_classic() +
      geom_vline(xintercept=moran_I(),lty=2) +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"),
            legend.text=element_text(size=12))
  })
  
  output$Note <- renderUI({
    note <- paste0("A <b>Percentagem de Voto no Município</b> é o percentual de votos válidos no município recebidos pelo candidato.<br> A <b>Medida QL</b> indica quantas vezes mais votos o candidate recebe no município em comparação com se eles recebessem apoio igual em todo o estado. Essa medida é diretamente proporcional à percentagem de votos, mas escala o indicador para que valores maiores que '1' indiquem os municípios em quais o candidato é particularmente dependente.<br> O <b>Índice G</b> mede o desvio de apoio do candidato em todo o estado de uma distribuição uniforme de apoio em proporção perfeita à população local. G = 0 indica uma taxa uniforme de conversão da população aos votos, e G = 1 indica concentração perfeita de apoio eleitoral em apenas um município.<br> O mapa destaca com <b>fronteiras verdes</b> os clusters de municípios onde votos são concentrados estatisticamente significativos..")
    HTML(note)
  })
  
  output$moran <- renderUI({
    str_moran <- paste0("<b> Moran's I: ",round(moran_I(),3),"<b>")
    HTML(str_moran)
  })

  output$quadrant <- renderPlot({
    ggplot() + 
      geom_point(data=d_uniq[d_uniq$anoEleicao==input$Year & d_uniq$sigla_UF==input$State,],aes(x=G_Index,y=MoranI,size=Number_Votes,shape=Result),color="blue",alpha=0.2) + 
      geom_point(data=d_uniq[d_uniq$NOME_URNA_CANDIDATO!=Candidate() & d_uniq$anoEleicao==input$Year & d_uniq$sigla_UF==input$State & d_uniq$NUMERO_PARTIDO==switch(input$Party,"PRB"=10,"PP"=11,"PDT"=12,"PT"=13,"PTB"=14,"PMDB"=15,"PSTU"=16,"PSL"=17,"REDE"=18,"PTN"=19,"PSC"=20,"PCB"=21,"PR"=22,"PPS"=23,"DEM"=25,"PSDC"=27,"PRTB"=28,"PCO"=29,"NOVO"=30,"PHS"=31,"PMN"=33,"PMB"=35,"PTC"=36,"PSB"=40,"PV"=43,"PRP"=44,"PSDB"=45,"PSOL"=50,"PEN"=51,"PPL"=54,"PSD"=55,"PCdoB"=65,"PTdoB"=70,"SD"=77,"PROS"=90),],aes(x=G_Index,y=MoranI,size=Number_Votes,shape=Result),color="red",alpha=0.8) + 
      geom_point(data=d_uniq[d_uniq$NOME_URNA_CANDIDATO==Candidate() & d_uniq$anoEleicao==input$Year & d_uniq$sigla_UF==input$State & d_uniq$NUMERO_PARTIDO==switch(input$Party,"PRB"=10,"PP"=11,"PDT"=12,"PT"=13,"PTB"=14,"PMDB"=15,"PSTU"=16,"PSL"=17,"REDE"=18,"PTN"=19,"PSC"=20,"PCB"=21,"PR"=22,"PPS"=23,"DEM"=25,"PSDC"=27,"PRTB"=28,"PCO"=29,"NOVO"=30,"PHS"=31,"PMN"=33,"PMB"=35,"PTC"=36,"PSB"=40,"PV"=43,"PRP"=44,"PSDB"=45,"PSOL"=50,"PEN"=51,"PPL"=54,"PSD"=55,"PCdoB"=65,"PTdoB"=70,"SD"=77,"PROS"=90),],aes(x=G_Index,y=MoranI,size=Number_Votes,shape=Result),color="dark green",alpha=1) + 
      theme_classic() + 
      geom_vline(xintercept=median(d_uniq[d_uniq$anoEleicao==input$Year & d_uniq$sigla_UF==input$State,"G_Index"],na.rm=TRUE),lty=2) +
      geom_hline(yintercept=median(d_uniq[d_uniq$anoEleicao==input$Year & d_uniq$sigla_UF==input$State,"MoranI"],na.rm=TRUE),lty=2) +
      theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),legend.text=element_text(size=12)) + 
      xlab("G Index") + 
      ylab("Moran's I")
    })
  
  mouse <- reactive({
    if (is.null(input$plot_click)){
      mouse_temp  <- d_uniq[d_uniq$anoEleicao==input$Year & d_uniq$sigla_UF==input$State & d_uniq$NOME_URNA_CANDIDATO==Candidate(),][1,]
    } else {
      mouse_temp <- nearPoints(d_uniq[d_uniq$anoEleicao==input$Year & d_uniq$sigla_UF==input$State,],input$plot_click,threshold=10,maxpoints=1)
    }
    mouse <- mouse_temp
  })

  G_Quadrant <- reactive({
    if(d_uniq[d_uniq$NUMERO_CANDIDATO==mouse()$NUMERO_CANDIDATO & d_uniq$anoEleicao==input$Year & d_uniq$sigla_UF==input$State & d_uniq$NUMERO_PARTIDO==switch(input$Party,"PRB"=10,"PP"=11,"PDT"=12,"PT"=13,"PTB"=14,"PMDB"=15,"PSTU"=16,"PSL"=17,"REDE"=18,"PTN"=19,"PSC"=20,"PCB"=21,"PR"=22,"PPS"=23,"DEM"=25,"PSDC"=27,"PRTB"=28,"PCO"=29,"NOVO"=30,"PHS"=31,"PMN"=33,"PMB"=35,"PTC"=36,"PSB"=40,"PV"=43,"PRP"=44,"PSDB"=45,"PSOL"=50,"PEN"=51,"PPL"=54,"PSD"=55,"PCdoB"=65,"PTdoB"=70,"SD"=77,"PROS"=90),"G_Index"]>median(d_uniq[d_uniq$anoEleicao==input$Year & d_uniq$sigla_UF==input$State,"G_Index"],na.rm=TRUE)){
      G_Quadrant_temp <-"above"
    } else {
      G_Quadrant_temp <- "below"
    }
    G_Quadrant <- G_Quadrant_temp
  })
  
  G_desc <- reactive({
    if(d_uniq[d_uniq$NUMERO_CANDIDATO==mouse()$NUMERO_CANDIDATO & d_uniq$anoEleicao==input$Year & d_uniq$sigla_UF==input$State & d_uniq$NUMERO_PARTIDO==switch(input$Party,"PRB"=10,"PP"=11,"PDT"=12,"PT"=13,"PTB"=14,"PMDB"=15,"PSTU"=16,"PSL"=17,"REDE"=18,"PTN"=19,"PSC"=20,"PCB"=21,"PR"=22,"PPS"=23,"DEM"=25,"PSDC"=27,"PRTB"=28,"PCO"=29,"NOVO"=30,"PHS"=31,"PMN"=33,"PMB"=35,"PTC"=36,"PSB"=40,"PV"=43,"PRP"=44,"PSDB"=45,"PSOL"=50,"PEN"=51,"PPL"=54,"PSD"=55,"PCdoB"=65,"PTdoB"=70,"SD"=77,"PROS"=90),"G_Index"]>median(d_uniq[d_uniq$anoEleicao==input$Year & d_uniq$sigla_UF==input$State,"G_Index"],na.rm=TRUE)){
      G_desc_temp <-"concentrated"
    } else {
      G_desc_temp <- "diffuse"
    }
    G_desc <- G_desc_temp
  })
  
  Moran_Quadrant <- reactive({
    if(d_uniq[d_uniq$NUMERO_CANDIDATO==mouse()$NUMERO_CANDIDATO & d_uniq$anoEleicao==input$Year & d_uniq$sigla_UF==input$State & d_uniq$NUMERO_PARTIDO==switch(input$Party,"PRB"=10,"PP"=11,"PDT"=12,"PT"=13,"PTB"=14,"PMDB"=15,"PSTU"=16,"PSL"=17,"REDE"=18,"PTN"=19,"PSC"=20,"PCB"=21,"PR"=22,"PPS"=23,"DEM"=25,"PSDC"=27,"PRTB"=28,"PCO"=29,"NOVO"=30,"PHS"=31,"PMN"=33,"PMB"=35,"PTC"=36,"PSB"=40,"PV"=43,"PRP"=44,"PSDB"=45,"PSOL"=50,"PEN"=51,"PPL"=54,"PSD"=55,"PCdoB"=65,"PTdoB"=70,"SD"=77,"PROS"=90),"MoranI"]>median(d_uniq[d_uniq$anoEleicao==input$Year & d_uniq$sigla_UF==input$State,"MoranI"],na.rm=TRUE)){
      Moran_Quadrant_temp <-"above"
    } else {
      Moran_Quadrant_temp <- "below"
    }
    Moran_Quadrant <- Moran_Quadrant_temp
  })
  
  Moran_desc <- reactive({
    if(d_uniq[d_uniq$NUMERO_CANDIDATO==mouse()$NUMERO_CANDIDATO & d_uniq$anoEleicao==input$Year & d_uniq$sigla_UF==input$State & d_uniq$NUMERO_PARTIDO==switch(input$Party,"PRB"=10,"PP"=11,"PDT"=12,"PT"=13,"PTB"=14,"PMDB"=15,"PSTU"=16,"PSL"=17,"REDE"=18,"PTN"=19,"PSC"=20,"PCB"=21,"PR"=22,"PPS"=23,"DEM"=25,"PSDC"=27,"PRTB"=28,"PCO"=29,"NOVO"=30,"PHS"=31,"PMN"=33,"PMB"=35,"PTC"=36,"PSB"=40,"PV"=43,"PRP"=44,"PSDB"=45,"PSOL"=50,"PEN"=51,"PPL"=54,"PSD"=55,"PCdoB"=65,"PTdoB"=70,"SD"=77,"PROS"=90),"MoranI"]>median(d_uniq[d_uniq$anoEleicao==input$Year & d_uniq$sigla_UF==input$State,"MoranI"],na.rm=TRUE)){
      Moran_desc_temp <-"contiguous"
    } else {
      Moran_desc_temp <- "dispersed"
    }
    Moran_desc <- Moran_desc_temp
    
  })
  
  mouse_cand <- reactive({
    mouse_cand <- d_uniq[d_uniq$NUMERO_CANDIDATO==mouse()$NUMERO_CANDIDATO & d_uniq$anoEleicao==input$Year & d_uniq$sigla_UF==input$State,"NOME_URNA_CANDIDATO"]
  })
  
  classify_note_text <- reactive({
    classify_note_text <- paste0("Each point represents a <font color=\"blue\"> Candidate </font> in this election, with the size proportionate to their total number of votes received. <font color=\"red\"> Red </font> points indicate votes for the selected party. The <font color=\"green\"> Green </font> point is the selected candidate. Click on <font color=\"red\">Red </font> or <font color=\"green\"> Green </font> points to view the distribution of Medida QLs for that candidate. <br> <br>")
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
  
  dy3 <- reactive({
    dy2 <- d()[NUMERO_CANDIDATO==mouse()$NUMERO_CANDIDATO]
    #dy2 <- d[NUMERO_CANDIDATO==mouse$NUMERO_CANDIDATO]
    dy3_temp <- merge(mun_state_contig(),dy2, by.x="GEOCOD",by.y="COD_MUN_IBGE",all.x=TRUE)
    #dy3_temp <- merge(mun_state_contig,dy2, by.x="GEOCOD",by.y="COD_MUN_IBGE",all.x=TRUE)
    dy3 <- dy3_temp
  })
  
  output$map_selected <- renderLeaflet({
    pal <- colorBin(palette=c("white","light blue","#fcbba1","#fb6a4a","#ef3b2c","#cb181d"),domain=c(0,1000), bins=c(1000,50,10,5,1,0.01,0), na.color="white")
    popup_text <- paste0(dy3()@data[,"NOME"],"<br> Valid Votes: ",dy3()@data[,"Tot_Mun"]," (",round((dy3()@data[,"Tot_Mun"]/dy3()@data[,"Tot_State"])*100,1),"% of State Total)","<br>",dy3()@data[,"NOME_URNA_CANDIDATO"]," received ",dy3()@data[,"QTDE_VOTOS"]," votes (",round((dy3()@data[,"QTDE_VOTOS"]/dy3()@data[,"Tot_Deputado"])*100,1),"% of their Statewide Total)","<br> Medida QL: ",round(dy3()@data[,"LQ"],3))
    leaflet() %>% addProviderTiles("CartoDB.Positron") %>% clearBounds() %>% addPolygons(data=state_shp(),fillOpacity=0,weight=3,color="black",fillColor=NULL) %>% addPolygons(data=dy3(), layerId=dy3()@data[,"LQ"],fillOpacity=0.8,weight=0.1,color=NA,fillColor=pal(dy3()@data[,"LQ"]), popup=popup_text) %>% addLegend(position="bottomleft", pal=pal,values=dy3()@data[,"LQ"],opacity=0.8) 
  })
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(d_uniq[d_uniq$anoEleicao==input$Year & d_uniq$sigla_UF==input$State,], hover, threshold = 5, maxpoints = 1, addDist = TRUE)
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
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> ", point$NOME_URNA_CANDIDATO, "<br/>",
                    "<b> ", point$NUMERO_PARTIDO, "<br/>")))
    )
  })
  
  filtered <- reactive({
    filtered <- d_uniq %>% filter(anoEleicao==input$Year,sigla_UF==input$State) %>% distinct(NOME_URNA_CANDIDATO,NUMERO_PARTIDO,G_Index,MoranI,NUMERO_CANDIDATO) %>% top_n(5,G_Index)
    filtered_low <- d_uniq %>% filter(anoEleicao==input$Year,sigla_UF==input$State) %>% distinct(NOME_URNA_CANDIDATO,NUMERO_PARTIDO,G_Index,MoranI,NUMERO_CANDIDATO) %>% top_n(-5,G_Index)
    filtered <- rbind(filtered,filtered_low)
    
    parties <- melt(data.frame("PRB"=10,"PP"=11,"PDT"=12,"PT"=13,"PTB"=14,"PMDB"=15,"PSTU"=16,"PSL"=17,"REDE"=18,"PTN"=19,"PSC"=20,"PCB"=21,"PR"=22,"PPS"=23,"DEM"=25,"PSDC"=27,"PRTB"=28,"PCO"=29,"NOVO"=30,"PHS"=31,"PMN"=33,"PMB"=35,"PTC"=36,"PSB"=40,"PV"=43,"PRP"=44,"PSDB"=45,"PSOL"=50,"PEN"=51,"PPL"=54,"PSD"=55,"PCdoB"=65,"PTdoB"=70,"SD"=77,"PROS"=90))
    filtered <- merge(filtered,parties,by.x="NUMERO_PARTIDO",by.y="value",all.x=TRUE,all.y=FALSE)
    filtered <- as.data.table(filtered)
    filtered[,NUMERO_PARTIDO:=NULL] 
    filtered <- filtered  %>% arrange(desc(G_Index))
    setnames(filtered,c("NOME_URNA_CANDIDATO","G_Index","MoranI","variable","NUMERO_CANDIDATO"),c("Candidate","G Index","Moran's I","Party","Candidate Number"))
    setcolorder(filtered,c("Candidate","Party","G Index","Moran's I","Candidate Number"))
    filtered
  })

  output$Extremes <- renderDataTable({
    filtered2 <- filtered()[,!names(filtered())=="Candidate Number"]
    
    datatable(filtered2, rownames=TRUE, options=list(dom = 't',columnDefs = list(list(visible=FALSE, targets=c("Party Number","Candidate Number")))), selection='single', style = 'bootstrap', class = 'table-bordered') %>% formatRound(c("G Index","Moran's I"),3) %>% formatStyle('G Index',target='row',backgroundColor=styleInterval(median(filtered2[['G Index']],na.rm=TRUE),c('lightblue','lightgreen')))
  })

   candidato_hi <- eventReactive(input$Extremes_row_last_clicked,{
    candidato_hi <- filtered()[input$Extremes_row_last_clicked,"Candidate Number"]
    candidato_hi
  })
  
  d_hi <- eventReactive(input$Extremes_row_last_clicked,{
    beginning <- Sys.time()
    vars <- list("UF","NUMERO_PARTIDO","ANO_ELEICAO","COD_MUN_IBGE","QTDE_VOTOS","NUMERO_CANDIDATO","SIGLA_PARTIDO","NOME_URNA_CANDIDATO","DESC_SIT_TOT_TURNO")
    names(vars) <- rep("selected_columns[]",length(vars))
    filter <- list("columns[0][name]"="UF","columns[0][search][value]"=input$State,"columns[1][name]"="NUMERO_CANDIDATO","columns[1][search][value]"=candidato_hi())
    consulta <- append(append(list(cached=TRUE,anos = input$Year,uf=input$State,agregacao_regional=6, agregacao_politica=2, cargo=6),vars),filter)
    d <- content(GET(url,query=consulta),type="text/csv")
    d <- data.table(d)
    
    if (dim(d)[1]!=0){
      
      #Ideally will be faster when can request specific state
      setkeyv(d,c('ANO_ELEICAO','COD_MUN_IBGE','NUMERO_CANDIDATO'))
      
      #### Aggregations
      d <- merge(d,mun_totals(),by="COD_MUN_IBGE")
      d <- merge(d,state_totals(),by="UF")

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
    d_hi
  })
  
  extreme_d <- reactive({
    extreme_d_temp <- d_hi()[NUMERO_CANDIDATO==candidato_hi()]
    extreme_d_temp <- merge(mun_state_contig(),extreme_d_temp, by.x="GEOCOD",by.y="COD_MUN_IBGE",all.x=TRUE)
    extreme_d <- extreme_d_temp
  })
  
  output$map_selected_hi <- renderLeaflet({
    pal <- colorBin(palette=c("white","light blue","#fcbba1","#fb6a4a","#ef3b2c","#cb181d"),domain=c(0,1000), bins=c(1000,50,10,5,1,0.01,0), na.color="white")
    popup_text <- paste0(extreme_d()@data[,"NOME"],"<br> Valid Votes: ",extreme_d()@data[,"Tot_Mun"]," (",round((extreme_d()@data[,"Tot_Mun"]/extreme_d()@data[,"Tot_State"])*100,1),"% of State Total)","<br>",extreme_d()@data[,"NOME_URNA_CANDIDATO"]," received ",extreme_d()@data[,"QTDE_VOTOS"]," votes (",round((extreme_d()@data[,"QTDE_VOTOS"]/extreme_d()@data[,"Tot_Deputado"])*100,1),"% of their Statewide Total)","<br> Medida QL: ",round(extreme_d()@data[,"LQ"],3))
    leaflet() %>% addProviderTiles("CartoDB.Positron") %>% clearBounds() %>% addPolygons(data=state_shp(),fillOpacity=0,weight=3,color="black",fillColor=NULL) %>% addPolygons(data=extreme_d(), layerId=extreme_d()@data[,"LQ"],fillOpacity=0.8,weight=0.1,color=NA,fillColor=pal(extreme_d()@data[,"LQ"]), popup=popup_text) %>% addLegend(position="bottomleft", pal=pal,values=extreme_d()@data[,"LQ"],opacity=0.8) 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

