library(shinythemes)
library(shinyBS)
library(mapview)
library(shinyalert)
library(leaflet)

sidebarPanelUi <- function () {
  return(
    tags$div(class="pad-20",
      useShinyalert(),
      h4("Opções:"),
      selectizeInput(
        "State",
        label = NULL,
        selected = NULL,
        choices = c("", "AC", "AM", "AL", "AP", "BA", "CE", "ES", "GO", "MA", "MS", "MG", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP","TO"),
        options = list(
          placeholder = "Selecione um estado"
        )
      ),
      selectizeInput(
        "cargo",
        label = NULL,
        selected = NULL,
        choices = c("",
          "Governador" = 3,
          "Senador"    = 5,
          "Deputado Federal" = 6,
          "Deputado Estadual" = 7),
        options = list(
          placeholder = "Selecione um cargo"
        )
      ),
      selectizeInput(
        "Year",
        label = NULL,
        selected = NULL,
        choices = c("", 1998, 2002, 2006, 2010, 2014, 2018),
        options = list(
          placeholder = "Selecione um ano"
        )
      ),
      uiOutput("turno_UI"),
      uiOutput("party_UI"),
      radioButtons(
        "Indicator",
        label = "Indicador:",
        choices = list(
          "Proporção de Votos do Partido" = 1,
          "Proporção de Votos no Município" = 2,
          "Medida QL"
        ),
        selected = "2"
      ),
      actionButton("button", label = strong("Atualizar"), class = "btn-primary btn-block"),
      bsTooltip(
        "cargo",
        "Todas as eleições onde o distrito eleitoral é o estado estão disponíveis",
        "right",
        options = list(container = "body")
      ),
      bsTooltip(
        "party_UI",
        "Escolha todos os partidos para pesquisar por nome do candidato na caixa abaixo",
        "right",
        options = list(container = "body")
      ),
      radioTooltip(
        id = "Indicator",
        choice = 2,
        title = "O percentual de votos válidos no município recebidos pelo candidato.",
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      ),
      radioTooltip(
        id = "Indicator",
        choice = "Medida QL",
        title = "A Medida QL indica quantas vezes mais votos o candidato recebeu no município em comparação com se ele tivesse recebido apoio igual em todo o estado. A QL é determinada pela razão entre duas proporções: (i) a proporção dos votos obtidos pelo candidato no município com relação à votação total do candidato no estado, e (ii) o número de eleitores do município sobre o eleitorado total do estado. QLs maiores que um indicam votação superior à esperada e potenciais bases eleitorais dos candidatos.",
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      ),
      conditionalPanel(
        condition = 'input.button > 0',
        downloadButton('downloadMap', class="btn btn-success btn-block mt-10", label = "Download")
      )
    )
  )
  
}

mapTabPanelUi <- function () {
  return(tabPanel(
    "Mapa",
    div(
      class = "outer",
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }",
                 "#controlPanel {background-color: rgba(255,255,255,0.8);}",
                 ".leaflet-top.leaflet-right .leaflet-control {
      margin-right: 10px; margin-top: 250px;
    }"),
      leafletOutput("map", width = "100%", height = "100%")
    ),
    absolutePanel(
      draggable = FALSE,
      top = "auto",
      left = "auto",
      right = 20,
      bottom = 20,
      width = "auto",
      height = "auto",
      actionButton("map_zoom_in", "+"),
      actionButton("map_zoom_out", "-")
    ),
    bootstrapPage(
      absolutePanel(
        id = "note",
        class = "panel panel-default",
        fixed = TRUE,
        draggable = FALSE,
        top = 60,
        left = "auto",
        right = 30,
        bottom = "auto",
        width = 200,
        height = "auto",
        HTML(
          '<button data-toggle="collapse" data-target="#demo">Indicadores</button>'
        ),
        tags$div(
          id = 'demo',
          class = "collapse in pad-20",
          htmlOutput("Indicators1"),
          tipify(
            htmlOutput("Indicators2"),
            "O Índice G mede o desvio de apoio do candidato em todo o estado de uma distribuição uniforme de apoio em proporção perfeita à população local. G = 0 indica uma taxa uniforme de conversão da população aos votos, e G = 1 indica concentração perfeita de apoio eleitoral em apenas um município.",
            "left"
          ),
          tipify(
            htmlOutput("Indicators3"),
            "O Morans I mede a correlação espacial de votos. Valores maiores indicam que o apoio do candidato está concentrado em um pequeno número de clusters geográficos. Identificamos vizinhos municipais com base nos 6 municípios vizinhos mais próximos.",
            "left"
          ),
          tipify(
            htmlOutput("Indicators4"),
            "O número de clusters geográficos estatisticamente significativos de votação (fronteiras verdes no mapa) com base na medida da QL e LISA (Indicadores Locais de Autocorrelação Espacial)",
            "left"
          )
        )
      )
    )
  ))
}


aboutTabPanelUi <- function () {
  return (
    tabPanel("Sobre",
             column(width = 3, ""),
             column(
               width = 8, h4("Sobre CepespMapas"), htmlOutput("Note")
             ))
  )
}

# ROOT UI COMPONENT
spatial2Ui <- function () {
  return (
    
    tagList(
      
      tags$head(includeCSS("styles.css")),
      tags$div(class = "btn-header", checked = NA,
               tags$a(href = "http://cepespdata.io/", class="btn btn-primary", "Ir para CepespData")),
      
      
      navbarPage("Spatial Maps - Partidos", theme = shinytheme("lumen"), collapsible = TRUE, fluid = TRUE,
                 
                 mapTabPanelUi(),
                 aboutTabPanelUi(),
                 
                 absolutePanel(id = "controls", class = "panel panel-primary", fixed = F,
                               draggable = F, top = 60, left = 10, right = "auto", bottom = "auto",
                               width = 260, height = "auto", sidebarPanelUi())
    ))
    
  )
}