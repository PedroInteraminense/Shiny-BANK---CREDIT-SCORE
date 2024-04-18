# Leitura dos Pacotes ####

#   ao subir novas bases. 
rm(list = ls())



library(stringr)
library(caret)
library(ROSE)
library(glue)
library(pROC)
library(tidyverse)
library(formattable)
library(openxlsx)
library(readxl)
library(zoo)
library(shinydashboardPlus)
library(dashboardthemes)
library(highcharter)
library(stringr)
library(DT)
library(shinydashboard)
library(shiny)
library(shinyjs)
library(lubridate)
library(htmlwidgets)
library(shinycssloaders)
library(shinyWidgets)
library(shinyalert)
library(shinyBS)
library(rhandsontable)
library(magrittr)
require(rpivotTable)
library(rio)
library(highcharter)
library(scales)
library(xgboost)
options(scipen = 999,
        shiny.maxRequestSize = 100*1024^2)


#### Carregando Módulos ----------------------------------------------------------
source("modulos/inputs/produtos_ui_harmonia.R", encoding = "UTF-8")
source("modulos/inputs/produtos_server_harmonia.R", encoding = "UTF-8")

source("modulos/analytics/analytics_score_ui.R", encoding = "UTF-8")
source("modulos/analytics/analytics_score_server.R", encoding = "UTF-8")

source("modulos/training/training_ui.R", encoding = "UTF-8")
source("modulos/training/training_server.R", encoding = "UTF-8")

#### Carregando Módulos ----------------------------------------------------------


# ---



# Design ######################################################################

brbg <- hsv(0.5, .35, seq(.25, .95, length.out = 12))

logo_blue <- shinyDashboardLogoDIY(
  boldText = ""
  , mainText = ""
  , textSize = 16
  , badgeText = ""
  , badgeTextColor = "white"
  , badgeTextSize = 0
  , badgeBackColor = "#"
  , badgeBorderRadius = 3
)


header <- uiOutput("ui_menu_top")

sidebar <- uiOutput("mainsidebar")

body <- dashboardBody(
  tags$head(
    # Atualizar - Favicon (imagem miniatura) do cliente
    tags$link(rel = "shortcut icon", href = "img/favicon2.png"),
    tags$div(HTML('<link rel="preconnect" href="https://fonts.googleapis.com">
                       <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
                       <link href="https://fonts.googleapis.com/css2?family=Grape+Nuts&display=swap" rel="stylesheet">')), # Texto dos títulos
    
    tags$div(HTML('<link rel="preconnect" href="https://fonts.googleapis.com">
                       <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
                       <link href="https://fonts.googleapis.com/css2?family=Akshar:wght@300;400;500&family=Grape+Nuts&display=swap" rel="stylesheet">')) # demais textos
  ),
  tags$link(rel = "stylesheet", type = "text/css", href = "css/bootstrap.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "css/style_dash.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "css/style_button.css"),
  useShinyalert(),
  useShinyjs(),
  uiOutput("mainbody")
)


ui <- dashboardPage(
  title = "BANK - CREDIT SCORE",
  header,
  sidebar,
  body
)


loader = function(element) {
  element %>% 
    withSpinner(type = getOption("spinner.type", default = 6),
                color = "#050f29")
}



# Servidor ####################################################################

server <- function(input, output, session) {
  
  # Login ------------
  addClass(selector = ".sidebar-toggle", class = "sidebar-toggle-none")
  
  Logged <- TRUE
  USER <- reactiveValues(Logged = Logged)
  
  # Atualizar - Token de acesso - Firebase
  tokenLogin <- reactive({
    if (input$userName == "") {
      tokenLogin <- ""
    } else {
      tokenLogin <- ""
    }
    
    tokenLogin
  })
  
  # login
  observeEvent(input$Login, {
    token <- auth(tokenLogin(),
                  email = input$userName,
                  password = input$passwd
    )
    
    
    if (length(token$registered) > 0) {
      USER$Logged <- TRUE
    } else {
      output$login_fail <- renderText("Email ou senha errado, tente novamente")
    }
  })
  
  # logout
  observeEvent(input$Logout, {
    USER$Logged <- FALSE
  })
  

  
  ### Interface #################################################################
  
  output$mainsidebar <- renderUI({
    if (USER$Logged == TRUE) {
      uiOutput("sidebarpanel")
    } else if (USER$Logged == FALSE) {
      NULL
    }
  })
  
  # sidebar login
  output$sidebarpanel <- renderUI({
    
    removeClass(selector = ".sidebar-toggle", class = "sidebar-toggle-none")
    
    dashboardSidebar(
      sidebarUserPanel("User Logged",
                       image = "img/favicon2.png"
      ),
      
     
      
      sidebarMenu(
        id = "tabs",
        menuItem("Inputs", tabName = "tab_produtos", icon = icon("database"), selected = T),
        menuItem("Analytics", tabName = "tab_analytics", icon = icon("percentage")),
        menuItem("Training", tabName = "tab_training", icon = icon("database"))
      )
      
    )
  })
  
  # sidebar_logout
  output$sidebarpanel_logout <- renderUI({
    NULL
  })
  
  
  # Header UI
  output$ui_menu_top <- renderUI({
    if (USER$Logged == TRUE) {
      
      # Opção de header - Ajustar ao cliente - adicionando a logo
      dashboardHeader(
        title = tagList(
          # Atualizar -  Logo cliente
          img(
            class = "logo-lg", 
            src = "img/favicon3.png", 
            width = "55px",
            style = "padding: 5px; margin: 0px auto;"
          ),
          # Atualizar - Logo miniatura do cliente
          img(
            class = "logo-mini", 
            style = "width: 42px; margin-left: -10px; margin-top: 5px;", 
            src = "img/projetic.png"
          )
        ),
        # Atualizar - Texto ou imagem que ficarão a direita - Caso seja necessário
        tags$li(
          class = "dropdown",
          h1(class="title-header", "CREDIT SCORE  ")
        )
      )
      
      # Caso queira que não apareça nada - Adicionar função vazia
      # dashboardHeader()
      
    } else if (USER$Logged == FALSE) {
      NULL
    }
    
  })
  
  
  
  
  ### Body ######################################################################
  
output$mainbody <- renderUI({
  if (USER$Logged == TRUE) {
    uiOutput("body")
  } else {
    if (USER$Logged == FALSE) {
      uiOutput("body_logout")
    }
  }
})
  
  
  
  
  output$body <- renderUI({
    tabItems(
      produtos_ui(
        id = "tab_produtos"
      ),
      analytics_ui(
        id = "tab_analytics"
      ),
      training_ui(
        id = "tab_training"
      )

    )
  })

  
  
  
 # LOGIN - Página ----
  output$body_logout <- renderUI({

    if (USER$Logged == FALSE && is.null(input$Login) || input$Login == 0) {


      fluidPage(
        tags$div(
          class = "loginColumns animated fadeInDown",
          style = "padding: 120px 20px 20px 20px; max-width: 1000px;"
        ),
        fluidRow(
          column(
            width = 7,
            class = "box-login",
            box(
              width = 11,
              style = "padding: 20px 20px;",
              title = "Entrar",
              textInput("userName", "Email:"),
              passwordInput("passwd", "Senha:"),
              br(),
              textOutput("login_fail"),
              actionButton("Login", "Log in")
            )
          ),
          column(
            width = 5,
            class = "text-center",
            tags$img(
              src = "./img/login.png",
              width = "90%",
              style = "margin:20px; margin-top: 40px;"
            )
          )
        )
      )

    } else if (USER$Logged == FALSE && input$Login > 0) {

      fluidPage(
        tags$div(
          class = "loginColumns animated fadeInDown",
          style = "padding: 100px 20px 20px 20px; max-width: 1000px;"
        ),
        fluidRow(
          column(
            width = 7,
            class = "box-login",
            box(
              width = 11,
              title = "Entrar", textInput("userName", "Username"),
              passwordInput("passwd", "Password"),
              br(),
              textOutput("login_fail"),
              tags$head(tags$style("#login_fail{color: red;}")),
              actionButton("Login", "Log in")
            )

          ),
          column(
            width = 5,
            class = "text-center",
            tags$img(
              src = "./img/login.png",
              width = "90%",
              style = "margin:20px; margin-top: 40px;"
            )
          )
        )
      )
    }
  })


  
  
  ## Output's  -----------------------------------------------------------------
  
  produtos <- callModule(
    module = produtos_server,
    id = "tab_produtos"
  )
  

  analytics <- callModule(
    module = analytics_server,
    id = "tab_analytics"
  )
  
  training <- callModule(
    module = training_server,
    id = "tab_training"
  )
  

  
 
}

shinyApp(ui = ui, server = server)
