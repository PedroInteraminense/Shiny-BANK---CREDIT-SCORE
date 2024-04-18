produtos_ui <- function(id) {
  
  ns = NS(id)
  
  tabItem(
    
    class = "active",
    
    tabName = "tab_produtos",
    
    fluidRow(
      column(8, h2("Import files", style = "margin-bottom: 5%;")),
      
      
    ),
    
    # fluidRow(
    #   column(3,
    #          selectInput(inputId = ns("input_data"), 
    #                      label = "Selecione a data:", 
    #                      choices = c(format(as.Date(Sys.Date()) %m-% months(1:5), "%m/%Y")),
    #                      width = "80%"
    #          )
    #   ),
    #   
    #   
    #   
    # ),
    
    fluidRow(
      column(3,
             fileInput(
               ns("importar_credit_score"),
               "Base Credit Score",
               buttonLabel = "Search for file",
               width = "80%"
             )
      ),
      
    
      
      # column(width = 3,
      #        style = "margin-top: 25px;",
      #        actionButton(ns("importar_base_historico_vendedor"),
      #                     "Preparar bases",
      #                     icon = icon("pencil-alt")),
      # ),
      
      
    ),
    
    
    fluidRow(
      
      h2("Overall Base", style = "margin-left: 1.5%; margin-top: 4%;"),
      
      box(width = 12,
          title = NULL,
          
          actionButton(ns("button_base_overall"),
                       "Edit",
                       width = '12%',
                       icon = icon("edit")),
          
          
          uiOutput(ns('tabela_credit_store'))
          
      )
      
      
    ),
    
    
  

    
    
    # fluidRow(
    # 
    #   h2(""),
    #   align = "right",
    #   tagList(
    # 
    # 
    #   ),
    # 
    #   # box(width = 12, title = "Inconsistências",
    #   #     fluidRow(
    #   #       h3("Produtos",  style = "margin-left: 1.5%; margin-bottom: 3%;"),
    #   #       column(width = 12, dataTableOutput(ns("incons_prod")))
    #   #       ),
    #   #     fluidRow(
    #   #       h3("Funcionários",  style = "margin-left: 1.5%; margin-bottom: 3%;"),
    #   #       column(width = 12, dataTableOutput(ns("incons_func")))
    #   #       ),
    #   #
    #   #
    #   #     fluidRow(
    #   #       h3("Painel",  style = "margin-left: 1.5%; margin-bottom: 3%;"),
    #   #       column(width = 12, dataTableOutput(ns("incons_pn")))
    #   #     )
    #   # ),
    #   
    #   tabBox(id = ns("tabset_produtos"),
    # 
    #          width = 12,
    #          title = "Inconsistências",
    # 
    #          tabPanel("Produtos",
    #                   fluidRow(
    #                     title = NULL
    #                   ),
    #                   fluidRow(
    #                     column(12,
    #                            style = "margin-bottom: 5%;",
    #                            loader(DT::dataTableOutput(ns("incons_prod")))))
    #          ),
    #          # 
    #          # 
    #          # tabPanel("Funcionários",
    #          #          fluidRow(
    #          #            title = NULL
    #          #          ),
    #          #          fluidRow(
    #          #            column(12,
    #          #                   style = "margin-bottom: 5%;",
    #          #                   loader(DTOutput(ns("incons_func")))))
    #          # ),
    #          # #
    #          # tabPanel("Painel",
    #          #          fluidRow(
    #          #            title = NULL
    #          #          ),
    #          #          fluidRow(
    #          #            column(12,
    #          #                   style = "margin-bottom: 5%;",
    #          #                   loader(DTOutput(ns("incons_pn")))))
    #          # ),
    #          # 
    #          # 
    #          # tabPanel("Estrutura Comercial - Fechamento",
    #          #          fluidRow(
    #          #            title = NULL
    #          #          ),
    #          #          fluidRow(
    #          #            column(12,
    #          #                   style = "margin-bottom: 5%;",
    #          #                   loader(DTOutput(ns("incons_estrutura_fechamento")))))
    #          # ),
    #          
    #          # tabPanel("Histórico vendedor - Cliven",
    #          #          fluidRow(
    #          #            title = NULL
    #          #          ),
    #          #          fluidRow(
    #          #            column(12,
    #          #                   style = "margin-bottom: 5%;",
    #          #                   loader(DTOutput(ns("historico_vendedor_incons")))))
    #          # ),
    # 
    #   )),





    # fluidRow(
    #
    #   h2("Hierarquia de produtos", style = "margin-left: 1.5%; margin-top: 4%;"),
    #
    #   box(width = 12,
    #       title = NULL,
    #
    #       actionButton(ns("btn_editar"),
    #                    "Editar",
    #                    width = '12%',
    #                    icon = icon("edit")),
    #
    #
    #       uiOutput(ns('ui_tabela_produtos'))
    #
    #   )
    # ),
    

    
    tags$script(src = "js/index.js")
    
  )
  
}

