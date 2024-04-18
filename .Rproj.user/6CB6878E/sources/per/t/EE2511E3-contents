analytics_ui <- function(id) {
  
  ns = NS(id)
  
  tabItem(
    
    class = "active",
    
    tabName = "tab_analytics",
    
    fluidRow(
       h2("Correlation matrix", style = "margin-bottom: 5%;"),
             box(width = 12,
                 title = NULL,
      highchartOutput(ns("plot_matrix_corr"))
      
    ),
    
    # fluidRow(
    #   tabsetPanel(
    #     tabPanel("Marital Status",
    #              fluidRow(
    #                column(12,
    #                       style = "margin-bottom: 5%;",
    #                       loader(DT::dataTableOutput(ns("plot_marital_status")))))
    #     )
    # )),
    
    tabBox(id = ns("tabset_analytics"),
           width = 12,
           tabsetPanel(
             tabPanel("Marital Status",
                      # fluidRow(
                      #   style = "margin-bottom: 5%;",
                      #  # actionButton(ns("reset_filters_category"), "Redefinir Filtros")
                      # ),
                      fluidRow(
                        column(12,
                               style = "margin-bottom: 5%;",
                               highchartOutput(ns("plot_marital_status"))))
             ),
             
             tabPanel("Income Category",
                      # fluidRow(
                      #   style = "margin-bottom: 5%;",
                      #  # actionButton(ns("reset_filters_category"), "Redefinir Filtros")
                      # ),
                      fluidRow(
                        column(12,
                               style = "margin-bottom: 5%;",
                               highchartOutput(ns("plot_Income_Category"))))
             ),
             tabPanel("Educational level",
                      # fluidRow(
                      #   style = "margin-bottom: 5%;",
                      #  # actionButton(ns("reset_filters_category"), "Redefinir Filtros")
                      # ),
                      fluidRow(
                        column(12,
                               style = "margin-bottom: 5%;",
                               highchartOutput(ns("plot_Education_Level"))))
             ),
             tabPanel("Card Category",
                      # fluidRow(
                      #   style = "margin-bottom: 5%;",
                      #  # actionButton(ns("reset_filters_category"), "Redefinir Filtros")
                      # ),
                      fluidRow(
                        column(12,
                               style = "margin-bottom: 5%;",
                               highchartOutput(ns("plot_Card_Category"))))
             
             
             ))),
    
    fluidRow(
      box(
        title = "Resume",
        width = 4,
        selectInput(ns("select"), "Escolha o atributo:",
                    choices = c("Marital_Status", "Income_Category", 
                                "Education_Level", "Card_Category")),
        
        uiOutput(ns("tabela"))
      ),
      box(Title = "desc",
          width = 8,
         
          tabsetPanel(
          tabPanel("Total Trans Ct",
                   # fluidRow(
                   #   style = "margin-bottom: 5%;",
                   #  # actionButton(ns("reset_filters_category"), "Redefinir Filtros")
                   # ),
                   fluidRow(
                     column(12,
                            style = "margin-bottom: 5%;",
                            highchartOutput(ns("grafico_Total_Trans_Ct"))))
          ),
          tabPanel("Total Revolving Bal",
                  
                   fluidRow(
                     column(12,
                            style = "margin-bottom: 5%;",
                            highchartOutput(ns("grafico_Total_Revolving_Bal"))))
          ),
          
          tabPanel("Avg Utilization Ratio",
                 
                   fluidRow(
                     column(12,
                            style = "margin-bottom: 5%;",
                            highchartOutput(ns("grafico_Avg_Utilization_Ratio"))))
          ),
          
          tabPanel("Total Ct Chng Q4 Q1",
                
                   fluidRow(
                     column(12,
                            style = "margin-bottom: 5%;",
                            highchartOutput(ns("grafico_Total_Ct_Chng_Q4_Q1"))))
          ),
          tabPanel("Total Ct Chng Q4 Q1",
                   
                   fluidRow(
                     column(12,
                            style = "margin-bottom: 5%;",
                            highchartOutput(ns("Total_Trans_Ct2"))))
          )
          ),
          
         
          
          )
      
    ),
    
    fluidRow(
      
    box(
      title = "Relative Frequency",
      width = 12,
      HTML("This dataset displays customer attributes (gender, education level, marital status, income category,
           and card category) grouped together, with the number of attrited customers in each group, sorted from 
           the group with the highest number of attrited customers to the lowest. This provides insight into which 
           customer groups are more likely to churn."),
      
      DTOutput(ns("process_dataa"))
      
    )
    ),
    
    fluidRow(
      
      box(
        title = "Pattern Identification",
        width = 12,
        HTML("These charts are useful for identifying patterns, anomalies, or insights about the variables in your 
             data, aiding in exploratory analysis and making informed decisions."),
        
        tabsetPanel(
          
          tabPanel("Total_Trans_Ct",
            highchartOutput(ns("h1")),
            highchartOutput(ns("h2"))
          ),
          
          
          tabPanel("Avg Utilization Ratio",
                   highchartOutput(ns("h3")),
                   highchartOutput(ns("h4"))
          ),
          
          tabPanel("Avg Total_Ct_Chng_Q4_Q1 Ratio",
                   highchartOutput(ns("h5")),
                   highchartOutput(ns("h6"))
          ),
          tabPanel("Total Trans Amt",
                   highchartOutput(ns("h7")),
                   highchartOutput(ns("h8"))
          ),
          tabPanel("Total Amt Chng Q4 Q1",
                   highchartOutput(ns("h9")),
                   highchartOutput(ns("h10"))
          ),
          tabPanel("Avg Open To Buy",
                   highchartOutput(ns("h11")),
                   highchartOutput(ns("h12"))
          ),
          tabPanel("Total Revolving Bal",
                   highchartOutput(ns("h13")),
                   highchartOutput(ns("h14"))
          ),
          tabPanel("Credit Limit",
                   highchartOutput(ns("h15")),
                   highchartOutput(ns("h16"))
          ),
          tabPanel("Contacts Count 12 mon",
                   highchartOutput(ns("h17")),
                   highchartOutput(ns("h18"))
          ),
          tabPanel("Months Inactive 12 mon",
                   highchartOutput(ns("h19")),
                   highchartOutput(ns("h20"))
          ),
          tabPanel("Total Relationship Count",
                   highchartOutput(ns("h21")),
                   highchartOutput(ns("h22"))
          ),
          tabPanel("Months on book",
                   highchartOutput(ns("h23")),
                   highchartOutput(ns("h24"))
          ),
          tabPanel("Dependent count",
                   highchartOutput(ns("h25")),
                   highchartOutput(ns("h26"))
          ),
          tabPanel("Customer Age",
                   highchartOutput(ns("h27")),
                   highchartOutput(ns("h28"))
          ),
        ),
        
        
      
      )
    ),
    
    
    tags$script(src = "js/index.js")
    
  )
  )
  
}