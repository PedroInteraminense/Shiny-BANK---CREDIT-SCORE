training_ui <- function(id) {
  
  ns = NS(id)
  
  
  tabItem(
    
    class = "active",
    
    tabName = "tab_training",
    
    fluidRow(
      
      actionButton(ns("processa_base"),
                   class = "btn-primary btn-lg",
                   style = "margin-top: 25px; margin-left: auto; margin-right: auto; display: block;",
                   "To prepare databases",
                   icon = icon("pencil-alt")),
      
      actionButton(ns("training_model"),
                   class = "btn-success btn-lg",
                   style = "margin-top: 50px; margin-left: auto; margin-right: auto; display: block;",
                   "Training Model",
                   icon = icon("cogs")),
      
      box(title = "Processed database",
      width = 12,
      
      
      DTOutput(ns("tabela_credit_store_traning")),
      
    ),
    box(title = "Validation Model",
        width = 12,
        
        plotOutput(ns("valid_model")),
        )
    ),
    
    
)
  
}