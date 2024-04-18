produtos_server <- function(input, output, session) {
  
  
  # # Input da data que vai para o nome dos arquivos a serem baixados
  # input_data <- reactive({
  #   input$input_data %>% 
  #     my() %>% as.character() %>% 
  #     str_sub(1, 7)
  # })
  # 
  # # Input da data que vai para a aba outputs
  # input_data_asDate <- reactive({
  #   # coloca no último dia do mês
  #   paste0("01/", input$input_data) %>% dmy() %m+% months(1) %m-% days(1) %>% 
  #     as.POSIXlt() %>% as.Date()
  # })
  
  bases <<- reactiveValues(
    credit_score = NULL
  )
  
  
  observe({
    
    req(input$importar_credit_score)
    
    input_arquivo <- input$importar_credit_score
    if (is.null(input_arquivo)) { return() } 
    src <- input_arquivo$datapath
    colunas <- c("CLIENTNUM","Attrition_Flag","Customer_Age","Gender","Dependent_count",
                 "Education_Level","Marital_Status","Income_Category","Card_Category",
                 "Months_on_book","Total_Relationship_Count","Months_Inactive_12_mon",
                 "Contacts_Count_12_mon","Credit_Limit","Total_Revolving_Bal",
                 "Avg_Open_To_Buy","Total_Amt_Chng_Q4_Q1","Total_Trans_Amt",
                 "Total_Trans_Ct","Total_Ct_Chng_Q4_Q1","Avg_Utilization_Ratio",
                 "Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1","Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2")
    
    
    
    dados = read_excel(src)
    colunas_faltantes = colunas[!colunas %in% (dados %>% names)]
    
    msg <- ifelse(!is.null(need(tools::file_ext(src) == "xlsx", " ")),
                  "Verifique se o arquivo selecionado é do formato .xlsx",
                  ifelse(
                    length(colunas_faltantes) != 0,
                    paste0('Falta as colunas: ',paste0("'",colunas_faltantes,"'", collapse = ', ')),
                    ""
                  ))
    
    
    if (msg != "") {   # avisa se o arquivo estiver na extensão errada ou com as variáveis erradas
      shinyalert(
        title = "Arquivo incorreto",
        text = msg,
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      reset("credit_score")
      return(NULL)
    } else {
      bases$credit_score <- dados %>% 
        select(-c("CLIENTNUM", 
                  "Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1", 
                  "Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2")) %>% 
        mutate(Avg_Utilization_Ratio = as.numeric(Avg_Utilization_Ratio),
               Credit_Limit = as.numeric(Credit_Limit),
               Avg_Open_To_Buy = as.numeric(Avg_Open_To_Buy),
               Total_Ct_Chng_Q4_Q1 = as.numeric(Total_Ct_Chng_Q4_Q1),
               Total_Amt_Chng_Q4_Q1 = as.numeric(Total_Amt_Chng_Q4_Q1),
               Avg_Open_To_Buy = as.numeric(Avg_Open_To_Buy))
      
      
  
      shinyalert(
        title = "Database inserted into the system!",
        text = " ",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "success",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )

    }
    
    
    
  })
  
  

  
  tabela_editavel <- FALSE 
  
  observeEvent(input$button_base_overall, {
    req(bases$credit_score)
    if (tabela_editavel == TRUE) {
      tabela_editavel <<- FALSE
      
      bases$credit_score <- hot_to_r(input$tabela_credit_store_hot)
      # salva a base editada e sai do modo editável
      saveRDS(bases$credit_score, "./dados/credit_score.rds") 
      
      updateActionButton(session,
                         'button_base_overall',
                         " Editar",
                         icon = icon("edit"))
    } else {
      tabela_editavel <<- TRUE
      
      updateActionButton(session,
                         'button_base_overall',
                         " Salvar",
                         icon = icon("save"))
    }
    
  })
  
  
  observe({
    
    output$tabela_credit_store <- renderUI({
      
      if (is.null(input$button_base_overall) | tabela_editavel == FALSE) {
        fluidRow(
          column(12,
                 shinycssloaders::withSpinner(DTOutput(session$ns("tabela_credit_store_data_table")))
          )
        )
      } else if (!is.null(input$button_base_overall) & tabela_editavel == TRUE) {
        fluidRow(
          column(12, style = "margin-top: 3.5%;",
                 shinycssloaders::withSpinner(rHandsontableOutput(session$ns("tabela_credit_store_hot")))
          )
        )
      }
      
    })
    
  })
  
  
  output$tabela_credit_store_data_table <- renderDT(server = FALSE, {
    
 

    head(bases$credit_score,100) %>% 
      datatable(extensions = 'Scroller',
                selection = "single",
                fillContainer = TRUE,
                rownames = FALSE,
                callback = JS("$.fn.dataTable.ext.errMode = 'none';"),
                options = list(
                  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'),
                  dom = 'ft',
                  deferRender = TRUE,
                  paging = FALSE,
                  scrollY = 350,
                  scroller = TRUE,
                  fixedColumns = TRUE,
                  columnDefs = list(
                    list(orderable = FALSE, targets = "_all", className = "dt-center"))
                ))
    
  })
  
  
  
  # Tabela produtos HandsOnTable ----
  # Tabela modo editável
  
  
  output$tabela_credit_store_hot <- renderRHandsontable({
    
    req(input$button_base_overall)
    
    rhandsontable(head(bases$credit_score, 20),
                  height = 500, 
                  selectCallback = TRUE, 
                  readOnly = T, 
                  language = "pt-BR",
                  filters = c("Sel", "Text", NA, "Auto"), 
                  sorting = TRUE,
                  search = TRUE) 

    
    
  })
  
 
  return(list())
  
}


