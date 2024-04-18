training_server <-function(input, output, session, credit_score){

  
  observeEvent(input$processa_base, {
    req(bases$credit_score)

    glimpse(bases$credit_score)
    dados <- bases$credit_score 
 
    
    
    data <- bases$credit_score
    cols <- c("Gender", "Education_Level", "Marital_Status", "Income_Category", "Card_Category")
    data <- data %>% 
      select(all_of(cols))
    
    vars_dummy <- data[, c("Gender", "Marital_Status", "Education_Level", "Income_Category", "Card_Category" )]
    dummy      <- dummyVars("~.", data = vars_dummy)
    dummy_     <- predict(dummy, vars_dummy) 
    
    data <- dplyr::select(cbind(data, dummy_), -c("Gender", "Marital_Status", , "Education_Level", "Income_Category", "Card_Category"))
    
    data <- data %>%
      mutate(across(everything(), as.numeric))
    
    # data <- data %>%
    #   mutate(Class = data$Attrition_Flag %>% as.factor %>% make.names)
  
    bases$credit_score2 <- bases$credit_score %>% select(-c("Gender", "Marital_Status", , "Education_Level", "Income_Category", "Card_Category")) %>% 
    bind_cols(data) 
    
    
    sub_target <- function(x){
      if(x == "Existing Customer"){
        return(0)
      } else {
        return(1)
      }
    }
 
    bases$credit_score2$Attrition_Flag <- sapply(bases$credit_score2$Attrition_Flag, sub_target)
    
    
    
  

  
    output$tabela_credit_store_traning <- renderDT(server = FALSE, {
  
    head(bases$credit_score2, 100) %>%
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
    
    shinyalert(
      title = "Processing completed!",
      text = "Database successfully processed.",
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
    
    
  
  })
  
observeEvent(input$training_model, {
  req(bases$credit_score2)

  # Substituir $ por nada
  names(bases$credit_score2) <- gsub("\\$", "", names(bases$credit_score2))
  
  # Substituir - por nada
  names(bases$credit_score2) <- gsub("-", "", names(bases$credit_score2))
  
  # Substituir + por nada
  names(bases$credit_score2) <- gsub("\\+", "", names(bases$credit_score2))
  
  # Substituir _ por nada
  names(bases$credit_score2) <- gsub("_", "", names(bases$credit_score2))
  
  # Substituir espaços por _
  names(bases$credit_score2) <- gsub("\\s", "_", names(bases$credit_score2))
  
  bases$credit_score2  <- bases$credit_score2 %>% mutate(AttritionFlag = as.factor(AttritionFlag))
  
  # Substituir números no início por nada
  # Applying - SMOTE
  set.seed(333)
 
 # browser()
  
  # Realize o oversampling usando SMOTE
  smote_train <- ovun.sample(AttritionFlag ~ ., data = bases$credit_score2, method = "over")
  print("aqui")
  smote_train <- smote_train$data %>% as_tibble() %>% mutate(AttritionFlag = as.numeric(AttritionFlag )) %>% 
    mutate(AttritionFlag = ifelse(AttritionFlag == 2, 1, ifelse(AttritionFlag == 1, 0, AttritionFlag)))

  
  set.seed(123)
  train_index <- sample(1:nrow(smote_train), nrow(smote_train)*0.8)
  train_data <- smote_train[train_index, ]
  test_data <- smote_train[-train_index, ]
  

  # 
  train_label <- train_data$AttritionFlag
  train_matrix <- xgb.DMatrix(data = as.matrix(train_data[,-which(names(train_data) == "AttritionFlag")]), label = train_label)
  
  test_label <- test_data$AttritionFlag
  test_matrix <- xgb.DMatrix(data = as.matrix(test_data[,-which(names(test_data) == "AttritionFlag")]), label = test_label)
  print("testlabel")
  princomp(test_label)
  params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    max_depth = 6,
    eta = 0.3,
    gamma = 0,
    colsample_bytree = 1,
    min_child_weight = 1
  )
  
  
  xgb_model <- xgb.train(
    params = params,
    data = train_matrix,
    nrounds = 500,
    watchlist = list(val = test_matrix),
    early_stopping_rounds = 10,
    maximize = TRUE
  )
  
  
  
  library(pROC)
  
  pred <- predict(xgb_model, test_matrix, type = "prob")
  roc_obj <- roc(test_label, pred)
  

  # As probabilidades previstas e os rótulos reais
  df_decile <- data.frame('y_true' = test_label, 'y_prob' = pred)
  
  
  # Observações com base nas probabilidades previstas
  df_decile <- df_decile %>% arrange(desc(y_prob))
  
  # Tamanho do decil
  n <- nrow(df_decile)
  decile_size <- n %/% 10
  
  # Vetor para armazenar os rótulos decil
  decile_labels <- rep(1:10, each = decile_size)
  
  # Se o tamanho do vetor decile_labels for menor que n, adicione rótulos extras
  if (length(decile_labels) < n) {
    decile_labels <- c(decile_labels, rep(10, n - length(decile_labels)))
  }
  
  # Atribua os rótulos decil ao data frame
  df_decile$decile <- decile_labels
  
  # Calcule a taxa de mau pagadores para cada grupo decil
  decile_stats <- df_decile %>% group_by(decile) %>% summarise('Mau Pagadores' = mean(y_true)) %>% ungroup()
  
  # Taxa de mau pagadores para cada decil
  print(decile_stats)
  
  # Gráfico de linhas com fundo branco
  
  # Gráfico de barras com representatividade
  
  output$valid_model <- renderPlot({
    req(decile_stats)
    
    
    decile_stats <- decile_stats %>% as_tibble()
    
  ggplot(data = decile_stats, aes(x = decile)) +
    geom_bar(aes(y = 1), stat = "identity", fill = "#00887D", alpha = 0.3) +
    geom_line(aes(y = `Mau Pagadores`), color = "#00887D") +
    geom_point(aes(y = `Mau Pagadores`), color = "#00887D") +
    geom_text(aes(y = `Mau Pagadores`, label = paste0(sprintf("%.2f", `Mau Pagadores` * 100), "%")), hjust = -0.2, vjust = 0.5) +
    scale_x_continuous(breaks = 1:10) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1), name = "Default rate") +
    theme_bw()
    

  
  
  })
  
  shinyalert(
    title = "Model trained successfully!",
    text = "The prediction model was successfully trained.",
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
  
})
  
}