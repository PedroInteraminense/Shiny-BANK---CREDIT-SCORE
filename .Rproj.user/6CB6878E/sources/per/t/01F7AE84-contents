analytics_server <-function(input, output, session, credit_score){
  
  
  #MATRIZ DE CORRELAÇÃO ----

 
  dados_cor <- reactive({
    # Selecting columns
    dados <- bases$credit_score   
    
    validate(
      need(!is.null(dados), "It is necessary to input the base")
    )
      #select(-c("Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1", "Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2"))
    
    # Replacing the elements of the Attrition_Flag target variable
    sub_target <- function(x){
      if(x == "Existing Customer"){
        return(0)
      } else {
        return(1)
      }
    }
    dados$Attrition_Flag <- sapply(dados$Attrition_Flag, sub_target)
    
    # Replacing the elements of the Attrition_Flag target variable
    sub_target1 <- function(x){
      if(x == "Existing.Customer"){
        return(0)
      } else {
        return(1)
      }
    }
    
    # Return the modified data
    return(dados)
  })
  

  
  output$plot_matrix_corr <- renderHighchart({
    
    
    # Use the reactive expression here
    dados_cor <- dados_cor()
    
   
    
    # Calculate correlation
    cor_spearman <- cor(dados_cor[, sapply(dados_cor, is.numeric)], method = 'spearman')
    
    # Generate the plot
    as.matrix(data.frame(cor_spearman)) %>%
      round(3) %>% 
      hchart() %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_title(text = "Spearman correlation coefficients", align = "center") %>%
      hc_legend(align = "center") %>%
      hc_colorAxis(stops = color_stops(colors = viridis::inferno(10))) %>%
      hc_plotOptions(
        series = list(
          borderWidth = 0,
          dataLabels = list(enabled = TRUE)))
  })
  
  
  # MARIAL STATUS ----
  # MARIAL STATUS ----
  dados_Marital_Status <- reactive({
    dados <- bases$credit_score
    
    validate(
      need(!is.null(dados), "It is necessary to input the base")
    )
    
    dados<-dados %>% 
      filter(Attrition_Flag == "Attrited Customer") %>%
      count(Marital_Status)
    
    # Validation
  
    
    # Return the data
    return(dados)
  })
  
 
  output$plot_marital_status <- renderHighchart({
   
    dados <- dados_Marital_Status()
    my_colors <- c("#818274", "#DBF227", "#506266", "#005C53", "#042940")
    # Generate the plot
    dados %>%
      hchart("treemap", hcaes(x = Marital_Status, value = n, color = n)) %>%
      hc_colorAxis(stops = color_stops(colors = my_colors)) %>%
      hc_title(text = "Number of Cancellations By Marital Status", align = "center")
  })
  
  # Income Category ----
  
  dados_Income_Category <- reactive({
    
    dados <- bases$credit_score
    
    validate(
      need(!is.null(dados), "It is necessary to input the base")
    )
    
    dados<- dados %>%
      filter(Attrition_Flag == "Attrited Customer") %>%
      count(Income_Category)
    
    return(dados)
  })
  
  output$plot_Income_Category <- renderHighchart({
    
    dados <- dados_Income_Category()
    my_colors <- c("#818274", "#DBF227", "#506266", "#005C53", "#042940")
    # Generate the plot
    dados %>%
      hchart("treemap", hcaes(x = Income_Category, value = n, color = n)) %>%
      hc_colorAxis(stops = color_stops(colors = my_colors)) %>%
      hc_title(text = "Number of Cancellations Per Annual Income", align = "center")
  })
  
  ### Educational level ----
  dados_Education_Level <- reactive({
    
    dados <- bases$credit_score
    
    validate(
      need(!is.null(dados), "It is necessary to input the base")
    )
    
    dados <- dados %>% 
      filter(Attrition_Flag == "Attrited Customer") %>%
      count(Education_Level)
    
    return(dados)
  })
  
  output$plot_Education_Level <- renderHighchart({
    
    dados <- dados_Education_Level()
    
    # Generate the plot
    my_colors <- c("#818274", "#DBF227", "#506266", "#005C53", "#042940")
    
    dados %>%
      hchart("treemap", hcaes(x = Education_Level, value = n, color = n)) %>%
      hc_colorAxis(stops = color_stops(colors = my_colors)) %>%
      hc_title(text = "Number of Cancellations By Educational Level", align = "center")
  })
  
  ### Card Category----
  dados_Card_Category <- reactive({
    
    
    dados <- bases$credit_score
    
    validate(
      need(!is.null(dados), "It is necessary to input the base")
    )
    
    dados <- dados %>% 
      filter(Attrition_Flag == "Attrited Customer") %>%
      count(Card_Category)
    
    return(dados)
  })
  
  output$plot_Card_Category <- renderHighchart({
    
    dados <- dados_Card_Category()
    
    # Generate the plot
    my_colors <- c("#818274", "#DBF227", "#506266", "#005C53", "#042940")
    
    dados %>%
      hchart("treemap", hcaes(x = Card_Category, value = n, color = n)) %>%
      hc_colorAxis(stops = color_stops(colors = my_colors)) %>%
      hc_title(text = "Number of Cancellations By Educational Level", align = "center")
  })
 
  tabs <- function(atributo){
    df <- bases$credit_score
    validate(
      need(!is.null(df), "It is necessary to input the base")
    )
   
    df <- filter(bases$credit_score, Attrition_Flag == "Attrited Customer")
    
  
    
    # Usando o argumento atributo para filtrar os dados
    frequence <- table(df[[atributo]]) * 100
    percent   <- round(prop.table(table(df[[atributo]])) * 100, 2)
    
    combine <- cbind(Frequency = frequence, "Relative Frequency" = percent)
    data.frame(combine)
    
  }
  
  
 
  
 
  # output$tabelaEducationLevel <- renderTable({
  #   tabs("Education_Level")
  # })
  # output$tabelaCardCategory <- renderTable({
  #   tabs("Card_Category")
  # })
  
  output$tabela <- renderUI({
    input_selecionado <- input$select
    
    if (is.null(input_selecionado)) {
      return()
    }
    
    switch(input_selecionado,
           "Marital_Status" = {
             
            renderTable({
               tabs("Marital_Status")
             })
             
           },
           "Income_Category" = {
             

            renderTable({
              tabs("Income_Category")
             })
           }, 
           
           "Education_Level" = {
             
             
             renderTable({
               tabs("Education_Level")
             })
           },
           
           "Card_Category" = {
             
             
             renderTable({
               tabs("Card_Category")
             })
           },
           
      
    )
  })
  
  output$grafico_Total_Trans_Ct <- renderHighchart({
    dados <- bases$credit_score  
    validate(
      need(!is.null(dados), "It is necessary to input the base")
    )
    
    dados <- dados %>%
      dplyr::select(Total_Trans_Ct, Attrition_Flag) %>%
      group_by(Total_Trans_Ct, Attrition_Flag) %>%
      count(Total_Trans_Ct) %>%
      arrange()
    
    
    # Generate the plot
    dados %>%
      hchart('areaspline', hcaes(x = Total_Trans_Ct, y = n, group = Attrition_Flag)) %>%
      hc_title(text = "Total transactions (last 12 months)", align = "center") %>%
      hc_yAxis(labels = list(format = "{value}"), title = list(text = "Frequency")) %>% 
      hc_xAxis(labels = list(format = "{value}"), title = list(text = "Total Transactions")) %>%
      hc_colors(c("#DBF227", "#005C53", "#818274")) %>%
      hc_add_theme(hc_theme_smpl()) %>% 
      hc_legend(align = "center") %>%
      hc_tooltip(formatter = JS("function(){
                              return (' <br> Frequency: ' + this.y + ' <br> Total Transactions: ' + this.x)}"))
  })
  
  
  output$grafico_Total_Revolving_Bal <- renderHighchart({
    dados <- bases$credit_score 
    
    validate(
      need(!is.null(dados), "It is necessary to input the base")
    )
    
    dados <- dados %>%
      dplyr::select(Total_Revolving_Bal, Attrition_Flag) %>%
      group_by(Total_Revolving_Bal, Attrition_Flag) %>%
      count(Total_Revolving_Bal) %>%
      arrange() 
      
      
      dados %>% 
      hchart('areaspline', hcaes(x = Total_Revolving_Bal, y = n, group = Attrition_Flag)) %>%
      hc_title(text = "Total revolving credit card balance", align = "center") %>%
      hc_yAxis(labels = list(format = "{value}"), title = list(text = "Frequency")) %>% 
      hc_xAxis(labels = list(format = "{value}"), title = list(text = "Revolving balance")) %>%
      hc_colors(c("#DBF227", "#005C53", "#818274")) %>%
      hc_add_theme(hc_theme_smpl()) %>% 
      hc_legend(align = "center") %>%
      hc_tooltip(formatter = JS("function(){
                            return (' <br> Frequency: ' + this.y + ' <br> Revolving balance: ' + this.x)}"))
    
  })
  
  
  output$grafico_Avg_Utilization_Ratio <- renderHighchart({
    
    dados <- bases$credit_score 
    validate(
      need(!is.null(dados), "It is necessary to input the base")
    )
    
    dados <- dados %>%
      dplyr::select(Avg_Utilization_Ratio, Attrition_Flag) %>%
      group_by(Avg_Utilization_Ratio, Attrition_Flag) %>%
      count(Avg_Utilization_Ratio) %>%
      arrange() 
      
      
      
      
      dados %>% 
      hchart('areaspline', hcaes(x = Avg_Utilization_Ratio, y = n, group = Attrition_Flag)) %>%
      hc_title(text = "Average card usage fee", align = "center") %>%
      hc_yAxis(labels = list(format = "{value}"), title = list(text = "Frequency")) %>% 
      hc_xAxis(labels = list(format = "{value}"), title = list(text = "Average card usage fee")) %>%
      hc_colors(c("#DBF227", "#005C53", "#818274")) %>%
      hc_add_theme(hc_theme_smpl()) %>% 
      hc_legend(align = "center") %>%
      hc_tooltip(formatter = JS("function(){
                            return (' <br> Frequency: ' + this.y + ' <br> Average card usage fee: ' + this.x)}"))
    
  })
  
  output$grafico_Total_Ct_Chng_Q4_Q1 <- renderHighchart({
    
    dados <- bases$credit_score 
    validate(
      need(!is.null(dados), "It is necessary to input the base")
    )
    
    dados <- dados %>%
      dplyr::select(Total_Ct_Chng_Q4_Q1, Attrition_Flag) %>%
      group_by(Total_Ct_Chng_Q4_Q1, Attrition_Flag) %>%
      count(Total_Ct_Chng_Q4_Q1) %>%
      arrange() 
      
      
      
      dados %>% 
      hchart('areaspline', hcaes(x = Total_Ct_Chng_Q4_Q1, y = n, group = Attrition_Flag)) %>%
      hc_title(text = "Change in transaction count (Q4 over Q1)", align = "center") %>%
      hc_yAxis(labels = list(format = "{value}"), title = list(text = "Frequency")) %>% 
      hc_xAxis(labels = list(format = "{value}"), title = list(text = "Change in transaction count (Q4 over Q1)")) %>%
      hc_colors(c("#DBF227", "#005C53", "#818274")) %>%
      hc_add_theme(hc_theme_smpl()) %>% 
      hc_legend(align = "center") %>%
      hc_tooltip(formatter = JS("function(){
                            return (' <br> Frequency: ' + this.y + ' <br> Change in transaction count (Q4 over Q1): ' + this.x)}"))
    
  })
  
  output$Total_Trans_Ct2 <- renderHighchart({
    
     bases$credit_score 
    
    validate(
      need(!is.null(bases$credit_score ), "It is necessary to input the base")
    )
    
 
    
    hcboxplot(outliers = TRUE, x = bases$credit_score$Total_Trans_Ct, var = bases$credit_score$Attrition_Flag, name = "Length") %>%
      hc_title(text = "", align = "center") %>%
      hc_yAxis(title = list(text = "Number of transactions in the last 12 months")) %>%
      hc_add_theme(hc_theme_smpl()) %>% 
      hc_legend(align = "center")
    
  })
  
  # OUTRO BOX
  dados_reactive <- reactive({
   
    atr_categorical <- function(df, atr, grupo){
      df <- df %>%
        count(!!sym(atr)) %>%
        mutate(Percent = n / sum(n) * 100,
               Grupo = grupo) %>%
        select(Class = !!sym(atr), Percent, Grupo)
      return(df)
    }
    
    cols <- colnames(bases$credit_score[, sapply(bases$credit_score, is.factor)])
    
    # Filtrar observações apenas de um grupo de clientes específico
    df_attritedc <- bases$credit_score %>%
      filter(Attrition_Flag == "Attrited Customer")
    df_existingc <- bases$credit_score %>%
      filter(Attrition_Flag == "Existing Customer")
    
    # Processamento para clientes que saíram (Attrited Customer)
    df1 <- map_dfr(cols, ~ atr_categorical(df_attritedc, .x, "Attrited Customer"))
    
    # Processamento para clientes existentes (Existing Customer)
    df2 <- map_dfr(cols, ~ atr_categorical(df_existingc, .x, "Existing Customer"))
    
    # Filtrar classes não relevantes
    df1 <- df1 %>%
      filter(Class != "Attrited Customer" & Class != "Existing Customer")
    df2 <- df2 %>%
      filter(Class != "Attrited Customer" & Class != "Existing Customer")
    
    # Combinação dos resultados
    df <- bind_rows(df1, df2)
    
    return(df)
  })
  
  
  
  output$process_dataa  <- renderDT({
    
    gp <- bases$credit_score %>%
      filter(Attrition_Flag == "Attrited Customer") %>%
      select(Attrition_Flag, Gender, Education_Level, Marital_Status, Income_Category, Card_Category) %>%
      group_by(Gender, Education_Level, Marital_Status, Income_Category, Card_Category) %>%
      count(Attrition_Flag) %>%
      select(Gender, Education_Level, Marital_Status, Income_Category, Card_Category, n) %>%
      arrange(desc(n))
    
    gp$Relative.Frequency <- round(prop.table(gp$n) * 100, 2)
    gp$n = NULL
    
  
   gp %>% 
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
                 list(orderable = FALSE, targets = "_all", className = "dt-center"))))
  })
  
  
  data_reactive <- reactive({
    # Your data
    
    dados <- bases$credit_score
    validate(
      need(!is.null(dados), "It is necessary to input the base")
    )
    
    
    dados %>% 
      # select(-c("CLIENTNUM", 
      #           "Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1", 
      #           "Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2")) %>% 
      mutate(Avg_Utilization_Ratio = as.numeric(Avg_Utilization_Ratio),
             Credit_Limit = as.numeric(Credit_Limit),
             Avg_Open_To_Buy = as.numeric(Avg_Open_To_Buy),
             Total_Ct_Chng_Q4_Q1 = as.numeric(Total_Ct_Chng_Q4_Q1),
             Total_Amt_Chng_Q4_Q1 = as.numeric(Total_Amt_Chng_Q4_Q1),
             Avg_Open_To_Buy = as.numeric(Avg_Open_To_Buy))
  
    
    # Creating a copy of the data set
    data <- dados
    
    # Applying the sturges rule to obtain the number of intervals
    n <- nrow(data)
    k <- round((1 + (10 / 3) * log10(n))) # 14 Grupos
    
    # Creating new columns with the grouped values
    cols     <- colnames(data[, sapply(data, is.numeric)])
    new_cols <- unlist(lapply(cols, function(x) paste(x, "_f", sep = "")))
    
    # Creating groups with the data_binning function
    data_binning <- function(x, k){
      
      gp <- cut(x, breaks = k, include.lowest = T, ordered_result = T)
      return(gp)
    }
    #data[, new_cols] <- Map(function(x, k) as.factor(data_binning(data[, x], k)), cols, k)
    data[, new_cols] <- Map(function(x, k) as.factor(data_binning(data[[x]], k)), cols, k)
    
    return(data)
    
   
  })

  #### Total_Trans_Ct

output$h1 <- renderHighchart({
 
  data <- data_reactive()


  hchart(hist(data$Total_Trans_Ct, breaks = 30, plot = F),
         type = 'histogram', name = 'Total_Trans_Ct') %>%
    hc_colors(c("#042940", "#DBF227", "#818274", "#005C53", "#1C1C1C", "#506266")) %>%
    hc_yAxis(title = list(text = ""))
})
  # 
  output$h2 <- renderHighchart({
    data <- data_reactive()
    h2 <- data %>%
      select(Total_Trans_Ct_f) %>%
      group_by(Total_Trans_Ct_f) %>%
      count()

    highchart() %>%
      hc_chart(type ="column", options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>%
      hc_xAxis(categories = h2$Total_Trans_Ct_f) %>%
      hc_add_series(data = h2$n, name = "Total_Trans_Ct_f") %>%
      hc_colors(c("#042940", "#DBF227", "#818274", "#005C53", "#1C1C1C", "#506266"))
  })


  #### Avg_Utilization_Ratio
  
  output$h3 <- renderHighchart({
    
    data <- data_reactive()
    
    
    hchart(hist(data$Avg_Utilization_Ratio, breaks = 30, plot = F), 
           type = 'histogram', name = 'Avg_Utilization_Ratio') %>%
      hc_colors(c("#042940", "#DBF227", "#818274", "#005C53", "#1C1C1C", "#506266")) %>%
      hc_yAxis(title = list(text = ""))
  })
  
  output$h4 <- renderHighchart({
    data <- data_reactive()
    
      h4 <- data %>%
        select(Avg_Utilization_Ratio_f) %>%
        group_by(Avg_Utilization_Ratio_f) %>%
        count()
    
    highchart() %>% 
      hc_chart(type ="column", options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>%
      hc_xAxis(categories = h4$Avg_Utilization_Ratio_f) %>% 
      hc_add_series(data = h4$n, name = "Avg_Utilization_Ratio_f") %>%
      hc_colors(c("#042940", "#DBF227", "#818274", "#005C53", "#1C1C1C", "#506266"))
  })
  
  #### Total_Ct_Chng_Q4_Q1
  output$h5 <- renderHighchart({
    
    data <- data_reactive()
    
    
    hchart(hist(data$Total_Ct_Chng_Q4_Q1, breaks = 30, plot = F), 
           type = 'histogram', name = 'Total_Ct_Chng_Q4_Q1') %>%
      hc_colors(c("#042940", "#DBF227", "#818274", "#005C53", "#1C1C1C", "#506266"))%>%
      hc_yAxis(title = list(text = ""))
  })
  
  
  output$h6 <- renderHighchart({
    data <- data_reactive()
    
    h6 <- data %>%
      select(Total_Ct_Chng_Q4_Q1_f) %>%
      group_by(Total_Ct_Chng_Q4_Q1_f) %>%
      count()
    
    highchart() %>% 
      hc_chart(type ="column", options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>%
      hc_xAxis(categories = h6$Total_Ct_Chng_Q4_Q1_f) %>% 
      hc_add_series(data = h6$n, name = "Total_Ct_Chng_Q4_Q1_f") %>%
      hc_colors(c("#042940", "#DBF227", "#818274", "#005C53", "#1C1C1C", "#506266"))
  })
  
  #### Total_Trans_Amt
  
  output$h7 <- renderHighchart({
    
    data <- data_reactive()
    
    
    hchart(hist(data$Total_Trans_Amt, breaks = 30, plot = F), 
           type = 'histogram', name = 'Total_Trans_Amt') %>%
      hc_colors(c("#042940", "#DBF227", "#818274", "#005C53", "#1C1C1C", "#506266"))%>%
      hc_yAxis(title = list(text = ""))
  })
  
  
  output$h8 <- renderHighchart({
    data <- data_reactive()
    
    h8 <- data %>%
      select(Total_Trans_Amt) %>%
      group_by(Total_Trans_Amt) %>%
      count()
    
    highchart() %>% 
      hc_chart(type ="column", options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>%
      hc_xAxis(categories = h8$Total_Trans_Amt) %>% 
      hc_add_series(data = h8$n, name = "Total_Trans_Amt") %>%
      hc_colors(c("#042940", "#DBF227", "#818274", "#005C53", "#1C1C1C", "#506266"))
  })
  
  
  #### Total_Amt_Chng_Q4_Q1
  
  output$h9 <- renderHighchart({
    
    data <- data_reactive()
    
    
    hchart(hist(data$Total_Amt_Chng_Q4_Q1, breaks = 30, plot = F), 
           type = 'histogram', name = 'Total_Amt_Chng_Q4_Q1') %>%
      hc_colors(c("#042940", "#DBF227", "#818274", "#005C53", "#1C1C1C", "#506266"))%>%
      hc_yAxis(title = list(text = ""))
  })
  
  
  output$h10 <- renderHighchart({
    data <- data_reactive()
    
    h10 <- data %>%
      select(Total_Amt_Chng_Q4_Q1) %>%
      group_by(Total_Amt_Chng_Q4_Q1) %>%
      count()
    
    highchart() %>% 
      hc_chart(type ="column", options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>%
      hc_xAxis(categories = h10$Total_Amt_Chng_Q4_Q1) %>% 
      hc_add_series(data = h10$n, name = "Total_Amt_Chng_Q4_Q1") %>%
      hc_colors(c("#042940", "#DBF227", "#818274", "#005C53", "#1C1C1C", "#506266"))
  })
  
  
  #### Avg_Open_To_Buy
  
  output$h11 <- renderHighchart({
    
    data <- data_reactive()
    
    
    hchart(hist(data$Avg_Open_To_Buy, breaks = 30, plot = F), 
           type = 'histogram', name = 'Avg_Open_To_Buy') %>%
      hc_colors(c("#042940", "#DBF227", "#818274", "#005C53", "#1C1C1C", "#506266"))%>%
      hc_yAxis(title = list(text = ""))
  })
  
  
  output$h12 <- renderHighchart({
    data <- data_reactive()
    
    h12 <- data %>%
      select(Avg_Open_To_Buy) %>%
      group_by(Avg_Open_To_Buy) %>%
      count()
    
    highchart() %>% 
      hc_chart(type ="column", options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>%
      hc_xAxis(categories = h12$Avg_Open_To_Buy) %>% 
      hc_add_series(data = h12$n, name = "Avg_Open_To_Buy") %>%
      hc_colors(c("#042940", "#DBF227", "#818274", "#005C53", "#1C1C1C", "#506266"))
  })
  
  #### Total_Revolving_Bal
  
  output$h13 <- renderHighchart({
    
    data <- data_reactive()
    
    
    hchart(hist(data$Total_Revolving_Bal, breaks = 30, plot = F), 
           type = 'histogram', name = 'Total_Revolving_Bal') %>%
      hc_colors(c("#042940", "#DBF227", "#818274", "#005C53", "#1C1C1C", "#506266"))%>%
      hc_yAxis(title = list(text = ""))
  })
  
  
  output$h14 <- renderHighchart({
    data <- data_reactive()
    
    h14 <- data %>%
      select(Total_Revolving_Bal) %>%
      group_by(Total_Revolving_Bal) %>%
      count()
    
    highchart() %>% 
      hc_chart(type ="column", options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>%
      hc_xAxis(categories = h14$Total_Revolving_Bal) %>% 
      hc_add_series(data = h14$n, name = "Total_Revolving_Bal") %>%
      hc_colors(c("#042940", "#DBF227", "#818274", "#005C53", "#1C1C1C", "#506266"))
  })
  
  
  #### Credit_Limit
  
  output$h15 <- renderHighchart({
    
    data <- data_reactive()
    
    
    hchart(hist(data$Credit_Limit, breaks = 30, plot = F), 
           type = 'histogram', name = 'Credit_Limit') %>%
      hc_colors(c("#042940", "#DBF227", "#818274", "#005C53", "#1C1C1C", "#506266"))%>%
      hc_yAxis(title = list(text = ""))
  })
  
  
  output$h16 <- renderHighchart({
    data <- data_reactive()
    
    h16 <- data %>%
      select(Credit_Limit) %>%
      group_by(Credit_Limit) %>%
      count()
    
    highchart() %>% 
      hc_chart(type ="column", options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>%
      hc_xAxis(categories = h16$Credit_Limit) %>% 
      hc_add_series(data = h16$n, name = "Credit_Limit") %>%
      hc_colors(c("#042940", "#DBF227", "#818274", "#005C53", "#1C1C1C", "#506266"))
  })
  
  
  #### Contacts_Count_12_mon
  
  output$h17 <- renderHighchart({
    
    data <- data_reactive()
    
    
    hchart(hist(data$Contacts_Count_12_mon, breaks = 30, plot = F), 
           type = 'histogram', name = 'Contacts_Count_12_mon') %>%
      hc_colors(c("#042940", "#DBF227", "#818274", "#005C53", "#1C1C1C", "#506266"))%>%
      hc_yAxis(title = list(text = ""))
  })
  
  
  output$h18 <- renderHighchart({
    data <- data_reactive()
    
    h18 <- data %>%
      select(Contacts_Count_12_mon) %>%
      group_by(Contacts_Count_12_mon) %>%
      count()
    
    highchart() %>% 
      hc_chart(type ="column", options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>%
      hc_xAxis(categories = h18$Contacts_Count_12_mon) %>% 
      hc_add_series(data = h18$n, name = "Contacts_Count_12_mon") %>%
      hc_colors(c("#042940", "#DBF227", "#818274", "#005C53", "#1C1C1C", "#506266"))
  })
  
  
  #### Months_Inactive_12_mon
  
  output$h19 <- renderHighchart({
    
    data <- data_reactive()
    
    
    hchart(hist(data$Months_Inactive_12_mon, breaks = 30, plot = F), 
           type = 'histogram', name = 'Months_Inactive_12_mon') %>%
      hc_colors(c("#042940", "#DBF227", "#818274", "#005C53", "#1C1C1C", "#506266"))%>%
      hc_yAxis(title = list(text = ""))
  })
  
  
  output$h20 <- renderHighchart({
    data <- data_reactive()
    
    h20 <- data %>%
      select(Months_Inactive_12_mon) %>%
      group_by(Months_Inactive_12_mon) %>%
      count()
    
    highchart() %>% 
      hc_chart(type ="column", options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>%
      hc_xAxis(categories = h20$Months_Inactive_12_mon) %>% 
      hc_add_series(data = h20$n, name = "Months_Inactive_12_mon") %>%
      hc_colors(c("#042940", "#DBF227", "#818274", "#005C53", "#1C1C1C", "#506266"))
  })
  
  
  #### Total_Relationship_Count
  
  output$h21 <- renderHighchart({
    
    data <- data_reactive()
    
    
    hchart(hist(data$Total_Relationship_Count, breaks = 30, plot = F), 
           type = 'histogram', name = 'Total_Relationship_Count') %>%
      hc_colors(c("#042940", "#DBF227", "#818274", "#005C53", "#1C1C1C", "#506266"))%>%
      hc_yAxis(title = list(text = ""))
  })
  
  
  output$h22 <- renderHighchart({
    data <- data_reactive()
    
    h22 <- data %>%
      select(Total_Relationship_Count) %>%
      group_by(Total_Relationship_Count) %>%
      count()
    
    highchart() %>% 
      hc_chart(type ="column", options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>%
      hc_xAxis(categories = h22$Total_Relationship_Count) %>% 
      hc_add_series(data = h22$n, name = "Total_Relationship_Count") %>%
      hc_colors(c("#042940", "#DBF227", "#818274", "#005C53", "#1C1C1C", "#506266"))
  })
  
  
  
  #### Months_on_book
  
  output$h23 <- renderHighchart({
    
    data <- data_reactive()
    
    
    hchart(hist(data$Months_on_book, breaks = 30, plot = F), 
           type = 'histogram', name = 'Months_on_book') %>%
      hc_colors(c("#042940", "#DBF227", "#818274", "#005C53", "#1C1C1C", "#506266"))%>%
      hc_yAxis(title = list(text = ""))
  })
  
  
  output$h24 <- renderHighchart({
    data <- data_reactive()
    
    h24 <- data %>%
      select(Months_on_book) %>%
      group_by(Months_on_book) %>%
      count()
    
    highchart() %>% 
      hc_chart(type ="column", options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>%
      hc_xAxis(categories = h24$Months_on_book) %>% 
      hc_add_series(data = h24$n, name = "Months_on_book") %>%
      hc_colors(c("#042940", "#DBF227", "#818274", "#005C53", "#1C1C1C", "#506266"))
  })
  
  #### Dependent_count
  
  output$h25 <- renderHighchart({
    
    data <- data_reactive()
    
    
    hchart(hist(data$Dependent_count, breaks = 30, plot = F), 
           type = 'histogram', name = 'Dependent_count') %>%
      hc_colors(c("#042940", "#DBF227", "#818274", "#005C53", "#1C1C1C", "#506266"))%>%
      hc_yAxis(title = list(text = ""))
  })
  
  
  output$h26 <- renderHighchart({
    data <- data_reactive()
    
    h26 <- data %>%
      select(Dependent_count) %>%
      group_by(Dependent_count) %>%
      count()
    
    highchart() %>% 
      hc_chart(type ="column", options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>%
      hc_xAxis(categories = h26$Dependent_count) %>% 
      hc_add_series(data = h26$n, name = "Dependent_count") %>%
      hc_colors(c("#042940", "#DBF227", "#818274", "#005C53", "#1C1C1C", "#506266"))
  })
  
  #### Customer_Age
  
  output$h27 <- renderHighchart({
    
    data <- data_reactive()
    
    
    hchart(hist(data$Customer_Age, breaks = 30, plot = F), 
           type = 'histogram', name = 'Customer_Age') %>%
      hc_colors(c("#042940", "#DBF227", "#818274", "#005C53", "#1C1C1C", "#506266"))%>%
      hc_yAxis(title = list(text = ""))
  })
  
  
  output$h28 <- renderHighchart({
    data <- data_reactive()
    
    h28 <- data %>%
      select(Customer_Age) %>%
      group_by(Customer_Age) %>%
      count()
    
    highchart() %>% 
      hc_chart(type ="column", options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>%
      hc_xAxis(categories = h28$Customer_Age) %>% 
      hc_add_series(data = h28$n, name = "Customer_Age") %>%
      hc_colors(c("#042940", "#DBF227", "#818274", "#005C53", "#1C1C1C", "#506266"))
  })
  
  
  
}