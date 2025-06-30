
## server.R

dados_cancer <- read_fst("data/dados_cancer_filtrado.fst",
                         as.data.table = TRUE) 
dados_cancer$DESFECHO <- as.numeric(dados_cancer$DESFECHO)
dados_cancer <- dados_cancer[1:1000,]

## Aqui fica a parte onde trabalhamos com os dados e criamos os outputs
## que serão utilizados na ui

server <- function(input, output) {
  
  #Objeto reativo, condicionada a inputs, quando chamar usar dados_filtrados()
  dados_filtrados <- reactive({
    dados_cancer |>
      filter(TOPOGRUP_GRUPO %in% input$grupo_cid_1)
  })
  
  output$grafico_barras <- renderHighchart({
    
    #Mensagem para quando nenhum grupo de CID for selecionado 
    if(nrow(dados_filtrados()) == 0) {
      return(
        highchart() %>%
          hc_title(text = "Nenhum dado disponível") %>%
          hc_subtitle(text = "Selecione outros filtros") %>%
          hc_add_theme(hc_theme_null())  # Tema limpo sem eixos
      )
    }
    
    contagem <- dados_filtrados() %>% 
      count(.data[[input$variavel_1]])
    
    hchart(contagem, "column", 
           hcaes(x = !!sym(input$variavel_1), y = n),
           name = "Número de observações",
           color = "#4682B4") %>%
      hc_title(text = paste("Distribuição por", input$variavel_1)) %>%
      hc_xAxis(title = list(text = input$variavel_1)) %>%
      hc_yAxis(title = list(text = "Número de observações"))
  })
  
  output$vazio <- renderHighchart({
    
      highchart() %>%
        hc_title(text = "Vazio") %>%
        hc_subtitle(text = "Vazio") %>%
        hc_add_theme(hc_theme_null())  # Tema limpo sem eixos

  })
  
  #######################
  #######################
  #######################
  library(survival)
  library(survminer)

  # Update selectInput choices
  observe({
    updateSelectInput(inputId = "km_variable", choices = c("Sexo" = "SEXO"))
  })
  dados_filtrados_km <- reactive({
    dados_cancer |> 
      filter(TOPOGRUP_GRUPO %in% input$grupo_cid_2)  # Note: grupo_cid_2
  })
  # Generate Kaplan-Meier plot
  output$km_plot <- renderPlot({
    req(input$km_variable, dados_filtrados_km())
    df <- dados_filtrados_km()
    
    # Handle empty data
    if(nrow(df) == 0) {
      return(plot(0, 0, type = "n", main = "Nenhum dado disponível", 
                  xlab = "", ylab = ""))
    }
    
    # 1. Convert variable to factor for proper grouping
    df$VAR_KM <- as.factor(df[[input$km_variable]])
    
    # 2. Create survival formula
    #formula <- as.formula(paste("Surv(TEMPO_OBS_DIAG, DESFECHO) ~", 
    #                            input$km_variable))
    
    # 3. Fit survival model
    fit <- survfit(Surv(TEMPO_OBS_DIAG, DESFECHO) ~VAR_KM, data = df)
    
    # 4. Create plot
    plot_obj <- ggsurvplot(
      fit,
      data = df,
      pval = FALSE,           # Add p-value
      conf.int = FALSE,
      risk.table = FALSE,     # Show risk table
      risk.table.height = 0.25,
      ggtheme = theme_bw(),
      palette = "jco",
      title = paste("Sobrevivência por", 
                    switch(input$km_variable,
                           "SEXO" = "Sexo",
                           "FAIXAETAR" = "Faixa Etária",
                           "GRUPO_EC" = "Estádio Clínico")),
      xlab = "Tempo (dias)",
      ylab = "Probabilidade de Sobrevivência",
      break.time.by = 365,
      legend = "right",
      legend.title = "",
      legend.labs = levels(df[[input$km_variable]])
    )
    
    # 5. Return the plot
    print(plot_obj)
  })
}
