
## server.R

dados_cancer <- read_fst("data/dados_cancer_filtrado.fst",
                         as.data.table = TRUE) 

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
  dados_filtrados_km <- reactive({
    dados_cancer |> 
      filter(TOPOGRUP_GRUPO %in% input$grupo_cid_2)  # Note: grupo_cid_2
  })
  # Generate Kaplan-Meier plot
  output$km_plot <- renderPlot({
    req(input$km_variable,input$len_tempo,input$Tempo_int, dados_filtrados_km())
    df <- dados_filtrados_km()
    df$Tempo_int <- (df[[input$Tempo_int]])
    df <- df %>%
      mutate(Tempo_int = floor(Tempo_int / as.numeric(input$len_tempo)) + 1)  # Starts at 1
    
    df$VAR_KM <- (df[[input$km_variable]])

    if (is.numeric(df$VAR_KM)) {
            cutpoint <- surv_cutpoint(
        data = df,
        time = "Tempo_int",
        event = "DESFECHO",
        variables = "VAR_KM"
      )
      df$VAR_KM <- surv_categorize(cutpoint)$VAR_KM
    }
    
    
    df$VAR_KM <- as.factor(df$VAR_KM)
    fit <- survfit(Surv(Tempo_int, DESFECHO) ~VAR_KM, data = df)
    ggsurvplot(
      fit,
      data = df,
      pval = FALSE,           # Add p-value
      conf.int = input$show_ci,
      risk.table = TRUE,     # Show risk table
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
      break.time.by = 4,
      legend = "right",
      legend.title = "",
      legend.labs = levels(df[[input$km_variable]])
      )$plot
    
  })
}
