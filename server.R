## server.R

dados_cancer <- read_fst("data/dados_cancer_filtrado.fst",
                         as.data.table = TRUE) |>
                filter(TEMPO_OBS_DIAG_MESES > 0)

## Aqui fica a parte onde trabalhamos com os dados e criamos os outputs
## que serão utilizados na ui

server <- function(input, output) {
  
  #### aba Análises Gráficas
  
  #Objeto reativo, condicionada a inputs, quando chamar usar dados_filtrados()
  dados_filtrados <- reactive({
    dados_cancer |>
      filter(TOPOGRUP_GRUPO %in% input$grupo_cid_1)
  })
  
  output$grafico_barras <- renderPlotly({
    
    contagem <- dados_filtrados() |>
      count(.data[[input$variavel_1]])
    
    nome_var <- case_when(input$variavel_1 == "SEXO" ~ "Sexo",
                          input$variavel_1 == "FAIXAETAR" ~ "Faixa etária",
                          input$variavel_1 == "GRUPO_EC" ~ "Estádio clínico",
                          input$variavel_1 == "ULTINFO" ~ "Desfecho Tratamento",
                          input$variavel_1 == "TRATAMENTO" ~ "Tratamento")
    
    plot_ly(
      data = contagem,
      x = ~get(input$variavel_1),
      y = ~n,
      type = "bar",
      marker = list(color = "#4682B4"),
      name = "Número de observações"
    ) |>
      layout(
        title = paste("Frequência observada para a variável", nome_var),
        xaxis = list(title = nome_var),
        yaxis = list(
          title = "Número de observações",
          range = c(0, max(20000, max(contagem$n, na.rm = TRUE))),
          tickformat = ",d"
        ))
  })
  
  #### aba Curvas de Kaplan-Meier
  
  dados_filtrados_km <- reactive({
    dados_cancer |>
      filter(TOPOGRUP_GRUPO %in% input$grupo_cid_2) |>
      select(tempo = paste0("TEMPO_OBS_",input$Tempo_int,"_",input$len_tempo),
             variavel = input$km_variable,
             DESFECHO
      ) |>
      mutate(variavel = as.factor(variavel)) |>
      as.data.frame()
  })
  
  output$km_plot <- renderHighchart({
    
    janela_tempo <- case_when(
      input$len_tempo == "MESES" ~ "Tempo (meses)",
      input$len_tempo == "TRI" ~ "Tempo (trimestres)",
      input$len_tempo == "ANO" ~ "Tempo (anos)"
    )
    
    dados <- dados_filtrados_km()
    
    # if (input$km_variable %in% c("Idade",)) {
    #   
    #   cutpoint <- surv_cutpoint(
    #     data = dados,
    #     time = "Tempo",
    #     event = "DESFECHO",
    #     variables = "variavel"
    #   )
    #   
    #   df$variavel <- surv_categorize(cutpoint)$variavel
    #   
    #   df$variavel <- case_when(
    #     variavel == "high" ~ paste0(">", (cutpoint$cutpoint)$cutpoint),
    #     variavel == "low" ~ paste0("<", (cutpoint$cutpoint)$cutpoint)
    #   )
    #   
    # } else {
    #   dados$variavel <- as.factor(dados$variavel)
    #   levels_variavel <- levels(dados$variavel)
    # }
    
    fit <- survfit(Surv(tempo, DESFECHO) ~ variavel, data = dados)
    
    hchart(fit, type = "line", ranges = input$show_ci) |>
      hc_title(text = "Gráfico de Sobrevivência") |>
      hc_xAxis(title = list(text = janela_tempo)) |>
      hc_yAxis(title = list(text = "Probabilidade de Sobrevivência"),
               labels = list(formatter = JS("function() { return Highcharts.numberFormat(this.value, 3); }"))) |>
      hc_tooltip(
        formatter = JS(paste0("function() {
                              
          if (typeof this.point.low !== 'undefined' && typeof this.point.high !== 'undefined') {

            return '<b> Intervalo </b><br/>' +
                   '", janela_tempo, ": <b>' + this.x + '</b><br/>' +
                   'IC%: <b>' + Highcharts.numberFormat(this.point.low, 3) + 
                   ' - ' + Highcharts.numberFormat(this.point.high, 3) + '</b>';
          
          } else {
          
            return '<b>' + this.series.name + '</b><br/>' +
                   '", janela_tempo, ": <b>' + this.x + '</b><br/>' +
                   'Sobrevivência: <b>' + Highcharts.numberFormat(this.y, 3) + '</b>';
          
          }
        }")),
        shared = FALSE,
        crosshairs = TRUE
      ) |>
      hc_legend(align = "center", verticalAlign = "bottom", layout = "horizontal") |>
      hc_colors(colors = viridis::viridis(length(levels(dados$variavel)))) |>
      hc_plotOptions(series = list(marker = list(radius = 0)))
  })
  
}