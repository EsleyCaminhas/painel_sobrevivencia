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
  
  janela_tempo <- reactive({
    case_when(
    input$len_tempo == "MESES" ~ "Tempo (meses)",
    input$len_tempo == "TRI" ~ "Tempo (trimestres)",
    input$len_tempo == "ANO" ~ "Tempo (anos)"
    )
  })
  
  #### aba Curvas de Kaplan-Meier
  
  dados_filtrados_km <- reactive({
    dados_cancer |>
      filter(TOPOGRUP_GRUPO %in% input$grupo_cid_2) |>
      select(tempo = paste0("TEMPO_OBS_",input$Tempo_int,"_",input$len_tempo),
             Grupo = input$km_variable,
             DESFECHO
      ) |>
      mutate(Grupo = as.factor(Grupo)) |>
      as.data.frame()
  })
  
  output$km_plot <- renderHighchart({
    
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
    
    fit <- survfit(Surv(tempo, DESFECHO) ~ Grupo, data = dados)
    
    # print(summary(fit))
    
    hchart(fit, type = "line", ranges = input$show_ci) |>
      hc_title(text = "Gráfico de Sobrevivência") |>
      hc_xAxis(title = list(text = janela_tempo())) |>
      hc_yAxis(title = list(text = "Probabilidade de Sobrevivência"),
               labels = list(formatter = JS("function() { return Highcharts.numberFormat(this.value, 3); }"))) |>
      hc_tooltip(
        formatter = JS(paste0("function() {
                              
          if (typeof this.point.low !== 'undefined' && typeof this.point.high !== 'undefined') {

            return '<b> Intervalo </b><br/>' +
                   '", janela_tempo(), ": <b>' + this.x + '</b><br/>' +
                   'IC%: <b>' + Highcharts.numberFormat(this.point.low, 3) + 
                   ' - ' + Highcharts.numberFormat(this.point.high, 3) + '</b>';
          
          } else {
          
            return '<b>' + this.series.name + '</b><br/>' +
                   '", janela_tempo(), ": <b>' + this.x + '</b><br/>' +
                   'Sobrevivência: <b>' + Highcharts.numberFormat(this.y, 3) + '</b>';
          
          }
        }")),
        shared = FALSE,
        crosshairs = TRUE
      ) |>
      hc_legend(align = "center", verticalAlign = "bottom", layout = "horizontal") |>
      hc_colors(colors = viridis::viridis(length(levels(dados$Grupo)))) |>
      hc_plotOptions(series = list(marker = list(radius = 0)))
  })
  
  ##############################################################################
  
  # output$km_summary <- renderDT({
  #   dados <- dados_filtrados_km()
  #   fit <- survfit(Surv(tempo, DESFECHO) ~ Grupo, data = dados)
  #   
  #   sum_fit <- summary(fit)
  #   
  #   table <- data.frame(
  #     tempo = sum_fit$time,
  #     Group = sum_fit$strata,
  #     Sobrevivência = sum_fit$surv,
  #     Erro.Padrão = sum_fit$std.err, 
  #     IC.Inferior = sum_fit$lower, 
  #     IC.Superior = sum_fit$upper,  
  #     Eventos = sum_fit$n.event,  
  #     Em.Risco = sum_fit$n.risk     
  #   )
  #   
  #   
  #   
  #   datatable(table, 
  #             options = list(pageLength = 10,
  #                            language = list(    
  #                              url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
  #                            ),
  #                            dom = 't', 
  #                            paging = FALSE, 
  #                            searching = FALSE,
  #                            info = FALSE,
  #                            ordering = FALSE),
  #             rownames = FALSE
  #             ) |>
  #     formatRound(columns = c('Sobrevivência', 'Erro.Padrão', 'IC.Inferior', 'IC.Superior'), 
  #                 digits = 4)
  # })
  
  
  output$tabelas_por_grupo <- renderUI({
    
    dados <- dados_filtrados_km() |> arrange(Grupo)

    fit <- survfit(Surv(tempo, DESFECHO) ~ Grupo, data = dados)

    grupos <- unique(dados$Grupo)
    
    # Cria uma lista de elementos UI para cada grupo
    tabelas <- map(grupos, ~ {
      sum_fit <- summary(fit[.x])
      
      tabela <- data.frame(
        tempo = sum_fit$time,
        Sobrevivência = sum_fit$surv,
        Erro_Padrão = sum_fit$std.err,
        IC_Inferior = sum_fit$lower,
        IC_Superior = sum_fit$upper,
        Eventos = sum_fit$n.event,
        Censuras = sum_fit$n.censor,
        Em_Risco = sum_fit$n.risk
      )
      
      names(tabela)[1] <- janela_tempo()
      
      tagList(
        h4(paste("Grupo:", .x)),
        renderDT({
          datatable(
            tabela,
            options = list(pageLength = 10,
                           lengthChange = FALSE,
                           language = list(    
                             url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
                           ),
                           # dom = 't',
                           # paging = FALSE, 
                           ordering = FALSE,
                           searching = FALSE,
                           info = FALSE),
            rownames = FALSE
          ) |>
            formatRound(columns = c('Sobrevivência', 'Erro_Padrão', 'IC_Inferior', 'IC_Superior'), 
                        digits = 4)
        
        })
      )
    })
    
    do.call(tagList, tabelas)
  })
  
}