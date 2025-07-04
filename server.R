## server.R

dados_cancer <- read_fst("data/dados_cancer_filtrado.fst",
                         as.data.table = TRUE) |>
                filter(TEMPO_OBS_DIAG_MESES > 0)

## Aqui fica a parte onde trabalhamos com os dados e criamos os outputs
## que serão utilizados na ui

server <- function(input, output) {
  
  #### aba Análises Gráficas
  
  ##############################################################################
  
  ## Histograma
  
  dados_filtrados <- reactive({
    dados_cancer |>
      filter(TOPOGRUP_GRUPO %in% input$grupo_cid_1)
  })
  
  output$alert_box1 <- renderUI({
    if(length(input$grupo_cid_1) < 1) {
      div(class = "alert alert-warning",
          icon("exclamation-triangle"),
          "Selecione, pelo menos, uma variável para visualizar o gráfico.")
    }
  })

  
  output$grafico_barras <- renderHighchart({
    
    req(input$grupo_cid_1)
    
    contagem <- dados_filtrados() |>
      count(.data[[input$variavel_1]])
    
    nome_var <- case_when(input$variavel_1 == "SEXO" ~ "Sexo",
                          input$variavel_1 == "FAIXAETAR" ~ "Faixa etária",
                          input$variavel_1 == "GRUPO_EC" ~ "Estádio clínico",
                          input$variavel_1 == "ULTINFO" ~ "Desfecho Tratamento",
                          input$variavel_1 == "TRATAMENTO" ~ "Tratamento")
    
    hchart(contagem, "column", 
           hcaes(x = !!sym(input$variavel_1), y = n),
           name = "Número de observações",
           color = "#4682B4") |>
      hc_title(text = paste("Frequência observada para a variável ", nome_var)) |>
      hc_xAxis(title = list(text = nome_var)) |>
      hc_yAxis(max = max(20000, max(contagem$n, na.rm = TRUE)),
               title = list(text = "Número de observações")) 
  })
  
  #### aba Curvas de Kaplan-Meier
  
  ##############################################################################
  
  janela_tempo <- reactive({
    case_when(
      input$len_tempo == "MESES" ~ "Tempo (meses)",
      input$len_tempo == "TRI" ~ "Tempo (trimestres)",
      input$len_tempo == "ANO" ~ "Tempo (anos)"
    )
  })
  
  ## Gráfico Kaplan-Meier
  
  dados_filtrados_km <- reactive({
    
    dados <- dados_cancer |>
      filter(TOPOGRUP_GRUPO %in% input$grupo_cid_2) |>
      select(tempo = paste0("TEMPO_OBS_",input$Tempo_int,"_",input$len_tempo),
             Grupo = input$km_variable,
             DESFECHO
      ) |>
      mutate(tempo = as.numeric(tempo))
    
    if (input$km_variable == "IDADE"){
      
      cutpoint <- surv_cutpoint(
        data = dados,
        time = "tempo",
        event = "DESFECHO",
        variables = "Grupo"
      )
      
      valor_cut <- (cutpoint$cutpoint)$cutpoint
      
      dados$Grupo <- factor(surv_categorize(cutpoint)$Grupo,
                             levels = c("low", "high"),
                             labels = c(paste0("Idade maior ou igual a ", round(valor_cut)),
                                        paste0("Idade menor que ", round(valor_cut))))
      
    } 
    
    dados
    
  })
  
  output$alert_box2 <- renderUI({
    if(length(input$grupo_cid_2) < 1) {
      div(class = "alert alert-warning",
          icon("exclamation-triangle"),
          "Selecione, pelo menos, uma variável para visualizar o gráfico.")
    }
  })
  
  output$km_plot <- renderHighchart({
    
    req(input$grupo_cid_2)
    
    dados <- dados_filtrados_km()
    
    fit <- survfit(Surv(tempo, DESFECHO) ~ Grupo, data = dados)
    
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
      hc_plotOptions(series = list(marker = list(radius = 0)))
  })
  
  ##############################################################################
  
  ## Tabela Kaplan-Meier
  
  output$tabelas_por_grupo <- renderUI({
    
    req(input$grupo_cid_2)
    
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
  
  ##############################################################################
  
  janela_tempo2 <- reactive({
    case_when(
      input$len_tempo2 == "MESES" ~ "Tempo (meses)",
      input$len_tempo2 == "TRI" ~ "Tempo (trimestres)",
      input$len_tempo2 == "ANO" ~ "Tempo (anos)"
    )
  })
  
  output$alert_box3 <- renderUI({
    if(length(input$grupo_cid_3) < 1) {
      div(class = "alert alert-warning",
          icon("exclamation-triangle"),
          "Selecione, pelo menos, uma variável para visualizar o gráfico.")
    }
  })
  
  ## Gráfico função de risco
  
  dados_filtrados_hazard <- reactive({
    dados_cancer |>
      filter(TOPOGRUP_GRUPO %in% input$grupo_cid_3) |>
      select(tempo = paste0("TEMPO_OBS_",input$Tempo_int2,"_",input$len_tempo2),
             Grupo = input$hazard_variable,
             DESFECHO
      ) |>
      mutate(Grupo = as.factor(Grupo)) |>
      as.data.frame()
  })
  
  output$hazard_plot <- renderHighchart({
    
    req(input$grupo_cid_3)
    
    dados <- dados_filtrados_hazard()
    
    categorias <- levels(dados$Grupo)
    lista_risco <- list()
    
    for(cat in categorias) {
      dados_cat <- dados |> filter(Grupo == cat)
      
      estimativa <- muhaz(
        times = as.numeric(dados_cat$tempo),
        delta = dados_cat$DESFECHO,
        min.time = min(dados_cat$tempo),
        max.time = max(dados_cat$tempo)
      )
      
      lista_risco[[cat]] <- data.frame(
        tempo = estimativa$est.grid,
        risco = estimativa$haz.est,
        categoria = cat
      )
    }
    
    df_plot <- bind_rows(lista_risco)
    
    hchart(
      df_plot,
      type = "line",
      hcaes(x = tempo, y = risco, group = categoria),
      marker = list(enabled = FALSE) 
      ) |>
      hc_title(text = "Função de Risco") |>
      hc_xAxis(title = list(text = paste0(janela_tempo2()))) |>
      hc_yAxis(title = list(text = "Taxa de Risco Instantânea")) |>
      hc_tooltip(
        headerFormat = "<b>{point.series.name}</b><br>",
        pointFormat = paste0(janela_tempo2(), ": {point.x:.2f} <br> Risco: {point.y:.4f}")
      ) |>
      hc_legend(align = "center", verticalAlign = "bottom", layout = "horizontal")
  })
  
  ##############################################################################
  
}