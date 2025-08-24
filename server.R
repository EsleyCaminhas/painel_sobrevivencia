# Carregue todos os pacotes necessários no início
library(shiny)
library(shinyWidgets)
library(fst)
library(dplyr)
library(data.table)
library(highcharter)
library(survival)
library(survminer)
library(DT)
library(broom)
library(purrr)
library(muhaz)
library(shinycssloaders)


# Dica: É uma boa prática usar caminhos relativos para que o projeto funcione em qualquer computador.
# O caminho original "data/dados_cancer_filtrado.fst" é mais portável.
dados_cancer_base <- read_fst("data/dados_cancer_filtrado.fst",
                              as.data.table = TRUE) |>
  filter(TEMPO_OBS_DIAG_MESES > 0)


## Aqui fica a parte onde trabalhamos com os dados e criamos os outputs
## que serão utilizados na ui

server <- function(input, output, session) {
  
  # ==============================================================================
  # ==== INÍCIO DA MODIFICAÇÃO: LÓGICA DE CUTPOINT MOVIDA PARA DENTRO DO SERVER ====
  # ==============================================================================
  
  # 1. Criar uma expressão reativa que depende das seleções de tempo do usuário.
  # Usaremos os inputs da aba Kaplan-Meier (input$Tempo_int e input$len_tempo) para controlar o cálculo.
  dados_cancer_reativo <- reactive({
    
    # Garante que os inputs de tempo existem antes de prosseguir
    req(input$Tempo_int, input$len_tempo)
    
    # Cria o nome da coluna de tempo dinamicamente com base na seleção do usuário
    tempo_selecionado <- paste0("TEMPO_OBS_", input$Tempo_int, "_", input$len_tempo)
    
    # Filtra o data.frame para garantir que o tempo seja maior que zero na escala selecionada
    dados_filtrados_tempo <- dados_cancer_base |>
      filter(.data[[tempo_selecionado]] > 0)
    
    # 2. Encontrar o ponto de corte ótimo para a idade usando o tempo dinâmico.
    res_cutpoint <- surv_cutpoint(
      data = dados_filtrados_tempo,
      time = tempo_selecionado, # Usa a coluna de tempo dinâmica
      event = "DESFECHO",
      variables = "IDADE",
      minprop = 0.2
    )
    
    # 3. Extrair o valor numérico do ponto de corte.
    ponto_de_corte <- res_cutpoint$cutpoint$cutpoint
    
    # 4. Criar as etiquetas descritivas.
    label_menor <- paste0("Idade menor que ", ponto_de_corte," anos")
    label_maior <- paste0("Idade maior ou igual a ", ponto_de_corte," anos")
    
    # 5. Adicionar a nova coluna e renomear os níveis do fator ao data.frame filtrado.
    dados_com_idade_estratificada <- dados_filtrados_tempo |>
      mutate(
        IDADE_ESTRATIFICADA = surv_categorize(res_cutpoint)$IDADE,
        IDADE_ESTRATIFICADA = recode_factor(IDADE_ESTRATIFICADA,
                                            "low" = label_menor,
                                            "high" = label_maior)
      )
    
    # Retorna o dataframe final e reativo
    return(dados_com_idade_estratificada)
  })
  # ==============================================================================
  # ==== FIM DA MODIFICAÇÃO ====
  # ==============================================================================
  
  
  #### aba Análises Gráficas
  
  ##############################################################################
  
  ## Histograma
  
  dados_filtrados <- reactive({
    # ALTERADO: Usa dados_cancer_reativo() em vez de dados_cancer
    dados_cancer_reativo() |>
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
                          input$variavel_1 == "IDADE_ESTRATIFICADA" ~ "Idade (ponto de corte)",
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
      input$len_tempo == "DIAS" ~ "Tempo (dias)",
      input$len_tempo == "MESES" ~ "Tempo (meses)",
      input$len_tempo == "TRI" ~ "Tempo (trimestres)",
      input$len_tempo == "ANO" ~ "Tempo (anos)"
    )
  })
  
  ## Gráfico Kaplan-Meier
  
  dados_filtrados_km <- reactive({
    # ALTERADO: Usa dados_cancer_reativo() em vez de dados_cancer
    dados <- dados_cancer_reativo() |>
      filter(TOPOGRUP_GRUPO %in% input$grupo_cid_2) |>
      select(tempo = paste0("TEMPO_OBS_",input$Tempo_int,"_",input$len_tempo),
             Grupo = input$km_variable,
             DESFECHO
      ) |>
      mutate(tempo = as.numeric(tempo))
    
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
  
  # ================= INÍCIO DA MODIFICAÇÃO =================
  output$logrank_test_output <- renderUI({
    # Requer que o switch esteja ativado e que haja filtros selecionados
    req(input$show_logrank, input$grupo_cid_2)
    
    dados <- dados_filtrados_km()
    
    # O teste só faz sentido para 2 ou mais grupos
    if (length(unique(dados$Grupo)) < 2) {
      return(
        div(class = "alert alert-info",
            style = "margin-top: 15px;",
            icon("info-circle"),
            "O teste de Log-Rank requer duas ou mais curvas para comparação.")
      )
    }
    
    # Realiza o teste de Log-Rank
    logrank_test <- survdiff(Surv(tempo, DESFECHO) ~ Grupo, data = dados)
    
    # Extrai a estatística e o p-valor
    chisq_stat <- logrank_test$chisq
    p_value <- 1 - pchisq(logrank_test$chisq, length(logrank_test$n) - 1)
    
    # Formata o p-valor para melhor visualização
    p_value_formatted <- if (p_value < 0.001) {
      "< 0.001"
    } else {
      format(round(p_value, 3), nsmall = 3)
    }
    
    # Cria o output em HTML
    tagList(
      hr(),
      h4(strong("Resultado do Teste de Log-Rank")),
      p(class = "justified-text",
        "O teste de Log-Rank compara as curvas de sobrevivência de dois ou mais grupos.
         Um p-valor pequeno (geralmente < 0.05) sugere que há uma diferença estatisticamente
         significativa entre as curvas."),
      p(HTML(paste0("<b>Estatística Qui-quadrado:</b> ", round(chisq_stat, 3)))),
      p(HTML(paste0("<b>P-valor:</b> ", p_value_formatted)))
    )
  })
  # ================= FIM DA MODIFICAÇÃO =================
  
  ##############################################################################
  
  ## Tabela Kaplan-Meier
  
  output$tabelas_por_grupo <- renderUI({
    req(input$grupo_cid_2)
    
    dados <- dados_filtrados_km() |> arrange(Grupo)
    fit <- survfit(Surv(tempo, DESFECHO) ~ Grupo, data = dados)
    grupos <- unique(dados$Grupo)
    
    tagList(
      shinyWidgets::pickerInput(
        inputId = "grupos_selecionados",
        label = "Selecione os grupos para exibir as tabelas:",
        choices = grupos,
        selected = grupos[1:min(3, length(grupos))],
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `deselect-all-text` = "Nenhuma", `select-all-text` = "Todas")
      ),
      withSpinner(uiOutput("tabelas_selecionadas"))
    )
  })
  
  output$tabelas_selecionadas <- renderUI({
    req(input$grupos_selecionados)
    
    dados <- dados_filtrados_km() |> arrange(Grupo)
    fit <- survfit(Surv(tempo, DESFECHO) ~ Grupo, data = dados)
    
    tabelas <- map(input$grupos_selecionados, ~ {
      
      grupo_selecionado <- .x
      indice_grupo <- which(names(fit$strata) == grupo_selecionado)
      
      if (length(indice_grupo) == 0) {
        indice_grupo <- which(grepl(grupo_selecionado, names(fit$strata)))
      }
      
      req(length(indice_grupo) > 0)
      
      inicio <- ifelse(indice_grupo == 1, 1, sum(fit$strata[1:(indice_grupo-1)]) + 1)
      fim <- sum(fit$strata[1:indice_grupo])
      
      dados_grupo <- list(
        time = fit$time[inicio:fim],
        surv = fit$surv[inicio:fim],
        std.err = fit$std.err[inicio:fim],
        lower = fit$lower[inicio:fim],
        upper = fit$upper[inicio:fim],
        n.event = fit$n.event[inicio:fim],
        n.censor = fit$n.censor[inicio:fim],
        n.risk = fit$n.risk[inicio:fim]
      )
      
      tabela <- data.frame(
        tempo = dados_grupo$time,
        Sobrevivência = dados_grupo$surv,
        Erro_Padrão = dados_grupo$std.err,
        IC_Inferior = dados_grupo$lower,
        IC_Superior = dados_grupo$upper,
        Eventos = dados_grupo$n.event,
        Censuras = dados_grupo$n.censor,
        Em_Risco = dados_grupo$n.risk
      )
      
      names(tabela)[1] <- janela_tempo()
      
      tagList(
        h4(paste("Grupo:", grupo_selecionado)),
        renderDT({
          datatable(
            tabela,
            options = list(
              pageLength = 10,
              lengthChange = FALSE,
              language = list(
                url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
              ),
              ordering = FALSE,
              searching = FALSE,
              info = FALSE
            ),
            rownames = FALSE
          ) |>
            formatRound(columns = c('Sobrevivência', 'Erro_Padrão', 'IC_Inferior', 'IC_Superior'),
                        digits = 4)
        }),
        hr()
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
    # ALTERADO: Usa dados_cancer_reativo() em vez de dados_cancer
    dados_cancer_reativo() |>
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
      
      # >>>>> INÍCIO DA CORREÇÃO <<<<<
      # O bloco tryCatch impede que o app feche se a função muhaz() falhar.
      estimativa <- tryCatch({
        muhaz(
          times = as.numeric(dados_cat$tempo),
          delta = dados_cat$DESFECHO,
          min.time = min(dados_cat$tempo),
          max.time = max(dados_cat$tempo)
        )
      }, error = function(e) {
        # Se ocorrer um erro, retorna NULL para que possamos ignorar este grupo.
        NULL
      })
      
      # Pula para a próxima iteração do loop se a estimativa falhou
      if (is.null(estimativa)) {
        next
      }
      # >>>>> FIM DA CORREÇÃO <<<<<
      
      lista_risco[[cat]] <- data.frame(
        tempo = estimativa$est.grid,
        risco = estimativa$haz.est,
        categoria = cat
      )
    }
    
    # Se nenhuma estimativa funcionou, mostra um gráfico vazio
    validate(
      need(length(lista_risco) > 0, "Não foi possível estimar a função de risco para a escala de tempo selecionada.")
    )
    
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
  
  #### aba Modelo de Cox
  
  ##############################################################################
  
  # Alerta para o usuário selecionar as variáveis necessárias
  output$alert_box4 <- renderUI({
    if(length(input$grupo_cid_4) < 1 || length(input$cox_variables) < 1) {
      div(class = "alert alert-warning",
          icon("exclamation-triangle"),
          "Selecione, pelo menos, um grupo de topografia e uma variável para ajustar o modelo.")
    }
  })
  
  # Filtra os dados de forma reativa para o modelo de Cox
  dados_filtrados_cox <- reactive({
    req(input$grupo_cid_4, input$cox_variables)
    
    # ALTERADO: Usa dados_cancer_reativo() em vez de dados_cancer
    dados_cancer_reativo() |>
      filter(TOPOGRUP_GRUPO %in% input$grupo_cid_4) |>
      select(
        tempo = paste0("TEMPO_OBS_", input$Tempo_int3, "_", input$len_tempo3),
        DESFECHO,
        all_of(input$cox_variables)
      ) |>
      mutate(tempo = as.numeric(tempo)) |>
      na.omit()
  })
  
  # Reactive para ajustar o modelo de Cox (para ser reutilizado)
  cox_model_fit <- reactive({
    req(input$cox_variables)
    
    dados <- dados_filtrados_cox()
    
    validate(
      need(nrow(dados) > 0, "")
    )
    
    formula_str <- paste("Surv(tempo, DESFECHO) ~", paste(input$cox_variables, collapse = " + "))
    cox_formula <- as.formula(formula_str)
    
    coxph(cox_formula, data = dados)
  })
  
  
  # Tabela de resultados agora usa o modelo do reactive `cox_model_fit`
  output$cox_summary_table <- renderDT({
    
    validate(
      need(nrow(dados_filtrados_cox()) > 0, "Não há dados suficientes para as seleções de filtros aplicadas.")
    )
    
    cox_model <- cox_model_fit()
    
    summary_df <- broom::tidy(cox_model, exponentiate = TRUE, conf.int = TRUE) |>
      select(
        Variável = term,
        `Hazard Ratio (HR)` = estimate,
        `IC Inferior` = conf.low,
        `IC Superior` = conf.high,
        `Valor-p` = p.value
      )
    
    datatable(
      summary_df,
      options = list(pageLength = 10, lengthChange = FALSE, searching = FALSE, info = FALSE,
                     language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json')),
      rownames = FALSE
    ) |>
      formatRound(columns = c('Hazard Ratio (HR)', 'IC Inferior', 'IC Superior'), digits = 3) |>
      formatRound(columns = c('Valor-p'), digits = 2)
    
  })
  
  # Forest Plot do Modelo de Cox
  output$cox_forest_plot <- renderPlot({
    
    validate(
      need(nrow(dados_filtrados_cox()) > 0, "Não há dados suficientes para gerar o forest plot.")
    )
    
    ggforest(
      model = cox_model_fit(),
      data = dados_filtrados_cox()
    )
  })
  
  # Lógica para os resíduos de Schoenfeld
  
  # 1. Gera o seletor de variáveis dinamicamente
  output$schoenfeld_variable_selector_ui <- renderUI({
    req(input$cox_variables)
    
    tagList(
      h3("Teste de Pressupostos (Resíduos de Schoenfeld)"),
      p("Este teste verifica a premissa de riscos proporcionais do modelo de Cox.
        Se a linha suavizada no gráfico for aproximadamente horizontal e o p-valor for maior que 0.05,
        a premissa é considerada atendida para aquela variável."),
      
      pickerInput(
        inputId = "schoenfeld_vars",
        label = "Selecione as variáveis para visualizar os resíduos:",
        choices = input$cox_variables,
        selected = input$cox_variables,
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `deselect-all-text` = "Nenhuma", `select-all-text` = "Todas")
      )
    )
  })
  
  # Trocado eventReactive por reactive para reagir a TODAS as alterações, incluindo tempo.
  terms_to_plot_reactive <- reactive({
    req(cox_model_fit(), input$schoenfeld_vars)
    
    schoenfeld_test <- cox.zph(cox_model_fit())
    all_terms <- rownames(schoenfeld_test$table)
    
    terms_to_plot <- unlist(sapply(input$schoenfeld_vars, function(var) {
      grep(paste0("^", var), all_terms, value = TRUE)
    }))
    
    list(schoenfeld_test = schoenfeld_test, terms = terms_to_plot)
  })
  
  # 2. Gera os placeholders para os gráficos
  output$schoenfeld_plots_ui <- renderUI({
    plot_data <- terms_to_plot_reactive()
    req(plot_data$terms)
    
    map(plot_data$terms, ~ {
      plot_id <- gsub("[^[:alnum:]]", "", .x)
      plotOutput(outputId = paste0("schoenfeld_plot_", plot_id), height = "400px")
    })
  })
  
  # 3. Renderiza cada gráfico individualmente
  observe({
    plot_data <- terms_to_plot_reactive()
    req(plot_data$terms)
    
    schoenfeld_test <- plot_data$schoenfeld_test
    
    for (term in plot_data$terms) {
      local({
        my_term <- term
        plot_id <- gsub("[^[:alnum:]]", "", my_term)
        
        output[[paste0("schoenfeld_plot_", plot_id)]] <- renderPlot({
          plot_list <- ggcoxzph(schoenfeld_test, var = my_term, point.alpha = 0.5)
          
          plot_list[[1]] +
            labs(
              title = paste("Resíduos de Schoenfeld para:", my_term),
              subtitle = paste("Teste de Proporcionalidade, p-valor:", round(schoenfeld_test$table[my_term, "p"], 4))
            ) +
            theme_bw()
        })
      })
    }
  })
  
  # PDF da documentação
  observeEvent(input$generate, {
    output$pdfview <- renderUI({
      tags$iframe(style = "height:600px; width:100%", src = "documentacao.pdf")
    })
  })
  
  # Atualizando a seleção para evitar bugs para CID's de sexo exclusivo
  
  ## KM
  observe({
    # ATENÇÃO: Adicione "IDADE_ESTRATIFICADA" às opções aqui
    opcoes_base <- c("Faixa etária" = "FAIXAETAR", "Sexo" = "SEXO",
                     "Idade (ponto de corte)" = "IDADE_ESTRATIFICADA",
                     "Estágio clínico" = "GRUPO_EC", "Tratamento" = "TRATAMENTO")
    
    req(input$grupo_cid_2)
    
    grupos_especificos <- c("C51-C58 Órgãos genitais femininos",
                            "C60-C63 Órgãos genitais masculinos")
    
    opcoes_finais <- opcoes_base
    
    # Se APENAS um grupo de sexo específico for selecionado, remove a opção "Sexo"
    if (any(grupos_especificos %in% input$grupo_cid_2) && length(input$grupo_cid_2) == 1) {
      opcoes_finais <- opcoes_base[names(opcoes_base) != "Sexo"]
      
      # Se "Sexo" estava selecionado, muda para outra opção para evitar erro
      if (!is.null(input$km_variable) && input$km_variable == "SEXO") {
        updatePickerInput(session, "km_variable", selected = "IDADE_ESTRATIFICADA")
      }
    }
    
    updatePickerInput(session, "km_variable", choices = opcoes_finais)
    
  })
  
  ## Hazard
  observeEvent(input$grupo_cid_3, {
    # ================= INÍCIO DA CORREÇÃO =================
    opcoes <- c("Faixa etária" = "FAIXAETAR", "Sexo" = "SEXO",
                "Idade (ponto de corte)" = "IDADE_ESTRATIFICADA",
                "Estágio clínico" = "GRUPO_EC", "Tratamento" = "TRATAMENTO")
    # ================= FIM DA CORREÇÃO =================
    
    grupos_especificos <- c("C51-C58 Órgãos genitais femininos",
                            "C60-C63 Órgãos genitais masculinos")
    
    algum <- any(grupos_especificos %in% input$grupo_cid_3)
    
    if ((algum & length(input$grupo_cid_3) == 1)) {
      opcoes <- opcoes[opcoes != "SEXO"]
      
      selecionado_atual <- input$hazard_variable
      if (selecionado_atual == "SEXO") {
        updateSelectInput(session, "hazard_variable", selected = "FAIXAETAR")
      }
    }
    
    updateSelectInput(session, "hazard_variable", choices = opcoes)
  })
  
}