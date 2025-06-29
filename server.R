
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
  
}