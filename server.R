
library(shiny)
library(dplyr)
library(highcharter)
library(data.table)

dados_cancer <- read_fst("data/dados_cancer_filtrado.fst",
                         as.data.table = TRUE) 

## Aqui fica a parte onde trabalhamos com os dados e criamos os outputs
## que serão utilizados na ui

server <- function(input, output) {
  
  #Objeto reativo, condicionada a inputs, quando chamar usar dados_filtrados()
  dados_filtrados <- reactive({
    dados_cancer |>
      filter(TOPOGRUP_GRUPO %in% input$grupo_cid)
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
      count(.data[[input$variavel]])
    
    hchart(contagem, "column", 
           hcaes(x = !!sym(input$variavel), y = n),
           name = "Número de observações",
           color = "#4682B4") %>%
      hc_title(text = paste("Distribuição por", input$variavel)) %>%
      hc_xAxis(title = list(text = input$variavel)) %>%
      hc_yAxis(title = list(text = "Número de observações"))
  })
  
}