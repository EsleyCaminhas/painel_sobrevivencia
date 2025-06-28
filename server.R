
library(shiny)
library(ggplot2)
library(data.table)

dados_cancer <- read_fst("data/dados_cancer_filtrado.fst",
                         as.data.table = TRUE)

## Aqui fica a parte onde trabalhamos com os dados e criamos os outputs
## que serão utilizados na ui

server <- function(input, output) {
  output$grafico_barras <- renderPlot({
    
    variavel_selecionada <- input$variavel
    
    ggplot(dados_cancer, aes(x = .data[[variavel_selecionada]])) +
      geom_bar(fill = "steelblue", alpha = 0.8) +
      labs(
        title = paste("Distribuição de Casos por", variavel_selecionada),
        x = paste0(variavel_selecionada),
        y = "Número de Casos"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Melhora legibilidade
  })
}