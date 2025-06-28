
library(shiny)
library(shinycssloaders)

## Aqui fica a parte onde definimos a aparencia do app e onde serão utilizados 
## os outputs definidos no server

ui <- fluidPage(
  titlePanel(""),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "variavel",
        label = "Selecione a variável para o gráfico:",
        
        choices = c("Sexo" = "SEXO",
                    "Faixa Etária" = "FAIXAETAR"),
        
        selected = "SEXO"
      )
    ),
    
    mainPanel(
      withSpinner(
      plotOutput("grafico_barras"),
      type = 6
      )
    )
  )
)
