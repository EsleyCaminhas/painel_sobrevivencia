
## ui.R

ui <- dashboardPage(
  
  #Cabecalho
  dashboardHeader(
    title = "Painel Sobrev"
  ),
  
  #Barra lateral
  dashboardSidebar(
    sidebarMenu(
      menuItem("Sobre os dados",
               tabName = "Info"),
      menuItem("Análises Gráficas",
               tabName = "Graphs")
    )
  ),
  
  #Corpo principal
  dashboardBody(
    tabItems(
      
      #Secao com as informacoes
      tabItem(tabName = "Info",
            
              h2("Lorem Ipsum Dolor Sit Amet"),
              
              p(
                "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam auctor,
              nisl eget ultricies tincidunt, nisl nisl aliquam nisl, eget ultricies
              nisl nisl eget nisl. Nullam auctor, nisl eget ultricies tincidunt,
              nisl nisl aliquam nisl, eget ultricies nisl nisl eget nisl."
              ),
      
              p(
                "Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere
              cubilia curae; Nullam auctor, nisl eget ultricies tincidunt, nisl nisl
              aliquam nisl, eget ultricies nisl nisl eget nisl. Donec euismod, nisl
              eget ultricies tincidunt, nisl nisl aliquam nisl, eget ultricies nisl
              nisl eget nisl."
              ),
              
              br(),
              
              h3("Pellentesque Habitant Morbi"),
              
              p(
                "Pellentesque habitant morbi tristique senectus et netus et malesuada fames
              ac turpis egestas. Nullam auctor, nisl eget ultricies tincidunt, nisl
              nisl aliquam nisl, eget ultricies nisl nisl eget nisl."
              )
      ),
      #Secao com os graficos
      tabItem(tabName = "Graphs",
              
              sidebarLayout(
                sidebarPanel(
                  width = 3,
                  
                  pickerInput(
                    inputId = "grupo_cid",
                    label = "Topografia (grupo)", 
                    choices = c("C50 Mama" = "C50 Mama",
                                "C51-C58 Órgãos genitais femininos" = "C51-C58 Órgãos genitais femininos",
                                "C60-C63 Órgãos genitais masculinos" = "C60-C63 Órgãos genitais masculinos",
                                "C64-C68 Trato urinário" = "C64-C68 Trato urinário",
                                "C69-C72 Olho, cérebro e outras partes do SNC" = "C69-C72 Olho, cérebro e outras partes do SNC",
                                "C73-C75 Tiróide e outras glândulas" = "C73-C75 Tiróide e outras glândulas",
                                "C76 Out. localizações e localizações mal definidas" = "C76 Out. localizações e localizações mal definidas",
                                "C77 Linfonodos" = "C77 Linfonodos",
                                "C80 Localização primária desconhecida" = "C80 Localização primária desconhecida"),
                    selected = c("C50 Mama",
                                 "C51-C58 Órgãos genitais femininos",
                                 "C60-C63 Órgãos genitais masculinos",
                                 "C64-C68 Trato urinário",
                                 "C69-C72 Olho, cérebro e outras partes do SNC",
                                 "C73-C75 Tiróide e outras glândulas",
                                 "C76 Out. localizações e localizações mal definidas",
                                 "C77 Linfonodos",
                                 "C80 Localização primária desconhecida"),
                    multiple = TRUE,
                    options = list(
                      `actions-box` = TRUE,       
                      `selected-text-format` = "count > 8",
                      `count-selected-text` = "C50-C80",
                      `none-selected-text` = "Nenhum item selecionado"
                    )),
                  
                  selectInput(
                    inputId = "variavel",
                    label = "Selecione a variável para o gráfico:",
                    choices = c("Sexo" = "SEXO",
                                "Faixa etária" = "FAIXAETAR",
                                "Estádio clínico" = "GRUPO_EC",
                                "Desfecho Tratamento" = "ULTINFO"),
                    selected = "SEXO"
                  )
                ),
                
                mainPanel(
                  withSpinner(
                    highchartOutput("grafico_barras"),
                    type = 6
                  )
                )
              )
      )
    )
  )
)
