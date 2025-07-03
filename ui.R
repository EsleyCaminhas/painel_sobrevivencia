
## ui.R

ui <- dashboardPage(
  
  skin = "blue",
  
  #Cabecalho
  dashboardHeader(
    title = "Painel Sobrev"
  ),
  
  #Barra lateral
  dashboardSidebar(
    sidebarMenu(
      menuItem("Sobre os dados",
               tabName = "Info"),
      menuItem("Análise das variáveis",
               tabName = "Graphs"),
      menuItem("Curvas de Kaplan-Meier",
               tabName = "KM")
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
                  
                  h4(strong("Filtros para grupo e variável")),
                  
                  hr(),
                  
                  width = 3,
                  
                  pickerInput(
                    inputId = "grupo_cid_1",
                    label = "Selecione o grupo (topografia):", 
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
                      `none-selected-text` = "Nenhum item selecionado",
                      `deselect-all-text` = "Desselecionar todas",
                      `select-all-text` = "Selecionar todas"
                    )),
                  
                  selectInput(
                    inputId = "variavel_1",
                    label = "Selecione a variável:",
                    choices = c("Sexo" = "SEXO",
                                "Faixa etária" = "FAIXAETAR",
                                "Estádio clínico" = "GRUPO_EC",
                                "Tratamento" = "TRATAMENTO",
                                "Desfecho Tratamento" = "ULTINFO"
                                ),
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
      ),
      
      #Secao com os graficos
      tabItem(tabName = "KM",
        sidebarLayout(
          sidebarPanel(
            width = 3,

            pickerInput(
              inputId = "grupo_cid_2",
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
                `none-selected-text` = "Nenhum item selecionado",
                `deselect-all-text` = "Desselecionar todas",
                `select-all-text` = "Selecionar todas"
              )),

            selectInput(
              inputId = "km_variable",
              label = "Selecione a variável para o gráfico:",
              choices = c(
                          "Sexo" = "SEXO",
                          "Idade" = "IDADE",
                          "Faixa etária" = "FAIXAETAR",
                          "Estágio clínico" = "GRUPO_EC",
                          "Tratamento" = "TRATAMENTO",
                          "Dias entre consulta e diagnóstico" = "CONSDIAG",
                          "Dias entre consulta e tratamento" = "TRATCONS",
                          "Dias entre diagnóstico e tratamento" = "DIAGTRAT",
                          #"Data da primeira consulta" = "DTCONSULT",
                          #"Data do diagnóstico" = "DTDIAG",
                          #"Data de início do tratamento" = "DTTRAT",
                          #"Data da última informação" = "DTULTINFO",
                          "Motivo de não tratamento" = "NAOTRAT"),
              selected = "SEXO"
              ),
            selectInput(
              inputId = "Tempo_int",
              label = "Selecione o tempo de interesse:",
              choices = c("TEMPO_OBS_DIAG" = "TEMPO_OBS_DIAG",
                          "TEMPO_OBS_CONSULT" = "TEMPO_OBS_CONSULT",
                          "TEMPO_OBS_TRAT" = "TEMPO_OBS_TRAT"),
              selected = "TEMPO_OBS_DIAG"
              ),
          
            selectInput(
              inputId = "len_tempo",
              label = "Selecione o intervalo de tempo:",
              choices = c("diário" = 1,
                          "mensal" = 30,
                          "trimestral" = 90,
                          "anual" = 365),
              selected = 365
              ),
            shinyWidgets::materialSwitch(
              inputId = "show_ci",
              label = "Mostrar intervalo de confiança", 
              value = FALSE,
              status = "info"
            )
            ),
          
          

          mainPanel(
            withSpinner(
              plotOutput("km_plot", height = "600px"),
              type = 6
            )
          )
        )
      )
    )
  )
)
