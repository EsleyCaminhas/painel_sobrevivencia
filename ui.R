
## ui.R

ui <- dashboardPage(
  
  skin = "purple",
  
  #Cabecalho
  dashboardHeader(
    title = "Painel ASO",
    tags$li(class = "dropdown",
            tags$style(".justified-text { text-align: justify; }")
    )
  ),
  
  #Barra lateral
  dashboardSidebar(
    sidebarMenu(
      menuItem("Sobre o Painel",
               tabName = "Info"),
      menuItem("Análise das Variáveis",
               tabName = "Graphs"),
      menuItem("Curvas de Kaplan-Meier",
               tabName = "KM"),
      menuItem("Função de Risco",
               tabName = "Hazard")
    )
  ),
  
  #Corpo principal
  dashboardBody(
    
    tags$style("justified-text { text-align: justify; }"),
    
    tabItems(
      
      #Secao com as informacoes
      tabItem(tabName = "Info",
            
              h2("Painel de Análise de Sobrevivência Oncológica (ASO)"),
              
              p(
                "Seja bem vinda(o)!"
              ),
              
              p(
                class = "justified-text",
                "Este painel foi desenvolvido como parte do seminário apresentado à disciplina de Análise de Sobrevivência, ministrada ao curso de Estatística da Universidade Federal do Espírito Santo.
                Seu objetivo é permitir a exploração e a análise visual de dados de sobrevivência de pacientes oncológicos, provenientes da Fundação Oncocentro de São Paulo (FOSP), instituição de referência vinculada à Secretaria de Estado de São Paulo.
                "
              ),
              
              p(
                class = "justified-text",
                "A FOSP é uma das principais fontes de dados oncológicos no Brasil, mantendo registros hospitalares de grande relevância para a pesquisa e o desenvolvimento de políticas públicas de combate ao câncer no país.
                "
              ),
              
              br(),
              
              h3("Sobre os Dados"),
              
              p(
                class = "justified-text",
                "Os dados utilizados neste painel compreendem um subconjunto específico de tipos de câncer, agrupados por topografia, segundo a Classificação Internacional de Doenças para Oncologia (CID-O), correspondendo aos códigos de CID C50 a CID C80. Este grupo engloba:"
              ),
              
              tags$ul(
                tags$li("C50:  Mama"),
                tags$li("C51-C58: Órgãos genitais femininos"),
                tags$li("C60-C63: Órgãos genitais masculinos"),
                tags$li("C64-C68: Trato urinário"),
                tags$li("C69-C72: Olho, cérebro e outras partes do SNC"),
                tags$li("C73-C75: Tiróide e outras glândulas"),
                tags$li("C76: Outras localizações e localizações mal definidas"),
                tags$li("C77: Linfonodos"),
                tags$li("C80: Localização primária desconhecida")
              ),
              
              br(),
              
              p(
                class = "justified-text",
                "Para mais informações sobre a FOSP visite o site:",
                a("https://fosp.saude.sp.gov.br/", href="https://fosp.saude.sp.gov.br/"),
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
                    choices = c("Faixa etária" = "FAIXAETAR",
                                "Sexo" = "SEXO",
                                "Estádio clínico" = "GRUPO_EC",
                                "Tratamento" = "TRATAMENTO",
                                "Desfecho Tratamento" = "ULTINFO"
                                ),
                    selected = "FAIXAETAR"
                  )
                ),
                
                mainPanel(
                  
                  uiOutput("alert_box1"),
                  
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
            
            h4(strong("Filtros para grupo e variável")),
            
            hr(),
            
            width = 3,

            pickerInput(
              inputId = "grupo_cid_2",
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
              inputId = "km_variable",
              label = "Selecione a variável:",
              choices = c(                          
                          "Faixa etária" = "FAIXAETAR",
                          "Sexo" = "SEXO",
                          "Idade" = "IDADE",
                          "Estágio clínico" = "GRUPO_EC",
                          "Tratamento" = "TRATAMENTO"),
              selected = "FAIXAETAR"
              ),
            
            br(),
            
            h4(strong("Filtros relacionados ao tempo de acompanhamento")),
            
            hr(),
            
            selectInput(
              inputId = "Tempo_int",
              label = "Selecione o inicio do acompanhamento:",
              choices = c("Diagnóstico" = "DIAG",
                          "Consulta" = "CONSULT"),
              selected = "DIAG"
              ),
          
            selectInput(
              inputId = "len_tempo",
              label = "Selecione a unidade de tempo:",
              choices = c("Meses (30 dias)" = "MESES",
                          "Trimestres (90 dias)" = "TRI",
                          "Anos (365 dias)" = "ANO"),
              selected = "TRI"
              ),
            
            hr(),
            
            shinyWidgets::materialSwitch(
              inputId = "show_ci",
              label = "Mostrar o intervalo de confiança?", 
              value = FALSE,
              status = "info"
            )
            ),
          mainPanel(
            
            uiOutput("alert_box2"),
            
            withSpinner(
              highchartOutput("km_plot", height = "600px"),
              type = 6
            ),
            withSpinner(
              uiOutput("tabelas_por_grupo"),
              type = 6
            )
          )
        )
      ),
      ##########################################################################
      
      tabItem(tabName = "Hazard",
              sidebarLayout(
                sidebarPanel(
                  
                  h4(strong("Filtros para grupo e variável")),
                  
                  hr(),
                  
                  width = 3,
                  
                  pickerInput(
                    inputId = "grupo_cid_3",
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
                    selected = c("C69-C72 Olho, cérebro e outras partes do SNC"),
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
                    inputId = "hazard_variable",
                    label = "Selecione a variável:",
                    choices = c(
                      "Sexo" = "SEXO",
                      # "Idade" = "IDADE",
                      "Faixa etária" = "FAIXAETAR",
                      "Estágio clínico" = "GRUPO_EC",
                      "Tratamento" = "TRATAMENTO"),
                    selected = "FAIXAETAR"
                  ),
                  
                  br(),
                  
                  h4(strong("Filtros relacionados ao tempo de acompanhamento")),
                  
                  hr(),
                  
                  selectInput(
                    inputId = "Tempo_int2",
                    label = "Selecione o inicio do acompanhamento:",
                    choices = c("Diagnóstico" = "DIAG",
                                "Consulta" = "CONSULT"),
                    selected = "DIAG"
                  ),
                  
                  selectInput(
                    inputId = "len_tempo2",
                    label = "Selecione a unidade de tempo:",
                    choices = c("Meses (30 dias)" = "MESES",
                                "Trimestres (90 dias)" = "TRI",
                                "Anos (365 dias)" = "ANO"),
                    selected = "TRI"
                  )
                ),
                mainPanel(
                  
                  uiOutput("alert_box3"),
                  
                  withSpinner(
                    highchartOutput("hazard_plot", height = "600px"),
                    type = 6
                  )
                )
              )
      )
    )
  )
)
