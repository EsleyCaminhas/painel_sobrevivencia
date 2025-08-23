
library(foreign)
library(dplyr)
library(lubridate) 

library(data.table)
library(fst)

## Aqui vamos filtrar as CID's que iremos utilizar

dados_cancer <- read.dbf("data/pacigeral.dbf") # disponivel no classroom

# Neoplasias Urogenitais, Urinárias, Olho e Sistema Nervoso Central, 
# Endócrinas e Secundárias/Mal Definidas 

# C50 a C80 (incluindo todas as CIDs neste intervalo)

grupo_cids <- paste0("C",50:80)
grupo_cids
## Filtrando cids
dados_cancer_filtrado1 <- dados_cancer |>
  filter(TOPOGRUP %in% grupo_cids)

## Filtrando variaveis 
dados_cancer_filtrado2 <- dados_cancer_filtrado1 |>
  select(
    TOPOGRUP, #Grupo da topografia (Seleção) 
    SEXO, #Sexo do paciente (KM)
    IDADE, #Candidata a maxstat
    FAIXAETAR, #Faixa etaria do paciente (KM)
    EC, #Estadio clinico (KM)
    
    #Candidatas para o uso do maxstat
    CONSDIAG, #Diferença em dias entre as datas de consulta o diagnostico
    TRATCONS, #Diferença em dias entre as datas de consulta e tratamento
    DIAGTRAT, #Diferença em dias entra as datas de tratamento e diagnostico
    
    DTCONSULT, #Data da 1a consulta
    DTDIAG, #Data do diagnostico
    
    DTTRAT, #Data de inicio do tratamento (Só para checar não entra na conta)
    NAOTRAT, #Código da razão para não realização do tratamento (Só para checar não entra na conta)
    
    TRATAMENTO, #Código de combinação dos tratamentos realizados
    
    DTULTINFO, #Data da ultima informacao do paciente 
    
    #Sobre desfecho
    ULTINFO, #Ultima informacao do paciente 
    ) |>
  
  mutate(
    TOPOGRUP_GRUPO = factor(
      case_when(
      TOPOGRUP == "C50" ~ "C50 Mama",
      TOPOGRUP %in% c("C51", "C52", "C53", "C54", "C55", "C56", "C57", "C58") ~ "C51-C58 Órgãos genitais femininos",
      TOPOGRUP %in% c("C60", "C61", "C62", "C63") ~ "C60-C63 Órgãos genitais masculinos",
      TOPOGRUP %in% c("C64", "C65", "C66", "C67", "C68") ~ "C64-C68 Trato urinário",
      TOPOGRUP %in% c("C69", "C70", "C71", "C72") ~ "C69-C72 Olho, cérebro e outras partes do SNC",
      TOPOGRUP %in% c("C73", "C74", "C75") ~ "C73-C75 Tiróide e outras glândulas",
      TOPOGRUP == "C76" ~ "C76 Out. localizações e localizações mal definidas",
      TOPOGRUP == "C77" ~ "C77 Linfonodos",
      TOPOGRUP == "C80" ~ "C80 Localização primária desconhecida",
    )),
      
    SEXO = factor(
      case_when(
        SEXO == 1 ~ "Masculino",
        SEXO == 2 ~ "Feminino"
    )),
    
    FAIXAETAR = factor(
      case_when(
        FAIXAETAR == "00-09" ~ "0 a 9 anos",
        FAIXAETAR == "10-19" ~ "10 a 19 anos",
        FAIXAETAR == "20-29" ~ "20 a 29 anos",
        FAIXAETAR == "30-39" ~ "30 a 39 anos",
        FAIXAETAR == "40-49" ~ "40 a 49 anos",
        FAIXAETAR == "50-59" ~ "50 a 59 anos",
        FAIXAETAR == "60-69" ~ "60 a 69 anos",
        FAIXAETAR == "70+" ~ "70 anos ou mais"
      ),
      levels = c(
        "0 a 9 anos",
        "10 a 19 anos",
        "20 a 29 anos",
        "30 a 39 anos",
        "40 a 49 anos",
        "50 a 59 anos",
        "60 a 69 anos",
        "70 anos ou mais"
      ),
      ordered = TRUE 
    ),
    
    DTULTINFO = dmy(as.character(DTULTINFO)),
    
    #Tempo observado
    TEMPO_OBS_DIAG_DIAS = as.numeric(DTULTINFO - DTDIAG),
    TEMPO_OBS_CONSULT_DIAS = as.numeric(DTULTINFO - DTCONSULT),
    
    TEMPO_OBS_DIAG_MESES = floor(as.numeric(DTULTINFO - DTDIAG) / 30) + 1,
    TEMPO_OBS_CONSULT_MESES = floor(as.numeric(DTULTINFO - DTCONSULT) / 30) + 1,
    
    TEMPO_OBS_DIAG_TRI = floor(as.numeric(DTULTINFO - DTDIAG) / 90) + 1,
    TEMPO_OBS_CONSULT_TRI = floor(as.numeric(DTULTINFO - DTCONSULT) / 90) + 1,
    
    TEMPO_OBS_DIAG_ANO = floor(as.numeric(DTULTINFO - DTDIAG) / 365) + 1,
    TEMPO_OBS_CONSULT_ANO = floor(as.numeric(DTULTINFO - DTCONSULT) / 365) + 1,

    #Tratamento
    TRATAMENTO = factor(
      case_when(
        TRATAMENTO == "A" ~ "Cirurgia",
        TRATAMENTO == "B" ~ "Radioterapia",
        TRATAMENTO == "C" ~ "Quimio",
        TRATAMENTO == "D" ~ "Cirurgia + Radioterapia",
        TRATAMENTO == "E" ~ "Cirurgia + Quimio",
        TRATAMENTO == "F" ~ "Radioterapia + Quimio",
        TRATAMENTO == "G" ~ "Cirurgia + Radio + Quimio",
        TRATAMENTO == "H" ~ "Cirurgia + Radio + Quimio + Hormonio",
        TRATAMENTO == "I" ~ "Outras combinações de tratamento",
        TRATAMENTO == "J" ~ "Nenhum tratamento realizado"
      ),
      levels = c(
        "Cirurgia",
        "Radioterapia",
        "Quimio",
        "Cirurgia + Radioterapia",
        "Cirurgia + Quimio",
        "Radioterapia + Quimio",
        "Cirurgia + Radio + Quimio",
        "Cirurgia + Radio + Quimio + Hormonio",
        "Outras combinações de tratamento",
        "Nenhum tratamento realizado"
      ),
      ordered = FALSE  # Se TRUE, define como variável ordinal (hierarquia entre categorias)
    ),
    
    #Estagio da doenca (precisa fazer um double check nisso aqui)
    GRUPO_EC = factor(
      case_when(
        EC %in% c("0", "0A", "0IS", "IS") ~ "Estágio 0",
        EC %in% c("I", "IA", "IA1", "IA2", "IB", "IB1", "IB2", "IC") ~ "Estágio I",
        EC %in% c("II", "IIA", "IIA1", "IIA2", "IIB", "IIC") ~ "Estágio II",
        EC %in% c("III", "IIIA", "IIIB", "IIIC", "IIIC1", "IIIC2") ~ "Estágio III",
        EC %in% c("IV", "IVA", "IVB", "IVC") ~ "Estágio IV",
        EC == "X" ~ "X",
        EC == "Y" ~ "Y"
    )),
    
    # 1 – VIVO, COM CÂNCER
    # 2 – VIVO, SOE
    # 3 – OBITO POR CANCER
    # 4 – OBITO POR OUTRAS CAUSAS, SOE
    
    ULTINFO = factor(
      case_when(
        ULTINFO == 1 ~ "Vivo, com câncer",
        ULTINFO == 2 ~ "Vivo, sem outra especificação",
        ULTINFO == 3 ~ "Óbito por câncer",
        ULTINFO == 4 ~ "Óbito por outras causas, sem outra especificação"
    )),
    
    DESFECHO = case_when(
      ULTINFO %in% c("Vivo, com câncer",
                     "Vivo, sem outra especificação",
                     "Óbito por outras causas, sem outra especificação") ~ "0", #censurado
      ULTINFO == "Óbito por câncer" ~ "1"
    ),
    NAOTRAT = as.factor(NAOTRAT),
    DESFECHO = as.numeric(DESFECHO)
  )

# ## encontrando ponto de corte para algumas variaveis
# 
# cutpoint_idade <- surv_cutpoint(
#   data = dados_cancer_filtrado2,
#   time = "TEMPO_OBS_DIAG_TRI",
#   event = "DESFECHO",
#   variables = "IDADE"
# )
# 
# (cutpoint_idade$cutpoint)$cutpoint
# 
# dados_cancer_filtrado2$CUT_IDADE <- surv_categorize(cutpoint_idade)$IDADE

## Salvando como data.table + FST 
dados_cancer_dt <- as.data.table(dados_cancer_filtrado2)
write_fst(dados_cancer_dt, "data/dados_cancer_filtrado.fst")




    




