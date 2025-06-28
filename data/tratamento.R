
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
    
    DTULTINFO, #Data da ultima informacao do paciente 
    
    #Sobre desfecho
    ULTINFO, #Ultima informacao do paciente 
    ) |>
  mutate(
    TOPOGRUP_GRUPO = case_when(
      TOPOGRUP == "C50" ~ "C50 Mama",
      TOPOGRUP %in% c("C51", "C52", "C53", "C54", "C55", "C56", "C57", "C58") ~ "C51-C58 Órgãos genitais femininos",
      TOPOGRUP %in% c("C60", "C61", "C62", "C63") ~ "C60-C63 Órgãos genitais masculinos",
      TOPOGRUP %in% c("C64", "C65", "C66", "C67", "C68") ~ "C64-C68 Trato urinário",
      TOPOGRUP %in% c("C69", "C70", "C71", "C72") ~ "C69-C72 Olho, cérebro e outras partes do SNC",
      TOPOGRUP %in% c("C73", "C74", "C75") ~ "C73-C75 Tiróide e outras glândulas",
      TOPOGRUP == "C76" ~ "C76 Out. localizações e localizações mal definidas",
      TOPOGRUP == "C77" ~ "C77 Linfonodos",
      TOPOGRUP == "C80" ~ "C80 Localização primária desconhecida",
    ),
      
    SEXO = case_when(
      SEXO == 1 ~ "Masculino",
      SEXO == 2 ~ "Feminino"
    ),
    
    DTULTINFO = dmy(as.character(DTULTINFO)),
    
    #Tempo observado
    TEMPO_OBS_DIAG = as.numeric(DTULTINFO - DTDIAG),
    TEMPO_OBS_CONSULT = as.numeric(DTULTINFO - DTCONSULT),
    
    #Estagio da doenca (precisa fazer um double check nisso aqui)
    GRUPO_EC = case_when(
      EC %in% c("0", "0A", "0IS", "IS") ~ "0",
      EC %in% c("I", "IA", "IA1", "IA2", "IB", "IB1", "IB2", "IC") ~ "I",
      EC %in% c("II", "IIA", "IIA1", "IIA2", "IIB", "IIC") ~ "II",
      EC %in% c("III", "IIIA", "IIIB", "IIIC", "IIIC1", "IIIC2") ~ "III",
      EC %in% c("IV", "IVA", "IVB", "IVC") ~ "IV",
      EC == "X" ~ "X",
      EC == "Y" ~ "Y"
    ),
    DESFECHO = case_when(
      ULTINFO %in% c(1,2,4) ~ "0", #censurado
      ULTINFO == 3 ~ "1"
    )
  )

## Salvando como data.table + FST 
dados_cancer_dt <- as.data.table(dados_cancer_filtrado2)
write_fst(dados_cancer_dt, "data/dados_cancer_filtrado.fst")



    




