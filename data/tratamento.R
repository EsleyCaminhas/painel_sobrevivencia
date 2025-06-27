
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

## Filtrando cids
dados_cancer_filtrado1 <- dados_cancer |>
  filter(TOPOGRUP %in% grupo_cids)

## Filtrando variaveis 
dados_cancer_filtrado2 <- dados_cancer_filtrado1 |>
  select(
    TOPOGRUP, #Grupo da topografia (Seleção)
    SEXO, #Sexo do paciente (KM)
    FAIXAETAR, #Faixa etaria do paciente (KM)
    EC, #Estadio clinico (KM)
    
    #Candidatas para o uso do maxstat
    CONSDIAG, #Diferença em dias entre as datas de consulta o diagnostico (KM)
    
    #Sobre a sobrevivencia
    DTDIAG, #Data do diagnostico
    DTTRAT, #Data de inicio do tratamento (Só para checar não entra na conta)
    NAOTRAT, #Código da razão para não realização do tratamento (Só para checar não entra na conta)
    DTULTINFO, #Data da ultima informacao do paciente 
    
    #Sobre desfecho
    ULTINFO, #Ultima informacao do paciente 
    ) |>
  mutate(
    DTULTINFO = dmy(as.character(DTULTINFO)),
    #Tempo observado
    TEMPO_OBS = as.numeric(DTULTINFO - DTDIAG),
    #Estagio da doenca (precisa fazer um double check nisso aqui)
    GRUPO_EC = case_when(
      EC %in% c("0", "0A", "0IS", "IS") ~ "0",
      EC %in% c("I", "IA", "IA1", "IA2", "IB", "IB1", "IB2", "IC") ~ "I",
      EC %in% c("II", "IIA", "IIA1", "IIA2", "IIB", "IIC") ~ "II",
      EC %in% c("III", "IIIA", "IIIB", "IIIC", "IIIC1", "IIIC2") ~ "III",
      EC %in% c("IV", "IVA", "IVB", "IVC") ~ "IV",
      EC %in% c("X", "Y") ~ "Não avaliável/Desconhecido"
    ),
    DESFECHO = case_when(
      ULTINFO %in% c(1,2,4) ~ "0", #censurado
      ULTINFO == 3 ~ "1"
    )
  )

## Salvando como data.table + FST 
dados_cancer_dt <- as.data.table(dados_cancer_filtrado2)
write_fst(dados_cancer_dt, "data/dados_cancer_filtrado.fst")


# # No Shiny, carregue assim:
# dados_cancer <- read_fst("data/dados_cancer_filtrado.fst",
#                          as.data.table = TRUE)


