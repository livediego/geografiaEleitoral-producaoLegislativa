####################################################
# Geografia do Voto - Diego de Oliveira Machado    #
####################################################

#instala e carrega bibliotecas
install.packages('rlang') #fun��es b�sicas requeridas por outros pacotes
install.packages('scales') #para utilizar formata��o n�merica. Ex: percent()
install.packages('magrittr') #ferramenta de simplifica��o de c�digo
install.packages('crayon') #impress�o de gr�ficos coloridos
install.packages('readxl') #para importa��o de dados do Excel, pela fun��o read_excel(). 
install.packages("writexl")#para exportar dados ao Excel

library(rlang)
library(scales)
library(magrittr)
library(crayon)
library(readxl)
library(writexl)

# Importa os arquivos .csv gerando uma �nica tabela de dados

#'vers�o para v�rios arquivos separados por UF
#arquivos = list.files(pattern="*.csv")
#votacao_geral = do.call(rbind, lapply(arquivos, function(x) read.csv(
#  x,sep=";",
#  stringsAsFactors = FALSE
#)))

#vers�o para arquivo �nico com a vota��o de todo o pa�s
if (!exists('autores')){
votacao_geral= read.csv("votacao_candidato_munzona_2018_BRASIL.csv",
  sep=";", stringsAsFactors = FALSE)
}

#filtra somente os deputados federais(CD_CARGO=6) (para estaduais, CD_CARGO=7)
federais_geral <- votacao_geral[ which(votacao_geral$CD_CARGO==6),]

#importa uma planilha de UFs x regi�es 
regioes <- read_excel("Regioes.xlsx")

#filtra da base total de vota��o os eleitos por QP (CD_SIT_TOT_TURNO=2) e eleitos por m�dia(CD_SIT_TOT_TURNO=3).
eleitos <- federais_geral[ which(is.element(federais_geral$CD_SIT_TOT_TURNO,list(2,3))),]

#totaliza os votos por municipio
eleitos_por_municipio <- setNames(aggregate(eleitos$QT_VOTOS_NOMINAIS,by = list(
eleitos$SG_UF,
eleitos$CD_MUNICIPIO,
eleitos$NM_MUNICIPIO,
eleitos$CD_CARGO,
eleitos$DS_CARGO,
eleitos$SQ_CANDIDATO,
eleitos$NM_CANDIDATO,
eleitos$NM_URNA_CANDIDATO,
eleitos$TP_AGREMIACAO,
eleitos$NR_PARTIDO,
eleitos$SG_PARTIDO,
eleitos$SQ_COLIGACAO,
eleitos$DS_COMPOSICAO_COLIGACAO
),FUN = sum),c(
"SG_UF",             
"CD_MUNICIPIO",
"NM_MUNICIPIO",             
"CD_CARGO",
"DS_CARGO",
"SQ_CANDIDATO",
"NM_CANDIDATO",
"NM_URNA_CANDIDATO",
"TP_AGREMIACAO",           
"NR_PARTIDO",
"SG_PARTIDO",
"SQ_COLIGACAO",
"DS_COMPOSICAO_COLIGACAO",
"QT_VOTOS_MUNICIPIO"))

#filtra da base de eleitos por munic�pio, somente os munic�pios com voto.
eleitos_por_municipio_com_voto <-eleitos_por_municipio[ which(eleitos_por_municipio$QT_VOTOS_MUNICIPIO>0), ]

#totaliza os votos no estado de cada eleito
votos_no_estado <- setNames(aggregate(eleitos_por_municipio_com_voto$QT_VOTOS_MUNICIPIO,by = list(
eleitos_por_municipio_com_voto$SQ_CANDIDATO
),FUN = sum),c(
"SQ_CANDIDATO", 
"QT_VOTOS_TOTAL"))
federais <- merge(eleitos_por_municipio_com_voto,votos_no_estado, by=c("SQ_CANDIDATO"))

#calcula o total de votos nominais v�lidos de cada munic�pio para deputados federais eleitos
municipios <- setNames(aggregate(federais$QT_VOTOS_MUNICIPIO,by = list(
federais$SG_UF,
federais$CD_MUNICIPIO,
federais$NM_MUNICIPIO
),FUN = sum),c(
"SG_UF", 
"CD_MUNICIPIO", 
"NM_MUNICIPIO",
"QT_VOTOS_ELEITOS_MUNICIPIO"))

#calcula o total de votos nominais v�lidos de cada munic�pio para deputado federal e tamb�m o �ndice de aproveitamento de votos
municipios_geral <- setNames(aggregate(federais_geral$QT_VOTOS_NOMINAIS,by = list(
federais_geral$SG_UF,
federais_geral$CD_MUNICIPIO,
federais_geral$NM_MUNICIPIO
),FUN = sum),c(
"SG_UF", 
"CD_MUNICIPIO", 
"NM_MUNICIPIO",
"QT_VOTOS_GERAL_MUNICIPIO"))
municipios_indice <- merge(municipios,municipios_geral, by=c("SG_UF", "CD_MUNICIPIO", "NM_MUNICIPIO"))
municipios_indice$IND_APROVEITAMENTO <- municipios_indice$QT_VOTOS_ELEITOS_MUNICIPIO/municipios_indice$QT_VOTOS_GERAL_MUNICIPIO
municipios_indice <- merge(municipios_indice,regioes, by=c("SG_UF"))
media_aproveitamento <- mean(municipios_indice$IND_APROVEITAMENTO)

#totaliza o �ndice de aproveitamento em cada estado e Regi�o
estados_indice <- setNames(aggregate(municipios_indice$IND_APROVEITAMENTO,by = list(
municipios_indice$Regi�o,
municipios_indice$SG_UF
),FUN = mean),c(
  'Regiao',
  "SG_UF", 
  "IND_APROVEITAMENTO"))
estados_indice$IND_APROVEITAMENTO <- label_percent()(estados_indice$IND_APROVEITAMENTO)

regioes_indice <- setNames(aggregate(municipios_indice$IND_APROVEITAMENTO,by = list(
  municipios_indice$Regi�o
),FUN = mean),c(
  "Regiao", 
  "IND_APROVEITAMENTO"))
regioes_indice$IND_APROVEITAMENTO <- label_percent()(regioes_indice$IND_APROVEITAMENTO)

#exclui o Distrito Federal
federais_semDF <- federais[ which(federais$SG_UF != "DF"),]

#calcula o quadrado do percentual de votos dos deputados federais em cada munic�pio
federais_semDF$PERCENTUAL_VOTOS=(federais_semDF$QT_VOTOS_MUNICIPIO/federais_semDF$QT_VOTOS_TOTAL)^2

#calcula o �ndice de fragmenta��o de cada deputado federal
geografia <- setNames(aggregate(federais_semDF$PERCENTUAL_VOTOS,by = list(
federais_semDF$SG_UF,
federais_semDF$SQ_CANDIDATO,
federais_semDF$NM_URNA_CANDIDATO,
federais_semDF$NR_PARTIDO,
federais_semDF$SG_PARTIDO,
federais_semDF$QT_VOTOS_TOTAL
),FUN = sum),c(
"SG_UF",             
"SQ_CANDIDATO",
"NM_URNA_CANDIDATO",
"NR_PARTIDO",
"SG_PARTIDO",
"QT_VOTOS_TOTAL",
"IND_FRAGMENTACAO"))
geografia$IND_FRAGMENTACAO <- 1/geografia$IND_FRAGMENTACAO
geografia$MEDIA_FRAGMENTACAO <- mean(geografia$IND_FRAGMENTACAO) 
geografia$FRAGMENTACAO <- ifelse (geografia$IND_FRAGMENTACAO > 7,"FRAGMENTADO","CONCENTRADO")

cidade_chave <- setNames(aggregate(federais_semDF$PERCENTUAL_VOTOS,by = list(
  federais_semDF$SQ_CANDIDATO
),FUN = max),c(
  "SQ_CANDIDATO",
  "PERCENTUAL_VOTOS"))
cidade_chave <- merge(cidade_chave,federais_semDF[c("SQ_CANDIDATO","NM_MUNICIPIO", "PERCENTUAL_VOTOS")])
cidade_chave$PERCENTUAL_VOTOS <-label_percent()(sqrt(cidade_chave$PERCENTUAL_VOTOS))
names(cidade_chave)[2:3] <- c("PERCENTUAL_VOTOS_CIDADE_CHAVE", "CIDADE_CHAVE")
geografia <- merge(geografia,cidade_chave)

#calcula o �ndice de domin�ncia de cada deputado federal
federais <- merge(federais,municipios_geral, by=c("SG_UF", "CD_MUNICIPIO", "NM_MUNICIPIO"))
federais$IND_DOMINANCIA <- (federais$QT_VOTOS_MUNICIPIO/federais$QT_VOTOS_GERAL_MUNICIPIO)*(federais$QT_VOTOS_MUNICIPIO/federais$QT_VOTOS_TOTAL)
indice_dominancia <- setNames(aggregate(federais$IND_DOMINANCIA,by = list(federais$SQ_CANDIDATO),FUN = sum),c("SQ_CANDIDATO","IND_DOMINANCIA"))
geografia <- merge(geografia,indice_dominancia, by=c("SQ_CANDIDATO"))
geografia$MEDIA_DOMINANCIA <-mean(geografia$IND_DOMINANCIA)
geografia$DOMINANCIA <- ifelse (geografia$IND_DOMINANCIA > geografia$MEDIA_DOMINANCIA,"DOMINANTE","COMPARTILHADO")
geografia$PADRAO <- paste(geografia$FRAGMENTACAO,geografia$DOMINANCIA,sep="/")
write_xlsx(geografia, "geografia2018.xlsx")

#calcula a frequ�ncia de cada padr�o consolidada por estado
estados_geografia <- setNames(aggregate(geografia$SQ_CANDIDATO,by = list(
  geografia$SG_UF,
  geografia$PADRAO
),FUN = length),c(
  "SG_UF",
  "PADRAO",
  "QTDE"))
estados_total= setNames(aggregate(geografia$SQ_CANDIDATO,by = list(
  geografia$SG_UF
),FUN = length),c(
  "SG_UF", 
  "TOTAL_UF"))
estados_geografia <- merge(estados_geografia,estados_total, by=c("SG_UF"))
estados_geografia$PERCENTUAL <- label_percent()(estados_geografia$QTDE/estados_geografia$TOTAL_UF) 
geografia_estados <- reshape(estados_geografia, v.names = c("QTDE", "PERCENTUAL"), idvar = "SG_UF", timevar="PADRAO",direction="wide")

#calcula a frequ�ncia de cada padr�o consolidada por regi�o do pa�s
geografia <- merge(geografia,regioes, by=c("SG_UF"))
regioes_geografia <- setNames(aggregate(geografia$SQ_CANDIDATO,by = list(
  geografia$Regi�o,
  geografia$PADRAO
),FUN = length),c(
  "Regiao", 
  "PADRAO",
  "QTDE"))
regioes_total= setNames(aggregate(geografia$SQ_CANDIDATO,by = list(
  geografia$Regi�o
),FUN = length),c(
  "Regiao", 
  "TOTAL_REGIAO"))
regioes_geografia <- merge(regioes_geografia,regioes_total, by=c("Regiao"))
regioes_geografia$PERCENTUAL <- label_percent()(regioes_geografia$QTDE/regioes_geografia$TOTAL_REGIAO)
geografia_regioes <- reshape(regioes_geografia, v.names = c("QTDE", "PERCENTUAL"), idvar = "Regiao", timevar="PADRAO",direction="wide")

#calcula a frequ�ncia de cada padr�o consolidada por partido
partidos_geografia <- setNames(aggregate(geografia$SQ_CANDIDATO,by = list(
  geografia$SG_PARTIDO,
  geografia$PADRAO
),FUN = length),c(
  "SG_PARTIDO",
  "PADRAO",
  "QTDE"))
partidos_total= setNames(aggregate(geografia$SQ_CANDIDATO,by = list(
  geografia$SG_PARTIDO
),FUN = length),c(
  "SG_PARTIDO", 
  "TOTAL_PARTIDO"))
partidos_geografia <- merge(partidos_geografia,partidos_total, by=c("SG_PARTIDO"))
partidos_geografia$PERCENTUAL <- label_percent()(partidos_geografia$QTDE/partidos_geografia$TOTAL_PARTIDO) 
geografia_partidos <- reshape(partidos_geografia, v.names = c("QTDE", "PERCENTUAL"), idvar = "SG_PARTIDO", timevar="PADRAO",direction="wide")
