####################################################
# Produção Legislativa - Diego de Oliveira Machado #
####################################################

#instala e carrega bibliotecas

install.packages('scales') #para utilizar formatação numérica. Ex: percent()
install.packages('readxl') #para importação de dados do Excel, pela função read_excel(). 
install.packages("writexl") #para exportação para o Excel
install.packages('reshape2') #biblioteca para agrupamento de dados
library(scales)
library(readxl)
library(writexl)
library(reshape2)

#IMPORTAÇÃO DE ARQUIVOS EXTERNOS

#importa os dados abertos os arquivos de autores de 2019 e 2020 (caso não tenham sido importados)
if (!exists('autores')){
autores <- read.csv("https://dadosabertos.camara.leg.br/arquivos/proposicoesAutores/csv/proposicoesAutores-2019.csv", sep=";", encoding = "UTF-8", stringsAsFactors = FALSE)
autores <- rbind(autores,read.csv("https://dadosabertos.camara.leg.br/arquivos/proposicoesAutores/csv/proposicoesAutores-2020.csv", sep=";", encoding = "UTF-8", stringsAsFactors = FALSE) )
}

if (!exists('proposicoes')){
#importa os dados abertos os arquivos de proposições de 2019 e 2020 (caso não tenham sido importados)
proposicoes <- read.csv("https://dadosabertos.camara.leg.br/arquivos/proposicoes/csv/proposicoes-2019.csv", sep=";", encoding = "UTF-8", stringsAsFactors = FALSE)
proposicoes <- rbind(proposicoes,read.csv("https://dadosabertos.camara.leg.br/arquivos/proposicoes/csv/proposicoes-2020.csv", sep=";", encoding = "UTF-8", stringsAsFactors = FALSE) )
}

rm(list=setdiff(ls(), c("autores", "proposicoes")))

#importa o arquivo excel com a geografia do voto
geografia <- read_excel("geografia2018.xlsx")

#importa o arquivo excel com a conversão de nome candidato parlamentar
conversao_nomes <- read_excel("nome_candidato_parlamentar.xlsx")

#importa o arquivo excel com a magnitude, eleitorado e votos válidos de cada distrito 
distrito <- read_excel("magnitude_eleitorado.xlsx")

#importa o arquivo excel com a fidelidade ao governo de cada parlamentar
governismo <- read_excel("governismo.xlsx")

#importa o arquivo excel com o alinhamento à esquerda cada parlamentar
esquerda <- read_excel("esquerda.xlsx")

#JUNÇÃO E FILTRAGEM DAS TABELAS

#filtra somente as autorias de deputado
autores_filtro <- autores[ which(autores$tipoAutor == "Deputado"),]

#filtra somente as proposições do tipo PL,PLP,PEC,PDL,PRC,RIC,INC
proposicoes_filtro <- proposicoes[ which(is.element(proposicoes$siglaTipo,list("PL","PLP","PEC","PDL","PRC","RIC","INC","PFC"))),]

#filtra somente as proposições apresentadas entre fev/2019 e jan/2020.
proposicoes_filtro <- proposicoes_filtro[ which(proposicoes_filtro$dataApresentacao >="2019-02-01" & proposicoes_filtro$dataApresentacao <="2020-01-31"),]

#proposicoes_filtro$funcao <-ifelse(is.element(proposicoes_filtro$siglaTipo,list("RIC","INC","PFC")),"Fisc","Leg")

colnames(proposicoes_filtro)[1] <-"idProp"

#agrega a produção por tipo de proposição
producao_por_tipo_geral <- setNames(aggregate(proposicoes_filtro$idProp, by = list(
  proposicoes_filtro$siglaTipo
),FUN = length),c(
  "siglaTipo",
  "Prod"))

#une as tableas proposições e autores
proposicoes_autorias <- merge(proposicoes_filtro,autores_filtro, by=c(1,2))

#consulta a quantidade de autores de cada proposição
qtde_autores <- data.frame(table(proposicoes_autorias$idProp))

#filtra somente as proposições com autoria individual
autoria_individual <- qtde_autores[ which(qtde_autores$Freq == 1),-c(2)]
proposicoes_individuais_totais <- merge(proposicoes_autorias,autoria_individual, by=c(1)) 

#filtra somente as proposições dos deputados eleitos (titulares)
proposicoes_individuais <- merge(proposicoes_autoria_individual, setNames(conversao_nomes[2], c("idDeputadoAutor")))

#agrega a produção legislativa por parlamentar
producao <- setNames(aggregate(proposicoes_individuais$idProp,by = list(
  proposicoes_individuais$idDeputadoAutor
),FUN = length),c(
  "idDeputado",
  "Prod"))

#agrega a produção por tipo de proposição e dispõe em ordem decrescente
producao_por_tipo <- setNames(aggregate(proposicoes_individuais$idProp, by = list(
  proposicoes_individuais$siglaTipo
),FUN = length),c(
  "Tipo de Proposição",
  "Qtde"))
producao_por_tipo <- producao_por_tipo[order(-producao_por_tipo$Qtde),]

#calcula o total e o percentual de cada tipo
producao_por_tipo$Total <- sum(producao_por_tipo$Qtde)
producao_por_tipo$Percentual = producao_por_tipo$Qtde/producao_por_tipo$Total

# exporta os dados de produção por tipo
write_xlsx(producao_por_tipo, "prodTipo.xlsx")

#filtra somente as proposições de caráter localista e exporta o banco de dados para conferência
proposicoes_localistas <- proposicoes_individuais[ which(grepl(" município de ",tolower(proposicoes_individuais$ementa))
                                                         |grepl("o município ",tolower(proposicoes_individuais$ementa))
                                                         |grepl(" distrito de ",tolower(proposicoes_individuais$ementa))
                                                         |grepl(" cidade de ",tolower(proposicoes_individuais$ementa))
                                                         |grepl(" municípios de ",tolower(proposicoes_individuais$ementa))
                                                         |grepl(" distritos de ",tolower(proposicoes_individuais$ementa))
                                                         |grepl(" cidades de ",tolower(proposicoes_individuais$ementa))),]

write_xlsx(proposicoes_localistas, "proposicoesLocalistas.xlsx")

#agrega a produção localista por parlamentar, dispõe em ordem decrescente e exporta em formato excel
producao_localista <- setNames(aggregate(proposicoes_localistas$idProp,by = list(
  proposicoes_localistas$idDeputadoAutor
),FUN = length),c(
  "idDeputado",
  "ProducaoLocal"))

#une a tabela geografia com as tabelas: distrito, conversao_nomes, produção e produção_localista
geografia <- merge(geografia,distrito, by='SG_UF')
geografia <- merge(geografia,conversao_nomes, by='SQ_CANDIDATO')
geografia_producao <- merge(geografia,producao, by = "idDeputado", all.x=TRUE)
geografia_producao <- merge(geografia_producao,producao_localista, by = "idDeputado", all.x=TRUE)

#atribui 0 para valores não encontrados nas categorias anteriores
geografia_producao[is.na(geografia_producao)] <- 0

#une a tabela geografia_produção com as tabelas: governismo e esquerda
geografia_producao_filtrado <- merge(geografia_producao,governismo, by = "idDeputado", all.x=TRUE)
geografia_producao_filtrado <- merge(geografia_producao_filtrado,esquerda, by = "idDeputado", all.x=TRUE)

#retira os valores não encontrados
geografia_producao_filtrado <- geografia_producao_filtrado[complete.cases(geografia_producao_filtrado),]

#calcula a ideologia e alinhamento ao governo de cada partido, dispõe em ordem decrescente e exporta em formato excel.
ideologia_governo_partido <- setNames(aggregate(list(
  geografia_producao_filtrado$Governismo,
  geografia_producao_filtrado$alinhamento_esquerda), by = list(
    geografia_producao_filtrado$SG_PARTIDO
  ),FUN = mean),c(
    "Partido",
    "Alinhamento_Governo",
    "Alinhamento_Esquerda"))
ideologia_governo_partido$Governo = ifelse(ideologia_governo_partido$Alinhamento_Governo>=0.5,"Sim","Não")
ideologia_governo_partido$Ideologia = ifelse(ideologia_governo_partido$Alinhamento_Esquerda>=0.4,"Esquerda",ifelse(
  ideologia_governo_partido$Alinhamento_Esquerda>=0.2,"Centro","Direita"))
ideologia_governo_partido <- ideologia_governo_partido[order(-ideologia_governo_partido$Alinhamento_Esquerda),]
write_xlsx(ideologia_governo_partido, "ideologiaGovernoPartido.xlsx")

#une a tabela geografia_producao com a tabela ideologia_governo_partido
geografia_producao <- merge(geografia_producao, ideologia_governo_partido, by.x = "SG_PARTIDO", by.y = "Partido") 

#calcula o percentual de votos na eleição de cada parlamentar
geografia_producao$Percentual_Votos <- geografia_producao$QT_VOTOS_TOTAL/geografia_producao$Votos_válidos

#cria uma coluna boolena informando se cada deputado apresentou ou não algum projeto localista
geografia_producao$Tem_Local <- (geografia_producao$ProducaoLocal > 0)
geografia_producao$ProdLocal <- (geografia_producao$ProducaoLocal/geografia_producao$Prod)

#Define as variáveis categóricas
geografia_producao$PADRAO <- factor(geografia_producao$PADRAO)
geografia_producao$SG_PARTIDO <- factor(geografia_producao$SG_PARTIDO)
geografia_producao$Região <- factor(geografia_producao$Região)
geografia_producao$DOMINANCIA <- factor (geografia_producao$DOMINANCIA)
geografia_producao$FRAGMENTACAO <- factor (geografia_producao$FRAGMENTACAO)
geografia_producao$Governo <- factor (geografia_producao$Governo)
geografia_producao$Ideologia <- factor (geografia_producao$Ideologia)

#atribui 0 para valores não encontrados nas categorias anteriores
geografia_producao[is.na(geografia_producao)] <- 0

#elimina os outliers
geografia_producao_completo <- geografia_producao
geografia_producao <- geografia_producao_completo[which(geografia_producao_completo$Prod<80),]

# CÁLCULO DA PRODUÇÃO LEGISLATIVA

#mostra a produção por deputado(com outliers), dispõe em ordem decrescente e exporta em formato excel.
producao_por_deputado_completo <- subset(geografia_producao_completo, select = c("nomeParlamentar","SG_PARTIDO","SG_UF", "Prod"))
producao_por_deputado_completo <- producao_por_deputado_completo[order(-producao_por_deputado_completo$Prod),]
write_xlsx(producao_por_deputado_completo, "prodDeputadoCompleto.xlsx")


#mostra a produção por deputado, dispõe em ordem decrescente e exporta em formato excel.
producao_por_deputado <- subset(geografia_producao, select = c("nomeParlamentar","SG_PARTIDO","SG_UF", "Prod"))
producao_por_deputado <- producao_por_deputado[order(-producao_por_deputado$Prod),]
write_xlsx(producao_por_deputado, "prodDeputado.xlsx")

#calcula a produção por partido, dispõe em ordem decrescente e exporta em formato excel.
producao_por_partido <- setNames(aggregate(geografia_producao$Prod, by = list(
  geografia_producao$SG_PARTIDO
),FUN = sum),c(
  "Partido",
  "Qtde"))

bancada <- setNames(aggregate(geografia_producao$idDeputado, by = list(
  geografia_producao$SG_PARTIDO
),FUN = length),c(
  "Partido",
  "Bancada"))
producao_por_partido <- merge(producao_por_partido, bancada)
producao_por_partido$Média <- producao_por_partido$Qtde/producao_por_partido$Bancada
producao_por_partido <- producao_por_partido[order(-producao_por_partido$Média),]
write_xlsx(producao_por_partido, "prodPartido.xlsx")

#calcula a produção por ideologia do partido, dispõe em ordem decrescente e exporta em formato excel.
producao_por_partido <- merge(producao_por_partido, ideologia_governo_partido, by="Partido")
producao_por_ideologia_partido <- setNames(aggregate(list(
  producao_por_partido$Qtde,
  producao_por_partido$Bancada), by = list(
  producao_por_partido$Ideologia
),FUN = sum),c(
  "Ideologia",
  "Qtde",
  "Bancada"))
producao_por_ideologia_partido$Média <- producao_por_ideologia_partido$Qtde/producao_por_ideologia_partido$Bancada
producao_por_ideologia_partido <- producao_por_ideologia_partido[order(-producao_por_ideologia_partido$Média),]
write_xlsx(producao_por_ideologia_partido, "prodIdeologiaPartido.xlsx")

#calcula a produção por alinhamento do partido ao governo, dispõe em ordem decrescente e exporta em formato excel.
producao_por_governo_partido <- setNames(aggregate(list(
  producao_por_partido$Qtde,
  producao_por_partido$Bancada), by = list(
    producao_por_partido$Governo
  ),FUN = sum),c(
    "Governo",
    "Qtde",
    "Bancada"))
producao_por_governo_partido$Média <- producao_por_governo_partido$Qtde/producao_por_governo_partido$Bancada
producao_por_governo_partido <- producao_por_governo_partido[order(-producao_por_governo_partido$Média),]
write_xlsx(producao_por_governo_partido, "prodGovernoPartido.xlsx")

#calcula a produção por padrão geográfico, dispõe em ordem decrescente e exporta em formato excel.
producao_por_geografia <- setNames(aggregate(geografia_producao$Prod, by = list(
  geografia_producao$PADRAO
),FUN = sum),c(
  "Padrao",
  "Qtde"))
freq_geografia <- setNames(aggregate(geografia_producao$idDeputado, by = list(
  geografia_producao$PADRAO
),FUN = length),c(
  "Padrao",
  "Freq"))
producao_por_geografia <- merge(producao_por_geografia, freq_geografia)
producao_por_geografia$Média <- producao_por_geografia$Qtde/producao_por_geografia$Freq
producao_por_geografia <- producao_por_geografia[order(-producao_por_geografia$Média),]
write_xlsx(producao_por_geografia, "prodGeografia.xlsx")

#calcula a produção por UF, dispõe em ordem decrescente e exporta em formato excel.
producao_por_uf <- setNames(aggregate(geografia_producao$Prod, by = list(
  geografia_producao$Região,
  geografia_producao$SG_UF,
  geografia_producao$Magnitude,
  geografia_producao$Eleitorado
),FUN = sum),c(
  "Região",
  "UF",
  "Magnitude",
  "Eleitorado",
  "Qtde"))

bancada_uf <- setNames(aggregate(geografia_producao$idDeputado, by = list(
  geografia_producao$SG_UF
),FUN = length),c(
  "UF",
  "BancadaUF"))
producao_por_uf <- merge(producao_por_uf, bancada_uf)
producao_por_uf$Média <- producao_por_uf$Qtde/producao_por_uf$BancadaUF
producao_por_uf <- producao_por_uf[order(-producao_por_uf$Média),]
write_xlsx(producao_por_uf, "prodUF.xlsx")

#calcula a produção por Região, dispõe em ordem decrescente e exporta em formato excel.
producao_por_regiao <- setNames(aggregate(list(
  producao_por_uf$Qtde
), by = list(
  producao_por_uf$Região
), FUN = sum),c(
  "Região",
  "Qtde"))

bancada_regiao <- setNames(aggregate(geografia_producao$idDeputado, by = list(
  geografia_producao$Região
),FUN = length),c(
  "Região",
  "BancadaRegiao"))

producao_por_regiao <- merge(producao_por_regiao, bancada_regiao)
producao_por_regiao$Média <- producao_por_regiao$Qtde/producao_por_regiao$BancadaRegiao
producao_por_regiao <- producao_por_regiao[order(-producao_por_regiao$Média),]
write_xlsx(producao_por_regiao, "prodRegiao.xlsx")

# CÁLCULO DA PRODUÇÃO LEGISLATIVA LOCALISTA

#mostra a produção por deputado, dispõe em ordem decrescente e exporta em formato excel.
producaoLocal_por_deputado <- subset(geografia_producao, select = c("nomeParlamentar","SG_PARTIDO","SG_UF", "ProdLocal"))
producaoLocal_por_deputado <- producaoLocal_por_deputado[order(-producaoLocal_por_deputado$ProdLocal),]
write_xlsx(producaoLocal_por_deputado, "prodLocalDeputado.xlsx")

#calcula a produção por partido, dispõe em ordem decrescente e exporta em formato excel.
producaoLocal_por_partido <- setNames(aggregate(geografia_producao$ProdLocal, by = list(
  geografia_producao$SG_PARTIDO
),FUN = sum),c(
  "Partido",
  "Qtde"))

producaoLocal_por_partido <- merge(producaoLocal_por_partido, bancada)
producaoLocal_por_partido$Média <- producaoLocal_por_partido$Qtde/producaoLocal_por_partido$Bancada
producaoLocal_por_partido <- producaoLocal_por_partido[order(-producaoLocal_por_partido$Média),]
write_xlsx(producaoLocal_por_partido, "prodLocalPartido.xlsx")

#calcula a produção por ideologia do partido, dispõe em ordem decrescente e exporta em formato excel.
producaoLocal_por_partido <- merge(producaoLocal_por_partido, ideologia_governo_partido, by="Partido")
producaoLocal_por_ideologia_partido <- setNames(aggregate(list(
  producaoLocal_por_partido$Qtde,
  producaoLocal_por_partido$Bancada), by = list(
    producaoLocal_por_partido$Ideologia
  ),FUN = sum),c(
    "Ideologia",
    "Qtde",
    "Bancada"))
producaoLocal_por_ideologia_partido$Média <- producaoLocal_por_ideologia_partido$Qtde/producaoLocal_por_ideologia_partido$Bancada
producaoLocal_por_ideologia_partido <- producaoLocal_por_ideologia_partido[order(-producaoLocal_por_ideologia_partido$Média),]
write_xlsx(producaoLocal_por_ideologia_partido, "prodLocalIdeologiaPartido.xlsx")

#calcula a produção por alinhamento do partido ao governo, dispõe em ordem decrescente e exporta em formato excel.
producaoLocal_por_governo_partido <- setNames(aggregate(list(
  producaoLocal_por_partido$Qtde,
  producaoLocal_por_partido$Bancada), by = list(
    producaoLocal_por_partido$Governo
  ),FUN = sum),c(
    "Governo",
    "Qtde",
    "Bancada"))
producaoLocal_por_governo_partido$Média <- producaoLocal_por_governo_partido$Qtde/producaoLocal_por_governo_partido$Bancada
producaoLocal_por_governo_partido <- producaoLocal_por_governo_partido[order(-producaoLocal_por_governo_partido$Média),]
write_xlsx(producaoLocal_por_governo_partido, "prodLocalGovernoPartido.xlsx")

#calcula a produção por padrão geográfico, dispõe em ordem decrescente e exporta em formato excel.
producaoLocal_por_geografia <- setNames(aggregate(geografia_producao$ProdLocal, by = list(
  geografia_producao$PADRAO
),FUN = sum),c(
  "Padrao",
  "Qtde"))
freq_geografia <- setNames(aggregate(geografia_producao$`nomeParlamentar`, by = list(
  geografia_producao$PADRAO
),FUN = length),c(
  "Padrao",
  "Freq"))
producaoLocal_por_geografia <- merge(producaoLocal_por_geografia, freq_geografia)
producaoLocal_por_geografia$Média <- producaoLocal_por_geografia$Qtde/producaoLocal_por_geografia$Freq
producaoLocal_por_geografia <- producaoLocal_por_geografia[order(-producaoLocal_por_geografia$Média),]
write_xlsx(producaoLocal_por_geografia, "prodLocalGeografia.xlsx")

#calcula a produção por UF, dispõe em ordem decrescente e exporta em formato excel.
producaoLocal_por_uf <- setNames(aggregate(geografia_producao$ProdLocal, by = list(
  geografia_producao$Região,
  geografia_producao$SG_UF,
  geografia_producao$Magnitude,
  geografia_producao$Eleitorado,
  geografia_producao$Votos_válidos
),FUN = sum),c(
  "Região",
  "UF",
  "Magnitude",
  "Eleitorado",
  "Votos válidos",
  "Qtde"))

producaoLocal_por_uf$Média <- producaoLocal_por_uf$Qtde/producaoLocal_por_uf$Magnitude
producaoLocal_por_uf <- producaoLocal_por_uf[order(-producaoLocal_por_uf$Média),]
write_xlsx(producaoLocal_por_uf, "prodLocalUF.xlsx")

#calcula a produção por Região, dispõe em ordem decrescente e exporta em formato excel.
producaoLocal_por_regiao <- setNames(aggregate(list(
  producaoLocal_por_uf$Magnitude,
  producaoLocal_por_uf$Eleitorado,
  producaoLocal_por_uf$`Votos válidos`,
  producaoLocal_por_uf$Qtde
), by = list(
  producaoLocal_por_uf$Região
), FUN = mean),c(
  "Região",
  "Magnitude",
  "Eleitorado",
  "Votos válidos",
  "Qtde"))

producaoLocal_por_regiao$Média <- producaoLocal_por_regiao$Qtde/producaoLocal_por_regiao$Magnitude
producaoLocal_por_regiao <- producaoLocal_por_regiao[order(-producaoLocal_por_regiao$Média),]
write_xlsx(producaoLocal_por_regiao, "prodLocalRegiao.xlsx")
