##############################################################
### UNIVERSIDADE FEDERAL DO CEARA  ###########################
### CENTRO DE TECNOLOGIA           ###########################
### DEPARTAMENTO DE ENGENHARIA HIDRAULICA E AMBIENTAL ########
### DISCIPLINA DE HIDROLOGIA - GRADUACAO #####################
### PROF. ASSIS - assis@ufc.br   #############################
##############################################################

##############################################################
### PROGRAMA ANALISE  EXPLORATORIA DE DADOS            #######
### E  ESTATISTICA DESCRITIVA                          #######
##############################################################

### estude este código POR BLOCO. Observe que algumas variáveis são reaproveitadas.  

##############
##BLOCO 1: INICIALIZANDO AS BIBLIOTECAS E ABRINDO E SALVANDO ARQUIVOS
#############

##  Inicilizacao das bibliotecas 

if (!require("pacman")){
  install.packages("pacman")
  library(pacman)
}else{library(pacman)}

p_load("tidyverse","dplyr","dtplyr","ggplot2", "lubridat")

## Definicao dos diretorios e arquivo de dados
## utiliza-se o arquivo "DadosCompletos.RDS" que contem serie temporal de precipitacao e vazao 
## na escala de tempo mensal 

###lendo arquivo de dados "FLUPLU.rds"

diretorio<-"D:/DOCUMENTOS/GitHub/R4Hydrology/GRAD/"
arquivoDIR<- paste(diretorio,"FLUPLU.rds", sep="", collapse = NULL) 
DadosCompletos <- readRDS(arquivoDIR) 

#DadosCompletos <- readRDS("D:/DOCUMENTOS/GitHub/R4Hydrology/GRAD/FLUPLU.rds")

###gravando arquivos
ZZ<-  DadosCompletos%>% select(Vazoes_compl,Prec_compl)
write_rds(x = ZZ, file = "D:/DOCUMENTOS/GitHub/R4Hydrology/GRAD/FLUPLU2.rds")




##################################
#### BLOCO 2: EXPLORANDO A SERIE COMO UM TODO
##################################

## Atribuindo  o formato "tibble" a tabela de dados
AA<-as_tibble(DadosCompletos)

## observando os dados que foram importados e encontram-se na tabela
AA

## definindo a varival a ser analisada
## Escolhe-se inialmente as vazões
dadosXX <- AA$Vazoes_compl

## Obtendo e formatando as datas em que ocorreram as observações
datasOBS<-as.POSIXct(AA$Datas_compl,format = "%Y/%m/%d")

## Plotagem dos dados para observar o padrao de ocorrencia
plot(datasOBS, dadosXX,type='line')


## Obtendo Sumario estistico Contendo informações sobre valores minimos e maximos, 
## quartis, mediana e madia
## armazenado os dados na variavel "estatistica"

estatistica<- summary(dadosXX)
estatistica

## Calculo da media, observe a remocao dos valores NA (ausentes) 

MEDIA<-mean(dadosXX, na.rm= TRUE)
MEDIA

## Calculo da mediana, observe a remocao dos valores NA (ausentes) 

MEDIANA<-median(dadosXX, na.rm= TRUE)
MEDIANA

## Cálculo da desvio padrão, observe a remocao dos valores NA (ausentes) 
desvpad<-sd(dadosXX, na.rm= TRUE)
desvpad

## Cálculo do coeficiente de variação

CV<-desvpad/MEDIA
CV




##################################
#### BLOCO 3: EXPLORANDO DADOS NA ESCALA MENSAL
##################################

## analisando os dados na tabela condicional ao mês da informação

## analisar os dados para um dado mês 

## definindo uma variável na tabela de dados que contenham os meses
AA$mes <- format(AA$Datas_compl,format='%m')

## cálculo da média das vazões para todos os meses do ano
media<-aggregate(AA$Vazoes_compl,list(Mes=AA$mes),mean,na.rm=T)
media 

## cálculo do desvio padrão das vazões para todos os meses do ano
dp<-aggregate(AA$Vazoes_compl,list(AA$mes),sd,na.rm=T)
dp

## obtendo a série de vazoes para o mês de janeiro
 Qjan <- AA$Vazoes_compl[(AA$mes>="01") & ( AA$mes <"02") ]
 
## obtendo a série de vazoes para o mês de fevereiro
 Qfev <- AA$Vazoes_compl[(AA$mes>="02") & ( AA$mes <"03") ]
 
## plotando a serie de vazoes para o mes de janeiro e fevereiro
plot(Qjan)
plot(Qfev)


## agrupar os dados da tabela por mes 

by_data<-group_by(AA,mes)

## Definindo nome dos meses
nummes<-c("01","02","03","04","05","06","07","08", "09","10","11", '12')

## Obtendo e analisando dados para um dado mes

## analisando o mês i utilize  nummes[i]
## definindo o mês que se deseja avaliar
i<- 3

## obtendo a serie de valor de janeiro
by_data2<-filter(by_data, mes==nummes[i])

##sumarizando os dados 
summary(by_data2)

## plotando a serie de valores mensais
plot(by_data2$Vazoes_compl)

## plotando o histograma da serie mensal

hist(by_data2$Vazoes_compl)

## plotando o histograma do logaritimo decimal dos valores da serie mensal

hist(log(by_data2$Vazoes_compl))

## plotando a densidade de frequencia  dos valores da serie mensal

by_data2 %>%
    ggplot( aes(x= by_data2$Vazoes_compl)) +
          geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

## obtendo informacao de qual a variável que esta sendo agrupada
group_vars(by_data)

## obtendo informacao de qual linha da tabela esta associada a que mês 
group_rows(by_data)


##################################
#### BLOCO 4: EXPLORANDO A SERIE ANUAL
##################################


## Obtendo a serie anual 

### Foi defnido anteiormente o campo dos meses
## definindo uma variável na tabela de dados que contenham os meses
#AA$mes <- format(AA$Datas_compl,format='%m')
AA$ano <- format(AA$Datas_compl,format='%Y')
AA$mes <- format(AA$Datas_compl,format='%m')

# obtendo valores medios, maximos e minimos  anuais

QANUAL<- AA%>% 
         group_by(ano)%>%
        summarise(vazMED = mean(Vazoes_compl ),vazMIN = min(Vazoes_compl),
             vazMAX = max(Vazoes_compl), na.rm=TRUE)

# plotando valores máximos, médios e mínimos anuais
plot(QANUAL$ano, QANUAL$vazMAX, type='l', col='red')
lines(QANUAL$ano, QANUAL$vazMED, type='l', col='blue')
lines (QANUAL$ano, QANUAL$vazMIN, col='GREEN')


## Plotagem  do boxplot dos dados para observar a frequencia de ocorrencia dos eventos

boxplot(QANUAL$vazMIN, QANUAL$vazMED,QANUAL$vazMAX )

# EstatIstica dos valores mEdios anuais

estatisticaMIN<- summary(QANUAL$vazMIN)
estatisticaMIN

estatistica<- summary(QANUAL$vazMED)
estatistica

estatisticaMAX<- summary(QANUAL$vazMAX)
estatisticaMAX


## Cálculo da media, observe a remocao dos valores NA (ausentes) 

MEDIA<-mean(QANUAL$vazMED, na.rm= TRUE)
MEDIA

## Calculo da mediana, observe a remocao dos valores NA (ausentes) 

MEDIANA<-median(QANUAL$vazMED, na.rm= TRUE)
MEDIANA

## Calculo da desvio padrão, observe a remocao dos valores NA (ausentes) 
desvpad<-sd(QANUAL$vazMED, na.rm= TRUE)
desvpad

## Calculo do coeficiente de variação

CV<-desvpad/MEDIA
CV

