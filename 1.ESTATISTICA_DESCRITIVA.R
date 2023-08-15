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


##  Inicilizacao das bibliotecas 

if (!require("pacman")){
  install.packages("pacman")
  library(pacman)
}else{library(pacman)}

p_load("tidyverse","dtplyr","ggplot2")

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


## Atribuindo  o formato "tibble" a tabela de dados
AA<-as_tibble(DadosCompletos)

## observando os dados que foram importados e encontram-se na tabela
AA

## definindo a varival a ser analisada
## Escolhe-se inialmente as vazões
dadosXX <- AA$Vazoes_compl

## Cbtendo informações sobre valores minimos e máximos,quartis, mediana e média
## armazenado os dados na variável "estatistica"
estatistica<- summary(dadosXX)
estatistica

## Cálculo da media, observe a remocao dos valores NA (ausentes) 

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


## Obtendo e formatando as datas em que ocorreram as observações
datasOBS<-as.POSIXct(AA$Datas_compl,format = "%Y/%m/%d")

## Plotagem dos dados para observar o padrao de ocorrencia
plot(datasOBS, dadosXX,type='line')

## Plotagem  do boxplot dos dados para observar a frequencia de ocorrencia dos eventos

boxplot(AA$Vazoes_compl)



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


## agrupar os dados da tabela 

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



