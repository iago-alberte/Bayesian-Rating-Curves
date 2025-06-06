rm(list = ls(all=TRUE))
library("dplyr")
library("rstan")
library("ggplot2")
library(RODBC)
library("bayesplot")
library(lubridate)
library(patchwork)
library("readxl")
library("writexl")
library("openxlsx")

#load("03.10.2024.1ctrl.sucesso.RData")

# caminho<-paste0(getwd(),"/56920000.mdb")
# 
# ch<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:\\Users\\iagoa\\OneDrive\\Pós-Graduações\\Mestrado UFMG\\Pesquisa de Mestrado\\Elaboração de artigos\\1º artigo\\Arquivos e códigos\\UHE Peti.mdb.mdb")
# query1<-paste0("select * from ResumoDescarga")    
# Med.consist<-sqlQuery(ch,query1,stringsAsFactors = FALSE, as.is = TRUE)
# Med.consist<-Med.consist %>% select(Data,Cota,Vazao,NivelConsistencia)
# 
# 
# #Criando uma coluna de data formatada com lubridate
# Med.consist<-Med.consist %>% mutate(data.lubri = lubridate::ymd_hms(Data))
# 
# #Puxando as medições de descarga líquida
# #Med.consist<-read.table("GaugingsComplete.csv",header=TRUE,sep=",",dec=".")
# Med.consist$Cota<-Med.consist$Cota/100
# head(Med.consist)
# 
# #Como o referencial de cotas até 19/09/2001 era 1 metro abaixo da referência atual, vamos adicionar 1 m às cotas das medições feitas até essa data
# for(i in 1:length(Med.consist$Cota)){
#   if((Med.consist$data.lubri[i]<"2001-09-20 00:00:00")==TRUE){
#     Med.consist$Cota[i] = Med.consist$Cota[i] + 1
#   }
# }
# 
# 
# ggplot()+geom_point(data=Med.consist,aes(x=Cota,y=Vazao,color=as.factor(NivelConsistencia)))+theme_minimal()+
#   labs(color="Nível de Consistência",x="Cota (m)",y="Vazão (m³/s")
# 
# #Filtrando apenas as medições consistidas
# Med.consist<-Med.consist[Med.consist$NivelConsistencia==2,]
# Med.consist$Per.Validade<-1
# Med.consist<-Med.consist %>% select(Data,Cota,Vazao,Per.Validade,NivelConsistencia)
# 
# datas.unique<-unique(Med.consist$Data)
# length(datas.unique)
# length(Med.consist$Data)
# 
# #Tratamento das medições que aconteceram no mesmo dia
# h.meds<-NULL
# q.meds<-NULL
# per.val<-NULL
# for(i in 1:length(datas.unique)){
#   h.meds[i]<-mean(Med.consist$Cota[Med.consist$Data==datas.unique[i]])
#   q.meds[i]<-mean(Med.consist$Vazao[Med.consist$Data==datas.unique[i]])
#   per.val[i]<-mean(Med.consist$Per.Validade[Med.consist$Data==datas.unique[i]])
# }
# Med2<-data.frame(datas.unique,h.meds,q.meds,per.val)
# head(Med2)
# colnames(Med2)<-colnames(Med.consist)[1:4]
# 
# #Comparação entre medições originais e medições médias
# plot(Med2$Cota,Med2$Vazao,type="p")
# lines(Med.consist$Cota,Med.consist$Vazao,type="p",col=2, pch=0.5)
# 
# 
# #Renomeando o conjunto de medições médias
# Med.consist<-Med2
# 
# #Inserindo a contagem de dias desde a primeira medição
# data_referencia <- as.Date(min(Med.consist$Data))
# dias_desde_referencia <- as.numeric(as.Date(Med.consist$Data) - data_referencia)
# Med.consist<-Med.consist %>% mutate(dias = as.numeric(as.Date(Med.consist$Data)-data_referencia))
# 
# #Validades.originais$DiasStart<-as.numeric(as.Date(Validades.originais$Start)-data_referencia)
# #Validades.originais$DiasEnd<-as.numeric(as.Date(Validades.originais$End)-data_referencia)
# 
# #Definindo a incerteza de todas as medições
# Med.consist<-Med.consist %>% mutate(mi = Vazao*0.05)
# #Med.consist$Per.Validade<-1
# 

#############################################################################################################
#Código da estação
codigo<-"56640001"

#Lendo o cotagrama
cotagrama<-read.table("C:/Users/c057932/OneDrive - CEMIG/GDHS/BANCO_OFICIAL_DADOS_GDHS/REDE HIDROMETEOROLÓGICA/OPERAÇÃO DA REDE/BACIA DO RIO DOCE/UHE PETI/SÉRIE HISTÓRICA/56640001 Nível Dia.csv",sep=";",dec=",",header = TRUE)
head(cotagrama)
cotagrama$Cotas<-cotagrama$Cotas/100
cotagrama<-cotagrama[cotagrama$NivelConsistencia==2,]
cotagrama$Data.Hora<-lubridate::ymd(cotagrama$Data.Hora)
ggplot(cotagrama,aes(x=Data.Hora,y=Cotas))+geom_line()

#Lendo o bmdl e renomeando as colunas
bmdl<-read_excel("BMDL Bacia Rio Doce.xlsx",sheet = codigo,skip = 3)
bmdl<-bmdl[,-c(ncol(bmdl),ncol(bmdl)-1)]
colnames(bmdl)[2]<-c("Data")
head(bmdl)

#Inserindo uma coluna das cotas de modelagem (em caso de ter havido reinstalação)
data.reinstalacao<-"2001-09-19"
bmdl<-bmdl %>% mutate(Cotas.Modelo = ifelse(Data<=data.reinstalacao,bmdl$`Cota (m)`+1,bmdl$`Cota (m)`))
colnames(bmdl)[ncol(bmdl)]<-c("Cotas Modelo (m)")
cotagrama<-cotagrama %>% mutate(Cotas.Modelo = ifelse(Data.Hora<=data.reinstalacao,Cotas+1,Cotas))
ggplot(cotagrama,aes(x=Data.Hora,y=Cotas.Modelo))+geom_line()

#Salvando o bmdl na primeira aba da memória de cálculo
memoria<-"memoria.xlsx"
lista_abas<-list("BMDL" = bmdl,"Cotagrama" = cotagrama)
write_xlsx(lista_abas,memoria)

#Validades originais
Validades.originais<-read_xlsx("OriginalPeriods.xlsx", sheet = "OriginalPeriods")

#Formatando as medições de descarga líquida para dar entrada nas funções
Med.consist<-bmdl[,c(2,ncol(bmdl),4)]
Med.consist$NivelConsistencia<-2
Med.consist$Per.Validade<-1
colnames(Med.consist)[2:3]<-c("Cota","Vazao")

#Med.consist<-read.table("GaugingsComplete.csv",header=TRUE,sep=",",dec=".")
#Med.consist$Cota<-Med.consist$Cota/100
head(Med.consist)

datas.unique<-unique(Med.consist$Data)
length(datas.unique)
length(Med.consist$Data)

#Tratamento das medições que aconteceram no mesmo dia
h.meds<-NULL
q.meds<-NULL
per.val<-NULL
for(i in 1:length(datas.unique)){
  h.meds[i]<-mean(Med.consist$Cota[Med.consist$Data==datas.unique[i]],na.rm = TRUE)
  q.meds[i]<-mean(Med.consist$Vazao[Med.consist$Data==datas.unique[i]],na.rm = TRUE)
  per.val[i]<-mean(Med.consist$Per.Validade[Med.consist$Data==datas.unique[i]],na.rm = TRUE)
}
Med2<-data.frame(datas.unique,h.meds,q.meds,per.val)
head(Med2)
colnames(Med2)<-colnames(Med.consist)[c(1,2,3,5)]

#Comparação entre medições originais e medições médias
plot(Med2$Cota,Med2$Vazao,type="p")
lines(Med.consist$Cota,Med.consist$Vazao,type="p",col=2, pch=0.5)


#Renomeando o conjunto de medições médias
Med2<-Med2[complete.cases(Med2$Data),]
Med.consist<-Med2

#Inserindo a contagem de dias desde a primeira medição
data_referencia <- as.Date(min(Med.consist$Data))
dias_desde_referencia <- as.numeric(as.Date(Med.consist$Data) - data_referencia)
Med.consist<-Med.consist %>% mutate(dias = as.numeric(as.Date(Med.consist$Data)-data_referencia))

Validades.originais$DiasStart<-as.numeric(as.Date(Validades.originais$Start)-data_referencia)
Validades.originais$DiasEnd<-as.numeric(as.Date(Validades.originais$End)-data_referencia)

#Definindo a incerteza de todas as medições
Med.consist<-Med.consist %>% mutate(mi = Vazao*0.05)

#############################################################################################################


SPD<-function(tau0,tauf,medicoes){
  Med.consist<-medicoes[medicoes$dias>=tau0 & medicoes$dias<=tauf,]
  DataSPD <- list(
    N = length(Med.consist$Cota),
    h = Med.consist$Cota,
    Q = Med.consist$Vazao,
    K = length(unique(Med.consist$Per.Validade)),
    k = Med.consist$Per.Validade,
    mi = Med.consist$mi,
    x_r = numeric(0),
    x_i = integer(0)
  )
  initsSPD <- function(){
    K<-length(unique(Med.consist$Per.Validade))
    
    a1 <- rnorm(1,40,0.1)
    c1 <- 1.5+ rnorm(1, 0, 0.0005)
    
    a2 <- rnorm(1,20,0.1)
    c2 <- 5/3+ rnorm(1, 0, 0.0005)
    
    a3 <- rnorm(1,20,0.1)
    c3 <- 5/3 + rnorm(1, 0, 0.005)
    
    a4 <- rnorm(1,20,0.1)
    c4 <- 1.75 + rnorm(1, 0, 0.005)
    
    gamma1 <- runif(1,0.05,0.299)
    gamma2 <- runif(1,0.05,0.99)
    
    k1 <- as.array(rnorm(K, 0, 0.15))
    
    k2 <- as.array(rnorm(K, 4, 0.05))
    k3 <- as.array(rnorm(K, 6, 0.05))
    # k3<-NULL
    b4<-0 + rnorm(1,0,0.25)
    # for(i in 1:K){
    #   k1[i]<- 0.80 + rnorm(1, 0, 0.05)
    #   k2[i]<- 4 + rnorm(1,0,0.05)
    #   # k3[i]<-8+rnorm(1,0,0.5)
    # }
    
    return(list(a1 = a1,
                c1 = c1,
                
                a2 = a2,
                c2 = c2,
                
                a3 = a3,
                c3 = c3,
                
                a4 = a4,
                c4 = c4,
                
                k1 = k1,
                k2 = k2,
                k3 = k3,
                b4 = b4,
                
                gamma1 = gamma1,
                gamma2 = gamma2))
  }
  
  fitSPD <- stan(control = list(adapt_delta = 0.8),
                 file = "PetiCarrapModErrLinearRev7.2.stan",  # Stan program
                 data = DataSPD,    # named list of data
                 init = initsSPD,
                 chains = 4,             # number of Markov chains
                 warmup = 6000,          # number of warmup iterations per chain
                 iter = 15000,            # total number of iterations per chain
                 cores = 4,              # number of cores (could use one per chain)
                 #refresh = 0,            # no progress shown
  )
  return(fitSPD)
}
Med.consist.SPD.auxiliares<-read_excel("memoria.xlsx",sheet="BMDL SPD",col_names = TRUE)
testespd<-SPD(0,25617,Med.consist.SPD.auxiliares)
names<-names(testespd)[1:15]
mcmc_trace(testespd)
df.testespd<-as.data.frame(testespd)
write.table(df.testespd,file="sim4.csv",sep=";",dec=",",row.names = FALSE)
# # testeresiduos<-residuos(as.data.frame(testespd),0,17915,Med.consist)
# # head(testeresiduos)
# # teste.tabela.baratin<-tabela.cch.baratin(0,10,0.01,as.data.frame(testespd))
# # head(teste.tabela.baratin)
# # 
# # ggplot()+geom_line(data=teste.tabela.baratin,aes(x=cotas,y=vazoes))+labs(x="Cotas",y="Vazões")+
# #   geom_point(data=Med.consist,aes(x=Cota,y=Vazao))

#fitSPD<-SPD(0,17915,Med.consist)
# names<-names(fitSPD)
# print(fitSPD,names)
# mcmc_trace(fitSPD)
# mcmc_hist(fitSPD)
# 
# df.fitSPD<-as.data.frame(fitSPD)
# head(df.fitSPD)


max.post<-function(vetor){
  dens.vetor<-density(vetor)
  max.post.vetor<-dens.vetor$x[dens.vetor$y==max(dens.vetor$y)]
  return(max.post.vetor)
}

residuos<-function(df.SPD,tau0,tauf,medicoes){
  Med.consist<-medicoes[medicoes$dias>=tau0 & medicoes$dias<=tauf,]
  cotas<-Med.consist$Cota
  
  a1<-max.post(df.SPD$a1)
  c1<-df.SPD$c1[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  k1<-df.SPD$k1[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  b1<-df.SPD$b1[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  
  a2<-df.SPD$a2[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  c2<-df.SPD$c2[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  k2<-df.SPD$k2[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  b2<-df.SPD$b2[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  
  a3<-df.SPD$a3[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  c3<-df.SPD$c3[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  k3<-df.SPD$k3[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  b3<-df.SPD$b3[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  
  
    vazoes<-NULL
  for(i in 1:length(cotas)){
    vazoes[i]<-ifelse(cotas[i]<=k2,a1*(cotas[i]-b1)^c1,ifelse(cotas[i]<=k3,a2*(cotas[i]-b2)^c2,a3*(cotas[i]-b3)^c3))
  }
  Q.calc<-vazoes
  R<-NULL
  for(i in 1:length(Med.consist$Data)){
    R[i]<-Med.consist$Vazao[i]-Q.calc[i]
  }
  #Cálculo dos desvios padrões das vazões calculadas a partir de gamma 1 e gamma 2
  dp.rcs<-NULL
  
  gamma1<-df.SPD$gamma1[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  
  gamma2<-df.SPD$gamma2[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  
  for(i in 1:length(Med.consist$Data)){
    dp.rcs[i]<-gamma1+gamma2*Q.calc[i]
  }
  df<-data.frame(R,dp.rcs,Q.calc)
  return(df)
}

residuos.novas.meds<-function(df.SPD,tau0,tauf,medicoes,tab.cch){
  Med.consist<-medicoes[medicoes$dias>=tau0 & medicoes$dias<=tauf,]
  cotas<-Med.consist$Cota
  
  a1<-max.post(df.SPD$a1)
  # c1<-df.SPD$c1[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  # k1<-df.SPD$k1[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  # b1<-df.SPD$b1[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  # 
  # a2<-df.SPD$a2[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  # c2<-df.SPD$c2[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  # k2<-df.SPD$k2[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  # b2<-df.SPD$b2[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  # 
  # a3<-df.SPD$a3[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  # c3<-df.SPD$c3[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  # k3<-df.SPD$k3[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  # b3<-df.SPD$b3[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  
  
  vazoes<-NULL
  for(i in 1:length(cotas)){
    vazoes[i]<-tab.cch$vazoes[abs(tab.cch$cotas-cotas[i])==min(abs(tab.cch$cotas-cotas[i]))]
  }
  Q.calc<-vazoes
  R<-NULL
  for(i in 1:length(Med.consist$Data)){
    R[i]<-Med.consist$Vazao[i]-Q.calc[i]
  }
  #Cálculo dos desvios padrões das vazões calculadas a partir de gamma 1 e gamma 2
  dp.rcs<-NULL
  
  gamma1<-df.SPD$gamma1[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  
  gamma2<-df.SPD$gamma2[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  
  for(i in 1:length(Med.consist$Data)){
    dp.rcs[i]<-gamma1+gamma2*Q.calc[i]
  }
  df<-data.frame(R,dp.rcs,Q.calc)
  return(df)
}


tabela.cch.baratin<-function(cota.min,cota.max,step.cotas,df.SPD){
  cotas<-seq(cota.min,cota.max,step.cotas)
  a1<-max.post(df.SPD$a1)
  c1<-df.SPD$c1[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  k1<-df.SPD$k1[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  b1<-df.SPD$b1[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  
  a2<-df.SPD$a2[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  c2<-df.SPD$c2[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  k2<-df.SPD$k2[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  b2<-df.SPD$b2[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  
  a3<-df.SPD$a3[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  c3<-df.SPD$c3[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  k3<-df.SPD$k3[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  b3<-df.SPD$b3[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  
  vazoes<-NULL
  for(i in 1:length(cotas)){
    vazoes[i]<-ifelse(cotas[i]<=k2,a1*(cotas[i]-b1)^c1,ifelse(cotas[i]<=k3,a2*(cotas[i]-b2)^c2,a2*(cotas[i]-b2)^c2+a3*(cotas[i]-b3)^c3))
  }
  df<-data.frame(cotas,vazoes)
  df<-df[df$cotas>=k1,]
  return(df)
}

tabela.cch.spd<-function(cota.min,cota.max,step.cotas,df.SPD,periodo,K){
  cotas<-seq(cota.min,cota.max-step.cotas,step.cotas)
  a1<-max.post(df.SPD$a1)
  c1<-df.SPD$c1[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  
  colunak1<-periodo+8
  colunab1<-periodo+8+2*K+1
  colunak2<-periodo+8+K
  colunab2<-periodo+8+3*K+1
  colunak3<-paste0("k3[",periodo,"]")
  
  
  k1<-df.fitSPD[[colunak1]][which.min(abs(df.fitSPD$a1 - a1))]
  b1<-df.fitSPD[[colunab1]][which.min(abs(df.fitSPD$a1 - a1))]
  
  a2<-df.SPD$a2[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  c2<-df.SPD$c2[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  
  k2<-df.fitSPD[[colunak2]][which.min(abs(df.fitSPD$a1 - a1))]
  b2<-df.fitSPD[[colunab2]][which.min(abs(df.fitSPD$a1 - a1))]
  
  a3<-df.SPD$a3[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  c3<-df.SPD$c3[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  k3<-df.SPD[[colunak3]][abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  b3<-df.SPD$b3[abs(df.SPD$a1-a1)==min(abs(df.SPD$a1-a1))]
  

  vazoes<-NULL
  if(k1<k2 && k2<k3){
    for(i in 1:(length(cotas))){
      vazoes[i]<-ifelse(cotas[i]<=k2,a1*(cotas[i]-b1)^c1,ifelse(cotas[i]<=k3,a2*(cotas[i]-b2)^c2,a3*(cotas[i]-b3)^c3))
    }    
  }

  df<-data.frame(cotas,vazoes)
  #df<-df[df$cotas>=k1,]
  return(df)
}

#Teste Funçao Resíduos
# df.residuos<-residuos(df.fitSPD,0,24789,Med.consist)
# df.tabelas<- tabela.cch.baratin(0,8,0.01,df.fitSPD)


mcpfit<-function(t0,tf,n.seg,medicoes,param.m,n_chains,n.iter,n.burn){
  medicoes<-medicoes[medicoes$dias>=t0 & medicoes$dias<tf,]
  R<-medicoes$R
  dp.rcs<-medicoes$dp.rcs
  segmentos<-n.seg
  
  Datamcp<-list(
    N = length(medicoes$Cota),
    K = segmentos,
    r = R,
    par_m = param.m,
    t = medicoes$dias,
    dp_meds = medicoes$mi,
    dp_rcs = dp.rcs
  )
  #Cálculo da máxima verossimilhança para definir valores iniciais
  #Para isso, serão amostrados 10000 valores iniciais para cada parâmetro
  #Em seguida, será calculada a verossimilhança para cada valor de parâmetro
  #Serão utilizados os logs para transformar o produtório em um somatório
  
  matriz.mi<-matrix(data=NA,nrow=10000,ncol=segmentos)
  matriz.tau<-matrix(data=NA,nrow=10000,ncol=segmentos-1)
  for(i in 1:10000){
    matriz.mi[i,] <- runif(segmentos, (-10^param.m)/2, (10^param.m)/2)
    if (segmentos > 1) {
      matriz.tau[i,] <- sort(runif(segmentos-1, t0, max(medicoes$dias)))
    }
  }
  
  
  verossim<-NULL
  t.med<-0
  seg<-0
  r_chap<-NULL
  log.vero<-NULL
  for(i in 1:10000){
    soma<-0
    for(j in 1:length(R)){
      #for(j in 300:328){
      t.med<-medicoes$dias[j]
      seg<-ifelse(segmentos>1,findInterval(t.med,matriz.tau[i,])+1,1)
      r_chap[j]<-matriz.mi[i,seg]
      dp<-sqrt(dp.rcs[j]^2+medicoes$mi[j]^2)
      result<-log(dnorm(R[j],r_chap[j],dp))
      #result
      soma<-soma+ifelse(is.na(result)==FALSE,result,0)
    }
    log.vero[i]<-soma
  }
  valor.max<-max(log.vero,na.rm = TRUE)
  pos.max.veross<-which.max(log.vero)
  mi.max.veross<-matriz.mi[pos.max.veross,]
  tau.max.veross<-matriz.tau[pos.max.veross,]
  
  
  # initf1 <- function(){
  #   mi <- as.array(rep(mi.max.veross + rnorm(1, 0, 0.5), n.seg))
  #   tau <- as.array(rep(mi.max.veross + rnorm(1, 0, 10), n.seg-1))
  #   return(list(mi = mi,
  #               tau = tau))
  # }
  
  # initf2 <- function(chain_id = 1) {
  #   list(mi = mi.max.veross+rnorm(segmentos,0,0.5), tau=tau.max.veross+rnorm(segmentos-1,0,10))
  # }
  # 
  # init_ll <- lapply(1:n_chains, function(id) initf2(chain_id = id))
  
  
  # initf2 <- function(chain_id = 1) {
  #  list(mi = as.vector(mi.max.veross+rnorm(segmentos,0,0.5)), tau=as.vector(tau.max.veross+rnorm(segmentos-1,0,10)))
  # }
  # 
  # init_ll <- lapply(1:n_chains, function(id) initf1(chain_id = id))
  
  initf2 <- function(chain_id = 1) {
    if (segmentos > 1) {
      return(list(
        mi = mi.max.veross + rnorm(segmentos, 0, 0.5),
        tau = as.array(tau.max.veross + rnorm(segmentos - 1, 0, 10)) # Garante que tau tenha tamanho correto
      ))
    } else {
      return(list(
        mi = as.array(mi.max.veross + rnorm(segmentos, 0, 0.5))

      ))
    }
  }
  
  
  fitmcp <- stan(control = list(adapt_delta = 0.9),
                 file = ifelse(n.seg>1,"segmentacao46d.stan","segmentacao46e.stan"),  # Stan program
                 data = Datamcp,    # named list of data
                 init = initf2,
                 chains = n_chains,             # number of Markov chains
                 warmup = n.burn,          # number of warmup iterations per chain
                 iter = n.iter,            # total number of iterations per chain
                 cores = 4,              # number of cores (could use one per chain)
                 thin = 1
                 #refresh = 0,             # no progress shown
  )
  return(fitmcp)
}

# Med.consist.2<-cbind(Med.consist,df.residuos)
# mcp<-mcpfit()
# #Teste função mcpfit
# n.iter<-8000
# n.burn<-4000
# n.chains<-6
# mcp<-mcpfit(0,24789,5,Med.consist.2,2,n.iter = 8000, n.burn = 4000, n_chains = 6)
# dfmcp<-as.data.frame(mcp)
# library(bayesplot)
# mcmc_trace(mcp)
# names<-names(mcp)
# print(mcp,names)


#Cálculo dos DICS
dic.calc<-function(df.fitmcp,n.seg,medicoes,t0,tf){
  df.fitmcp<-df.fitmcp[seq(1,length(df.fitmcp$`mi[1]`),10),]
  Med.consist<-medicoes[medicoes$dias>=t0 & medicoes$dias<tf,]
  segmentos<-n.seg
  vetor.tau <- NULL
  vetor.mi <- NULL
  R<-Med.consist$R
  dp.rcs<-Med.consist$dp.rcs
  # Inicializar outras variáveis
  D_teta <- numeric(length(df.fitmcp[, 1])) # Inicializa D_teta com comprimento 10000
  r_chapeu <- numeric(length(Med.consist$Data)) # Inicializa r_chapeu com comprimento de Med.consist$Data
  for (i in 1:length(df.fitmcp[, 1])) {
    if (segmentos > 1) {
      vetor.tau<- df.fitmcp[i, (segmentos + 1):(2 * segmentos - 1)]
    } else {
      vetor.tau <- max(Med.consist$dias)
    }
    
    soma <- 0
    for (j in 1:length(Med.consist$Data)) {
      t.med <- Med.consist$dias[j]
      seg <- if (segmentos > 1) {
        findInterval(t.med, vetor.tau) + 1
      } else {
        1
      }
      r_chapeu[j] <- df.fitmcp[i, seg]
      dp <- sqrt(dp.rcs[j]^2 + Med.consist$mi[j]^2)
      soma <- soma + log(dnorm(R[j], r_chapeu[j], dp))
    }
    D_teta[i] <- -2 * soma
  }
  
  DIC = mean(D_teta)+0.5*(sd(D_teta)^2)
  return(DIC)
}


Med.consist<-Med.consist[order(Med.consist$Data),]
rownames(Med.consist)<-NULL

#Exclusão de medições dúbias
#library(readxl)
#Med.consist<-read_excel("medicoesok.xlsx")
# Med.consist<-Med.consist[-c(30,131,138),]

head(Med.consist)


lista.base<-c(0,24904)
lista.flutuante<-lista.base
df.referencia<-NULL
list.dfs<-NULL

#lista.base<-list.taus.backup1.1
#lista.flutuante<-lista.base
tolerancia.residuo<-0.015
contagem.iteracoes<-0
teste.parada<-TRUE
n.max.iteracoes<-8
n.iter<-8000
n.burn<-4000
n.chains<-6
param.m<-1.3
h.min<--1
h.max<-12
r.hat.max<-1.6
n.min.med.iteracao<-8
list.df.referencia<-list()
while(teste.parada==TRUE){
  contagem.iteracoes<-contagem.iteracoes+1
  lista.base<-lista.flutuante
  list.dics<-list()
  list.dics.auxiliar<-list()
  list.min.dics<-list()
  list.dics.relat<-list()
  df.Baratin<-NULL
  df.3seg<-NULL
  df.2seg<-NULL
  df.1seg<-NULL
  for (i in 1:(length(lista.base)-1)){
    t0<-lista.base[i]
    tf<-lista.base[i+1]
    med.filtro<-Med.consist[Med.consist$dias>=t0 & Med.consist$dias<tf,]
    list.dics<-list()
    list.dics.auxiliar<-list()
    list.dics.relat<-list()
    #list.min.dics<-list()
    df.Baratin<-NULL
    df.3seg<-NULL
    df.2seg<-NULL
    df.1seg<-NULL
    if(length(med.filtro$Data)>n.min.med.iteracao){
      #Necessário recalcular Baratin para o Med.consist compreendido entre os novos t0 e tf
      df.Baratin<-SPD(t0,tf,med.filtro)
      capture.output(print(df.Baratin),file=paste0(getwd(),"/Resumos.Iterações/Baratin parte",i,"-iter.",contagem.iteracoes,".csv"))
      p.baratin.cadeias<-mcmc_trace(df.Baratin)+theme_minimal()
      p.baratin.posts<-mcmc_hist(df.Baratin)+theme_minimal()
      ggsave(file=paste0(getwd(),"/Grafs.Iterações/ChainsBaratin parte",i,"-iter.",contagem.iteracoes,".png"),p.baratin.cadeias,width = 10,height = 6,dpi=300, bg="white")
      ggsave(file=paste0(getwd(),"/Grafs.Iterações/PostsBaratin parte",i,"-iter.",contagem.iteracoes,".png"),p.baratin.posts,width = 15,height = 10,dpi=300, bg="white")
      tab.baratin<-tabela.cch.baratin(cota.min=h.min,cota.max = h.max,step.cotas = 0.01,df.SPD = as.data.frame(df.Baratin))
      rc.and.meds<-ggplot()+geom_line(data=tab.baratin[tab.baratin$cotas<=6,],aes(x=cotas,y=vazoes,color="Curva Chave"))+
        geom_point(data=med.filtro,aes(x=Cota,y=Vazao,color="Medições"))+
        scale_color_manual(values=c("Curva Chave" = "blue","Medições" = "red"))+
        labs(x = "Cotas (m)", y="Vazões (m³/s)",color="Legenda")
      ggsave(file=paste0(getwd(),"/Grafs.Iterações/CurvaGeral parte",i,"-iter.",contagem.iteracoes,".png"),rc.and.meds,width = 15,height = 10,dpi=300, bg="white")
      #Calcular também os novos resíduos
      residuos.filtro<-residuos(as.data.frame(df.Baratin),t0,tf,med.filtro)
      
      
      med.filtro$R<-residuos.filtro$R
      med.filtro$dp.rcs<-residuos.filtro$dp.rcs
      med.filtro$Rmin<-med.filtro$R-2*med.filtro$dp.rcs
      med.filtro$Rmax<-med.filtro$R+2*med.filtro$dp.rcs
      p<-ggplot(data=med.filtro,aes(x=dias,y=R))+
        geom_point(shape=21,fill=NA)+
        geom_errorbar(aes(ymin = Rmin,ymax=Rmax),width=10)+
        labs(y="Resíduos")+
        theme_minimal()
      #df.5seg<-mcpfit(t0,tf,5,med.filtro,1.0,6)
      #df.4seg<-mcpfit(t0,tf,4,med.filtro,1.0,6)
      df.3seg<-mcpfit(t0,tf,3,med.filtro,param.m,n.chains,n.iter,n.burn)
      if(length(as.data.frame(df.3seg)[,1])<=(2*n.iter)){
        df.3seg<-mcpfit(t0,tf,3,med.filtro,param.m,10,n.iter,n.burn)
      }
      df.2seg<-mcpfit(t0,tf,2,med.filtro,param.m,n.chains,n.iter,n.burn)
      if(length(as.data.frame(df.2seg)[,1])<=(2*n.iter)){
        df.2seg<-mcpfit(t0,tf,2,med.filtro,param.m,10,n.iter,n.burn)
      }
      df.1seg<-mcpfit(t0,tf,1,med.filtro,param.m,n.chains,n.iter,n.burn)
      if(length(as.data.frame(df.1seg)[,1])<=(2*n.iter)){
        df.1seg<-mcpfit(t0,tf,1,med.filtro,param.m,10,n.iter,n.burn)
      }
      
      #Geração e salvamento de gráficos
      # p5.cadeias<-mcmc_trace(df.5seg)
      # p5.posts<-mcmc_hist(df.5seg)
      # 
      # p4.cadeias<-mcmc_trace(df.4seg)
      # p4.posts<-mcmc_hist(df.4seg)
      if(length(as.data.frame(df.3seg)[,1])>(2*n.chains)){
        p3.cadeias<-mcmc_trace(df.3seg)
        p3.posts<-mcmc_hist(df.3seg)
        nome3.cadeias<-(paste0("Chains.3seg.","parte ",i,"-iter.",contagem.iteracoes,".png"))
        nome3.posts<-(paste0("Posts.3seg.","parte ",i,"-iter.",contagem.iteracoes,".png"))
        caminho3.cadeias<-paste0(getwd(),"/Grafs.Iterações/",nome3.cadeias)
        caminho3.posts<-paste0(getwd(),"/Grafs.Iterações/",nome3.posts)
        ggsave(caminho3.cadeias,plot=p3.cadeias,width=15, height=6,dpi=300, bg="white")
        ggsave(caminho3.posts,plot=p3.posts,width=15, height=6,dpi=300, bg="white")
        caminho3.resumo<-paste0(getwd(),"/Resumos.Iterações/","3seg.","parte ",i,"-iter.",contagem.iteracoes,".csv")
        capture.output(print(df.3seg), file = caminho3.resumo)
        
        if(max(rhat(df.3seg))<=r.hat.max){
          df.3seg<-as.data.frame(df.3seg)
          DIC3<-dic.calc(df.3seg,3,med.filtro,t0,tf)  
        }else{
          DIC3<-10^8
        }
      }else{
        DIC3<-10^9
      }
      
      
      if(length(as.data.frame(df.2seg)[,1])>(2*n.chains)){
        p2.cadeias<-mcmc_trace(df.2seg)
        p2.posts<-mcmc_hist(df.2seg)
        nome2.cadeias<-(paste0("Chains.2seg.","parte ",i,"-iter.",contagem.iteracoes,".png"))
        nome2.posts<-(paste0("Posts.2seg.","parte ",i,"-iter.",contagem.iteracoes,".png"))
        caminho2.cadeias<-paste0(getwd(),"/Grafs.Iterações/",nome2.cadeias)
        caminho2.posts<-paste0(getwd(),"/Grafs.Iterações/",nome2.posts)
        ggsave(caminho2.cadeias,plot=p2.cadeias,width=15, height=6,dpi=300, bg="white")
        ggsave(caminho2.posts,plot=p2.posts,width=15, height=6,dpi=300, bg="white")
        caminho2.resumo<-paste0(getwd(),"/Resumos.Iterações/","2seg.","parte ",i,"-iter.",contagem.iteracoes,".csv")
        capture.output(print(df.2seg), file = caminho2.resumo)
        
        if(max(rhat(df.2seg))<=r.hat.max){
          df.2seg<-as.data.frame(df.2seg)
          DIC2<-dic.calc(df.2seg,2,med.filtro,t0,tf)  
        }else{
          DIC2<-10^8
        }
        
      }else{
        DIC2<-10^9
      }
      
      
      p1.cadeias<-mcmc_trace(df.1seg)
      p1.posts<-mcmc_hist(df.1seg)
      
      
      
      nome1.cadeias<-(paste0("Chains.1seg.","parte ",i,"-iter.",contagem.iteracoes,".png"))
      nome1.posts<-(paste0("Posts.1seg.","parte ",i,"-iter.",contagem.iteracoes,".png"))
      
      caminho1.cadeias<-paste0(getwd(),"/Grafs.Iterações/",nome1.cadeias)
      caminho1.posts<-paste0(getwd(),"/Grafs.Iterações/",nome1.posts)
      
      ggsave(caminho1.cadeias,plot=p1.cadeias,width=15, height=6,dpi=300, bg="white")
      ggsave(caminho1.posts,plot=p1.posts,width=15, height=6,dpi=300, bg="white")
      
      
      #Geração e salvamento de resumos
      
      
      
      caminho1.resumo<-paste0(getwd(),"/Resumos.Iterações/","1seg.","parte ",i,"-iter.",contagem.iteracoes,".csv")
      
      capture.output(print(df.1seg), file = caminho1.resumo)
      
      #Transformação dos codas em data frames
      
      
      df.1seg<-as.data.frame(df.1seg)
      list.dfs<-list(df.1seg,df.2seg,df.3seg)
      
      
      
      # DIC5<-dic.calc(df.5seg,5,med.filtro,t0,tf)
      # DIC4<-dic.calc(df.4seg,4,med.filtro,t0,tf)
      
      
      DIC1<-dic.calc(df.1seg,1,med.filtro,t0,tf)
      
      #list.dics<-c(DIC1,DIC2,DIC3,DIC4,DIC5)
      list.dics<-c(DIC1,DIC2,DIC3)
      list.dics.relat<-list.dics/min(list.dics)
      list.dics.auxiliar<-list.dics
      for(div in 3:2){
        dif<-abs(list.dics.relat[div]-list.dics.relat[div-1])
        list.dics.auxiliar[div]<-ifelse(dif<=tolerancia.residuo,10^9,list.dics.auxiliar[div])
      }
      list.min.dics<-c(list.min.dics,which.min(list.dics.auxiliar))
      capture.output(list.dics,file=paste0(getwd(),"/Resumos.Iterações/","lista dics part. ",i,"- iteração",contagem.iteracoes,".csv"))
      capture.output(list.dics.auxiliar,file=paste0(getwd(),"/Resumos.Iterações/","lista dics considered part. ",i,"- iteração",contagem.iteracoes,".csv"))
      if(which.min(list.dics.auxiliar)>1){
        df.referencia<-list.dfs[[which.min(list.dics.auxiliar)]]
        
        mis.referencia<-df.referencia[1:(which.min(list.dics.auxiliar))]
        taus.referencia<-df.referencia[, (which.min(list.dics.auxiliar) + 1):(2 * which.min(list.dics.auxiliar) - 1)]
        list.df.referencia<-append(list.df.referencia,df.referencia)#list(list.df.referencia,taus.referencia)
        capture.output(df.referencia,file=paste0(getwd(),"/Resumos.Iterações/","post taus part. ",i,"- iteração",contagem.iteracoes,".csv"))
        capture.output(list.df.referencia,file=paste0(getwd(),"/Resumos.Iterações/","acumum dfs refer. ",i,"- iteração",contagem.iteracoes,".csv"))
        #função para o caso em que o número de taus é maior que 1
        calculate_density1 <- function(column) {
          dens <- density(column, na.rm = TRUE)
          return(list(x = dens$x, y = dens$y))
        }
        
        #função para o caso em que o número de taus é igual a 1
        calculate_density2 <- function(vetor){
          dens<-density(vetor,na.rm = TRUE)
          return(list(x = dens$x, y = dens$y))
        }
        
        n.taus<-(2 * which.min(list.dics.auxiliar) - 1)-(which.min(list.dics.auxiliar) + 1)+1
        
        if((which.min(list.dics.auxiliar) + 1)==(2 * which.min(list.dics.auxiliar) - 1)){
          densities.taus<-calculate_density2(taus.referencia)
          
        }else{
          densities.taus<-apply(taus.referencia, 2, calculate_density1)
        }
        
        
        taus.ref.max.posts<-NULL
        taus.ref.inf<-NULL
        taus.ref.sup<-NULL
        if(n.taus>1){
          for(j in 1:n.taus){
            taus.ref.max.posts[j]<-densities.taus[[j]]$x[densities.taus[[j]]$y==max(densities.taus[[j]]$y)]
            taus.ref.inf[j]<-as.numeric(quantile(taus.referencia[,j],probs=c(0.025)))
            taus.ref.sup[j]<-as.numeric(quantile(taus.referencia[,j],probs=c(0.975)))
          }      
        }else{
          taus.ref.max.posts<-densities.taus$x[densities.taus$y==max(densities.taus$y)]
          taus.ref.inf<-as.numeric(quantile(taus.referencia,probs=c(0.025)))
          taus.ref.sup<-as.numeric(quantile(taus.referencia,probs=c(0.975)))
        }
        df.taus.ref<-data.frame(taus.ref.max.posts,taus.ref.inf,taus.ref.sup)
        # for(t in 1:length(taus.ref.max.posts)){
        #   p<-p + geom_vline(xintercept = taus.ref.max.posts[t],linetype="dashed",color="blue")+
        #           geom_rect(aes(xmin=taus.ref.inf[t],xmax=taus.ref.sup[t],ymin=-Inf,ymax=Inf),
        #                     fill="blue",alpha=0.01)
        # }
        for(t in 1:length(taus.ref.max.posts)){
          p <- p + 
            geom_vline(xintercept = taus.ref.max.posts[t], linetype = "dashed", color = "blue")
        }
        
        
        
        taus.auxiliar<-c(t0,taus.ref.max.posts,tf)
        mi.sup<-vector("numeric",length=0)
        mi.inf<-vector("numeric",length=0)
        mi.med<-vector("numeric",length=0)
        for(m in 1:length(mis.referencia)){
          mi.sup<-append(mi.sup,as.numeric(quantile(mis.referencia[,m],probs=c(0.975))))
          mi.inf<-append(mi.inf,as.numeric(quantile(mis.referencia[,m],probs=c(0.025))))
          mi.med<-append(mi.med,as.numeric(quantile(mis.referencia[,m],probs=c(0.500))))
        }
        
        #Adicionando ao data frame das meds filtradas as colunas de período de validade médio,
        #resíduo médio (mi.med), resíduo sup (mi.sup) e resíduo inf (mi.inf)
        med.filtro<-med.filtro %>% mutate(per.med=findInterval(dias,taus.auxiliar))
        med.filtro<-med.filtro %>% mutate(res.med=mi.med[per.med],
                                          res.sup=mi.sup[per.med],
                                          res.inf=mi.inf[per.med])
        #Adicionando ao gráfico p as ribbons referentes aos IC's da variável mi
        p<-p + geom_ribbon(data=med.filtro,aes(ymin=res.inf,ymax=res.sup,group=per.med),fill="red",alpha=0.3)+
          geom_line(data=med.filtro,aes(y=res.med,group=per.med),color="red",linetype="dashed")

          
        
        lista.flutuante<-sort(c(unlist(lista.flutuante),taus.ref.max.posts))
        #print(p)
        caminho.residuos<-paste0(getwd(),"/Grafs.Iterações/","Residuos","parte ",i,"- iter",contagem.iteracoes,".png")
        ggsave(caminho.residuos,plot=p,width=15, height=6,dpi=300, bg="white")
      }else{
        mi.sup<-vector("numeric",length=0)
        mi.inf<-vector("numeric",length=0)
        mi.med<-vector("numeric",length=0)
        df.referencia<-list.dfs[[which.min(list.dics.auxiliar)]]
        mis.referencia<-df.referencia[1:(which.min(list.dics.auxiliar))]
        for(m in 1:length(mis.referencia)){
          mi.sup<-append(mi.sup,as.numeric(quantile(df.1seg$`mi[1]`,probs=c(0.975))))
          mi.inf<-append(mi.inf,as.numeric(quantile(df.1seg$`mi[1]`,probs=c(0.025))))
          mi.med<-append(mi.med,as.numeric(quantile(df.1seg$`mi[1]`,probs=c(0.5))))
        }
        med.filtro$per.med<-1
        med.filtro<-med.filtro %>% mutate(res.med=mi.med[per.med],
                                          res.sup=mi.sup[per.med],
                                          res.inf=mi.inf[per.med])
        p<-p + geom_ribbon(data=med.filtro,aes(ymin=res.inf,ymax=res.sup,group=per.med),fill="red",alpha=0.3)+
          geom_segment(x = t0, 
                            xend = tf, 
                            y = max.post(df.1seg[,1]), 
                            yend = max.post(df.1seg[,1]),
                            color = "red", linetype = "dashed")
            
        caminho.residuos<-paste0(getwd(),"/Grafs.Iterações/","Res. sem quebra ","parte ",i,"- iter",contagem.iteracoes,".png")
        ggsave(caminho.residuos,plot=p,width=15, height=6,dpi=300, bg="white")
      }
    }
  }
  capture.output(list.min.dics,file=paste0(getwd(),"/Resumos.Iterações/","lista min dics","- iteração",contagem.iteracoes,".csv"))
  #teste.parada<-any(list.min.dics !=1)
  teste.parada<-contagem.iteracoes<n.max.iteracoes
}
lista.flutuante.backup<-lista.flutuante
lista.flutuante



#Ajuste das validades manualmente
datas.quebra.manuais<-c("1955-01-25","1960-01-24","1962-12-04","1974-12-29","1977-01-29",
                        "1979-02-01","1986-11-14","1992-01-24","1995-12-26","2000-01-29",
                        "2002-01-19","2003-12-07","2004-12-24","2011-11-28","2020-01-25",
                        "2022-05-21")

datas.quebra.manuais<-lubridate::ymd(datas.quebra.manuais)
lista.flutuante<-c(0,datas.quebra.manuais-data_referencia)

# lista.flutuante.backup2<-lista.flutuante
# lista.flutuante.backup3<-lista.flutuante
# lista.flutuante.final<-append(lista.flutuante.backup,lista.flutuante)
# lista.flutuante.final<-lista.flutuante.final[-c(17)]
# lista.flutuante.final<-sort(lista.flutuante.final)
# lista.flutuante.final<-lista.flutuante.final[-c(9)]
#lista.flutuante<-lista.flutuante.backup

#Pegando apenas as realizações de tau
# Filtra apenas os elementos que contêm 'tau' no nome
list.tau <- list.df.referencia[grep("tau", names(list.df.referencia))]

# Redefine os nomes dos elementos da lista para serem sequenciais de 1 a n
names(list.tau) <- paste0("tau", seq_along(list.tau))

# Exibe a estrutura da nova lista para verificar
str(list.tau)
tau.vertical<-NULL
for(i in 1:length(list.tau)){
  tau.vertical<-c(tau.vertical,list.tau[[i]])  
}
head(tau.vertical)

#cotagrama<-read.table(file="cotagrama.csv",sep=";",dec=",",header=TRUE)
head(cotagrama)
cotagrama$data.hora<-lubridate::dmy(cotagrama$data.hora)

densidades.tau<-density(tau.vertical)
#histograma.tau<-hist(tau.vertical)
datas.tau<-data_referencia+days(as.integer(densidades.tau$x))

par(mfrow=c(2,1))
ggplot(cotagrama,aes(x=Data.Hora,y=Cotas.Modelo))+geom_line()
plot(cotagrama$Data.Hora,cotagrama$Cotas.Modelo,type="l",xlab="Data",ylab="Cota(m)")
plot(datas.tau,densidades.tau$y,col=2,xlab="Data",ylab="Probs", xlim=c(min(cotagrama$Data.Hora,na.rm = TRUE),max(cotagrama$Data.Hora,na.rm = TRUE)),type="l")

#Adicionando uma aba com as densidades tau ao excel da memória de cálculo
# Carregar o arquivo existente
wb <- loadWorkbook(memoria)

# Adicionar uma nova aba com as densidades de probabilidade da variável tau
addWorksheet(wb, "Probabilidades de quebra")
df.densidades.tau<-data.frame(datas.tau,densidades.tau$y)
head(df.densidades.tau)
colnames(df.densidades.tau)<-c("Datas","Probabilidades")

writeData(wb, "Probabilidades de quebra", df.densidades.tau)

# Salvar o arquivo sem alterar as abas anteriores
saveWorkbook(wb, memoria, overwrite = TRUE)


#Plotagem do histograma (se desejado)
#plot(data_referencia+days(as.integer(histograma.tau$mids)),histograma.tau$density,col=2,
     #xlab="Data",ylab="Probs", xlim=c(min(cotagrama$data.hora),max(cotagrama$data.hora)),type="h")

#vamos configurar o último tau da lista flutuante para corresponder a 01/01/2099
#lista.flutuante[length(lista.flutuante)]<-46105
#lista.flutuante<-lista.flutuante[-c(3,4)]

#Data Frame de Validades Manuais
Inicio<-NULL
Fim<-data_referencia+days(as.integer(lista.flutuante[2:length(lista.flutuante)]))
Fim[length(Fim)]<-Fim[length(Fim)]+days(1)
Inicio<-data_referencia+days(as.integer(lista.flutuante[1:(length(lista.flutuante)-1)]))
Inicio[2:(length(lista.flutuante)-1)]<-Inicio[2:(length(lista.flutuante)-1)]+days(1)

Validades<-data.frame(Inicio,Fim)
colnames(Validades)[1]<-c("Início")


#Data Frame de Validades Automáticas
Inicio<-NULL
Fim<-data_referencia+days(as.integer(lista.flutuante.backup[2:length(lista.flutuante.backup)]))
Fim[length(Fim)]<-Fim[length(Fim)]+days(1)
Inicio<-data_referencia+days(as.integer(lista.flutuante.backup[1:(length(lista.flutuante.backup)-1)]))
Inicio[2:(length(lista.flutuante.backup)-1)]<-Inicio[2:(length(lista.flutuante.backup)-1)]+days(1)

Validades.Modelo<-data.frame(Inicio,Fim)
colnames(Validades.Modelo)[1]<-c("Início")


#Adicionando uma aba com os pontos de segmentação sugeridos pelo modelo
wb<-loadWorkbook(memoria)

abas_validades<-list("Quebras Modelo" = Validades.Modelo,"Quebras Manuais" = Validades)

for(nome_aba in names(abas_validades)){
  addWorksheet(wb,nome_aba)
  writeData(wb,nome_aba,abas_validades[[nome_aba]])  
}

saveWorkbook(wb, memoria, overwrite = TRUE)


#Ajuste manual dos períodos
#Validades$Fim[1]<-lubridate::ymd("1992-02-09")
#Validades$Fim[2]<-lubridate::ymd("2016-01-22")

# Validades$Início[2]<-lubridate::ymd("1992-02-10")
# Validades$Início[3]<-lubridate::ymd("2016-01-23")






#Data frame de medições para modelagem SPD

##Se desejar utilizar o Med.consist original, execute a linha abaixo:
#Med.consist.SPD<-Med.consist %>% mutate(Per.Validade = findInterval(dias,lista.flutuante,all.inside = TRUE))

##Se desejar ler de alguma aba da memória de cálculo do Excel, execute a linha abaixo:
Med.consist.SPD<-read_excel("memoria.xlsx",sheet = "BMDL SPD")
Med.consist.SPD<-Med.consist.SPD[Med.consist.SPD$Ação!="Excluir",]
Med.consist.SPD<-Med.consist.SPD %>% select(Data,Cota,Vazao,Per.Validade,dias,mi)
ggplot()+geom_point(data=Med.consist.SPD,aes(x=Cota,y=Vazao,color=as.factor(Per.Validade)))+
  scale_y_log10()+
  scale_x_log10()+
  labs(x="Cotas (m)",y="Vazões (m³/s)",color="Período de Validade")

##Caso tenha lido as medições do excel, recalcular o data frame de Validades
Validades <- data.frame(Início = as.Date(rep(NA,K)), Fim = as.Date(rep(NA,K)))
Validades$Início[1]<-min(Med.consist.SPD$Data)
for(i in 1:(K-1)){
  Validades$Fim[i]<-max(Med.consist.SPD$Data[Med.consist.SPD$Per.Validade==i])
  Validades$Início[i+1]<-Validades$Fim[i]+days(1)
}
Validades$Fim[K]<-max(Med.consist.SPD$Data)
Validades

##Inserindo a coluna das validades manuais
wb<-loadWorkbook(memoria)

abas_validades<-list("Quebras Manuais" = Validades)

for(nome_aba in names(abas_validades)){
  addWorksheet(wb,nome_aba)
  writeData(wb,nome_aba,abas_validades[[nome_aba]])  
}

saveWorkbook(wb, memoria, overwrite = TRUE)

#Altere os limites de tau0 e tauf conforme necessário
Med.consist.SPD<-read_excel("memoria.xlsx",sheet="BMDL SPD",col_names = TRUE)
#RC.SPD.final<-SPD(0,25618,Med.consist.SPD)
RC.SPD.final<-testespd
mcmc_trace(RC.SPD.final)
mcmc_hist(RC.SPD.final)
#df.fitSPD.backup<-df.fitSPD #Backup para pegar incertezas reais da parte alta
df.fitSPD<-as.data.frame(RC.SPD.final)
#df.fitSPD<-df.fitSPD[c(20000:24000),]
#df.fitSPD<-df.fitSPD[3001:9000,]
par(mfrow=c(3,4))
names<-colnames(df.fitSPD)
for(i in 1:length(df.fitSPD)){
  titulo<-names[[i]]
  hist(df.fitSPD[,i],main=titulo)
  plot(df.fitSPD[,i],type="l",main=titulo)
}
K<-length(unique(Med.consist.SPD$Per.Validade))


cota.min<--1
cota.max<-10
passo<-0.01
dif<-(cota.max - cota.min)*(1/passo)



preenche.matriz<-function(fitSPD,IC,K,cota.min,cota.max,passo,med.sup.ou.inf,n.realizacoes){
  indices<-seq(1,length(fitSPD$a1),round(length(fitSPD$a1)/n.realizacoes,digits = 0))
  fitSPD<-fitSPD[indices,]
  dif<-(cota.max - cota.min)*(1/passo)
  matriz<-array(data = NA, dim = c(dif, length(fitSPD$a1)+1, K))
  for(i in 1:K){
    matriz[,1,i]<- seq(cota.min,cota.max-0.01,passo)
    for(j in 1:length(fitSPD$a1)){ #j varia para cada iteração
      a1<-fitSPD$a1[j]
      c1<-fitSPD$c1[j]
      
      a2<-fitSPD$a2[j]
      c2<-fitSPD$c2[j]
      
      a3<-fitSPD$a3[j]
      c3<-fitSPD$c3[j]
      
      a4<-fitSPD$a4[j]
      c4<-fitSPD$c4[j]
      
      colunak1<-paste0("k1[",i,"]")
      colunak2<-paste0("k2[",i,"]")
      colunak3<-paste0("k3[",i,"]")
      colunak4<-paste0("k4[",i,"]")
      colunab1<-paste0("b1[",i,"]")
      colunab2<-paste0("b2[",i,"]")
      colunab3<-paste0("b3[",i,"]")
      
      
      
      k1<-fitSPD[[colunak1]][j]
      k2<-fitSPD[[colunak2]][j]
      k3<-fitSPD[[colunak3]][j]
      k4<-fitSPD[[colunak4]][j]
      
      b1<-fitSPD[[colunab1]][j]
      b2<-fitSPD[[colunab2]][j]
      b3<-fitSPD[[colunab3]][j]
      b4<-fitSPD$b4[j]
      
      gamma1<-fitSPD$gamma1[j]
      gamma2<-fitSPD$gamma2[j]
      
      
      Q<-NULL
      dp<-NULL
      #cálculo de quantos desvios padrão são necessários para gerar o IC desejado
      n.dp<-abs(qnorm((1-IC)/2))
      if(k1<k2 && k2<k3){
        for(n in 1:dif){#n varia para cada valor de cota
          h<-matriz[n,1,i]
          if(h>=k1 & h<=k2){
            Q[n]<-a1*(h-b1)^c1
          }
          else if(h>=k2 & h<k3){
            Q[n]<-a2*(h-b2)^c2
          }
          else if(h>=k3 & h<k4){
            Q[n]<-a3*(h-b3)^c3
          }
          else if(h>=k4){
            Q[n]<-a4*(h-b4)^c4
          }
          else{
            Q[n]<-0
          }
          #O erro estrutural é uma normal centrada em 0 e com desvio padrão dado por raiz(gamma1 + gamma2*Q)
          #Vamos simular váris realizações dessa normal e calcular seu dp
          dp[n]<-ifelse(Q[n]>0,gamma1 + gamma2*Q[n],0)
        }
        #Se eu estiver calculando a matriz das médias, ela será o próprio valor de Q;
        #Se for a matriz do limite superior do IC, vamos acrescer a Q o valor de n.dp*dp;
        #Se for a matriz do limite inferior do IC, vamos retirar de Q o valor de n.dp*dp.
        if (med.sup.ou.inf == "med") {
          resultado <- Q
        } else if (med.sup.ou.inf == "sup") {
          resultado <- Q + n.dp * dp
        } else {
          resultado <- Q - n.dp * dp
        }
      }else{
        resultado<-rep(NA,dif)
      }

      #matriz[,1+j,i]<-ifelse(med.sup.ou.inf=="med",Q,ifelse(med.sup.ou.inf=="sup",Q+n.dp*dp,Q-n.dp*dp))
      matriz[,1+j,i]<-resultado
    }
  }
  return(matriz)
}
n.realizacoes<-50
matriz.q.sim<-array(data = NA, dim = c(dif, n.realizacoes+1, K))

matriz.q.sim.sup<-array(data = NA, dim = c(dif, n.realizacoes+1, K))
matriz.q.sim.inf<-array(data = NA, dim = c(dif, n.realizacoes+1, K))

matriz.q.sim<-preenche.matriz(fitSPD = df.fitSPD,IC=0.95,K=K,cota.min = -1,cota.max = 10,passo = 0.01,med.sup.ou.inf = "med",n.realizacoes = n.realizacoes)
matriz.q.sim.sup<-preenche.matriz(fitSPD = df.fitSPD,IC=0.95,K=K,cota.min = -1,cota.max = 10,passo = 0.01,med.sup.ou.inf = "sup",n.realizacoes = n.realizacoes)
matriz.q.sim.inf<-preenche.matriz(fitSPD = df.fitSPD,IC=0.95,K=K,cota.min = -1,cota.max = 10,passo = 0.01,med.sup.ou.inf = "inf",n.realizacoes = n.realizacoes)


#Agora que já temos a matriz tridimensional, vamos plotar as vazões dos quantis 2.5%, 50% e 97.5% para cada cota e cada período de validade
Q.calc.sup <- matrix(NA, nrow = dif, ncol = K)
Q.calc.inf <- matrix(NA, nrow = dif, ncol = K)
Q.calc.med <- matrix(NA, nrow = dif, ncol = K)

Q.calc.sup.param <- matrix(NA, nrow = dif, ncol = K)
Q.calc.inf.param <- matrix(NA, nrow = dif, ncol = K)
Q.calc.med.param <- matrix(NA, nrow = dif, ncol = K)

par(mfrow=c(3,3))
for(i in 1:K){ #iteração entre os períodos de validade
  for(j in 1:dif){#iteração entre cada vazão simulada
    Q.calc.sup[j,i]<-quantile(c(matriz.q.sim[j,,i],matriz.q.sim.inf[j,,i],matriz.q.sim.sup[j,,i]), probs = c(0.975), na.rm = TRUE)
    Q.calc.inf[j,i]<-quantile(c(matriz.q.sim[j,,i],matriz.q.sim.inf[j,,i],matriz.q.sim.sup[j,,i]), probs = c(0.025), na.rm = TRUE)
    #Q.calc.med[j,i]<-quantile(c(matriz.q.sim[j,,i],matriz.q.sim.inf[j,,i],matriz.q.sim.sup[j,,i]), probs = c(0.50), na.rm = TRUE)
    Q.calc.sup.param[j,i]<-quantile(matriz.q.sim[j,,i], probs = c(0.975), na.rm = TRUE)
    Q.calc.inf.param[j,i]<-quantile(matriz.q.sim[j,,i], probs = c(0.025), na.rm = TRUE)
    Q.calc.med.param[j,i]<-quantile(matriz.q.sim[j,,i], probs = c(0.5), na.rm = TRUE)
  }
  curva.max.post<-tabela.cch.spd(cota.min, cota.max, passo, df.fitSPD, i, K)
  Q.calc.med[,i]<-curva.max.post$vazoes
  plot(y = Med.consist.SPD$Cota[y=Med.consist.SPD$Per.Validade==i], x = Med.consist.SPD$Vazao[Med.consist.SPD$Per.Validade==i], type = "p", ylab = "Cotas", xlab = "Vazões", ylim=c(0,10), xlim=c(0, 1000), main = paste("Período", i))
  lines(y = seq(cota.min,cota.max-0.01,passo), x = Q.calc.med[,i], type = "l", col=2)
  lines(y = seq(cota.min,cota.max-0.01,passo), x = Q.calc.sup[,i], type = "l", col=2, lty=2)
  lines(y = seq(cota.min,cota.max-0.01,passo), x = Q.calc.inf[,i], type = "l", col=2, lty=2)
  lines(y = seq(cota.min,cota.max-0.01,passo), x = Q.calc.med.param[,i], type = "l", col=1)
  lines(y = seq(cota.min,cota.max-0.01,passo), x = Q.calc.sup.param[,i], type = "l", col=4, lty=2)
  lines(y = seq(cota.min,cota.max-0.01,passo), x = Q.calc.inf.param[,i], type = "l", col=4, lty=2)
  #arrows(x0 = Med.consist$Cota[Med.consist$Per.Validade==i], y0 = Med.consist$Vazao[Med.consist$Per.Validade==i] - Med.consist$mi[Med.consist$Per.Validade==i],
  #      x1 = Med.consist$Cota[Med.consist$Per.Validade==i], y1 = Med.consist$Vazao[Med.consist$Per.Validade==i] + Med.consist$mi[Med.consist$Per.Validade==i],
  #     angle = 90, code = 3, length = 0.1)
}


#Vamos salvar os resultados em um txt
cotas<-matriz.q.sim[,1,1]

#Função para alimentar um data frame com os resultados os Intervalos de Credibilidade
preenche.ics<-function(cotas,Qcalcmed,Qcalcinf,Qcalcsup,Validades,n.validades){
  data.frame.ics<-NULL
  data.frame.ics<-data.frame(matrix(data=NA,nrow=length(cotas),ncol=n.validades))
  for(i in 1:n.validades){
    data.frame.ics[,1+(i-1)*6]<-format(as.Date(Validades$Início[i]),"%d-%m-%Y")
    data.frame.ics[,1+(i-1)*6 + 1]<-format(as.Date(Validades$Fim[i]),"%d-%m-%Y")
    data.frame.ics[,1+(i-1)*6 + 2]<-cotas
    data.frame.ics[,1+(i-1)*6 + 3]<-Qcalcinf[,i]
    data.frame.ics[,1+(i-1)*6 + 4]<-Qcalcmed[,i]
    data.frame.ics[,1+(i-1)*6 + 5]<-Qcalcsup[,i]
  }
  for(i in 1:ncol(data.frame.ics)){
    if(i %% 6 == 1){
      colnames(data.frame.ics)[i]<-paste("CCH",trunc(i/6)+1,"_Validade_Inicio", sep = "")
    }
    else if(i %% 6 == 2){
      colnames(data.frame.ics)[i]<-paste("CCH",trunc(i/6)+1,"_Validade_Fim", sep = "")
    }
    else if(i %% 6 == 3){
      colnames(data.frame.ics)[i]<-paste("CCH",trunc(i/6)+1,"_Cotas", sep = "")
    }
    else if(i %% 6 == 4){
      colnames(data.frame.ics)[i]<-paste("CCH",trunc(i/6)+1,"_Q_min", sep = "")
    }
    else if(i %% 6 == 5){
      colnames(data.frame.ics)[i]<-paste("CCH",trunc(i/6)+1,"_Q_med", sep = "")
    }
    else if(i %% 6 == 0){
      colnames(data.frame.ics)[i]<-paste("CCH",trunc(i/6),"_Q_max", sep = "")
    }
  }
  return(data.frame.ics)
}



#alimentando um data frame com os ICs paramétricos
df.resultados.param<-preenche.ics(cotas=cotas,Qcalcmed = Q.calc.med.param,Qcalcsup = Q.calc.sup.param,Qcalcinf = Q.calc.inf.param,
                                  Validades = Validades,n.validades = K)

#Alimentando um data frame com os ICs totais
df.resultados<-preenche.ics(cotas=cotas,Qcalcmed = Q.calc.med,Qcalcsup = Q.calc.sup,Qcalcinf = Q.calc.inf,Validades = Validades,n.validades = K)

##Ajustando manualmente algumas colunas do df.resultados
df.resultados$CCH4_Q_med<-matriz.q.sim[,84,4] #Realização 83 para a curva 4
df.resultados$CCH8_Q_med<-matriz.q.sim[,54,8] #Realização 53 para a curva 8
df.resultados$CCH12_Q_med<-matriz.q.sim[,79,12] #Realização 78 para a curva 12
df.resultados$CCH18_Q_med<-matriz.q.sim[,31,18] #Realização 31 para a curva 18
df.resultados$CCH6_Q_med<-matriz.q.sim[,18,6] #Realização 17 para a curva 6
df.resultados$CCH2_Q_med<-matriz.q.sim[,96,2] #Realização 95 para a curva 2

  
write.table(df.resultados, "PetiCarrapato24.02.25_ajuste.manual_incertezatotal.csv", row.names = FALSE,sep=";",dec=",")
write.table(df.resultados.param, "PetiCarrapato24.02.25_ajuste.manual_incerteza.param.csv", row.names = FALSE,sep=";",dec=",")

#Escrevendo novas abas na memória com o df.resultados, o df.resultados.param e o Med.consist.SPD
wb<-loadWorkbook(memoria)

abas_adicionais<-list("ICs totais" = df.resultados,"ICs paramétricos" = df.resultados.param)

for(nome_aba in names(abas_adicionais)){
  addWorksheet(wb,nome_aba)
  writeData(wb,nome_aba,abas_adicionais[[nome_aba]])
}

saveWorkbook(wb, memoria, overwrite = TRUE)


##Agora vá ao excel e indique as ações a praticar em cada medição estranha
  ##Caso a ação seja de aumentar o valor de mi, aplique a alteração diretamente no excel
  ##Caso deseje alterar os períodos de validade das medições, altere diretamente no excel também
##Renomeie a aba BMDL SPD para BMDL SPD revX e rode novamente o código das linhas acima.
  ##Ao invés de usar o mesmo Med.consist SPD, leia diretamente da
    ##aba do BMDL SPD revX e aplique as exclusões indicadas





#Vamos plotar as nuvens de dispersão usando ggplot2
#install.packages("patchwork")
library(patchwork)
plots_list <- list()

# Ajustes de tema para os gráficos individuais
theme_individual <- theme(
  plot.title = element_text(size = 10, face = "bold"), 
  axis.title = element_text(size = 10), 
  axis.text = element_text(size = 10), 
  legend.title = element_text(size = 10), 
  legend.text = element_text(size = 8)
)
Med.consist.SPD.backup<-Med.consist.SPD
#Med.consist.SPD<-Med.consist.SPD.auxiliares
#Plotagem em Inglês com Q x H
for(i in 1:K){
  col.init<-6*(i-1)+3
  col.fin<-6*i
  df<-df.resultados[,col.init:col.fin]
  df.med<-Med.consist.SPD[Med.consist.SPD$Per.Validade==i,]
  
  colnames(df)<-c("Cotas","Q.min","Q.med","Q.sup")
  df<-df[df$Q.med>=min(df.med$Cota),]
  titulo<-paste0("Per. ",i)
  #caminho<-paste0("PaperFiles/ModeloLinear/Per",i,".png")
  p<-ggplot(data=df,aes(x=Cotas)) +
    geom_line(aes(y = Q.med), color = "black")+
    geom_ribbon(aes(ymin=Q.min,ymax=Q.sup),fill="red", alpha=0.5)+
    labs(x = "h (m)", y="Q (m³/s)", title = titulo) +
    geom_point(data=df.med,aes(x=Cota, y=Vazao), color="blue", size=1.5)+
    scale_x_continuous(limits = c(0,10))+
    scale_y_continuous(limits = c(1,900))+
    theme_individual
  #ggsave(caminho, plot=p, width = 10, height = 8, dpi = 300)
  plots_list[[i]] <- p
}

combined_plot <- wrap_plots(plots_list, ncol = 5)
combined_plot

#plotagem plotly painel
library(plotly)

# Inicializar lista para armazenar gráficos plotly
plots_list_plotly <- list()

# Loop para gerar gráficos
for (i in 1:K) {
  col.init <- 6 * (i - 1) + 3
  col.fin <- 6 * i
  df <- df.resultados[, col.init:col.fin]
  df.med <- Med.consist.SPD[Med.consist.SPD$Per.Validade == i, ]
  
  colnames(df) <- c("Cotas", "Q.min", "Q.med", "Q.sup")
  df <- df[df$Q.med >= min(df.med$Cota), ]
  titulo <- paste0("Per. ", i)  # Título individual para cada gráfico
  
  # Criar gráfico plotly
  p <- plot_ly() %>%
    # Adicionar faixas horizontais como "polígonos"
    add_trace(
      data = df,
      x = c(df$Q.min, rev(df$Q.sup)), # Combinar min e sup para formar o polígono
      y = c(df$Cotas, rev(df$Cotas)), # Mesma altura para criar a faixa
      type = "scatter",
      mode = "lines",
      fill = "toself",
      fillcolor = "rgba(255, 0, 0, 0.5)", # Vermelho translúcido
      line = list(color = "rgba(255, 0, 0, 0)"), # Sem borda visível
      name = paste0("Per ", i, " - Faixa")
    ) %>%
    # Linha central (Q.med)
    add_lines(
      data = df,
      x = ~Q.med,
      y = ~Cotas,
      line = list(color = "black"),
      name = paste0("Per ", i, " - Linha")
    ) %>%
    # Pontos
    add_markers(
      data = df.med,
      x = ~Vazao,
      y = ~Cota,
      marker = list(color = "blue", size = 5),
      name = paste0("Per ", i, " - Pontos")
    ) %>%
    # Configurações do layout
    layout(
      annotations = list(
        list(
          x = 0.5,
          y = 1.1,
          text = titulo,
          showarrow = FALSE,
          xref = "paper",
          yref = "paper",
          font = list(size = 14, color = "black"),
          align = "center"
        )
      ),
      xaxis = list(type = "log", title = "Q (m³/s)", range = c(log10(1), log10(800))),
      yaxis = list(title = "h (m)", range = c(1, 8)),
      showlegend = FALSE # Desativa legendas individuais para clareza
    )
  
  # Adicionar gráfico à lista
  plots_list_plotly[[i]] <- p
}

# Combinar gráficos com subplot
combined_plot_plotly <- subplot(
  plots_list_plotly,
  nrows = ceiling(K / 4), # Ajustar número de linhas com base no número de gráficos
  shareX = TRUE,
  shareY = TRUE,
  titleX = TRUE,
  titleY = TRUE
)

# Exibir o painel
combined_plot_plotly


#ggsave("RC_Tumiritinga_01.png", plot = combined_plot, width = 50, height = 19, dpi = 300, limitsize = FALSE)


#Plotar todos os resultados em 1 só gráfico
# Data frame acumulado
df_acumulado <- data.frame()

# Loop para acumular os dados de cada período
for(i in 1:K){
  col.init <- 6 * (i - 1) + 3
  col.fin <- 6 * i
  df <- df.resultados[, col.init:col.fin]
  df.med <- Med.consist.SPD[Med.consist.SPD$Per.Validade == i,]
  
  colnames(df) <- c("Cotas", "Q.min", "Q.med", "Q.sup")
  df <- df[df$Q.med >= min(df.med$Cota),]
  df$Período <- factor(i)  # Adiciona a coluna indicando o período
  
  df_acumulado <- bind_rows(df_acumulado, df)
  
  df.med$Período <- factor(i)  # Adiciona a coluna indicando o período em df.med
  df_acumulado <- bind_rows(df_acumulado, df.med)
}
library(RColorBrewer)
#paleta_cores <- seq(1,K*30,5)
#paleta_cores <- c(brewer.pal(11, "Set3"), brewer.pal(3, "Paired"))
paleta_cores <- c(brewer.pal(12,"Set3"),brewer.pal(7,"Dark2"))
  
  #c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
                  # "#A65628", "#F781BF", "#999999", "#66C2A5",
                  # "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854")


#Plotagem de cotas no eixo y
df_acumulado2<-df_acumulado[is.na(df_acumulado$Vazao)==FALSE,]
p <- ggplot(data = df_acumulado2, aes(x = Vazao, color = Período, fill = Período)) + 
  geom_line(data=df_acumulado,aes(x = Q.med,y=Cotas)) +
  geom_point(data = df_acumulado2, aes(y = Cota, x = Vazao), size = 1.5) +
  labs(y = "h (m)", x = "Q (m³/s)") +
  scale_x_continuous(limits = c(1, 900)) +
  scale_color_manual(values = paleta_cores, name = "Período") +
  scale_fill_manual(values = paleta_cores, guide= "none") +
  scale_y_continuous(limits = c(min(df_acumulado$Cota, na.rm = TRUE), 10)) +
  theme(legend.position = "right")

# Exibir o gráfico
print(p)

library(plotly)

# Geração do gráfico plotly com séries separadas para as linhas
plotly_chart <- plot_ly() %>%
  # Adiciona as linhas como séries separadas
  add_lines(
    data = df_acumulado,
    x = ~Q.med,
    y = ~Cotas,
    color = ~Período, # Agrupa por "Período" para separar séries
    colors = paleta_cores,
    name = ~Período, # Nome das séries na legenda
    line = list(width = 1.5)
  ) %>%
  # Adiciona os pontos de df_acumulado2
  add_markers(
    data = df_acumulado2,
    x = ~Vazao,
    y = ~Cota,
    color = ~Período, # Mesma paleta para os pontos
    colors = paleta_cores,
    marker = list(size = 8),
    name = ~Período,
  ) %>%
  # Configura os eixos
  layout(
    xaxis = list(
      # type = "log",
      title = "Q (m³/s)",
      range = c(0, 900)
    ),
    yaxis = list(
      title = "h (m)",
      range = c(min(df_acumulado$Cota, na.rm = TRUE), 10)
    ),
    legend = list(title = list(text = "Período"), orientation = "v", x = 1.05),
    title = "Gráfico Plotly"
  )

# Mostra o gráfico
plotly_chart


#Inspecionando a matriz de curvas simuladas para verificar quais realizações podem garantir 
#melhores ajustes para as curvas mal ajustadas
#Inspecionando a matriz de curvas simuladas para verificar quais realizações podem garantir 
#melhores ajustes para as curvas mal ajustadas
plot.realizacoes<-function(matriz.q.sim,periodo,cota.min,cota.max,Q.min,Q.max){
  par(mfrow=c(3,3))
  plot(matriz.q.sim[,2,periodo],matriz.q.sim[,1,periodo],type="l",ylim=c(cota.min,cota.max),xlim=c(Q.min,Q.max),
       main="Curva 2")
  lines(Med.consist.SPD$Vazao[Med.consist.SPD$Per.Validade==periodo],
        Med.consist.SPD$Cota[Med.consist.SPD$Per.Validade==periodo],
        type="p",col=1)
  for(i in 3:100){
    plot(matriz.q.sim[,i,periodo],matriz.q.sim[,1,periodo],type="l",col=2,ylim=c(cota.min,cota.max),xlim=c(Q.min,Q.max),
         main=paste("Curva ",i))
    #lines(matriz.q.sim[,i,1],matriz.q.sim.inf[,1,1],type="l",col=2)
    lines(Med.consist.SPD$Vazao[Med.consist.SPD$Per.Validade==periodo],
          Med.consist.SPD$Cota[Med.consist.SPD$Per.Validade==periodo],
          type="p",col=1)
  }
  
}
#Plotando realizações do período desejado
plot.realizacoes(matriz.q.sim,12,1,2,1,25)

#Verificando a sopreposição de todas as realizações
# par(mfrow=c(1,1))
# plot(matriz.q.sim[,2,1],matriz.q.sim[,1,1],type="l",ylim=c(1.9,6),xlim=c(1,400),log="x",
#      main="Curva 1")
# lines(Med.consist.SPD$Vazao[Med.consist.SPD$Per.Validade==1],
#       Med.consist.SPD$Cota[Med.consist.SPD$Per.Validade==1],
#       type="p",col=1)
# 
# lines(tabela.cch.spd(1,6,0.01,df.fitSPD,4,18)[,2],tabela.cch.spd(1,6,0.01,df.fitSPD,4,18)[,1],
#       type="l",col=3)
# for(i in 3:100){
#   #plot(matriz.q.sim[,i,1],matriz.q.sim[,1,1],type="l",col=2,ylim=c(1.9,2.3),xlim=c(3,10),log="x",
#        #main=paste("Curva ",i))
#   lines(matriz.q.sim[,i,1],matriz.q.sim.inf[,1,1],type="l",col=2)
#   # lines(Med.consist.SPD$Vazao[Med.consist.SPD$Per.Validade==1],
#   #       Med.consist.SPD$Cota[Med.consist.SPD$Per.Validade==1],
#   #       type="p",col=1)
# }



#Avaliando max posts dos períodos 4 e 8
# for(i in 1:K){
#   curva<-tabela.cch.spd(1, 10, 0.01, df.fitSPD, i, 18)
#   plot(curva$vazoes, curva$cotas, type="l", ylim=c(1,3), xlim=c(0.01,50), main=paste("Curva ",i), xlab="Vazão", ylab="Cota",log="x")
#   lines(Med.consist.SPD$Vazao[Med.consist.SPD$Per.Validade == i], 
#         Med.consist.SPD$Cota[Med.consist.SPD$Per.Validade == i], type="p", col=2)
# }



#Agora vamos calcular os desvios
#Função para interpolar
interp<-function(y0,y1,x0,x1,xi){
  yi<-y0+((xi-x0)/(x1-x0))*(y1-y0)
  return(yi)
}

#Interpolando os dataframes das curvas do modelo linear para termos vazões a cada cm

cotas<-seq(min(df.resultados$CCH1_Cotas),max(df.resultados$CCH1_Cotas),0.01)

# df.resultados.interp<-matrix(data=NA,nrow=length(cotas),ncol=max(Med.consist.SPD$Per.Validade)*6)
# df.resultados.interp<-data.frame(df.resultados.interp)
# colnames(df.resultados.interp)<-colnames(df.resultados)

df.resultados.interp<-df.resultados

# for(i in 1:max(Med.consist.SPD$Per.Validade)){
#   df.resultados.interp[,6*i-3]<-cotas
# }

#Colunas onde estão as vazões
# cols<-NULL
# for(i in 1:max(Med.consist.SPD$Per.Validade)){
#   cols<-c(cols,6*i,6*i-1,6*i-2)
# }
# 
# for(col in cols){
#   for(linha in 1:(length(df.resultados$CCH1_Cotas)-1)){
#     x0<-df.resultados$CCH1_Cotas[linha]
#     x1<-df.resultados$CCH1_Cotas[linha+1]
#     xis<-seq(x0,x1,0.01) 
#     y0<-df.resultados[linha,col]
#     y1<-df.resultados[linha+1,col]
#     linhas.destino<-seq(((linha-1)*10+1),(linha*10+1),1)
#     for(i in 1:length(xis)){
#       xi<-xis[i]
#       yi<-interp(y0,y1,x0,x1,xi)
#       df.resultados.interp[linhas.destino[i],col]<-yi
#     }
#   }
# }

#Calculando as vazões referentes a cada cota medida para o modelo linear
q.calc.mod.linear<-NULL
for(i in 1:length(Med.consist.SPD$Cota)){
  p.validade<-Med.consist.SPD$Per.Validade[i]
  Cota<-df.resultados.interp[,6*p.validade-3]
  Vazao.calc<-df.resultados.interp[,6*p.validade-1]
  h.q.calc<-data.frame(cbind(Cota,Vazao.calc))
  q.calc.mod.linear[i]<-mean(h.q.calc$Vazao.calc[abs(h.q.calc$Cota-Med.consist.SPD$Cota[i])<0.009])
}
Med.consist.SPD$Q.calc<-q.calc.mod.linear

#Plotagem das vazões medidas vs as vazões calculadas
par(mfrow=c(1,1))
plot(Med.consist.SPD$Vazao,q.calc.mod.linear,type="p")


#Calculando os desvios para cada medição em relação ao modelo linear
par(mfrow = c(1,1))
desv.mod.lin<-(q.calc.mod.linear-Med.consist.SPD$Vazao)/q.calc.mod.linear
desv.abs<-q.calc.mod.linear-Med.consist.SPD$Vazao
#Med.consist.SPD$Data<-lubridate::ymd(Med.consist.SPD$Data)
Med.consist.SPD$desvios<-desv.mod.lin
Med.consist.SPD$desv.abs<-desv.abs
Med.consist.SPD$data.formatada<-as.POSIXct(Med.consist.SPD$Data,format="%Y-%m-%d %H:%M:%S")
#Med.consist.SPD$Data<-as.character(Med.consist.SPD$Data)                                                                
#Med.consist.SPD<-Med.consist.SPD %>% mutate(data.formatada = format(data.formatada,"%m/%Y"))
#head(Med.consist.SPD)

#head(Med.consist.SPD$year)

# plot(Med.consist.SPD$Data,Med.consist.SPD$desv.abs,type="h")
# plot(Med.consist.SPD$Data,Med.consist.SPD$desvios,type="h")
# ggplot(data=Med.consist.SPD,aes(x=Data,y=desvios,fill=Per.Validade))+
#   geom_bar(stat="identity", position = "dodge")+
#   labs(x = "Data", y = "Desvios (m³/s)", fill = "Período de Validade")+
#   theme_minimal()+
#   scale_x_date(date_labels = "%d-%m-%Y", date_breaks = "1 month")
# 
# ggplot(Med.consist.SPD, aes(x = Data, y = desvios, fill = as.factor(Per.Validade))) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Data", y = "Desvio", fill = "Período de Validade") +
#   theme_minimal() +  scale_fill_brewer(palette = "Set1") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   scale_x_date(date_labels = "%d-%m-%Y", date_breaks = "1 month") +
#   theme(panel.grid.minor = element_blank())

custom_colors <- colorRampPalette(brewer.pal(9, "Set1"))(18)

#Vamos plotar os resíduos por tempo para os períodos de 3 a 3
pivos<-seq(1,18,2)
plots.desv.list<-list()
plots.desv.list.cotas<-list()
for (i in seq_along(pivos)) {
  pivo <- pivos[i]
  meds.filter <- Med.consist.SPD[Med.consist.SPD$Per.Validade >= pivo & Med.consist.SPD$Per.Validade <= (pivo + 2), ]
  
  p.desv <- ggplot() +
    geom_bar(stat = "identity", data = meds.filter, aes(x = Data, y = desvios, fill = as.factor(Per.Validade))) +
    theme_minimal() +
    labs(x = "Data", y = "Desvios Relativos", fill = "Períodos") +
    scale_fill_manual(values = custom_colors)
  
  p <- p.desv + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  p.cotas <- p.desv.cota + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  plots.desv.list[[i]] <- p  # Adiciona o gráfico à lista usando o índice i
}
for(i in 1:length(plots.desv.list)){
  name<-paste0("Resíduos p ",pivos[i],pivos[i]+1,pivos[i]+2,".png")
  plot<-wrap_plots(plots.desv.list[[i]],nrow=1)
  ggsave(name,plot = plot,path="Figuras Resíduos/Resíduos Por Tempo (Ajuste manual)")
  print(wrap_plots(plots.desv.list[[i]],nrow=1))
}

#Vamos plotar os resíduos por cota para cada período
for(i in 1:K){
  meds.filter <- Med.consist.SPD[Med.consist.SPD$Per.Validade == i,]
  
  p.desv.cota <- ggplot()+
    geom_bar(stat = "identity", data = meds.filter %>% arrange(Per.Validade,Cota), aes(x = Cota, y = desvios, fill = as.factor(Per.Validade))) +
    theme_minimal() +
    labs(x = "Cota", y = "Desvios Relativos", fill = "Períodos") +
    scale_fill_manual(values = custom_colors)
  
  p.cotas <- p.desv.cota + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  plots.desv.list.cotas[[i]]<-p.cotas   
}

for(i in 1:length(plots.desv.list.cotas)){
  name<-paste0("Resíduos p ",i,".png")
  plot.cotas<-wrap_plots(plots.desv.list.cotas[[i]],nrow=1)
  ggsave(name,plot = plot.cotas,path="Figuras Resíduos/Resíduos Por Cota (Ajuste manual)")
  print(wrap_plots(plots.desv.list.cotas[[i]],nrow=1))
}




# p1 <- ggplot() +
#   geom_bar(stat = "identity", data = Med.consist.SPD, aes(x = Data, y = desvios, fill = as.factor(Per.Validade))) +
#   theme_minimal() +
#   labs(x = "Data", y = "Desvios Relativos", fill = "Períodos") +
#   scale_fill_manual(values = custom_colors)
# 
# p1 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# 
# p2<-ggplot(data=Med.consist.SPD,aes(x=Data,y=desv.abs,fill=as.factor(Per.Validade)))+geom_bar(stat="identity")+theme_minimal()
# p2 + theme(axis.text.x = element_text(angle = 90, hjust = 2))

#densidades.tau<-density(tau.vertical)
#datas.tau<-data_referencia+days(as.integer(densidades.tau$x))

# par(mfrow=c(3,1))
# plot(cotagrama$Data.Hora,cotagrama$Cotas.Modelo,type="l",xlab="Data",ylab="Cota(m)")
# plot(datas.tau,densidades.tau$y,col=2,xlab="Data",ylab="Probs de Quebra", xlim=c(min(cotagrama$Data.Hora,na.rm = TRUE),max(cotagrama$Data.Hora,na.rm = TRUE)),type="l")
# plot(Med.consist.SPD$data.formatada,Med.consist.SPD$desvios,col=1,type="h",xlab="Data",ylab="Desvios Relativos",xlim=c(min(Med.consist.SPD$data.formatada),max(cotagrama$Data.Hora,na.rm = TRUE)),ylim=c(-1,1))
# datas_quebra<-as.POSIXct(Validades$Fim, format = "%Y-%m-%d")
# Validades$Fim.format<-datas_quebra
# abline(v = datas_quebra, col = "red", lty = 2, lwd = 1)
# 
# ##Análise gráfica dos desvios
# hist(Med.consist.SPD$desvios)
# boxplot(Med.consist.SPD$desvios)
# qqnorm(Med.consist.SPD$desvios)
# qqline(Med.consist.SPD$desvios,col=2)
# #Vamos plotar o cotagrama, as medições e as probabilidades de transição entre os períodos
# #prepagação de dados para plotagem:
# cotagrama2<-cotagrama
# cotagrama2$data.hora <- as.POSIXct(cotagrama2$data.hora, format = "%Y-%m-%d")
# df.probs.trans<-data.frame(datas.tau,densidades.tau$y)
# df.probs.trans$datas.tau<-as.POSIXct(datas.tau,format="%Y-%m-%d")


# #Vamos salvar o cotagrama para definir manualmente os pontos de segmentação
# # Instalar e carregar o pacote openxlsx
# if (!require("openxlsx")) install.packages("openxlsx")
# library(openxlsx)
# 
# 
# # Criar um novo workbook
# wb<-createWorkbook()
# 
# # Adicionar abas e escrever dados
# addWorksheet(wb, "Cotagrama")
# writeData(wb,sheet="Cotagrama",x=cotagrama2)
# 
# addWorksheet(wb,"Medições")
# writeData(wb,"Medições",x=Med.consist.SPD)
# 
# addWorksheet(wb,"Probs Transição")
# writeData(wb,"Probs Transição",x=df.probs.trans)
# 
# addWorksheet(wb,"Valid. Modelo")
# writeData(wb,"Valid. Modelo",x=Validades)
# 
# saveWorkbook(wb,"ResumoPeti2.xlsx",overwrite = TRUE)




# library(patchwork)
# plots.list.2<-list()
# cotag<-ggplot() +
#   geom_line(data = cotagrama2, aes(x = data.hora, y = Cotas, color = "Cotas")) +
#   geom_point(data = Med.consist.SPD, aes(x = data.formatada, y = Cota, color = "Medições")) +
#   scale_color_manual(values = c("Cotas" = "black", "Medições" = "red")) +
#   labs(x = "Tempo", y = "Cotas", color = "Legenda")
# 
# probs.trans<-ggplot()+
#   geom_line(data=df.probs.trans,aes(datas.tau,y=densidades.tau.y))+labs(x="Data",y="Probabilidades de quebra")
#   
# # probs.trans.modelo <- probs.trans +
# #   geom_vline(data = Validades, aes(xintercept = Fim.format), 
# #              color = "red", linetype = "dashed") +
# #   labs(title = "Probabilidades de Quebra com quebras do modelo")
# 
# probs.trans.manuais <- probs.trans +
#   geom_vline(data=Validades,aes(xintercept = Fim.format),
#                                         color = "red", linetype = "dashed") + 
#   labs(title = "Probabilidades de Quebra com quebras ajustadas manualmente")
#   
# 
# plots.list.<-list(cotag,probs.trans.manuais)
# wrap_plots(plots.list.3,nrow=2)


#Vamos avaliar as métricas da ANA

#Cálculo do mae de cada período
Med.consist.SPD %>%
  group_by(Per.Validade) %>%
  summarise(media_desvios = mean(abs(desvios), na.rm = TRUE))

#Cálculo da distribuição de desvios de cada período
metricas.ana<-Med.consist.SPD %>%
  group_by(Per.Validade) %>%
  summarise(
    media_desvios = mean(abs(desvios), na.rm = TRUE),
    perc_desv_positivos = sum(desvios > 0) / n() * 100,
    perc_desv_negativos = sum(desvios < 0) / n() * 100,
    contagem = n()
  )

colnames(metricas.ana)<-c("Per. Validade","MAE", "% Desvios Positivos","% Desvios Negativos", "Nº de Medições")
metricas.ana

#Escrevendo novas abas na memória

wb<-loadWorkbook(memoria)

abas_adicionais<-list("Metricas" = metricas.ana,
                      "BMDL SPD" = Med.consist.SPD)

for(nome_aba in names(abas_adicionais)){
  addWorksheet(wb,nome_aba)
  writeData(wb,nome_aba,abas_adicionais[[nome_aba]])  
}

saveWorkbook(wb, memoria, overwrite = TRUE)


#Definindo as equações da ANA para cada validade
# Exemplo de dados (substitua pelos seus dados reais)
curvas.memoria<-readxl::read_xlsx("memoria.xlsx",sheet="ICs totais")
nomes.vazoes<-paste0("CCH",seq(1,K,1),"_Q_med")
nomes.cotas<-paste0("CCH",seq(1,K,1),"_Cotas")



df <- data.frame(
  h = curvas.memoria[[nomes.cotas[[1]]]],
  Q = curvas.memoria[[nomes.vazoes[[1]]]]
)

# Ajuste da regressão não linear
modelo <- nls(Q ~ a * (h - b)^c, data = df, 
              start = list(a = 1.9, b = min(df$h) - 1, c = 1.5), 
              algorithm = "port",
              lower = c(0.5, -0.5, 1.3))  # Restrições: a > 0, c > 0

# Resumo do modelo
summary(modelo)

# Plotando os dados e o ajuste
plot(df$h, df$Q, pch = 16, col = "blue", xlab = "Cota (h)", ylab = "Vazão (Q)")
curve(coef(modelo)["a"] * (x - coef(modelo)["b"])^coef(modelo)["c"], 
      add = TRUE, col = "red", lwd = 2)


#library(openxlsx)
#write.xlsx(metricas.ana,file="Metricas ANA.xlsx")

#Ajuste das validades manualmente
# datas.quebra.manuais<-c("1955-01-25","1960-01-24","1964-01-18","1974-12-29","1977-01-29",
#                         "1979-02-01","1986-12-26","1992-01-24","1995-12-26","2000-01-29",
#                         "2002-01-19","2003-12-07","2004-12-24","2011-11-28","2020-01-25",
#                         "2022-05-21")




save.image("24.02.2025_ajustemanual.RData")
#load("09.12.2024_ajustemanual.RData")



#Avaliando o ajuste fino de uma curva-chave qualquer
#library(openxlsx)
#bmdl.full<-read.xlsx("BMDL Bacia Rio Doce.xlsx","56640001",startRow = 4, colNames = TRUE,detectDates = TRUE)

#Selecionando apenas as colunas de interesse
#bmdl.full<-bmdl.full %>% select('Data', `Cota.(m)`,`Vazão.(m³/s)`)

#Alterando nomes para coincidir com Med.consist
#colnames(bmdl.full)<-c("Data","Cota","Vazao")

#Convertendo a coluna de data para character
#bmdl.full$Data<-as.character(bmdl.full$Data)

#Inserindo colunas de Per.Validade dias e mi
#bmdl.full<-bmdl.full %>% mutate(Per.Validade = 1, dias = as.numeric(as.Date(Data)-data_referencia),
#                                mi = Vazao*0.05)

#Como a última medição utilizada no ajuste das curvas-chave foi de 21/05/2022, vamos pegar a partir
#dela para adicionar ao Med.consist
#Med.consist<-rbind(Med.consist,bmdl.full[470:479,])

#Agora vamos rodar o mcpd considerando a última curva válida e as medições do último período de validade,
#Que começa em 26/01/2020
Med.consist<-Med.consist.SPD
Med.consist<-Med.consist %>% mutate(Per.Validade = 1)
head(Med.consist)
row.names(Med.consist)<-NULL
Med.consist<-Med.consist[-c(343),]

#lista.base<-c(as.numeric(Validades$Início[16]-data_referencia),25610)
lista.base<-c(20973,25610)
lista.flutuante<-lista.base
df.referencia<-NULL
list.dfs<-NULL

#lista.base<-list.taus.backup1.1
#lista.flutuante<-lista.base
tolerancia.residuo<-0.015
contagem.iteracoes<-0
teste.parada<-TRUE
n.max.iteracoes<-3
n.iter<-6000
n.burn<-3000
n.chains<-6
param.m<-1.3
h.min<--1
h.max<-12
r.hat.max<-1.6
list.df.referencia<-list()
while(teste.parada==TRUE){
  contagem.iteracoes<-contagem.iteracoes+1
  lista.base<-lista.flutuante
  list.dics<-list()
  list.dics.auxiliar<-list()
  list.min.dics<-list()
  list.dics.relat<-list()
  df.Baratin<-NULL
  df.3seg<-NULL
  df.2seg<-NULL
  df.1seg<-NULL
  for (i in 1:(length(lista.base)-1)){
    t0<-lista.base[i]
    tf<-lista.base[i+1]
    med.filtro<-Med.consist[Med.consist$dias>=t0 & Med.consist$dias<tf,]

    list.dics<-list()
    list.dics.auxiliar<-list()
    list.dics.relat<-list()
    #list.min.dics<-list()
    df.Baratin<-NULL
    df.3seg<-NULL
    df.2seg<-NULL
    df.1seg<-NULL
    if(length(med.filtro$Data)>n.min.med.iteracao){
      #Necessário recalcular Baratin para o Med.consist compreendido entre os novos t0 e tf
      df.Baratin<-SPD(t0,tf,med.filtro)
      capture.output(print(df.Baratin),file=paste0(getwd(),"/Resumos.Iterações/Baratin parte",i,"-iter.",contagem.iteracoes,".csv"))
      p.baratin.cadeias<-mcmc_trace(df.Baratin)+theme_minimal()
      p.baratin.posts<-mcmc_hist(df.Baratin)+theme_minimal()
      ggsave(file=paste0(getwd(),"/Grafs.Iterações/ChainsBaratin parte",i,"-iter.",contagem.iteracoes,".png"),p.baratin.cadeias,width = 10,height = 6,dpi=300, bg="white")
      ggsave(file=paste0(getwd(),"/Grafs.Iterações/PostsBaratin parte",i,"-iter.",contagem.iteracoes,".png"),p.baratin.posts,width = 15,height = 10,dpi=300, bg="white")
      #tab.baratin<-data.frame(df.resultados$CCH16_Cotas,df.resultados$CCH16_Q_med)
      #colnames(tab.baratin)<-c("cotas","vazoes")
      tab.baratin<-tabela.cch.baratin(cota.min=h.min,cota.max = h.max,step.cotas = 0.01,df.SPD = as.data.frame(df.Baratin))
      rc.and.meds<-ggplot()+geom_line(data=tab.baratin,aes(x=cotas,y=vazoes,color="Curva Chave"))+
        geom_point(data=med.filtro,aes(x=Cota,y=Vazao,color="Medições"))+
        scale_color_manual(values=c("Curva Chave" = "blue","Medições" = "red"))+
        labs(x = "Cotas (m)", y="Vazões (m³/s)",color="Legenda")+
        xlim(0.6, 3) +  # Limitar o eixo X (cotas)
        ylim(0, 50)  # Limitar o eixo Y (vazões)
      ggsave(file=paste0(getwd(),"/Grafs.Iterações/CurvaGeral parte",i,"-iter.",contagem.iteracoes,".png"),rc.and.meds,width = 15,height = 10,dpi=300, bg="white")
      #Calcular também os novos resíduos
      residuos.filtro<-residuos.novas.meds(df.fitSPD,t0,tf,med.filtro,tab.baratin)
      
      
      med.filtro$R<-residuos.filtro$R
      med.filtro$dp.rcs<-residuos.filtro$dp.rcs
      med.filtro$Rmin<-med.filtro$R-2*med.filtro$dp.rcs
      med.filtro$Rmax<-med.filtro$R+2*med.filtro$dp.rcs
      p<-ggplot(data=med.filtro,aes(x=dias,y=R))+
        geom_point(shape=21,fill=NA)+
        geom_errorbar(aes(ymin = Rmin,ymax=Rmax),width=10)+
        labs(y="Resíduos")+
        theme_minimal()
      #df.5seg<-mcpfit(t0,tf,5,med.filtro,1.0,6)
      #df.4seg<-mcpfit(t0,tf,4,med.filtro,1.0,6)
      df.3seg<-mcpfit(t0,tf,3,med.filtro,param.m,n.chains,n.iter,n.burn)
      if(length(as.data.frame(df.3seg)[,1])<=(2*(n.iter-n.burn))){
        df.3seg<-mcpfit(t0,tf,3,med.filtro,param.m,10,n.iter,n.burn)
      }
      df.2seg<-mcpfit(t0,tf,2,med.filtro,param.m,n.chains,n.iter,n.burn)
      if(length(as.data.frame(df.2seg)[,1])<=(2*(n.iter-n.burn))){
        df.2seg<-mcpfit(t0,tf,2,med.filtro,param.m,10,n.iter,n.burn)
      }
      df.1seg<-mcpfit(t0,tf,1,med.filtro,param.m,n.chains,n.iter,n.burn)
      if(length(as.data.frame(df.1seg)[,1])<=(2*(n.iter-n.burn))){
        df.1seg<-mcpfit(t0,tf,1,med.filtro,param.m,10,n.iter,n.burn)
      }
      
      #Geração e salvamento de gráficos
      # p5.cadeias<-mcmc_trace(df.5seg)
      # p5.posts<-mcmc_hist(df.5seg)
      # 
      # p4.cadeias<-mcmc_trace(df.4seg)
      # p4.posts<-mcmc_hist(df.4seg)
      if(length(as.data.frame(df.3seg)[,1])>(2*n.chains)){
        p3.cadeias<-mcmc_trace(df.3seg)
        p3.posts<-mcmc_hist(df.3seg)
        nome3.cadeias<-(paste0("Chains.3seg.","parte ",i,"-iter.",contagem.iteracoes,".png"))
        nome3.posts<-(paste0("Posts.3seg.","parte ",i,"-iter.",contagem.iteracoes,".png"))
        caminho3.cadeias<-paste0(getwd(),"/Grafs.Iterações/",nome3.cadeias)
        caminho3.posts<-paste0(getwd(),"/Grafs.Iterações/",nome3.posts)
        ggsave(caminho3.cadeias,plot=p3.cadeias,width=15, height=6,dpi=300, bg="white")
        ggsave(caminho3.posts,plot=p3.posts,width=15, height=6,dpi=300, bg="white")
        caminho3.resumo<-paste0(getwd(),"/Resumos.Iterações/","3seg.","parte ",i,"-iter.",contagem.iteracoes,".csv")
        capture.output(print(df.3seg), file = caminho3.resumo)
        
        if(max(rhat(df.3seg))<=r.hat.max){
          df.3seg<-as.data.frame(df.3seg)
          DIC3<-dic.calc(df.3seg,3,med.filtro,t0,tf)  
        }else{
          DIC3<-10^8
        }
      }else{
        DIC3<-10^9
      }
      
      
      if(length(as.data.frame(df.2seg)[,1])>(2*n.chains)){
        p2.cadeias<-mcmc_trace(df.2seg)
        p2.posts<-mcmc_hist(df.2seg)
        nome2.cadeias<-(paste0("Chains.2seg.","parte ",i,"-iter.",contagem.iteracoes,".png"))
        nome2.posts<-(paste0("Posts.2seg.","parte ",i,"-iter.",contagem.iteracoes,".png"))
        caminho2.cadeias<-paste0(getwd(),"/Grafs.Iterações/",nome2.cadeias)
        caminho2.posts<-paste0(getwd(),"/Grafs.Iterações/",nome2.posts)
        ggsave(caminho2.cadeias,plot=p2.cadeias,width=15, height=6,dpi=300, bg="white")
        ggsave(caminho2.posts,plot=p2.posts,width=15, height=6,dpi=300, bg="white")
        caminho2.resumo<-paste0(getwd(),"/Resumos.Iterações/","2seg.","parte ",i,"-iter.",contagem.iteracoes,".csv")
        capture.output(print(df.2seg), file = caminho2.resumo)
        
        if(max(rhat(df.2seg))<=r.hat.max){
          df.2seg<-as.data.frame(df.2seg)
          DIC2<-dic.calc(df.2seg,2,med.filtro,t0,tf)  
        }else{
          DIC2<-10^8
        }
        
      }else{
        DIC2<-10^9
      }
      
      
      p1.cadeias<-mcmc_trace(df.1seg)
      p1.posts<-mcmc_hist(df.1seg)
      
      
      
      nome1.cadeias<-(paste0("Chains.1seg.","parte ",i,"-iter.",contagem.iteracoes,".png"))
      nome1.posts<-(paste0("Posts.1seg.","parte ",i,"-iter.",contagem.iteracoes,".png"))
      
      caminho1.cadeias<-paste0(getwd(),"/Grafs.Iterações/",nome1.cadeias)
      caminho1.posts<-paste0(getwd(),"/Grafs.Iterações/",nome1.posts)
      
      ggsave(caminho1.cadeias,plot=p1.cadeias,width=15, height=6,dpi=300, bg="white")
      ggsave(caminho1.posts,plot=p1.posts,width=15, height=6,dpi=300, bg="white")
      
      
      #Geração e salvamento de resumos
      
      
      
      caminho1.resumo<-paste0(getwd(),"/Resumos.Iterações/","1seg.","parte ",i,"-iter.",contagem.iteracoes,".csv")
      
      capture.output(print(df.1seg), file = caminho1.resumo)
      
      #Transformação dos codas em data frames
      
      
      df.1seg<-as.data.frame(df.1seg)
      list.dfs<-list(df.1seg,df.2seg,df.3seg)
      
      
      
      # DIC5<-dic.calc(df.5seg,5,med.filtro,t0,tf)
      # DIC4<-dic.calc(df.4seg,4,med.filtro,t0,tf)
      
      
      DIC1<-dic.calc(df.1seg,1,med.filtro,t0,tf)
      
      #list.dics<-c(DIC1,DIC2,DIC3,DIC4,DIC5)
      list.dics<-c(DIC1,DIC2,DIC3)
      list.dics.relat<-list.dics/min(list.dics)
      list.dics.auxiliar<-list.dics
      for(div in 3:2){
        dif<-abs(list.dics.relat[div]-list.dics.relat[div-1])
        list.dics.auxiliar[div]<-ifelse(dif<=tolerancia.residuo,10^9,list.dics.auxiliar[div])
      }
      list.min.dics<-c(list.min.dics,which.min(list.dics.auxiliar))
      capture.output(list.dics,file=paste0(getwd(),"/Resumos.Iterações/","lista dics part. ",i,"- iteração",contagem.iteracoes,".csv"))
      capture.output(list.dics.auxiliar,file=paste0(getwd(),"/Resumos.Iterações/","lista dics considered part. ",i,"- iteração",contagem.iteracoes,".csv"))
      if(which.min(list.dics.auxiliar)>1){
        df.referencia<-list.dfs[[which.min(list.dics.auxiliar)]]
        
        mis.referencia<-df.referencia[1:(which.min(list.dics.auxiliar))]
        taus.referencia<-df.referencia[, (which.min(list.dics.auxiliar) + 1):(2 * which.min(list.dics.auxiliar) - 1)]
        list.df.referencia<-append(list.df.referencia,df.referencia)#list(list.df.referencia,taus.referencia)
        capture.output(df.referencia,file=paste0(getwd(),"/Resumos.Iterações/","post taus part. ",i,"- iteração",contagem.iteracoes,".csv"))
        capture.output(list.df.referencia,file=paste0(getwd(),"/Resumos.Iterações/","acumum dfs refer. ",i,"- iteração",contagem.iteracoes,".csv"))
        #função para o caso em que o número de taus é maior que 1
        calculate_density1 <- function(column) {
          dens <- density(column, na.rm = TRUE)
          return(list(x = dens$x, y = dens$y))
        }
        
        #função para o caso em que o número de taus é igual a 1
        calculate_density2 <- function(vetor){
          dens<-density(vetor,na.rm = TRUE)
          return(list(x = dens$x, y = dens$y))
        }
        
        n.taus<-(2 * which.min(list.dics.auxiliar) - 1)-(which.min(list.dics.auxiliar) + 1)+1
        
        if((which.min(list.dics.auxiliar) + 1)==(2 * which.min(list.dics.auxiliar) - 1)){
          densities.taus<-calculate_density2(taus.referencia)
          
        }else{
          densities.taus<-apply(taus.referencia, 2, calculate_density1)
        }
        
        
        taus.ref.max.posts<-NULL
        taus.ref.inf<-NULL
        taus.ref.sup<-NULL
        if(n.taus>1){
          for(j in 1:n.taus){
            taus.ref.max.posts[j]<-densities.taus[[j]]$x[densities.taus[[j]]$y==max(densities.taus[[j]]$y)]
            taus.ref.inf[j]<-as.numeric(quantile(taus.referencia[,j],probs=c(0.025)))
            taus.ref.sup[j]<-as.numeric(quantile(taus.referencia[,j],probs=c(0.975)))
          }      
        }else{
          taus.ref.max.posts<-densities.taus$x[densities.taus$y==max(densities.taus$y)]
          taus.ref.inf<-as.numeric(quantile(taus.referencia,probs=c(0.025)))
          taus.ref.sup<-as.numeric(quantile(taus.referencia,probs=c(0.975)))
        }
        df.taus.ref<-data.frame(taus.ref.max.posts,taus.ref.inf,taus.ref.sup)
        # for(t in 1:length(taus.ref.max.posts)){
        #   p<-p + geom_vline(xintercept = taus.ref.max.posts[t],linetype="dashed",color="blue")+
        #           geom_rect(aes(xmin=taus.ref.inf[t],xmax=taus.ref.sup[t],ymin=-Inf,ymax=Inf),
        #                     fill="blue",alpha=0.01)
        # }
        for(t in 1:length(taus.ref.max.posts)){
          p <- p + 
            geom_vline(xintercept = taus.ref.max.posts[t], linetype = "dashed", color = "blue")
        }
        
        
        
        taus.auxiliar<-c(t0,taus.ref.max.posts,tf)
        mi.sup<-vector("numeric",length=0)
        mi.inf<-vector("numeric",length=0)
        mi.med<-vector("numeric",length=0)
        for(m in 1:length(mis.referencia)){
          mi.sup<-append(mi.sup,as.numeric(quantile(mis.referencia[,m],probs=c(0.975))))
          mi.inf<-append(mi.inf,as.numeric(quantile(mis.referencia[,m],probs=c(0.025))))
          mi.med<-append(mi.med,as.numeric(quantile(mis.referencia[,m],probs=c(0.500))))
        }
        
        #Adicionando ao data frame das meds filtradas as colunas de período de validade médio,
        #resíduo médio (mi.med), resíduo sup (mi.sup) e resíduo inf (mi.inf)
        med.filtro<-med.filtro %>% mutate(per.med=findInterval(dias,taus.auxiliar))
        med.filtro<-med.filtro %>% mutate(res.med=mi.med[per.med],
                                          res.sup=mi.sup[per.med],
                                          res.inf=mi.inf[per.med])
        #Adicionando ao gráfico p as ribbons referentes aos IC's da variável mi
        p<-p + geom_ribbon(data=med.filtro,aes(ymin=res.inf,ymax=res.sup,group=per.med),fill="red",alpha=0.3)+
          geom_line(data=med.filtro,aes(y=res.med,group=per.med),color="red",linetype="dashed")
        
        
        
        lista.flutuante<-sort(c(unlist(lista.flutuante),taus.ref.max.posts))
        #print(p)
        caminho.residuos<-paste0(getwd(),"/Grafs.Iterações/","Residuos","parte ",i,"- iter",contagem.iteracoes,".png")
        ggsave(caminho.residuos,plot=p,width=15, height=6,dpi=300, bg="white")
      }else{
        mi.sup<-vector("numeric",length=0)
        mi.inf<-vector("numeric",length=0)
        mi.med<-vector("numeric",length=0)
        df.referencia<-list.dfs[[which.min(list.dics.auxiliar)]]
        mis.referencia<-df.referencia[1:(which.min(list.dics.auxiliar))]
        for(m in 1:length(mis.referencia)){
          mi.sup<-append(mi.sup,as.numeric(quantile(df.1seg$`mi[1]`,probs=c(0.975))))
          mi.inf<-append(mi.inf,as.numeric(quantile(df.1seg$`mi[1]`,probs=c(0.025))))
          mi.med<-append(mi.med,as.numeric(quantile(df.1seg$`mi[1]`,probs=c(0.5))))
        }
        med.filtro$per.med<-1
        med.filtro<-med.filtro %>% mutate(res.med=mi.med[per.med],
                                          res.sup=mi.sup[per.med],
                                          res.inf=mi.inf[per.med])
        p<-p + geom_ribbon(data=med.filtro,aes(ymin=res.inf,ymax=res.sup,group=per.med),fill="red",alpha=0.3)+
          geom_segment(x = t0, 
                       xend = tf, 
                       y = max.post(df.1seg[,1]), 
                       yend = max.post(df.1seg[,1]),
                       color = "red", linetype = "dashed")
        
        caminho.residuos<-paste0(getwd(),"/Grafs.Iterações/","Res. sem quebra ","parte ",i,"- iter",contagem.iteracoes,".png")
        ggsave(caminho.residuos,plot=p,width=15, height=6,dpi=300, bg="white")
      }
    }
  }
  capture.output(list.min.dics,file=paste0(getwd(),"/Resumos.Iterações/","lista min dics","- iteração",contagem.iteracoes,".csv"))
  #teste.parada<-any(list.min.dics !=1)
  teste.parada<-contagem.iteracoes<n.max.iteracoes
}

save.image("24.02.2025.RData")
load("24.02.2025.RData")

#Retrocálculo
Cotas <- read_excel("memoria.xlsx", sheet = "Cotagrama")
Q.retrocalc <- numeric(length(Cotas$Data.Hora)) # Inicialização correta
Q.retrocalc.sup<-numeric(length(Cotas$Data.Hora)) # Inicialização correta
Q.retrocalc.inf<-numeric(length(Cotas$Data.Hora)) # Inicialização correta
for (i in 1:length(Cotas$Data.Hora)) {
  data <- ifelse(Cotas$Data.Hora[i] < min(Med.consist.SPD$Data), min(Med.consist.SPD$Data), Cotas$Data.Hora[i])
  
  idx_data <- max(1, findInterval(data, Med.consist.SPD$Data)) # Evita índice 0
  validade <- Med.consist.SPD$Per.Validade[idx_data]
  
  if (!is.na(validade) && validade > 0) {
    vazoes.curva <- df.resultados[, validade * 6 - 1]
    vazoes.lim.sup<-df.resultados[,validade * 6]
    vazoes.lim.inf<-df.resultados[,validade * 6 - 2]
  } else {
    vazoes.curva <- rep(NA, length(df.resultados$CCH1_Cotas)) # Para evitar erro
    vazoes.lim.sup<-rep(NA, length(df.resultados$CCH1_Cotas)) # Para evitar erro
    vazoes.lim.inf<-rep(NA, length(df.resultados$CCH1_Cotas)) # Para evitar erro
  }
  
  cotas.curva <- df.resultados$CCH1_Cotas
  idx <- findInterval(Cotas$Cotas.Modelo[i], cotas.curva)
  idx <- min(idx + 1, length(vazoes.curva)) # Garante que o índice esteja no intervalo
  
  Q.retrocalc[i] <- vazoes.curva[idx]
  Q.retrocalc.sup[i] <- vazoes.lim.sup[idx]
  Q.retrocalc.inf[i] <- vazoes.lim.inf[idx]
}
Cotas<-Cotas %>% mutate(Q.median.retroc = Q.retrocalc, Q.sup.retrocalc = Q.retrocalc.sup,
                        Q.inf.retrocalc = Q.retrocalc.inf)

View(Cotas) # Garante visualização correta
plot(Cotas$Cotas.Modelo,Cotas$Q.median.retroc,type="p")

#Salvando o retrocálculo numa nova aba
wb<-loadWorkbook(memoria)

abas_adicionais<-list("Retrocálculo" = Cotas)

for(nome_aba in names(abas_adicionais)){
  addWorksheet(wb,nome_aba)
  writeData(wb,nome_aba,abas_adicionais[[nome_aba]])  
}

saveWorkbook(wb, memoria, overwrite = TRUE)




