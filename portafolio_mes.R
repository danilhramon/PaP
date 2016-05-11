rm(list=ls())         # Remover objetos del environment
cat("\014")           # Limpiar la Consola

Pkg <- c("base","fBasics","fPortfolio","grid","httr","lubridate","PerformanceAnalytics",
         "quantmod","xts","zoo","quadprog","quantmod","ggplot2","timeDate","plyr","Quandl",
         "timeDate","ggplot2")

inst <- Pkg %in% installed.packages()
if(length(Pkg[!inst]) > 0) install.packages(Pkg[!inst])
instpackages <- lapply(Pkg, library, character.only=TRUE)


portafolio <-  read.csv("C:/Users/USER/Documents/iteso/8vo semestre/pap1/OPIIFPrecios2.csv",header=TRUE)
row.names(portafolio)<-portafolio[,1]
time<-portafolio[,1]
portafolio<-portafolio[,2:length(portafolio[1,])]


Rf <- .035

time<-as.Date(time)
#para sacar las ventanas de tiempo 
mes<-fortify.zoo(time)
mes<-mes[,2]
mes<-format(mes,format="%m")


ano<-261
cont<-0
desordenar<-function(vector){
    acomodo<-matrix(runif(length(vector)*2),2,length(vector))
    acomodo[2,]<-vector
    acomodo<-acomodo[,order(acomodo[1,])]
    return(acomodo[2,])
}
LugarVarianzaOptima<-c()
meanP<-c()
VarianzaOptima<-c()
composicionP<-matrix(0,24,length(portafolio[1,]))
jensenp<-c()
maximaponderacion<-.3
minimaponderacion<-.001

for (i in ano:length(mes)){
  if (mes[i-1]!= mes[i]){
    rendimientosln<-matrix(0,ano,length(portafolio[1,]))
    rendimientosln<-diff(as.matrix(log(portafolio[((i+1)-ano):i,])))
    
    por<-10000#nuemro de portafolios
    composicion<-matrix(runif(por*(length(portafolio[1,])),0,maximaponderacion),por,length(portafolio[1,]))
    composicion[,2]<-(1-composicion[,1])*composicion[,2]
    for(i in 3:(length(portafolio[1,])-1)){
      composicion[,i]<-(1-apply(composicion[,(1:(i-1))],1,sum))*composicion[,i]
    }
    composicion[,(length(portafolio[1,]))]<-1-apply(composicion[,1:(length(portafolio[1,])-1)],1,sum)
    composicion<-apply(composicion,1,desordenar)
    composicion<-t(composicion)
    composicion[which(composicion>maximaponderacion)]<-NA
    composicion[which(composicion<minimaponderacion)]<-NA
    composicion<-na.omit(composicion)
    
    ValoresEsperados<- (composicion) %*% apply(rendimientosln,2,mean)*360 
    Desviacion<-sqrt(rowSums(((composicion)%*%cov(rendimientosln)) * (composicion)))*sqrt(360)
    cont<-cont+1
    
    jensen<-(ValoresEsperados-Rf)/Desviacion
    LugarVarianzaOptima[cont]<-which.max(jensen)
    meanP[cont]<-ValoresEsperados[LugarVarianzaOptima[cont]]
    VarianzaOptima[cont]<-Desviacion[LugarVarianzaOptima[cont]]
    composicionP[cont,]<-composicion[LugarVarianzaOptima[cont],]
    jensenp[cont]<-jensen[LugarVarianzaOptima[cont]]
    #plot(Desviacion,ValoresEsperados)
  }
}

l<-0
t<-0
porta<-c()
valorp<-matrix(0,60,23)

for (i in 2:length(mes)){
  if (mes[i-1]!= mes[i]){
    l<-l+1
    if(i<ano){
      valorp[l,1]<-sum(portafolio[i,]*composicionP[1,])
      valorp[l,2]<-mes[i]
      valorp[l,3:(length(composicionP[1,])+2)]<-composicionP[1,]
    }else{
      t<-t+1
      valorp[l,1]<-sum(portafolio[i,]*composicionP[t,])
      valorp[l,2]<-mes[i]
      valorp[l,3:(length(composicionP[1,])+2)]<-composicionP[t,]
    }
  }
  if(i<ano+30){
    porta[i]<-sum(portafolio[i,]*composicionP[1,])
  }else{
    porta[i]<-sum(portafolio[i,]*composicionP[t,])
  }
}
TasaDeCrecimiento<-matrix(0,1,774)
TasaDeCrecimiento[1,1:2]<-1
for(i in 3:length(mes)){
  if (mes[i-1]!= mes[i]){
    TasaDeCrecimiento[1,i]<-TasaDeCrecimiento[1,i-1]
  }else{
    TasaDeCrecimiento[1,i]<-TasaDeCrecimiento[1,i-1]*(porta[i]/porta[i-1])
  }
}
maxjen<-matrix(0,length(meanP),2)
#valorp# valores historicos del portafolio, mensuales.
maxjen[,1]<-meanP*360
maxjen[,2]<-VarianzaOptima*sqrt(360)
#composicionP
#jensenp
#porta
#View(portafolio[,0:2])
plot(t(TasaDeCrecimiento))
m<-matrix(0,length(mes),2)
m[,1]<-mes
m[,2]<-TasaDeCrecimiento
