rm(list=ls())         # Remover objetos del environment
cat("\014")           # Limpiar la Consola

Pkg <- c("base","fBasics","fPortfolio","grid","httr","lubridate","PerformanceAnalytics",
         "plyr","quantmod","xts","zoo","quadprog","quantmod","Quandl","ggplot2",
         "timeDate")

inst <- Pkg %in% installed.packages()
if(length(Pkg[!inst]) > 0) install.packages(Pkg[!inst])
instpackages <- lapply(Pkg, library, character.only=TRUE)


portafolio1 <-  read.csv("C:/Users/USER/Documents/iteso/8vo semestre/pap1/RendMensualesBra.csv",header=TRUE)
row.names(portafolio1)<-portafolio1[,1]
portafolio1<-portafolio1[,2:length(portafolio1[1,])]

portafolio <-  read.csv("C:/Users/USER/Documents/iteso/8vo semestre/pap1/OPIIFPrecios2.csv",header=TRUE)
row.names(portafolio)<-portafolio[,1]
time<-as.Date(portafolio[,1])
portafolio<-portafolio[,2:length(portafolio[1,])]
#para sacar las ventanas de tiempo 
mes<-fortify.zoo(time)
mes<-mes[,2]
mes<-format(mes,format="%m")

ano<-261
t<-1
porta<-c()
for (i in 2:length(mes)){
  if (mes[i-1]!= mes[i]){
    if(i<27){
      
    }else{
    t<-t+1
    }
  }
  porta[i]<-sum(portafolio[i,]*portafolio1[t,])
}
TasaDeCrecimiento1<-matrix(0,1,774)
TasaDeCrecimiento1[1,1:2]<-1
for(i in 3:length(mes)){
  if (mes[i-1]!= mes[i]){
    TasaDeCrecimiento1[1,i]<-TasaDeCrecimiento1[1,i-1]
  }else{
    TasaDeCrecimiento1[1,i]<-TasaDeCrecimiento1[1,i-1]*(porta[i]/porta[i-1])
  }
}

#PERFORMANCE RATIOS DE OMRVMXA.MX DE OLD MUTUAL

# Obtencion de precios 
FInicial <- "2013-03-01"
FFinal   <- "2016-04-10"

# Acomodo de todos los datos necesarios 
# Tasa Libre de Riesgo (Cetes) ANUAL
# BenchMark (IPC)
Bm <- Cl(get(getSymbols(Symbols="^MXX", env=.GlobalEnv, from=FInicial, to=FFinal)))
IndexBm <- (fortify.zoo(Bm))[,1]
Bm <- fortify.zoo(to.monthly(Bm))
Bm <- data.frame(Bm[-1,1], round(diff(log(Bm[,5])),4))
Bm<-Bm[1:35,]
colnames(Bm) <- c("Index","Bm")

Rf <- Quandl("BDM/SF282", trim_start=FInicial, order = "asc")
colnames(Rf) <- c("Index","Rf")
IndexRf  <- Rf$Index
Rf$Index <- format(Rf$Index, format="%b %Y")
Rf[,2] <- round((Rf[,2]/36000)*30,4)
Rf[,1]<-Bm[,1]
Rf<-Rf[2:36,]
colnames(Rf) <- c("Index","Rf")


# Acomodo de todos los datos necesarios 
# Tasa Libre de Riesgo (Cetes) ANUAL
# Fondo de inversion ITESO
activos1<-c("HBCN.MX")
getSymbols.yahoo(Symbols = activos1,env=.GlobalEnv,from=FInicial,to=FFinal)
#matriz del portafolio con precios de cierre
portafolioNA <- do.call(merge, lapply(activos1, function(x) Cl(get(x))))
portafolio1 <- na.omit(portafolioNA)
porta2<-portafolio1[1:length(TasaDeCrecimiento1),1]
porta2[,1]<-TasaDeCrecimiento1

Fd <- porta2
IndexFd <- IndexBm[1:length(TasaDeCrecimiento1)]
Fd <- fortify.zoo(to.monthly(Fd))
Fd <- data.frame(Fd[-1,1], round(diff(log(Fd[,2])),4))
colnames(Fd) <- c("Index","Fd")




Df.Datos  <- join_all(list(Rf,Bm,Fd), by = 'Index', type = 'full')
Df.Datos  <- Df.Datos[,-1]
Xts.Datos <- as.xts(Df.Datos, order.by = unique(IndexRf[2:37],IndexBm,IndexFd)[-1])

rm(list = "MXX","OMRVMXA.MX","Bm","Fd","Rf")

# En DF.Datos estan todos los datos necesarios. Tipo cuadro de datos
# En Xts.Datos estan todos los datos necearios. Tipo serie de tiempo

#Renombrar valores
Xts.Datos<-na.omit(Xts.Datos)
rf<-Xts.Datos[,1]
xtsRendIPC<-Xts.Datos[,2]
xtsRendFondo1<-Xts.Datos[,3]
stdevf<-stdev(xtsRendFondo1)

#Medidas de desempeño
#Ratio de Jensen
AJFondo1 <- CAPM.jensenAlpha(Xts.Datos[,3],Xts.Datos[,2], Rf=Xts.Datos[,1])
Jensen1<-mean(as.matrix(AJFondo1))
#Ratio de Sharpe, de Informacion y Omega
Sh<-0
for (i in 1:length(Xts.Datos[,1])){
  Sh[i]<-((xtsRendFondo1[i]-rf)/stdevf)
}
Sharpe1<-mean(Sh)
Sortino1 <- SortinoRatio(xtsRendFondo1,rf, scale=12)
Rinf1  <- InformationRatio(xtsRendFondo1,xtsRendIPC, scale=12)
RatioInformacion1<-Rinf1 
Omega1 <- Omega(xtsRendFondo1,xtsRendIPC, Rf=rf)
Msq1 <- MSquared(xtsRendFondo1, xtsRendIPC, Rf = Xts.Datos[1,1])

RatiosItesoPC <-c(Jensen1, Sharpe1, Sortino1, RatioInformacion1, Omega1, Msq1)

