#PERFORMANCE RATIOS DE OMRVMXA.MX DE OLD MUTUAL



Pkg <- c("base","fBasics","fPortfolio","grid","httr","lubridate","PerformanceAnalytics",
         "plyr","quantmod","xts","zoo","quadprog","quantmod","Quandl","ggplot2",
         "timeDate")

inst <- Pkg %in% installed.packages()
if(length(Pkg[!inst]) > 0) install.packages(Pkg[!inst])
instpackages <- lapply(Pkg, library, character.only=TRUE)

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
colnames(Bm) <- c("Index","Bm")

Rf <- Quandl("BDM/SF282", trim_start=FInicial, order = "asc")
colnames(Rf) <- c("Index","Rf")
IndexRf  <- Rf$Index
Rf$Index <- format(Rf$Index, format="%b %Y")
Rf[,2] <- round((Rf[,2]/36000)*30,4)
Rf[,1]<-Bm[,1]
colnames(Rf) <- c("Index","Rf")

# Fondo de inversion OldMutual
Fd <- Cl(get(getSymbols(Symbols="OMRVMXA.MX", env=.GlobalEnv, from=FInicial, to=FFinal)))
IndexFd <- (fortify.zoo(Fd))[,1]
Fd <- fortify.zoo(to.monthly(Fd))
Fd <- data.frame(Fd[-1,1], round(diff(log(Fd[,5])),4))
colnames(Fd) <- c("Index","Fd")

Df.Datos  <- join_all(list(Rf,Bm,Fd), by = 'Index', type = 'full')
Df.Datos  <- Df.Datos[,-1]
Xts.Datos <- as.xts(Df.Datos, order.by = unique(IndexRf,IndexBm,IndexFd))

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
Jensen<-mean(as.matrix(AJFondo1))
#Ratio de Sharpe, de Informacion y Omega
Sh<-0
for (i in 1:length(Xts.Datos[,1])){
  Sh[i]<-((xtsRendFondo1[i]-rf)/stdevf)
}
Sharpe<-mean(Sh)
Sortino <- SortinoRatio(xtsRendFondo1,rf, scale=12)
Rinf  <- InformationRatio(xtsRendFondo1,xtsRendIPC, scale=12)
RatioInformacion<-Rinf 
Omega <- Omega(xtsRendFondo1,xtsRendIPC, Rf=rf)
Msq <- MSquared(xtsRendFondo1, xtsRendIPC, Rf = Xts.Datos[1,1])

RatiosOM <-c(Jensen, Sharpe, Sortino, RatioInformacion, Omega, Msq)



FInicial <- "2013-03-01"
FFinal   <- "2016-04-10"

#optencion de los precios  
portafolio <-  read.csv("C:/Users/USER/Documents/iteso/8vo semestre/pap1/OPIIFPrecios2.csv",header=TRUE)
row.names(portafolio)<-portafolio[,1]
time<-portafolio[,1]
portafolio<-portafolio[,2:length(portafolio[1,])]

####FInicial <- "2014-01-01"
#####FFinal   <- "2016-02-08"


#para sacar las ventanas de tiempo 
time<-as.Date(time)
#para sacar las ventanas de tiempo 
mes<-fortify.zoo(time)
mes<-mes[,2]
mes<-format(mes,format="%m")

Bm <- Cl(get(getSymbols(Symbols="^MXX", env=.GlobalEnv, from=FInicial, to=FFinal)))
IndexBm <- (fortify.zoo(Bm))[,1]
Bm <- fortify.zoo(to.monthly(Bm))
Bm <- data.frame(Bm[-1,1], round(diff(log(Bm[,5])),4))
colnames(Bm) <- c("Index","Bm")
Rf <- Quandl("BDM/SF282", trim_start=FInicial, order = "asc")
colnames(Rf) <- c("Index","Rf")
IndexRf  <- Rf$Index
Rf$Index <- format(Rf$Index, format="%b %Y")
Rf[,2] <- round((Rf[,2]/36000)*30,4)
Rf[,1]<-Bm[,1]
colnames(Rf) <- c("Index","Rf")
Rf<-Rf[,2]

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
    
    jensen<-(ValoresEsperados-Rf[cont])/Desviacion
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
TasaDeCrecimiento<-c()
TasaDeCrecimiento[1:2]<-1
for(i in 3:length(mes)){
  if (mes[i-1]!= mes[i]){
    TasaDeCrecimiento[i]<-TasaDeCrecimiento[i-1]
  }else{
    TasaDeCrecimiento[i]<-TasaDeCrecimiento[i-1]*(porta[i]/porta[i-1])
  }
}


#PERFORMANCE RATIOS DE OMRVMXA.MX DE OLD MUTUAL

# Obtencion de precios 
FInicial <- "2013-03-01"
FFinal   <- "2016-04-10"

# Acomodo de todos los datos necesarios 
# Tasa Libre de Riesgo (Cetes) ANUAL
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
Rf<-Rf[2:36,]
Rf[,1]<-Bm[,1]
colnames(Rf) <- c("Index","Rf")

# BenchMark (IPC)


# Acomodo de todos los datos necesarios 
# Tasa Libre de Riesgo (Cetes) ANUAL
# Fondo de inversion ITESO
activos1<-c("HBCN.MX")
getSymbols.yahoo(Symbols = activos1,env=.GlobalEnv,from=FInicial,to=FFinal)
#matriz del portafolio con precios de cierre
portafolioNA <- do.call(merge, lapply(activos1, function(x) Cl(get(x))))
portafolio1 <- na.omit(portafolioNA)
porta2<-portafolio1[1:length(TasaDeCrecimiento),1]
porta2[,1]<-TasaDeCrecimiento

Fd <- porta2
IndexFd <- IndexBm[1:length(TasaDeCrecimiento)]
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
Rf<-Rf[2:36,]
Rf[,1]<-Bm[,1]
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

RatiosItesoCLA <-c(Jensen1, Sharpe1, Sortino1, RatioInformacion1, Omega1, Msq1)

ratios<-matrix(0,length(RatiosItesoPC),3)
ratios[,1]<-RatiosOM
ratios[,2]<-RatiosItesoPC
ratios[,3]<-RatiosItesoCLA
colnames(ratios)<-c("OM","PC","CLA")
rownames(ratios)<-c("Jensen", "Sharpe", "Sortino", "RatioInformacion", "Omega", "Msq")
ratios
