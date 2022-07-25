#-------------------------------------------------------------#
#-------------------------------------------------------------#
#            TVE - Bolsas de Valores Americanas               #
#-------------------------------------------------------------#
#-------------------------------------------------------------#

#Pacotes 

library(quantmod)
library(xts)
library(tseries)
library(ismev)
library(PerformanceAnalytics)

#Remover notação científica 
options(scipen=999)

#Limpando o ambiente 
rm(list=ls(all=TRUE))

#------------------------------PREPARANDO OS DADOS---------------------------------#

# DOW JONES
getSymbols(Symbols = "^DJI", src = "yahoo", from = "2015-01-09", to = "2019-12-31")

class(DJI) #xts zoo 
head(DJI) #09/01/2015
tail(DJI) #30/12/2019
dim(DJI) #1252 observações 
DJI <- na.omit(DJI)
dim(DJI) #1252 observações

DJI <- DJI$DJI.Adjusted
plot(DJI, col="darkblue", main = "Dow Jones")
ret_dow = diff(log(DJI$DJI.Adjusted))
a = plot(ts(ret_dow$DJI.Adjusted), col="darkblue", xlab = "Tempo (em dias)",
     ylab = "")
DJI$norm_dow = DJI$DJI.Adjusted/as.vector(DJI$DJI.Adjusted[1]) *100

# NASDAQ
getSymbols(Symbols = "^IXIC", src = "yahoo", from = "2015-01-09", to = "2019-12-31")

class(IXIC) #xts zoo 
head(IXIC) #09/01/2015
tail(IXIC) #30/12/2019
dim(IXIC) #1252 observações 
IXIC <- na.omit(IXIC)
dim(IXIC) #1252 observações 

IXIC <- IXIC$IXIC.Adjusted
plot(IXIC, col="darkblue", main = "NASDAQ")
ret_nasdaq = diff(log(IXIC$IXIC.Adjusted))
b = plot(ts(ret_nasdaq$IXIC.Adjusted), col="darkblue", xlab = "Tempo (em dias)",
     ylab = "")
IXIC$norm_nasdaq = IXIC$IXIC.Adjusted/as.vector(IXIC$IXIC.Adjusted[1]) *100

# NYSE
getSymbols(Symbols = "^NYA", src = "yahoo", from = "2015-01-09", to = "2019-12-31")

class(NYA) #xts zoo 
head(NYA) #09/01/2015
tail(NYA) #30/12/2019
dim(NYA) #1252 observações 
NYA <- na.omit(NYA)
dim(NYA) #1252 observações 

NYA <- NYA$NYA.Adjusted
plot(NYA, col="darkblue", main = "NYSE")
ret_nyse = diff(log(NYA$NYA.Adjusted))
c = plot(ts(ret_nyse$NYA.Adjusted), col="darkblue", xlab = "Tempo (em dias)",
         ylab = "")
NYA$norm_nyse = NYA$NYA.Adjusted/as.vector(NYA$NYA.Adjusted[1]) *100

dados = merge.xts(DJI, NYA, IXIC)
class(dados) #xts zoo 
head(dados) #09/01/2015
tail(dados) #30/12/2019
dim(dados) #1252 observações 
dados <- na.omit(dados)
dim(dados) #1252 observações

# juntando os gráficos
plot(ts(dados$IXIC.Adjusted), main="", xlab="Tempo (em dias)",
     lwd=2, col="darkblue", ylab="", ylim=c(1000,30000))
lines(ts(dados$DJI.Adjusted), col="red", lwd=2)
lines(ts(dados$NYA.Adjusted), col="darkgreen", lwd=2)
legend("topleft", c("NASDAQ", "Dow Jones","NYSE"), 
       col=c("darkblue","red","darkgreen" ), lwd=2, cex=0.8)

# series normalizadas por 100
plot(ts(dados$norm_nasdaq), main="", xlab="Tempo (em dias)",
     lwd=2, col="darkblue", ylab="")
lines(ts(dados$norm_dow), col="red", lwd=2)
lines(ts(dados$norm_nyse), col="darkgreen", lwd=2)
legend("topleft", c("NASDAQ", "Dow Jones","NYSE"), 
       col=c("darkblue","red","darkgreen" ), lwd=2, cex=0.8)


#-------------------------------DOW JONES------------------------------------------#
Dow_Jones = dados$DJI.Adjusted

#Amostra de avaliação
teste <- Dow_Jones["2019-01-01::2019-12-31"]
length(teste) #251
class(teste)

#Amostra de estimação
treinamento <- Dow_Jones["2015-01-09::2019-01-01"]
length(treinamento) #1001
class(treinamento)

#------------------------------------------------------------#
#------------------------------------------------------------#

#Teste i.i.d 

z <- as.vector(treinamento)

dlz <- diff(log(z))
for (i in 1:8) {
  l <- Box.test(dlz,lag=i, type="Ljung-Box")
  print(round(l$p.value,3))
}

#------------------------------------------------------------#
#                   Considerando xi = 0                      #
#                         Gumbel                             #
#------------------------------------------------------------#

#Máximos 

#block <- 5
#data <- dlz
#nblocks <- (length(data)%/%block) + 1
#grouping <- rep(1:nblocks, rep(block, nblocks))[1:length(data)]
##data <- tapply(data, grouping, max)
#data <- as.numeric(data)
#gfit <- gum.fit(data)

#Gráfico 
#gum.diag(gfit)

#Parâmetros 
#mu <- gfit$mle[1] ; mu
#sc <- gfit$mle[2] ; sc

#Teste de Adequação a Distribuição Gumbel 

#Dsn <- function(x) {
#  n <- length(x)
#  xo <- sort(x)
#  yplus <- 1:n/n-exp(-exp(-(xo-mu)/sc))
#  yminus <- exp(-exp(-(xo-mu)/sc))-0:(n-1)/n
#  sqrt(n)*max(max(yplus), max(yminus))
#} 
#round(Dsn(data),3)

#0.657 (menor que 0.874) - Passa com 5% (Blocos de tamanho 5) 

#Mínimos 

block <- 5
data <- -dlz
nblocks <- (length(data)%/%block) + 1
grouping <- rep(1:nblocks, rep(block, nblocks))[1:length(data)]
data <- tapply(data, grouping, max)
data <- as.numeric(data)
gfit <- gum.fit(data)

#Gráfico 
gum.diag(gfit)
# analise grafica nao esta boa

#Parâmetros 
mu <- gfit$mle[1] ; mu
sc <- gfit$mle[2] ; sc

#Teste de Adequação a Distribuição Gumbel 

Dsn <- function(x) {
  n <- length(x)
  xo <- sort(x)
  yplus <- 1:n/n-exp(-exp(-(xo-mu)/sc))
  yminus <- exp(-exp(-(xo-mu)/sc))-0:(n-1)/n
  sqrt(n)*max(max(yplus), max(yminus))
} 
round(Dsn(data),3)

#1.387 (maior que 0.874) - não passa com 5% (Bloco de tamanho 5)


#-----------------------------------------------------------#
#                   Considerando xi != 0                    #
#                             GEV                           #
#-----------------------------------------------------------#

#Mínimos 

block <- 5
data <- -dlz
nblocks <- (length(data)%/%block) + 1
grouping <- rep(1:nblocks, rep(block, nblocks))[1:length(data)]
data <- tapply(data, grouping, max)
data <- as.numeric(data)
dfit <- gev.fit(data)

#Gráfico 
gev.diag(dfit)
# analise grafica ok

#Parâmetros 
mu <- dfit$mle[1] ; mu 
sc <- dfit$mle[2] ; sc
xi <- dfit$mle[3] ; xi 

#Analisando o gráfico (COLES)
#Blocos de tamanho 5 - Passa 

#------------------------------------------------------------#
#------------------------------------------------------------#

#Teste de Kupiec 

#Para Gumbel (Xi = 0)

#ea <-c(7,1,0,0,0)
#eb <-c(19,6,4,1,0)

#test <- as.vector(teste)
#dltest <- diff(log(test))
#f <- function(p) {
#  py <- (1-p)^block
#  iVaR <- -(mu - sc*log(-log(py)))
#  sum(dltest < iVaR)
#}
#fl <- c(f(0.05), f(0.01), f(0.005), f(0.001), f(0.0001))
#df <- data.frame(fl=fl,
#                 ea=ea, eb=eb, logic= (ea<=fl & fl <=eb))
#rownames(df) <- c("5%","1%","0.5%", "0.1%", "0.01%")
#print(df)

#Para GEV (Xi != 0)

ea <-c(7,1,0,0,0)
eb <-c(19,6,4,1,0)

test <- as.vector(teste)
dltest <- diff(log(test))

#### VaR por extremos
f <- function(p) {
  py <- (1-p)^block
  iVaR <- -(mu + sc*((-log(py))^(-xi)-1)/xi)
  #xi!=0
  sum(dltest < iVaR)
}
fl <- c(f(0.05), f(0.01), f(0.005), f(0.001), f(0.0001))
df <- data.frame(fl=fl,
                 ea=ea, eb=eb, logic= (ea<=fl & fl <=eb))
rownames(df) <- c("5%","1%","0.5%", "0.1%", "0.01%")
print(df)
# PASSOU

### calcular o VaR normal e historico (performance analytics)

########## VAR HISTORICO
f <- function(d) {
  alfa = 1-d
  iVaR <- VaR(dlz, p = alfa, method = "historical")
  iVaR = as.vector(iVaR)
  #xi!=0
  sum(dltest < iVaR)
}
fl <- c(f(0.05), f(0.01), f(0.005), f(0.001), f(0.0001))
df <- data.frame(fl=fl,
                 ea=ea, eb=eb, logic= (ea<=fl & fl <=eb))
rownames(df) <- c("5%","1%","0.5%", "0.1%", "0.01%")
print(df)
# PASSOU

####### VAR NORMAL
f <- function(d) {
  alfa = 1-d
  iVaR <- VaR(dlz, p = alfa, method = "gaussian")
  #iVaR <- qnorm(d)*sd(dlz) + mean(dlz)
  iVaR = as.vector(iVaR)
  #xi!=0
  sum(dltest < iVaR)
}
fl <- c(f(0.05), f(0.01), f(0.005), f(0.001), f(0.0001))
df <- data.frame(fl=fl,
                 ea=ea, eb=eb, logic= (ea<=fl & fl <=eb))
rownames(df) <- c("5%","1%","0.5%", "0.1%", "0.01%")
print(df)
# nao PASSOU para 0.5% e 0.1%




#-------------------------------NASDAQ------------------------------------------#
Nasdaq = dados$IXIC.Adjusted

#Amostra de avaliação
teste <- Nasdaq["2019-01-01::2019-12-31"]
length(teste) #251
class(teste)

#Amostra de estimação
treinamento <- Nasdaq["2015-01-09::2019-01-01"]
length(treinamento) #1001
class(treinamento)

#------------------------------------------------------------#
#------------------------------------------------------------#

#Teste i.i.d 

z <- as.vector(treinamento)

dlz <- diff(log(z))
for (i in 1:8) {
  l <- Box.test(dlz,lag=i, type="Ljung-Box")
  print(round(l$p.value,3))
}

#------------------------------------------------------------#
#                   Considerando xi = 0                      #
#                         Gumbel                             #
#------------------------------------------------------------#

#Máximos 

#block <- 5
#data <- dlz
#nblocks <- (length(data)%/%block) + 1
#grouping <- rep(1:nblocks, rep(block, nblocks))[1:length(data)]
##data <- tapply(data, grouping, max)
#data <- as.numeric(data)
#gfit <- gum.fit(data)

#Gráfico 
#gum.diag(gfit)

#Parâmetros 
#mu <- gfit$mle[1] ; mu
#sc <- gfit$mle[2] ; sc

#Teste de Adequação a Distribuição Gumbel 

#Dsn <- function(x) {
#  n <- length(x)
#  xo <- sort(x)
#  yplus <- 1:n/n-exp(-exp(-(xo-mu)/sc))
#  yminus <- exp(-exp(-(xo-mu)/sc))-0:(n-1)/n
#  sqrt(n)*max(max(yplus), max(yminus))
#} 
#round(Dsn(data),3)

#0.657 (menor que 0.874) - Passa com 5% (Blocos de tamanho 5) 

#Mínimos 

block <- 5
data <- -dlz
nblocks <- (length(data)%/%block) + 1
grouping <- rep(1:nblocks, rep(block, nblocks))[1:length(data)]
data <- tapply(data, grouping, max)
data <- as.numeric(data)
gfit <- gum.fit(data)

#Gráfico 
gum.diag(gfit)
# analise grafica nao esta boa

#Parâmetros 
mu <- gfit$mle[1] ; mu
sc <- gfit$mle[2] ; sc

#Teste de Adequação a Distribuição Gumbel 

Dsn <- function(x) {
  n <- length(x)
  xo <- sort(x)
  yplus <- 1:n/n-exp(-exp(-(xo-mu)/sc))
  yminus <- exp(-exp(-(xo-mu)/sc))-0:(n-1)/n
  sqrt(n)*max(max(yplus), max(yminus))
} 
round(Dsn(data),3)

#1.085 (maior que 0.874) - nao passa com 5% (Bloco de tamanho 5) 

#-----------------------------------------------------------#
#                   Considerando xi != 0                    #
#                             GEV                           #
#-----------------------------------------------------------#

#Mínimos 

block <- 5
data <- -dlz
nblocks <- (length(data)%/%block) + 1
grouping <- rep(1:nblocks, rep(block, nblocks))[1:length(data)]
data <- tapply(data, grouping, max)
data <- as.numeric(data)
dfit <- gev.fit(data)

#Gráfico 
gev.diag(dfit)
# analise grafica esta ok

#Parâmetros 
mu <- dfit$mle[1] ; mu 
sc <- dfit$mle[2] ; sc
xi <- dfit$mle[3] ; xi 

#Analisando o gráfico (COLES)
#Blocos de tamanho 5 - passa 

#------------------------------------------------------------#
#------------------------------------------------------------#

#Teste de Kupiec 

#Para Gumbel (Xi = 0)

#ea <-c(7,1,0,0,0)
#eb <-c(19,6,4,1,0)

#test <- as.vector(teste)
#dltest <- diff(log(test))
#f <- function(p) {
#  py <- (1-p)^block
#  iVaR <- -(mu - sc*log(-log(py)))
#  sum(dltest < iVaR)
#}
#fl <- c(f(0.05), f(0.01), f(0.005), f(0.001), f(0.0001))
#df <- data.frame(fl=fl,
#                 ea=ea, eb=eb, logic= (ea<=fl & fl <=eb))
#rownames(df) <- c("5%","1%","0.5%", "0.1%", "0.01%")
#print(df)
# nao PASSOU para 1% 

#Para GEV (Xi != 0)

ea <-c(7,1,0,0,0)
eb <-c(19,6,4,1,0)

test <- as.vector(teste)
dltest <- diff(log(test))

#### VaR por extremos
f <- function(p) {
  py <- (1-p)^block
  iVaR <- -(mu + sc*((-log(py))^(-xi)-1)/xi)
  #xi!=0
  sum(dltest < iVaR)
}
fl <- c(f(0.05), f(0.01), f(0.005), f(0.001), f(0.0001))
df <- data.frame(fl=fl,
                 ea=ea, eb=eb, logic= (ea<=fl & fl <=eb))
rownames(df) <- c("5%","1%","0.5%", "0.1%", "0.01%")
print(df)
# PASSOU

### calcular o VaR normal e historico (performance analytics)

########## VAR HISTORICO
f <- function(d) {
  alfa = 1-d
  iVaR <- VaR(dlz, p = alfa, method = "historical")
  iVaR = as.vector(iVaR)
  #xi!=0
  sum(dltest < iVaR)
}
fl <- c(f(0.05), f(0.01), f(0.005), f(0.001), f(0.0001))
df <- data.frame(fl=fl,
                 ea=ea, eb=eb, logic= (ea<=fl & fl <=eb))
rownames(df) <- c("5%","1%","0.5%", "0.1%", "0.01%")
print(df)
# PASSOU

####### VAR NORMAL
f <- function(d) {
  alfa = 1-d
  iVaR <- VaR(dlz, p = alfa, method = "gaussian")
  iVaR = as.vector(iVaR)
  #xi!=0
  sum(dltest < iVaR)
}
fl <- c(f(0.05), f(0.01), f(0.005), f(0.001), f(0.0001))
df <- data.frame(fl=fl,
                 ea=ea, eb=eb, logic= (ea<=fl & fl <=eb))
rownames(df) <- c("5%","1%","0.5%", "0.1%", "0.01%")
print(df)
# nao PASSOU para 0.5% e 0.1%



#-------------------------------------NYSE--------------------------------------------#
Nyse = dados$NYA.Adjusted

#Amostra de avaliação
teste <- Nyse["2019-01-01::2019-12-31"]
length(teste) #251
class(teste)

#Amostra de estimação
treinamento <- Nyse["2015-01-09::2019-01-01"]
length(treinamento) #1001
class(treinamento)

#------------------------------------------------------------#
#------------------------------------------------------------#

#Teste i.i.d 

z <- as.vector(treinamento)

dlz <- diff(log(z))
for (i in 1:8) {
  l <- Box.test(dlz,lag=i, type="Ljung-Box")
  print(round(l$p.value,3))
}

#------------------------------------------------------------#
#                   Considerando xi = 0                      #
#                         Gumbel                             #
#------------------------------------------------------------#

#Máximos 

#block <- 5
#data <- dlz
#nblocks <- (length(data)%/%block) + 1
#grouping <- rep(1:nblocks, rep(block, nblocks))[1:length(data)]
##data <- tapply(data, grouping, max)
#data <- as.numeric(data)
#gfit <- gum.fit(data)

#Gráfico 
#gum.diag(gfit)

#Parâmetros 
#mu <- gfit$mle[1] ; mu
#sc <- gfit$mle[2] ; sc

#Teste de Adequação a Distribuição Gumbel 

#Dsn <- function(x) {
#  n <- length(x)
#  xo <- sort(x)
#  yplus <- 1:n/n-exp(-exp(-(xo-mu)/sc))
#  yminus <- exp(-exp(-(xo-mu)/sc))-0:(n-1)/n
#  sqrt(n)*max(max(yplus), max(yminus))
#} 
#round(Dsn(data),3)

#0.657 (menor que 0.874) - Passa com 5% (Blocos de tamanho 5) 

#Mínimos 

block <- 5
data <- -dlz
nblocks <- (length(data)%/%block) + 1
grouping <- rep(1:nblocks, rep(block, nblocks))[1:length(data)]
data <- tapply(data, grouping, max)
data <- as.numeric(data)
gfit <- gum.fit(data)

#Gráfico 
gum.diag(gfit)
# analise grafica nao esta boa

#Parâmetros 
mu <- gfit$mle[1] ; mu
sc <- gfit$mle[2] ; sc

#Teste de Adequação a Distribuição Gumbel 

Dsn <- function(x) {
  n <- length(x)
  xo <- sort(x)
  yplus <- 1:n/n-exp(-exp(-(xo-mu)/sc))
  yminus <- exp(-exp(-(xo-mu)/sc))-0:(n-1)/n
  sqrt(n)*max(max(yplus), max(yminus))
} 
round(Dsn(data),3)

#1.175 (maior que 0.874) - nao passa com 5% (Bloco de tamanho 5)


#-----------------------------------------------------------#
#                   Considerando xi != 0                    #
#                             GEV                           #
#-----------------------------------------------------------#

#Mínimos 

block <- 5
data <- -dlz
nblocks <- (length(data)%/%block) + 1
grouping <- rep(1:nblocks, rep(block, nblocks))[1:length(data)]
data <- tapply(data, grouping, max)
data <- as.numeric(data)
dfit <- gev.fit(data)

#Gráfico 
gev.diag(dfit)
# analise grafica esta melhor

#Parâmetros 
mu <- dfit$mle[1] ; mu 
sc <- dfit$mle[2] ; sc
xi <- dfit$mle[3] ; xi 

#Analisando o gráfico (COLES)
#Blocos de tamanho 5 - Passa 

#------------------------------------------------------------#
#------------------------------------------------------------#

#Teste de Kupiec 

#Para Gumbel (Xi = 0)

#ea <-c(7,1,0,0,0)
#eb <-c(19,6,4,1,0)

#test <- as.vector(teste)
#dltest <- diff(log(test))
#f <- function(p) {
#  py <- (1-p)^block
#  iVaR <- -(mu - sc*log(-log(py)))
#  sum(dltest < iVaR)
#}
#fl <- c(f(0.05), f(0.01), f(0.005), f(0.001), f(0.0001))
#df <- data.frame(fl=fl,
#                 ea=ea, eb=eb, logic= (ea<=fl & fl <=eb))
#rownames(df) <- c("5%","1%","0.5%", "0.1%", "0.01%")
#print(df)
# PASSOU 

#Para GEV (Xi != 0)

ea <-c(7,1,0,0,0)
eb <-c(19,6,4,1,0)

test <- as.vector(teste)
dltest <- diff(log(test))

#### VaR por extremos
f <- function(p) {
  py <- (1-p)^block
  iVaR <- -(mu + sc*((-log(py))^(-xi)-1)/xi)
  #xi!=0
  sum(dltest < iVaR)
}
fl <- c(f(0.05), f(0.01), f(0.005), f(0.001), f(0.0001))
df <- data.frame(fl=fl,
                 ea=ea, eb=eb, logic= (ea<=fl & fl <=eb))
rownames(df) <- c("5%","1%","0.5%", "0.1%", "0.01%")
print(df)
# PASSOU

### calcular o VaR normal e historico (performance analytics)

########## VAR HISTORICO
f <- function(d) {
  alfa = 1-d
  iVaR <- VaR(dlz, p = alfa, method = "historical")
  iVaR = as.vector(iVaR)
  #xi!=0
  sum(dltest < iVaR)
}
fl <- c(f(0.05), f(0.01), f(0.005), f(0.001), f(0.0001))
df <- data.frame(fl=fl,
                 ea=ea, eb=eb, logic= (ea<=fl & fl <=eb))
rownames(df) <- c("5%","1%","0.5%", "0.1%", "0.01%")
print(df)
# PASSOU

####### VAR NORMAL
f <- function(d) {
  alfa = 1-d
  iVaR <- VaR(dlz, p = alfa, method = "gaussian")
  iVaR = as.vector(iVaR)
  #xi!=0
  sum(dltest < iVaR)
}
fl <- c(f(0.05), f(0.01), f(0.005), f(0.001), f(0.0001))
df <- data.frame(fl=fl,
                 ea=ea, eb=eb, logic= (ea<=fl & fl <=eb))
rownames(df) <- c("5%","1%","0.5%", "0.1%", "0.01%")
print(df)
# nao PASSOU para 0.1%


#-------------------------------------------------------------#
#-------------------------------------------------------------#
#                          CITAÇÃO                            #
#-------------------------------------------------------------#
#-------------------------------------------------------------#

citation( )
version

RStudio.Version()

citation(package = "quantmod")
citation(package = "xts")
citation(package = "tseries")
citation(package = "ismev")
citation(package = "PerformanceAnalytics")


###########################################################################################
###########################################################################################
###############     DAQUI EM DIANTE NAO FOI USADO PARA O TCC      #########################
###########################################################################################
###########################################################################################

#-------------------------------------------------------------#
#-------------------------------------------------------------#
#            TVE - Criptomoeda (Bitcoin)                      #
#-------------------------------------------------------------#
#-------------------------------------------------------------#

getSymbols(Symbols = "BTC-USD", src = "yahoo", from = "2014-01-01", to = "2019-12-31")

BTC = `BTC-USD`

class(BTC) #xts zoo 
head(BTC) #17/09/2014
tail(BTC) #31/12/2019
dim(BTC) #1932 observações 
BTC <- na.omit(BTC)
dim(BTC) #1932 observações

BTC <- BTC$`BTC-USD.Adjusted`
plot(BTC, col="darkblue")


#Amostra de avaliação
teste <- BTC["2019-04-25::2019-12-31"]
length(teste) #251
class(teste)

#Amostra de estimação
treinamento <- BTC["2016-07-29::2019-04-25"]
length(treinamento) #1001
class(treinamento)

#------------------------------------------------------------#
#------------------------------------------------------------#

#Teste i.i.d 

z <- as.vector(treinamento)

dlz <- diff(log(z))
for (i in 1:8) {
  l <- Box.test(dlz,lag=i, type="Ljung-Box")
  print(round(l$p.value,3))
}

#------------------------------------------------------------#
#                   Considerando xi = 0                      #
#                         Gumbel                             #
#------------------------------------------------------------#

#Máximos 

#block <- 5
#data <- dlz
#nblocks <- (length(data)%/%block) + 1
#grouping <- rep(1:nblocks, rep(block, nblocks))[1:length(data)]
##data <- tapply(data, grouping, max)
#data <- as.numeric(data)
#gfit <- gum.fit(data)

#Gráfico 
#gum.diag(gfit)

#Parâmetros 
#mu <- gfit$mle[1] ; mu
#sc <- gfit$mle[2] ; sc

#Teste de Adequação a Distribuição Gumbel 

#Dsn <- function(x) {
#  n <- length(x)
#  xo <- sort(x)
#  yplus <- 1:n/n-exp(-exp(-(xo-mu)/sc))
#  yminus <- exp(-exp(-(xo-mu)/sc))-0:(n-1)/n
#  sqrt(n)*max(max(yplus), max(yminus))
#} 
#round(Dsn(data),3)

#0.657 (menor que 0.874) - Passa com 5% (Blocos de tamanho 5) 

#Mínimos 

block <- 5
data <- -dlz
nblocks <- (length(data)%/%block) + 1
grouping <- rep(1:nblocks, rep(block, nblocks))[1:length(data)]
data <- tapply(data, grouping, max)
data <- as.numeric(data)
gfit <- gum.fit(data)

#Gráfico 
gum.diag(gfit)
# analise grafica nao esta boa

#Parâmetros 
mu <- gfit$mle[1] ; mu
sc <- gfit$mle[2] ; sc

#Teste de Adequação a Distribuição Gumbel 

Dsn <- function(x) {
  n <- length(x)
  xo <- sort(x)
  yplus <- 1:n/n-exp(-exp(-(xo-mu)/sc))
  yminus <- exp(-exp(-(xo-mu)/sc))-0:(n-1)/n
  sqrt(n)*max(max(yplus), max(yminus))
} 
round(Dsn(data),3)

#1.379 (maior que 0.874) - não passa com 5% (Bloco de tamanho 5)


#-----------------------------------------------------------#
#                   Considerando xi != 0                    #
#                             GEV                           #
#-----------------------------------------------------------#

#Mínimos 

block <- 5
data <- -dlz
nblocks <- (length(data)%/%block) + 1
grouping <- rep(1:nblocks, rep(block, nblocks))[1:length(data)]
data <- tapply(data, grouping, max)
data <- as.numeric(data)
dfit <- gev.fit(data)

#Gráfico 
gev.diag(dfit)
# analise grafica ok

#Parâmetros 
mu <- dfit$mle[1] ; mu 
sc <- dfit$mle[2] ; sc
xi <- dfit$mle[3] ; xi 

#Analisando o gráfico (COLES)
#Blocos de tamanho 5 - Passa 

#------------------------------------------------------------#
#------------------------------------------------------------#

#Teste de Kupiec 

#Para Gumbel (Xi = 0)

#ea <-c(7,1,0,0,0)
#eb <-c(19,6,4,1,0)

#test <- as.vector(teste)
#dltest <- diff(log(test))
#f <- function(p) {
#  py <- (1-p)^block
#  iVaR <- -(mu - sc*log(-log(py)))
#  sum(dltest < iVaR)
#}
#fl <- c(f(0.05), f(0.01), f(0.005), f(0.001), f(0.0001))
#df <- data.frame(fl=fl,
#                 ea=ea, eb=eb, logic= (ea<=fl & fl <=eb))
#rownames(df) <- c("5%","1%","0.5%", "0.1%", "0.01%")
#print(df)

#Para GEV (Xi != 0)

ea <-c(7,1,0,0,0)
eb <-c(19,6,4,1,0)

test <- as.vector(teste)
dltest <- diff(log(test))

#### VaR por extremos
f <- function(p) {
  py <- (1-p)^block
  iVaR <- -(mu + sc*((-log(py))^(-xi)-1)/xi)
  #xi!=0
  sum(dltest < iVaR)
}
fl <- c(f(0.05), f(0.01), f(0.005), f(0.001), f(0.0001))
df <- data.frame(fl=fl,
                 ea=ea, eb=eb, logic= (ea<=fl & fl <=eb))
rownames(df) <- c("5%","1%","0.5%", "0.1%", "0.01%")
print(df)
# PASSOU

### calcular o VaR normal e historico (performance analytics)

########## VAR HISTORICO
f <- function(d) {
  alfa = 1-d
  iVaR <- VaR(dlz, p = alfa, method = "historical")
  iVaR = as.vector(iVaR)
  #xi!=0
  sum(dltest < iVaR)
}
fl <- c(f(0.05), f(0.01), f(0.005), f(0.001), f(0.0001))
df <- data.frame(fl=fl,
                 ea=ea, eb=eb, logic= (ea<=fl & fl <=eb))
rownames(df) <- c("5%","1%","0.5%", "0.1%", "0.01%")
print(df)
# PASSOU

####### VAR NORMAL
f <- function(d) {
  alfa = 1-d
  iVaR <- VaR(dlz, p = alfa, method = "gaussian")
  #iVaR <- qnorm(d)*sd(dlz) + mean(dlz)
  iVaR = as.vector(iVaR)
  #xi!=0
  sum(dltest < iVaR)
}
fl <- c(f(0.05), f(0.01), f(0.005), f(0.001), f(0.0001))
df <- data.frame(fl=fl,
                 ea=ea, eb=eb, logic= (ea<=fl & fl <=eb))
rownames(df) <- c("5%","1%","0.5%", "0.1%", "0.01%")
print(df)
# nao PASSOU para 0.1% e 0.01%










