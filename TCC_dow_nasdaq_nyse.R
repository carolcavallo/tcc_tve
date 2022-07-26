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

#Remover nota��o cient�fica 
options(scipen=999)

#Limpando o ambiente 
rm(list=ls(all=TRUE))

#------------------------------PREPARANDO OS DADOS---------------------------------#

# DOW JONES
getSymbols(Symbols = "^DJI", src = "yahoo", from = "2015-01-09", to = "2019-12-31")

class(DJI) #xts zoo 
head(DJI) #09/01/2015
tail(DJI) #30/12/2019
dim(DJI) #1252 observa��es 
DJI <- na.omit(DJI)
dim(DJI) #1252 observa��es

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
dim(IXIC) #1252 observa��es 
IXIC <- na.omit(IXIC)
dim(IXIC) #1252 observa��es 

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
dim(NYA) #1252 observa��es 
NYA <- na.omit(NYA)
dim(NYA) #1252 observa��es 

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
dim(dados) #1252 observa��es 
dados <- na.omit(dados)
dim(dados) #1252 observa��es

# juntando os gr�ficos
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

#Amostra de avalia��o
teste <- Dow_Jones["2019-01-01::2019-12-31"]
length(teste) #251
class(teste)

#Amostra de estima��o
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

#M�ximos 

#block <- 5
#data <- dlz
#nblocks <- (length(data)%/%block) + 1
#grouping <- rep(1:nblocks, rep(block, nblocks))[1:length(data)]
##data <- tapply(data, grouping, max)
#data <- as.numeric(data)
#gfit <- gum.fit(data)

#Gr�fico 
#gum.diag(gfit)

#Par�metros 
#mu <- gfit$mle[1] ; mu
#sc <- gfit$mle[2] ; sc

#Teste de Adequa��o a Distribui��o Gumbel 

#Dsn <- function(x) {
#  n <- length(x)
#  xo <- sort(x)
#  yplus <- 1:n/n-exp(-exp(-(xo-mu)/sc))
#  yminus <- exp(-exp(-(xo-mu)/sc))-0:(n-1)/n
#  sqrt(n)*max(max(yplus), max(yminus))
#} 
#round(Dsn(data),3)

#0.657 (menor que 0.874) - Passa com 5% (Blocos de tamanho 5) 

#M�nimos 

block <- 5
data <- -dlz
nblocks <- (length(data)%/%block) + 1
grouping <- rep(1:nblocks, rep(block, nblocks))[1:length(data)]
data <- tapply(data, grouping, max)
data <- as.numeric(data)
gfit <- gum.fit(data)

#Gr�fico 
gum.diag(gfit)
# analise grafica nao esta boa

#Par�metros 
mu <- gfit$mle[1] ; mu
sc <- gfit$mle[2] ; sc

#Teste de Adequa��o a Distribui��o Gumbel 

Dsn <- function(x) {
  n <- length(x)
  xo <- sort(x)
  yplus <- 1:n/n-exp(-exp(-(xo-mu)/sc))
  yminus <- exp(-exp(-(xo-mu)/sc))-0:(n-1)/n
  sqrt(n)*max(max(yplus), max(yminus))
} 
round(Dsn(data),3)

#1.387 (maior que 0.874) - n�o passa com 5% (Bloco de tamanho 5)


#-----------------------------------------------------------#
#                   Considerando xi != 0                    #
#                             GEV                           #
#-----------------------------------------------------------#

#M�nimos 

block <- 5
data <- -dlz
nblocks <- (length(data)%/%block) + 1
grouping <- rep(1:nblocks, rep(block, nblocks))[1:length(data)]
data <- tapply(data, grouping, max)
data <- as.numeric(data)
dfit <- gev.fit(data)

#Gr�fico 
gev.diag(dfit)
# analise grafica ok

####################################Gr�ficos separados!!!!
# pp
a <- dfit$mle
dat <- dfit$data
plot((1:length(dat))/length(dat), gevf(a, sort(dat)), xlab = "Emp�rico",
     ylab = "Modelo", main = "")
abline(0, 1, col = 4)

# qq
dat <- dfit$data
a <- dfit$mle
plot(gevq(a, 1 - (1:length(dat)/(length(dat) + 1))), sort(dat),
     ylab = "Emp�rico", xlab = "Modelo", main = "")
abline(0, 1, col = 4)

# rl
a <- dfit$mle
mat <- dfit$cov
dat <- dfit$data
eps <- 1e-06
a1 <- a
a2 <- a
a3 <- a
a1[1] <- a[1] + eps
a2[2] <- a[2] + eps
a3[3] <- a[3] + eps
f <- c(seq(0.01, 0.09, by = 0.01), 0.1, 0.2, 0.3, 0.4, 0.5, 
       0.6, 0.7, 0.8, 0.9, 0.95, 0.99, 0.995, 0.999)
q <- gevq(a, 1 - f)
d <- t(gev.rl.gradient(a = a, p = 1 - f))
v <- apply(d, 1, q.form, m = mat)
plot(-1/log(f), q, log = "x", type = "n", xlim = c(0.1, 1000), 
     ylim = c(min(dat, q), max(dat, q)), xlab = "Per�odo de retorno", 
     ylab = "N�vel de retorno")
lines(-1/log(f), q)
lines(-1/log(f), q + 1.96 * sqrt(v), col = 4)
lines(-1/log(f), q - 1.96 * sqrt(v), col = 4)
points(-1/log((1:length(dat))/(length(dat) + 1)), sort(dat))

# his
a <- dfit$mle
dat <- dfit$data
h <- hist(dat, plot = FALSE)
if (a[3] < 0) {
  x <- seq(min(h$breaks), min(max(h$breaks), (a[1] - a[2]/a[3] -
                                                0.001)), length = 100)
} else {
  x <- seq(max(min(h$breaks), (a[1] - a[2]/a[3] + 0.001)),
           max(h$breaks), length = 100)
}
y <- gev.dens(a, x)
hist(dat, freq = FALSE, ylim = c(0, max(max(h$density), max(y))),
     xlab = "z", ylab = "f(z)", main = "")
points(dat, rep(0, length(dat)))
lines(x, y)
####################################Gr�ficos separados!!!!

#Par�metros 
mu <- dfit$mle[1] ; mu 
sc <- dfit$mle[2] ; sc
xi <- dfit$mle[3] ; xi 

#Analisando o gr�fico (COLES)
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

#Amostra de avalia��o
teste <- Nasdaq["2019-01-01::2019-12-31"]
length(teste) #251
class(teste)

#Amostra de estima��o
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

#M�ximos 

#block <- 5
#data <- dlz
#nblocks <- (length(data)%/%block) + 1
#grouping <- rep(1:nblocks, rep(block, nblocks))[1:length(data)]
##data <- tapply(data, grouping, max)
#data <- as.numeric(data)
#gfit <- gum.fit(data)

#Gr�fico 
#gum.diag(gfit)

#Par�metros 
#mu <- gfit$mle[1] ; mu
#sc <- gfit$mle[2] ; sc

#Teste de Adequa��o a Distribui��o Gumbel 

#Dsn <- function(x) {
#  n <- length(x)
#  xo <- sort(x)
#  yplus <- 1:n/n-exp(-exp(-(xo-mu)/sc))
#  yminus <- exp(-exp(-(xo-mu)/sc))-0:(n-1)/n
#  sqrt(n)*max(max(yplus), max(yminus))
#} 
#round(Dsn(data),3)

#0.657 (menor que 0.874) - Passa com 5% (Blocos de tamanho 5) 

#M�nimos 

block <- 5
data <- -dlz
nblocks <- (length(data)%/%block) + 1
grouping <- rep(1:nblocks, rep(block, nblocks))[1:length(data)]
data <- tapply(data, grouping, max)
data <- as.numeric(data)
gfit <- gum.fit(data)

#Gr�fico 
gum.diag(gfit)
# analise grafica nao esta boa

#Par�metros 
mu <- gfit$mle[1] ; mu
sc <- gfit$mle[2] ; sc

#Teste de Adequa��o a Distribui��o Gumbel 

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

#M�nimos 

block <- 5
data <- -dlz
nblocks <- (length(data)%/%block) + 1
grouping <- rep(1:nblocks, rep(block, nblocks))[1:length(data)]
data <- tapply(data, grouping, max)
data <- as.numeric(data)
dfit <- gev.fit(data)

#Gr�fico 
gev.diag(dfit)
# analise grafica esta ok

####################################Gr�ficos separados!!!!
# pp
a <- dfit$mle
dat <- dfit$data
plot((1:length(dat))/length(dat), gevf(a, sort(dat)), xlab = "Emp�rico",
     ylab = "Modelo", main = "")
abline(0, 1, col = 4)

# qq
dat <- dfit$data
a <- dfit$mle
plot(gevq(a, 1 - (1:length(dat)/(length(dat) + 1))), sort(dat),
     ylab = "Emp�rico", xlab = "Modelo", main = "")
abline(0, 1, col = 4)

# rl
a <- dfit$mle
mat <- dfit$cov
dat <- dfit$data
eps <- 1e-06
a1 <- a
a2 <- a
a3 <- a
a1[1] <- a[1] + eps
a2[2] <- a[2] + eps
a3[3] <- a[3] + eps
f <- c(seq(0.01, 0.09, by = 0.01), 0.1, 0.2, 0.3, 0.4, 0.5, 
       0.6, 0.7, 0.8, 0.9, 0.95, 0.99, 0.995, 0.999)
q <- gevq(a, 1 - f)
d <- t(gev.rl.gradient(a = a, p = 1 - f))
v <- apply(d, 1, q.form, m = mat)
plot(-1/log(f), q, log = "x", type = "n", xlim = c(0.1, 1000), 
     ylim = c(min(dat, q), max(dat, q)), xlab = "Per�odo de retorno", 
     ylab = "N�vel de retorno")
lines(-1/log(f), q)
lines(-1/log(f), q + 1.96 * sqrt(v), col = 4)
lines(-1/log(f), q - 1.96 * sqrt(v), col = 4)
points(-1/log((1:length(dat))/(length(dat) + 1)), sort(dat))

# his
a <- dfit$mle
dat <- dfit$data
h <- hist(dat, plot = FALSE)
if (a[3] < 0) {
  x <- seq(min(h$breaks), min(max(h$breaks), (a[1] - a[2]/a[3] -
                                                0.001)), length = 100)
} else {
  x <- seq(max(min(h$breaks), (a[1] - a[2]/a[3] + 0.001)),
           max(h$breaks), length = 100)
}
y <- gev.dens(a, x)
hist(dat, freq = FALSE, ylim = c(0, max(max(h$density), max(y))),
     xlab = "z", ylab = "f(z)", main = "")
points(dat, rep(0, length(dat)))
lines(x, y)
####################################Gr�ficos separados!!!!

#Par�metros 
mu <- dfit$mle[1] ; mu 
sc <- dfit$mle[2] ; sc
xi <- dfit$mle[3] ; xi 

#Analisando o gr�fico (COLES)
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

#Amostra de avalia��o
teste <- Nyse["2019-01-01::2019-12-31"]
length(teste) #251
class(teste)

#Amostra de estima��o
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

#M�ximos 

#block <- 5
#data <- dlz
#nblocks <- (length(data)%/%block) + 1
#grouping <- rep(1:nblocks, rep(block, nblocks))[1:length(data)]
##data <- tapply(data, grouping, max)
#data <- as.numeric(data)
#gfit <- gum.fit(data)

#Gr�fico 
#gum.diag(gfit)

#Par�metros 
#mu <- gfit$mle[1] ; mu
#sc <- gfit$mle[2] ; sc

#Teste de Adequa��o a Distribui��o Gumbel 

#Dsn <- function(x) {
#  n <- length(x)
#  xo <- sort(x)
#  yplus <- 1:n/n-exp(-exp(-(xo-mu)/sc))
#  yminus <- exp(-exp(-(xo-mu)/sc))-0:(n-1)/n
#  sqrt(n)*max(max(yplus), max(yminus))
#} 
#round(Dsn(data),3)

#0.657 (menor que 0.874) - Passa com 5% (Blocos de tamanho 5) 

#M�nimos 

block <- 5
data <- -dlz
nblocks <- (length(data)%/%block) + 1
grouping <- rep(1:nblocks, rep(block, nblocks))[1:length(data)]
data <- tapply(data, grouping, max)
data <- as.numeric(data)
gfit <- gum.fit(data)

#Gr�fico 
gum.diag(gfit)
# analise grafica nao esta boa

#Par�metros 
mu <- gfit$mle[1] ; mu
sc <- gfit$mle[2] ; sc

#Teste de Adequa��o a Distribui��o Gumbel 

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

#M�nimos 

block <- 5
data <- -dlz
nblocks <- (length(data)%/%block) + 1
grouping <- rep(1:nblocks, rep(block, nblocks))[1:length(data)]
data <- tapply(data, grouping, max)
data <- as.numeric(data)
dfit <- gev.fit(data)

#Gr�fico 
gev.diag(dfit)
# analise grafica esta melhor

####################################Gr�ficos separados!!!!
# pp
a <- dfit$mle
dat <- dfit$data
plot((1:length(dat))/length(dat), gevf(a, sort(dat)), xlab = "Emp�rico",
     ylab = "Modelo", main = "")
abline(0, 1, col = 4)

# qq
dat <- dfit$data
a <- dfit$mle
plot(gevq(a, 1 - (1:length(dat)/(length(dat) + 1))), sort(dat),
     ylab = "Emp�rico", xlab = "Modelo", main = "")
abline(0, 1, col = 4)

# rl
a <- dfit$mle
mat <- dfit$cov
dat <- dfit$data
eps <- 1e-06
a1 <- a
a2 <- a
a3 <- a
a1[1] <- a[1] + eps
a2[2] <- a[2] + eps
a3[3] <- a[3] + eps
f <- c(seq(0.01, 0.09, by = 0.01), 0.1, 0.2, 0.3, 0.4, 0.5, 
       0.6, 0.7, 0.8, 0.9, 0.95, 0.99, 0.995, 0.999)
q <- gevq(a, 1 - f)
d <- t(gev.rl.gradient(a = a, p = 1 - f))
v <- apply(d, 1, q.form, m = mat)
plot(-1/log(f), q, log = "x", type = "n", xlim = c(0.1, 1000), 
     ylim = c(min(dat, q), max(dat, q)), xlab = "Per�odo de retorno", 
     ylab = "N�vel de retorno")
lines(-1/log(f), q)
lines(-1/log(f), q + 1.96 * sqrt(v), col = 4)
lines(-1/log(f), q - 1.96 * sqrt(v), col = 4)
points(-1/log((1:length(dat))/(length(dat) + 1)), sort(dat))

# his
a <- dfit$mle
dat <- dfit$data
h <- hist(dat, plot = FALSE)
if (a[3] < 0) {
  x <- seq(min(h$breaks), min(max(h$breaks), (a[1] - a[2]/a[3] -
                                                0.001)), length = 100)
} else {
  x <- seq(max(min(h$breaks), (a[1] - a[2]/a[3] + 0.001)),
           max(h$breaks), length = 100)
}
y <- gev.dens(a, x)
hist(dat, freq = FALSE, ylim = c(0, max(max(h$density), max(y))),
     xlab = "z", ylab = "f(z)", main = "")
points(dat, rep(0, length(dat)))
lines(x, y)
####################################Gr�ficos separados!!!!

#Par�metros 
mu <- dfit$mle[1] ; mu 
sc <- dfit$mle[2] ; sc
xi <- dfit$mle[3] ; xi 

#Analisando o gr�fico (COLES)
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
#                          CITA��O                            #
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
dim(BTC) #1932 observa��es 
BTC <- na.omit(BTC)
dim(BTC) #1932 observa��es

BTC <- BTC$`BTC-USD.Adjusted`
plot(BTC, col="darkblue")


#Amostra de avalia��o
teste <- BTC["2019-04-25::2019-12-31"]
length(teste) #251
class(teste)

#Amostra de estima��o
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

#M�ximos 

#block <- 5
#data <- dlz
#nblocks <- (length(data)%/%block) + 1
#grouping <- rep(1:nblocks, rep(block, nblocks))[1:length(data)]
##data <- tapply(data, grouping, max)
#data <- as.numeric(data)
#gfit <- gum.fit(data)

#Gr�fico 
#gum.diag(gfit)

#Par�metros 
#mu <- gfit$mle[1] ; mu
#sc <- gfit$mle[2] ; sc

#Teste de Adequa��o a Distribui��o Gumbel 

#Dsn <- function(x) {
#  n <- length(x)
#  xo <- sort(x)
#  yplus <- 1:n/n-exp(-exp(-(xo-mu)/sc))
#  yminus <- exp(-exp(-(xo-mu)/sc))-0:(n-1)/n
#  sqrt(n)*max(max(yplus), max(yminus))
#} 
#round(Dsn(data),3)

#0.657 (menor que 0.874) - Passa com 5% (Blocos de tamanho 5) 

#M�nimos 

block <- 5
data <- -dlz
nblocks <- (length(data)%/%block) + 1
grouping <- rep(1:nblocks, rep(block, nblocks))[1:length(data)]
data <- tapply(data, grouping, max)
data <- as.numeric(data)
gfit <- gum.fit(data)

#Gr�fico 
gum.diag(gfit)
# analise grafica nao esta boa

#Par�metros 
mu <- gfit$mle[1] ; mu
sc <- gfit$mle[2] ; sc

#Teste de Adequa��o a Distribui��o Gumbel 

Dsn <- function(x) {
  n <- length(x)
  xo <- sort(x)
  yplus <- 1:n/n-exp(-exp(-(xo-mu)/sc))
  yminus <- exp(-exp(-(xo-mu)/sc))-0:(n-1)/n
  sqrt(n)*max(max(yplus), max(yminus))
} 
round(Dsn(data),3)

#1.379 (maior que 0.874) - n�o passa com 5% (Bloco de tamanho 5)


#-----------------------------------------------------------#
#                   Considerando xi != 0                    #
#                             GEV                           #
#-----------------------------------------------------------#

#M�nimos 

block <- 5
data <- -dlz
nblocks <- (length(data)%/%block) + 1
grouping <- rep(1:nblocks, rep(block, nblocks))[1:length(data)]
data <- tapply(data, grouping, max)
data <- as.numeric(data)
dfit <- gev.fit(data)

#Gr�fico 
gev.diag(dfit)
# analise grafica ok

#Par�metros 
mu <- dfit$mle[1] ; mu 
sc <- dfit$mle[2] ; sc
xi <- dfit$mle[3] ; xi 

#Analisando o gr�fico (COLES)
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










