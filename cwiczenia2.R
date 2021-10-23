# ps; z ksiag ksiecia polkrwi: uzywaj ctrl+alt+p jak zmieniasz tylko wartosci w wywolaniu funkcji
#
#
#
#
#cwiczenie 0
k1 <- sample(c(1:6), 100, replace = T)
k1wykres = table(k1)
k1wykres
wykres = barplot(k1wykres, col = 2, las = 1, ylim=c(0,30), main = "wykres1")
str(wykres)
text(wykres, max(k1wykres + 3), labels = k1wykres)

k2 <- sample(c(1:6), 100, replace = T)
k2wykres = table(k2)
k2wykres
c = barplot(k2wykres, col = 2, las = 1, ylim=c(0,30), main = "wykres2")
str(c)
text(wykres, max(k2wykres + 3), labels = k2wykres)

M=rbind(k1wykres,k2wykres)

wykres = barplot(M,col=c(2,3),las=1,ylim=c(0,30), main="wykres 3", beside=T, space=c(0.2,1), names.arg = c(1:6))
str(M)
text(wykres, M + 2, labels = M)

#
#
#
#
#
#
#cwiczenie 1
#czy granie w lotto tymi samymi numerami jest lepsze niz ciagle skreslanie nowych numerow
length(intersect(c(0,1),c(0,2))) # test co zwraca length dla intersecta, matcha, zero czy jeden
choose(35, 5)

lottoLosowe <- function(n){
  kuponLosowy = sample(c(1:35), 5 ,replace = F)
  wygrane = c(0,0,0,0,0,0)
  for( i in c(1:n) ){
    los = sample(c(1:35), 5 ,replace = F)
    trafione = length(intersect(kupon,los))
    wygrane[trafione + 1] = wygrane[trafione + 1] + 1
  }
  wygraneSochastyczne = wygrane/n
  zaokragloneWynikiProcentowe=round(100*wygraneSochastyczne, 1)
  results = rbind(zaokragloneWynikiProcentowe)
  colnames(results) = (c(0:5))
  rownames(results) = c("%")
  tytul = paste("Liczba losowan = ", n)
  wykres=barplot(wygraneSochastyczne, col=2,las=1,ylim=c(0,1),names=c(0:5),main = tytul)
  text(wykres, wygraneSochastyczne+0.2,labels=zaokragloneWynikiProcentowe)
  return (results)
}

lotto <- function(kupon, n){
  wygrane = c(0,0,0,0,0,0)
  for( i in c(1:n) ){
    los = sample(c(1:35), 5 ,replace = F)
    trafione = length(intersect(kupon,los))
    wygrane[trafione + 1] = wygrane[trafione + 1] + 1
  }
  wygraneSochastyczne = wygrane/n
  zaokragloneWynikiProcentowe=round(100*wygraneSochastyczne, 1)
  results = rbind(zaokragloneWynikiProcentowe)
  colnames(results) = (c(0:5))
  rownames(results) = c("%")
  tytul = paste("Liczba losowan = ", n)
  wykres=barplot(wygraneSochastyczne, col=2,las=1,ylim=c(0,1),names=c(0:5),main = tytul)
  text(wykres, wygraneSochastyczne+0.2,labels=zaokragloneWynikiProcentowe)
  return (results)
}



kupon=c(1,2,5,10,15)
liczbaLosowan = 10
lottoLosowe <- lottoLosowe(liczbaLosowan)
lottoZKuponem <- lotto(kupon, liczbaLosowan)
M=rbind(lottoLosowe, lottoZKuponem)
M

wykres = barplot(M/100,col=c(2,3),las=1,ylim=c(0,1), names = c(0:5),
                 beside=T, space=c(0.5,2), main = paste("Ilosc losowan = ", liczbaLosowan))

text(wykres, M/100 + 0.08, M)

#
#
#
#
#
#cwiczenie 2 prawo wielkich liczb
cummean <- function(v){
  l = length(v)
  u=cumsum(v)
  for( i in c(1:l)){
    u[i] = u[i]/i
  }
  return (u)
}

pwl <- function(n,m){ #n-liczba trajektori, m-ile liczb losujemy
  liczbyLosowe = runif(m,0,1)
  plot(c(1:m), cummean(liczbyLosowe), type = "l", ylim=c(0,1), las=1, cex.axis=0.75)
  if(n>1){
    for(i in c(2:n)){
      liczbyLosowe = runif(m,0,1)
      lines(c(1:m), cummean(liczbyLosowe), type = "l")
    }
  }
}

pwl_Cauchy <- function(n,m){ #n-liczba trajektori, m-ile liczb losujemy
  liczbyLosowe = rcauchy(m,0,1)
  plot(c(1:m), cummean(liczbyLosowe), type = "l", ylim=c(-50,50), las=1, cex.axis=0.75)
  if(n>1){
    for(i in c(2:n)){
      liczbyLosowe = rcauchy(m,0,1)
      lines(c(1:m), cummean(liczbyLosowe), type = "l")
    }
  }
}

v = c(1,2,3,0,0,0)
cummean(v)

pwl(100, 1000)
pwl_Cauchy(10,100)
abline(h=0.5, col=2)
#
#
#
#
#
#cwiczenie 3 - histogramy
liczbyLosoweZRozkladuNormalnego = rnorm(200,mean=100, sd = 16)
liczbyLosoweZRozkladuNormalnego
hist(liczbyLosoweZRozkladuNormalnego)
histogram = hist(liczbyLosoweZRozkladuNormalnego, breaks=20, las=1, xlim=c(0,200), freq = F, ylim=c(0,0.06)) #F- pole bd 1
arg = seq(40,160,0.1)
wartosci = dnorm(arg, mean=100, sd=16)
lines(arg,wartosci,col=2)
str(histogram)
histogram$counts
#
#
#
#
#
#cwiczenie 4 - CTG
ctg <- function(n,m){
  tablicaSum=numeric()
  liczbyLosowe = runif(m,min=0,max=1)
  tablicaSum[1] = sum(liczbyLosowe)
  plot(c(1:m), cumsum(liczbyLosowe), type="l", ylim=c(0,100), las=1, cex.axis=0.75)
  if(n>1){
    for(i in c(2:n)){
      liczbyLosowe=runif(m,0,1)
      tablicaSum[i] = sum(liczbyLosowe)
      lines(c(1:m), cumsum(liczbyLosowe), type="l")
    }
  }
  return (tablicaSum)
}

vec = ctg(300,100)
vec
hist(vec, breaks = 15, freq = F, las = 1 , ylim=c(0,0.2), xlim=c(30,70))
# tutaj wartosc oczekiwana E = 100 * 1/2 = 50
# tutaj wariancja to 100 * 1/2 
arg = seq(30,70,0.1)
m = 50
d_kwadrat = sqrt(100/12)
wartosci = dnorm(arg, m, d_kwadrat)
lines(arg,wartosci,col=2, lwd=2)
#
#
#
#
#
#cwiczenie 5 - ciag Fibonacciego
fib <- function(n){
  u = numeric()
  u[1] = 1
  u[2] = 1
  for(i in c(3:n)){
    u[i] = u[i-1] + u[i-2]
  }
  return (u)
}

cyfra <- function(x,n){
  y = as.character(abs(x))
  d = substr(y,n,n)
  d2 = as.numeric(d)
  return (d2)
}

v1 = fib(100)
v1
v2 = cyfra(v1, 1)
v2
v3 = table(v2)
v3
barplot(v3, las=1, col=2)
#
#
#
#
#
#cwiczenie 6 - modulo
modulo <- function(x,n){
  u=numeric()
  u[1] = x
  for(i in c(2:n)){
    u[i] = (3*u[i-1]) %% 1
  }
  return (u)
}
 v1 = modulo(0.1,300)
 v1
 hist(v1, freq=F, ylim=c(0,1.5))
 plot(c(1:300), v1,type="l")
 #
 #
 #
 #
 #
 #
 #cwiczenie 6 - test chi kwadrat dla rzutu kostka
 wyniki=c(4,6,9,5,11,4,3,4,6,7,9,6)
 wyniki
 names(wyniki)=c(1:12)
 wyniki
 
 barplot(wyniki, ylim=c(0,15), las=1, col = 2)
 abline(h=74/12, col=2)
 chisq.test(wyniki) #jezeli p-value > 5% to nie ma podstaw do odrzucenia hipotezy zero,
 #czyli wyniki MOGA byc uzyskane kostka uczcciwa, czyli JESZCZE nie ma podstaw do odrzucenia hipotezy
