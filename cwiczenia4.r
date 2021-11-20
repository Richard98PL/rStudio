#install.packages("randtests") #musisz zaintstalowac R tools z neta, bez kitu
#install.packages("ggplot2")
#install.packages("TeachingDemons") #pobierz z neta i porawej packages->install .zip

modulo <- function(x,n)
{
  u=numeric()
  u[1]=x
  for(i in c(2:n))
  {
    u[i] = (3*u[i-1])%%1
  }
  return (u)
}
v1=modulo(0.4,40)
v1
runs.test(v1, plot=T)

plot(c(1:length(v1)), v1.type='l', las=1)
abline(h=0.5,col=2)

# zadanie 1 - test serii
x1=c(5,17,21,29,33,45,56,66,72,88)
x2=runif(30,0.1)
x3=c(1,10,2,8,3,7,4,6,5,5)

detach(package:lawstat, unload=TRUE)
library(randtests)
runs.test(x1,plot=T)
ob = runs.test(x3,plot=T)
str(ob)
ob$runs
h=runs.test(x1,plot=T)
h
h$runs

#zadanie 2 - test serii
# test serii, symulacja
seria <- function(n,m)
{
  s=numeric()
  for(i in c(1:n)){
    proba=runif(m,0.1)
    h=runs.test(proba)
    s[i] = h$runs
  }
  return (s)
}

y=seria(1000,100)
range(y)
y

hist(y,las=1,breaks=20)
shapiro.test(y)
mean(y)
sd(y)

#zadanie 3 - kruskal.test
typ1=rnorm(40,100,16)
typ2=rnorm(60,100,16)
typ3=rnorm(65,115,16)
L=list(typ1,typ2,typ3)
mean(typ3)
sd(typ3)
boxplot(L,las=1,col=5,boxwex=0.5)
kruskal.test(L)

ob=kruskal.test(L)
str(ob)
typ1

names(typ1)= rep("a", length(typ1))
names(typ2)= rep("b", length(typ2))
names(typ3)= rep("c", length(typ3))

#zadanie 4 - pliki csv  
if(egz){
  
}else{
  egz=read.csv2(file.choose(), head=TRUE, sep = ";")
}
head(egz)
egz

head(egz, n=10)
tail(egz, n=10)
egz$x=NULL
kol=egz$punkty
mean(kol)
mean(egz$punkty)

boxplot(punkty~grupa,data=egz,las=1,col=5,boxwex=0.5)
#boxplot(punkty~plec,data=egz,las=1,col=5,boxwex=0.5)
kruskal.test(punkty~grupa, data=Egz)
tail(egz)
summary(egz)
nrow(egz)

#zadanie 5 - ggplot2
library(ggplot2)
qplot(grupa,punkty,geom=c("boxplot"), data=egz,fill=grupa)
range(egz$punkty)
kruskal.test(punkty~plec,data=egz)

qplot(plec,punkty,geom=c("boxplot"), data=egz,fill=plec)
range(egz$punkty)
kruskal.test(punkty~plec,data=egz)

#zadanie 6 - rangi
typ1=rnorm(40,100,16)
typ2=rnorm(60,100,16)
typ3=rnorm(65,115,16)

names(typ1)= rep("a", length(typ1))
names(typ2)= rep("b", length(typ2))
names(typ3)= rep("c", length(typ3))


razem=c(typ1,typ2,typ3)
razem
rangi=rank(razem)
sort(rangi)

R1=mean(rangi[names(rangi)=="a"])
R1

R2=mean(rangi[names(rangi)=="b"])
R2

R3=mean(rangi[names(rangi)=="c"])
R3

mean(typ3)
sd(typ3)
boxplot(L,las=1,col=5,boxwex=0.5)
kruskal.test(L)

ob=kruskal.test(L)
str(ob)
typ1

#zadanie 7 - niezaleznosc
if(usa){
  
}else{
  usa=read.csv2(file.choose(), head=TRUE, sep = ";")
}
head(usa)
usa

plot(usa$pow,usa$lud,las=1)
lud = usa$lud
pow = usa$pow
cor(pow,lud)

range(pow)
range(lud)
median(lud)

br1=c(0,100,200,2000)
pow1=cut(pow,breaks=br1)
pow1

br2=c(0,5,40)
lud1=cut(lud,breaks=br2)
lud1

USA2=table(pow1,lud1)
USA2
chisq.test(USA2)


abline(v=br1,col=2)
abline(h=br2,col=2)
abline(v=160,col=2)
abline(v=240,col=2)
abline(h=5,col=2)

library(TeachingDemos)
chisq.detail(USA2)
fisher.test(USA2)

#zadanie 8 - włosy i oczy
# wyszlo, ze nie sa niezalezne
ramka = HairEyeColor
ramka

dim(ramka)
ramka
Male=ramka[,,1]
Male

Male
Female=ramka[,,2]
Female
chisq.test(Male)
chisq.detail(Male)

#zadanie 9 - miasta,powierzchnia

if(pol){
  
}else{
  pol=read.csv2(file.choose(), head=TRUE, sep = ";")
}

head(pol,n=10)
pow=pol$pow
lud=pol$lud
plot(pow,lud, xlim=c(0,50))

range(pow)

br1=c(0,25,50,520)
pow1=cut(pow,breaks=br1)

range(lud)
br2=c(0,2.5e+4,5e+4,2e+6)
lud1=cut(lud,breaks=br2)
pol2=table(pow1,lud1)
pol2

chisq.test(pol2)
chisq.detail(pol2)

#zadanie 10, zadanie badawcze, sredni czas pworotu do zera
walk <- function()
{
  s=numeric()
  s[1] = sample(c(-1,1), size=1)
  k=1
  while(s[k]!=0)
  {
    k = k + 1
    s[k]=s[k-1]+sample(c(-1,1),size=1)
  }
  #plot(c(0:k),c(0,s), las=1,type="l")
  #abline(h=0,col=2)
  return(k)
}
walk()

symulacja=numeric()
for(i in c(1:1000)) #nie odpalac tej petli jezeli w funkcji walk jest plot lub abline, bo nie wyrabia pc
{
  symulacja[i]=walk()
}

hist(symulacja,las=1)
range(symulacja)

sym2=table(symulacja)
sym2
barplot(sym2)
mean(symulacja)
boxplot(symulacja,horizontal = T,ylim=c(0,100))
median(symulacja)
symulacja
length(which(symulacja==2))

#zadanie 11 - równość wariancji
x1 = rnorm(30,100, sqrt(100/12))
x1
x2 = runif(25,0,10) ## wariancja (b-a)^2/12 -> a = 0, b = 10 stad u gory sqrt(100/12)
#zobaczymy czy test jest odporny na to ze takie same wariancje, ale inne rozklady
#bo to testy wariancji!
x2

L = list(x1,x2)
boxplot(L,las=1,boxwex=0.5,col=5)
var.test(x1,x2)
var(x1)
var(x2)

hist(x2)
shapiro.test(x2) #shapiro wykazal ze x2 nie jest normalny, robust test. czasem 0.2 czasem 0.02
qqnorm(x2,asp=1,las=1)
qqline(x2)

