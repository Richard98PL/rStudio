A=rnorm(15,100,16)
B=rnorm(20,100,16)
L=list(A,B)
L
boxplot(L,las=1,names=c("A","B"),col=5,boxwex=0.5,horizontal=T)

shapiro.test(A)
shapiro.test(B)

qqnorm(A,las=1)
qqline(A)

qqnorm(B,las=1)
qqline(B)

var(A)
var(B)

var.test(A,B)
mean(A)
mean(B)
t.test(A,B,var.equal = T,alternative=c("less"))

library(lawstat)
levene.test(L,gr)

gr1=rep(1,length(A))
gr2=rep(2,length(B))
gr=c(gr1,gr2)
gr
L2=c(A,B)
levene.test(L2,gr)
L2

pw1 = function(n,m)
{
  p=numeric()
  for(i in c(1:n))
  {
    proba1=rnorm(m,mean=0,sd=5)
    proba2=rnorm(m,mean=0,sd=5)
    h=var.test(proba1,proba2)
    p[i]=h$p.value
  }
  return (p)
}
dane=pw1(100,50)

dane
length(which(dane>0.05))
hist(dane,las=1,freq=F)
abline(h=1,col=2)

wek1=rnorm(10,mean=0,sd=3)
wek1
wek2=rnorm(10,mean=0,sd=3)
h=var.test(wek1,wek2)

pw2=function(n,m)
{
  p1=numeric()
  p2=numeric()
  for(i in c(1:n)){
    proba1=rnorm(m,mean=0,sd=1)
    proba2=rexp(m,rate=1)
    h1=var.test(proba1,proba2)
    p1[i]=h1$p.value
    h2=levene.test(c(proba1,proba2),c(rep(1,length(proba1)),rep(2,length(proba2))))
    p2[i]=h2$p.value
  }
  return (list(p1,p2))
}
dane2=pw2(100,15)

dvar=dane2[[1]]
dvar
dlev=dane2[[2]]

length(which(dvar>0.05))

length(which(dlev>0.05))
par(mfrow=c(1,2))
dev.off()
hist(dvar,las=1,cex.axis=0.8,freq=F)
abline(h=1,col=2)

hist(dlev,las=1,cex.axis=0.8,freq=F)
abline(h=1,col=2)

pw3=function(n,m)
{
  p1=numeric()
  p2=numeric()
  for(i in c(1:n)){
    proba1=rexp(m,rate=1)
    proba2=rexp(m,rate=1)
    h1=var.test(proba1,proba2)
    p1[i]=h1$p.value
    h2=levene.test(c(proba1,proba2),c(rep(1,length(proba1)),rep(2,length(proba2))))
    p2[i]=h2$p.value
  }
  return (list(p1,p2))
}

dane2=pw3(100,15)

dvar=dane2[[1]]
dvar
dlev=dane2[[2]]

length(which(dvar>0.05))

length(which(dlev>0.05))
par(mfrow=c(1,2))
dev.off()
hist(dvar,las=1,cex.axis=0.8,freq=F)
abline(h=1,col=2)

hist(dlev,las=1,cex.axis=0.8,freq=F)
abline(h=1,col=2)

##adanie 5 5 wariacji
dev.off()
tech1=rnorm(20,100,16)
tech2=rnorm(25,100,16)
tech3=rnorm(30,100,16)
tech4=rnorm(40,100,16)
tech5=rnorm(27,100,16)

L=list(tech1,tech2,tech3,tech4,tech5)
boxplot(L, lis=1,col=5)
length(tech5)
var(tech2)
shapiro.test(tech1)

qqnorm(tech4)
qqline(tech4)
var(tech1);var(tech2);var(tech3);var(tech4);var(tech5)
bartlett.test(L)
flinger.test(L)

grupy=c(rep(1,length(tech1)),
        rep(2,length(tech2)),
        rep(3,length(tech3)),
        rep(4,length(tech4)),
        rep(5,length(tech5))
        )
grupy
levene.test(c(tech1,tech2,tech3,tech4,tech5),grupy)

##zadanie 6 - DWIE SREDNIE
dev.off()
pub1=rnorm(15,2000,500)
mean(pub1)
priv=rnorm(12,2300,500)
mean(priv)
shapiro.test(priv)
shapiro.test(pub1)
qqnorm(pub1)
qqline(pub1)

var.test(pub1,priv)
var(pub1)
var(priv)

t.test(priv,pub1,alternative=c("two.sided"))
t.test(priv,pub1,mu=1000,alternative=c("less"))
t.test(priv,pub1,mu=500,alternative=c("greater"))
t.test(priv,pub1,mu=650,alternative=c("less"))
t.test(priv,pub1,mu=700,alternative=c("less"))

mean(priv)-mean(pub1)
boxplot(pub1,priv,horizontal=T,names=c("pub1","priv"),las=1,col=5)

##zadanie 7 - substancja X 100m
przed=runif(20,9.5,10.5)
po=przed-0.1

plot(c(1:length(przed)),przed,las=1,type="b",xlim=c(1,length(przed)))
lines(c(1:length(po)),po,las=1,type="b",col=2)

t.test(po,przed,paired=T,alternative="less")

##zadanie 7 - zarobki przed/po
przed=c(4000,5000,4200,4800,5300,6000,2391,3744,3891)
po=c(4200,4800,4391,4850,5400,6221,3000,4000,4100)

boxplot(przed,po,las=1,horizontal=T,names=c("przed","po"),col=5)
plot(c(1:length(przed)),przed,las=1,type="b",xlim=c(1,length(przed)))
lines(c(1:length(po)),po,las=1,type="b",col=2)

t.test(po,przed,paired=T,alternative="less")

#zadanie - 9 technika, liceum
sukcesy=c(450,517)
proby=c(705,1320)
prop.test(x=sukcesy,n=proby,alternative="greater")
450/705
517/1320

#zadanie 10 - braki
braki=c(12,18,6,8)
proby=c(1000,1000,1000,1000)

prop.test(braki,proby)

pairwise.prop.test(braki,proby)
barplot(braki/proby,las=1,col=2,ylim=c(0,0.2))
mean(braki)
abline(h=mean(braki)/1000)

#zadanie 11 - polio
zachorowania=c(57,142)
liczba=c(200745,200745)

57/200745
142/200745

prop.test(zachorowania,liczba,alternative="less")

#zadanie 12 - symulacja
sym1=function(x,n){
  roznice=numeric()
  for(i in c(1:n)){
    x1=sample(c(0,1),size=n,replace=T,prob=c(0.95,0.05))
    x2=sample(c(0,1),size=n,replace=T,prob=c(0.95,0.05))
    roznice[i]=sum(x1)-sum(x2)
  }
  return (roznice);
}

dane1=sym1(1000,1000)
dane1
range(dane1)
dane1b=table(dane1)
dane1b
hist(dane1,las=1)
barplot(dane1b/sum(dane1b),las=1,col=2)
range(dane1)
shapiro.test(dane1)
qqnorm(dane1)
qqline(dane1)

#zadanie 12 - paradoks urodzin
mfrow = c(1,1)  # 3 rows and 2 columns
dev.off()
unique(c(1,1,1,1,1,1))
duplicated(c(1,1,1,1,2,3,4,5,6))

birth=function(x){
  unik=numeric()
  for(i in c(1:x)){
    x=sample(c(1:365),size=23,replace=T)
    unik[i]=length(unique(x))
  }
  return(unik)
}

dane3=birth(20000)
dane3
y=table(dane3)
hist(dane3)
b = barplot(y/(sum(y)),las=1,col=2,ylim=c(0,0.6))
text(b,y/sum(y)+0.05,y/sum(y))
u=length(unique(x))
unique(c(1,1,2))
#zadanie 13 - wykresy skrzypcowe
library(vioplot)
#zadanie 14 - laktacja
lak=read.csv2(file.choose(),header=TRUE)
rasa=rep(c("Rasa1","Rasa2","Rasa3"),c(length(r1),length(l2),length(l3)))
dane=data.frame(lak,rasa)
head(dane)
model=aov(lak-rasa,data=dane)
summary(model)
t.test(r3,r2,alternative="less")
mean(r3)
library(granova)
dane=data.frame()
