
##twierdzenie Gliwienki! 
  set.seed(43);
  x1=rnorm(100,mean=100,sd=16);
  x1
  obj1=ecdf(x1)
  str(obj1)
  plot(ecdf(x1), las=1,ylim=c(0,1.1))
  arg=seq(50,150,0.1)
  wart=pnorm(arg,100,16)
  lines(arg,wart,col=2,lwd=2)
##wykresy pudelkowe
  x2=rchisq(100,df=12)
  hist(x2, breaks=15)
  boxplot(x2,col=5,las=1,boxwex=0.5, main="pudelkowy")
  
  par(mfrow=c(1,2))
##### dwa wykresy pudelkowe
  x2=rchisq(100,df=12)
  x3=rchisq(100,df=12)
  L = list(x2,x3)
  boxplot(L, col=5,las=1,boxwex=0.5)
  
  pudelko=function(n,m)
  {
    L=list()
    for(i in c(1:n))
    {
      L[[i]] = rchisq(m,df=12)
    }
    boxplot(L, col=5,las=1,boxwex=0.5)
  }
  pudelko(10,25)
  ##porownanie wachan mediany i sredniej
  zmiana=function(n,m)
  {
    srednia=numeric()
    mediana=numeric()
    L=list()
    for(i in c(1:n))
    {
      los = rchisq(m,df=12)
      srednia[i]=mean(los)
      mediana[i]=median(los)
    }
    M=cbind(srednia,mediana)
    matplot(M, col=2:3, pch=2:3,type='l', las=1)
    legend("topright",col=2:3,pch=2:3,legend=colnames(M))
  }
  zmiana(100,30)
  ##porownanie wykresow typu QQ
  ##punkty powinny na przekatnej byc
  x1=rnorm(20,100,16)
  x2=rnorm(20,120,16)
  qqplot(x1,x2,las=1,asp=1) ## wykres qq plot
  abline(0,1) ##przektna ktora musi zostac nalozona na qqplot
  
  plot(x1,rep(1,length(x1)),type = "p",ylim=c(0,2), xlim=c(0,200))
  lines(x2,rep(0.5,length(x2)),type = "p", col=2)
  
  plot(qchisq(ppoints(x1),100,16),sort(x1),asp=1,las=1)
  abline(0,1)
  
  plot(qchisq(ppoints(x1),120,16),sort(x1),asp=1,las=1)
  abline(0,1)
  
  par(mfrow=c(2,2)) #zmienna srodowiskowa ile chcemy wykresow po prawej

#####test Kolmogorova, test Kolmgorova-Smirnova, Test Shapiro-Wilka
  
  ##sam kolmogorov - szacujemy czy pochodzi z jakiegos rozkladu
  x=rnorm(20,mean=100,sd=16)
  #szacujemy mean i sd, tu mamy wyliczone, ale trzeba policzyc
  #zeby wiedizec jaki rozklad podstawic...
  
  ks.test(x,"pnorm",110,16)
  plot(ecdf(x),las=1)
  arg=seq(50,150,0.1)
  wart=pnorm(arg,110,16)
  lines(arg,wart,col=2,lwd=2)
  #jezeli p.value mniejsze od 0.05 to mozna odrzucic hipoteze H0


##kolmogorv-smirnov testujemy czy dwie moga pochodzic z jakiegos wspolnego dowolnego
  dane1=rexp(20,1)
  dane2=rexp(15,1)
  ks.test(dane1,dane2)
  plot(ecdf(dane1),las=1)
  lines(ecdf(dane2),las=1, col=2)
  #p<5% dorzucamy hipoteze o wspolnym rozkladzie
  mean(dane1)
  mean(dane2)
  qqplot(dane1,dane2,las=1,asp=1)
  abline
  boxplot(dane1,dane2)
  shapiro.test(x)
  qqnorm(x,las=1)
  qqline(x)

##kolejny test KS - max(F1-F2), mamy p i D to to D to ta roznica najwiesksza dystrybuant
  dane3=rnorm(20,100,16)
  dane4=rnorm(20,110,16)
  ks.test(dane3,dane4)
  qqplot(dane3,dane4)
  abline(0,1)
  mean(dane3)
  mean(dane4)
  abline(0,1)

##kolejny powszechny popularny test Shapiro-Wilka
##tutaj H0 - cecha ma rozklad normalny, tylko to
##jest dobry bo dziala OK juz dla malych próbek (3+)
#zwykle trzeba 20,30,40.....
  dane1=c(-1,0,2,5,10)
  dane1
  shapiro.test(dane1)
  qqnorm(dane1, las=1,asp=1)
  qqline(dane1)
  mean(dane1)
  sd(dane1)
  ##p=0.5347 czyli taka probka byla by w 53% procentach

##kolejny test Shapiro na EXP
  dane2=rexp(100,1)
  hist(dane2)
  shapiro.test(dane2)
  qqnorm(dane2)
  qqline(dane2)
  
  ks.test(dane2,"pnorm",1,1)

##test Chi kwadrat
#tez jest popularny bo wiaze sie z podzialem
##probki na elementy
##tutaj h0, czy dana ceca ma rozklad zadany, lub dystrybuante lub funkcje gestosci
##h1 = ~h0
##porownuje licznosci w przedzialach

  kostka=c(6,8,10,8,4,6)
  names(kostka)=c(1:6)
  barplot(kostka/sum(kostka),col=2,las=1,ylim=c(0,0.3))
  chisq.test(kostka)
  abline(h=1/6,lwd=2)
  
  ##h0, czy ta liczba goli ma rozklad płasona
  ##liczba goli, Poisson, lambda=2
  #staramy sie laczyc w klasy po 5
  gole=c(0:7)
  mecze=c(14,18,29,18,10,7,3,1)
  names(mecze)=gole
  ilosc=sum(mecze)
  mecze[6]=11 #klasa 5,6,7 dodana razem
  mecze = mecze[-8]
  mecze = mecze[-7]
  names(mecze)[6]=">=5"
  barplot(mecze/ilosc, col=2, las=1,ylim=c(0,0.3))
  
  #srednia w rozkladzie passon = x lambda
  #czyli srednia wazona
  mecze=c(14,18,29,18,10,7,3,1)
  names(mecze)=gole
  lan=weighted.mean(gole, w=mecze)
  lan
  #tutaj zakladamy ze lambda = 2, ale to sfalszowane zawsze
  #powinnismy hipoteze zrobic przed badaniem, ale tutaj zalozmy
  #ze to byla nasza poczatkowa hipoteza zalozona, przed badaniem
  arg=c(0:4)
  wart=dpois(arg,lambda=2)
  wart[6] = ppois(4,lambda=2,lower.tail = F)
  wart
  sum(wart)
  mecze[6]=11 #klasa 5,6,7 dodana razem
  mecze = mecze[-8]
  mecze = mecze[-7]
  names(mecze)[6]=">=5"
  mac=rbind(mecze/sum(mecze),wart)
  barplot(mac,beside=T,col=c(2,4),space=c(0.2,1),
          las=1,ylim=c(0,0.3))
  chisq.test(mecze,p=wart)
  wart

#planety #plotrix - piramid == wykresy piramidowe
  planets=c("MERCURY","VENUS","EARTH","MARS","JUPITER"
            ,"SATURN","URANUS","NEPTUNE")
  planets
  diam=c(0.38,0.94,1,0.53,11.2,9.45,4.00,3.88)
  mass=c(0.0552,0.815,1,0.1074,317.8,95.1620,14.5360,17.1470)
  library(plotrix)
  pyramid.plot(mass,diam,labels=plaents,
               top.labels=c("mass","plaent","diameter"),
               main="The solar System",
               space=0.2,gap=100,labelcex=0.9,laxlab=c(0,400),
               show.values=T,unit="",ndig=2)
