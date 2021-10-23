#cwiczenie 0
k1 <- sample(c(1:6), 100, replace = T)
k1b = table(k1)
k1b
b = barplot(k1b, col = 2, las = 1, ylim=c(0,30), main = "wykres1")
str(b)
text(b, max(k1b + 3), labels = k1b)

k2 <- sample(c(1:6), 100, replace = T)
k2b = table(k2)
k2b
c = barplot(k2b, col = 2, las = 1, ylim=c(0,30), main = "wykres2")
str(c)
text(b, max(k2b + 3), labels = k2b)

M=rbind(k1b,k2b)

b = barplot(M,col=c(2,3),las=1,ylim=c(0,30), main="wykres 3", beside=T, space=c(0.2,1), names.arg = c(1:6))
str(M)
text(b, M + 2, labels = M)

#cwiczenie 1
#czy granie w lotto tymi samymi numerami jest lepsze niz ciagle skreslanie nowych numerow
choose(35, 5)
lotto <- function(kupon, n){
  wygrane1 = c(0,0,0,0,0,0)
  for( i in c(1:n) ){
    los = sample(c(1:35), 5 ,replace = F)
    trafione = length(intersect(kupon,los)) + 1
    wygrane1[trafione] = wygrane1[trafione] + 1
  }
  wygr1b = wygrane1/n
  num1=round(100*wygr1b, 1)
  results = rbind(num1)
  colnames(results) = (c(0:5))
  rownames(results) = c("%")
  b=barplot(wygr1b, col=2,las=1,ylim=c(0,1),names=c(0:5),main="Lotto")
  text(b, wygr1b+0.2,labels=num1, xdp=T)
  return (results)
}

kupon=c(1,2,5,10,15)
lotto(kupon, 1000)
