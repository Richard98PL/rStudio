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
