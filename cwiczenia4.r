#install.packages("randtests")

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

y=seria(100,30)
range(y)
y

hist(y,las=1,)
shapiro.test(y)
mean(y)
sd(y)
