#TP6
rm(list = ls())

f <- function(x, b, mu_1, sd_1){
  xM <- (mu_1-b)/sd_1
  C <- 1/pnorm(xM, mean=0, sd=1)
  return((x>=b)*C*dnorm(x, mean=mu_1, sd=sd_1))
}


tr_norm <- function(n, b, mu_1, sd_1){
  u = runif(n, 0, 1)
  y = rnorm(n, mu_1, sd_1)
  xM <- (mu_1-b)/sd_1
  M = 1/pnorm(xM, mean=0, sd=1)
  simu = y[u < f(y, b, mu_1, sd_1) /  M*dnorm(y, mu_1, sd_1)]
  return(simu)
}

length(tr_norm(10000, 2, 0, 2)) / 10000


tr_norm2 <- function(n, b, lambda, mu, sd){
  u = runif(n, 0, 1)
  y = rexp(n, lambda) + b
  C <- 1/(pnorm((mu-b)/sd, 0, 1)*(sd* sqrt(2*pi)))
  M1 <- exp(lambda*(mu-b) + 0.5*(lambda*sd)**2)/lambda
  M2 <- exp(-0.5*((b-mu)/sd)**2)/lambda
  M = C*((mu + lambda*sd**2 > b)*M1 + (mu + lambda*sd**2 <= b)*M2)
  simu = y[u < f(y, b, mu, sd) / (M*dexp(y-b, lambda))]
  return(list(simu = simu, rate = 1/M*100))
}

lambdaopt = function(b, mu, sd){
  return((b-mu + sqrt((mu-b)**2 + 2*sd**2))/(2*sd**2)) 
}

b = 2
sd = sqrt(2)
mu = 0

lambdaopti = lambdaopt(b, mu, sd)
hist(tr_norm2(100000, b, lambdaopti, mu, sd)$simu, breaks=40, freq=FALSE)
length(tr_norm2(100000, b, lambdaopti, mu, sd)$simu) / 100000 * 100 # taux d'acceptation empirique 
tr_norm2(100000, b, lambdaopti, mu, sd)$rate # taux d'acceptation theorique 


xval2 <- seq(2, 6, by=0.1)
yval2 <- numeric(length(xval2))
for (i in 1:length(xval2)){
  yval2[i] <- f(xval2[i], b, mu, sd)
} 
lines(xval2, yval2, col="red", lwd=3)

n = 10000
es1 = function(n){
  x = runif(n, 0, 5)
  y = rnorm(n, 0, sqrt(2))
  simu = mean(10*sqrt(2*pi)*sin(y**4)*exp(-3*x/2)*(y>2))
  return(simu)
}
es1_simu = es1(n)

es2 = function(n){
  y = rnorm(n, 0, sqrt(2))
  x = rexp(n, 3/2)
  y[y<= 2] = 0
  simu = mean((4/3) * sqrt(pi*2)* sqrt(x+y) * sin(y*4)*(y>2)*(x>0)*(x<5))
  ech = (4/3) * sqrt(pi*2)* sqrt(x+y) * sin(y*4)*(y>2)*(x>0)*(x<5)
  return(list(simu = simu, ech = ech))
}
es2_simu = es2(n)


tr_norm3 <- function(n, b, lambda, mu, sd){
  u = runif(n, 0, 1)
  y = rexp(n, lambda) + b
  C <- 1/(pnorm((mu-b)/sd, 0, 1)*(sd* sqrt(2*pi)))
  M1 <- exp(lambda*(mu-b) + 0.5*(lambda*sd)**2)/lambda
  M2 <- exp(-0.5*((b-mu)/sd)**2)/lambda
  M = C*((mu + lambda*sd**2 > b)*M1 + (mu + lambda*sd**2 <= b)*M2)
  simu = y[u < f(y, b, mu, sd) / (M*dexp(y-b, lambda))]
  
  tot_prop <- n
  while(length(simu) < n) {
    extr <- n - length(simu)
    
    u <- runif(extr)
    y <- rexp(extr, lambda) + b
    
    accept_condit_ext <- u <f(y, b, mu, sd)/(M*dexp(y-b, lambda))
    
    simu <- c(simu, y[accept_condit_ext])
    tot_prop <- tot_prop + extr
  }

  return(list(simu = simu[1:n], rate = 1/M*100))
}

es3 = function(n){
  x = rexp(n, 3/2)
  y = tr_norm3(n, b, lambdaopti, mu, sd)$simu
  simu = mean(2*sqrt(pi*2)*(2/3)* pnorm((mu-b)/sd, 0, 1)*sqrt(x+y) * sin(y*4)*(y>2)*(x>0)*(x<5))
  ech = 2*sqrt(pi*2)*(2/3)* pnorm((mu-b)/sd, 0, 1)*sqrt(x+y) * sin(y*4)*(y>2)*(x>0)*(x<5)
  return(list(simu = simu, ech = ech))
}

es3_simu = es3(n)

library(microbenchmark)
c = microbenchmark(es2(n), es3(n))
c1 = median(c$time[c$expr=="es2(n)"])
c2 = median(c$time[c$expr=="es3(n)"])

v2 = var(es2(n)$ech)
v3 = var(es3(n)$ech)

(v2*c1)/(v3*c2) # es3 est meilleur estimateur (variance permet de pallier le cout de calcul)


#exo 11
rm(list = ls())
n = 10000
es1 = function(n){
  x = rcauchy(n, 0, 1)
  simu = mean(x>50)
  ech = x>50
  return(list(simu= simu, ech = ech))
}

library(actuar)

es2 = function(n){
  x = rpareto1(n,1,50)
  simu= mean(((1/(pi*(1+x**2))) * (x**2/50)))
  ech =((1/(pi*(1+x**2))) * (x**2/50))
  return(list(simu= simu, ech = ech))
}

library(microbenchmark)
c = microbenchmark(es1(n), es2(n))
c1 = median(c$time[c$expr == "es1(n)"])
c2 = median(c$time[c$expr == "es2(n)"])

v1= var(es1(n)$ech)
v2 = var(es2(n)$ech)

v1*c1 / v2*c2 # estimateur 2 est bien meilleur

#exo 12

rm(list = ls())

es4 = function(n){
  y = rexp(n, 1) + 2
  x = rexp(n, 3/s2)
  simu = mean((x<5)*(x>2)*sqrt(x+y)*sin(y**4)*(2/3)*exp(-y**2/4)*(exp(-(x-2))))
  ech = (x<5)*(x>2)*sqrt(x+y)*sin(y**4)*(2/3)*exp(-y**2/4)*(exp(-(x-2)))
  return(list(simu= simu, ech = ech))
}

v1 = var(es3(n)$ech)
v2 = var(es4(n)$ech)

library(microbenchmark)
c = microbenchmark(es3(n), es4(n))
c1 = median(c$time[c$expr == "es3(n)"])
c2 = median(c$time[c$expr == "es4(n)"])


v1 * c1 /(v2*c2) #bien meilleur avec la methode d'echantillon preferentielle

#exo 13

rm(list = ls())
n = 10000
es1 = function(n){
  x = rnorm(n, 0, 1/sqrt(2))
  simu = mean(sqrt(pi)*(x>0)*(x<2))
  return(simu)
}

es1b = function(n){
  x = runif(n, 0, 2)
  simu = mean(2*exp(-x**2))
  return(simu)
}

es2 = function(n){
  x = rnorm(n, 0,1/sqrt(2))
  simu = mean((x>0)*(x<2)*sqrt(pi) + (x<0)*(x>-2)*sqrt(pi)) /2
  return(simu)
}

es2b = function(n){
  x = runif(n, 0, 2)
  simu = mean(2*exp(-x**2) + 2*exp(-(2-x)**2)) /2
  return(simu)
}

#variable de controle

rm(list = ls())
n = 10000

h<-function(x){
  return(2*exp(-x**2))
}

h0<-function(x,N=10){ 
  y<-0
  for (k in 0:N){
    y<-y+((-x*x)**k)/factorial(k)
  }
  return(y)
}

E_h0<-function(N= 10){
  k<-0:N
  return(sum(((-4)**k)/((2*k+1)*factorial(k))))
}

es1 = function(n){
  x = runif(n, 0, 2)
  simu = mean(2*exp(-x**2)-(h0(x) - E_h0(10)))
  return(simu)
}
es1(n)


b<-rep(0, n)
x = runif(n, 0, 2)

for (i in 1:n){
  b[i]<-cov(h0(x[1:i],N=10),h(x[1:i]))/var(h0(x[1:i],N=10))
}

plot(b)
burn = 100
b_star = b[burn]

es2 = function(n){
  x = runif(n, 0, 2)
  x= x[-(1:burn)]
  simu = mean(2*exp(-x**2)-b_star*(h0(x, N = 10) - E_h0(10)))
  return(simu)
}

es2(n)

#Stratification

rm(list = ls())
K<-10

D<-seq(0,2, 0.2)
n = 10000

h<-function(x){
  return(2*exp(-x**2))
}

x<-runif(n,0,2)

F1<-function(x){
  return(2*x)
}

F<-function(x){
  return(x/2)
}

es1 = function(n){
  simu = 0
  for (i in 1:K){
    u<-runif(n/i, 0, 1) #allocation proportionnelle pk = qk
    ux<-F1(F(D[i]) + (F(D[i+1])-F(D[i]))*u) #TD2
    simu<-simu+(1/K)*mean(h(ux))
  }
  return(simu)
}

es2 = function(n){
  simu = 0
  for(i in 1:K){
    u = runif(n/K, 0, 1)
    ux = F1(F(D[i]) + (F(D[i+1])-F(D[i]))*u)
    nu = n*(1/K)
    simu = simu + (1/K)*(1/nu)*sum(h(ux))
  }
  return(simu)
}

es2(n)
es1(n)


#exo 15

rm(list = ls())
n = 100000
es1 = function(n){
  x = rep(0,n)
  for(i in 1:n){
    x[i] = sum(rweibull(rpois(1, 3.7), 0.5, 2))
  }
  simu = mean(x<3)
  return(simu)
}
es1(n)


part = 6
lambda = 3.7
h = function(x){
  return(x<3)
}

rg = 500
Finv = function(u){
  for(i in 1:rg){
    if (u > ppois(i - 1, lambda) && u <= ppois(i, lambda)){
      return(i)
    }
    if(u <= ppois(0, lambda)){
      return(0)
    }
  }
}

es2 = function(n){
  simu = 0
  for (i in 1:part){
    pk = dpois(i, lambda)
    nk = round(n*pk)
    obs = rep(0, nk)
    for(j in 1:nk){
      obs[j] = sum(rweibull(i, 0.5, 2))
    }
    simu = simu + pk*(1/nk)*sum(h(obs))
  }
  pk = 1 - ppois(6, lambda)
  nk = round(n*pk)
  obs = rep(0, nk)
  for (j in 1:nk){
    i = Finv(ppois(7, lambda) + (1 - ppois(7, lambda))* runif(1, 0, 1))
    obs[j] = sum(rweibull(i, 0.5, 2))
  }
  simu = simu + pk*(1/nk)*sum(h(obs))
  return(simu)
}

es2(n)

##exo 14

rm(list = ls())
sd= 2
n = 100000
h<-function(x,y){
  A<-(1/2)*exp(-(sd**2)/2+sd*x)+(1/2)*exp(-(sd**2)/2+sd*y)-1
  A[A<0]<-0
  return(A)
}
sigma<-matrix(c(1, 1/2, 1/2, 1), nrow = 2)
L<-t(chol(sigma))
Z<-matrix(c(rnorm(n),rnorm(n)),2)
x =L%*%Z 

es1 = function(n){
  simu = h(x[1,],x[2,])
  return(mean(simu))
}

A<-(sqrt(2)/2) *matrix(c(1,-1,1,1),2)
y<-A%*%Z

es2 = function(n){
  simu = (h(x[1,], x[2,]) + h(y[1,], y[1,])) /2
  return(mean(simu))
}

m<-2*exp((sd**2)/2)

h0<-function(x,y){
  return(exp(sd*x)+exp(sd*y))
}

b<-sum((h0(x[1,],x[2,])-m)*(h(x[1,],x[2,])-mean(h(x[1,],x[2,]))))/sum((h0(x[1,],x[2,])-m)**2)

es3 = function(n){
  simu = h(x[1,],x[2,])-b*(h0(x[1,],x[2,])-m)
  return(mean(simu))
}
es1(n)
es2(n)
es3(n)






