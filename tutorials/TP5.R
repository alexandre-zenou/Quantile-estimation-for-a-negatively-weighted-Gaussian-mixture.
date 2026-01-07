## exo 9 

rm(list = ls())

n = 150000
x = runif(n, 0, 1)
y = runif(n, 0, 1)

pi1 = 4* 1/n * sum(sqrt(1- x**2))
pi2 = (4/n) * sum(x**2 + y**2 < 1)

v1 =var(sqrt(1 - x**2))
v2= var(x**2 + y**2 < 1)

pi1 = function(n){
  x = runif(n)
  return( 4* 1/n * sum(sqrt(1- x**2)))
}
pi2 = function(n){
  x = runif(n)
  y = runif(n)
  return((4/n) * sum(x**2 + y**2 < 1))
}

library(microbenchmark)
c = microbenchmark(pi1(n), pi2(n))
c1 = median(c$time[c$expr=="pi1(n)"])
c2 = median(c$time[c$expr=="pi2(n)"])

(v1*c1) / (v2*c2)

