
##exo 8 

##1 
f = function(x, b, mean, sd){
  return((1/ (sd*sqrt(2*pi)) ) * (exp(-(x-mean)**2 / 2* sd**2)) * (x >=b) * (1/(pnorm((mean - b) / sd))))
}

g = function(x, mean, sd){
  return((1/ (sd*sqrt(2*pi)) ) * (exp(-(x-mean)**2/ (2* sd**2))))
}


tr_norm = function(n, b, mean, sd){
  u = runif(n, 0, 1)
  y = rnorm(n, mean, sd)
  M = 1 / pnorm( (mean - b) /sd )
  return(y[u < (f(y, b, mean, sd) / (M* g(y, mean, sd))) ])
}

x0 = seq(2, 3, 0.1)
hist(tr_norm(100000, 2, 0, 2), prob = TRUE)
par(new = TRUE)
plot(x0, f(x0, 2, 0, 2), type = 'l', col = 'blue')

?rnorm
##2 

g = function(x, lambda){
  return(lambda * exp(-lambda * x) *(x>= 0))
}

f = function(x, b, lambda){
  return(lambda * exp(-lambda * x) *(x>= 0) * (exp(lambda* b)) * (x >=b))
}

tr_norm_2 = function(n, b, lambda){
  u = runif(n, 0, 1)
  y = rexp(n, lambda)
  M = (exp(lambda* b))
  return(y[u < (f(y, b, lambda / (M* g(y, lambda)))) ])
}
  
hist(tr_norm_2(1000000, 2, 2))
par(new = TRUE)
plot(x0, f(x0, 2, 2), type = 'l', col = 'blue')




