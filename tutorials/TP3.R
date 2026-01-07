##exo 6
rm(list = ls())
x0 = seq(-1, 1, 0.01)
f = function(x){
  return ((2/pi)*sqrt(1 - x**2))
  }


g = function(x){
  return(dunif(x, -1, 1))
}
  

u = runif(1000000, 0,1)
y = runif(1000000, -1, 1)
M = 4/pi

simu = y[u < (f(y) / (M*g(y)))]


hist(simu)
par(new = TRUE)
plot(x0, f(x0), type = 'l')

length(simu) /1000000
 1/M

## exo 7

f = function(x, y){
  return((1/pi)*(x**2 +y**2 < 1) )
}

g = function(x, y){
  return((1/4)*(x<1 & x > -1)*(y<1 & y > -1))
}

u1 = runif(100000, 0, 1)


y1 = runif(100000, -1, 1)
y2 = runif(100000, -1, 1)
y = cbind(runif(1, -1, 1),runif(1, -1, 1))
        
M = 4/pi


simu_ob = u1 < f(y1, y2)/ M*g(y1, y2)

simu_final = c(0)
for (i in 1:100000) {
  if (simu_ob[i]== TRUE){
    simu_ob2 = c(y1[i], y2[i])
    simu_final = rbind(simu_final, simu_ob2)
  }
}

plot(simu_final, asp =1)
par(new = TRUE)
contour(x0, x0, outer(x0, x0, f), asp = 1, col = 'blue')



