##exo 1

x = runif(10000, min = 0, max = 1)
y = rep(0, 10000)
for(i in 1:10000){
  
  if (x[i] < 0.4){
    y[i] = 0
  }
  if (x[i] >= 0.4 && x[i] < 0.6) {
    y[i] <- 5
  }
  
  if (x[i] >= 0.6 && x[i] < 0.7) {
    y[i] <- 6
  }
  
  if (x[i] >= 0.7 && x[i] < 0.9) {
    y[i] <- 7
  }
  
  if (x[i] >= 0.9 && x[i] < 1) {
    y[i] <- 8
  }
}


barplot(table(y))

## Exo 3

xbar = runif(10000, min = 0, max = 1)
x1 = sqrt(-2*log(x))*cos(2*pi*xbar)
x2 = sqrt(-2*log(x))*sin(2*pi*xbar)

hist(c(x1, x2), col = rgb(1, 0, 0, 0.5))

n1 = rnorm(20000)

hist(n1,col = rgb(0, 0, 1, 0.5), add = TRUE)



