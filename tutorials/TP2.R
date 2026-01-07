## Exo 5 
##me
z = rnorm(1110)
t0 = c(0, 1:100 / 100, 1 + (101:110 - 100)/10, 2 + (111:1110 - 110)/1000)
t1 = c(1:100 / 100, 1 + (101:110 - 100)/10, 2 + (111:1110 - 110)/1000, 0)
tt = t1 -t0
tt = tt[1:1110]
cumsum(sqrt(tt)*z)

plot(time, c(0,cumsum(sqrt(tt)*z)))

##cor

time = c(seq(0, 1, 0.01), seq(1.1, 2, 0.1), seq(2.001, 3, 0.001))
brownian = rnorm(length(time) -1,  std=sqrt(diff(time)))

plot(time, c(0,cumsum(brownian)))

## Exo 2

