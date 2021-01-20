# Plotando gráficos no R

##### FUNÇÕES, LIMITES E CONTINUIDADE

## QUESTÃO 1

# Raiz quadrada
f1 <- function(x){
  out <- sqrt(x)
  return(out)
}
plot(f1,xlim = c(0,100),ylab = "Sqrt(x)")


# Logaritmo neperiano
f2 <- function(x){
  out <- log(x)
  return(out)
}
plot(f2,xlim = c(0,100),ylab = "Ln(x)")


# Logaritmo na base 10
f3 <- function(x){
  out <- log10(x)
  return(out)
}
plot(f3,xlim = c(0,100),ylab = "Log(x)")


# Exponencial
f4 <- function(x){
  out <- exp(x)
  return(out)
}
plot(f4,xlim = c(0,10),ylab = "Exp(x)")


# Gamma
f5 <- function(x){
  out <- gamma(x)
  return(out)
}
plot(f5,xlim = c(0,5),ylab = "Gamma(x)")


# 1/x
f6 <- function(x){
  out <- 1/x
  return(out)
}
plot(f6,xlim = c(-20,20),ylab = "1/x")


# |x+1|+2
f7 <- function(x){
  out <- abs(x-1)+2
  return(out)
}
plot(f7,xlim = c(-20,20),ylab = "|x+1|+2")


# Beta(x,0.5)
f8 <- function(x){
  out <- beta(x,0.5)
  return(out)
}
plot(f8,xlim = c(0,100),ylab = "Beta(x,0.5)")


# (x-1)^3
f9 <- function(x){
  out <- (x-1)^3
  return(out)
}
plot(f9,xlim = c(0,10),ylab = "(x-1)^3")


# (x+1)/x
f10 <- function(x){
  out <-(x+1)/x
  return(out)
}
plot(f10,xlim = c(-10,10),ylab = "(x+1)/x")


## QUESTÃO 2

# 2a
f11 <- function(theta,x){
  out <-2*(x*log(x/theta)-x+theta)
  return(out)
}
f11 <- Vectorize(f11,"theta")
plot(f11(theta = seq(0,100), 10)~seq(0,100),type = "l",xlab="x",ylab="f(x)")


# 2b
f12 <- function(theta,x){
  out <- choose(100,x)*exp(x*log(theta/(1-theta))+100*log(1-theta))
  return(out)
}
f12 <- Vectorize(f12,"theta")
plot(f12(theta = seq(0,0.3,by = 0.001), 10)~seq(0,0.3,by = 0.001),type = "l",xlab="x",ylab="f(x)")


# 2c
f13 <- function(theta,x){
  out <- 2*((x/theta)-log(x/theta)-1)
  return(out)
}
f13 <- Vectorize(f13,"theta")
plot(f13(theta = seq(5,100), 10)~seq(5,100),type = "l",xlab="x",ylab="f(x)")


# 2d
f14 <- function(theta,x,p){
  out <- 2*((x^(2-p))/((1-p)*(2-p))-(x*theta^(1-p))/(1-p)+(theta^(2-p))/(2-p))
  return(out)
}
f14 <- Vectorize(f14,"theta")
plot(f14(theta = seq(-5,5,by = 0.5), 10, 10)~seq(-5,5,by = 0.5),type = "l",xlab="x",ylab="f(x)")


# 2e
f15 <- function(theta,x,p){
  out <- 2*(1-cos(x-theta))
  return(out)
}
f15 <- Vectorize(f15,"theta")
plot(f15(theta = seq(-10,10,by = 0.01), 5, 5)~seq(-10,10,by = 0.01),type = "l",xlab="x",ylab="f(x)")


## QUESTÃO 3



## QUESTÃO 4



##### DERIVADAS

## QUESTÃO 1


## QUESTÃO 2

f16 <- function(x){
  out <- 1/x
  return(out)
}
f16_prime <- function(x){
  out <- -1/x^2
  return(out)
}
intercept16 <- (f16(x=2)-f16_prime(x=2)*2)
slope16 <- f16_prime(x=2)
plot(f16(x=seq(0,20))~seq(0,20),type = "l",xlab="x",ylab="f(x)", lwd = 1.5)
abline(intercept16, slope16, col = "red", lwd=1.5)
points(2,f16(2), lwd = 1.5)


f17 <- function(x){
  out <- x^3
  return(out)
}
f17_prime <- function(x){
  out <- 3*x^2
  return(out)
}
intercept17a <- (f17(x=-3)-f17_prime(x=-3)*(-3))
slope17a <- f17_prime(x=-3)
intercept17b <- (f17(x=3)-f17_prime(x=3)*(3))
slope17b <- f17_prime(x=3)
plot(f17(x=seq(-10,10))~seq(-10,10),type = "l",xlab="x",ylab="f(x)", lwd = 1.5)
abline(intercept17a, slope17a, col = "red", lwd=1.5)
abline(intercept17b, slope17b, col = "red", lwd=1.5)
points(-3,f17(-3), lwd = 1.5)
points(3,f17(3), lwd = 1.5)


f18 <- function(x){
  out <- exp(x)
  return(out)
}
f18_prime <- function(x){
  out <- exp(x)
  return(out)
}
intercept18 <- (f18(x=0)-f18_prime(x=0)*0)
slope18 <- f18_prime(x=0)
plot(f18(x=seq(-4,4))~seq(-4,4),type = "l",xlab="x",ylab="f(x)", lwd = 1.5)
abline(intercept18, slope18, col = "red", lwd=1.5)
points(0,f18(0), lwd = 1.5)


f19 <- function(x){
  out <- log(x)
  return(out)
}
f19_prime <- function(x){
  out <- 1/x
  return(out)
}
intercept19 <- (f19(x=2)-f19_prime(x=2)*2)
slope19 <- f19_prime(x=2)
plot(f19(x=seq(0,30))~seq(0,30),type = "l",xlab="x",ylab="f(x)", lwd = 1.5)
abline(intercept19, slope19, col = "red", lwd=1.5)
points(2,f19(2), lwd = 1.5)


## QUESTÃO 3


## QUESTÃO 4


## QUESTÃO 5

f20 <- function(mu,y){
  out <- sum((y-mu)^2)
  return(out)
}

y <- c(2.09,-1.32,-0.20,0.05,-0,07)

f20(mu = 10, y = y)
mu_grid <- seq(-15,20, l = 100)
temp <- c()
for(i in 1:100){
  temp[i] <- f20(mu = mu_grid[i], y=y)
}

taylor_ap <-function(mu, mu0, f20, f_prime, f_dprime) {
  out <-f20(mu = mu0,y = y)+(mu-mu0)*f_prime(mu = mu0, y = y)+
    (((mu-mu0)^2)/(2))*f_dprime(mu = mu0, y = y)
  return(out)
}

f_prime <- function(mu, y){
  out <- -2*sum(y-mu)
  return(out)
}

f_dprime <- function(mu, y){
  n <- length(y)
  return(n*2)
}

taylor_ap(mu = c(9,10,11), mu0 = 10, f20 = f20,
          f_prime = f_prime, f_dprime = f_dprime)

tempap <- c()
for(i in 1:100){
  tempap[i] <- taylor_ap(mu = mu_grid[i], mu0 = 10, f20 = f20,
                         f_prime = f_prime, f_dprime = f_dprime)
}

plot(temp ~ mu_grid, type = "l")
lines(mu_grid,tempap,col = 2,lty = 3, lwd =3)     
     


f21 <- function(mu,y){
  out <- sum(2*(y*log(y/mu)+mu-y))
  return(out)
}

y <- c(4,4,4,6,5)

f21(mu = 10, y = y)
mu_grid <- seq(0,20, l = 100)
temp <- c()
for(i in 1:100){
  temp[i] <- f21(mu = mu_grid[i], y=y)
}

plot(temp ~ mu_grid, type = "l")

taylor_ap <-function(mu, mu0, f21, f_prime, f_dprime) {
  out <-f21(mu = mu0,y = y)+(mu-mu0)*f_prime(mu = mu0, y = y)+
    (((mu-mu0)^2)/(2))*f_dprime(mu = mu0, y = y)
  return(out)
}

f_prime <- function(mu, y){
  n <- length(y)
  out <- -2*sum(y/mu)+n*2
  return(out)
}

f_dprime <- function(mu, y){
  out <- 2*sum(y/mu)
  return(out)
}

taylor_ap(mu = c(9,10,11), mu0 = 10, f21 = f21,
          f_prime = f_prime, f_dprime = f_dprime)

tempap <- c()
for(i in 1:100){
  tempap[i] <- taylor_ap(mu = mu_grid[i], mu0 = 10, f21 = f21,
                         f_prime = f_prime, f_dprime = f_dprime)
}

plot(temp ~ mu_grid, type = "l")
lines(mu_grid,tempap,col = 2,lty = 3, lwd =3)     

   
     