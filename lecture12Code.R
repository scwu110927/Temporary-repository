
# initial value
n <- 300
b0 <- -1
b1 <- 1
Xstar <- rnorm(n, 0, 2)
epislon <- rnorm(n, 0, 1)
delta <- rnorm(n, 0, 1)
Ystar <- b0+b1*Xstar
Y <- Ystar+epislon
X <- Xstar+delta
mydata <- data.frame(Ystar, Xstar, Y, X)
head(mydata)

# get (gamma0, gamma1)
gamma <- coef(lm(Y~X, data = mydata))

# adjusted (beta0, beta1)
vard <- 1
varXstar <- var(X)-vard
kappa <- varXstar/(varXstar+vard)
beta1 <- gamma[2]/kappa
beta0 <- mean(Y)-beta1*mean(X)
c(beta0, beta1) #true is(-1, 1)

# se(beta1)
se.gamma1 <- summary(lm(Y~X, data = mydata))$coefficients[2, 2]
se.beta1 <- se.gamma1/kappa

# 95%CI of beta1
indi.b1 <- (abs((beta1-b1)/se.beta1) <= 1.96)

# rep1000
result <- NULL
for(i in 1:1000){
  Xstar <- rnorm(n, 0, 2)
  epislon <- rnorm(n, 0, 1)
  delta <- rnorm(n, 0, 1)
  Ystar <- b0+b1*Xstar
  Y <- Ystar+epislon
  X <- Xstar+delta
  mydata <- data.frame(Ystar, Xstar, Y, X)
  gamma <- coef(lm(Y~X, data = mydata))
  varXstar <- var(mydata$X)-1
  kappa <- varXstar/(varXstar+1)
  beta1 <- gamma[2]/kappa
  se.gamma1 <- summary(lm(Y~X, data = mydata))$coefficients[2, 2]
  se.beta1 <- se.gamma1/kappa
  indi.b1 <- (abs((beta1-b1)/se.beta1) <= 1.96)
  a <- c(gamma[2], se.gamma1, beta1, se.beta1, indi.b1)
  result <- rbind(result, a)
}
colnames(result) <- c("gamma1", "se.gamma1", "beta1", "se.beta1", "indi.b1")
apply(result, 2, mean)
apply(result, 2, sd)



#bootstrap
beta1.boot <- NULL
for(i in 1:80){
data.boot <- mydata[sample(n, replace = TRUE), ]
gamma <- coef(lm(Y~X, data = data.boot))
varXstar <- var(data.boot$X)-1
kappa <- varXstar/(varXstar+1)
beta1 <- gamma[2]/kappa
beta1.boot <- c(beta1.boot, beta1)
}
beta1.boot.m <- mean(beta1.boot)
beta1.boot.sd <- sd(beta1.boot)



#rep1000 with bootstrap
result <- NULL
for(i in 1:1000){
  Xstar <- rnorm(n, 0, 2)
  epislon <- rnorm(n, 0, 1)
  delta <- rnorm(n, 0, 1)
  Ystar <- b0+b1*Xstar
  Y <- Ystar+epislon
  X <- Xstar+delta
  mydata <- data.frame(Ystar, Xstar, Y, X)
  gamma <- coef(lm(Y~X, data = mydata))
  varXstar <- var(mydata$X)-1
  kappa <- varXstar/(varXstar+1)
  beta1 <- gamma[2]/kappa
  se.gamma1 <- summary(lm(Y~X, data = mydata))$coefficients[2, 2]
  se.beta1 <- se.gamma1/kappa
  indi.b1 <- (abs((beta1-b1)/se.beta1) <= 1.96)

  beta1.boot.set <- NULL
  for(j in 1:80){
    data.boot <- mydata[sample(n, replace = TRUE), ]
    gamma.boot <- coef(lm(Y~X, data = data.boot))
    varXstar.boot <- var(data.boot$X)-1
    kappa.boot <- varXstar.boot/(varXstar.boot+1)
    beta1.boot <- gamma.boot[2]/kappa.boot
    beta1.boot.set <- c(beta1.boot.set, beta1.boot)
  }
  beta1.boot.m <- mean(beta1.boot.set)
  beta1.boot.sd <- sd(beta1.boot.set)
  
  a <- c(gamma[2], se.gamma1, beta1, se.beta1, indi.b1, beta1.boot.m, beta1.boot.sd)
  result <- rbind(result, a)
}
colnames(result) <- c("gamma1", "se.gamma1", "beta1", "se.beta1", "indi.b1", "beta1.boot", "se.beta1.boot")
apply(result, 2, mean)
apply(result, 2, sd)


