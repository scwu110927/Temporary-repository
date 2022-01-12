#HW4
##Q1############################################################################
set.seed(1234)
X <- rbinom(500, 1, 0.5)
Y <- rbinom(500, 1, 0.3*X+0.6*(1-X))
(XY.tab <- table(X, Y))
chisq.test(XY.tab)

##Q2############################################################################
W <- rbinom(500, 1, 0.9*X+0.75*(1-X))
(WXY.tab <- table(W, X, Y))
(theta1 <- sum(WXY.tab[2, 2, ])/sum(WXY.tab[, 2, ]))
(theta0 <- sum(WXY.tab[2, 1, ])/sum(WXY.tab[, 1, ]))
(n1 <- sum(WXY.tab[2, , 2])/sum(WXY.tab[, , 2]))
alpha1 <- sum(WXY.tab[, 2, 2])/sum(WXY.tab[, , 2])
(c1 <- alpha1*theta1+(1-alpha1)*theta0)

n0 <- sum(WXY.tab[2, , 1])/sum(WXY.tab[, , 1])
(n1-n0)
alpha0 <- sum(WXY.tab[, 2, 1])/sum(WXY.tab[, , 1])
(c2 <- (alpha1-alpha0)*(theta1-theta0))

##Q3############################################################################
Xv <- rbinom(100, 1, 0.4)
Wv <- rbinom(100, 1, 0.9*Xv+0.75*(1-Xv))
(XWv.tab <- table(Wv, Xv))
theta1 <- XWv.tab[2, 2]/sum(XWv.tab[, 2])
theta0 <- XWv.tab[2, 1]/sum(XWv.tab[, 1])
c(theta1, theta0)
a <- matrix(c(theta1-theta0, 0, 0, theta1-theta0), ncol=2)
b <- c(n1, n0)-theta0
c <- solve(a)%*%b
c[1]-c[2]

WY <- cbind(W, Y)
XWv <- cbind(Wv, Xv)
alpdiff.boot <- NULL
for(i in 1:100){
  WY.boot <- WY[sample(500, replace = TRUE), ]
  WY.tab.boot <- table(WY.boot[, 1], WY.boot[, 2])
  n1.boot <- WY.tab.boot[2, 2]/sum(WY.tab.boot[, 2])
  n0.boot <- WY.tab.boot[2, 1]/sum(WY.tab.boot[, 1])
  XWv.boot <- XWv[sample(100, replace = TRUE), ]
  XWv.tab.boot <- table(XWv.boot[, 1], XWv.boot[, 2])
  theta1.boot <- XWv.tab.boot[2, 2]/sum(XWv.tab.boot[, 2])
  theta0.boot <- XWv.tab.boot[2, 1]/sum(XWv.tab.boot[, 1])
  a.boot <- matrix(c(theta1.boot-theta0.boot, 0, 0, theta1.boot-theta0.boot), ncol=2) 
  b.boot <- c(n1.boot, n0.boot)-theta0.boot
  c.boot <- solve(a.boot)%*%b.boot
  alpdiff.boot <- c(alpdiff.boot, c.boot[1]-c.boot[2])
}
c(mean(alpdiff.boot), sd(alpdiff.boot))

##Q4############################################################################
index <- sample(500, 100,replace = FALSE)
XWYu <- cbind(X, W, Y)[index, ]
(WY.tab <- table(W, Y))
WY.joint <- matrix(WY.tab/sum(WY.tab))
(XWYu.tab <- table(XWYu[, 1], XWYu[, 2], XWYu[, 3]))
block1 <- rbind(XWYu.tab[1, , 1]/colSums(XWYu.tab[, , 1]),
                XWYu.tab[2, , 1]/colSums(XWYu.tab[, , 1]))
block2 <- rbind(XWYu.tab[1, , 2]/colSums(XWYu.tab[, , 2]),
                XWYu.tab[2, , 2]/colSums(XWYu.tab[, , 2]))
block0 <- matrix(0, nrow = 2, ncol = 2)
XWY.block <- rbind(cbind(block1, block0), cbind(block0, block2))
joint <- XWY.block%*%WY.joint
joint[4]/(joint[3]+joint[4])
joint[2]/(joint[1]+joint[2])

