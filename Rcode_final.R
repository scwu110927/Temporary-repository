# Question3 ####################################################################

set.seed(12345)
time <- strptime(c("01/01/2020", "12/31/2021"), "%m/%d/%Y")
shop.time <- sample(seq(from = time[1], to = time[2], by = "day"), 1000, replace = T)
price <- sample(100:1000, 1000, replace = T)
subject <- sample(letters[1:20], 1000, replace = T)
data <- data.frame(shop.time, subject, price)

#(a)
order.data <- data[order(data$subject), ]    #5pt

#(b)
j.last.index <- max(which(order.data$subject == "j"))
data.a.to.j <- order.data[1:j.last.index, ]    #5pt
boxplot(data.a.to.j$price ~ data.a.to.j$subject, xlab = "subject", ylab = "cost")    #5pt     

#(c)
shop.avg <- NULL
for (i in 1:20){
  per.sub.price <- order.data$price[order.data$subject == letters[i]]    #3pt
  mean.per.sub.price <- mean(per.sub.price)    #3pt
  names(mean.per.sub.price) = letters[i]
  shop.avg <- c(shop.avg, mean.per.sub.price)    #4pt
}
shop.avg

#(d)
a.last.index <- max(which(order.data$subject == "a"))
data.a <- order.data[1:a.last.index, ]    #3pt
order.data.a <- data.a[order(data.a$shop.time), ]    #3pt
plot(order.data.a$shop.time, order.data.a$price, type = "b", pch = 20, xlab = "time", ylab = "cost")    #4pt


