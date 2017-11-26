#Production of Total Industry


#setwd("Z:/zeitreihen/jap-ind")
setwd("~/r-ws/zeitreihen/proj1/uni-zeitreihen")
H <- read.csv("JAP_IND.csv")

t <- as.Date(H[,1])
y <- H[,2]
n <- length(y)

y.log <- log(y)
power <- 1
g <- y.log**power


prod.lm <- lm(g~t)

#plot(t, g, type = "l")
#lines(t, prod.lm$fitted.values, col = "red")
#plot(t, prod.lm$residuals)

#first power
q <- 168
q2 <- 400
z <- rep(0, q)
z <- c(z, 1:(n - q))
z2 <- rep(0, q2)
z2 <- c(z2, 1:(n - q2))
prod.btrend <- lm(g~t + z + z2)

#fourth power
# q <- 365
# z <- rep(0, q)
# z <- c(z, 1:(n - q))
# prod.btrend <- lm(g~t + z)


plot(t, g, type = "l")
lines(t, prod.btrend$fitted.values, col = "red")
# abline(v = t[q], col = "blue")

abline(v = t[q], col = "green")
abline(v = t[q2], col = "blue")
# plot(t, prod.btrend$residuals)


