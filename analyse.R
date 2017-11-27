#Production of Total Industry

library(mFilter)

#setwd("Z:/zeitreihen/jap-ind")
setwd("~/r-ws/zeitreihen/proj1/uni-zeitreihen")
H <- read.csv("JAP_IND.csv")

t <- as.Date(H[,1])
y <- H[,2]
n <- length(y)

y.log <- log(y)
power <- 1
g1 <- y.log
g2 <- y.log**2
g3 <- y.log**3
g4 <- y.log**4


prod.lm1 <- lm(g1~t)
prod.lm2 <- lm(g2~t)
prod.lm3 <- lm(g3~t)
prod.lm4 <- lm(g4~t)

#plot(t, g, type = "l")
#lines(t, prod.lm$fitted.values, col = "red")
#plot(t, prod.lm$residuals)

#first power
q1 <- 168
q2 <- 400
z <- rep(0, q1)
z <- c(z, 1:(n - q1))
z2 <- rep(0, q2)
z2 <- c(z2, 1:(n - q2))
z3 <- ifelse(z > 0, 1, 0)
z4 <- ifelse(z2 > 0, 1, 0)
prod.trend <- lm(g1~t + z)
prod.btrend1 <- lm(g1~t + z + z2)
prod.btrend2 <- lm(g1~t + z + z2 + z3 + z4)



# par(mfrow=c(2,2),mar=c(2,2,1,1)) 
# plot(t, g1,type="l"); mtext("(a)",side=3,cex=0.7); lines(t, prod.lm1$fitted.values, col = "red")
# plot(t, g2,type="l"); mtext("(b)",side=3,cex=0.7); lines(t, prod.lm2$fitted.values, col = "red")
# plot(t, g3,type="l"); mtext("(c)",side=3,cex=0.7); lines(t, prod.lm3$fitted.values, col = "red")
# plot(t, g4,type="l"); mtext("(d)",side=3,cex=0.7); lines(t, prod.lm4$fitted.values, col = "red")

# par(mfrow=c(2,2),mar=c(2,2,1,1)) 
# plot(t, prod.lm1$residuals); mtext("(a)",side=3,cex=0.7); 
# plot(t, prod.lm2$residuals); mtext("(b)",side=3,cex=0.7); 
# plot(t, prod.lm3$residuals); mtext("(c)",side=3,cex=0.7); 
# plot(t, prod.lm4$residuals); mtext("(d)",side=3,cex=0.7);

# par(mfrow=c(3,1),mar=c(2, 2, 1, 1))
# 
# plot(t, g1, type = "l"); mtext("(a)",side=3,cex=0.7)
# lines(t, prod.trend$fitted.values, col = "red")
# abline(v = t[q1])
# 
# plot(t, g1, type = "l"); mtext("(b)",side=3,cex=0.7); 
# lines(t, prod.btrend1$fitted.values, col = "red")
# abline(v = t[q1], col = "green")
# abline(v = t[q2], col = "blue")
# 
# plot(t, g1, type = "l"); mtext("(c)",side=3,cex=0.7); 
# lines(t, prod.btrend2$fitted.values, col = "red")
# abline(v = t[q1], col = "green")
# abline(v = t[q2], col = "blue")

# par(mfrow=c(3,1),mar=c(2, 2, 1, 1))
# 
# plot(t, prod.trend$residuals); mtext("(a)",side=3,cex=0.7)
# abline(v = t[q1])
# 
# plot(t, prod.btrend1$residuals); mtext("(b)",side=3,cex=0.7); 
# abline(v = t[q1], col = "green")
# abline(v = t[q2], col = "blue")
# 
# plot(t, prod.btrend2$residuals); mtext("(c)",side=3,cex=0.7); 
# abline(v = t[q1], col = "green")
# abline(v = t[q2], col = "blue")


prod.lts <- ts(g1)
 
# par(mfrow=c(2,2),mar=c(2,2,1,1)) 
# 
# plot(t, prod.lts,pch=20); mtext("(a)",side=3,cex=0.7);
# h <- hpfilter(prod.lts, type = "lambda", freq = 500) 
# lines(t, h$trend, col="red", lwd=2) 
# 
# plot(t, prod.lts,pch=20); mtext("(b)",side=3,cex=0.7);
# h <- hpfilter(prod.lts, type = "lambda", freq = 50000) 
# lines(t, h$trend, col="red", lwd=2) 
# 
# plot(t, prod.lts,pch=20); mtext("(c)",side=3,cex=0.7);
# h <- hpfilter(prod.lts, type = "lambda", freq = 500000) 
# lines(t, h$trend, col="red", lwd=2) 
# 
# plot(t, prod.lts,pch=20); mtext("(d)",side=3,cex=0.7);
# h <- hpfilter(prod.lts, type = "lambda", freq = 10000000) 
# lines(t, h$trend, col="red", lwd=2)



# par(mfrow=c(2,2),mar=c(2,2,1,1)) 
# 
# h <- hpfilter(prod.lts, type = "lambda", freq = 500) 
# plot(t, h$trend - prod.lts, pch=20); mtext("(a)",side=3,cex=0.7);
# 
# h <- hpfilter(prod.lts, type = "lambda", freq = 50000) 
# plot(t, h$trend - prod.lts, pch=20); mtext("(b)",side=3,cex=0.7);
# 
# 
# h <- hpfilter(prod.lts, type = "lambda", freq = 500000) 
# plot(t, h$trend - prod.lts, pch=20); mtext("(c)",side=3,cex=0.7);
# 
# h <- hpfilter(prod.lts, type = "lambda", freq = 10000000) 
# plot(t, h$trend - prod.lts, pch=20); mtext("(d)",side=3,cex=0.7)



par(mfrow=c(1,1),mar=c(2,2,1,1)) 

d <- g1[c(1:length(g1) - 1)] - g1[c(2:length(g1))]

par(mar=c(2,2,1,1));
plot(t[c(2:length(g1))], d, type="o", pch = 20) 
