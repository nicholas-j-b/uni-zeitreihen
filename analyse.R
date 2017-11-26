
#setwd("Z:/zeitreihen/stoxx50e")
setwd("~/r-ws/zeitreihen/proj1/uni-zeitreihen")



#1Date 2Open 3High 4Low 5Close* 6Adj 7Close** 8Volume
H <- read.csv("^STOXX50E.csv")
a <- 1558
b <- 7928
n <- b - a + 1
t <- as.Date(H[,1])
y <- H[,6]
y <- c(y[a:b])
t <- c(t[a:b])
y <- ifelse(y == 7678, NaN, y)
r <- y[2:n] - y[1:(n - 1)]





#plot(t, y, type = "l")


par(mfrow=c(2,2),mar=c(2,2,1,1)) 
plot(t,y,type="l"); mtext("(a)",side=3,cex=0.7)  
plot(t[2:n],r,type="l"); mtext("(b)",side=3,cex=0.7)  
plot(t[2:n],r^2,type="l"); 
mtext("(c)",side=3,cex=0.7)  
plot(t[2:n],r^4,type="l"); 
mtext("(d)",side=3,cex=0.7)  


