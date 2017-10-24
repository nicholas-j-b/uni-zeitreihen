
setwd("Z:/zeitreihen/stoxx50e")


#1Date 2Open 3High 4Low 5Close* 6Adj 7Close** 8Volume
H <- read.csv("^STOXX50E.csv")

t <- as.Date(H[,1])
y <- H[,2]
y <- c(y[1:7928])
y.2 <- ifelse(y == 7676, NAN, y)

y.3 <- c()

for(i in 1:(length(y))){
	if(y[i] == 7676){
		y.3 <- c(y.3, y[i - 1])
	}else{
		y.3 <- c(y.3, y)
	}
}



y.log <- log(y)

y.lm <- lm(y.log~t)

layout(1:3)

plot(t, y, type = "l")

q <- ifelse(y > 7200, 0, y)
plot(t, y.3, na.rm = TRUE, type = "l")

plot(t, y.log, type = "l")