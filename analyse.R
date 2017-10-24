
setwd("Z:/zeitreihen/stoxx50e")


#1Date 2Open 3High 4Low 5Close* 6Adj 7Close** 8Volume
H <- read.csv("^STOXX50E.csv")

t <- as.Date(H[,1])
y <- H[,2]
y <- c(y[1:7928])
y.2 <- ifelse(y == 7676, NaN, y)

y.3 <- y.2[a:b]

# for(i in 1:(length(y))){
# 	if(y[i] == 7676){
# 		y.3 <- c(y.3, y[i - 1])
# 	}else{
# 		y.3 <- c(y.3, y)
# 	}
# }
# 
# for(i in 1:7928){
# 	if(y[i] == 7676){
# 		y.3 <- c(y.3, y[i - 1])
# 	}else{
# 		y.3 <- c(y.3, y)
# 	}
# }



y.log <- log(y)

y.lm <- lm(y.log~t)

layout(1:2)

plot(t, y, type = "l")

q <- ifelse(y > 7200, 0, y)
plot(t, y.2, type = "l")

# plot(t[a:b], y.3, type = "l")
# 
# a <- 700
# b <- a + 50
# y.2[a:b]


