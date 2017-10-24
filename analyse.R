

setwd("Z:/zeitreihen/jap-ind")

H <- read.csv("JAP_IND.csv")

t <- as.Date(H[,1])

y <- H[,2]

layout(1)

plot(t, y, type = "l")