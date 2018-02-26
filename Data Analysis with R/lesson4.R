#Lesson 4
library(ggplot2) #must load the ggplot package first
data(diamonds) #l
str(diamonds)
ggplot(data=diamonds, aes(x=price))+
  geom_histogram(binwidth=50, color="red", fill="blue")+
  scale_x_continuous(limits=c(300,1500), breaks=seq(0,1500,100))

summary(diamonds$price)
length(diamonds$price[which(diamonds$price<250)])
str(subset(diamonds, diamonds$price<250))
str(subset(diamonds, diamonds$price>=15000))

ggplot(data=diamonds, aes(x=price))+
  geom_histogram(binwidth=50, color="red", fill="blue")+
  facet_wrap(~cut, scales="free")
  
ggplot(data=diamonds, aes(x=price/carat))+
  geom_histogram( color="black", fill="orange")+
  facet_grid(cut~., scales="free")+
  scale_x_log10()

ggplot(data=diamonds, aes(y=price, x=cut))+
  geom_boxplot( color="black", fill="orange")
  
by(diamonds$price, diamonds$color, summary)

common=table(factor(diamonds$carat, ordered = TRUE))
common[common>2000]

  by(diamonds$price, diamonds$cut, summary)
  
  ggplot(data=diamonds, aes(x=carat))+
    geom_freqpoly(binwidth=0.1 )+
    scale_x_continuous(limits=c(0,2), breaks=seq(0,2,0.1))
  
library(tidyr)
library(dplyr)
  marriage=read.xlsx("indicator age of marriage.xlsx", sheetName = "data")

   