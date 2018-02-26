#lession 8
library(ggplot2) #must load the ggplot package first
library(gridExtra)
library(dplyr)
library('alr3')
library("GGally")
library('reshape2')
library('scales')
library(knitr)
library('bitops')
library('RCurl')
library(MASS)
library(lattice)

data(diamonds) 
str(diamonds)
ggplot(data=diamonds, aes(x=price))+
  geom_histogram(aes(color=cut),binwidth=50)+
  scale_x_continuous(limits=c(300,1500), breaks=seq(0,1500,100))+
  facet_wrap(~color)+
  scale_fill_brewer(type = 'qual')

ggplot(data=diamonds, aes(x=table, y=price))+
  geom_point(aes(color=cut))+
  xlim(50,80)
 
by(diamonds$table,diamonds$cut, summary)
diamonds$volume=with(diamonds,x*y*z)

ggplot(data=diamonds, aes(x=volume, y=price))+
  geom_point(aes(color=clarity))+
  xlim(0, quantile(diamonds$volume, 0.99))
str(pf)

ggplot(data=diamonds, aes(x=carat, y=price))+
  geom_point(color="orange")+
  xlim(0.2, quantile(diamonds$carat, 0.99))+
  ylim(326, quantile(diamonds$price, 0.99))

library(GGally)
library(dplyr)
ggpairs(sample_n(diamonds,1000))

p1=ggplot(data=diamonds, aes(x=price))+
  geom_histogram(color="orange")+
  xlim(326, quantile(diamonds$price, 0.99))+
  ggtitle("Price")
p2=ggplot(data=diamonds, aes(x=log10(price)))+
  geom_histogram(color="orange")+
  xlim(log10(326), quantile(log10(diamonds$price), 0.99))+
  ggtitle("Log10 price")
library(gridExtra)
grid.arrange(p1,p2,ncol=2)
cuberoot_trans = function(x) { x^(1/3 )};


library(RColorBrewer)
ggplot(aes(x = carat, y = price), data = diamonds) + 
  geom_point(aes(clarity),alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Clarity', reverse = T,
                                                   override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Clarity')

m1 <- lm(I(log(price)) ~ I(carat^(1/3)), data = diamonds)
m2 <- update(m1, ~ . + carat)
m3 <- update(m2, ~ . + cut)
m4 <- update(m3, ~ . + color)
m5 <- update(m4, ~ . + clarity) 
mtable(m1, m2, m3, m4, 'm5', sdigits = 3)

install.package('bitops')
install.packages('RCurl')
library('bitops')
library('RCurl')
library(MASS)
library(lattice)

load("BigDiamonds.rda")

