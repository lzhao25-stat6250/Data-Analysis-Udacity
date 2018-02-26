#Lession5
pf=read.csv('pseudo_facebook.tsv', sep='\t')
library(ggplot2)
ggplot(data=pf, aes(x=age, y=friend_count))+
  geom_point()+
  xlim(c(13,90))

ggplot(data=pf, aes(x=age, y=friend_count))+
  geom_point(alpha=1/20, position=position_jitter(h=0))+
  xlim(c(13,90))+
  coord_trans(y="sqrt")

ggplot(data=pf, aes(x=age, y=friend_count))+
  geom_jitter(alpha=1/20)+
  xlim(13,90)

summary(pf$age)

pf %>%
  group_by(age) %>%
 summarise(avg_friend=mean(friend_count)) %>%
  ggplot( aes(x=age, y=avg_friend))+
  geom_line()+
  xlim(13,90)

ggplot(data=pf, aes(x=age, y=friend_count))+
  geom_point(alpha=1/20, position=position_jitter(h=0), col="orange")+
  xlim(c(13,90))+
  coord_trans(y="sqrt")+
  geom_line(stat = "summary", fun.y=mean, col="blue")+
  geom_line(stat = "summary", fun.y=quantile, fun.args=list(probs=0.25), linetype=2, col="blue")

with(subset(pf, age<=70), cor.test(age, friend_count, method="pearson"))

ggplot(data=pf, aes(x=www_likes_received, y=likes_received ))+
  geom_point( col="orange")+
  xlim(0,quantile(pf$www_likes_received,0.95))+
  ylim(0,quantile(pf$likes_received, 0.95))+
  geom_smooth(method="lm", col="blue")

with(pf, cor.test(www_likes_received, likes_received, method="pearson"))

library(alr3)
data("Mitchell")
ggplot(data=Mitchell, aes(x=Month, y=Temp ))+
  geom_point( col="orange")+
  scale_x_continuous(breaks=seq(0,203,12))

with(Mitchell, cor.test(Month, Temp, method="pearson"))

pf$age_with_months=pf$age+(1 - pf$dob_month / 12)
str(pf)

pf %>%
  group_by(age_with_months) %>%
  pf.fc_by_age_months=
  summarise(friend_count_mean=mean(friend_count),
            friend_count_median=median(friend_count), n=n()) 
  
age_groups <- group_by(pf, age_with_months)
pf.fc_by_age <- summarise(age_groups,
                          friend_count_mean = mean(friend_count),
                          friend_count_median = median(friend_count),
                          n = n())
pf.fc_by_age <- arrange(pf.fc_by_age, age_with_months)
head(pf.fc_by_age)

pf.fc_by_age_months=pf %>%
    group_by(age_with_months) %>%
    summarise(                   friend_count_mean = mean(friend_count),
                                 friend_count_median = median(friend_count),
                                 n = n()) 
head(pf.fc_by_age_months)




  
 p1=ggplot(subset(pf.fc_by_age_months, age_with_months<71), aes(x=age_with_months, y=friend_count_mean))+
  geom_line()+
    geom_smooth()
  p2=ggplot(data=subset(pf,age<71), aes(x=round(age/5)*5, y=friend_count))+
    geom_line(stat="summary", fun.y=mean)
  library(gridExtra)
  grid.arrange(p1,p2,ncol=1)
  
  
  