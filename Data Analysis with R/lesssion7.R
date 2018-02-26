#lession7
pf.fc_by_age_gender=pf %>%
  filter(!is.na(gender))%>%
  group_by(age, gender) %>%
  summarise(mean_friend_count = mean(friend_count),
            median_friend_count = median(friend_count),
            n = n()) %>%
  ungroup()%>%
  arrange(age)
head(pf.fc_by_age_gender)


ggplot(data=subset(pf,!is.na(gender)), aes(x=age,y=friend_count, color=gender))+
  geom_line(stat="summary", fun.y=mean)

ggplot(data=pf.fc_by_age_gender, aes(x=age, y=mean_friend_count,color=gender))+
  geom_line()
library("tidyr")
pf.fc_by_age_gender.wide=spread(select(pf.fc_by_age_gender,age,gender,median_friend_count),
                                gender,median_friend_count)
ggplot(data=pf.fc_by_age_gender.wide,aes(x=age,y=female/male))+
  geom_line()+
  geom_hline(yintercept=1,linetype=2, color="blue")
pf$year_joined <- floor(2014 - pf$tenure / 365)

pf$year_joined.bucket=cut(pf$year_joined, breaks=c(2004,2009,2011,2012,2014))
table(pf$year_joined.bucket)

ggplot(data=subset(pf,!is.na(year_joined.bucket)), aes(x=age,y=friend_count))+
  geom_line( aes(color=year_joined.bucket), stat="summary", fun.y=mean)+
  geom_line(stat="summary", fun.y=mean, linetype=2)

summary(mutate(subset(pf, tenure>0),friend_count/tenure))

yo=read.csv("yogurt.csv")
yo$id=factor(yo$id)
str(yo)
summary(yo)

ggplot(data=yo, aes(x=price))+
  geom_histogram()

yo$all.purchases=with(yo, strawberry+blueberry+pina.colada +plain+mixed.berry)

ggplot(data=yo, aes(x=time, y=price))+
  geom_jitter(alpha=1/20)

set.seed(1234)
sample.ids=sample(levels(yo$id),16)
ggplot(data=subset(yo, id %in% sample.ids), aes(x=time, y=price))+
  facet_wrap(~id)+
  geom_point(aes(size=all.purchases))+
  geom_line()

library(GGally)
set.seed(1234)
ggpairs(sample_n(diamonds,1000))

#heat map
nci=read.table("nci.tsv")
str(nci)
colnames(nci)
library(reshape2)
nci.long.samp <- melt(as.matrix(nci[1:200,]))
names(nci.long.samp) <- c("gene", "case", "value")
head(nci.long.samp)
ggplot(aes(y = gene, x = case, fill = value),
       data = nci.long.samp) +
  geom_tile() +
  scale_fill_gradientn(colours = colorRampPalette(c("blue", "red"))(100))
