# pie chart
ggplot(diamonds, aes(x=factor(1), fill=cut))+
  geom_bar(width = 1)+
  coord_polar(theta = "y")
# 
ggplot(diamonds, aes(x=factor(1), fill=cut))+
  geom_bar(width=1)+
  coord_polar(theta = "x")
# rose chart
ggplot(diamonds, aes(x=cut))+
  geom_bar(width = 0.95)+
  coord_polar(theta = "x")
