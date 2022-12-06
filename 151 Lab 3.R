#install plotly
install.packages("plotly")
yes
library(plotly)
install.packages("gapminder")

library(gapminder)
head(gapminder)

#interactive graph of PE risj
p<- ggplot(PErisk, aes(x=barb2, y=gdpw2)) + 
  geom_point()+ geom_smooth(method="lm", se=F)
p
ggplotly(p)

#interactive graph of total bill vs. tip
q<- ggplot(tips, aes(x=totbill, y=tip)) + 
  geom_point(aes(col=smoker)) +geom_smooth(aes(color=smoker), method="lm", se=F)
q
ggplotly(q)

#interactive graph of total bill vs. tip framed for day
r<- ggplot(tips, aes(x=totbill, y=tip)) + 
  geom_point(aes(col=smoker, frame=day))
r
ggplotly(r)

table(tips$day)
tips$day <- factor(as.character(tips$day), levels=c("Thu", "Fri", "Sat", "Sun"))

r<- ggplot(tips, aes(x=totbill, y=tip)) + 
  geom_point(aes(col=smoker, frame=day))
r
ggplotly(r) %>%
  animation_opts(transition=500, easing="linear", mode="immediate")

#practicing animation options
r<- ggplot(tips, aes(x=totbill, y=tip)) + 
  geom_point(aes(frame=day, ids=obs))
ggplotly(r) %>%
  animation_opts(transition=500, easing="elastic-in-out", mode="next")
 
tipssmall<- tips %>% sample_n(100)
