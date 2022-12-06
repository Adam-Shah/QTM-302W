install.packages("ggplot2")
install.packages("ggthemes")
library(ggplot2)
library(ggthemes)
?txhousing
str(txhousing)
txhousing[743,1]
austin_housing<-subset(txhousing, subset=txhousing$city=="Austin")
ggplot(austin_housing, aes(x=year, y=median))+
  geom_point(color="red")+
  geom_point(data=sanantonio_housing, aes(x=year, y=median), color = "blue")
  labs(x="Year", y="Price", title="Figure 1: Austin median house prices by year")+
  theme_grey()+
  scale_color_calc(name="City", red="Austin", blue="San Antonio")


txhousing[6900,1]
sanantonio_housing<-subset(txhousing,subset=txhousing$city=="San Antonio")
ggplot(sanantonio_housing, aes(x=year, y=median))+
  geom_point()+
  labs(x="Year", y="Price", title="Figure 2: San Antonio median house prices by year")+
  theme_grey()


