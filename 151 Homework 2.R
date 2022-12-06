ggplot(gapminder[gapminder$year==2007], aes(x=pop, y=lifeExp)) +
  geom_point()

?gapminder

gapminder2007<-subset(gapminder, subset=gapminder$year==2007)
plot_ly(data=gapminder2007, x= ~pop, y= ~lifeExp, color = ~continent)

#(3 points) For the data `gapminder`, make a `plotly` graph 
#between `gapPercap` and `lifeExp`, set color to the variable
#continent, size to the population, add linear smooth lines to 
#different continent, show them at each year by animation 
#(set frame to the `year`).
p<-plot_ly(data=gapminder, x=~gdpPercap, y=~lifeExp, color=~continent, size=~pop, frame=~year) %>% 
  add_lines(x=~gdpPercap, y= ~lifeExp, color=~continent)
p
      
#For the data `txhousing`, make a line graph to show monthly median house 
#price among all Texan cities, choose your two favorite cities, and adding 
#two lines to highlight these two cities. Moreover, for the same graph, add 
#another two lines to show the maximum and minimum of monthly median house 
#price among all Texan cities by date.

txhousing %>%
  group_by(city) %>%
  plot_ly(x=~date, y=~median) %>%
  add_lines(name="Austin", color=I('green'))

txhousing$city
%>%
  add_lines(name="Texan cities", alpha=I(0.2)) %>%  #first trace: add one line per city
  filter(city=="Houston") %>%
  add_lines(name="Houston", color=I("red")) 
