knitr::opts_chunk$set(echo = TRUE)
library(babynames)
library(tidyverse)
library(nycflights13)
library(plotly)

names_by_year <- babynames %>% group_by(sex) %>% count(year, sort=T)
names_by_yearplot <- ggplot(test, aes(year, n, color=sex)) +
  geom_line()
ggplotly(names_by_yearplot)

babycounts <- babynames %>% group_by(name, sex) %>%
  summarize(count = as.numeric(sum(n)))

namesrank <- babycounts %>% group_by(name, sex) %>%
  summarise(count=sum(count)) %>%
  group_by(sex) %>%
  mutate(rank = min_rank(desc(count))) %>%
  filter(rank < 6) %>%
  arrange(sex, rank) %>%
  select(name, sex, count)

print(namesrank)

plot_ly(data=namesrank, x=~name, y=~count, color=~sex, type = 'bar')

summary(babynames1)

flightavgs <- flights %>% group_by(dest) %>%
  summarise(avg_distance=mean(distance, na.rm = T), avg_arr_delay=mean(arr_delay, na.rm = T)) 

totalflights <- flights %>% group_by(dest) %>% count(dest)

flightdata <- merge(flightavgs, totalflights, by="dest")

flightdata <- flightdata %>%
  filter(!dest=="HNL") %>% 
  filter(!n < 20)

head(flightdata)

flightplot<- ggplot(flightdata, aes(x=avg_distance, y=avg_arr_delay, size=n))+ geom_point(alpha=.5)+ geom_smooth(method="lm", se=F)
flightplot
