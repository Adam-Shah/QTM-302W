library(tidyverse)
install.packages('maps')
install.packages('ggmap')
library(nycflights13)
library(maps)
library(ggmap)

GAdat<-read.csv("https://raw.githubusercontent.com/umbertomig/qtm151/main/datasets/GAdat.csv")
GAdat$County <- tolower(GAdat$County)
head(GAdat, 2)

data1<-flights %>%
  drop_na() %>%
  group_by(dest) %>%
  summarise(average=mean(arr_delay)) %>%
  left_join(airports, by=c("dest"="faa"))
head(data1, 2)

us <- c(left = -125, bottom = 25, right = -67, top = 49)
map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite")
ggmap(map)  # create a layer US map

ggmap(map) + geom_point(data=data1, 
                        aes(x=lon, y=lat, color=average, size=average), na.rm = T) +
  scale_color_gradient(low = "green", high="darkblue")

head(flights, 15)
data2<-flights %>%
  drop_na() %>%
  group_by(dest) %>%
  summarise(average=mean(distance)) %>%
  left_join(airports, by=c("dest"="faa"))
head(data2, 15)

ggmap(map) + geom_point(data=data2, 
                        aes(x=lon, y=lat, color=average, size=average), na.rm = T) +
  scale_color_gradient(low = "green", high="darkblue")

data3<-flights %>%
  drop_na() %>% filter(carrier == 'DL') %>%
  group_by(dest) %>%
  summarise(sum=n()) %>%
  left_join(airports, by=c("dest"="faa")) 
head(data3, 2)

ggmap(map) + geom_point(data=data3, 
                        aes(x=lon, y=lat, color=sum, size=sum), na.rm = T) +
  scale_color_gradient(low = "green", high="darkblue")

states<-map_data("state")  
head(states, 100)

qplot(long, lat, data=states) #wrong
qplot(long, lat, data=states, geom='path') #wrong
qplot(long, lat, data=states, geom='path', group=group) #right
qplot(long, lat, data=states, geom="polygon", group=group)

ggplot(states)+
  geom_polygon(aes(x=long, y=lat, group=group), color="cyan", fill="gray")

qplot(long, lat, data=states, geom="polygon", 
      group=group, fill=long)

states <- map_data("state")
ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)

west_coast <- subset(states, region %in% c("california", "oregon", "washington"))

ggplot(data = west_coast) + 
  geom_polygon(aes(x = long, y = lat, group=group), fill = "palegreen", 
               color = "black") +
  coord_fixed(1.3) # so the scale/stretch is correct

colonies <- subset(states, region %in% c('delaware', 'pennsylvania', 'new york', 'massachusetts', 'new jersey', 'north carolina', 'south carolina', 'virginia', 'new hampshire', 'connecticut', 'rhode island', 'maryland', 'georgia'))
ggplot(data = colonies) + 
  geom_polygon(aes(x = long, y = lat, group=group), fill = "palegreen", 
               color = "black") +
  coord_fixed(1.3)

ga_df <- states %>%
  filter(region == "georgia")
ggplot(data = ga_df) + 
  geom_polygon(aes(x = long, y = lat), fill = "palegreen", color = "black")
ggplot(data = ga_df) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") + 
  coord_quickmap()

pa_df <- states %>% filter(region == 'pennsylvania')
ggplot(data = pa_df) + 
  geom_polygon(aes(x = long, y = lat), fill = "palegreen", color = "black")
ggplot(data = pa_df) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") + 
  coord_quickmap() + theme_void()

county_df <- map_data("county") %>% filter(region == "georgia")
state_df <- map_data("state") %>% filter(region == "georgia")
ga_base <- ggplot(data = ga_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_quickmap() + 
  geom_polygon(color = "black", fill = "gray")

ga_base + theme_void() + 
  geom_polygon(data = county_df, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)

county_df$subregion <- replace(county_df$subregion, county_df$subregion=="de kalb", "dekalb")
mapdat <- left_join(GAdat,county_df, by = c("County"="subregion"))
p <- ggplot(mapdat, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = Population), color="yellow")  +
  scale_fill_gradient(low="blue", high="red")+
  geom_polygon(data = state_df, color = "black", fill = NA) + 
  theme_void() +
  coord_fixed(1.2) 
p

ggplot(mapdat, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = Population, color="yellow"),
               colour = alpha("red", 1/2))  +
  geom_polygon(data = state_df, colour = "black", fill = NA) + 
  theme_void() + coord_fixed(1.2) +
  scale_fill_gradientn(colours = rev(rainbow(7)),
                       breaks = c(2, 4, 10, 100, 1000, 10000), trans = "log10")

county_df$subregion <- replace(county_df$subregion, county_df$subregion=="de kalb", "dekalb")

mapdat <- left_join(GAdat,county_df, by = c("County"="subregion"))

p <- ggplot(mapdat, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = Population), color="yellow")  +
  scale_fill_gradient(low="blue", high="red")+
  geom_polygon(data = state_df, colour = "black", fill = NA) + 
  theme_void() +
  coord_fixed(1.2) 
p
