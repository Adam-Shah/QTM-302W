library(dplyr)
library(ggplot2)
library(plotly)
library(ggpubr)

abortion_data <- read.csv("abortion_mortality.csv")
head(abortion_data$Age.Group)
head(abortion_data,30)

all_age_groups <- subset(abortion_data, abortion_data$Age.Group == "[All]" & 
                           abortion_data$Sex == "All")

head(all_age_groups,200)
summary(all_age_groups$Percentage.of.cause.specific.deaths.out.of.total.deaths)

hexcodes<-c("#ea3d2f","#1cb0ca","#428ac9","#a52a29","#ffe100")

all_age_groups %>% 
  filter(#Country.Name == "Egypt" | #1
        Country.Name == "Philippines" | #1
        Country.Name == "Brazil" | #2
       # Country.Name == "Lebanon" |
        Country.Name == "United States of America" | #3
        Country.Name == "Netherlands"| #5
       Country.Name == "Japan" #| #4 
       # Country.Name == "United Kingdom" #| #4
        #Country.Name == "China, Hong Kong SAR"#5 |
        #Country.Name == "Colombia"
        ) %>%
  group_by(Country.Code) %>%
  ggplot(aes(x=Year,y=Percentage.of.cause.specific.deaths.out.of.total.deaths)) +
    geom_point(aes(col=Country.Name)) + 
    #theme(legend.position = "none") +
    ylab("% of cause specific deaths") +
    scale_color_manual(values=hexcodes)

fig <- plot_ly(all_age_groups,
    x = ~Year,
    y = ~Percentage.of.cause.specific.deaths.out.of.total.deaths,
    color = ~Country.Name,
    colors=hexcodes,
    type = 'scatter',
    mode = 'markers',
  )
fig


ourdata <- subset(all_age_groups, all_age_groups$Country.Name == "Philippines" | 
                    all_age_groups$Country.Name == "Brazil" |
                    all_age_groups$Country.Name == "United States of America" |
                    all_age_groups$Country.Name == "Japan" |
                    all_age_groups$Country.Name == "Netherlands")
summary(ourdata)
head(ourdata, 200)

ourdata <- subset(ourdata, ourdata$Year>2007)

ggplotly(ourdata %>%
  group_by(Country.Code) %>%
  ggplot(aes(x=Year,y=Percentage.of.cause.specific.deaths.out.of.total.deaths)) +
  geom_point(aes(col=Country.Name)) + theme(legend.position = "none") +
  ylab("% of cause specific deaths") +coord_cartesian(xlim=c(2007,2020))
)

#ourdata %>%
#  mutate(restrictiveness.score = factor(Country.Name)) %>%
#  rename(restrictiveness.score, '1' = Philippines,
#         '2'= Brazil, '3' = United States of America, '4'= Japan, '5'= Netherlands)

summary(ourdata)
ourdata <- ourdata %>%
  mutate(Country.Name= factor(Country.Name)) %>%
  mutate(restrictiveness.score = recode(Country.Name, "Philippines" = "1",
         "Brazil" = "2", "United States of America" = "3", "Japan" = "4", "Netherlands" = "5")) %>%
  mutate(restrictiveness.score = factor(restrictiveness.score, levels = c("1","2","3","4","5")))



summary(ourdata)
ourdata$restrictiveness_score <- as.numeric(ourdata$restrictiveness.score)
class(ourdata$restrictiveness_score)

ggplot(ourdata, aes(y=Percentage.of.cause.specific.deaths.out.of.total.deaths)) +
  geom_boxplot()

summary(ourdata$Percentage.of.cause.specific.deaths.out.of.total.deaths)

#Plot showing 
p <- ggplot(ourdata, aes(x=restrictiveness_score, y= Percentage.of.cause.specific.deaths.out.of.total.deaths)) +
  geom_boxplot(aes(col= Country.Name), show.legend = F) +
  xlab("restrictiveness score") +ylab("% of cause specific deaths")
ggplotly(p)

#New plot for poster
p <- ggplot(ourdata, aes(x=restrictiveness_score, y= Percentage.of.cause.specific.deaths.out.of.total.deaths)) +
  geom_boxplot(aes(col= Country.Name), show.legend =T) +
  xlab("restrictiveness score") +ylab("% of cause specific deaths") +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(aes(label=..rr.label..), label.x=2, label.y=0.03)+
  stat_cor(method="pearson", label.x=2) +
  scale_color_manual(values=hexcodes)
p
ggplotly(p)

plot <- ourdata %>% plot_ly(x = ~restrictiveness_score, 
       y= ~Percentage.of.cause.specific.deaths.out.of.total.deaths,
       color = ~Country.Name,
       text = ~paste("Country:", Country.Name),
       type = "box")
plot

#  geom_smooth(method="lm", se= F, formula= y~exp((-1)*x)) +
#  stat_regline_equation(formula= y~exp((-1)*x)) +
#  stat_cor(label.y=0.03) 
#  stat_compare_means(aes(group=restrictiveness_score), method = "t.test",  
#                     hide.ns =T, paired=F) +
#  geom_text()

#look at distributions by age group
str(abortion_data$Age.Group)

#agegroupdata <- subset(abortion_data, abortion_data$Country.Name == "Philippines" | 
#                         abortion_data$Country.Name == "Brazil" |
#                         abortion_data$Country.Name == "United States of America" |
#                         abortion_data$Country.Name == "Japan" |
#                         abortion_data$Country.Name == "Netherlands")
#ggplot(agegroupdata, aes(x=Age.Group, y=Percentage.of.cause.specific.deaths.out.of.total.deaths)) +
#  geom_bar(aes(col=Country.Name))

summary(abortion_data)
agegroupdata <-abortion_data %>% 
  mutate(Age_Group = recode(Age.Group,
                            "[0]" = "[<20]",
                            "[1-4]" = "[<20]",
                            "[5-9]"= "[<20]",
                            "[10-14]"= "[<20]",
                            "[15-19]"= "[<20]",
                            "[20-24]"= "[20-29]",
                            "[25-29]"= "[20-29]",
                            "[30-34]"= "[30-39]",
                            "[35-39]"= "[30-39]",
                            "[40-44]"="[40-49]",
                            "[45-49]"= "[40-49]",
                            "[50-54]"="[50-59]",
                            "[55-59]"= "[50-59]",
                            "[60-64]"= "[60+]",
                            "[65-69]"= "[60+]",
                            "[70-74]"= "[60+]",
                            "[75-79]"= "[60+]",
                            "[80-84]"= "[60+]",
                            "[85+]"= "[60+]")) #%>%
  select(Age.Group, Age_Group)
agegroupdata <- subset(agegroupdata, abortion_data$Country.Name == "Philippines" | 
                         abortion_data$Country.Name == "Brazil" |
                         abortion_data$Country.Name == "United States of America" |
                         abortion_data$Country.Name == "Japan" |
                         abortion_data$Country.Name == "Netherlands")
agegroupdata <- subset(agegroupdata, agegroupdata$Year>2007)
agegroupdata <- subset(agegroupdata, agegroupdata$Age.Group != "[All]")
agegroupdata <- subset(agegroupdata, agegroupdata$Age.Group != "[Unknown]")
agegroupdata <- agegroupdata %>%
  mutate(Country.Name= factor(Country.Name)) %>%
  mutate(restrictiveness.score = recode(Country.Name, "Philippines" = "1",
                                        "Brazil" = "2", "United States of America" = "3", "Japan" = "4", "Netherlands" = "5")) %>%
  mutate(restrictiveness.score = factor(restrictiveness.score, levels = c("1","2","3","4","5")))


ggplot(agegroupdata, aes(x=Age_Group, y=Percentage.of.cause.specific.deaths.out.of.total.deaths)) +
  geom_boxplot(aes(col=restrictiveness.score), show.legend = T) +
  ylab("% of cause specific deaths") + xlab("Age Group") +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(3, 3, 3, 3)
  ) + labs(col="Restrictiveness Score")




