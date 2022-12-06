install.packages('Hmisc')
library(Hmisc)
mydata <- read.xport("c:/RXQ_RX_J.xpt")

Demographics <- read.xport("~/Downloads/DEMO_J.xpt")
Prescription <- read.xport("~/Downloads/RXQ_RX_J.xpt")

Demographics <- Demographics %>%
  select(SEQN, INDFMPIR, RIAGENDR)
Prescription <- Prescription %>%
  select(SEQN, RXDDAYS, RXDCOUNT)

DF <- full_join(Demographics, Prescription, by="SEQN")

ggplot(data=DF) +
  geom_point(aes(x=INDFMPIR, y=RXDDAYS), color="skyblue") + 
  geom_smooth(aes(x=INDFMPIR, y=RXDCOUNT), method=lm, color="black") +
  labs(title = "Days taken medicine vs. Family income", 
       x= "Number of Days taken Medicine", y="Ratio of Family Income to Poverty")+
  theme_classic()

ggplot(data=DF) +
  geom_point(aes(x=INDFMPIR, y=RXDCOUNT), color="skyblue") +
  geom_smooth(aes(x=INDFMPIR, y=RXDCOUNT), method=lm, color="black") +
  labs(title = "Number of currently prescribed medicines vs. Family income", 
       x= "Number of Prescription Medicines taken", y="Ratio of Family Income to Poverty")+
  theme_classic()