#In this document, I worked on a few other analyses relevant to my project-
#I calculated ICC of the different VA measures, correlations in healthy volunteers
#and with NeurEx panels, differences between diagnostic groups, and calculated
#the yearly change for patients in AUC 
#APS

library(ggplot2)
library(dplyr)
library(plotly)
library(mlbench)
library(glmnet)
library(ggpubr)
library(corrplot)
library(Hmisc)
library(reshape2)
library(caret)
install.packages("forcats")
library(forcats)
library(lubridate)

final_vision_data <- read.csv("final_vision_data.csv")

VA_subset <- subset(final_vision_data, select= c("VA_1.25_contrast_OU", 
                                                 "VA_2.5_contrast_OU",
                                                 "VA_100_contrast_OU",
                                                 "condition2", "lcva_ID"))
VA_subset <- na.omit(VA_subset)

n <- count(VA_subset)[1,1]
res <- rep(NA, n)
slo <- rep(NA, n)
int <- rep(NA, n)
auc <- rep(NA, n)

for (i in 1:n){
  patient <- VA_subset[i,]
  VA_OUsubset <- subset(patient, select = c("VA_1.25_contrast_OU", 
                                            "VA_2.5_contrast_OU", 
                                            "VA_100_contrast_OU",
                                            "condition2"))
  VA_OUsubset2 <- rename(VA_OUsubset, "33.33333" = VA_1.25_contrast_OU, 
                         "52.63158" = VA_2.5_contrast_OU, 
                         "100"=VA_100_contrast_OU)
  VA_OUsubsetlong2 <- melt(VA_OUsubset2, id="condition2")
  VA_OUsubsetlong2 <- rename(VA_OUsubsetlong2, scaled_contrast = variable, score=value)
  VA_OUsubsetlong2$scaled_contrast <- as.numeric(as.character(VA_OUsubsetlong2$scaled_contrast))
  
  r <- summary(lm(score~scaled_contrast, VA_OUsubsetlong2))
  res[i] <- r$r.squared
  slo[i] <- r$coefficients[2,1]
  int[i] <- r$coefficients[1,1]
  auc[i] <- 0.5*VA_subset[i,"VA_100_contrast_OU"]*
    (100+(r$coefficients[1,1]/r$coefficients[2,1]))
}

summary(res)
head(res,400)
tail(res,400)
summary(int)
head(int)
summary(auc)
head(auc)
summary(slo)
VA_subset$VA_rsquared <- res
VA_subset$VA_yint <- int
VA_subset$VA_slope <- slo
VA_subset$VA_AUC <- auc

summary(VA_subset)
summary(VA_subset$VA_AUC)

VA_subset <- subset(VA_subset, select= c("lcva_ID",
                                         "VA_rsquared",
                                         "VA_yint",
                                         "VA_slope",
                                         "VA_AUC"))
final_vision_data <- full_join(final_vision_data, VA_subset, by="lcva_ID")
summary(final_vision_data)

#Assess Lin's concordance correlation coefficient comparing VA AUC to 1.25% contrast
?CCC()

install.packages("DescTools")
library(DescTools)
?CCC()

VA_subset <- subset(final_vision_data, select = c("patientID","lcva_ID", "VA_1.25_contrast_OU",
                                                  "VA_2.5_contrast_OU",
                                                  "VA_100_contrast_OU",
                                                  "VA_slope", "VA_yint",
                                                  "VA_AUC"))
VA_subset <- na.omit(VA_subset)
                                                  
ccc <- CCC(VA_subset$VA_1.25_contrast_OU, VA_subset$VA_AUC, ci="z-transform",
           conf.level = 0.95)

ccc$l.shift
ccc$s.shift
ccc$rho.c
ccc$C.b
ccc$blalt

#I think it's supposed to be ICC (Intraclass Correlation Coefficient)
install.packages("multilevel")
library(multilevel)
?mult.icc()

ICCdata <- subset(final_vision_data, select= c("VA_1.25_contrast_OU",
                                       "VA_2.5_contrast_OU",
                                       "VA_100_contrast_OU",
                                         "VA_yint",
                                         "VA_slope",
                                         "VA_AUC", "patientID"))
ICCdata <- rename(ICCdata, AUC=VA_AUC, "1.25_contrast"= VA_1.25_contrast_OU,
                  "2.5_contrast" = VA_2.5_contrast_OU, "100_contrast"=VA_100_contrast_OU,
                  slope=VA_slope, intercept= VA_yint)

results <- mult.icc(ICCdata[,c("AUC",
                                 "1.25_contrast", 
                                 "2.5_contrast", 
                                 "100_contrast", 
                                 "slope", "intercept"
                                 )], 
                    grpid= ICCdata$patientID)
results
ggplot(results, aes(x=Variable, y=ICC1)) + 
  geom_bar(stat="identity", aes(fill=Variable))
summary(VA_subset)
ggplot(results, aes(x=ICC1, y=ICC2))+geom_point() +
  geom_smooth(method="lm")+
  stat_regline_equation() +
  stat_cor(method="spearman", label.y=0.87)

results %>%
  mutate(Variable = fct_reorder(Variable, ICC1)) %>%
  ggplot(aes(x=Variable, y=ICC1)) +
  geom_bar(stat="identity", aes(fill=Variable), alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw() +
  theme(legend.position = "none") +
  geom_text(stat="identity", aes(label = round(ICC1, digits=3)), 
            vjust = 0.5, hjust = 1.1, colour = "black") 
?geom_text

#correlations in Healthy
healthydonors <- subset(final_vision_data, condition2 == "Healthy")

ggplot(final_vision_data, aes(x=Age, y=VA_AUC)) +
  geom_point(aes(col=condition2)) +
  stat_cor(method="spearman")+
  stat_regline_equation(aes(label=..rr.label..), label.y=5000)
ggplot(healthydonors, aes(x=Age, y=VA_AUC)) +
  geom_point() +
  stat_cor(method="spearman")+
  stat_regline_equation(aes(label=..rr.label..), label.y=5000) +
  stat_regline_equation(label.y=5250)+
  geom_smooth(method="lm", se=T)

ggplot(healthydonors, aes(x=Age, y=VA_yint)) +
  geom_point() +
  stat_cor(method="spearman")+
  stat_regline_equation(aes(label=..rr.label..),label.y=20) +
  stat_regline_equation(label.y=30)+
  geom_smooth(method="lm", se=T)

ggplot(healthydonors, aes(x=Age, y=VA_1.25_contrast_OU)) +
  geom_point() +
  stat_cor(method="spearman")+
  stat_regline_equation(aes(label=..rr.label..),label.y=20) +
  stat_regline_equation(label.y=30)+
  geom_smooth(method="lm", se=T)

ggplot(healthydonors, aes(x=Age, y=VA_2.5_contrast_OU)) +
  geom_point() +
  stat_cor(method="spearman")+
  stat_regline_equation(aes(label=..rr.label..),label.y=20) +
  stat_regline_equation(label.y=30)+
  geom_smooth(method="lm", se=T)

ggplot(healthydonors, aes(x=Age, y=VA_100_contrast_OU)) +
  geom_point() +
  stat_cor(method="spearman")+
  stat_regline_equation(aes(label=..rr.label..),label.y=20) +
  stat_regline_equation(label.y=30)+
  geom_smooth(method="lm", se=T)

ggplot(healthydonors, aes(x=Age, y=VA_slope)) +
  geom_point() +
  stat_cor(method="spearman")+
  stat_regline_equation(aes(label=..rr.label..),label.y=1) +
  stat_regline_equation(label.y=0.92)+
  geom_smooth(method="lm", se=T)

#Try with "normal eyes"
summary(normeyes_new)

ggplot(normeyes_new, aes(x=Age, y=VA_AUC)) +
  geom_point(aes(col=diagnosis)) +
  stat_cor(method="spearman") +
  stat_regline_equation(aes(label=..rr.label..), label.y=5000)+
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=4500)

ggplot(normeyes_new, aes(x=Age, y=VA_1.25_contrast_OU)) +
  geom_point(aes(col=diagnosis)) +
  stat_cor(method="spearman") +
  stat_regline_equation(aes(label=..rr.label..),label.y=35)+
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=30)

ggplot(normeyes_new, aes(x=Age, y=VA_2.5_contrast_OU)) +
  geom_point(aes(col=diagnosis)) +
  stat_cor(method="spearman") +
  stat_regline_equation(aes(label=..rr.label..),label.y=35)+
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=30)

ggplot(normeyes_new, aes(x=Age, y=VA_100_contrast_OU)) +
  geom_point(aes(col=diagnosis)) +
  stat_cor(method="spearman") +
  stat_regline_equation(aes(label=..rr.label..),label.y=35)+
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=30)

ggplot(normeyes_new, aes(x=Age, y=VA_yint)) +
  geom_point(aes(col=diagnosis)) +
  stat_cor(method="spearman") +
  stat_regline_equation(aes(label=..rr.label..),label.y=35)+
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=30)

ggplot(normeyes_new, aes(x=Age, y=VA_slope)) +
  geom_point(aes(col=diagnosis)) +
  stat_cor(method="spearman") +
  stat_regline_equation(aes(label=..rr.label..),label.y=35)+
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=30)

#Correlations with NeurEx panels
summary(final_vision_data)
final_vision_data$NeurEx_Panel_02_B <- (final_vision_data$NeurEx_Panel_02_L +
  final_vision_data$NeurEx_Panel_02_R)/2
final_vision_data$NeurEx_Panel_03_B <- (final_vision_data$NeurEx_Panel_03_L +
                                          final_vision_data$NeurEx_Panel_03_R)/2
final_vision_data$NeurEx_Panel_04_B <- (final_vision_data$NeurEx_Panel_04_L +
                                          final_vision_data$NeurEx_Panel_04_R)/2

ggplot(final_vision_data, aes(x=NeurEx_Panel_02_B, y=VA_AUC)) +
  geom_point(aes(col=condition2)) +
  stat_cor(method="spearman", label.x=5, label.y= 5000) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.x=6, label.y=4000)+
  stat_regline_equation(aes(label=..rr.label..), label.x=6, label.y=4500)
ggplot(final_vision_data, aes(x=NeurEx_Panel_03_B, y=VA_AUC)) +
  geom_point(aes(col=condition2)) +
  stat_cor(method="spearman", label.x=2.5, label.y= 5000) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.x=5, label.y=4000)+
  stat_regline_equation(aes(label=..rr.label..), label.x=5, label.y=4500)
ggplot(final_vision_data, aes(x=NeurEx_Panel_04_B, y=VA_AUC)) +
  geom_point(aes(col=condition2)) +
  stat_cor(method="spearman", label.x=3, label.y= 5000) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.x=3, label.y=4000)+
  stat_regline_equation(aes(label=..rr.label..), label.x=3, label.y=4500)

#correlation table
VAcorr <- subset(final_vision_data, select = c("Age", "NeurEx_Panel_02_B",
                                               "NeurEx_Panel_03_B","NeurEx_Panel_04_B",
                                               "VA_1.25_contrast_OU", "VA_2.5_contrast_OU",
                                               "VA_100_contrast_OU", "VA_AUC", 
                                               "VA_yint", "VA_slope"))

VAcorr <- rename(VAcorr, Vision = NeurEx_Panel_02_B, 
                 Eye_Movement = NeurEx_Panel_03_B,
                 Visual_Field = NeurEx_Panel_04_B, 
                 "1.25_contrast" = VA_1.25_contrast_OU, 
                 "2.5_contrast" = VA_2.5_contrast_OU,
                 "100_contrast" = VA_100_contrast_OU,
                 AUC = VA_AUC, intercept = VA_yint, slope= VA_slope)

library(Hmisc)
VAcorrMatrix <- rcorr(as.matrix(VAcorr), type="spearman")
VAcorrMatrix

VAcorrMatrix$r #extract correlation coefficients
VAcorrMatrix$n 
VAcorrMatrix$P #extract p-values

library(corrplot)
corrplot(VAcorrMatrix$r, method="number", type="upper",
         p.mat=VAcorrMatrix$P, sig.level = 0.05, insig="blank", 
         tl.cex=0.5,tl.srt=60, number.cex=0.8)

?corrplot()
#differences between diagnostic groups
ggplot(final_vision_data, aes(x=condition2, y=VA_AUC)) +
  geom_boxplot(fill="grey")

ggplot(final_vision_data, aes(x=condition2, y=VA_yint)) +
  geom_boxplot(fill="grey")

ggplot(final_vision_data, aes(x=condition2, y=VA_slope)) +
  geom_boxplot(fill="grey")

ggplot(final_vision_data, aes(x=condition2, y=VA_1.25_contrast_OU)) +
  geom_boxplot(fill="grey")

ggplot(final_vision_data, aes(x=condition2, y=VA_2.5_contrast_OU)) +
  geom_boxplot(fill="grey")

ggplot(final_vision_data, aes(x=condition2, y=VA_100_contrast_OU)) +
  geom_boxplot(fill="grey")

#progression - yearly change
final_vision_data %>%
  group_by(patientID) %>%
  summarise(delta = min_rank(lcva_date))

str(final_vision_data$lcva_date)
final_vision_data$lcva_date <- as.Date(final_vision_data$lcva_date)
str(final_vision_data$lcva_date)

#first try
data<- final_vision_data %>%
  group_by(patientID) %>%
  mutate(lcva_day = as.numeric(lcva_date)) %>%
  arrange(lcva_day) %>%
  mutate(delta = lcva_day-lag(lcva_day)) %>%
  mutate(AUCchange = VA_AUC-lag(VA_AUC)) %>%
  mutate(yearlychange = (AUCchange/delta)*365)
#this isn't right- need to run a regression for each patient

summary(final_vision_data$lcva_date)

summary(data)
length(unique(data$patientID))
data2 <- subset(data, select=c("patientID", "lcva_date", "VA_AUC", "yearlychange"))
summary(data2)
tail(data2,100)

ggplot(data, aes(x=condition2, y=yearlychange)) +
  geom_boxplot()

summary(lm(VA_AUC~Age, data=final_vision_data))$coefficients[2,1]


final_vision_data[1, "VA_AUC"]
final_vision_data[2, "VA_AUC"] -final_vision_data[1, "VA_AUC"]
final_vision_data[2, "lcva_date"] - final_vision_data[1, "lcva_date"]

as.numeric(final_vision_data$lcva_date)
summary(final_vision_data)

#second try
progressiondata <- subset(final_vision_data, select= c("lcva_ID", "VA_AUC", 
                                            "lcva_date", "patientID", "condition2"))
progressiondata <- na.omit(progressiondata)
data<- progressiondata %>%
  group_by(patientID) %>%
  filter(n()>1) %>%
  mutate(lcva_day = as.numeric(as.Date(lcva_date))) %>%
  summarise(yearly_change = (summary(lm(VA_AUC~lcva_day))$coefficients[2,1])*365)
#that works

data
mean(data$yearly_change)

full_prog_data <- full_join(final_vision_data, data, by="patientID")
head(full_prog_data)
full_prog_data[204,]
?full_join

length(unique(full_prog_data$patientID))
unique(full_prog_data$patientID)

full_prog_data %>%
  group_by(patientID) %>%
  ggplot(aes(x=condition2, y=yearly_change)) +
  geom_boxplot()

max(full_prog_data$yearly_change)
min(full_prog_data$yearly_change)
summary(full_prog_data$yearly_change)
head(full_prog_data$VA_AUC)
head(full_prog_data$yearly_change)
head(full_prog_data$lcva_date)

test <- subset(full_prog_data, subset= full_prog_data$yearly_change > 3500)
test

#try again with only patients with at least 3 time points
progressiondata <- subset(final_vision_data, select= c("VA_AUC", 
                                                       "lcva_date", "patientID"))
progressiondata <- na.omit(progressiondata)
data<- progressiondata %>%
  group_by(patientID) %>%
  filter(n()>2) %>%
  mutate(lcva_day = as.numeric(as.Date(lcva_date))) %>%
  summarise(yearly_change = (summary(lm(VA_AUC~lcva_day))$coefficients[2,1])*365)
data
mean(data$yearly_change)

full_prog_data <- full_join(final_vision_data, data, by="patientID")

full_prog_data %>%
  group_by(patientID) %>%
  ggplot(aes(x=condition2, y=yearly_change)) +
  geom_boxplot() 

head(full_prog_data %>%
          group_by(patientID))
head(full_prog_data)

data1 <- subset(full_prog_data, select=c("patientID", "condition2", "yearly_change"))
head(data1)
head(unique(data1))

ggplot(unique(data1), aes(x=condition2, y=yearly_change)) +
  geom_boxplot()
