#In this document, I looked for correlations between different regions in RNFL
#(in same eye and across eyes), created correlation tables for all data, non-MS,
#and healthy patients, and looked for other RNFL relationships with height, Age, 
#and RNFL machine used
#APS

library(dplyr)
library(ggplot2)
library(plotly)
library(ggpubr)

final_RNFL_data <- read.csv("final_vision_data.csv")
summary(final_RNFL_data)

#RNFL correlations in same eye
ggplot(final_RNFL_data, aes(x=RNFL_OD_Inferior, y=RNFL_OD_Superior)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson") +
  geom_smooth(method="lm") +
  stat_regline_equation(label.y=185) +
  geom_abline(slope=1, intercept = 0, alpha=0.5) +
  coord_cartesian(xlim=c(40,200), ylim=c(40,200))

#force regression line through 0
model1 <- lm(RNFL_OD_Superior ~ 0 + RNFL_OD_Inferior, data=final_RNFL_data)
summary(model1)
ggplot(final_RNFL_data, aes(x=RNFL_OD_Inferior, y=RNFL_OD_Superior)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson") +
  geom_abline(intercept=0, slope=model1$coefficients[1], color='blue', show.legend = T) +
  stat_regline_equation(label.y=185, formula = y~x-1) +
  geom_smooth(method="lm",se= F, formula=y~x-1) +
  geom_abline(slope=1, intercept = 0, alpha=0.5) +
  coord_cartesian(xlim=c(40,200), ylim=c(40,200))

model1 <- lm(RNFL_OD_Superior ~ RNFL_OD_Inferior, data=final_RNFL_data)
sum((model1$residuals)^2)
model2 <- lm(RNFL_OD_Superior ~ 0 + RNFL_OD_Inferior, data=final_RNFL_data)
sum((model2$residuals)^2)


ggplot(final_RNFL_data, aes(x=RNFL_OD_Superior, y=RNFL_OD_Inferior)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson") +
  geom_smooth(method="lm", formula=y~x-1) +
  stat_regline_equation(label.y=185, formula=y~x-1) +
  geom_abline(slope=1, intercept = 0, alpha=0.5)

#Trying to investigate properties of regression through origin vs. regular
model1 <- lm(RNFL_OD_Inferior ~ RNFL_OD_Superior, data=final_RNFL_data)
summary(model1)
deviance(model1)
sum((model1$residuals)^2)
sum(model1$residuals)
model2 <- lm(RNFL_OD_Superior ~ RNFL_OD_Inferior, data=final_RNFL_data)
sum(model2$residuals)
sum((model2$residuals)^2)

?nlm
?lm

ggplot(final_RNFL_data, aes(x=RNFL_OD_Temporal, y=RNFL_OD_Nasal)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson", label.x=60) +
  geom_smooth(method="lm", se=F,formula = y~x-1) +
  stat_regline_equation(label.x=60, label.y=160, formula = y~x-1) +
  geom_abline(slope=1, intercept=0, alpha=0.2)
model1 <- lm(RNFL_OD_Nasal ~ RNFL_OD_Temporal, data=final_RNFL_data)
sum((model1$residuals)^2)
model2 <- lm(RNFL_OD_Nasal ~ 0 + RNFL_OD_Temporal, data=final_RNFL_data)
sum((model2$residuals)^2)

ggplot(final_RNFL_data, aes(x=RNFL_OD_Nasal, y=RNFL_OD_Inferior)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson", label.x=0) +
  geom_smooth(method="lm", se=F, formula=y~x-1) +
  stat_regline_equation(label.x=0, label.y=182, formula=y~x-1) +
  geom_abline(slope=1, intercept = 0, alpha = 0.2)

ggplot(final_RNFL_data, aes(x=RNFL_OD_Superior, y=RNFL_OD_Nasal)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson") +
  geom_smooth(method="lm", se=F, formula=y~x-1) +
  stat_regline_equation(label.y=125, formula=y~x-1) +
  geom_abline(slope=1, intercept = 0, alpha = 0.2)

ggplot(final_RNFL_data, aes(x=RNFL_OD_Superior, y=RNFL_OD_Temporal)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson") +
  geom_smooth(method="lm", se=F, formula=y~x-1) +
  stat_regline_equation(label.y=100, formula=y~x-1) +
  geom_abline(slope=1, intercept = 0, alpha = 0.2)

ggplot(final_RNFL_data, aes(x=RNFL_OD_Inferior, y=RNFL_OD_Temporal)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson") +
  geom_smooth(method="lm", se=F, formula=y~x-1) +
  stat_regline_equation(label.y=100, formula=y~x-1) +
  geom_abline(slope=1, intercept = 0, alpha = 0.2)

#Across eyes
ggplot(final_RNFL_data, aes(x=RNFL_OD_Inferior, y=RNFL_OS_Inferior)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson") +
  geom_smooth(method="lm", se=F, formula= y~x-1) +
  stat_regline_equation(label.y=160, formula=y~x-1) + 
  geom_abline(slope=1, intercept = 0, alpha=0.3)

ggplot(final_RNFL_data, aes(x=RNFL_OD_Nasal, y=RNFL_OS_Nasal)) + 
  geom_point(aes(col=diagnosis)) +
  stat_cor(method="pearson", label.y=160) +
  geom_smooth(method="lm", se=F, formula=y~x-1) +
  stat_regline_equation(label.y=150, formula=y~x-1) + 
  geom_abline(slope=1, intercept = 0, alpha=0.3)

ggplot(final_RNFL_data, aes(x=RNFL_OD_Temporal, y=RNFL_OS_Temporal)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson") +
  geom_smooth(method="lm", se=F, formula=y~x-1) +
  stat_regline_equation(label.y=115, formula=y~x-1) + 
  geom_abline(slope=1, intercept = 0, alpha=0.3) +
  coord_cartesian(xlim=c(20,125), ylim=c(20,125))

ggplot(final_RNFL_data, aes(x=RNFL_OD_Superior, y=RNFL_OS_Superior)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson", label.y=160) +
  geom_smooth(method="lm") +
  stat_regline_equation(label.y=150) + 
  geom_abline(slope=1, intercept = 0, alpha=0.5) +
  coord_cartesian(xlim=c(35,165), ylim=c(35,165))

#non-MS cohort

nonMScohort <- subset(final_RNFL_data,diagnosis == "CIS" | 
                        diagnosis == "Healthy Donor"|diagnosis ==  "NIND"|
                        diagnosis ==  "OIND"|diagnosis ==  "RIS"|diagnosis=="pOIND")
#I was marking which graphs to send to Bibi
#use this
ggplot(nonMScohort, aes(x=RNFL_OD_Inferior, y=RNFL_OD_Superior)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson") +
  geom_smooth(method="lm", se=F, formula= y~x-1) +
  stat_regline_equation(label.y=185, formula=y~x-1) +
  geom_abline(slope=1, intercept = 0, alpha=0.5)

#use this
ggplot(nonMScohort, aes(x=RNFL_OD_Temporal, y=RNFL_OD_Nasal)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson", label.x=50, label.y=30) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.x=50, label.y=25, ) +
  geom_abline(slope=0, intercept= mean(na.omit(nonMScohort$RNFL_OD_Nasal)), alpha=0.2)

ggplot(nonMScohort, aes(x=RNFL_OD_Inferior, y=RNFL_OD_Temporal)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson") +
  geom_smooth(method="lm", se=F, formula=y~x-1) +
  stat_regline_equation(label.y=100, formula = y~x-1) +
  geom_abline(slope=1, intercept = 0, alpha = 0.2)

#use this
ggplot(nonMScohort, aes(x=RNFL_OD_Inferior, y=RNFL_OS_Inferior)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson") +
  geom_smooth(method="lm", formula=y~x-1, se=F) +
  stat_regline_equation(label.y=160, formula=y~x-1) + 
  geom_abline(slope=1, intercept = 0, alpha=0.3) +
  coord_cartesian(xlim=c(40,180),ylim=c(40,180))

#use this
ggplot(nonMScohort, aes(x=RNFL_OD_Nasal, y=RNFL_OS_Nasal)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson") +
  geom_smooth(method="lm", se=F, formula=y~x-1) +
  stat_regline_equation(label.y=100, formula=y~x-1) + 
  geom_abline(slope=1, intercept = 0, alpha=0.3)

#use this
ggplot(nonMScohort, aes(x=RNFL_OD_Temporal, y=RNFL_OS_Temporal)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson") +
  geom_smooth(method="lm", se=F, formula=y~x-1) +
  stat_regline_equation(label.y=100, formula=y~x-1) + 
  geom_abline(slope=1, intercept = 0, alpha=0.3) +
  coord_cartesian(xlim=c(25,115), ylim=c(25,115))

#let me try something
ggplot(nonMScohort, aes(x=RNFL_OD_Inferior, y=RNFL_OD_Superior)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson") +
  geom_smooth(method="lm") +
  stat_regline_equation(label.y=185) +
  geom_abline(slope=1, intercept = 0, alpha=0.5) +
  coord_cartesian(xlim=c(40,200), ylim=c(40,200))

#healthy donors

healthydonors <- subset(final_RNFL_data, diagnosis == "Healthy Donor")
ggplot(healthydonors, aes(x=RNFL_OD_Inferior, y=RNFL_OD_Superior)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson") +
  geom_smooth(method="lm",se=F, formula=y~x-1) +
  stat_regline_equation(label.y=190, formula=y~x-1) +
  geom_abline(slope=1, intercept = 0)

ggplot(healthydonors, aes(x=RNFL_OD_Inferior, y=RNFL_OD_Nasal)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson") +
  geom_smooth(method="lm", se=F, formula=y~x-1) +
  stat_regline_equation(label.y=100, formula=y~x-1) + 
  geom_abline(slope=1, intercept = 0, alpha=0.4)

ggplot(healthydonors, aes(x=RNFL_OD_Temporal, y=RNFL_OD_Nasal)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson", label.x=50, label.y=53) +
  geom_smooth(method="lm", alpha=0.2) +
  stat_regline_equation(label.x=50, label.y=50) +
  geom_abline(slope=0, intercept= mean(na.omit(healthydonors$RNFL_OD_Nasal)), alpha=0.2)

ggplot(healthydonors, aes(x=RNFL_OS_Temporal, y=RNFL_OS_Nasal)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson", label.x=50, label.y=53) +
  geom_smooth(method="lm", alpha=0.2) +
  stat_regline_equation(label.x=50, label.y=50) +
  geom_abline(slope=0, intercept= mean(na.omit(healthydonors$RNFL_OS_Nasal)), alpha=0.2)

ggplot(healthydonors, aes(x=RNFL_OD_Inferior, y=RNFL_OS_Inferior)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson") +
  geom_smooth(method="lm", alpha=0.2) +
  stat_regline_equation(label.y=150) + 
  geom_abline(slope=1, intercept = 0)

ggplot(healthydonors, aes(x=RNFL_OD_Temporal, y=RNFL_OS_Temporal)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson") +
  geom_smooth(method="lm", se=F, formula=y~x-1) +
  stat_regline_equation(label.y=98, formula=y~x-1) + 
  geom_abline(slope=1, intercept = 0, alpha=0.3) +
  coord_cartesian(xlim=c(45,105), ylim=c(45,105))

ggplot(healthydonors, aes(x=RNFL_OD_Nasal, y=RNFL_OS_Nasal)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson") +
  geom_smooth(method="lm") +
  stat_regline_equation(label.y=122) + 
  geom_abline(slope=1, intercept = 0, alpha=0.3) #+
  #coord_cartesian(xlim=c(40,105), ylim=c(40,105))

#let me try something
ggplot(healthydonors, aes(x=RNFL_OD_Inferior, y=RNFL_OD_Superior)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson", label.x=75) +
  geom_smooth(method="lm") +
  stat_regline_equation(label.y=185, label.x=75) +
  geom_abline(slope=1, intercept = 0, alpha=0.5) +
  coord_cartesian(xlim=c(40,200), ylim=c(40,200))

#RNFL correlation and p-values
RNFLcorr <- subset(final_RNFL_data, select = c("RNFL_OD_Inferior", "RNFL_OD_Nasal",
                                             "RNFL_OD_Superior","RNFL_OD_Temporal",
                                             "RNFL_OS_Inferior","RNFL_OS_Nasal",
                                             "RNFL_OS_Superior","RNFL_OS_Temporal"
                                             ))
RNFLcorr <- rename(RNFLcorr, OD_I = RNFL_OD_Inferior, OD_N= RNFL_OD_Nasal,
                  OD_S= RNFL_OD_Superior, OD_T= RNFL_OD_Temporal,
                  OS_I= RNFL_OS_Inferior, OS_N= RNFL_OS_Nasal,
                  OS_S= RNFL_OS_Superior, OS_T= RNFL_OS_Temporal)
library(Hmisc)

RNFLcorrMatrix <- rcorr(as.matrix(RNFLcorr), type="pearson")
RNFLcorrMatrix

RNFLcorrMatrix$r #extract correlation coefficients
RNFLcorrMatrix$n 
RNFLcorrMatrix$P #extract p-values

library(corrplot)
corrplot(RNFLcorrMatrix$r, method="circle", type="full", order= "hclust",
         p.mat=RNFLcorrMatrix$P, sig.level = 0.05, insig="blank")

#Non-MS
RNFLcorr <- subset(nonMScohort, select = c("RNFL_OD_Inferior", "RNFL_OD_Nasal",
                                               "RNFL_OD_Superior","RNFL_OD_Temporal",
                                               "RNFL_OS_Inferior","RNFL_OS_Nasal",
                                               "RNFL_OS_Superior","RNFL_OS_Temporal"
))
RNFLcorr <- rename(RNFLcorr, OD_I = RNFL_OD_Inferior, OD_N= RNFL_OD_Nasal,
                   OD_S= RNFL_OD_Superior, OD_T= RNFL_OD_Temporal,
                   OS_I= RNFL_OS_Inferior, OS_N= RNFL_OS_Nasal,
                   OS_S= RNFL_OS_Superior, OS_T= RNFL_OS_Temporal)
library(Hmisc)

RNFLcorrMatrix <- rcorr(as.matrix(RNFLcorr), type="pearson")
RNFLcorrMatrix

RNFLcorrMatrix$r #extract correlation coefficients
RNFLcorrMatrix$n 
RNFLcorrMatrix$P #extract p-values

library(corrplot)
corrplot(RNFLcorrMatrix$r, method="circle", type="full", order= "hclust",
         p.mat=RNFLcorrMatrix$P, sig.level = 0.05, insig="blank")

#healthy donors
RNFLcorr <- subset(healthydonors, select = c("RNFL_OD_Inferior", "RNFL_OD_Nasal",
                                               "RNFL_OD_Superior","RNFL_OD_Temporal",
                                               "RNFL_OS_Inferior","RNFL_OS_Nasal",
                                               "RNFL_OS_Superior","RNFL_OS_Temporal"
))
RNFLcorr <- rename(RNFLcorr, OD_I = RNFL_OD_Inferior, OD_N= RNFL_OD_Nasal,
                   OD_S= RNFL_OD_Superior, OD_T= RNFL_OD_Temporal,
                   OS_I= RNFL_OS_Inferior, OS_N= RNFL_OS_Nasal,
                   OS_S= RNFL_OS_Superior, OS_T= RNFL_OS_Temporal)
library(Hmisc)

RNFLcorrMatrix <- rcorr(as.matrix(RNFLcorr), type="pearson")
RNFLcorrMatrix

RNFLcorrMatrix$r #extract correlation coefficients
RNFLcorrMatrix$n 
RNFLcorrMatrix$P #extract p-values

library(corrplot)
corrplot(RNFLcorrMatrix$r, method="circle", type="full", order= "hclust",
         p.mat=RNFLcorrMatrix$P, sig.level = 0.05, insig="blank")

#other relationships
summary(final_RNFL_data)
ggplot(final_RNFL_data, aes(x=Age, y=Mean_RNFL_OS)) + 
  geom_point(aes(col=diagnosis)) +
  stat_cor(method="pearson")+
  geom_smooth(method="lm", alpha=0.3)+
  stat_regline_equation(label.y=125)

ggplot(nonMScohort, aes(x=Age, y=Mean_RNFL_OS)) + 
  geom_point(aes(col=diagnosis)) +
  stat_cor(method="pearson")+
  geom_smooth(method="lm", alpha=0.3)+
  stat_regline_equation(label.y=125)

ggplot(healthydonors, aes(x=Age, y=Mean_RNFL_OS)) + 
  geom_point(aes(col=diagnosis)) +
  stat_cor(method="pearson")+
  geom_smooth(method="lm", alpha=0.3)+
  stat_regline_equation(label.y=125)
#Mean RNFL shows significant decrease with age in full set, non-MS, 
#and healthy donors in OS (skips non-MS in OD)

ggplot(final_RNFL_data, aes(x=height, y=Mean_RNFL_OS)) + 
  geom_point(aes(col=diagnosis)) +
  stat_cor(method="pearson")+
  geom_smooth(method="lm", alpha=0.3)+
  stat_regline_equation(label.y=124)

ggplot(nonMScohort, aes(x=height, y=Mean_RNFL_OS)) + 
  geom_point(aes(col=diagnosis)) +
  stat_cor(method="pearson")+
  geom_smooth(method="lm", alpha=0.3)+
  stat_regline_equation(label.y=124)

ggplot(healthydonors, aes(x=height, y=Mean_RNFL_OS)) + 
  geom_point(aes(col=diagnosis)) +
  stat_cor(method="pearson")+
  geom_smooth(method="lm", alpha=0.3)+
  stat_regline_equation(label.y=127)
#Mean RNFL does not show significant correlation with height in any subset
#except for p=0.48 in full data for OS

#effect of RNFL machine?
boxplot(Mean_RNFL_OS ~ RNFL_Machine, data=final_RNFL_data)
ggplot(subset(final_RNFL_data, RNFL_Machine != "NA")) +
  geom_boxplot(aes(x=RNFL_Machine, y=Mean_RNFL_OS, fill=RNFL_Machine)) +
  stat_compare_means(aes(x=RNFL_Machine, y=Mean_RNFL_OS#, label= ..p.signif..),
                     ),method="t.test", label.x=1.25) #+
  #stat_compare_means(comparisons = c("4Q", "6Q"), label.y=300)

#I was trying to figure out how to add brackets showing significance to a boxplot with more than 2 boxes
my_comparisons <- c("4Q", "6Q")
ggplot(subset(final_RNFL_data, RNFL_Machine != "NA")) +
  geom_boxplot(aes(x=RNFL_Machine, y=Mean_RNFL_OS, fill=RNFL_Machine)) +
  stat_compare_means(aes(x=RNFL_Machine, y=Mean_RNFL_OS#, label= ..p.signif..),
                    ), method="t.test", label.x=1.25) #+
  #stat_compare_means(aes(x=RNFL_Machine, y=Mean_RNFL_OS), comparisons = my_comparisons, label.y=300)


ggplot(final_RNFL_data, aes(x=RNFL_Machine, y=Mean_RNFL_OS)) + 
  geom_point(aes(col=RNFL_Machine))

final_RNFL_data %>% 
  count(RNFL_Machine)
