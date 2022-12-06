#In this document, I found correlations between all OCT measurements and VA, 
#isolated "normal eyes" based on RNFL, and calculated the scale for the x-axis for 
#VA that I later used to calculate VA AUC
#APS

library(ggplot2)
library(ggpubr)
library(dplyr)
library(plotly)
library(corrplot)
install.packages("Hmisc")
library(Hmisc)
install.packages("reshape2")
library(reshape2)
install.packages("farver")
library(farver)

final_RNFL_data <- read.csv("final_vision_data.csv")
summary(final_RNFL_data)

#initial RNFL correlation plots with full data
ggplot(final_RNFL_data, aes(x=RNFL_OD_Inferior, y=RNFL_OD_Superior)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson") +
  #geom_abline(intercept=0, slope=model1$coefficients[1], color='blue', show.legend = T) +
  stat_regline_equation(label.y=185, formula = y~x-1) +
  geom_smooth(method="lm",se= F, formula=y~x-1) +
  geom_abline(slope=1, intercept = 0, alpha=0.5) +
  coord_cartesian(xlim=c(40,200), ylim=c(40,200))

ggplot(final_RNFL_data, aes(x=RNFL_OD_Temporal, y=RNFL_OD_Nasal)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson", label.x=60) +
  geom_smooth(method="lm", se=F,formula = y~x-1) +
  stat_regline_equation(label.x=60, label.y=160, formula = y~x-1) +
  geom_abline(slope=1, intercept=0, alpha=0.2)

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

#non-MS patients
nonMScohort <- subset(final_RNFL_data,diagnosis == "CIS" | 
                        diagnosis == "Healthy Donor"|diagnosis ==  "NIND"|
                        diagnosis ==  "OIND"|diagnosis ==  "RIS"|diagnosis=="pOIND")

ggplot(nonMScohort, aes(x=RNFL_OD_Inferior, y=RNFL_OD_Superior)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson") +
  geom_smooth(method="lm", se=F, formula= y~x-1) +
  stat_regline_equation(label.y=185, formula=y~x-1) +
  geom_abline(slope=1, intercept = 0, alpha=0.5)

ggplot(nonMScohort, aes(x=RNFL_OD_Inferior, y=RNFL_OD_Temporal)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson") +
  geom_smooth(method="lm", se=F, formula=y~x-1) +
  stat_regline_equation(label.y=100, formula = y~x-1) +
  geom_abline(slope=1, intercept = 0, alpha = 0.2)

ggplot(nonMScohort, aes(x=RNFL_OD_Temporal, y=RNFL_OD_Nasal)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson", label.x=50, label.y=30) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.x=50, label.y=25, ) +
  geom_abline(slope=0, intercept= mean(na.omit(nonMScohort$RNFL_OD_Nasal)), alpha=0.2)

ggplot(nonMScohort, aes(x=RNFL_OD_Inferior, y=RNFL_OS_Inferior)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson") +
  geom_smooth(method="lm", formula=y~x-1, se=F) +
  stat_regline_equation(label.y=160, formula=y~x-1) + 
  geom_abline(slope=1, intercept = 0, alpha=0.3) +
  coord_cartesian(xlim=c(40,180),ylim=c(40,180))

ggplot(nonMScohort, aes(x=RNFL_OD_Temporal, y=RNFL_OS_Temporal)) + 
  geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson") +
  geom_smooth(method="lm", se=F, formula=y~x-1) +
  stat_regline_equation(label.y=100, formula=y~x-1) + 
  geom_abline(slope=1, intercept = 0, alpha=0.3) +
  coord_cartesian(xlim=c(25,115), ylim=c(25,115))

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

#new correlation tables w/ ILM/RPE (macula)
maculaRNFLcorr <- subset(final_RNFL_data, select = c("RNFL_OD_Inferior", "RNFL_OD_Nasal",
                                               "RNFL_OD_Superior","RNFL_OD_Temporal",
                                               "RNFL_OS_Inferior","RNFL_OS_Nasal",
                                               "RNFL_OS_Superior","RNFL_OS_Temporal",
                                               "ILM_RPE_Thx_OD_Central", "ILM_RPE_Thx_OD_I1",
                                               "ILM_RPE_Thx_OD_I2","ILM_RPE_Thx_OD_N1",
                                               "ILM_RPE_Thx_OD_N2","ILM_RPE_Thx_OD_S1",
                                               "ILM_RPE_Thx_OD_S2","ILM_RPE_Thx_OD_T1",
                                               "ILM_RPE_Thx_OD_T2","ILM_RPE_Thx_OS_Central", 
                                               "ILM_RPE_Thx_OS_I1", "ILM_RPE_Thx_OS_I2","ILM_RPE_Thx_OS_N1",
                                               "ILM_RPE_Thx_OS_N2","ILM_RPE_Thx_OS_S1",
                                               "ILM_RPE_Thx_OS_S2","ILM_RPE_Thx_OS_T1",
                                               "ILM_RPE_Thx_OS_T2", "Age"))
maculaRNFLcorr <- rename(maculaRNFLcorr, RNFL_OD_I = RNFL_OD_Inferior, RNFL_OD_N= RNFL_OD_Nasal,
                   RNFL_OD_S= RNFL_OD_Superior, RNFL_OD_T= RNFL_OD_Temporal,
                   RNFL_OS_I= RNFL_OS_Inferior, RNFL_OS_N= RNFL_OS_Nasal,
                   RNFL_OS_S= RNFL_OS_Superior, RNFL_OS_T= RNFL_OS_Temporal,
                   Mac_OD_C = ILM_RPE_Thx_OD_Central, Mac_OD_I1= ILM_RPE_Thx_OD_I1,
                   Mac_OD_I2= ILM_RPE_Thx_OD_I2, Mac_OD_N1= ILM_RPE_Thx_OD_N1,
                   Mac_OD_N2= ILM_RPE_Thx_OD_N2, Mac_OD_S1= ILM_RPE_Thx_OD_S1,
                   Mac_OD_S2= ILM_RPE_Thx_OD_S2, Mac_OD_T1= ILM_RPE_Thx_OD_T1,
                   Mac_OD_T2= ILM_RPE_Thx_OD_T2, Mac_OS_C = ILM_RPE_Thx_OS_Central,
                   Mac_OS_I1 = ILM_RPE_Thx_OS_I1,Mac_OS_I2 = ILM_RPE_Thx_OS_I2,
                   Mac_OS_N1 = ILM_RPE_Thx_OS_N1,Mac_OS_N2 = ILM_RPE_Thx_OS_N2,
                   Mac_OS_S1 = ILM_RPE_Thx_OS_S1, Mac_OS_S2 = ILM_RPE_Thx_OS_S2,
                   Mac_OS_T1 = ILM_RPE_Thx_OS_T1,Mac_OS_T2 = ILM_RPE_Thx_OS_T2)
summary(maculaRNFLcorr)

library(Hmisc)
maculaRNFLcorrMatrix <- rcorr(as.matrix(maculaRNFLcorr), type="spearman")
maculaRNFLcorrMatrix

maculaRNFLcorrMatrix$r #extract correlation coefficients
maculaRNFLcorrMatrix$n 
maculaRNFLcorrMatrix$P #extract p-values

library(corrplot)
corrplot(maculaRNFLcorrMatrix$r, method="circle", type="full", order = "hclust",
         p.mat=maculaRNFLcorrMatrix$P, sig.level = 0.05, insig="blank", 
         tl.cex=0.5,tl.srt=70)

#non-MS cohort
maculaRNFLcorr <- subset(nonMScohort, select = c("RNFL_OD_Inferior", "RNFL_OD_Nasal",
                                                     "RNFL_OD_Superior","RNFL_OD_Temporal",
                                                     "RNFL_OS_Inferior","RNFL_OS_Nasal",
                                                     "RNFL_OS_Superior","RNFL_OS_Temporal",
                                                     "ILM_RPE_Thx_OD_Central", "ILM_RPE_Thx_OD_I1",
                                                     "ILM_RPE_Thx_OD_I2","ILM_RPE_Thx_OD_N1",
                                                     "ILM_RPE_Thx_OD_N2","ILM_RPE_Thx_OD_S1",
                                                     "ILM_RPE_Thx_OD_S2","ILM_RPE_Thx_OD_T1",
                                                     "ILM_RPE_Thx_OD_T2","ILM_RPE_Thx_OS_Central", 
                                                     "ILM_RPE_Thx_OS_I1", "ILM_RPE_Thx_OS_I2","ILM_RPE_Thx_OS_N1",
                                                     "ILM_RPE_Thx_OS_N2","ILM_RPE_Thx_OS_S1",
                                                     "ILM_RPE_Thx_OS_S2","ILM_RPE_Thx_OS_T1",
                                                     "ILM_RPE_Thx_OS_T2", "Age"))
maculaRNFLcorr <- rename(maculaRNFLcorr, RNFL_OD_I = RNFL_OD_Inferior, RNFL_OD_N= RNFL_OD_Nasal,
                         RNFL_OD_S= RNFL_OD_Superior, RNFL_OD_T= RNFL_OD_Temporal,
                         RNFL_OS_I= RNFL_OS_Inferior, RNFL_OS_N= RNFL_OS_Nasal,
                         RNFL_OS_S= RNFL_OS_Superior, RNFL_OS_T= RNFL_OS_Temporal,
                         Mac_OD_C = ILM_RPE_Thx_OD_Central, Mac_OD_I1= ILM_RPE_Thx_OD_I1,
                         Mac_OD_I2= ILM_RPE_Thx_OD_I2, Mac_OD_N1= ILM_RPE_Thx_OD_N1,
                         Mac_OD_N2= ILM_RPE_Thx_OD_N2, Mac_OD_S1= ILM_RPE_Thx_OD_S1,
                         Mac_OD_S2= ILM_RPE_Thx_OD_S2, Mac_OD_T1= ILM_RPE_Thx_OD_T1,
                         Mac_OD_T2= ILM_RPE_Thx_OD_T2, Mac_OS_C = ILM_RPE_Thx_OS_Central,
                         Mac_OS_I1 = ILM_RPE_Thx_OS_I1,Mac_OS_I2 = ILM_RPE_Thx_OS_I2,
                         Mac_OS_N1 = ILM_RPE_Thx_OS_N1,Mac_OS_N2 = ILM_RPE_Thx_OS_N2,
                         Mac_OS_S1 = ILM_RPE_Thx_OS_S1, Mac_OS_S2 = ILM_RPE_Thx_OS_S2,
                         Mac_OS_T1 = ILM_RPE_Thx_OS_T1,Mac_OS_T2 = ILM_RPE_Thx_OS_T2)
summary(maculaRNFLcorr)

library(Hmisc)
maculaRNFLcorrMatrix <- rcorr(as.matrix(maculaRNFLcorr), type="pearson")
maculaRNFLcorrMatrix

maculaRNFLcorrMatrix$r #extract correlation coefficients
maculaRNFLcorrMatrix$n 
maculaRNFLcorrMatrix$P #extract p-values

library(corrplot)
corrplot(maculaRNFLcorrMatrix$r, method="circle", type="full", order = "hclust",
         p.mat=maculaRNFLcorrMatrix$P, sig.level = 0.05, insig="blank", 
         tl.cex=0.5,tl.srt=70)

#healthy donors
healthydonors <- subset(final_RNFL_data, diagnosis == "Healthy Donor")
maculaRNFLcorr <- subset(healthydonors, select = c("RNFL_OD_Inferior", "RNFL_OD_Nasal",
                                                     "RNFL_OD_Superior","RNFL_OD_Temporal",
                                                     "RNFL_OS_Inferior","RNFL_OS_Nasal",
                                                     "RNFL_OS_Superior","RNFL_OS_Temporal",
                                                     "ILM_RPE_Thx_OD_Central", "ILM_RPE_Thx_OD_I1",
                                                     "ILM_RPE_Thx_OD_I2","ILM_RPE_Thx_OD_N1",
                                                     "ILM_RPE_Thx_OD_N2","ILM_RPE_Thx_OD_S1",
                                                     "ILM_RPE_Thx_OD_S2","ILM_RPE_Thx_OD_T1",
                                                     "ILM_RPE_Thx_OD_T2","ILM_RPE_Thx_OS_Central", 
                                                     "ILM_RPE_Thx_OS_I1", "ILM_RPE_Thx_OS_I2","ILM_RPE_Thx_OS_N1",
                                                     "ILM_RPE_Thx_OS_N2","ILM_RPE_Thx_OS_S1",
                                                     "ILM_RPE_Thx_OS_S2","ILM_RPE_Thx_OS_T1",
                                                     "ILM_RPE_Thx_OS_T2", "Age"))
maculaRNFLcorr <- rename(maculaRNFLcorr, RNFL_OD_I = RNFL_OD_Inferior, RNFL_OD_N= RNFL_OD_Nasal,
                         RNFL_OD_S= RNFL_OD_Superior, RNFL_OD_T= RNFL_OD_Temporal,
                         RNFL_OS_I= RNFL_OS_Inferior, RNFL_OS_N= RNFL_OS_Nasal,
                         RNFL_OS_S= RNFL_OS_Superior, RNFL_OS_T= RNFL_OS_Temporal,
                         Mac_OD_C = ILM_RPE_Thx_OD_Central, Mac_OD_I1= ILM_RPE_Thx_OD_I1,
                         Mac_OD_I2= ILM_RPE_Thx_OD_I2, Mac_OD_N1= ILM_RPE_Thx_OD_N1,
                         Mac_OD_N2= ILM_RPE_Thx_OD_N2, Mac_OD_S1= ILM_RPE_Thx_OD_S1,
                         Mac_OD_S2= ILM_RPE_Thx_OD_S2, Mac_OD_T1= ILM_RPE_Thx_OD_T1,
                         Mac_OD_T2= ILM_RPE_Thx_OD_T2, Mac_OS_C = ILM_RPE_Thx_OS_Central,
                         Mac_OS_I1 = ILM_RPE_Thx_OS_I1,Mac_OS_I2 = ILM_RPE_Thx_OS_I2,
                         Mac_OS_N1 = ILM_RPE_Thx_OS_N1,Mac_OS_N2 = ILM_RPE_Thx_OS_N2,
                         Mac_OS_S1 = ILM_RPE_Thx_OS_S1, Mac_OS_S2 = ILM_RPE_Thx_OS_S2,
                         Mac_OS_T1 = ILM_RPE_Thx_OS_T1,Mac_OS_T2 = ILM_RPE_Thx_OS_T2)
summary(maculaRNFLcorr)

library(Hmisc)
maculaRNFLcorrMatrix <- rcorr(as.matrix(maculaRNFLcorr), type="spearman")
maculaRNFLcorrMatrix

maculaRNFLcorrMatrix$r #extract correlation coefficients
maculaRNFLcorrMatrix$n 
maculaRNFLcorrMatrix$P #extract p-values
?corrplot

library(corrplot)
corrplot(maculaRNFLcorrMatrix$r, method="circle", type="full", order = "hclust",
         p.mat=maculaRNFLcorrMatrix$P, sig.level = 0.05, insig="blank", 
         tl.cex=0.4, tl.srt=70, )
summary(maculaRNFLcorr)

#find "normal" difference between eyes for RNFL
final_RNFL_data$RNFL_Temporal_diff <- abs(final_RNFL_data$RNFL_OD_Temporal-final_RNFL_data$RNFL_OS_Temporal)
final_RNFL_data$RNFL_Inferior_diff <- abs(final_RNFL_data$RNFL_OD_Inferior-final_RNFL_data$RNFL_OS_Inferior)
final_RNFL_data$RNFL_Superior_diff <- abs(final_RNFL_data$RNFL_OD_Superior-final_RNFL_data$RNFL_OS_Superior)
final_RNFL_data$RNFL_Nasal_diff <- abs(final_RNFL_data$RNFL_OD_Nasal-final_RNFL_data$RNFL_OS_Nasal)

my_comparisons <- list( c("healthy", "MS"), c("healthy", "OND"), c("MS", "OND") )

ggplot(final_RNFL_data) +
  geom_boxplot(aes(x= condition, y=RNFL_Temporal_diff, fill=condition)) +
  stat_compare_means(aes(x=condition, y=RNFL_Temporal_diff),method="t.test")
ggplot(final_RNFL_data) +
  geom_boxplot(aes(y=RNFL_Inferior_diff, fill=condition)) 
ggplot(final_RNFL_data) +
  geom_boxplot(aes(y=RNFL_Superior_diff, fill=condition))
ggplot(final_RNFL_data) +
  geom_boxplot(aes(y=RNFL_Nasal_diff, fill=condition))

nonMScohort <- subset(final_RNFL_data,diagnosis == "CIS" | 
                        diagnosis == "Healthy Donor"|diagnosis ==  "NIND"|
                        diagnosis ==  "OIND"|diagnosis ==  "RIS"|diagnosis=="pOIND")
summary(nonMScohort$RNFL_Inferior_diff)
summary(nonMScohort$RNFL_Nasal_diff)
summary(nonMScohort$RNFL_Superior_diff)
summary(nonMScohort$RNFL_Temporal_diff)

healthydonors <- subset(final_RNFL_data, diagnosis == "Healthy Donor")
summary(healthydonors$RNFL_Inferior_diff)
summary(healthydonors$RNFL_Nasal_diff)
summary(healthydonors$RNFL_Superior_diff)
summary(healthydonors$RNFL_Temporal_diff)
#Used these summaries to find IQR of healthy donors for RNFL difference in each region

#create dataset with only individuals with normal difference between eyes in RNFL
normaleyesRNFL <- subset(final_RNFL_data, subset = final_RNFL_data$RNFL_Inferior_diff <= 11 & 
                         final_RNFL_data$RNFL_Nasal_diff <= 13 &
                         final_RNFL_data$RNFL_Superior_diff <= 14 &
                         final_RNFL_data$RNFL_Temporal_diff <= 13)
summary(normaleyesRNFL)

#Add average of OD and OS to data set for normal eye difference
normaleyesRNFL$RNFL_OU_Inferior <- (normaleyesRNFL$RNFL_OS_Inferior + normaleyesRNFL$RNFL_OD_Inferior)/2
summary(normaleyesRNFL$RNFL_OU_Inferior)
summary(normaleyesRNFL$RNFL_OD_Inferior)
summary(normaleyesRNFL$RNFL_OS_Inferior)

normaleyesRNFL$RNFL_OU_Superior <- (normaleyesRNFL$RNFL_OS_Superior + normaleyesRNFL$RNFL_OD_Superior)/2
summary(normaleyesRNFL$RNFL_OU_Superior)
summary(normaleyesRNFL$RNFL_OD_Superior)
summary(normaleyesRNFL$RNFL_OS_Superior)

normaleyesRNFL$RNFL_OU_Nasal <- (normaleyesRNFL$RNFL_OS_Nasal + normaleyesRNFL$RNFL_OD_Nasal)/2
summary(normaleyesRNFL$RNFL_OU_Nasal)
summary(normaleyesRNFL$RNFL_OD_Nasal)
summary(normaleyesRNFL$RNFL_OS_Nasal)

normaleyesRNFL$RNFL_OU_Temporal <- (normaleyesRNFL$RNFL_OS_Temporal + normaleyesRNFL$RNFL_OD_Temporal)/2
summary(normaleyesRNFL$RNFL_OU_Temporal)
summary(normaleyesRNFL$RNFL_OD_Temporal)
summary(normaleyesRNFL$RNFL_OS_Temporal)

#Find correlations between RNFL for both eyes and VA data
#Temporal
ggplotly(ggplot(normaleyesRNFL, aes(x=RNFL_OU_Temporal, y=VA_100_contrast_OU)) +
  geom_point(aes(col=diagnosis)))
ggplotly(ggplot(normaleyesRNFL, aes(x=RNFL_OU_Temporal, y=VA_2.5_contrast_OU)) +
  geom_point(aes(col=diagnosis)))
ggplotly(ggplot(normaleyesRNFL, aes(x=RNFL_OU_Temporal, y=VA_1.25_contrast_OU))+
  geom_point(aes(col=diagnosis)))

ggplot(normaleyesRNFL, aes(x=RNFL_OU_Temporal, y=VA_100_contrast_OU)) +
  geom_point(aes(col=diagnosis))+
  stat_cor(method="spearman", label.y=76) +
  geom_smooth(method="lm",se=F) +
  stat_regline_equation(label.y=72.5, col="blue") +
  geom_smooth(method="lm", formula=y~x-1, se=F, col="orange")+
  stat_regline_equation(label.y=69, formula= y~x-1,col="orange") +
  coord_cartesian(ylim=c(0,75))

ggplot(normaleyesRNFL, aes(x=RNFL_OU_Temporal, y=VA_2.5_contrast_OU)) +
  geom_point(aes(col=diagnosis)) +
  stat_cor(method="spearman", label.y=76) +
  geom_smooth(method="lm",se=F) +
  stat_regline_equation(label.y=72.5, col="blue") +
  geom_smooth(method="lm", formula=y~x-1, se=F, col="orange")+
  stat_regline_equation(label.y=69, formula= y~x-1,col="orange") +
  coord_cartesian(ylim=c(0,75))

ggplot(normaleyesRNFL, aes(x=RNFL_OU_Temporal, y=VA_1.25_contrast_OU)) +
  geom_point(aes(col=diagnosis)) +
  stat_cor(method="spearman", label.y=76) +
  geom_smooth(method="lm",se=F) +
  stat_regline_equation(label.y=72.5, col="blue") +
  geom_smooth(method="lm", formula=y~x-1, se=F, col="orange")+
  stat_regline_equation(label.y=69, formula= y~x-1,col="orange") +
  coord_cartesian(ylim=c(0,75))

#Inferior
ggplotly(ggplot(normaleyesRNFL, aes(x=RNFL_OU_Inferior, y=VA_100_contrast_OU)) +
           geom_point(aes(col=diagnosis)))
ggplotly(ggplot(normaleyesRNFL, aes(x=RNFL_OU_Inferior, y=VA_2.5_contrast_OU)) +
           geom_point(aes(col=diagnosis)))
ggplotly(ggplot(normaleyesRNFL, aes(x=RNFL_OU_Inferior, y=VA_1.25_contrast_OU)) +
           geom_point(aes(col=diagnosis)))

ggplot(normaleyesRNFL, aes(x=RNFL_OU_Inferior, y=VA_100_contrast_OU)) +
  geom_point(aes(col=diagnosis))+
  stat_cor(method="spearman", label.y=76) +
  geom_smooth(method="lm",se=F) +
  stat_regline_equation(label.y=72.5, col="blue") +
  geom_smooth(method="lm", formula=y~x-1, se=F, col="orange")+
  stat_regline_equation(label.y=69, formula= y~x-1,col="orange") +
  coord_cartesian(ylim=c(0,75))

ggplot(normaleyesRNFL, aes(x=RNFL_OU_Inferior, y=VA_2.5_contrast_OU)) +
  geom_point(aes(col=diagnosis)) +
  stat_cor(method="spearman", label.y=76) +
  geom_smooth(method="lm",se=F) +
  stat_regline_equation(label.y=72.5, col="blue") +
  geom_smooth(method="lm", formula=y~x-1, se=F, col="orange")+
  stat_regline_equation(label.y=69, formula= y~x-1,col="orange") +
  coord_cartesian(ylim=c(0,75))

ggplot(normaleyesRNFL, aes(x=RNFL_OU_Inferior, y=VA_1.25_contrast_OU)) +
  geom_point(aes(col=diagnosis)) +
  stat_cor(method="spearman", label.y=76) +
  geom_smooth(method="lm",se=F) +
  stat_regline_equation(label.y=72.5, col="blue") +
  geom_smooth(method="lm", formula=y~x-1, se=F, col="orange")+
  stat_regline_equation(label.y=69, formula= y~x-1,col="orange") +
  coord_cartesian(ylim=c(0,75))

#Superior
ggplot(normaleyesRNFL, aes(x=RNFL_OU_Superior, y=VA_100_contrast_OU)) +
  geom_point(aes(col=diagnosis))+
  stat_cor(method="spearman", label.y=76) +
  geom_smooth(method="lm",se=F) +
  stat_regline_equation(label.y=72.5, col="blue") +
  geom_smooth(method="lm", formula=y~x-1, se=F, col="orange")+
  stat_regline_equation(label.y=69, formula= y~x-1,col="orange") +
  coord_cartesian(ylim=c(0,75))

ggplot(normaleyesRNFL, aes(x=RNFL_OU_Superior, y=VA_2.5_contrast_OU)) +
  geom_point(aes(col=diagnosis)) +
  stat_cor(method="spearman", label.y=76) +
  geom_smooth(method="lm",se=F) +
  stat_regline_equation(label.y=72.5, col="blue") +
  geom_smooth(method="lm", formula=y~x-1, se=F, col="orange")+
  stat_regline_equation(label.y=69, formula= y~x-1,col="orange") +
  coord_cartesian(ylim=c(0,75))

ggplot(normaleyesRNFL, aes(x=RNFL_OU_Superior, y=VA_1.25_contrast_OU)) +
  geom_point(aes(col=diagnosis)) +
  stat_cor(method="spearman", label.y=76) +
  geom_smooth(method="lm",se=F) +
  stat_regline_equation(label.y=72.5, col="blue") +
  geom_smooth(method="lm", formula=y~x-1, se=F, col="orange")+
  stat_regline_equation(label.y=69, formula= y~x-1,col="orange") +
  coord_cartesian(ylim=c(0,75))

#Nasal
ggplot(normaleyesRNFL, aes(x=RNFL_OU_Nasal, y=VA_100_contrast_OU)) +
  geom_point(aes(col=diagnosis))+
  stat_cor(method="spearman", label.y=76) +
  geom_smooth(method="lm",se=F) +
  stat_regline_equation(label.y=72.5, col="blue") +
  geom_smooth(method="lm", formula=y~x-1, se=F, col="orange")+
  stat_regline_equation(label.y=69, formula= y~x-1,col="orange") +
  coord_cartesian(ylim=c(0,75))

ggplot(normaleyesRNFL, aes(x=RNFL_OU_Nasal, y=VA_2.5_contrast_OU)) +
  geom_point(aes(col=diagnosis)) +
  stat_cor(method="spearman", label.y=76) +
  geom_smooth(method="lm",se=F) +
  stat_regline_equation(label.y=72.5, col="blue") +
  geom_smooth(method="lm", formula=y~x-1, se=F, col="orange")+
  stat_regline_equation(label.y=69, formula= y~x-1,col="orange") +
  coord_cartesian(ylim=c(0,75))

ggplot(normaleyesRNFL, aes(x=RNFL_OU_Nasal, y=VA_1.25_contrast_OU)) +
  geom_point(aes(col=diagnosis)) +
  stat_cor(method="spearman", label.y=76) +
  geom_smooth(method="lm",se=F) +
  stat_regline_equation(label.y=72.5, col="blue") +
  geom_smooth(method="lm", formula=y~x-1, se=F, col="orange")+
  stat_regline_equation(label.y=69, formula= y~x-1,col="orange") +
  coord_cartesian(ylim=c(0,75))

#Correlation tables between RNFL and VA for OU
VA_RNFL_corr <- subset(normaleyesRNFL, select = c("RNFL_OU_Inferior", "RNFL_OU_Nasal",
                                               "RNFL_OU_Superior","RNFL_OU_Temporal",
                                               "VA_1.25_contrast_OU","VA_2.5_contrast_OU",
                                               "VA_100_contrast_OU","Age", "height"))
VA_RNFL_corr <- rename(VA_RNFL_corr, RNFL_OU_I = RNFL_OU_Inferior, RNFL_OU_N= RNFL_OU_Nasal,
                   RNFL_OU_S= RNFL_OU_Superior, RNFL_OU_T= RNFL_OU_Temporal,
                   VA_OU_1.25= VA_1.25_contrast_OU, VA_OU_2.5= VA_2.5_contrast_OU,
                   VA_OU_100= VA_100_contrast_OU)

library(Hmisc)

VA_RNFL_corrMatrix <- rcorr(as.matrix(VA_RNFL_corr), type="spearman")
VA_RNFL_corrMatrix

VA_RNFL_corrMatrix$r #extract correlation coefficients
VA_RNFL_corrMatrix$n 
VA_RNFL_corrMatrix$P #extract p-values

library(corrplot)
corrplot(VA_RNFL_corrMatrix$r, method="circle", type="full", order= "hclust",
         p.mat=VA_RNFL_corrMatrix$P, sig.level = 0.05, insig="blank")

#boxplot of VA contrasts?
ggplot(normaleyesRNFL) +
  geom_boxplot(aes(y=VA_1.25_contrast_OU, fill=condition)) 
ggplot(normaleyesRNFL) +
  geom_boxplot(aes(y=VA_2.5_contrast_OU, fill=condition)) 
ggplot(normaleyesRNFL) +
  geom_boxplot(aes(y=VA_100_contrast_OU, fill=condition)) 

#Creating scale for VA plots from which AUC is calculated
#Try to reshape to put contrast on x-axis
VA_OUsubset <- subset(normaleyesRNFL, select = c("VA_1.25_contrast_OU", 
                                                 "VA_2.5_contrast_OU", 
                                                 "VA_100_contrast_OU",
                                                 "condition"))
VA_OUsubset <- rename(VA_OUsubset, "1.25" = VA_1.25_contrast_OU, 
                      "2.5" = VA_2.5_contrast_OU, 
                      "100"=VA_100_contrast_OU)
library(reshape2)
VA_OUsubsetlong <- melt(VA_OUsubset, id="condition")
VA_OUsubsetlong <- rename(VA_OUsubsetlong, contrast = variable, score=value)
head(VA_OUsubsetlong)
summary(VA_OUsubsetlong$contrast)
sapply(VA_OUsubsetlong, class)

VA_OUsubsetlong$scaled_contrast <- as.numeric(as.character(VA_OUsubsetlong$contrast))


ggplot(VA_OUsubsetlong, aes(x = contrast, y = score, fill = condition)) +
  geom_boxplot() 

ggplot(VA_OUsubsetlong, aes(x = as.numeric(as.character(contrast)), y = score)) +
  geom_point(aes(col=condition)) + xlab("percent contrast") +
  scale_x_log10() + 
  #scale_x_continuous(trans = "log2") +
  stat_cor(method="spearman") +
  #geom_smooth(method="loess", formula=y~x, col="red", se=F) +
  geom_smooth(method="lm", formula=y~x-1, col="orange", se=F) +
  geom_smooth(method="lm", se=F, col="blue") +
  stat_regline_equation(label.y=60, formula=y~x-1, col="orange") +
  stat_regline_equation(label.y=63, col="blue") +
  coord_cartesian(xlim=c(1,100))

ggplot(VA_OUsubsetlong, aes(x = as.numeric(as.character(contrast)), y = score)) +
  geom_point(aes(col=condition)) + xlab("percent contrast") +
  #scale_x_log10() + 
  #scale_x_continuous(trans = "log2") +
  #stat_cor(method="spearman") +
  #geom_smooth(method="loess", formula=y~x, col="red", se=F) +
  geom_smooth(method="lm", formula=y~x-1, col="orange", se=F) +
  #geom_smooth(method="lm", se=F, col="blue") +
  stat_regline_equation(label.y=60, formula=y~x-1, col="orange") +
  #stat_regline_equation(label.y=63, col="blue") +
  #coord_cartesian(xlim=c(1,100))
  geom_abline(slope=(median(VA_OUsubset$`100`)/100), intercept=0)

median(VA_OUsubset$`100`)
median(VA_OUsubset$`2.5`)/(median(VA_OUsubset$`100`)/100)
median(VA_OUsubset$`1.25`)/(median(VA_OUsubset$`100`)/100)




#try to scale contrasts so medians make a straight line from the origin
VA_OUsubset <- subset(normeyes_new, select = c("VA_1.25_contrast_OU", 
                                                 "VA_2.5_contrast_OU", 
                                                 "VA_100_contrast_OU",
                                                 "condition"))
VA_OUsubset2 <- rename(VA_OUsubset, "33.33333" = VA_1.25_contrast_OU, 
                      "52.63158" = VA_2.5_contrast_OU, 
                      "100"=VA_100_contrast_OU)
library(reshape2)
VA_OUsubsetlong2 <- melt(VA_OUsubset2, id="condition")
VA_OUsubsetlong2 <- rename(VA_OUsubsetlong2, scaled_contrast = variable, score=value)
head(VA_OUsubsetlong2)
#VA_OUsubsetlong2$contrast <- VA_OUsubsetlong2$scaled_contrast
#VA_OUsubsetlong2$contrast[VA_OUsubsetlong2$scaled_contrast == "33.33333"] <- "1.25"
#VA_OUsubsetlong2$contrast[VA_OUsubsetlong2$scaled_contrast == "NA"] <- "1.25"
summary(VA_OUsubsetlong2$contrast)

#library(tibble)
#VA_OUsubsetlong2 <- VA_OUsubsetlong2 %>%
#  add_column(contrast = if_else(.$scaled_contrast == "33.33333", 1.25, 2.5))
#VA_OUsubsetlong2 <- VA_OUsubsetlong2 %>%
#  add_column(contrast = if_else(.$scaled_contrast == "100" & .$contrast== "2.5", 100, 2.5))
#VA_OUsubsetlong$contrast <- rep(NA, )

VA_OUsubsetlong2 <- VA_OUsubsetlong2 %>% mutate(contrast =
                     case_when(scaled_contrast == 33.33333 ~ "1.25", 
                               scaled_contrast == 52.63158 ~ "2.5",
                               scaled_contrast == 100 ~ "100"))
  
summary(VA_OUsubsetlong2)
sapply(VA_OUsubsetlong2, class)
ggplot(VA_OUsubsetlong2, aes(x=as.numeric(as.character(scaled_contrast)),y=score)) +
  geom_boxplot(aes(fill=contrast)) +xlab("scaled contrast") +
  coord_cartesian(xlim=c(0,100))+
  geom_abline(slope=0.57, intercept=0)

#Created plot with straight line through median values for each contrast in "normal eyes"-
#used this scale to calculate AUC

#To create the scale values for the x-axis, I just did the algebra for a line
#through the origin to the median value for 100% contrast and moved the other contrasts
#so that the medians fit on that line