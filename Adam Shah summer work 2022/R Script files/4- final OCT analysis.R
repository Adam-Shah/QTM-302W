#This is just ILM-RPE OCT (not RNFL)
#In this document, I found correlations in OCT measurements, created correlation
#tables for all data, non-MS, and healthy patients, and looked into other 
#relationships with Age and height
#APS

library(dplyr)
library(ggplot2)
library(plotly)

final_OCT_data <- read.csv("final_vision_data.csv")
summary(final_OCT_data)

#effect of age on retina thickness
q <- ggplot(final_OCT_data, aes(x= Age, y= Macular_Volume_OS)) + geom_point(aes(col=diagnosis))
ggplotly(q)

q <- ggplot(subset(final_OCT_data,diagnosis != "" ), aes(x=Age, y= ILM_RPE_Thx_OS_Central)) + geom_point(aes(col=diagnosis))
ggplotly(q)

ggplot(subset(final_OCT_data,diagnosis != "" ), aes(x=Age, y= ILM_RPE_Thx_OS_Central)) + 
  geom_point(aes(col=diagnosis)) + stat_cor(method="pearson") + 
  geom_smooth(method="lm") + stat_regline_equation(label.y=360)

ggplot(subset(final_OCT_data,diagnosis != "" ), aes(x=height, y= ILM_RPE_Thx_OD_Central)) + 
  geom_point(aes(col=diagnosis)) +stat_cor(method="pearson") + 
  geom_smooth(method="lm") + stat_regline_equation(label.y=340)

ggplotly(ggplot(subset(final_OCT_data,diagnosis != "" ), aes(x=height, y= ILM_RPE_Thx_OD_Central)) + 
           geom_point(aes(col=diagnosis)) +stat_cor(method="pearson") + 
           geom_smooth(method="lm") + stat_regline_equation(label.y=340))


#OCT and RNFL measurements?
ggplotly(ggplot(subset(final_OCT_data,diagnosis != "" ), aes(x=ILM_RPE_Thx_OD_S1, y=RNFL_OD_TS)) +geom_point(aes(col=diagnosis)))

#correlation for all OCT outcomes in same eye
ggplot(subset(final_OCT_data,diagnosis != "" ), aes(x=ILM_RPE_Thx_OS_N1, y=ILM_RPE_Thx_OS_I1)) + geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson")

ggplot(subset(final_OCT_data,diagnosis != "" ), aes(x=ILM_RPE_Thx_OD_T2, y=ILM_RPE_Thx_OD_S1)) + geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson")

ggplot(subset(final_OCT_data,diagnosis != "" ), aes(x=ILM_RPE_Thx_OS_Central, y=ILM_RPE_Thx_OS_S1)) + geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson")

ggplotly(ggplot(final_OCT_data, aes(x=ILM_RPE_Thx_OS_S1, y=ILM_RPE_Thx_OS_S2)) + geom_point(aes(col=diagnosis)))

ggplot(subset(final_OCT_data,diagnosis != ""), 
       aes(x=ILM_RPE_Thx_OS_N1, y=ILM_RPE_Thx_OS_I2)) +
  geom_point(aes(col=diagnosis))+stat_cor(method="pearson")

#How to make plot with only certain diagnoses (excluding MS cohort)??
ggplot(subset(final_OCT_data,diagnosis == "CIS" | diagnosis == "Healthy Donor"|diagnosis ==  "NIND"|diagnosis ==  "OIND"|diagnosis ==  "RIS"), 
       aes(x=ILM_RPE_Thx_OS_S1, y=ILM_RPE_Thx_OS_S2)) +
  geom_point(aes(col=diagnosis))
#ggplot(full_data[full_data$diagnosis == c("CIS", "Healthy Donor", "NIND", "OIND", "RIS"),], aes(x=ILM_RPE_Thx_OS_S1, y=ILM_RPE_Thx_OS_S2)) +
#  geom_point(aes(col=diagnosis))
?subset

#graphs with non-MS patients
ggplot(subset(final_OCT_data,diagnosis == "CIS" | diagnosis == "Healthy Donor"|diagnosis ==  "NIND"|diagnosis ==  "OIND"|diagnosis ==  "RIS"|diagnosis == "pOIND"), 
       aes(x=ILM_RPE_Thx_OS_Central, y=ILM_RPE_Thx_OD_Central)) +
  geom_point(aes(col=diagnosis)) +stat_cor(method="pearson") +
  geom_smooth(method="lm") + stat_regline_equation(label.x=220, label.y=320)

ggplot(subset(final_OCT_data,diagnosis == "CIS" | diagnosis == "Healthy Donor"|diagnosis ==  "NIND"|diagnosis ==  "OIND"|diagnosis ==  "RIS"|diagnosis == "pOIND"), 
       aes(x=ILM_RPE_Thx_OS_N1, y=ILM_RPE_Thx_OS_I1)) +
  geom_point(aes(col=diagnosis)) + stat_cor(method="pearson")

ggplot(subset(final_OCT_data,diagnosis == "CIS" | diagnosis == "Healthy Donor"|diagnosis ==  "NIND"|diagnosis ==  "OIND"|diagnosis ==  "RIS"|diagnosis=="pOIND"), 
       aes(x=ILM_RPE_Thx_OD_S1, y=ILM_RPE_Thx_OS_S1)) +
  geom_point(aes(col=diagnosis)) + stat_cor(method="pearson")

ggplot(subset(final_OCT_data,diagnosis == "CIS" | diagnosis == "Healthy Donor"|diagnosis ==  "NIND"|diagnosis ==  "OIND"|diagnosis ==  "RIS"|diagnosis=="pOIND"), 
       aes(x=ILM_RPE_Thx_OD_N2, y=ILM_RPE_Thx_OS_S2)) +
  geom_point(aes(col=diagnosis)) + stat_cor(method="pearson")

model <- lm(ILM_RPE_Thx_OS_S2 ~ ILM_RPE_Thx_OD_N2, data=final_OCT_data)
summary(model)

#correlation in OCT outcomes across eyes
ggplot(subset(final_OCT_data,diagnosis != "" ), aes(x=ILM_RPE_Thx_OD_Central, y=ILM_RPE_Thx_OS_Central)) + geom_point(aes(col=diagnosis)) +
  stat_cor(method="pearson") + geom_smooth(method="lm") + stat_regline_equation(label.x=200, label.y= 360)
model <- lm(ILM_RPE_Thx_OS_Central~ILM_RPE_Thx_OD_Central, data=final_OCT_data)
summary(model)

ggplot(subset(final_OCT_data,diagnosis != "" ), aes(x=ILM_RPE_Thx_OD_N1, y=ILM_RPE_Thx_OS_N1)) + geom_point(aes(col=diagnosis)) +
  stat_cor(method="pearson")
ggplot(subset(final_OCT_data,diagnosis != "" ), aes(x=ILM_RPE_Thx_OD_S2, y=ILM_RPE_Thx_OS_T1)) + geom_point(aes(col=diagnosis)) +
  stat_cor(method="pearson")
ggplot(subset(final_OCT_data,diagnosis != "" ), aes(x=ILM_RPE_Thx_OD_Central, y=ILM_RPE_Thx_OS_I2)) + geom_point(aes(col=diagnosis)) +
  stat_cor(method="pearson")

#correlation matrix and p-values
OCTcorr <- subset(final_OCT_data, select = c("ILM_RPE_Thx_OD_Central", "ILM_RPE_Thx_OD_I1",
                                        "ILM_RPE_Thx_OD_I2","ILM_RPE_Thx_OD_N1",
                                        "ILM_RPE_Thx_OD_N2","ILM_RPE_Thx_OD_S1",
                                        "ILM_RPE_Thx_OD_S2","ILM_RPE_Thx_OD_T1",
                                        "ILM_RPE_Thx_OD_T2","ILM_RPE_Thx_OS_Central", 
                                        "ILM_RPE_Thx_OS_I1", "ILM_RPE_Thx_OS_I2","ILM_RPE_Thx_OS_N1",
                                        "ILM_RPE_Thx_OS_N2","ILM_RPE_Thx_OS_S1",
                                        "ILM_RPE_Thx_OS_S2","ILM_RPE_Thx_OS_T1",
                                        "ILM_RPE_Thx_OS_T2"))
OCTcorr <- rename(OCTcorr, OD_C = ILM_RPE_Thx_OD_Central, OD_I1= ILM_RPE_Thx_OD_I1,
                  OD_I2= ILM_RPE_Thx_OD_I2, OD_N1= ILM_RPE_Thx_OD_N1,
                  OD_N2= ILM_RPE_Thx_OD_N2, OD_S1= ILM_RPE_Thx_OD_S1,
                  OD_S2= ILM_RPE_Thx_OD_S2, OD_T1= ILM_RPE_Thx_OD_T1,
                  OD_T2= ILM_RPE_Thx_OD_T2, OS_C = ILM_RPE_Thx_OS_Central,
                  OS_I1 = ILM_RPE_Thx_OS_I1,OS_I2 = ILM_RPE_Thx_OS_I2,
                  OS_N1 = ILM_RPE_Thx_OS_N1,OS_N2 = ILM_RPE_Thx_OS_N2,
                  OS_S1 = ILM_RPE_Thx_OS_S1, OS_S2 = ILM_RPE_Thx_OS_S2,
                  OS_T1 = ILM_RPE_Thx_OS_T1,OS_T2 = ILM_RPE_Thx_OS_T2)

install.packages("Hmisc")
library(Hmisc)

OCTcorrMatrix <- rcorr(as.matrix(OCTcorr), type="pearson")
OCTcorrMatrix

OCTcorrMatrix$r #extract correlation coefficients
OCTcorrMatrix$n 
OCTcorrMatrix$P #extract p-values

install.packages("corrplot")
library(corrplot)
corrplot(OCTcorrMatrix$r, method="circle", type="full", order= "hclust",
         p.mat=OCTcorrMatrix$P, sig.level = 0.01, insig="blank")
#this is a correlation matrix plot with color to indicate correlation values 
#and using p-value matrix to put blanks where the p-value is deemed insig.

#same for only non-MS patients
nonMScohort <- subset(final_OCT_data,diagnosis == "CIS" | 
                        diagnosis == "Healthy Donor"|diagnosis ==  "NIND"|
                        diagnosis ==  "OIND"|diagnosis ==  "RIS"|diagnosis=="pOIND")

OCTcorr <- subset(nonMScohort, select = c("ILM_RPE_Thx_OD_Central", "ILM_RPE_Thx_OD_I1",
                                          "ILM_RPE_Thx_OD_I2","ILM_RPE_Thx_OD_N1",
                                          "ILM_RPE_Thx_OD_N2","ILM_RPE_Thx_OD_S1",
                                          "ILM_RPE_Thx_OD_S2","ILM_RPE_Thx_OD_T1",
                                          "ILM_RPE_Thx_OD_T2","ILM_RPE_Thx_OS_Central", 
                                          "ILM_RPE_Thx_OS_I1", "ILM_RPE_Thx_OS_I2","ILM_RPE_Thx_OS_N1",
                                          "ILM_RPE_Thx_OS_N2","ILM_RPE_Thx_OS_S1",
                                          "ILM_RPE_Thx_OS_S2","ILM_RPE_Thx_OS_T1",
                                          "ILM_RPE_Thx_OS_T2"))
OCTcorr <- rename(OCTcorr, OD_C = ILM_RPE_Thx_OD_Central, OD_I1= ILM_RPE_Thx_OD_I1,
                  OD_I2= ILM_RPE_Thx_OD_I2, OD_N1= ILM_RPE_Thx_OD_N1,
                  OD_N2= ILM_RPE_Thx_OD_N2, OD_S1= ILM_RPE_Thx_OD_S1,
                  OD_S2= ILM_RPE_Thx_OD_S2, OD_T1= ILM_RPE_Thx_OD_T1,
                  OD_T2= ILM_RPE_Thx_OD_T2, OS_C = ILM_RPE_Thx_OS_Central,
                  OS_I1 = ILM_RPE_Thx_OS_I1,OS_I2 = ILM_RPE_Thx_OS_I2,
                  OS_N1 = ILM_RPE_Thx_OS_N1,OS_N2 = ILM_RPE_Thx_OS_N2,
                  OS_S1 = ILM_RPE_Thx_OS_S1, OS_S2 = ILM_RPE_Thx_OS_S2,
                  OS_T1 = ILM_RPE_Thx_OS_T1,OS_T2 = ILM_RPE_Thx_OS_T2)

library(Hmisc)
OCTcorrMatrix <- rcorr(as.matrix(OCTcorr), type="pearson")
OCTcorrMatrix

OCTcorrMatrix$r #extract correlation coefficients
OCTcorrMatrix$n 
OCTcorrMatrix$P #extract p-values

library(corrplot)
corrplot(OCTcorrMatrix$r, method="circle", type="full", order = "hclust",
         p.mat=OCTcorrMatrix$P, sig.level = 0.01, insig="blank")

#What about just healthy donors?
healthydonors <- subset(final_OCT_data, diagnosis == "Healthy Donor")

ggplot(healthydonors, aes(x=height, y= ILM_RPE_Thx_OD_Central)) + 
  geom_point(aes(col=diagnosis)) +stat_cor(method="pearson") + 
  geom_smooth(method="lm") + stat_regline_equation(label.y=315)

#same eye
ggplot(healthydonors, aes(x=ILM_RPE_Thx_OS_N1, y=ILM_RPE_Thx_OS_I1)) + geom_point()+
  stat_cor(method="pearson")

ggplot(healthydonors, aes(x=ILM_RPE_Thx_OD_T2, y=ILM_RPE_Thx_OD_S1)) + geom_point()+
  stat_cor(method="pearson")

ggplot(healthydonors, aes(x=ILM_RPE_Thx_OS_Central, y=ILM_RPE_Thx_OS_S1)) + geom_point()+
  stat_cor(method="pearson")

#across eyes
ggplot(healthydonors, aes(x=ILM_RPE_Thx_OD_Central, y=ILM_RPE_Thx_OS_Central)) + geom_point() +
  stat_cor(method="pearson")
ggplot(healthydonors, aes(x=ILM_RPE_Thx_OD_N1, y=ILM_RPE_Thx_OS_N1)) + geom_point() +
  stat_cor(method="pearson")
ggplot(healthydonors, aes(x=ILM_RPE_Thx_OD_S2, y=ILM_RPE_Thx_OS_I2)) + geom_point() +
  stat_cor(method="pearson")
ggplot(healthydonors, aes(x=ILM_RPE_Thx_OD_Central, y=ILM_RPE_Thx_OS_I2)) + geom_point() +
  stat_cor(method="pearson")

OCTcorr <- subset(healthydonors, select = c("ILM_RPE_Thx_OD_Central", "ILM_RPE_Thx_OD_I1",
                                          "ILM_RPE_Thx_OD_I2","ILM_RPE_Thx_OD_N1",
                                          "ILM_RPE_Thx_OD_N2","ILM_RPE_Thx_OD_S1",
                                          "ILM_RPE_Thx_OD_S2","ILM_RPE_Thx_OD_T1",
                                          "ILM_RPE_Thx_OD_T2","ILM_RPE_Thx_OS_Central", 
                                          "ILM_RPE_Thx_OS_I1", "ILM_RPE_Thx_OS_I2","ILM_RPE_Thx_OS_N1",
                                          "ILM_RPE_Thx_OS_N2","ILM_RPE_Thx_OS_S1",
                                          "ILM_RPE_Thx_OS_S2","ILM_RPE_Thx_OS_T1",
                                          "ILM_RPE_Thx_OS_T2"))
OCTcorr <- rename(OCTcorr, OD_C = ILM_RPE_Thx_OD_Central, OD_I1= ILM_RPE_Thx_OD_I1,
                  OD_I2= ILM_RPE_Thx_OD_I2, OD_N1= ILM_RPE_Thx_OD_N1,
                  OD_N2= ILM_RPE_Thx_OD_N2, OD_S1= ILM_RPE_Thx_OD_S1,
                  OD_S2= ILM_RPE_Thx_OD_S2, OD_T1= ILM_RPE_Thx_OD_T1,
                  OD_T2= ILM_RPE_Thx_OD_T2, OS_C = ILM_RPE_Thx_OS_Central,
                  OS_I1 = ILM_RPE_Thx_OS_I1,OS_I2 = ILM_RPE_Thx_OS_I2,
                  OS_N1 = ILM_RPE_Thx_OS_N1,OS_N2 = ILM_RPE_Thx_OS_N2,
                  OS_S1 = ILM_RPE_Thx_OS_S1, OS_S2 = ILM_RPE_Thx_OS_S2,
                  OS_T1 = ILM_RPE_Thx_OS_T1,OS_T2 = ILM_RPE_Thx_OS_T2)

library(Hmisc)
OCTcorrMatrix <- rcorr(as.matrix(OCTcorr), type="pearson")
OCTcorrMatrix

OCTcorrMatrix$r #extract correlation coefficients
OCTcorrMatrix$n 
OCTcorrMatrix$P #extract p-values

library(corrplot)
corrplot(OCTcorrMatrix$r, method="circle", type="full", order = "hclust",
         p.mat=OCTcorrMatrix$P, sig.level = 0.01, insig="blank")


#other relationships (I'm curious)
ggplot(final_OCT_data, aes(x=Age, y=ILM_RPE_Thx_OS_Central)) + 
  geom_point(aes(col=diagnosis)) +
  stat_cor(method="pearson")+
  geom_smooth(method="lm") +
  stat_regline_equation(label.y = 350)
#Central fovea thickness increases with Age??
ggplot(nonMScohort, aes(x=Age, y=ILM_RPE_Thx_OS_Central)) + 
  geom_point(aes(col=diagnosis)) +
  stat_cor(method="pearson")+
  geom_smooth(method="lm") +
  stat_regline_equation(label.y = 350)
#non significant increase in non-MS cohort

ggplot(final_OCT_data, aes(x=height, y= ILM_RPE_Thx_OS_Central)) +
  geom_point(aes(col=diagnosis)) +
  stat_cor(method="pearson") +
  geom_smooth(method="lm")+ 
  stat_regline_equation(label.y=350)
ggplot(nonMScohort, aes(x=height, y= ILM_RPE_Thx_OS_Central)) +
  geom_point(aes(col=diagnosis)) +
  stat_cor(method="pearson") +
  geom_smooth(method="lm")+ 
  stat_regline_equation(label.y=350)
#significant increase in central fovea thickness with height in both full 
#data and non-MS cohort