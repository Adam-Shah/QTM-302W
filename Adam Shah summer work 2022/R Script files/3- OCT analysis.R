#This is just ILM-RPE OCT (not RNFL)
#In this document, I looked for correlations in OCT measurements and created 
#correlation tables for ILM-RPE 
#This was from before I had the full dataset
#APS

library(dplyr)
library(ggplot2)
library(plotly)

full_data <- read.csv("full_vision_data.csv")
summary(full_data)

#effect of age on macular degeneration
q <- ggplot(full_data, aes(x= Age, y= Macular_Volume_OS)) + geom_point(aes(col=diagnosis))
ggplotly(q)

q <- ggplot(subset(full_data,diagnosis != "" ), aes(x=Age, y= ILM_RPE_Thx_OD_T1)) + geom_point(aes(col=diagnosis))
ggplotly(q)


#OCT and RNFL measurements?
ggplotly(ggplot(subset(full_data,diagnosis != "" ), aes(x=ILM_RPE_Thx_OD_S1, y=RNFL_OD_Superior)) +geom_point(aes(col=diagnosis)))

#correlation for all OCT outcomes in same eye
ggplot(subset(full_data,diagnosis != "" ), aes(x=ILM_RPE_Thx_OS_N1, y=ILM_RPE_Thx_OS_I1)) + geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson")

ggplot(subset(full_data,diagnosis != "" ), aes(x=ILM_RPE_Thx_OD_T2, y=ILM_RPE_Thx_OD_S1)) + geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson")

ggplot(subset(full_data,diagnosis != "" ), aes(x=ILM_RPE_Thx_OS_Central, y=ILM_RPE_Thx_OS_S1)) + geom_point(aes(col=diagnosis))+
  stat_cor(method="pearson")

ggplotly(ggplot(full_data, aes(x=ILM_RPE_Thx_OS_S1, y=ILM_RPE_Thx_OS_S2)) + geom_point(aes(col=diagnosis)))

ggplot(subset(full_data,diagnosis != ""), 
       aes(x=ILM_RPE_Thx_OS_N1, y=ILM_RPE_Thx_OS_I2)) +
  geom_point(aes(col=diagnosis))+stat_cor(method="pearson")

#How to make plot with only certain diagnoses (excluding MS cohort)
ggplot(subset(full_data,diagnosis == "CIS" | diagnosis == "Healthy Donor"|diagnosis ==  "NIND"|diagnosis ==  "OIND"|diagnosis ==  "RIS"), 
       aes(x=ILM_RPE_Thx_OS_S1, y=ILM_RPE_Thx_OS_S2)) +
  geom_point(aes(col=diagnosis))
#ggplot(full_data[full_data$diagnosis == c("CIS", "Healthy Donor", "NIND", "OIND", "RIS"),], aes(x=ILM_RPE_Thx_OS_S1, y=ILM_RPE_Thx_OS_S2)) +
#  geom_point(aes(col=diagnosis))
?subset

#graphs with non-MS patients
ggplot(subset(full_data,diagnosis == "CIS" | diagnosis == "Healthy Donor"|diagnosis ==  "NIND"|diagnosis ==  "OIND"|diagnosis ==  "RIS"), 
       aes(x=ILM_RPE_Thx_OS_Central, y=ILM_RPE_Thx_OD_Central)) +
  geom_point(aes(col=diagnosis)) +stat_cor(method="pearson")

ggplot(subset(full_data,diagnosis == "CIS" | diagnosis == "Healthy Donor"|diagnosis ==  "NIND"|diagnosis ==  "OIND"|diagnosis ==  "RIS"), 
       aes(x=ILM_RPE_Thx_OS_N1, y=ILM_RPE_Thx_OS_I2)) +
  geom_point(aes(col=diagnosis)) + stat_cor(method="pearson")

ggplot(subset(full_data,diagnosis == "CIS" | diagnosis == "Healthy Donor"|diagnosis ==  "NIND"|diagnosis ==  "OIND"|diagnosis ==  "RIS"), 
       aes(x=ILM_RPE_Thx_OD_I2, y=ILM_RPE_Thx_OS_I2)) +
  geom_point(aes(col=diagnosis)) + stat_cor(method="pearson")

#correlation in OCT outcomes across eyes
ggplot(subset(full_data,diagnosis != "" ), aes(x=ILM_RPE_Thx_OD_Central, y=ILM_RPE_Thx_OS_Central)) + geom_point(aes(col=diagnosis)) +
  stat_cor(method="pearson")
ggplot(subset(full_data,diagnosis != "" ), aes(x=ILM_RPE_Thx_OD_I1, y=ILM_RPE_Thx_OS_I1)) + geom_point(aes(col=diagnosis)) +
  stat_cor(method="pearson")
ggplot(subset(full_data,diagnosis != "" ), aes(x=ILM_RPE_Thx_OD_S2, y=ILM_RPE_Thx_OS_T1)) + geom_point(aes(col=diagnosis)) +
  stat_cor(method="pearson")


#correlation matrix and p-values
OCTcorr <- subset(full_data, select = c("ILM_RPE_Thx_OD_Central", "ILM_RPE_Thx_OD_I1",
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
nonMScohort <- subset(full_data,diagnosis == "CIS" | 
                        diagnosis == "Healthy Donor"|diagnosis ==  "NIND"|
                        diagnosis ==  "OIND"|diagnosis ==  "RIS")

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
corrplot(OCTcorrMatrix$r, method="circle", type="full", order= "hclust",
         p.mat=OCTcorrMatrix$P, sig.level = 0.01, insig="blank")
