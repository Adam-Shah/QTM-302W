#In this document, I created correlation matrices with all OCT and VA measures,
#experimented with calculating AUC, created (and eventually combined)
#for loops to calculate AUC and other measures form individual VA regressions,
#and found more relationships with those new measures
#There were a few attempts that weren't quite right
#APS

library(ggplot2)
library(ggpubr)
library(dplyr)
library(plotly)
library(corrplot)
library(Hmisc)
library(reshape2)

VA_OCT_corr <- subset(normaleyesRNFL, select = c("RNFL_OU_Inferior", "RNFL_OU_Nasal",
                                                  "RNFL_OU_Superior","RNFL_OU_Temporal",
                                                  "VA_1.25_contrast_OU","VA_2.5_contrast_OU",
                                                  "VA_100_contrast_OU","Age", "height","ILM_RPE_Thx_OD_Central", "ILM_RPE_Thx_OD_I1",
                                                  "ILM_RPE_Thx_OD_I2","ILM_RPE_Thx_OD_N1",
                                                  "ILM_RPE_Thx_OD_N2","ILM_RPE_Thx_OD_S1",
                                                  "ILM_RPE_Thx_OD_S2","ILM_RPE_Thx_OD_T1",
                                                  "ILM_RPE_Thx_OD_T2","ILM_RPE_Thx_OS_Central", 
                                                  "ILM_RPE_Thx_OS_I1", "ILM_RPE_Thx_OS_I2","ILM_RPE_Thx_OS_N1",
                                                  "ILM_RPE_Thx_OS_N2","ILM_RPE_Thx_OS_S1",
                                                  "ILM_RPE_Thx_OS_S2","ILM_RPE_Thx_OS_T1",
                                                  "ILM_RPE_Thx_OS_T2"))
VA_OCT_corr <- rename(VA_OCT_corr, RNFL_OU_I = RNFL_OU_Inferior, RNFL_OU_N= RNFL_OU_Nasal,
                       RNFL_OU_S= RNFL_OU_Superior, RNFL_OU_T= RNFL_OU_Temporal,
                       VA_OU_1.25= VA_1.25_contrast_OU, VA_OU_2.5= VA_2.5_contrast_OU,
                       VA_OU_100= VA_100_contrast_OU,
                      Mac_OD_C = ILM_RPE_Thx_OD_Central, Mac_OD_I1= ILM_RPE_Thx_OD_I1,
                       Mac_OD_I2= ILM_RPE_Thx_OD_I2, Mac_OD_N1= ILM_RPE_Thx_OD_N1,
                       Mac_OD_N2= ILM_RPE_Thx_OD_N2, Mac_OD_S1= ILM_RPE_Thx_OD_S1,
                       Mac_OD_S2= ILM_RPE_Thx_OD_S2, Mac_OD_T1= ILM_RPE_Thx_OD_T1,
                       Mac_OD_T2= ILM_RPE_Thx_OD_T2, Mac_OS_C = ILM_RPE_Thx_OS_Central,
                       Mac_OS_I1 = ILM_RPE_Thx_OS_I1,Mac_OS_I2 = ILM_RPE_Thx_OS_I2,
                       Mac_OS_N1 = ILM_RPE_Thx_OS_N1,Mac_OS_N2 = ILM_RPE_Thx_OS_N2,
                       Mac_OS_S1 = ILM_RPE_Thx_OS_S1, Mac_OS_S2 = ILM_RPE_Thx_OS_S2,
                       Mac_OS_T1 = ILM_RPE_Thx_OS_T1,Mac_OS_T2 = ILM_RPE_Thx_OS_T2)
summary(VA_OCT_corr)
library(Hmisc)

VA_OCT_corrMatrix <- rcorr(as.matrix(VA_OCT_corr), type="spearman")
VA_OCT_corrMatrix

VA_OCT_corrMatrix$r #extract correlation coefficients
VA_OCT_corrMatrix$n 
VA_OCT_corrMatrix$P #extract p-values

library(corrplot)
corrplot(VA_OCT_corrMatrix$r, method="circle", type="full", order= "hclust",
         p.mat=VA_OCT_corrMatrix$P, sig.level = 0.05, insig="blank", tl.cex=0.5)

#use averages across eyes for ILM_RPE measures 
ggplotly(ggplot(normaleyesRNFL, aes(x=ILM_RPE_Thx_OD_Central, y=ILM_RPE_Thx_OS_Central)) +
  geom_point(aes(col=diagnosis))+
  geom_smooth(method="lm", formula=y~x-1, se=F)+
  stat_regline_equation(formula=y~x-1))

normeyes_RNFL_OCT <- subset(normaleyesRNFL, subset = ILM_RPE_Thx_OS_Central != 349 & ILM_RPE_Thx_OS_Central != 359)

ggplotly(ggplot(normeyes_RNFL_OCT, aes(x=ILM_RPE_Thx_OD_N1, y=ILM_RPE_Thx_OS_N1)) +
           geom_point(aes(col=diagnosis))+
           geom_smooth(method="lm", formula=y~x-1, se=F)+
           stat_regline_equation(formula=y~x-1))

normeyes_RNFL_OCT$ILM_RPE_Thx_OU_Central <- (normeyes_RNFL_OCT$ILM_RPE_Thx_OD_Central + normeyes_RNFL_OCT$ILM_RPE_Thx_OS_Central)/2
summary(normeyes_RNFL_OCT$ILM_RPE_Thx_OU_Central)
summary(normeyes_RNFL_OCT$ILM_RPE_Thx_OD_Central)
summary(normeyes_RNFL_OCT$ILM_RPE_Thx_OS_Central)

normeyes_RNFL_OCT$ILM_RPE_Thx_OU_S1 <- (normeyes_RNFL_OCT$ILM_RPE_Thx_OD_S1 + normeyes_RNFL_OCT$ILM_RPE_Thx_OS_S1)/2
summary(normeyes_RNFL_OCT$ILM_RPE_Thx_OU_S1)
summary(normeyes_RNFL_OCT$ILM_RPE_Thx_OD_S1)
summary(normeyes_RNFL_OCT$ILM_RPE_Thx_OS_S1)

normeyes_RNFL_OCT$ILM_RPE_Thx_OU_S2 <- (normeyes_RNFL_OCT$ILM_RPE_Thx_OD_S2 + normeyes_RNFL_OCT$ILM_RPE_Thx_OS_S2)/2
normeyes_RNFL_OCT$ILM_RPE_Thx_OU_N1 <- (normeyes_RNFL_OCT$ILM_RPE_Thx_OD_N1 + normeyes_RNFL_OCT$ILM_RPE_Thx_OS_N1)/2
normeyes_RNFL_OCT$ILM_RPE_Thx_OU_I1 <- (normeyes_RNFL_OCT$ILM_RPE_Thx_OD_I1 + normeyes_RNFL_OCT$ILM_RPE_Thx_OS_I1)/2
normeyes_RNFL_OCT$ILM_RPE_Thx_OU_T1 <- (normeyes_RNFL_OCT$ILM_RPE_Thx_OD_T1 + normeyes_RNFL_OCT$ILM_RPE_Thx_OS_T1)/2
normeyes_RNFL_OCT$ILM_RPE_Thx_OU_N2 <- (normeyes_RNFL_OCT$ILM_RPE_Thx_OD_N2 + normeyes_RNFL_OCT$ILM_RPE_Thx_OS_N2)/2
normeyes_RNFL_OCT$ILM_RPE_Thx_OU_I2 <- (normeyes_RNFL_OCT$ILM_RPE_Thx_OD_I2 + normeyes_RNFL_OCT$ILM_RPE_Thx_OS_I2)/2
normeyes_RNFL_OCT$ILM_RPE_Thx_OU_T2 <- (normeyes_RNFL_OCT$ILM_RPE_Thx_OD_T2 + normeyes_RNFL_OCT$ILM_RPE_Thx_OS_T2)/2

#new correlation matrix
VA_OCT_corr <- subset(normeyes_RNFL_OCT, select = c("RNFL_OU_Inferior", "RNFL_OU_Nasal",
                                                 "RNFL_OU_Superior","RNFL_OU_Temporal",
                                                 "VA_1.25_contrast_OU","VA_2.5_contrast_OU",
                                                 "VA_100_contrast_OU","Age", "height","ILM_RPE_Thx_OU_Central", "ILM_RPE_Thx_OU_I1",
                                                 "ILM_RPE_Thx_OU_I2","ILM_RPE_Thx_OU_N1",
                                                 "ILM_RPE_Thx_OU_N2","ILM_RPE_Thx_OU_S1",
                                                 "ILM_RPE_Thx_OU_S2","ILM_RPE_Thx_OU_T1",
                                                 "ILM_RPE_Thx_OU_T2"))
VA_OCT_corr <- rename(VA_OCT_corr, RNFL_OU_I = RNFL_OU_Inferior, RNFL_OU_N= RNFL_OU_Nasal,
                      RNFL_OU_S= RNFL_OU_Superior, RNFL_OU_T= RNFL_OU_Temporal,
                      VA_OU_1.25= VA_1.25_contrast_OU, VA_OU_2.5= VA_2.5_contrast_OU,
                      VA_OU_100= VA_100_contrast_OU,
                      Mac_OU_C = ILM_RPE_Thx_OU_Central, Mac_OU_I1= ILM_RPE_Thx_OU_I1,
                      Mac_OU_I2= ILM_RPE_Thx_OU_I2, Mac_OU_N1= ILM_RPE_Thx_OU_N1,
                      Mac_OU_N2= ILM_RPE_Thx_OU_N2, Mac_OU_S1= ILM_RPE_Thx_OU_S1,
                      Mac_OU_S2= ILM_RPE_Thx_OU_S2, Mac_OU_T1= ILM_RPE_Thx_OU_T1,
                      Mac_OU_T2= ILM_RPE_Thx_OU_T2)
summary(VA_OCT_corr)
library(Hmisc)

VA_OCT_corrMatrix <- rcorr(as.matrix(VA_OCT_corr), type="spearman")
VA_OCT_corrMatrix

VA_OCT_corrMatrix$r #extract correlation coefficients
VA_OCT_corrMatrix$n 
VA_OCT_corrMatrix$P #extract p-values

library(corrplot)
corrplot(VA_OCT_corrMatrix$r, method="circle", type="full", order= "hclust",
         p.mat=VA_OCT_corrMatrix$P, sig.level = 0.05, insig="blank", tl.cex=0.5)


#attempting AUC for 1 patient
patient1 <- normeyes_RNFL_OCT[1,]

.5*patient1$VA_100_contrast_OU*(100-(200/3)*((3/2)-(patient1$VA_100_contrast_OU/(patient1$VA_100_contrast_OU-patient1$VA_1.25_contrast_OU))))
patient1$VA_100_contrast_OU
patient1$VA_1.25_contrast_OU

normeyes_RNFL_OCT[1,"VA_100_contrast_OU"]
normeyes_RNFL_OCT[1,"VA_1.25_contrast_OU"]
0.5*normeyes_RNFL_OCT[1,"VA_100_contrast_OU"]*(100-(200/3)*((3/2)-(normeyes_RNFL_OCT[1,"VA_100_contrast_OU"]/(normeyes_RNFL_OCT[1,"VA_100_contrast_OU"]-normeyes_RNFL_OCT[1,"VA_1.25_contrast_OU"]))))


#for loop for AUC
n <- count(normeyes_RNFL_OCT)[1,1]
n
n <- 193
res <- rep(NA, n)
  
for (i in 1:n){
  res[i] <- 0.5*normeyes_RNFL_OCT[i,"VA_100_contrast_OU"]*(100-(200/3)*((3/2)-(
    normeyes_RNFL_OCT[i,"VA_100_contrast_OU"]/(normeyes_RNFL_OCT[i,"VA_100_contrast_OU"]-
                                                 normeyes_RNFL_OCT[i,"VA_1.25_contrast_OU"]))))
}
summary(res)
normeyes_RNFL_OCT$VA_AUC <- res
summary(normeyes_RNFL_OCT$VA_AUC)
head(normeyes_RNFL_OCT)

#for loop for intercept
n <- count(normeyes_RNFL_OCT)[1,1]
int <- rep(NA, n)

for(i in 1:n){
  int[i] <- (normeyes_RNFL_OCT[i,"VA_100_contrast_OU"] - 
               (3/2)*(normeyes_RNFL_OCT[i,"VA_100_contrast_OU"]-
                        normeyes_RNFL_OCT[i,"VA_1.25_contrast_OU"]))
    
}
summary(int)
normeyes_RNFL_OCT$VA_yint <- int
summary(normeyes_RNFL_OCT$VA_yint)

##different for just normal RNFL?
n <- count(normaleyesRNFL)[1,1]
n

res <- rep(NA, n)

for (i in 1:n){
  res[i] <- 0.5*normaleyesRNFL[i,"VA_100_contrast_OU"]*(100-(200/3)*((3/2)-(
    normaleyesRNFL[i,"VA_100_contrast_OU"]/(normaleyesRNFL[i,"VA_100_contrast_OU"]-
                                                 normaleyesRNFL[i,"VA_1.25_contrast_OU"]))))
}
summary(res)
normaleyesRNFL$VA_AUC <- res
summary(normaleyesRNFL$VA_AUC)
head(normaleyesRNFL)

#for loop for intercept
n <- count(normaleyesRNFL)[1,1]
int <- rep(NA, n)

for(i in 1:n){
  int[i] <- (normaleyesRNFL[i,"VA_100_contrast_OU"] - 
               (3/2)*(normaleyesRNFL[i,"VA_100_contrast_OU"]-
                        normaleyesRNFL[i,"VA_1.25_contrast_OU"]))
  
}
summary(int)
normaleyesRNFL$VA_yint <- int
summary(normaleyesRNFL$VA_yint)

#for loop for R-squared?
VA_OUsubset <- subset(normeyes_RNFL_OCT, select = c("VA_1.25_contrast_OU", 
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

summary(VA_OUsubsetlong2$contrast)

sapply(VA_OUsubsetlong2, class)
ggplot(VA_OUsubsetlong2, aes(x=as.numeric(as.character(scaled_contrast)),y=score)) +
  geom_boxplot(aes(fill=scaled_contrast)) +xlab("scaled contrast") +
  coord_cartesian(xlim=c(0,100))+
  geom_abline(slope=0.57, intercept=0)

#how to do auc with regression line?
patient <- normeyes_RNFL_OCT[5,]
VA_OUsubset <- subset(patient, select = c("VA_1.25_contrast_OU", 
                                          "VA_2.5_contrast_OU", 
                                          "VA_100_contrast_OU",
                                          "condition"))
VA_OUsubset2 <- rename(VA_OUsubset, "33.33333" = VA_1.25_contrast_OU, 
                       "52.63158" = VA_2.5_contrast_OU, 
                       "100"=VA_100_contrast_OU)
VA_OUsubsetlong2 <- melt(VA_OUsubset2, id="condition")
VA_OUsubsetlong2 <- rename(VA_OUsubsetlong2, scaled_contrast = variable, score=value)
VA_OUsubsetlong2$scaled_contrast <- as.numeric(as.character(VA_OUsubsetlong2$scaled_contrast))

r <- summary(lm(score~scaled_contrast, VA_OUsubsetlong2))
res[i] <- r$r.squared
int[i] <- r$coefficients[1,1]
r
r$coefficients[1,1]
r$coefficients[2,1]
(100+(r$coefficients[1,1]/r$coefficients[2,1]))
0.5*normeyes_RNFL_OCT[5,"VA_100_contrast_OU"]*(100+(r$coefficients[1,1]/r$coefficients[2,1]))


#lets try this
n <- count(normeyes_RNFL_OCT)[1,1]
res <- rep(NA, n)
int <- rep(NA, n)
auc <- rep(NA, n)
slo <- rep(NA, n)

for (i in 1:n){
  patient <- normeyes_RNFL_OCT[i,]
  VA_OUsubset <- subset(patient, select = c("VA_1.25_contrast_OU", 
                                            "VA_2.5_contrast_OU", 
                                            "VA_100_contrast_OU",
                                            "condition"))
  VA_OUsubset2 <- rename(VA_OUsubset, "33.33333" = VA_1.25_contrast_OU, 
                         "52.63158" = VA_2.5_contrast_OU, 
                         "100"=VA_100_contrast_OU)
  VA_OUsubsetlong2 <- melt(VA_OUsubset2, id="condition")
  VA_OUsubsetlong2 <- rename(VA_OUsubsetlong2, scaled_contrast = variable, score=value)
  VA_OUsubsetlong2$scaled_contrast <- as.numeric(as.character(VA_OUsubsetlong2$scaled_contrast))
  
  r <- summary(lm(score~scaled_contrast, VA_OUsubsetlong2))
  res[i] <- r$r.squared
  int[i] <- r$coefficients[1,1]
  slo[i] <- r$coefficients[2,1]
  auc[i] <- 0.5*normeyes_RNFL_OCT[i,"VA_100_contrast_OU"]*
    (100+(r$coefficients[1,1]/r$coefficients[2,1]))
}
summary(res)
head(res)
summary(int)
head(int)
summary(auc)
head(auc)
normeyes_RNFL_OCT$VA_rsquared <- res
normeyes_RNFL_OCT$VA_yint <- int
normeyes_RNFL_OCT$VA_slope <- slo
normeyes_RNFL_OCT$VA_AUC <- auc

#bingo#

summary(normeyes_RNFL_OCT)
ggplot(normeyes_RNFL_OCT, aes(x=VA_yint, y=RNFL_OU_Temporal)) +
  geom_point(aes(col=diagnosis))+
  stat_cor(method="spearman") +
  geom_smooth(method="lm") +
  stat_regline_equation(label.y=100)

ggplot(normeyes_RNFL_OCT, aes(x=VA_AUC, y=RNFL_OU_Temporal)) +
  geom_point(aes(col=diagnosis))+
  stat_cor(method="spearman") +
  geom_smooth(method="lm") +
  stat_regline_equation(label.y=100)

ggplot(normeyes_RNFL_OCT, aes(x=RNFL_OU_Temporal, y=VA_rsquared)) +
  geom_point(aes(col=diagnosis))+
  stat_cor(method="spearman") +
  geom_smooth(method="lm") +
  stat_regline_equation(label.y=1)

#new correlations with new VA measures

VA_OCT_corr <- subset(normeyes_RNFL_OCT, select = c("RNFL_OU_Inferior", "RNFL_OU_Nasal",
                                                    "RNFL_OU_Superior","RNFL_OU_Temporal",
                                                    "VA_yint","VA_rsquared",
                                                    "VA_AUC","Age", "height","ILM_RPE_Thx_OU_Central", "ILM_RPE_Thx_OU_I1",
                                                    "ILM_RPE_Thx_OU_I2","ILM_RPE_Thx_OU_N1",
                                                    "ILM_RPE_Thx_OU_N2","ILM_RPE_Thx_OU_S1",
                                                    "ILM_RPE_Thx_OU_S2","ILM_RPE_Thx_OU_T1",
                                                    "ILM_RPE_Thx_OU_T2", "NeurEx_total"))
VA_OCT_corr <- rename(VA_OCT_corr, RNFL_OU_I = RNFL_OU_Inferior, RNFL_OU_N= RNFL_OU_Nasal,
                      RNFL_OU_S= RNFL_OU_Superior, RNFL_OU_T= RNFL_OU_Temporal,
                      Mac_OU_C = ILM_RPE_Thx_OU_Central, Mac_OU_I1= ILM_RPE_Thx_OU_I1,
                      Mac_OU_I2= ILM_RPE_Thx_OU_I2, Mac_OU_N1= ILM_RPE_Thx_OU_N1,
                      Mac_OU_N2= ILM_RPE_Thx_OU_N2, Mac_OU_S1= ILM_RPE_Thx_OU_S1,
                      Mac_OU_S2= ILM_RPE_Thx_OU_S2, Mac_OU_T1= ILM_RPE_Thx_OU_T1,
                      Mac_OU_T2= ILM_RPE_Thx_OU_T2)
summary(VA_OCT_corr)
library(Hmisc)

VA_OCT_corrMatrix <- rcorr(as.matrix(VA_OCT_corr), type="spearman")
VA_OCT_corrMatrix

VA_OCT_corrMatrix$r #extract correlation coefficients
VA_OCT_corrMatrix$n 
VA_OCT_corrMatrix$P #extract p-values

library(corrplot)
corrplot(VA_OCT_corrMatrix$r, method="circle", type="full", order= "hclust",
         p.mat=VA_OCT_corrMatrix$P, sig.level = 0.05, insig="blank", tl.cex=0.5)

#remove some patients that have poor VA 100 contrast score
summary(normeyes_RNFL_OCT$VA_100_contrast_OU)
normeyes_new <- subset(normeyes_RNFL_OCT, VA_100_contrast_OU > 30)
summary(normeyes_new)

#same results for isolating MS patients 

#why doesn't this work?
MScohort <- subset(normaleyesRNFL, condition == "MS")

summary(MScohort)
VA_OCT_corr <- subset(MScohort, select = c("RNFL_OU_Inferior", "RNFL_OU_Nasal",
                                                    "RNFL_OU_Superior","RNFL_OU_Temporal",
                                                    "VA_1.25_contrast_OU","VA_2.5_contrast_OU",
                                                    "VA_100_contrast_OU","Age", "height",
                                                    "ILM_RPE_Thx_OU_Central", "ILM_RPE_Thx_OU_I1",
                                                    "ILM_RPE_Thx_OU_I2","ILM_RPE_Thx_OU_N1",
                                                    "ILM_RPE_Thx_OU_N2","ILM_RPE_Thx_OU_S1",
                                                    "ILM_RPE_Thx_OU_S2","ILM_RPE_Thx_OU_T1",
                                                    "ILM_RPE_Thx_OU_T2"))
VA_OCT_corr <- rename(VA_OCT_corr, RNFL_OU_I = RNFL_OU_Inferior, RNFL_OU_N= RNFL_OU_Nasal,
                      RNFL_OU_S= RNFL_OU_Superior, RNFL_OU_T= RNFL_OU_Temporal,
                      VA_OU_1.25= VA_1.25_contrast_OU, VA_OU_2.5= VA_2.5_contrast_OU,
                      VA_OU_100= VA_100_contrast_OU,
                      Mac_OU_C = ILM_RPE_Thx_OU_Central, Mac_OU_I1= ILM_RPE_Thx_OU_I1,
                      Mac_OU_I2= ILM_RPE_Thx_OU_I2, Mac_OU_N1= ILM_RPE_Thx_OU_N1,
                      Mac_OU_N2= ILM_RPE_Thx_OU_N2, Mac_OU_S1= ILM_RPE_Thx_OU_S1,
                      Mac_OU_S2= ILM_RPE_Thx_OU_S2, Mac_OU_T1= ILM_RPE_Thx_OU_T1,
                      Mac_OU_T2= ILM_RPE_Thx_OU_T2)
summary(VA_OCT_corr)
library(Hmisc)

VA_OCT_corrMatrix <- rcorr(as.matrix(VA_OCT_corr), type="spearman")
VA_OCT_corrMatrix

VA_OCT_corrMatrix$r #extract correlation coefficients
VA_OCT_corrMatrix$n 
VA_OCT_corrMatrix$P #extract p-values

library(corrplot)
corrplot(VA_OCT_corrMatrix$r, method="circle", type="full", order= "hclust",
         p.mat=VA_OCT_corrMatrix$P, sig.level = 0.05, insig="blank", tl.cex=0.5)


#other correlations with AUC and intercept
ggplot(normeyes_new, aes(x=VA_AUC, y=VA_yint)) +
  geom_point(aes(col=diagnosis)) +
  stat_cor(method="spearman") +
  geom_smooth(method="lm", formula=y~log(x), se=F) +
  stat_regline_equation(label.y=20, formula= y~log(x))

?geom_smooth
ggplot(normeyes_new, aes(x=Age, y=VA_AUC)) +
  geom_point(aes(col=diagnosis))+
  stat_cor(method="spearman") +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=5200)

ggplot(normeyes_new, aes(x=Age, y=VA_yint)) +
  geom_point(aes(col=diagnosis))+
  stat_cor(method="spearman")+
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=20)

ggplot(normeyes_new, aes(x=height, y=VA_AUC)) +
  geom_point(aes(col=diagnosis))+
  stat_cor(method="spearman")+
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=5200)

ggplot(normeyes_new, aes(x=height, y=VA_yint)) +
  geom_point(aes(col=diagnosis))+
  stat_cor(method="spearman")+
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=20)

#negative correlation with all NeurEx panels!
ggplot(normeyes_new, aes(x=NeurEx_Panel_02_L, y=VA_yint)) +
  geom_point(aes(col=diagnosis))+
  stat_cor(method="spearman")
summary(normeyes_new)

ggplot(normeyes_new, aes(x=NeurEx_total, y=VA_AUC)) +
  geom_point(aes(col=diagnosis))+
  stat_cor(method="spearman")+
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=5200)
ggplot(normeyes_new, aes(x=NeurEx_total, y=VA_yint)) +
  geom_point(aes(col=diagnosis))+
  stat_cor(method="spearman")+
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=20)

#w/ RNFL
ggplot(normeyes_new, aes(x=RNFL_OU_Inferior, y=VA_AUC)) +
  geom_point(aes(col=diagnosis))+
  stat_cor(method="spearman")+
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=5200)
ggplot(normeyes_new, aes(x=RNFL_OU_Superior, y=VA_AUC)) +
  geom_point(aes(col=diagnosis))+
  stat_cor(method="spearman")+
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=20)
ggplot(normeyes_new, aes(x=RNFL_OU_Temporal, y=VA_AUC)) +
  geom_point(aes(col=diagnosis))+
  stat_cor(method="spearman")+
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=20)
ggplot(normeyes_new, aes(x=RNFL_OU_Nasal, y=VA_AUC)) +
  geom_point(aes(col=diagnosis))+
  stat_cor(method="spearman")+
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=5200)

summary(normeyes_new)
