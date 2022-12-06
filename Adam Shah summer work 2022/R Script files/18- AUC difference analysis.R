#In this document, I calculated the difference between AUC for binocular vision 
#and for better monocular eye to attempt to quantify the effect of processing in
#the visual cortex for different diagnostic groups.
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
library(forcats)
library(multilevel)

final_data <- read.csv("final_vision_data.csv")

#binocular AUC
VA_subset <- subset(final_data, select= c("VA_1.25_contrast_OU", 
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
final_data <- full_join(final_data, VA_subset, by="lcva_ID")

#OD
VA_subset_OD <- subset(final_data, select= c("VA_1.25_contrast_OD", 
                                                 "VA_2.5_contrast_OD",
                                                 "VA_100_contrast_OD",
                                                 "condition2", "lcva_ID"))
VA_subset_OD <- na.omit(VA_subset_OD)

n <- count(VA_subset_OD)[1,1]
res <- rep(NA, n)
slo <- rep(NA, n)
int <- rep(NA, n)
auc <- rep(NA, n)

for (i in 1:n){
  patient <- VA_subset_OD[i,]
  VA_OUsubset <- subset(patient, select = c("VA_1.25_contrast_OD", 
                                            "VA_2.5_contrast_OD", 
                                            "VA_100_contrast_OD",
                                            "condition2"))
  VA_OUsubset2 <- rename(VA_OUsubset, "33.33333" = VA_1.25_contrast_OD, 
                         "52.63158" = VA_2.5_contrast_OD, 
                         "100"=VA_100_contrast_OD)
  VA_OUsubsetlong2 <- melt(VA_OUsubset2, id="condition2")
  VA_OUsubsetlong2 <- rename(VA_OUsubsetlong2, scaled_contrast = variable, score=value)
  VA_OUsubsetlong2$scaled_contrast <- as.numeric(as.character(VA_OUsubsetlong2$scaled_contrast))
  
  r <- summary(lm(score~scaled_contrast, VA_OUsubsetlong2))
  res[i] <- r$r.squared
  slo[i] <- r$coefficients[2,1]
  int[i] <- r$coefficients[1,1]
  auc[i] <- 0.5*VA_subset_OD[i,"VA_100_contrast_OD"]*
    (100+(r$coefficients[1,1]/r$coefficients[2,1]))
}

VA_subset_OD$VA_rsquared_OD <- res
VA_subset_OD$VA_yint_OD <- int
VA_subset_OD$VA_slope_OD <- slo
VA_subset_OD$VA_AUC_OD <- auc

summary(VA_subset_OD)
summary(VA_subset_OD$VA_AUC)

VA_subset_OD <- subset(VA_subset_OD, select= c("lcva_ID",
                                         "VA_rsquared_OD",
                                         "VA_yint_OD",
                                         "VA_slope_OD",
                                         "VA_AUC_OD"))
final_data <- full_join(final_data, VA_subset_OD, by="lcva_ID")

#OS
VA_subset_OS <- subset(final_data, select= c("VA_1.25_contrast_OS", 
                                                    "VA_2.5_contrast_OS",
                                                    "VA_100_contrast_OS",
                                                    "condition2", "lcva_ID"))
VA_subset_OS <- na.omit(VA_subset_OS)

n <- count(VA_subset_OS)[1,1]
res <- rep(NA, n)
slo <- rep(NA, n)
int <- rep(NA, n)
auc <- rep(NA, n)

for (i in 1:n){
  patient <- VA_subset_OS[i,]
  VA_OUsubset <- subset(patient, select = c("VA_1.25_contrast_OS", 
                                            "VA_2.5_contrast_OS", 
                                            "VA_100_contrast_OS",
                                            "condition2"))
  VA_OUsubset2 <- rename(VA_OUsubset, "33.33333" = VA_1.25_contrast_OS, 
                         "52.63158" = VA_2.5_contrast_OS, 
                         "100"=VA_100_contrast_OS)
  VA_OUsubsetlong2 <- melt(VA_OUsubset2, id="condition2")
  VA_OUsubsetlong2 <- rename(VA_OUsubsetlong2, scaled_contrast = variable, score=value)
  VA_OUsubsetlong2$scaled_contrast <- as.numeric(as.character(VA_OUsubsetlong2$scaled_contrast))
  
  r <- summary(lm(score~scaled_contrast, VA_OUsubsetlong2))
  res[i] <- r$r.squared
  slo[i] <- r$coefficients[2,1]
  int[i] <- r$coefficients[1,1]
  auc[i] <- 0.5*VA_subset_OS[i,"VA_100_contrast_OS"]*
    (100+(r$coefficients[1,1]/r$coefficients[2,1]))
}

VA_subset_OS$VA_rsquared_OS <- res
VA_subset_OS$VA_yint_OS <- int
VA_subset_OS$VA_slope_OS <- slo
VA_subset_OS$VA_AUC_OS <- auc

summary(VA_subset_OS)
summary(VA_subset_OS$VA_AUC)

VA_subset_OS <- subset(VA_subset_OS, select= c("lcva_ID",
                                               "VA_rsquared_OS",
                                               "VA_yint_OS",
                                               "VA_slope_OS",
                                               "VA_AUC_OS"))
final_data <- full_join(final_data, VA_subset_OS, by="lcva_ID")


summary(final_data)
bwplot(final_data$VA_AUC)
bwplot(final_data$VA_AUC_OD)
bwplot(final_data$VA_AUC_OS)

#create subset to compare binocular AUC to better eye
AUC_data <- subset(final_data, select=c("lcva_ID", "VA_AUC", "VA_AUC_OD", "VA_AUC_OS"))
summary(AUC_data)
AUC_data <- na.omit(AUC_data)

head(final_data$VA_AUC_OD)
head(final_data$VA_AUC_OS)
#pmax function used to select better eye AUC
?pmax()
head(pmax(final_data$VA_AUC_OD,final_data$VA_AUC_OS))

final_data$AUC_diff <- final_data$VA_AUC - 
  pmax(final_data$VA_AUC_OD,final_data$VA_AUC_OS)

summary(final_data$AUC_diff)
bwplot(final_data$AUC_diff)

length(final_data[final_data$VA_AUC_OD > final_data$VA_AUC, ])
length(final_data[final_data$VA_AUC_OS > final_data$VA_AUC, ])

summary(final_data[final_data$VA_AUC_OS > final_data$VA_AUC, ])

#plotting difference between binocular and better eye
ggplot(final_data, aes(x=Age, y=AUC_diff)) +
  geom_point() +
  stat_cor(method="spearman")

ggplot(final_data, aes(x= condition2, y=AUC_diff)) +
  geom_boxplot()
ggplot(final_data, aes(x= condition2, y=VA_AUC)) +
  geom_boxplot()  

#probably a circular argument- patients with lower AUC in general will also likely
#have lower difference 

final_data$AUC_diff_bweyes <- abs(final_data$VA_AUC_OD-final_data$VA_AUC_OS)

ggplot(final_data, aes(x= condition2, y=AUC_diff_bweyes)) +
  geom_boxplot()

#scaled difference (percentage of better monocular eye compared to binocular vision)
final_data$AUC_mono_percentage <- (pmax(final_data$VA_AUC_OD,final_data$VA_AUC_OS)/
                                     final_data$VA_AUC)*100
summary(final_data$AUC_mono_percentage)
ggplot(final_data, aes(x= condition2, y=AUC_mono_percentage)) +
  geom_boxplot()

#This is better for quantifying effect of visual cortex