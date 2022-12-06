#In this document, I created elastic net models for VA AUC in the right and left 
#eyes (although I eventually found out I was not supposed to do this)
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

final_RNFL_data <- read.csv("final_vision_data.csv")

#elastic net for right eye VA_AUC
elasticAUC_OD <- subset(final_RNFL_data, select = c("RNFL_OD_Inferior", "RNFL_OD_Nasal",
                                                "RNFL_OD_Superior","RNFL_OD_Temporal",
                                                "Age", "ILM_RPE_Thx_OD_Central", "ILM_RPE_Thx_OD_I1",
                                                "ILM_RPE_Thx_OD_I2","ILM_RPE_Thx_OD_N1",
                                                "ILM_RPE_Thx_OD_N2","ILM_RPE_Thx_OD_S1",
                                                "ILM_RPE_Thx_OD_S2","ILM_RPE_Thx_OD_T1",
                                                "ILM_RPE_Thx_OD_T2", "VA_1.25_contrast_OD",
                                                "VA_2.5_contrast_OD", "VA_100_contrast_OD", "condition", "diagnosis")) 
summary(elasticAUC_OD)
elasticAUC_OD <- na.omit(elasticAUC_OD)
summary(elasticAUC_OD)

summary(elasticAUC_OD$VA_100_contrast_OD)
bwplot(elasticAUC_OD$VA_100_contrast_OD)

elasticAUC_OD <- elasticAUC_OD[elasticAUC_OD$VA_100_contrast_OD>19,]
summary(elasticAUC_OD)
bwplot(elasticAUC_OD$VA_100_contrast_OD)

n <- count(elasticAUC_OD)[1,1]
res <- rep(NA, n)
int <- rep(NA, n)
auc <- rep(NA, n)

for (i in 1:n){
  patient <- elasticAUC_OD[i,]
  VA_ODsubset <- subset(patient, select = c("VA_1.25_contrast_OD", 
                                            "VA_2.5_contrast_OD", 
                                            "VA_100_contrast_OD",
                                            "condition"))
  VA_ODsubset2 <- rename(VA_ODsubset, "33.33333" = VA_1.25_contrast_OD, 
                         "52.63158" = VA_2.5_contrast_OD, 
                         "100"=VA_100_contrast_OD)
  VA_ODsubsetlong2 <- melt(VA_ODsubset2, id="condition")
  VA_ODsubsetlong2 <- rename(VA_ODsubsetlong2, scaled_contrast = variable, score=value)
  VA_ODsubsetlong2$scaled_contrast <- as.numeric(as.character(VA_ODsubsetlong2$scaled_contrast))
  
  r <- summary(lm(score~scaled_contrast, VA_ODsubsetlong2))
  res[i] <- r$r.squared
  int[i] <- r$coefficients[1,1]
  auc[i] <- 0.5*elasticAUC_OD[i,"VA_100_contrast_OD"]*
    (100+(r$coefficients[1,1]/r$coefficients[2,1]))
}

summary(res)
bwplot(res)
head(res)
summary(int)
head(int)
bwplot(int)
summary(auc)
head(auc)
bwplot(auc)
elasticAUC_OD$VA_rsquared <- res
elasticAUC_OD$VA_yint <- int
elasticAUC_OD$VA_AUC <- auc

summary(elasticAUC_OD)
elasticAUC_OD[elasticAUC_OD$VA_rsquared < 0.55,]
elasticAUC_OD <- elasticAUC_OD[elasticAUC_OD$VA_rsquared>0.8,]
summary(elasticAUC_OD)

#data partition (create test and train groups)
set.seed(747)
ind <- sample(2, nrow(elasticAUC_OD), replace=T, prob = c((2/3), (1/3)))
train2OD <- elasticAUC_OD[ind==1,]
test2OD <- elasticAUC_OD[ind==2,]

summary(train2OD)

trainOD <- subset(train2OD, select = c("RNFL_OD_Inferior", "RNFL_OD_Nasal",
                                   "RNFL_OD_Superior","RNFL_OD_Temporal",
                                   "VA_AUC","Age", "ILM_RPE_Thx_OD_Central", "ILM_RPE_Thx_OD_I1",
                                   "ILM_RPE_Thx_OD_I2","ILM_RPE_Thx_OD_N1",
                                   "ILM_RPE_Thx_OD_N2","ILM_RPE_Thx_OD_S1",
                                   "ILM_RPE_Thx_OD_S2","ILM_RPE_Thx_OD_T1",
                                   "ILM_RPE_Thx_OD_T2"))
testOD <- subset(test2OD, select = c("RNFL_OD_Inferior", "RNFL_OD_Nasal",
                                 "RNFL_OD_Superior","RNFL_OD_Temporal",
                                 "VA_AUC","Age", "ILM_RPE_Thx_OD_Central", "ILM_RPE_Thx_OD_I1",
                                 "ILM_RPE_Thx_OD_I2","ILM_RPE_Thx_OD_N1",
                                 "ILM_RPE_Thx_OD_N2","ILM_RPE_Thx_OD_S1",
                                 "ILM_RPE_Thx_OD_S2","ILM_RPE_Thx_OD_T1",
                                 "ILM_RPE_Thx_OD_T2"))
#custom control parameters
custom <- trainControl(method= "repeatedcv", 
                       number = 10,
                       repeats= 5,
                       verboseIter = T)

#elastic net baby
set.seed(747)
elasticnetOD <- train(VA_AUC ~ ., 
                    trainOD,
                    method="glmnet",
                    tuneGrid= expand.grid(alpha=seq(0,1, length=10), 
                                          lambda = seq(100, 500, length=10)),
                    trControl=custom)
plot(elasticnetOD)
elasticnetOD
plot(elasticnetOD$finalModel, xvar="lambda", label=T)
plot(elasticnetOD$finalModel, xvar="dev", label=T)
plot(varImp(elasticnetOD, scale=F))

#best fit model parameters and coefficients
elasticnetOD$bestTune
best <- elasticnetOD$finalModel
coef(best, s = elasticnetOD$bestTune$lambda)

saveRDS(elasticnetOD, "full_elasticnet_model_OD.rds")
en <- readRDS("full_elasticnet_model_OD.rds")
print(en)

p1 <- predict(en, trainOD)
sqrt(mean((trainOD$VA_AUC-p1)^2))

trainOD$prediction <- p1
train2OD$prediction <- p1

p2 <- predict(en, testOD)
sqrt(mean((testOD$VA_AUC-p2)^2))

testOD$prediction <- p2
test2OD$prediction <- p2

ggplot(train2OD, aes(x=prediction, y=VA_AUC)) +
  geom_point(aes(col=condition)) +
  ggtitle("Training Observed vs. Predicted (OD)") +
  stat_cor(method="spearman", label.y=4250) +
  stat_regline_equation(label.y=4000, aes(label=..rr.label..)) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=4500) +
  geom_abline(slope=1, intercept=0)

ggplot(test2OD, aes(x=prediction, y=VA_AUC)) +
  geom_point(aes(col=condition)) +
  ggtitle("Test Observed vs. Predicted (OD)") +
  stat_cor(method="spearman", label.y=4250) +
  stat_regline_equation(label.y=4000, aes(label=..rr.label..)) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=4500) +
  geom_abline(slope=1, intercept=0)

#now try with OS
elasticAUC_OS <- subset(final_RNFL_data, select = c("RNFL_OS_Inferior", "RNFL_OS_Nasal",
                                                    "RNFL_OS_Superior","RNFL_OS_Temporal",
                                                    "Age", "ILM_RPE_Thx_OS_Central", "ILM_RPE_Thx_OS_I1",
                                                    "ILM_RPE_Thx_OS_I2","ILM_RPE_Thx_OS_N1",
                                                    "ILM_RPE_Thx_OS_N2","ILM_RPE_Thx_OS_S1",
                                                    "ILM_RPE_Thx_OS_S2","ILM_RPE_Thx_OS_T1",
                                                    "ILM_RPE_Thx_OS_T2", "VA_1.25_contrast_OS",
                                                    "VA_2.5_contrast_OS", "VA_100_contrast_OS", "condition", "diagnosis"))

summary(elasticAUC_OS)
elasticAUC_OS <- na.omit(elasticAUC_OS)
summary(elasticAUC_OS)

summary(elasticAUC_OS$VA_100_contrast_OS)
bwplot(elasticAUC_OS$VA_100_contrast_OS)

elasticAUC_OS <- elasticAUC_OS[elasticAUC_OS$VA_100_contrast_OS>19,]
summary(elasticAUC_OS)
bwplot(elasticAUC_OS$VA_100_contrast_OS)

n <- count(elasticAUC_OS)[1,1]
res <- rep(NA, n)
int <- rep(NA, n)
auc <- rep(NA, n)

for (i in 1:n){
  patient <- elasticAUC_OS[i,]
  VA_OSsubset <- subset(patient, select = c("VA_1.25_contrast_OS", 
                                            "VA_2.5_contrast_OS", 
                                            "VA_100_contrast_OS",
                                            "condition"))
  VA_OSsubset2 <- rename(VA_OSsubset, "33.33333" = VA_1.25_contrast_OS, 
                         "52.63158" = VA_2.5_contrast_OS, 
                         "100"=VA_100_contrast_OS)
  VA_OSsubsetlong2 <- melt(VA_OSsubset2, id="condition")
  VA_OSsubsetlong2 <- rename(VA_OSsubsetlong2, scaled_contrast = variable, score=value)
  VA_OSsubsetlong2$scaled_contrast <- as.numeric(as.character(VA_OSsubsetlong2$scaled_contrast))
  
  r <- summary(lm(score~scaled_contrast, VA_OSsubsetlong2))
  res[i] <- r$r.squared
  int[i] <- r$coefficients[1,1]
  auc[i] <- 0.5*elasticAUC_OS[i,"VA_100_contrast_OS"]*
    (100+(r$coefficients[1,1]/r$coefficients[2,1]))
}

summary(res)
head(res)
bwplot(res)
summary(int)
head(int)
summary(auc)
head(auc)
bwplot(auc)
elasticAUC_OS$VA_rsquared <- res
elasticAUC_OS$VA_yint <- int
elasticAUC_OS$VA_AUC <- auc

summary(elasticAUC_OS)
elasticAUC_OS <- elasticAUC_OS[elasticAUC_OS$VA_rsquared>0.8,]
summary(elasticAUC_OS)

#data partition (create test and train groups)
set.seed(707)
ind <- sample(2, nrow(elasticAUC_OS), replace=T, prob = c((2/3), (1/3)))
train2OS <- elasticAUC_OS[ind==1,]
test2OS <- elasticAUC_OS[ind==2,]

summary(train2OS)

trainOS <- subset(train2OS, select = c("RNFL_OS_Inferior", "RNFL_OS_Nasal",
                                       "RNFL_OS_Superior","RNFL_OS_Temporal",
                                       "VA_AUC","Age", "ILM_RPE_Thx_OS_Central", "ILM_RPE_Thx_OS_I1",
                                       "ILM_RPE_Thx_OS_I2","ILM_RPE_Thx_OS_N1",
                                       "ILM_RPE_Thx_OS_N2","ILM_RPE_Thx_OS_S1",
                                       "ILM_RPE_Thx_OS_S2","ILM_RPE_Thx_OS_T1",
                                       "ILM_RPE_Thx_OS_T2"))
testOS <- subset(test2OS, select = c("RNFL_OS_Inferior", "RNFL_OS_Nasal",
                                     "RNFL_OS_Superior","RNFL_OS_Temporal",
                                     "VA_AUC","Age", "ILM_RPE_Thx_OS_Central", "ILM_RPE_Thx_OS_I1",
                                     "ILM_RPE_Thx_OS_I2","ILM_RPE_Thx_OS_N1",
                                     "ILM_RPE_Thx_OS_N2","ILM_RPE_Thx_OS_S1",
                                     "ILM_RPE_Thx_OS_S2","ILM_RPE_Thx_OS_T1",
                                     "ILM_RPE_Thx_OS_T2"))
#custom control parameters
custom <- trainControl(method= "repeatedcv", 
                       number = 10,
                       repeats= 5,
                       verboseIter = T)

#elastic net baby
set.seed(747)
elasticnetOS <- train(VA_AUC ~ ., 
                      trainOS,
                      method="glmnet",
                      tuneGrid= expand.grid(alpha=seq(0,1, length=10), 
                                            lambda = seq(100, 1000, length=10)),
                      trControl=custom)
plot(elasticnetOS)
elasticnetOS
plot(elasticnetOS$finalModel, xvar="lambda", label=T)
plot(elasticnetOS$finalModel, xvar="dev", label=T)
plot(varImp(elasticnetOS, scale=F))

#best fit model parameters and coefficients
elasticnetOS$bestTune
best <- elasticnetOS$finalModel
coef(best, s = elasticnetOS$bestTune$lambda)

saveRDS(elasticnetOS, "full_elasticnet_model_OS.rds")
en <- readRDS("full_elasticnet_model_OS.rds")
print(en)

p1 <- predict(en, trainOS)
sqrt(mean((trainOS$VA_AUC-p1)^2))

#trainOS$prediction <- p1
train2OS$prediction <- p1

p2 <- predict(en, testOS)
sqrt(mean((testOS$VA_AUC-p2)^2))

#testOS$prediction <- p2
test2OS$prediction <- p2

ggplot(train2OS, aes(x=prediction, y=VA_AUC)) +
  geom_point(aes(col=condition)) +
  ggtitle("Training Observed vs. Predicted (OS)") +
  stat_cor(method="spearman", label.y=4250) +
  stat_regline_equation(label.y=4000, aes(label=..rr.label..)) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=4500) +
  geom_abline(slope=1, intercept=0)

ggplot(test2OS, aes(x=prediction, y=VA_AUC)) +
  geom_point(aes(col=condition)) +
  ggtitle("Test Observed vs. Predicted (OS)") +
  stat_cor(method="spearman", label.y=4250) +
  stat_regline_equation(label.y=4000, aes(label=..rr.label..)) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=4500) +
  geom_abline(slope=1, intercept=0)

