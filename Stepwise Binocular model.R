#In this document, I reconstructed the binocular VA AUC model in a stepwise manner
#to include first OCT, then atrophy, then VEP data. 
#Question for this: why do atrophy measures show significant correlation with
#residuals from original OCT model, but when added to the model they do not 
#explain a significant larger portion of the variation in AUC?? 
#Also, better matching of VEP data could increase the number of observations 
#for VEP.
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

model_data <- read.csv("final_vision_data.csv")
atrophy_data <- read.csv("Brain_atrophy_data.csv")
atrophy_data <- subset(atrophy_data, select = c("patientID", "lcva_ID", 
                                                "matching.MRI.date", "BPFr", 
                                                "VentricularCSF"))
VEPdata <- read.csv("VEP_model_data.csv")

#Add missing columns to whole dataset
model_data <- read.csv("final_vision_data.csv")

summary(model_data)
model_data$RNFL_OU_Inferior <- (model_data$RNFL_OS_Inferior + model_data$RNFL_OD_Inferior)/2
model_data$RNFL_OU_Superior <- (model_data$RNFL_OS_Superior + model_data$RNFL_OD_Superior)/2
model_data$RNFL_OU_Nasal <- (model_data$RNFL_OS_Nasal + model_data$RNFL_OD_Nasal)/2
model_data$RNFL_OU_Temporal <- (model_data$RNFL_OS_Temporal + model_data$RNFL_OD_Temporal)/2
model_data$ILM_RPE_Thx_OU_S2 <- (model_data$ILM_RPE_Thx_OD_S2 + model_data$ILM_RPE_Thx_OS_S2)/2
model_data$ILM_RPE_Thx_OU_N1 <- (model_data$ILM_RPE_Thx_OD_N1 + model_data$ILM_RPE_Thx_OS_N1)/2
model_data$ILM_RPE_Thx_OU_I1 <- (model_data$ILM_RPE_Thx_OD_I1 + model_data$ILM_RPE_Thx_OS_I1)/2
model_data$ILM_RPE_Thx_OU_T1 <- (model_data$ILM_RPE_Thx_OD_T1 + model_data$ILM_RPE_Thx_OS_T1)/2
model_data$ILM_RPE_Thx_OU_N2 <- (model_data$ILM_RPE_Thx_OD_N2 + model_data$ILM_RPE_Thx_OS_N2)/2
model_data$ILM_RPE_Thx_OU_I2 <- (model_data$ILM_RPE_Thx_OD_I2 + model_data$ILM_RPE_Thx_OS_I2)/2
model_data$ILM_RPE_Thx_OU_T2 <- (model_data$ILM_RPE_Thx_OD_T2 + model_data$ILM_RPE_Thx_OS_T2)/2
model_data$ILM_RPE_Thx_OU_S1 <- (model_data$ILM_RPE_Thx_OD_S1 + model_data$ILM_RPE_Thx_OS_S1)/2
model_data$ILM_RPE_Thx_OU_Central <- (model_data$ILM_RPE_Thx_OD_Central + model_data$ILM_RPE_Thx_OS_Central)/2
summary(model_data)

summary(model_data$VA_1.25_contrast_OU)
summary(model_data$VA_2.5_contrast_OU)
summary(model_data$VA_100_contrast_OU)

naomitted <- subset(model_data, select = c("lcva_ID", "RNFL_OU_Inferior", "RNFL_OU_Nasal",
                                                "RNFL_OU_Superior","RNFL_OU_Temporal",
                                                "Age", "ILM_RPE_Thx_OU_Central", "ILM_RPE_Thx_OU_I1",
                                                "ILM_RPE_Thx_OU_I2","ILM_RPE_Thx_OU_N1",
                                                "ILM_RPE_Thx_OU_N2","ILM_RPE_Thx_OU_S1",
                                                "ILM_RPE_Thx_OU_S2","ILM_RPE_Thx_OU_T1",
                                                "ILM_RPE_Thx_OU_T2", "VA_1.25_contrast_OU",
                                                "VA_2.5_contrast_OU", "VA_100_contrast_OU", "condition2", "diagnosis")) 
summary(naomitted)
naomitted <- na.omit(naomitted)
summary(naomitted)

#calculate VA regression measures
n <- count(naomitted)[1,1]
res <- rep(NA, n)
int <- rep(NA, n)
auc <- rep(NA, n)

for (i in 1:n){
  patient <- naomitted[i,]
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
  int[i] <- r$coefficients[1,1]
  auc[i] <- 0.5*naomitted[i,"VA_100_contrast_OU"]*
    (100+(r$coefficients[1,1]/r$coefficients[2,1]))
}

summary(res)
head(res,400)
tail(res,400)
summary(int)
head(int)
summary(auc)
head(auc)
naomitted$VA_rsquared <- res
naomitted$VA_yint <- int
naomitted$VA_AUC <- auc

summary(naomitted)
summary(naomitted$VA_AUC)

elasticAUC <- subset(naomitted, select = c("lcva_ID", "RNFL_OU_Inferior", "RNFL_OU_Nasal",
                                           "RNFL_OU_Superior","RNFL_OU_Temporal",
                                           "VA_AUC","Age", "ILM_RPE_Thx_OU_Central", "ILM_RPE_Thx_OU_I1",
                                           "ILM_RPE_Thx_OU_I2","ILM_RPE_Thx_OU_N1",
                                           "ILM_RPE_Thx_OU_N2","ILM_RPE_Thx_OU_S1",
                                           "ILM_RPE_Thx_OU_S2","ILM_RPE_Thx_OU_T1",
                                           "ILM_RPE_Thx_OU_T2", "diagnosis", "condition2")) 
summary(elasticAUC)
elasticAUC <- na.omit(elasticAUC)
summary(elasticAUC)

head(elasticAUC$VA_AUC)

#data partition (create test and train groups)
set.seed(1234)
ind <- sample(2, nrow(elasticAUC), replace=T, prob = c((2/3), (1/3)))
train2 <- elasticAUC[ind==1,]
test2 <- elasticAUC[ind==2,]

summary(train2)

train <- subset(train2, select = c("RNFL_OU_Inferior", "RNFL_OU_Nasal",
                                   "RNFL_OU_Superior","RNFL_OU_Temporal",
                                   "VA_AUC","Age", "ILM_RPE_Thx_OU_Central", "ILM_RPE_Thx_OU_I1",
                                   "ILM_RPE_Thx_OU_I2","ILM_RPE_Thx_OU_N1",
                                   "ILM_RPE_Thx_OU_N2","ILM_RPE_Thx_OU_S1",
                                   "ILM_RPE_Thx_OU_S2","ILM_RPE_Thx_OU_T1",
                                   "ILM_RPE_Thx_OU_T2"))
test <- subset(test2, select = c("RNFL_OU_Inferior", "RNFL_OU_Nasal",
                                 "RNFL_OU_Superior","RNFL_OU_Temporal",
                                 "VA_AUC","Age", "ILM_RPE_Thx_OU_Central", "ILM_RPE_Thx_OU_I1",
                                 "ILM_RPE_Thx_OU_I2","ILM_RPE_Thx_OU_N1",
                                 "ILM_RPE_Thx_OU_N2","ILM_RPE_Thx_OU_S1",
                                 "ILM_RPE_Thx_OU_S2","ILM_RPE_Thx_OU_T1",
                                 "ILM_RPE_Thx_OU_T2"))

#custom control parameters
custom <- trainControl(method= "repeatedcv", 
                       number = 10,
                       repeats= 5,
                       verboseIter = T)

#lets go elastic net
set.seed(1234)
elasticnet <- train(VA_AUC ~ ., 
                    train,
                    method="glmnet",
                    tuneGrid= expand.grid(alpha=seq(0,1, length=10), 
                                          lambda = seq(10, 150, length=10)),
                    trControl=custom)
plot(elasticnet)
elasticnet
plot(elasticnet$finalModel, xvar="lambda", label=T)
plot(elasticnet$finalModel, xvar="dev", label=T)
plot(varImp(elasticnet, scale=F))

elasticnet$bestTune
best <- elasticnet$finalModel
coef(best, s = elasticnet$bestTune$lambda)

#save final model for later use
saveRDS(elasticnet, "full_elasticnet_model.rds")
fm <- readRDS("full_elasticnet_model.rds")
print(fm)

#Use model for prediction
p1 <- predict(fm, train)
sqrt(mean((train$VA_AUC-p1)^2))

train$OCT_prediction <- p1
train2$OCT_prediction <- p1

ggplot(train2, aes(x=OCT_prediction, y=VA_AUC)) +
  geom_point(aes(col=condition2)) +
  ggtitle("Training Observed vs. Predicted (OCT)") +
  stat_cor(method="spearman") +
  stat_regline_equation(label.y=4000, aes(label=..rr.label..)) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=4500) +
  geom_abline(slope=1, intercept=0)

p2 <- predict(fm, test)
sqrt(mean((test$VA_AUC-p2)^2))

test$OCT_prediction <- p2
test2$OCT_prediction <- p2

ggplot(test2, aes(x=OCT_prediction, y=VA_AUC)) +
  geom_point(aes(col=condition2)) +
  ggtitle("Test Observed vs. Predicted (OCT)") +
  stat_cor(method="spearman") +
  stat_regline_equation(label.y=4000, aes(label=..rr.label..)) +
  geom_smooth(method="lm", se=F, formula=y~x) +
  stat_regline_equation(label.y=4500, formula=y~x) +
  geom_abline(slope=1, intercept=0) 

elasticAUC <- rbind(train2, test2)

ggplot(elasticAUC, aes(x=OCT_prediction, y=VA_AUC)) +
  geom_point(aes(col=condition2), show.legend=F) +
  #ggtitle("Test Observed vs. Predicted (OCT)") +
  stat_cor(method="spearman") +
  stat_regline_equation(label.y=5000, aes(label=..rr.label..)) +
  geom_smooth(method="lm", se=F, formula=y~x, col="blue") +
  stat_regline_equation(label.y=4500, formula=y~x, col="blue") +
  geom_abline(slope=1, intercept=0) 

#now try to add atrophy data
atrophy_model <- full_join(elasticAUC, atrophy_data, by="lcva_ID")

summary(elasticAUC)
summary(atrophy_data)
summary(atrophy_model)

AUC_OCT_atrophy <- na.omit(atrophy_model)
summary(AUC_OCT_atrophy)

ggplot(AUC_OCT_atrophy, aes(x=OCT_prediction, y=BPFr)) +
  geom_point(aes(col=condition2)) +
  stat_cor(method="spearman") +
  stat_regline_equation

#data partition (create test and train groups)
set.seed(1234)
ind <- sample(2, nrow(AUC_OCT_atrophy), replace=T, prob = c((2/3), (1/3)))
train2B <- AUC_OCT_atrophy[ind==1,]
test2B <- AUC_OCT_atrophy[ind==2,]

summary(train2B)

trainB <- subset(train2B, select = c("BPFr", "VentricularCSF",
                                   "VA_AUC"
                                   ,"OCT_prediction"
                                   ))
testB <- subset(test2B, select = c("BPFr", "VentricularCSF",
                                 "VA_AUC"
                                 , "OCT_prediction"
                                 ))
#custom control parameters
custom <- trainControl(method= "repeatedcv", 
                       number = 10,
                       repeats= 5,
                       verboseIter = T)

#lets go elastic net
set.seed(747)
elasticnet <- train(VA_AUC ~ ., 
                    trainB,
                    method="glmnet",
                    tuneGrid= expand.grid(alpha=seq(0,1, length=10), 
                                          lambda = seq(10, 150, length=10)),
                    trControl=custom)
plot(elasticnet)
elasticnet
plot(elasticnet$finalModel, xvar="lambda", label=T)
plot(elasticnet$finalModel, xvar="dev", label=T)
plot(varImp(elasticnet, scale=F))

#Use model for prediction
p1 <- predict(elasticnet, trainB)
sqrt(mean((trainB$VA_AUC-p1)^2))

trainB$atrophy_prediction <- p1
train2B$atrophy_prediction <- p1

ggplot(train2B, aes(x=atrophy_prediction, y=VA_AUC)) +
  geom_point(aes(col=condition2)) +
  ggtitle("Training Observed vs. Predicted (OCT + atrophy)") +
  stat_cor(method="spearman") +
  stat_regline_equation(label.y=4000, aes(label=..rr.label..)) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=4500) +
  geom_abline(slope=1, intercept=0)

p2 <- predict(elasticnet, testB)
sqrt(mean((testB$VA_AUC-p2)^2))

testB$atrophy_prediction <- p2
test2B$atrophy_prediction <- p2

ggplot(test2B, aes(x=atrophy_prediction, y=VA_AUC)) +
  geom_point(aes(col=condition2)) +
  ggtitle("Test Observed vs. Predicted (OCT+atrophy)") +
  stat_cor(method="spearman", label.y=3500) +
  stat_regline_equation(label.y=4000, aes(label=..rr.label..)) +
  geom_smooth(method="lm", se=F, formula=y~x) +
  stat_regline_equation(label.y=4500, formula=y~x) +
  geom_abline(slope=1, intercept=0) 

AUC_OCT_atrophy <- rbind(train2B, test2B)

ggplot(AUC_OCT_atrophy, aes(x=atrophy_prediction, y=VA_AUC)) +
  geom_point(aes(col=condition2)) +
  labs(x="OCT+atrophy prediction") +
  #ggtitle("Test Observed vs. Predicted (OCT+atrophy)") +
  stat_cor(method="spearman", label.y=5500) +
  stat_regline_equation(label.y=5000, aes(label=..rr.label..)) +
  geom_smooth(method="lm", se=F, formula=y~x) +
  stat_regline_equation(label.y=4500, formula=y~x, col="blue") +
  geom_abline(slope=1, intercept=0) 

#set up VEP data
VEPdata <- read.csv("VEP_model_data.csv")

#Use PCA to add VEP index
VEPdata <- na.omit(VEPdata)
VEP_OD <- subset(VEPdata, select=c(VEP_OD_large, VEP_OD_small))
VEP_OD_pca <- prcomp(VEP_OD, scale=T)

VEP_index_OD <- (-1)*VEP_OD_pca$x[,1]

VEPdata$VEP_index_OD <- VEP_index_OD
summary(VEPdata)

#OS
VEP_OS <- subset(VEPdata, select=c(VEP_OS_large, VEP_OS_small))
VEP_OS_pca <- prcomp(VEP_OS, scale=T)

VEP_index_OS <- (-1)*VEP_OS_pca$x[,1]

VEPdata$VEP_index_OS <- VEP_index_OS
summary(VEPdata)

VEPdata$VEP_index_OU <- (VEPdata$VEP_index_OD +VEPdata$VEP_index_OS)/2

summary(model_data)
VEP_ident <- subset(model_data, select=c("lcva_ID", "VEP_ID"))

AUC_all <- full_join(AUC_OCT_atrophy, VEP_ident, by= "lcva_ID")
AUC_all <- full_join(AUC_all, VEPdata, by="VEP_ID")
summary(AUC_all)

AUC_all <- na.omit(AUC_all)
#damn only 22 people have data for all of these

#data partition (create test and train groups)
set.seed(747)
ind <- sample(2, nrow(AUC_all), replace=T, prob = c((2/3), (1/3)))
train2C <- AUC_all[ind==1,]
test2C <- AUC_all[ind==2,]

trainC <- subset(train2C, select = c("atrophy_prediction",
                                     "VA_AUC", "VEP_index_OU"
                                     ,"OCT_prediction"
                                     ))
testC <- subset(test2C, select = c("atrophy_prediction",
                                   "VA_AUC", "VEP_index_OU"
                                   ,"OCT_prediction"
                                   ))

#custom control parameters
custom <- trainControl(method= "repeatedcv", 
                       number = 10,
                       repeats= 5,
                       verboseIter = T)

#lets go elastic net
set.seed(747)
elasticnet <- train(VA_AUC ~ ., 
                    trainC,
                    method="glmnet",
                    tuneGrid= expand.grid(alpha=seq(0,1, length=10), 
                                          lambda = seq(10, 100, length=10)),
                    trControl=custom)
plot(elasticnet)
elasticnet
plot(elasticnet$finalModel, xvar="lambda", label=T)
plot(elasticnet$finalModel, xvar="dev", label=T)
plot(varImp(elasticnet, scale=F))

#Use model for prediction
p1 <- predict(elasticnet, trainC)
sqrt(mean((trainC$VA_AUC-p1)^2))

trainC$full_prediction <- p1
train2C$full_prediction <- p1

ggplot(train2C, aes(x=full_prediction, y=VA_AUC)) +
  geom_point(aes(col=condition2)) +
  ggtitle("Training Observed vs. Predicted (OCT + atrophy + VEP)") +
  stat_cor(method="spearman", label.y=3500) +
  stat_regline_equation(label.y=4000, aes(label=..rr.label..)) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=4500) +
  geom_abline(slope=1, intercept=0)

p2 <- predict(elasticnet, testC)
sqrt(mean((testC$VA_AUC-p2)^2))

testC$full_prediction <- p2
test2C$full_prediction <- p2

ggplot(test2C, aes(x=full_prediction, y=VA_AUC)) +
  geom_point(aes(col=condition2)) +
  ggtitle("Test Observed vs. Predicted (OCT + atrophy + VEP)") +
  stat_cor(method="spearman", label.y=3500) +
  stat_regline_equation(label.y=4000, aes(label=..rr.label..)) +
  geom_smooth(method="lm", se=F, formula=y~x) +
  stat_regline_equation(label.y=4500, formula=y~x) +
  geom_abline(slope=1, intercept=0) 



