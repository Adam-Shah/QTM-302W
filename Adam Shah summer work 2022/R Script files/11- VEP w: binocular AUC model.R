#In this document, I reconstructed the binocular elastic net model for AUC so 
#that I could add the VEP index as a part of the model or use it to explain the 
#residuals of the original OCT model
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

#set up VEP data
VEPdata <- read.csv("VEP_model_data.csv")

#Use PCA to add VEP index
#OD
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

full_data <- full_join(model_data, VEPdata, by= "VEP_ID")
summary(full_data)

full_data$VEP_index_OU <- (full_data$VEP_index_OD +full_data$VEP_index_OS)/2


summary(full_data)

#set up full data set
modelvariables <- subset(full_data, select = c("RNFL_OU_Inferior", "RNFL_OU_Nasal",
                                                "RNFL_OU_Superior","RNFL_OU_Temporal",
                                                "Age", "ILM_RPE_Thx_OU_Central", "ILM_RPE_Thx_OU_I1",
                                                "ILM_RPE_Thx_OU_I2","ILM_RPE_Thx_OU_N1",
                                                "ILM_RPE_Thx_OU_N2","ILM_RPE_Thx_OU_S1",
                                                "ILM_RPE_Thx_OU_S2","ILM_RPE_Thx_OU_T1",
                                                "ILM_RPE_Thx_OU_T2", "VA_1.25_contrast_OU",
                                                "VA_2.5_contrast_OU", "VA_100_contrast_OU", 
                                                "condition2", "diagnosis", "VEP_index_OU",
                                                "lcva_ID", "VEP_ID")) 
summary(modelvariables)

VEPsubset <- subset(modelvariables, select=c("lcva_ID", "VEP_ID", "VEP_index_OU"))
summary(VEPsubset)

#this line is important for including VEP in the model or testing it after
naomitted <- subset(modelvariables, select = -c(VEP_ID,
                                             VEP_index_OU))
naomitted <- na.omit(naomitted)
summary(naomitted)

n <- count(naomitted)[1,1]
res <- rep(NA, n)
int <- rep(NA, n)
auc <- rep(NA, n)

#calculate measures from VA regression
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
  res[i] <- r$coefficients[2,1]
  int[i] <- r$coefficients[1,1]
  auc[i] <- 0.5*naomitted[i,"VA_100_contrast_OU"]*
    (100+(r$coefficients[1,1]/r$coefficients[2,1]))
  
}

summary(res)
head(res)
tail(res)
summary(int)
head(int)
summary(auc)
head(auc)
naomitted$VA_slope <- res
naomitted$VA_yint <- int
naomitted$VA_AUC <- auc

summary(naomitted)
summary(naomitted$VA_AUC)

#ignore this, I was validating VA measures with Bibi
#ggplotly(ggplot(naomitted, aes(y=VA_yint, x=VA_slope)) +
#           geom_point(aes(col=condition2)))

#healthy <- subset(naomitted, condition2 == "Healthy")
#median(healthy$VA_slope)

#ggplot(naomitted, aes(x=Age, y=VA_slope)) + geom_point() +
#  stat_cor(method="spearman")


#Use this step to either include or not include VEP in the model
elasticAUC <- subset(naomitted, select = c("RNFL_OU_Inferior", "RNFL_OU_Nasal",
                                           "RNFL_OU_Superior","RNFL_OU_Temporal",
                                           "VA_AUC","Age", "ILM_RPE_Thx_OU_Central", "ILM_RPE_Thx_OU_I1",
                                           "ILM_RPE_Thx_OU_I2","ILM_RPE_Thx_OU_N1",
                                           "ILM_RPE_Thx_OU_N2","ILM_RPE_Thx_OU_S1",
                                           "ILM_RPE_Thx_OU_S2","ILM_RPE_Thx_OU_T1",
                                           "ILM_RPE_Thx_OU_T2", "diagnosis", "condition2",
                                           "lcva_ID"))
                                           #"VEP_index_OU")) 

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
                                   #"VEP_index_OU"))
test <- subset(test2, select = c("RNFL_OU_Inferior", "RNFL_OU_Nasal",
                                 "RNFL_OU_Superior","RNFL_OU_Temporal",
                                 "VA_AUC","Age", "ILM_RPE_Thx_OU_Central", "ILM_RPE_Thx_OU_I1",
                                 "ILM_RPE_Thx_OU_I2","ILM_RPE_Thx_OU_N1",
                                 "ILM_RPE_Thx_OU_N2","ILM_RPE_Thx_OU_S1",
                                 "ILM_RPE_Thx_OU_S2","ILM_RPE_Thx_OU_T1",
                                 "ILM_RPE_Thx_OU_T2"))
                                #"VEP_index_OU"))
#custom control parameters
custom <- trainControl(method= "repeatedcv", 
                       number = 10,
                       repeats= 5,
                       verboseIter = T)
#lets do elastic net
set.seed(1234)
elasticnet <- train(VA_AUC ~ ., 
                    train,
                    method="glmnet",
                    tuneGrid= expand.grid(alpha=seq(0,1, length=10), 
                                          lambda = seq(10, 100, length=10)),
                    trControl=custom)
plot(elasticnet)
elasticnet
plot(elasticnet$finalModel, xvar="lambda", label=T)
plot(elasticnet$finalModel, xvar="dev", label=T)
plot(varImp(elasticnet, scale=F))

#best coefficients
elasticnet$bestTune
best <- elasticnet$finalModel
coef(best, s = elasticnet$bestTune$lambda)

#save final model for later use
saveRDS(elasticnet, "full_elasticnet_model.rds")
fm <- readRDS("full_elasticnet_model.rds")
print(fm)

#Use model for prediction and validate
p1 <- predict(fm, train)
sqrt(mean((train$VA_AUC-p1)^2))

train$prediction <- p1
train2$prediction <- p1

ggplot(train2, aes(x=prediction, y=VA_AUC)) +
  geom_point(aes(col=condition2)) +
  ggtitle("Training Observed vs. Predicted (EN)") +
  stat_cor(method="spearman", label.y=4250) +
  stat_regline_equation(label.y=4000, aes(label=..rr.label..)) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=4500, col="blue") +
  geom_abline(slope=1, intercept=0)

p2 <- predict(fm, test)
sqrt(mean((test$VA_AUC-p2)^2))

test$prediction <- p2
test2$prediction <- p2

ggplot(test2, aes(x=prediction, y=VA_AUC)) +
  geom_point(aes(col=condition2)) +
  ggtitle("Test Observed vs. Predicted (EN)") +
  stat_cor(method="spearman", label.y=4250) +
  stat_regline_equation(label.y=4000, aes(label=..rr.label..)) +
  geom_smooth(method="lm", se=F, formula=y~x) +
  stat_regline_equation(label.y=4500, formula=y~x) +
  geom_abline(slope=1, intercept=0) #+
#coord_cartesian(xlim=c(0,4000), ylim=c(0,4000))


#relationship vs residuals when VEP is not included in model
#training data
r <- lm(prediction~VA_AUC, data=train2)
summary(r)
train2$AUC_model_residuals <- r$residuals

VEPmodel <- full_join(train2, VEPsubset, by="lcva_ID")
summary(VEPmodel)

ggplot(aes(x=AUC_model_residuals, y=VEP_index_OU), data=VEPmodel) +
  geom_point(aes(col=condition2)) +
  stat_cor(method="spearman", label.y=5, label.x=1.5) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=4, col="blue",label.x=1.5) +
  stat_regline_equation(aes(label=..rr.label..), label.y=4.5,label.x=1.5)+
  ggtitle("VEP vs. residuals (binocular training data)")

#testing data
q <- lm(prediction~VA_AUC, data=test2)
summary(q)
test2$AUC_model_residuals <- q$residuals

VEPmodel <- full_join(test2, VEPsubset, by="lcva_ID")
summary(VEPmodel)

ggplot(aes(x=AUC_model_residuals, y=VEP_index_OU), data=VEPmodel) +
  geom_point(aes(col=condition2)) +
  stat_cor(method="spearman", label.y=5, label.x=1.5) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=4, col="blue",label.x=1.5) +
  stat_regline_equation(aes(label=..rr.label..), label.y=4.5,label.x=1.5)+
  ggtitle("VEP vs. residuals (binocular test data)")
