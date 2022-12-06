#In this document, I reconstructed the binocular AUC model to add atrophy measures
#to the model or use them to explain the residuals in the OCT model
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

#Add missing binocular (OU) variables to the dataset
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

#set up atrophy data
atrophy_data <- read.csv("Brain_atrophy_data.csv")
summary(atrophy_data)

atrophy_data <- subset(atrophy_data, select = c("patientID", "lcva_ID", 
                                                "matching.MRI.date", "BPFr", 
                                                "VentricularCSF"))

joineddata <- full_join(model_data, atrophy_data, by="lcva_ID")
summary(joineddata)

#look at relationships in atrophy data
ggplot(joineddata, aes(x=BPFr, y=VentricularCSF))+
  geom_point(aes(col=condition2)) +
  geom_smooth(method="lm", se = F)+
  stat_cor(method="spearman")+
  stat_regline_equation(label.y=100000) +
  stat_regline_equation(aes(label=..rr.label..), label.y=110000)

ggplotly(ggplot(joineddata, aes(x=BPFr, y=VentricularCSF))+
           geom_point(aes(col=condition2)))

ggplot(joineddata) +
  geom_boxplot(aes(x=condition,fill=diagnosis, y=VentricularCSF))
ggplot(joineddata) +
  geom_boxplot(aes(x=condition, fill=condition, y=BPFr)) +
  stat_compare_means(aes(x=condition, y=BPFr), method="t.test", label.sep =":", 
                     label.x.npc = 0.25)
?stat_compare_means

#add atrophy to VA AUC model
modelvariables <- subset(joineddata, select = c("RNFL_OU_Inferior", "RNFL_OU_Nasal",
                                               "RNFL_OU_Superior","RNFL_OU_Temporal",
                                               "Age", "ILM_RPE_Thx_OU_Central", "ILM_RPE_Thx_OU_I1",
                                               "ILM_RPE_Thx_OU_I2","ILM_RPE_Thx_OU_N1",
                                               "ILM_RPE_Thx_OU_N2","ILM_RPE_Thx_OU_S1",
                                               "ILM_RPE_Thx_OU_S2","ILM_RPE_Thx_OU_T1",
                                               "ILM_RPE_Thx_OU_T2", "VA_1.25_contrast_OU",
                                               "VA_2.5_contrast_OU", "VA_100_contrast_OU", 
                                               "condition2", "diagnosis", "BPFr",
                                               "lcva_ID", "VentricularCSF"))

atrophysubset <- subset(modelvariables, select=c("lcva_ID", "BPFr", "VentricularCSF"))
summary(atrophysubset)

#this line is important for including atrophy in the model or testing it after
modelvariables <- subset(modelvariables, select = -c(BPFr,
                                                VentricularCSF))

modelvariables <- na.omit(modelvariables)
summary(modelvariables)

#calculate VA regression measures
n <- count(modelvariables)[1,1]
res <- rep(NA, n)
int <- rep(NA, n)
auc <- rep(NA, n)

for (i in 1:n){
  patient <- modelvariables[i,]
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
  auc[i] <- 0.5*modelvariables[i,"VA_100_contrast_OU"]*
    (100+(r$coefficients[1,1]/r$coefficients[2,1]))
}

summary(res)
head(res)
tail(res)
summary(int)
head(int)
summary(auc)
head(auc)
modelvariables$VA_rsquared <- res
modelvariables$VA_yint <- int
modelvariables$VA_AUC <- auc

summary(modelvariables)
summary(modelvariables$VA_AUC)

elasticAUC <- subset(modelvariables, select = c("RNFL_OU_Inferior", "RNFL_OU_Nasal",
                                           "RNFL_OU_Superior","RNFL_OU_Temporal",
                                           "VA_AUC","Age", "ILM_RPE_Thx_OU_Central", "ILM_RPE_Thx_OU_I1",
                                           "ILM_RPE_Thx_OU_I2","ILM_RPE_Thx_OU_N1",
                                           "ILM_RPE_Thx_OU_N2","ILM_RPE_Thx_OU_S1",
                                           "ILM_RPE_Thx_OU_S2","ILM_RPE_Thx_OU_T1",
                                           "ILM_RPE_Thx_OU_T2", "diagnosis", "condition2",
                                           "lcva_ID"
                                           #, "BPFr", "VentricularCSF"
                                           ))

#data partition (create test and train groups)
set.seed(747)
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
                                   "ILM_RPE_Thx_OU_T2"
                                  #, "BPFr", "VentricularCSF"
                                    ))
test <- subset(test2, select = c("RNFL_OU_Inferior", "RNFL_OU_Nasal",
                                 "RNFL_OU_Superior","RNFL_OU_Temporal",
                                 "VA_AUC","Age", "ILM_RPE_Thx_OU_Central", "ILM_RPE_Thx_OU_I1",
                                 "ILM_RPE_Thx_OU_I2","ILM_RPE_Thx_OU_N1",
                                 "ILM_RPE_Thx_OU_N2","ILM_RPE_Thx_OU_S1",
                                 "ILM_RPE_Thx_OU_S2","ILM_RPE_Thx_OU_T1",
                                 "ILM_RPE_Thx_OU_T2"
                                  #, "BPFr", "VentricularCSF"
                                  ))

#custom control parameters
custom <- trainControl(method= "repeatedcv", 
                       number = 10,
                       repeats= 5,
                       verboseIter = T)
#lets go elastic net
set.seed(747)
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

train$prediction <- p1
train2$prediction <- p1

ggplot(train2, aes(x=prediction, y=VA_AUC)) +
  geom_point(aes(col=condition2)) +
  ggtitle("Training Observed vs. Predicted (binocular w/ atrophy)") +
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
  ggtitle("Test Observed vs. Predicted (binocular w/ atrophy)") +
  stat_cor(method="spearman", label.y=4250) +
  stat_regline_equation(label.y=4000, aes(label=..rr.label..)) +
  geom_smooth(method="lm", se=F, formula=y~x) +
  stat_regline_equation(label.y=4500, formula=y~x) +
  geom_abline(slope=1, intercept=0)

#finding relationship with residuals when VEP is not in model
#training data
r <- lm(prediction~VA_AUC, data=train2)
summary(r)
train2$AUC_model_residuals <- r$residuals

atrophymodel <- full_join(train2, atrophysubset, by="lcva_ID")
summary(atrophymodel)

ggplot(aes(x=AUC_model_residuals, y=BPFr), data=atrophymodel) +
  geom_point(aes(col=condition2)) +
  stat_cor(method="spearman", label.y=.5, label.x=1.5) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=.4, col="blue",label.x=1.5) +
  stat_regline_equation(aes(label=..rr.label..), label.y=.45,label.x=1.5)+
  ggtitle("BPFr vs. residuals (binocular training data)")

ggplot(aes(x=AUC_model_residuals, y=VentricularCSF), data=atrophymodel) +
  geom_point(aes(col=condition2)) +
  stat_cor(method="spearman", label.y=125000, label.x=1.5) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=100000, col="blue",label.x=1.5) +
  stat_regline_equation(aes(label=..rr.label..), label.y=112500,label.x=1.5)+
  ggtitle("Ventricular CSF vs. residuals (binocular training data)")

#testing data
q <- lm(prediction~VA_AUC, data=test2)
summary(q)
test2$AUC_model_residuals <- q$residuals

atrophymodel <- full_join(test2, atrophysubset, by="lcva_ID")
summary(atrophymodel)

ggplot(aes(x=AUC_model_residuals, y=BPFr), data=atrophymodel) +
  geom_point(aes(col=condition2)) +
  stat_cor(method="spearman", label.y=.5, label.x=1.5) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=.4, col="blue",label.x=1.5) +
  stat_regline_equation(aes(label=..rr.label..), label.y=.45,label.x=1.5)+
  ggtitle("BPFr vs. residuals (binocular test data)")

ggplot(aes(x=AUC_model_residuals, y=VentricularCSF), data=atrophymodel) +
  geom_point(aes(col=condition2)) +
  stat_cor(method="spearman", label.y=125000, label.x=1.5) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=100000, col="blue",label.x=1.5) +
  stat_regline_equation(aes(label=..rr.label..), label.y=112500,label.x=1.5)+
  ggtitle("Ventricular CSF vs. residuals (binocular test data)")


#for reference
ggplot(atrophymodel, aes(x=BPFr, y=VA_AUC)) +
  geom_point(aes(col=condition2)) +
  geom_smooth(method="lm") +
  stat_cor(method="spearman")

ggplot(atrophymodel, aes(x=VentricularCSF, y=VA_AUC)) +
  geom_point(aes(col=condition2)) +
  geom_smooth(method="lm") +
  stat_cor(method="spearman")
