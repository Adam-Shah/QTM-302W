#In this document, I reconstructed the monocular AUC model to add atrophy measures
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

#set up atrophy data
atrophy_data <- read.csv("Brain_atrophy_data.csv")
summary(atrophy_data)

atrophy_data <- subset(atrophy_data, select = c("patientID", "lcva_ID", 
                                                "matching.MRI.date", "BPFr", 
                                                "VentricularCSF"))

full_data <- full_join(model_data, atrophy_data, by= "lcva_ID")
summary(full_data)

#set up data set
elasticAUC_OD <- subset(full_data, select = c("lcva_ID", "RNFL_OD_Inferior", "RNFL_OD_Nasal",
                                              "RNFL_OD_Superior","RNFL_OD_Temporal",
                                              "Age", "ILM_RPE_Thx_OD_Central", "ILM_RPE_Thx_OD_I1",
                                              "ILM_RPE_Thx_OD_I2","ILM_RPE_Thx_OD_N1",
                                              "ILM_RPE_Thx_OD_N2","ILM_RPE_Thx_OD_S1",
                                              "ILM_RPE_Thx_OD_S2","ILM_RPE_Thx_OD_T1",
                                              "ILM_RPE_Thx_OD_T2", "VA_1.25_contrast_OD",
                                              "VA_2.5_contrast_OD", "VA_100_contrast_OD", "condition2", "diagnosis",
                                              "BPFr", "VentricularCSF"))

elasticAUC_OS <- subset(full_data, select = c("lcva_ID", "RNFL_OS_Inferior", "RNFL_OS_Nasal",
                                              "RNFL_OS_Superior","RNFL_OS_Temporal",
                                              "Age", "ILM_RPE_Thx_OS_Central", "ILM_RPE_Thx_OS_I1",
                                              "ILM_RPE_Thx_OS_I2","ILM_RPE_Thx_OS_N1",
                                              "ILM_RPE_Thx_OS_N2","ILM_RPE_Thx_OS_S1",
                                              "ILM_RPE_Thx_OS_S2","ILM_RPE_Thx_OS_T1",
                                              "ILM_RPE_Thx_OS_T2", "VA_1.25_contrast_OS",
                                              "VA_2.5_contrast_OS", "VA_100_contrast_OS", "condition2", "diagnosis", 
                                              "BPFr", "VentricularCSF"))

summary(elasticAUC_OS$lcva_ID) 

#negate lcva ID for left eye to track all data entries in monocular dataset
elasticAUC_OS$lcva_ID <- -1*elasticAUC_OS$lcva_ID
summary(elasticAUC_OS$lcva_ID)

merger_OD <- rename(elasticAUC_OD, RNFL_Inferior = RNFL_OD_Inferior, RNFL_Nasal= RNFL_OD_Nasal,
                    RNFL_Superior= RNFL_OD_Superior, RNFL_Temporal= RNFL_OD_Temporal,
                    ILM_RPE_Thx_Central = ILM_RPE_Thx_OD_Central, ILM_RPE_Thx_I1= ILM_RPE_Thx_OD_I1,
                    ILM_RPE_Thx_I2= ILM_RPE_Thx_OD_I2, ILM_RPE_Thx_N1= ILM_RPE_Thx_OD_N1,
                    ILM_RPE_Thx_N2= ILM_RPE_Thx_OD_N2, ILM_RPE_Thx_S1= ILM_RPE_Thx_OD_S1,
                    ILM_RPE_Thx_S2= ILM_RPE_Thx_OD_S2, ILM_RPE_Thx_T1= ILM_RPE_Thx_OD_T1,
                    ILM_RPE_Thx_T2= ILM_RPE_Thx_OD_T2,
                    VA_1.25_contrast = VA_1.25_contrast_OD, VA_2.5_contrast = VA_2.5_contrast_OD,
                    VA_100_contrast = VA_100_contrast_OD)

merger_OS <- rename(elasticAUC_OS, RNFL_Inferior = RNFL_OS_Inferior, RNFL_Nasal= RNFL_OS_Nasal,
                    RNFL_Superior= RNFL_OS_Superior, RNFL_Temporal= RNFL_OS_Temporal,
                    ILM_RPE_Thx_Central = ILM_RPE_Thx_OS_Central, ILM_RPE_Thx_I1= ILM_RPE_Thx_OS_I1,
                    ILM_RPE_Thx_I2= ILM_RPE_Thx_OS_I2, ILM_RPE_Thx_N1= ILM_RPE_Thx_OS_N1,
                    ILM_RPE_Thx_N2= ILM_RPE_Thx_OS_N2, ILM_RPE_Thx_S1= ILM_RPE_Thx_OS_S1,
                    ILM_RPE_Thx_S2= ILM_RPE_Thx_OS_S2, ILM_RPE_Thx_T1= ILM_RPE_Thx_OS_T1,
                    ILM_RPE_Thx_T2= ILM_RPE_Thx_OS_T2,
                    VA_1.25_contrast = VA_1.25_contrast_OS, VA_2.5_contrast = VA_2.5_contrast_OS,
                    VA_100_contrast = VA_100_contrast_OS)
summary(merger_OD)
summary(merger_OS)

merger<- rbind(merger_OD, merger_OS)
summary(merger)
summary(merger_OD)
summary(merger_OS)

atrophysubset <- subset(merger, select=c("lcva_ID", "BPFr", "VentricularCSF"))
summary(atrophysubset)

#this line is important for including atrophy in the model or testing it after
#merger <- subset(merger, select = -c(BPFr,
#                                    VentricularCSF))

monocular_data <- na.omit(merger)

n <- count(monocular_data)[1,1]
res <- rep(NA, n)
int <- rep(NA, n)
auc <- rep(NA, n)

for (i in 1:n){
  patient <- monocular_data[i,]
  VA_subset <- subset(patient, select = c("VA_1.25_contrast", 
                                          "VA_2.5_contrast", 
                                          "VA_100_contrast",
                                          "condition2"))
  VA_subset2 <- rename(VA_subset, "33.33333" = VA_1.25_contrast, 
                       "52.63158" = VA_2.5_contrast, 
                       "100"=VA_100_contrast)
  VA_subsetlong2 <- melt(VA_subset2, id="condition2")
  VA_subsetlong2 <- rename(VA_subsetlong2, scaled_contrast = variable, score=value)
  VA_subsetlong2$scaled_contrast <- as.numeric(as.character(VA_subsetlong2$scaled_contrast))
  
  r <- summary(lm(score~scaled_contrast, VA_subsetlong2))
  res[i] <- r$r.squared
  int[i] <- r$coefficients[1,1]
  auc[i] <- 0.5*monocular_data[i,"VA_100_contrast"]*
    (100+(r$coefficients[1,1]/r$coefficients[2,1]))
}
summary(res)
bwplot(res)
summary(int)
bwplot(int)
summary(auc)
head(auc)
bwplot(auc)
monocular_data$VA_rsquared <- res
monocular_data$VA_yint <- int
monocular_data$VA_AUC <- auc

summary(monocular_data) #why 6 NAs in rsquared and AUC?
monocular_data <- na.omit(monocular_data)

#Elastic Net
#data partition
set.seed(747)
ind <- sample(2, nrow(monocular_data), replace=T, prob = c((2/3), (1/3)))
monotrain2 <- monocular_data[ind==1,]
monotest2 <- monocular_data[ind==2,]

monotrain <- subset(monotrain2, select = c("RNFL_Inferior", "RNFL_Nasal",
                                           "RNFL_Superior","RNFL_Temporal",
                                           "VA_AUC","Age", "ILM_RPE_Thx_Central", "ILM_RPE_Thx_I1",
                                           "ILM_RPE_Thx_I2","ILM_RPE_Thx_N1",
                                           "ILM_RPE_Thx_N2","ILM_RPE_Thx_S1",
                                           "ILM_RPE_Thx_S2","ILM_RPE_Thx_T1",
                                           "ILM_RPE_Thx_T2" 
                                          ,"BPFr", "VentricularCSF"
                                          ))

monotest <- subset(monotest2, select = c("RNFL_Inferior", "RNFL_Nasal",
                                         "RNFL_Superior","RNFL_Temporal",
                                         "VA_AUC","Age", "ILM_RPE_Thx_Central", "ILM_RPE_Thx_I1",
                                         "ILM_RPE_Thx_I2","ILM_RPE_Thx_N1",
                                         "ILM_RPE_Thx_N2","ILM_RPE_Thx_S1",
                                         "ILM_RPE_Thx_S2","ILM_RPE_Thx_T1",
                                         "ILM_RPE_Thx_T2"
                                         , "BPFr", "VentricularCSF"
                                         )) 

summary(monotrain)
str(monotrain)
str(monotest)

#custom control parameters
custom <- trainControl(method= "repeatedcv", 
                       number = 10,
                       repeats= 5,
                       verboseIter = T)

#elastic net (monocular)
set.seed(747)
mono_en <- train(VA_AUC ~ ., 
                 monotrain,
                 method="glmnet",
                 tuneGrid= expand.grid(alpha=seq(0,1, length=10), 
                                       lambda = seq(50, 200, length=10)),
                 trControl=custom)
plot(mono_en)
mono_en
plot(mono_en$finalModel, xvar="lambda", label=T)
plot(mono_en$finalModel, xvar="dev", label=T)
plot(varImp(mono_en, scale=F))

#best fit model parameters and coefficients
mono_en$bestTune
best <- mono_en$finalModel
coef(best, s = mono_en$bestTune$lambda)

#save model
saveRDS(mono_en, "monocular_elasticnet_model.rds")
mono <- readRDS("monocular_elasticnet_model.rds")
print(mono)

#prediction
p1 <- predict(mono, monotrain)
sqrt(mean((monotrain$VA_AUC-p1)^2))

#monotrain$prediction <- p1
monotrain2$prediction <- p1

p2 <- predict(mono, monotest)
sqrt(mean((monotest$VA_AUC-p2)^2))

#monotest$prediction <- p2
monotest2$prediction <- p2

ggplot(monotrain2, aes(x=prediction, y=VA_AUC)) +
  geom_point(aes(col=condition2)) +
  ggtitle("Training Observed vs. Predicted (monocular)") +
  stat_cor(method="spearman", label.y=4250) +
  stat_regline_equation(label.y=4000, aes(label=..rr.label..)) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=4500, col="blue") +
  geom_abline(slope=1, intercept=0)

ggplot(monotest2, aes(x=prediction, y=VA_AUC)) +
  geom_point(aes(col=condition2)) +
  ggtitle("Test Observed vs. Predicted (monocular)") +
  stat_cor(method="spearman", label.y=4500) +
  stat_regline_equation(label.y=5000, aes(label=..rr.label..), 
                        formula=y~x) +
  geom_smooth(method="lm", se=F, formula=y~x) +
  stat_regline_equation(label.y=4000, col= "blue", formula=y~x) +
  geom_abline(slope=1, intercept=0)

#finding residuals when VEP is not in model
#training data
r <- lm(prediction~VA_AUC, data=monotrain2)
summary(r)
monotrain2$AUC_model_residuals <- r$residuals

atrophymodel <- full_join(monotrain2, atrophysubset, by="lcva_ID")
summary(atrophymodel)

ggplot(aes(x=AUC_model_residuals, y=BPFr), data=atrophymodel) +
  geom_point(aes(col=condition2)) +
  stat_cor(method="spearman", label.y=.5, label.x=1.5) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=.4, col="blue",label.x=1.5) +
  stat_regline_equation(aes(label=..rr.label..), label.y=.45,label.x=1.5)+
  ggtitle("BPFr vs. residuals (monocular training data)")

ggplot(aes(x=AUC_model_residuals, y=VentricularCSF), data=atrophymodel) +
  geom_point(aes(col=condition2)) +
  stat_cor(method="spearman", label.y=125000, label.x=1.5) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=100000, col="blue",label.x=1.5) +
  stat_regline_equation(aes(label=..rr.label..), label.y=112500,label.x=1.5)+
  ggtitle("Ventricular CSF vs. residuals (monocular training data)")

#testing data
q <- lm(prediction~VA_AUC, data=monotest2)
summary(q)
monotest2$AUC_model_residuals <- q$residuals

atrophymodel <- full_join(monotest2, atrophysubset, by="lcva_ID")
summary(atrophymodel)

ggplot(aes(x=AUC_model_residuals, y=BPFr), data=atrophymodel) +
  geom_point(aes(col=condition2)) +
  stat_cor(method="spearman", label.y=.5, label.x=1.5) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=.4, col="blue",label.x=1.5) +
  stat_regline_equation(aes(label=..rr.label..), label.y=.45,label.x=1.5)+
  ggtitle("BPFr vs. residuals (monocular test data)")

ggplot(aes(x=AUC_model_residuals, y=VentricularCSF), data=atrophymodel) +
  geom_point(aes(col=condition2)) +
  stat_cor(method="spearman", label.y=125000, label.x=1.5) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=100000, col="blue",label.x=1.5) +
  stat_regline_equation(aes(label=..rr.label..), label.y=112500,label.x=1.5)+
  ggtitle("Ventricular CSF vs. residuals (monocular test data)")
