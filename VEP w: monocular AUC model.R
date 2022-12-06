#In this document, I reconstructed the monocular AUC model to add VEP to the 
#model or use it to explain the residuals of the original OCT model.
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
VEPdata <- read.csv("VEP_model_data.csv")

#Use PCA to add VEP index
VEPdata <- na.omit(VEPdata)
VEP_OD <- subset(VEPdata, select=c(VEP_OD_large, VEP_OD_small))
VEP_OD_pca <- prcomp(VEP_OD, scale=T)
plot(VEP_OD_pca$x[,1], VEP_OD_pca$x[,2])

pca_var_VEP_OD <- VEP_OD_pca$sdev^2
pca_var_per_VEP_OD <- round(pca_var_VEP_OD/sum(pca_var_VEP_OD)*100, 1)
barplot(pca_var_per_VEP_OD, main= "Scree Plot", xlab = "Principal Component", ylab= "Percent of Variation")

VEP_OD_pca_plotdata <- data.frame(sample=rownames(VEP_OD_pca$x), x=VEP_OD_pca$x[,1], y=VEP_OD_pca$x[,2])
summary(VEP_OD_pca_plotdata)

ggplot(data=VEP_OD_pca_plotdata, aes(x=x, y=y, label=sample)) + geom_point() + 
  xlab(paste("PC1 - ", pca_var_per_VEP_OD[1], "%", sep="")) + 
  ylab(paste("PC2 - ", pca_var_per_VEP_OD[2], "%", sep="")) +
  theme_bw() + labs(title="VEP OD PCA")

VEP_index_OD <- (-1)*VEP_OD_pca$x[,1]

VEPdata$VEP_index_OD <- VEP_index_OD
summary(VEPdata)

#OS
VEP_OS <- subset(VEPdata, select=c(VEP_OS_large, VEP_OS_small))
VEP_OS_pca <- prcomp(VEP_OS, scale=T)
plot(VEP_OS_pca$x[,1], VEP_OS_pca$x[,2])

pca_var_VEP_OS <- VEP_OS_pca$sdev^2
pca_var_per_VEP_OS <- round(pca_var_VEP_OS/sum(pca_var_VEP_OS)*100, 1)
barplot(pca_var_per_VEP_OS, main= "Scree Plot", xlab = "Principal Component", ylab= "Percent of Variation")

VEP_OS_pca_plotdata <- data.frame(sample=rownames(VEP_OS_pca$x), x=VEP_OS_pca$x[,1], y=VEP_OD_pca$x[,2])
summary(VEP_OS_pca_plotdata)
ggplot(data=VEP_OS_pca_plotdata, aes(x=x, y=y, label=sample)) + geom_point() + 
  xlab(paste("PC1 - ", pca_var_per_VEP_OS[1], "%", sep="")) + 
  ylab(paste("PC2 - ", pca_var_per_VEP_OS[2], "%", sep="")) +
  theme_bw() + labs(title="VEP OS PCA")

VEP_index_OS <- (-1)*VEP_OS_pca$x[,1]

VEPdata$VEP_index_OS <- VEP_index_OS
summary(VEPdata)

full_data <- full_join(model_data, VEPdata, by= "VEP_ID")
summary(full_data)

full_data$VEP_index_OU <- (full_data$VEP_index_OD +full_data$VEP_index_OS)/2

#set up data set
elasticAUC_OD <- subset(full_data, select = c("lcva_ID", "RNFL_OD_Inferior", "RNFL_OD_Nasal",
                                               "RNFL_OD_Superior","RNFL_OD_Temporal",
                                               "Age", "ILM_RPE_Thx_OD_Central", "ILM_RPE_Thx_OD_I1",
                                               "ILM_RPE_Thx_OD_I2","ILM_RPE_Thx_OD_N1",
                                               "ILM_RPE_Thx_OD_N2","ILM_RPE_Thx_OD_S1",
                                               "ILM_RPE_Thx_OD_S2","ILM_RPE_Thx_OD_T1",
                                               "ILM_RPE_Thx_OD_T2", "VA_1.25_contrast_OD",
                                               "VA_2.5_contrast_OD", "VA_100_contrast_OD", "condition2", "diagnosis",
                                               "VEP_ID", "VEP_index_OD"))

elasticAUC_OS <- subset(full_data, select = c("lcva_ID", "RNFL_OS_Inferior", "RNFL_OS_Nasal",
                                               "RNFL_OS_Superior","RNFL_OS_Temporal",
                                               "Age", "ILM_RPE_Thx_OS_Central", "ILM_RPE_Thx_OS_I1",
                                               "ILM_RPE_Thx_OS_I2","ILM_RPE_Thx_OS_N1",
                                               "ILM_RPE_Thx_OS_N2","ILM_RPE_Thx_OS_S1",
                                               "ILM_RPE_Thx_OS_S2","ILM_RPE_Thx_OS_T1",
                                               "ILM_RPE_Thx_OS_T2", "VA_1.25_contrast_OS",
                                               "VA_2.5_contrast_OS", "VA_100_contrast_OS", "condition2", "diagnosis", 
                                               "VEP_ID", "VEP_index_OS"))

summary(elasticAUC_OS$lcva_ID) 
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
                    VA_100_contrast = VA_100_contrast_OD, VEP_index = VEP_index_OD)

merger_OS <- rename(elasticAUC_OS, RNFL_Inferior = RNFL_OS_Inferior, RNFL_Nasal= RNFL_OS_Nasal,
                    RNFL_Superior= RNFL_OS_Superior, RNFL_Temporal= RNFL_OS_Temporal,
                    ILM_RPE_Thx_Central = ILM_RPE_Thx_OS_Central, ILM_RPE_Thx_I1= ILM_RPE_Thx_OS_I1,
                    ILM_RPE_Thx_I2= ILM_RPE_Thx_OS_I2, ILM_RPE_Thx_N1= ILM_RPE_Thx_OS_N1,
                    ILM_RPE_Thx_N2= ILM_RPE_Thx_OS_N2, ILM_RPE_Thx_S1= ILM_RPE_Thx_OS_S1,
                    ILM_RPE_Thx_S2= ILM_RPE_Thx_OS_S2, ILM_RPE_Thx_T1= ILM_RPE_Thx_OS_T1,
                    ILM_RPE_Thx_T2= ILM_RPE_Thx_OS_T2,
                    VA_1.25_contrast = VA_1.25_contrast_OS, VA_2.5_contrast = VA_2.5_contrast_OS,
                    VA_100_contrast = VA_100_contrast_OS,VEP_index = VEP_index_OS)
summary(merger_OD)
summary(merger_OS)

?full_join()
#merger <- full_join(merger_OD, merger_OS, by = "lcva_ID")
summary(merger)
tail(merger$diagnosis.x)
#full join doesn't work

merger <- rbind(merger_OD, merger_OS)
summary(merger)
summary(merger_OD)
summary(merger_OS)

VEPsubset <- subset(merger, select=c("lcva_ID", "VEP_ID", "VEP_index"))
summary(VEPsubset)

#this line is important for including VEP in the model or testing it after
monocular_data <- subset(merger, select = -c(VEP_ID,
                                             VEP_index))
summary(monocular_data)
monocular_data <- na.omit(monocular_data)

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
                                           "ILM_RPE_Thx_T2")) 
                                           #"VEP_index"))

monotest <- subset(monotest2, select = c("RNFL_Inferior", "RNFL_Nasal",
                                         "RNFL_Superior","RNFL_Temporal",
                                         "VA_AUC","Age", "ILM_RPE_Thx_Central", "ILM_RPE_Thx_I1",
                                         "ILM_RPE_Thx_I2","ILM_RPE_Thx_N1",
                                         "ILM_RPE_Thx_N2","ILM_RPE_Thx_S1",
                                         "ILM_RPE_Thx_S2","ILM_RPE_Thx_T1",
                                         "ILM_RPE_Thx_T2")) 
                                         #"VEP_index"))

summary(monotrain)
str(monotrain)
str(monotest)

#custom control parameters
custom <- trainControl(method= "repeatedcv", 
                       number = 10,
                       repeats= 5,
                       verboseIter = T)

#elastic net baby (monocular)
set.seed(747)
mono_en <- train(VA_AUC ~ ., 
                 monotrain,
                 method="glmnet",
                 tuneGrid= expand.grid(alpha=seq(0,1, length=10), 
                                       lambda = seq(50, 150, length=10)),
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

VEPmodel <- full_join(monotrain2, VEPsubset, by="lcva_ID")
summary(VEPmodel)

VEPmodel2 <- rbind(monotrain2, monotest2)

ggplot(aes(x=AUC_model_residuals, y=VEP_index), data=VEPmodel) +
  geom_point(aes(col=condition2)) +
  stat_cor(method="spearman", label.y=5, label.x=1.5) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=4, col="blue",label.x=1.5) +
  stat_regline_equation(aes(label=..rr.label..), label.y=4.5,label.x=1.5)+
  ggtitle("VEP vs. Residuals (monocular training data)")

#testing data
q <- lm(prediction~VA_AUC, data=monotest2)
summary(q)
monotest2$AUC_model_residuals <- q$residuals

VEPmodel <- full_join(monotest2, VEPsubset, by="lcva_ID")
summary(VEPmodel)

VEPmodel2 <- rbind(monotrain2, monotest2)
VEPmodel2 <- full_join(VEPmodel2, VEPsubset, by="lcva_ID")


ggplot(aes(x=AUC_model_residuals, y=VEP_index), data=VEPmodel) +
  geom_point(aes(col=condition2)) +
  stat_cor(method="spearman", label.y=5, label.x=1.5) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=4, col="blue",label.x=1.5) +
  stat_regline_equation(aes(label=..rr.label..), label.y=4.5,label.x=1.5)+
  ggtitle("VEP vs. Residuals (monocular test data)")

ggplot(aes(x=AUC_model_residuals, y=VEP_index), data=VEPmodel2) +
  geom_point(aes(col=condition2)) +
  stat_cor(method="spearman", label.y=5, label.x=1.5) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=4, col="blue",label.x=1.5) +
  stat_regline_equation(aes(label=..rr.label..), label.y=4.5,label.x=1.5)+
  coord_cartesian(xlim=c(-600, 600))
  #ggtitle("VEP vs. Residuals (monocular test data)")

#for context
ggplot(VEPmodel, aes(x=VEP_index, y=VA_AUC)) +
  geom_point(aes(col=diagnosis)) +
  stat_cor(method="spearman") +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(aes(label=..rr.label..))
             