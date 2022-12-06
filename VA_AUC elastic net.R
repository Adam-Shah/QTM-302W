#In this document, I experimented with different techniques to model the new 
#VA AUC measure- linear, lasso, ridge, and elastic net regression. 
#APS

library(ggplot2)
library(dplyr)
library(plotly)
#install.packages("glmnet")
library(glmnet)
#install.packages("mlbench")
library(mlbench)
library(ggpubr)
library(corrplot)
library(Hmisc)
library(reshape2)

install.packages("ipred")
install.packages("caret")
library(caret)

#lets try some elastic net regression to predict VA AUC in normal-eyed people
summary(normeyes_new)
summary(normeyes_new$VA_rsquared)
summary(normeyes_new$VA_yint)

elasticAUC <- subset(normeyes_new, select = c("RNFL_OU_Inferior", "RNFL_OU_Nasal",
                                              "RNFL_OU_Superior","RNFL_OU_Temporal",
                                              "VA_AUC","Age", "ILM_RPE_Thx_OU_Central", "ILM_RPE_Thx_OU_I1",
                                              "ILM_RPE_Thx_OU_I2","ILM_RPE_Thx_OU_N1",
                                              "ILM_RPE_Thx_OU_N2","ILM_RPE_Thx_OU_S1",
                                              "ILM_RPE_Thx_OU_S2","ILM_RPE_Thx_OU_T1",
                                              "ILM_RPE_Thx_OU_T2")) 
summary(elasticAUC)
elasticAUC <- na.omit(elasticAUC)
summary(elasticAUC)

head(elasticAUC$VA_AUC)

#data partition (create test and train groups)
set.seed(123)
ind <- sample(2, nrow(elasticAUC), replace=T, prob = c((2/3), (1/3)))
train <- elasticAUC[ind==1,]
test <- elasticAUC[ind==2,]

summary(train)

#custom control parameters
custom <- trainControl(method= "repeatedcv", 
                       number = 10,
                       repeats= 5,
                       verboseIter = T)

#try linear model
set.seed(1234)
lm <- train(VA_AUC ~ ., 
            train, 
            method = "lm",
            trControl = custom)

lm$results
lm
summary(lm)
plot(lm$finalModel)

#try ridge regression
set.seed(1234)
ridge <- train(VA_AUC ~ ., 
               train,
               method="glmnet",
               tuneGrid= expand.grid(alpha=0, 
                                   lambda = seq(1000, 1300, length=10)),
               trControl=custom)
plot(ridge)
ridge
plot(ridge$finalModel, xvar="lambda", label=T)
plot(ridge$finalModel, xvar="dev", label=T)
plot(varImp(ridge, scale=T))

#try lasso regression
set.seed(1234)
lasso <- train(VA_AUC ~ ., 
               train,
               method="glmnet",
               tuneGrid= expand.grid(alpha=1, 
                                     lambda = seq(1, 100, length=10)),
               trControl=custom)
plot(lasso)
lasso
plot(lasso$finalModel, xvar="lambda", label=T)
plot(lasso$finalModel, xvar="dev", label=T)
plot(varImp(lasso, scale=T))

#lets go elastic net
set.seed(1234)
custom <- trainControl(method= "repeatedcv", 
                       number = 10,
                       repeats= 5,
                       verboseIter = T)
elasticnet <- train(VA_AUC ~ ., 
               train,
               method="glmnet",
               tuneGrid= expand.grid(alpha=seq(0,1, length=10), 
                                     lambda = seq(80, 140, length=10)),
               trControl=custom)
plot(elasticnet)
elasticnet
plot(elasticnet$finalModel, xvar="lambda", label=T)
plot(elasticnet$finalModel, xvar="dev", label=T)
plot(varImp(elasticnet, scale=F))

#compare the models
model_list <- list(LinearModel = lm, Ridge = ridge, Lasso = lasso, ElasticNet = elasticnet)
res <- resamples(model_list)

summary(res)
bwplot(res)
bwplot(res, metric="Rsquared")
bwplot(res, metric="RMSE")
xyplot(res, metric="RMSE")
#how do you compare other models besides linear and ridge?
?xyplot

#which is the best model?
elasticnet$bestTune
best <- elasticnet$finalModel
coef(best, s = elasticnet$bestTune$lambda)

#save final model for later use
saveRDS(elasticnet, "elasticnet_model.rds")
fm <- readRDS("elasticnet_model.rds")
print(fm)

#Use model for prediction
p1 <- predict(fm, train)
sqrt(mean((train$VA_AUC-p1)^2))

p2 <- predict(fm, test)
sqrt(mean((test$VA_AUC-p2)^2))

?predict()

#compare with correlation matrix
summary(elasticAUC)
elasticAUCcorr <- rename(elasticAUC, RNFL_OU_I = RNFL_OU_Inferior, RNFL_OU_N= RNFL_OU_Nasal,
                                        RNFL_OU_S= RNFL_OU_Superior, RNFL_OU_T= RNFL_OU_Temporal,
                                        Mac_OU_C = ILM_RPE_Thx_OU_Central, Mac_OU_I1= ILM_RPE_Thx_OU_I1,
                                        Mac_OU_I2= ILM_RPE_Thx_OU_I2, Mac_OU_N1= ILM_RPE_Thx_OU_N1,
                                        Mac_OU_N2= ILM_RPE_Thx_OU_N2, Mac_OU_S1= ILM_RPE_Thx_OU_S1,
                                        Mac_OU_S2= ILM_RPE_Thx_OU_S2, Mac_OU_T1= ILM_RPE_Thx_OU_T1,
                                        Mac_OU_T2= ILM_RPE_Thx_OU_T2)
summary(VA_OCT_corr)
library(Hmisc)

elasticAUC_corrMatrix <- rcorr(as.matrix(elasticAUCcorr), type="spearman")
elasticAUC_corrMatrix

elasticAUC_corrMatrix$r #extract correlation coefficients
elasticAUC_corrMatrix$n 
elasticAUC_corrMatrix$P #extract p-values

library(corrplot)
corrplot(elasticAUC_corrMatrix$r, method="circle", type="full", order= "hclust",
         p.mat=elasticAUC_corrMatrix$P, sig.level = 0.05, insig="blank", tl.cex=0.5)

##Shall I do it with everyone in the dataset?? Don't mind if I do

#Add missing columns to whole dataset
final_RNFL_data <- read.csv("final_vision_data.csv")

summary(final_RNFL_data)
final_RNFL_data$RNFL_OU_Inferior <- (final_RNFL_data$RNFL_OS_Inferior + final_RNFL_data$RNFL_OD_Inferior)/2
final_RNFL_data$RNFL_OU_Superior <- (final_RNFL_data$RNFL_OS_Superior + final_RNFL_data$RNFL_OD_Superior)/2
final_RNFL_data$RNFL_OU_Nasal <- (final_RNFL_data$RNFL_OS_Nasal + final_RNFL_data$RNFL_OD_Nasal)/2
final_RNFL_data$RNFL_OU_Temporal <- (final_RNFL_data$RNFL_OS_Temporal + final_RNFL_data$RNFL_OD_Temporal)/2
final_RNFL_data$ILM_RPE_Thx_OU_S2 <- (final_RNFL_data$ILM_RPE_Thx_OD_S2 + final_RNFL_data$ILM_RPE_Thx_OS_S2)/2
final_RNFL_data$ILM_RPE_Thx_OU_N1 <- (final_RNFL_data$ILM_RPE_Thx_OD_N1 + final_RNFL_data$ILM_RPE_Thx_OS_N1)/2
final_RNFL_data$ILM_RPE_Thx_OU_I1 <- (final_RNFL_data$ILM_RPE_Thx_OD_I1 + final_RNFL_data$ILM_RPE_Thx_OS_I1)/2
final_RNFL_data$ILM_RPE_Thx_OU_T1 <- (final_RNFL_data$ILM_RPE_Thx_OD_T1 + final_RNFL_data$ILM_RPE_Thx_OS_T1)/2
final_RNFL_data$ILM_RPE_Thx_OU_N2 <- (final_RNFL_data$ILM_RPE_Thx_OD_N2 + final_RNFL_data$ILM_RPE_Thx_OS_N2)/2
final_RNFL_data$ILM_RPE_Thx_OU_I2 <- (final_RNFL_data$ILM_RPE_Thx_OD_I2 + final_RNFL_data$ILM_RPE_Thx_OS_I2)/2
final_RNFL_data$ILM_RPE_Thx_OU_T2 <- (final_RNFL_data$ILM_RPE_Thx_OD_T2 + final_RNFL_data$ILM_RPE_Thx_OS_T2)/2
final_RNFL_data$ILM_RPE_Thx_OU_S1 <- (final_RNFL_data$ILM_RPE_Thx_OD_S1 + final_RNFL_data$ILM_RPE_Thx_OS_S1)/2
final_RNFL_data$ILM_RPE_Thx_OU_Central <- (final_RNFL_data$ILM_RPE_Thx_OD_Central + final_RNFL_data$ILM_RPE_Thx_OS_Central)/2
summary(final_RNFL_data)

summary(final_RNFL_data$VA_1.25_contrast_OU)
summary(final_RNFL_data$VA_2.5_contrast_OU)
summary(final_RNFL_data$VA_100_contrast_OU)

naomitted <- subset(final_RNFL_data, select = c("RNFL_OU_Inferior", "RNFL_OU_Nasal",
                                                 "RNFL_OU_Superior","RNFL_OU_Temporal",
                                                 "Age", "ILM_RPE_Thx_OU_Central", "ILM_RPE_Thx_OU_I1",
                                                 "ILM_RPE_Thx_OU_I2","ILM_RPE_Thx_OU_N1",
                                                 "ILM_RPE_Thx_OU_N2","ILM_RPE_Thx_OU_S1",
                                                 "ILM_RPE_Thx_OU_S2","ILM_RPE_Thx_OU_T1",
                                                 "ILM_RPE_Thx_OU_T2", "VA_1.25_contrast_OU",
                                                "VA_2.5_contrast_OU", "VA_100_contrast_OU", "condition", "diagnosis")) 
summary(naomitted)
naomitted <- na.omit(naomitted)
summary(naomitted)

n <- count(naomitted)[1,1]
res <- rep(NA, n)
int <- rep(NA, n)
auc <- rep(NA, n)


for (i in 1:n){
  patient <- naomitted[i,]
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

elasticAUC <- subset(naomitted, select = c("RNFL_OU_Inferior", "RNFL_OU_Nasal",
                                              "RNFL_OU_Superior","RNFL_OU_Temporal",
                                              "VA_AUC","Age", "ILM_RPE_Thx_OU_Central", "ILM_RPE_Thx_OU_I1",
                                              "ILM_RPE_Thx_OU_I2","ILM_RPE_Thx_OU_N1",
                                              "ILM_RPE_Thx_OU_N2","ILM_RPE_Thx_OU_S1",
                                              "ILM_RPE_Thx_OU_S2","ILM_RPE_Thx_OU_T1",
                                              "ILM_RPE_Thx_OU_T2", "diagnosis", "condition")) 
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

#try linear model
set.seed(1234)
lm <- train(VA_AUC ~ ., 
            train, 
            method = "lm",
            trControl = custom)

lm$results
lm
summary(lm)
plot(lm$finalModel)

#try ridge regression
set.seed(1234)
ridge <- train(VA_AUC ~ ., 
               train,
               method="glmnet",
               tuneGrid= expand.grid(alpha=0, 
                                     lambda = seq(50, 150, length=10)),
               trControl=custom)
plot(ridge)
ridge
plot(ridge$finalModel, xvar="lambda", label=T)
plot(ridge$finalModel, xvar="dev", label=T)
plot(varImp(ridge, scale=T))

#try lasso regression
set.seed(1234)
lasso <- train(VA_AUC ~ ., 
               train,
               method="glmnet",
               tuneGrid= expand.grid(alpha=1, 
                                     lambda = seq(1, 10, length=10)),
               trControl=custom)
plot(lasso)
lasso
plot(lasso$finalModel, xvar="lambda", label=T)
plot(lasso$finalModel, xvar="dev", label=T)
plot(varImp(lasso, scale=T))

#lets go elastic net
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

#compare the models
model_list <- list(LinearModel = lm, Ridge = ridge, Lasso = lasso, ElasticNet = elasticnet)
res <- resamples(model_list)

summary(res)
bwplot(res)
bwplot(res, metric="Rsquared")
bwplot(res, metric="RMSE")

xyplot(res, metric="RMSE")
#how do you compare other models besides linear and ridge?
?xyplot

#which is the best model?
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

ggplot(train, aes(x=prediction, y=VA_AUC)) +
  geom_point() +
  ggtitle("Training Observed vs. Predicted (EN)") +
  stat_cor(method="spearman") +
  stat_regline_equation(label.y=4000, aes(label=..rr.label..)) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=4500) +
  geom_abline(slope=1, intercept=0)

p2 <- predict(fm, test)
sqrt(mean((test$VA_AUC-p2)^2))

test$prediction <- p2
test2$prediction <- p2

ggplot(test, aes(x=prediction, y=VA_AUC)) +
  geom_point() +
  ggtitle("Test Observed vs. Predicted (EN)") +
  stat_cor(method="spearman") +
  stat_regline_equation(label.y=4000, aes(label=..rr.label..)) +
  geom_smooth(method="lm", se=F, formula=y~x) +
  stat_regline_equation(label.y=4500, formula=y~x) +
  geom_abline(slope=1, intercept=0) #+
  #coord_cartesian(xlim=c(0,4000), ylim=c(0,4000))

#save other models to validate
saveRDS(lm, "linear_model.rds")
linear <- readRDS("linear_model.rds")
print(linear)

saveRDS(ridge, "ridge_model.rds")
ridgemod <- readRDS("ridge_model.rds")
print(ridgemod)

saveRDS(lasso, "lasso_model.rds")
lassomod <- readRDS("lasso_model.rds")
print(lassomod)

#validate linear
p1 <- predict(linear, train)
sqrt(mean((train$VA_AUC-p1)^2))

p2 <- predict(linear, test)
sqrt(mean((test$VA_AUC-p2)^2))

train$prediction <- p1
test$prediction <- p2

ggplot(train, aes(x=prediction, y=VA_AUC)) +
  geom_point() +
  ggtitle("Training Observed vs. Predicted (Linear)") +
  stat_cor(method="spearman") +
  stat_regline_equation(label.y=4000, aes(label=..rr.label..)) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=4500) +
  geom_abline(slope=1, intercept=0)

ggplot(test, aes(x=prediction, y=VA_AUC)) +
  geom_point() +
  ggtitle("Test Observed vs. Predicted (Linear)") +
  stat_cor(method="spearman") +
  stat_regline_equation(label.y=4000, aes(label=..rr.label..)) +
  geom_smooth(method="lm", se=F, formula=y~x) +
  stat_regline_equation(label.y=4500, formula=y~x) +
  geom_abline(slope=1, intercept=0)

#validate ridge
p1 <- predict(ridgemod, train)
sqrt(mean((train$VA_AUC-p1)^2))

p2 <- predict(ridgemod, test)
sqrt(mean((test$VA_AUC-p2)^2))

train$prediction <- p1
test$prediction <- p2

ggplot(train, aes(x=prediction, y=VA_AUC)) +
  geom_point() +
  ggtitle("Training Observed vs. Predicted (Ridge)") +
  stat_cor(method="spearman", label.y=5000) +
  stat_regline_equation(label.y=4000, aes(label=..rr.label..)) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=4500) +
  geom_abline(slope=1, intercept=0)

ggplot(test, aes(x=prediction, y=VA_AUC)) +
  geom_point() +
  ggtitle("Test Observed vs. Predicted (Ridge)") +
  stat_cor(method="spearman", label.y=5000) +
  stat_regline_equation(label.y=4000, aes(label=..rr.label..)) +
  geom_smooth(method="lm", se=F, formula=y~x) +
  stat_regline_equation(label.y=4500, formula=y~x) +
  geom_abline(slope=1, intercept=0)

#validate lasso
p1 <- predict(lassomod, train)
sqrt(mean((train$VA_AUC-p1)^2))

p2 <- predict(lassomod, test)
sqrt(mean((test$VA_AUC-p2)^2))

train$prediction <- p1
test$prediction <- p2

ggplot(train, aes(x=prediction, y=VA_AUC)) +
  geom_point() +
  ggtitle("Training Observed vs. Predicted (Lasso)") +
  stat_cor(method="spearman", label.y=5000) +
  stat_regline_equation(label.y=4000, aes(label=..rr.label..)) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=4500) +
  geom_abline(slope=1, intercept=0)

ggplot(test, aes(x=prediction, y=VA_AUC)) +
  geom_point() +
  ggtitle("Test Observed vs. Predicted (Lasso)") +
  stat_cor(method="spearman", label.y=5000) +
  stat_regline_equation(label.y=4000, aes(label=..rr.label..)) +
  geom_smooth(method="lm", se=F, formula=y~x) +
  stat_regline_equation(label.y=4500, formula=y~x) +
  geom_abline(slope=1, intercept=0)

#try including diagnosis (train2)
ggplot(train2, aes(x=prediction, y=VA_AUC)) +
  geom_point(aes(col=condition)) +
  ggtitle("Training Observed vs. Predicted (EN)") +
  stat_cor(method="spearman") +
  stat_regline_equation(label.y=4000, aes(label=..rr.label..)) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=4500) +
  geom_abline(slope=1, intercept=0)

ggplot(test2, aes(x=prediction, y=VA_AUC)) +
  geom_point(aes(col=condition)) +
  ggtitle("Test Observed vs. Predicted (EN)") +
  stat_cor(method="spearman") +
  stat_regline_equation(label.y=4500, aes(label=..rr.label..)) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=5000) +
  geom_abline(slope=1, intercept=0)
