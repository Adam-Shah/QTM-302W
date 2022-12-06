#In this document, I revisited creating the area under the curve measure but
#for 5 contrasts instead of 3. This changed the scale of the contrasts, looked for
#correlations in 5 contrasts, recompared ICC, looked for correlations with NeurEx,
#and ran an elastic net model with OCT for 5 contrasts

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

mydata <- read.csv("fullVAdata.csv")

summary(mydata)

healthyppl <- subset(mydata, mydata$diagnosis == "Healthy Donor")
summary(healthyppl)

VA_subset <- subset(healthyppl, select = c("VA_1.25_contrast_OU", 
                                                 "VA_2.5_contrast_OU", 
                                                 "VA_100_contrast_OU",
                                                  "VA_10_contrast_OU",
                                              "VA_5_contrast_OU",
                                             "condition"))
summary(VA_subset)
VA_subset <- rename(VA_subset, "1.25" = VA_1.25_contrast_OU, 
                      "2.5" = VA_2.5_contrast_OU, 
                      "100"=VA_100_contrast_OU, 
                    "5" = VA_5_contrast_OU,
                    "10" = VA_10_contrast_OU)
library(reshape2)
VA_subsetlong <- melt(VA_subset, id="condition")
VA_subsetlong <- rename(VA_subsetlong, contrast = variable, score=value)
head(VA_subsetlong)
summary(VA_subsetlong)
summary(VA_subsetlong$contrast)
sapply(VA_subsetlong, class)

VA_subsetlong$scaled_contrast <- as.numeric(as.character(VA_subsetlong$contrast))

ggplot(VA_subsetlong, aes(x = as.numeric(as.character(contrast)), y = score)) +
  geom_point(aes(col=condition)) + xlab("percent contrast") +
  scale_x_log10() + 
  #scale_x_continuous(trans = "log2") +
  stat_cor(method="spearman") +
  #geom_smooth(method="loess", formula=y~x, col="red", se=F) +
  geom_smooth(method="lm", formula=y~x-1, col="orange", se=F) +
  geom_smooth(method="lm", se=F, col="blue") +
  stat_regline_equation(label.y=60, formula=y~x-1, col="orange") +
  stat_regline_equation(label.y=63, col="blue") +
  coord_cartesian(xlim=c(1,100))

summary(VA_subset)
median(VA_subset$`5`)

#find scale for each contrast
median(VA_subset$`100`)
median(VA_subset$`2.5`)/(median(VA_subset$`100`)/100)
median(VA_subset$`1.25`)/(median(VA_subset$`100`)/100)
46/(median(VA_subset$`100`)/100)
50/(median(VA_subset$`100`)/100)

#created new scaled contrasts so medians make a straight line from the origin
VA_subset <- subset(healthyppl, select = c("VA_1.25_contrast_OU", 
                                               "VA_2.5_contrast_OU", 
                                               "VA_100_contrast_OU",
                                                "VA_5_contrast_OU", 
                                                "VA_10_contrast_OU",
                                               "condition"))
VA_subset2 <- rename(VA_subset, "46.66667" = VA_1.25_contrast_OU, 
                       "58.33333" = VA_2.5_contrast_OU, 
                       "100"=VA_100_contrast_OU,
                       "76.66667" = VA_5_contrast_OU,
                      "83.33333" = VA_10_contrast_OU)

VA_subsetlong2 <- melt(VA_subset2, id="condition")
VA_subsetlong2 <- rename(VA_subsetlong2, scaled_contrast = variable, score=value)

VA_subsetlong2 <- VA_subsetlong2 %>% mutate(contrast =
                                                  case_when(scaled_contrast == 46.66667 ~ "1.25", 
                                                            scaled_contrast == 58.33333 ~ "2.5",
                                                            scaled_contrast == 100 ~ "100",
                                                            scaled_contrast == 76.66667 ~ "5",
                                                            scaled_contrast == 83.33333 ~ "10"))


ggplot(VA_subsetlong2, aes(x=as.numeric(as.character(scaled_contrast)),y=score)) +
  geom_boxplot(aes(fill=contrast)) +xlab("scaled contrast") +
  coord_cartesian(xlim=c(0,100))+
  geom_abline(slope=0.6, intercept=0)

#When calculating AUC for 5 contrasts, use these scales

summary(mydata)
mydataVA <- subset(mydata, select=c("VA_1.25_contrast_OU", 
                                    "VA_2.5_contrast_OU", 
                                    "VA_100_contrast_OU",
                                    "VA_10_contrast_OU",
                                    "VA_5_contrast_OU",
                                    "lcva_ID"))
mydataVA<- na.omit(mydataVA)
summary(mydataVA)
head(mydataVA)

#AUC for 5 contrasts
n <- count(mydataVA)[1,1]
res <- rep(NA, n)
slo <- rep(NA, n)
int <- rep(NA, n)
auc <- rep(NA, n)

for (i in 1:n){
  patient <- mydataVA[i,]
  VA_subset <- subset(patient, select = c("VA_1.25_contrast_OU", 
                                          "VA_2.5_contrast_OU", 
                                          "VA_100_contrast_OU",
                                          "VA_10_contrast_OU",
                                          "VA_5_contrast_OU",
                                          "lcva_ID"))
  VA_subset2 <- rename(VA_subset, "46.66667" = VA_1.25_contrast_OU, 
                       "58.33333" = VA_2.5_contrast_OU, 
                       "76.66667" = VA_5_contrast_OU,
                       "83.33333" = VA_10_contrast_OU,
                       "100"= VA_100_contrast_OU)
  VA_subsetlong2 <- melt(VA_subset2, id="lcva_ID")
  VA_subsetlong2 <- rename(VA_subsetlong2, scaled_contrast = variable, score=value)
  VA_subsetlong2$scaled_contrast <- as.numeric(as.character(VA_subsetlong2$scaled_contrast))
  
  r <- summary(lm(score~scaled_contrast, VA_subsetlong2))
  res[i] <- r$r.squared
  int[i] <- r$coefficients[1,1]
  slo[i] <- r$coefficients[2,1]
  auc[i] <- 0.5*mydataVA[i,"VA_100_contrast_OU"]*
    (100+(r$coefficients[1,1]/r$coefficients[2,1]))
}
#This code should give the measures for VA regression run with 5 contrasts

mydataVA$VA_rsquared_5c <- res
mydataVA$VA_yint_5c <- int
mydataVA$VA_slope_5c <- slo
mydataVA$VA_AUC_5c <- auc

VAresults5c <- subset(mydataVA, select=c("VA_rsquared_5c", 
                                         "VA_yint_5c",
                                         "VA_slope_5c",
                                         "VA_AUC_5c",
                                         "lcva_ID"))

VA_5c_data<- full_join(mydata, VAresults5c, by="lcva_ID")
summary(VA_5c_data)

#look at relationships with 5c VA AUC w/ Age and height
ggplot(VA_5c_data, aes(x=Age, y=VA_AUC_5c)) +
  geom_point(aes(col=condition2)) +
  stat_cor(method="spearman",label.y=3500)+
  stat_regline_equation(label.y=3250) +
  stat_regline_equation(aes(label=..rr.label..), label.y=3000)+
  geom_smooth(method="lm", se=F)

ggplot(VA_5c_data, aes(x=height, y=VA_AUC_5c)) +
  geom_point(aes(col=condition2)) +
  stat_cor(method="spearman",label.y=3500)+
  stat_regline_equation(label.y=3250) +
  stat_regline_equation(aes(label=..rr.label..), label.y=3000)+
  geom_smooth(method="lm", se=F)
#5c AUCshows significant negative correlation with Age and height

#5c AUC by diagnosis
ggplot(VA_5c_data, aes(x=condition2, y=VA_AUC_5c)) +
  geom_boxplot(aes(fill=condition2))

VA_5c_data %>%
  mutate(diagnostic_group = fct_reorder(condition2, VA_AUC_5c)) %>%
  ggplot(aes(x=diagnostic_group, y=VA_AUC_5c)) +
  geom_boxplot(aes(fill=condition2), alpha=.6, width=.4) +
  theme_bw() +
  theme(legend.position = "none") 

fct_reorder(as.factor(condition2), VA_AUC_5c, .desc=F)
?fct_reorder()
#I'm not sure why putting the groups in descending order is not working, but it's
#a good plot with clear diagnostic differences

#Add 3 contrast VA AUC to dataset
VAdata3c <- subset(mydata, select=c("VA_1.25_contrast_OU", 
                                    "VA_2.5_contrast_OU", 
                                    "VA_100_contrast_OU",
                                    "lcva_ID"))
VAdata3c <- na.omit(VAdata3c)

n <- count(VAdata3c)[1,1]
res <- rep(NA, n)
slo <- rep(NA, n)
int <- rep(NA, n)
auc <- rep(NA, n)

for (i in 1:n){
  patient <- VAdata3c[i,]
  VA_OUsubset <- subset(patient, select = c("VA_1.25_contrast_OU", 
                                            "VA_2.5_contrast_OU", 
                                            "VA_100_contrast_OU",
                                            "lcva_ID"))
  VA_OUsubset2 <- rename(VA_OUsubset, "33.33333" = VA_1.25_contrast_OU, 
                         "52.63158" = VA_2.5_contrast_OU, 
                         "100"=VA_100_contrast_OU)
  VA_OUsubsetlong2 <- melt(VA_OUsubset2, id="lcva_ID")
  VA_OUsubsetlong2 <- rename(VA_OUsubsetlong2, scaled_contrast = variable, score=value)
  VA_OUsubsetlong2$scaled_contrast <- as.numeric(as.character(VA_OUsubsetlong2$scaled_contrast))
  
  r <- summary(lm(score~scaled_contrast, VA_OUsubsetlong2))
  res[i] <- r$r.squared
  slo[i] <- r$coefficients[2,1]
  int[i] <- r$coefficients[1,1]
  auc[i] <- 0.5*VAdata3c[i,"VA_100_contrast_OU"]*
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
VAdata3c$VA_rsquared_3c <- res
VAdata3c$VA_yint_3c <- int
VAdata3c$VA_slope_3c <- slo
VAdata3c$VA_AUC_3c <- auc

VAresults3c <- subset(VAdata3c, select= c("VA_rsquared_3c", 
                                          "VA_yint_3c",
                                          "VA_slope_3c", 
                                          "VA_AUC_3c",
                                          "lcva_ID"))
fullVAdata <- full_join(VA_5c_data, VAresults3c, by="lcva_ID")

#look for relationships between 3 contrast and 5 contrast AUC in full dataset

ggplot(fullVAdata, aes(x=VA_AUC_3c, y=VA_AUC_5c)) +
  geom_point(aes(col=condition2)) +
  stat_cor(method="spearman", label.y=3500)+
  stat_regline_equation(label.y=3250) +
  stat_regline_equation(aes(label=..rr.label..), label.y=3000) +
  geom_smooth(method="lm", se=F)+
  geom_abline(slope=1, intercept=0)

#difference from 1:1 line probably due to different x-axis scales, I'll try again
#using the same scale for 3 contrasts as for 5
#difference in scales is because earlier, the scale for 3 contrast was made from
#medians of "normal-eyed" individuals, while now it is just based on median of 
#healthy donors. It looks like it does not matter which scale is used for correlation
#with 5 contrasts

VAdata3c <- subset(mydata, select=c("VA_1.25_contrast_OU", 
                                    "VA_2.5_contrast_OU", 
                                    "VA_100_contrast_OU",
                                    "lcva_ID"))
VAdata3c <- na.omit(VAdata3c)

n <- count(VAdata3c)[1,1]
res <- rep(NA, n)
slo <- rep(NA, n)
int <- rep(NA, n)
auc <- rep(NA, n)

for (i in 1:n){
  patient <- VAdata3c[i,]
  VA_OUsubset <- subset(patient, select = c("VA_1.25_contrast_OU", 
                                            "VA_2.5_contrast_OU", 
                                            "VA_100_contrast_OU",
                                            "lcva_ID"))
  VA_OUsubset2 <- rename(VA_OUsubset, "46.66667" = VA_1.25_contrast_OU, 
                         "58.33333" = VA_2.5_contrast_OU, 
                         "100"=VA_100_contrast_OU)
  VA_OUsubsetlong2 <- melt(VA_OUsubset2, id="lcva_ID")
  VA_OUsubsetlong2 <- rename(VA_OUsubsetlong2, scaled_contrast = variable, score=value)
  VA_OUsubsetlong2$scaled_contrast <- as.numeric(as.character(VA_OUsubsetlong2$scaled_contrast))
  
  r <- summary(lm(score~scaled_contrast, VA_OUsubsetlong2))
  res[i] <- r$r.squared
  slo[i] <- r$coefficients[2,1]
  int[i] <- r$coefficients[1,1]
  auc[i] <- 0.5*VAdata3c[i,"VA_100_contrast_OU"]*
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
VAdata3c$VA_rsquared_3c <- res
VAdata3c$VA_yint_3c <- int
VAdata3c$VA_slope_3c <- slo
VAdata3c$VA_AUC_3c <- auc

VAresults3c <- subset(VAdata3c, select= c("VA_rsquared_3c", 
                                          "VA_yint_3c",
                                          "VA_slope_3c", 
                                          "VA_AUC_3c",
                                          "lcva_ID"))
fullVAdata <- full_join(VA_5c_data, VAresults3c, by="lcva_ID")

#try it again
ggplot(fullVAdata, aes(x=VA_AUC_3c, y=VA_AUC_5c)) +
  geom_point(aes(col=condition2)) +
  stat_cor(method="spearman", label.y=3500)+
  stat_regline_equation(label.y=3250) +
  stat_regline_equation(aes(label=..rr.label..), label.y=3000) +
  geom_smooth(method="lm", se=F)+
  geom_abline(slope=1, intercept=0)

#Nice, that looks like it perfectly aligns with the 5 contrast AUC

summary(fullVAdata)
#Let's compare 3 contrast to 5 contrast in ICC
ICCdata2 <- subset(fullVAdata, select= c("VA_1.25_contrast_OU",
                                               "VA_2.5_contrast_OU",
                                               "VA_100_contrast_OU",
                                               "VA_yint_3c",
                                               "VA_slope_3c",
                                               "VA_AUC_3c", 
                                              "VA_yint_5c",
                                              "VA_slope_5c",
                                              "VA_AUC_5c", 
                                              "patientID"))
ICCdata2 <- rename(ICCdata2, AUC_3c = VA_AUC_3c, "1.25_contrast"= VA_1.25_contrast_OU,
                  "2.5_contrast" = VA_2.5_contrast_OU, "100_contrast"=VA_100_contrast_OU,
                  slope_3c=VA_slope_3c, intercept_3c= VA_yint_3c,
                  slope_5c=VA_slope_5c, intercept_5c= VA_yint_5c,
                  AUC_5c = VA_AUC_5c)

results <- mult.icc(ICCdata2[,c("AUC_3c",
                               "1.25_contrast", 
                               "2.5_contrast", 
                               "100_contrast", 
                               "slope_3c", "intercept_3c",
                               "slope_5c","intercept_5c",
                               "AUC_5c")], grpid= ICCdata2$patientID)
results

results %>%
  mutate(Variable = fct_reorder(Variable, ICC1)) %>%
  ggplot(aes(x=Variable, y=ICC1)) +
  geom_bar(stat="identity", aes(fill=Variable), alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw() +
  theme(legend.position = "none") +
  geom_text(stat="identity", aes(label = round(ICC1, digits=3)), 
            vjust = 0.5, hjust = 1.1, colour = "black") 
#changing the scales for 3 contrast measures did not affect their ICC, but all 
#measures from 5 contrast had greater ICC

#look for correlations with NeurEx
fullVAdata$NeurEx_Panel_02 <- (fullVAdata$NeurEx_Panel_02_L +
                                          fullVAdata$NeurEx_Panel_02_R)/2
fullVAdata$NeurEx_Panel_03 <- (fullVAdata$NeurEx_Panel_03_L +
                                          fullVAdata$NeurEx_Panel_03_R)/2
fullVAdata$NeurEx_Panel_04 <- (fullVAdata$NeurEx_Panel_04_L +
                                          fullVAdata$NeurEx_Panel_04_R)/2

#correlation table
VAcorr <- subset(fullVAdata, select = c("Age", "NeurEx_Panel_02",
                                               "NeurEx_Panel_03","NeurEx_Panel_04",
                                               "VA_1.25_contrast_OU", "VA_2.5_contrast_OU",
                                               "VA_100_contrast_OU", "VA_AUC_5c", 
                                               "VA_yint_5c", "VA_slope_5c",
                                               "VA_AUC_3c", "VA_yint_3c", "VA_slope_3c"))

VAcorr <- rename(VAcorr,  
                 "1.25_contrast" = VA_1.25_contrast_OU, 
                 "2.5_contrast" = VA_2.5_contrast_OU,
                 "100_contrast" = VA_100_contrast_OU,
                 AUC_5c = VA_AUC_5c, intercept_5c = VA_yint_5c, slope_5c= VA_slope_5c,
                 AUC_3c = VA_AUC_3c, intercept_3c = VA_yint_3c, slope_3c= VA_slope_3c)

library(Hmisc)
VAcorrMatrix <- rcorr(as.matrix(VAcorr), type="spearman")
VAcorrMatrix

VAcorrMatrix$r #extract correlation coefficients
VAcorrMatrix$n 
VAcorrMatrix$P #extract p-values

library(corrplot)
corrplot(VAcorrMatrix$r, method="number", type="upper",
         p.mat=VAcorrMatrix$P, sig.level = 0.05, insig="blank", 
         tl.cex=0.5,tl.srt=60, number.cex=0.6)

#Run elastic net regression to predict 5 contrast AUC with OCT
#Add missing variables to dataset
fullVAdata$RNFL_OU_Inferior <- (fullVAdata$RNFL_OS_Inferior + fullVAdata$RNFL_OD_Inferior)/2
fullVAdata$RNFL_OU_Superior <- (fullVAdata$RNFL_OS_Superior + fullVAdata$RNFL_OD_Superior)/2
fullVAdata$RNFL_OU_Nasal <- (fullVAdata$RNFL_OS_Nasal + fullVAdata$RNFL_OD_Nasal)/2
fullVAdata$RNFL_OU_Temporal <- (fullVAdata$RNFL_OS_Temporal + fullVAdata$RNFL_OD_Temporal)/2
fullVAdata$ILM_RPE_Thx_OU_S2 <- (fullVAdata$ILM_RPE_Thx_OD_S2 + fullVAdata$ILM_RPE_Thx_OS_S2)/2
fullVAdata$ILM_RPE_Thx_OU_N1 <- (fullVAdata$ILM_RPE_Thx_OD_N1 + fullVAdata$ILM_RPE_Thx_OS_N1)/2
fullVAdata$ILM_RPE_Thx_OU_I1 <- (fullVAdata$ILM_RPE_Thx_OD_I1 + fullVAdata$ILM_RPE_Thx_OS_I1)/2
fullVAdata$ILM_RPE_Thx_OU_T1 <- (fullVAdata$ILM_RPE_Thx_OD_T1 + fullVAdata$ILM_RPE_Thx_OS_T1)/2
fullVAdata$ILM_RPE_Thx_OU_N2 <- (fullVAdata$ILM_RPE_Thx_OD_N2 + fullVAdata$ILM_RPE_Thx_OS_N2)/2
fullVAdata$ILM_RPE_Thx_OU_I2 <- (fullVAdata$ILM_RPE_Thx_OD_I2 + fullVAdata$ILM_RPE_Thx_OS_I2)/2
fullVAdata$ILM_RPE_Thx_OU_T2 <- (fullVAdata$ILM_RPE_Thx_OD_T2 + fullVAdata$ILM_RPE_Thx_OS_T2)/2
fullVAdata$ILM_RPE_Thx_OU_S1 <- (fullVAdata$ILM_RPE_Thx_OD_S1 + fullVAdata$ILM_RPE_Thx_OS_S1)/2
fullVAdata$ILM_RPE_Thx_OU_Central <- (fullVAdata$ILM_RPE_Thx_OD_Central + fullVAdata$ILM_RPE_Thx_OS_Central)/2

elasticdata_5c <- subset(fullVAdata, select= c("lcva_ID", "RNFL_OU_Inferior", "RNFL_OU_Nasal",
                                               "RNFL_OU_Superior","RNFL_OU_Temporal",
                                               "VA_AUC_5c","Age", "ILM_RPE_Thx_OU_Central", "ILM_RPE_Thx_OU_I1",
                                               "ILM_RPE_Thx_OU_I2","ILM_RPE_Thx_OU_N1",
                                               "ILM_RPE_Thx_OU_N2","ILM_RPE_Thx_OU_S1",
                                               "ILM_RPE_Thx_OU_S2","ILM_RPE_Thx_OU_T1",
                                               "ILM_RPE_Thx_OU_T2", "diagnosis", "condition2"))
elasticdata_5c<- na.omit(elasticdata_5c)

#data partition
set.seed(747)
ind <- sample(2, nrow(elasticdata_5c), replace=T, prob = c((2/3), (1/3)))
train2 <- elasticdata_5c[ind==1,]
test2 <- elasticdata_5c[ind==2,]

train <- subset(train2, select = c("RNFL_OU_Inferior", "RNFL_OU_Nasal",
                                           "RNFL_OU_Superior","RNFL_OU_Temporal",
                                           "VA_AUC_5c","Age", "ILM_RPE_Thx_OU_Central", "ILM_RPE_Thx_OU_I1",
                                           "ILM_RPE_Thx_OU_I2","ILM_RPE_Thx_OU_N1",
                                           "ILM_RPE_Thx_OU_N2","ILM_RPE_Thx_OU_S1",
                                           "ILM_RPE_Thx_OU_S2","ILM_RPE_Thx_OU_T1",
                                           "ILM_RPE_Thx_OU_T2"))

test <- subset(test2, select = c("RNFL_OU_Inferior", "RNFL_OU_Nasal",
                                         "RNFL_OU_Superior","RNFL_OU_Temporal",
                                         "VA_AUC_5c","Age", "ILM_RPE_Thx_OU_Central", "ILM_RPE_Thx_OU_I1",
                                         "ILM_RPE_Thx_OU_I2","ILM_RPE_Thx_OU_N1",
                                         "ILM_RPE_Thx_OU_N2","ILM_RPE_Thx_OU_S1",
                                         "ILM_RPE_Thx_OU_S2","ILM_RPE_Thx_OU_T1",
                                         "ILM_RPE_Thx_OU_T2"))

summary(train)
str(train)
str(test)

#custom control parameters
custom <- trainControl(method= "repeatedcv", 
                       number = 10,
                       repeats= 5,
                       verboseIter = T)

#elastic net
set.seed(1077)
en <- train(VA_AUC_5c ~ ., 
                 train,
                 method="glmnet",
                 tuneGrid= expand.grid(alpha=seq(0,1, length=10), 
                                       lambda = seq(10, 50, length=10)),
                 trControl=custom)
plot(en)
en
plot(en$finalModel, xvar="lambda", label=T)
plot(en$finalModel, xvar="dev", label=T)
plot(varImp(en, scale=F))

#best fit model parameters and coefficients
en$bestTune
best <- en$finalModel
coef(best, s = en$bestTune$lambda)

#save model
saveRDS(en, "5c_elasticnet_model.rds")
model <- readRDS("5c_elasticnet_model.rds")
print(model)

#Use model for prediction
p1 <- predict(en, train)
sqrt(mean((train$VA_AUC_5c-p1)^2))

train$AUC_prediction_5c <- p1
train2$AUC_prediction_5c <- p1

ggplot(train2, aes(x=AUC_prediction_5c, y=VA_AUC_5c)) +
  geom_point(aes(col=condition2)) +
  ggtitle("Training Observed vs. Predicted (5c w/OCT binocular)") +
  stat_cor(method="spearman") +
  stat_regline_equation(label.y=4000, aes(label=..rr.label..)) +
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=4500) +
  geom_abline(slope=1, intercept=0)

p2 <- predict(en, test)
sqrt(mean((test$VA_AUC_5c-p2)^2))

test$AUC_prediction_5c <- p2
test2$AUC_prediction_5c <- p2

ggplot(test2, aes(x=AUC_prediction_5c, y=VA_AUC_5c)) +
  geom_point(aes(col=condition2)) +
  ggtitle("Test Observed vs. Predicted (5c w/OCT binocular)") +
  stat_cor(method="spearman") +
  stat_regline_equation(label.y=4000, aes(label=..rr.label..)) +
  geom_smooth(method="lm", se=F, formula=y~x) +
  stat_regline_equation(label.y=4500, formula=y~x) +
  geom_abline(slope=1, intercept=0) 

#graphs for validating elastic net prediction

AUC_data_5c <- rbind(train2, test2)


