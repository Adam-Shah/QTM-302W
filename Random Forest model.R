#In this document, I started researching and trying out a random forest model,
#but I stopped since Bibi suspected I wouldn't get better results than EN.
#APS

install.packages("randomForest")
library(randomForest)
library(ggplot2)
library(cowplot)

summary(elasticAUC)
forest <- elasticAUC
str(forest)

set.seed(747)
forestmodel <- randomForest(VA_AUC ~ ., data=forest, proximity = T)
forestmodel
forestmodel$predicted
summary(forestmodel)
plot(forestmodel)
