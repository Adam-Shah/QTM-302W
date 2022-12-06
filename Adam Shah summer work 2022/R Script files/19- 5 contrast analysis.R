#In this document, I revisited creating the area under the curve measure but
#for 5 contrasts instead of 3. This changed the scale of the contrasts, and there
#was not as much data as for 3 contrasts, but worth investigating 


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

mydata <- read.csv("final_vision_data.csv")

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

