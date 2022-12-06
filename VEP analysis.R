#In this document, I worked with VEP data, experimented with PCA, and used PCA
#to calculate the VEP index I would later use when adding VEP to vision model
#APS

install.packages("ggplot2")
install.packages("plotly")
#install.packages("dplyr")
install.packages("knitr")
install.packages("markdown")
#install.packages("xfun")

library(ggplot2)
library(plotly)
library(dplyr)
library(knitr)
library(markdown)
library(xfun)

install.packages("ggpubr")
library(ggpubr)

VEPdata<- read.csv("VEP data 2.csv")
#VEPdata<- read.csv("VEP_model_data.csv")
VEPdata <- na.omit(VEPdata)
summary(VEPdata)

#Graphs for right eye and left eye comparing small and large VEP frame
OD <- ggplot(VEPdata, aes(x=VEP_OD_large, y=VEP_OD_small)) + 
  geom_point(aes(col=diagnosis))
OD

OS <- ggplot(VEPdata, aes(x=VEP_OS_large, y=VEP_OS_small)) + 
  geom_point(aes(col=diagnosis))
OS

#Graphs for small and large VEP frame comparing left and right eyes
VEPsmall <- ggplot(VEPdata, aes(x=VEP_OD_small, y=VEP_OS_small)) + 
  geom_point(aes(col=diagnosis))
VEPsmall

VEPlarge <- ggplot(VEPdata, aes(x=VEP_OD_large, y=VEP_OS_large)) + 
  geom_point(aes(col=diagnosis))
ggplotly(VEPlarge)

#Does VEP cutoff depend on age?
VEPcutoff <- ggplot(VEPdata, aes(x=Age, y=VEP_cutoff)) +
  geom_point(aes(col=diagnosis))

ggplotly(VEPcutoff)

#first experimentation with PCA- tried to calculate PCA with many inputs
#PCA analysis
pcadata<- subset(VEPdata, select = -c(gender, diagnosis, Date, VEP_date, patientID, VEP_difference_small, VEP_difference_large, VEP_cutoff))
?prcomp

is.na(pcadata)
pcadata2 <- pcadata[-44,1:5]
pca <- prcomp(pcadata2, scale=T)
plot(pca$x[,1], pca$x[,2])

pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main= "Scree Plot", xlab = "Principal Component", ylab= "Percent of Variation")
#Scree plot 

#create dataset with coordinates for PCA plot
pca_plotdata <- data.frame(sample=rownames(pca$x), x=pca$x[,1], y=pca$x[,2])
summary(pca_plotdata)

#PCA plot in ggplot
ggplot(data=pca_plotdata, aes(x=x, y=y, label=sample)) + geom_text() + 
    xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) + 
    ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
    theme_bw()

#Which variables have greatest weight on PCs?
#PC1
loading_scores <- pca$rotation[,1]
variable_scores <- abs(loading_scores)
variable_scores_ranked <- sort(variable_scores, decreasing=T)
top_variables <- names(variable_scores_ranked)
top_variables

pca$rotation[top_variables, 1]

#PCA to combine large and small into single VEP index (right eye/OD)
#useful for finding index to use to add to model later
VEP_OD <- subset(VEPdata, select=c(VEP_OD_large, VEP_OD_small))
VEP_OD_pca <- prcomp(VEP_OD, scale=T)
plot(VEP_OD_pca$x[,1], VEP_OD_pca$x[,2])

pca_var_VEP_OD <- VEP_OD_pca$sdev^2
pca_var_per_VEP_OD <- round(pca_var_VEP_OD/sum(pca_var_VEP_OD)*100, 1)
barplot(pca_var_per_VEP_OD, main= "Scree Plot", xlab = "Principal Component", ylab= "Percent of Variation")

VEP_OD_pca_plotdata <- data.frame(sample=rownames(VEP_OD_pca$x), x=VEP_OD_pca$x[,1], y=VEP_OD_pca$x[,2])
summary(VEP_OD_pca_plotdata)

h <- ggplot(data=VEP_OD_pca_plotdata, aes(x=x, y=y, label=sample)) + geom_point() + 
  xlab(paste("PC1 - ", pca_var_per_VEP_OD[1], "%", sep="")) + 
  ylab(paste("PC2 - ", pca_var_per_VEP_OD[2], "%", sep="")) +
  theme_bw() + labs(title="VEP OD PCA")
ggplotly(h)
?ggplot
?aes

summary(VEP_OD_pca$x[,1])

#add OD index to data frame
#VEP_index_OD <- VEP_OD_pca$x[,1]
#or multiply by -1 to flip and improve interpretation: 
VEP_index_OD <- (-1)*VEP_OD_pca$x[,1]
?data.frame
VEPdata1 <- data.frame(VEPdata, VEP_index_OD)
summary(VEPdata1)
VEPdata1
tail(VEPdata1)
VEPdata1[44,]

#check for high correlation with original merged variables
ggplot(VEPdata1, aes(x=VEP_index_OD, VEP_OD_small)) + 
  geom_point() +
  stat_cor(method="spearman")+
  geom_smooth(method="lm", se=F) +
  stat_regline_equation(label.y=150) +
  stat_regline_equation(aes(label=..rr.label..), label.y=160)

#repeat for left eye data (OS)
VEP_OS <- subset(VEPdata, select=c(VEP_OS_large, VEP_OS_small), na.rm=T)
summary(VEP_OS)
VEP_OS1 <- na.omit(VEP_OS)
VEP_OS_pca <- prcomp(VEP_OS1, scale=T)
plot(VEP_OS_pca$x[,1], VEP_OS_pca$x[,2])

pca_var_VEP_OS <- VEP_OS_pca$sdev^2
pca_var_per_VEP_OS <- round(pca_var_VEP_OS/sum(pca_var_VEP_OS)*100, 1)
barplot(pca_var_per_VEP_OS, main= "Scree Plot", xlab = "Principal Component", ylab= "Percent of Variation")

VEP_OS_pca_plotdata <- data.frame(sample=rownames(VEP_OS_pca$x), x=VEP_OS_pca$x[,1], y=VEP_OS_pca$x[,2])
summary(VEP_OD_pca_plotdata)

r <- ggplot(data=VEP_OS_pca_plotdata, aes(x=x, y=y, label=sample)) + geom_point() + 
  xlab(paste("PC1 - ", pca_var_per_VEP_OS[1], "%", sep="")) + 
  ylab(paste("PC2 - ", pca_var_per_VEP_OS[2], "%", sep="")) +
  theme_bw() + labs(title="VEP OS PCA")
ggplotly(r)

summary(VEP_OS_pca$x[,1])

#Add OS index to data frame
#VEP_index_OS <- VEP_OS_pca$x[,1]
VEP_index_OS <- (-1)*VEP_OS_pca$x[,1]
?data.frame
summary(VEP_index_OS)
#took out entry 44, which had NAs
VEPdata1 <- na.omit(VEPdata1)
newVEPdata <- data.frame(VEPdata1, VEP_index_OS)
summary(newVEPdata)

ggplot(newVEPdata, aes(x=VEP_index_OS, VEP_OS_large)) + 
  geom_point(aes(col=diagnosis)) +
  geom_smooth(method="lm", se=F)
  

#comparing left and right eyes using new VEP index

ggplot(newVEPdata, aes(x=VEP_index_OD, y=VEP_index_OS)) + 
  geom_point(aes(col=diagnosis)) + 
  stat_cor(method="pearson") +
  geom_smooth(method="lm", se=F, formula=y~x-1) +
  stat_regline_equation(label.y=4, formula= y~x-1) +
  stat_regline_equation(aes(label=..rr.label..), label.y=3) +
  geom_abline(slope=1, intercept =0)+
  coord_cartesian(xlim=c(-2, 4.5), ylim=c(-2, 4.5))
summary(lm(VEP_index_OS~VEP_index_OD, data=newVEPdata))
?stat_cor

p <- ggplotly(ggplot(newVEPdata, aes(x=VEP_index_OS, y=VEP_index_OD)) + geom_point(aes(col=diagnosis)))
p
htmlwidgets::saveWidget(as_widget(p), "VEP_index_plot.html")

#correlation between Age and VEP index, can look at non-MS patients separately
q <- ggplotly(ggplot(newVEPdata, aes(x=Age, y=VEP_index_OD)) + geom_point(aes(col=diagnosis)))
q
htmlwidgets::saveWidget(as_widget(q), "VEP_index_vs_Age.html")

#and Height
r <- ggplotly(ggplot(newVEPdata, aes(x=Height, y=VEP_index_OD)) + geom_point(aes(col=diagnosis)))
r
htmlwidgets::saveWidget(as_widget(r), "VEP_index_vs_Height.html")


#How do we define "normal" VEP results (using PCA score)? 
nonMScohort <- subset(newVEPdata,diagnosis == "CIS" | 
                        diagnosis == "Healthy Donor"|diagnosis ==  "NIND"|
                        diagnosis ==  "OIND"|diagnosis ==  "RIS")
ggplot(nonMScohort, aes(x=VEP_index_OD, y=VEP_index_OS)) + geom_point(aes(col=diagnosis)) +
  geom_smooth(method="lm", se=F) + stat_cor(method="pearson", label.y=2) +
  stat_regline_equation(label.y=4) +
  stat_regline_equation(aes(label=..rr.label..), label.y=3)+
  geom_abline(slope=1, intercept=0)+
  coord_cartesian(xlim=c(-2,4.5), ylim=c(-2, 4.5))
model <- lm(VEP_index_OS ~ VEP_index_OD, data=nonMScohort)
summary(model)
?stat_cor

#Add correlation coefficients and p values to initial plots w/ VEP index
ggplot(nonMScohort, aes(x=Age, y=VEP_index_OD)) + 
  geom_point(aes(col=diagnosis)) + stat_cor(method="pearson")

ggplot(nonMScohort, aes(x=Height, y=VEP_index_OD)) + 
  geom_point(aes(col=diagnosis)) + stat_cor(method="pearson")

#analyze "normal" difference between eyes in VEP index
nonMScohort$VEP_index_diff <- abs(nonMScohort$VEP_index_OD - nonMScohort$VEP_index_OS)
head(nonMScohort)
summary(nonMScohort$VEP_index_diff)
plot(nonMScohort$VEP_index_diff)
boxplot(nonMScohort$VEP_index_diff, main="VEP index difference between eyes (non-MS)")
ggplot(nonMScohort) + geom_boxplot(aes(y=VEP_index_diff))
?geom_boxplot
#plot of IQR for nonMS patients' VEP difference

newVEPdata$VEP_index_diff <- abs(newVEPdata$VEP_index_OD - newVEPdata$VEP_index_OS)
summary(newVEPdata$VEP_index_diff)
boxplot(newVEPdata$VEP_index_diff, main="VEP index difference between eyes (all subjects)")
ggplot(newVEPdata) + geom_boxplot(aes(y=VEP_index_diff))
#plot of IQR for whole data set VEP difference


#creating data set of "normal-eyed" people based on VEP
normaleyesVEP <- subset(newVEPdata, VEP_index_OD < 0 & VEP_index_OS < 0 & VEP_index_diff < 0.44)
ggplot(normaleyesVEP, aes(x=VEP_index_OD, y=VEP_index_OS)) + geom_point(aes(col=diagnosis)) +
  stat_cor(method="pearson") + 
  stat_regline_equation(label.y=-0.5)+
  geom_smooth(method="lm", se=F)+
  stat_regline_equation(aes(label=..rr.label..), label.y= -0.6)

#finding abnormal eyes in the non-MS cohort
abnormaleyesVEP_nonMS <- subset(nonMScohort, VEP_index_OD >= 0 | VEP_index_OS >= 0 | VEP_index_diff >= 0.44)
abnormaleyesVEP_nonMS

?labs
