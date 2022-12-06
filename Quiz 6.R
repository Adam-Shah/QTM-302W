USArrests<-read.csv("USArrests.csv")
gss2k<-read.csv("gss2k.csv")
str(USArrests)
#Which state shows the 18th lowest assault rate?
USArrests1<-USArrests[order(USArrests$Rape, decreasing=TRUE),]
USArrests1[18,]
str(USArrests1)
#Which state shows the 16th highest murder rate?
USArrests2<-USArrests[order(USArrests$Murder, decreasing=FALSE),]
USArrests2[16,]
str(USArrests2)
#What is the average murder rate of the states with higher than 66% of urban population?
urban_states<-subset(USArrests, subset=USArrests$UrbanPop>66)
mean(urban_states$Murder, na.rm = TRUE)
#What is the average murder rate of the states with less than or equal to 66% of urban population? 
rural_states<-subset(USArrests, subset=USArrests$UrbanPop<67)
mean(rural_states$Murder, na.rm=TRUE)
#Using the "income" variable of gss2k data frame, create a binary factor called "income2" which consists of two levels: "$25K or More" and "Less than $25K".  Make sure both "DK" and "REFUSED" of the "income" variable are recoded as NAs in "income2".
#Question: What is the median age of the individuals whose income is less than 25K?
gss2k$income2<-factor(gss2k$income, levels=c("25K or More", "Less than 25K"))
summary(gss2k$income)
levels(gss2k$income2)
summary(gss2k$income2)
gss2k$income2[gss2k$income=="$25000 OR MORE"]<- "25K or More"
gss2k$income2[gss2k$income=="$1000 TO 2999" |gss2k$income== "$10000 - 14999"|gss2k$income== "$15000 - 19999"|gss2k$income== "$20000 - 24999" |gss2k$income=="$3000 TO 3999"|gss2k$income=="4000 TO 4999" | gss2k$income== "$5000 TO 5999"|gss2k$income=="$6000 TO 6999"|gss2k$income=="$7000 TO 7999"|gss2k$income=="$8000 TO 9999"|gss2k$income=="LT $1000"]<-"Less than 25K"
gss2k$income2[gss2k$income=="DK"] <- NA
gss2k$income2[gss2k$income=="REFUSED"] <- NA
summary(gss2k$income2)
mean(gss2k$age[gss2k$income2=="Less than 25K"], na.rm = TRUE)
#What is the average age of females whose income is less than 25K?
mean(gss2k$age[gss2k$sex=="MALE"&gss2k$income2=="Less than 25K"], na.rm=TRUE)
#What is the average age of strong republicans who earn less than $25K?
mean(gss2k$age[gss2k$health=="POOR"&gss2k$income2=="Less than 25K"], na.rm=TRUE)
head(gss2k$health)
