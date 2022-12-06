gss<-read.csv("gss.csv")
set.seed(1111)
gss1001<-gss[sample(gss,1001)]
gss[1:10,c("income","happy","age")]
str(gss)
sum(is.na(gss$income))
gss1001_NAincome <- gss[is.na(gss$income),]
gss1001 <- gss[sample(nrow(gss),1001),]
str(gss1001)
gss1001[1:10, c("income","happy","age")]
gss1001[c(1:10), c(2:4)]
sum(!is.na(gss1001$income))
gss1001_NAincome <- gss1001[is.na(gss1001$income),]
mean(gss1001$age[gss1001$sex=="FEMALE"], na.rm=TRUE)
gss1001_middleaged_1<-subset(gss1001, subset=age>=45)
summary(gss1001_middleaged_1$age)
gss1001_middleaged<-subset(gss1001_middleaged_1, subset=age<65)
summary(gss1001_middleaged$age)

happy_factor<-factor(gss1001[,"happy"], ordered=TRUE, levels=c("NOT TOO HAPPY", "PRETTY HAPPY", "VERY HAPPY")
gss
head(gss1001$happy)

happy<-order(gss1001$happy)
gss1001[happy,]
gss1001$happy
