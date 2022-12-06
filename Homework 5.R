library(ggplot2)
gss2k<-read.csv("gss2k.csv", stringsAsFactors = TRUE)
#1
summary(gss2k$degree)
tapply(gss2k$sex, gss2k$degree, summary)

#2
ggplot(gss2k, aes(x=degree, y=) + geom_boxplot())
rlang::last_error()
plot(gss2k$degree, main= "Frequency of Degrees", xlab="Degree", ylab="Frequency")
ggplot(gss2k)+ labs(title ="frequency of degrees") + geom_bar(aes(x=degree), na.rm = TRUE)

#3
plot(gss2k$degree, main= "Frequency of Degrees", xlab="Degree", ylab="Frequency") + geom_boxplot(aes(color=sex))
ggplot(gss2k)+geom_bar(aes(x=degree))
?ggplot
ggplot(gss2k)+ labs(title ="frequency of degrees by sex") + geom_bar(aes(x=degree, color=sex))

#4
ggplot(gss2k)+ labs(title ="frequency of degrees across sexes and regions") + geom_bar(aes(x=degree, color=sex))+facet_wrap(~region)

#5
summary(gss2k)
ggplot(gss2k)+ geom_boxplot(aes(x=region, y=age, color=sex)) + labs(title="age and sex vs. region")
#The plot shows information of age ranges for each sex by region by placing region on the x axis and using a box plot to mark the average and distribution of ages. Also, the colors show separation in these ages based on sex. 

                         