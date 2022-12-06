## Lab 2 - QTM 151
 #load tidyverse
library(tidyverse)
install.packages(tidyverse)
library(tidyverse)

#load dataset
PErisk <- read.csv('https://raw.githubusercontent.com/umbertomig/qtm151/main/datasets/PErisk.csv')
tips <- read.csv('https://raw.githubusercontent.com/umbertomig/qtm151/main/datasets/tips.csv')

#plots for continuous variable
#histogram for tips
qplot(tips$tip, geom= 'histogram', bins=20, main= 'Tips', xlab= 'Tips ($)', fill= I('cyan'),color=I('black'))
#density plot for total bill
qplot(tips$totbill, geom='density', main= 'Total Bill', color=I('blue'))
#boxplot of total bill
qplot(y= tips$totbill, geom= 'boxplot', color=I('black'), fill=I('cyan'))

#plots for discrete variables
#bar graph for smokers
qplot(tips$smoker, geom= 'bar', fill=I('cyan'), color=I('black'))
#scatter plot for tips and totbill
qplot(tips$totbill, tips$tip, geom='point', color=I('orange'), xlab='total bill', ylab='tips')+
    geom_rug() + geom_smooth(method = lm)
#scatter with color for smokers
qplot(totbill, tip, geom='point', color=smoker, data=tips, xlab='total bill', ylab='tips')
#with facets
qplot(totbill, tip, geom='point', data=tips, xlab='total bill', ylab='tips', facets=.~smoker)
#boxplot of tips by smokers
qplot(x=smoker, y=tip, data=tips, geom="boxplot", fill=I('cyan') )
