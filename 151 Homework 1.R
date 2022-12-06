## Homework 1

#1
library(tidyverse)
data(diamonds)

dsmall<- diamonds %>% sample_n(1000)

#2
qplot(diamonds$carat, geom= 'histogram', 
      binwidth=0.1, main= 'Carat', xlab= 'Carat', 
      fill= I('cyan'),color=I('black'), xlim=c(0,3))

#3
qplot(carat, data= diamonds, geom= 'density', 
      main= 'Carat', xlab= 'Carat', 
      color=color, xlim=c(0,3))

#4
qplot(carat, price, geom="point", data=diamonds, 
      main='Price by Carat', xlab='Carat', ylab='Price',
      color=cut, alpha=0.1)
## The trend of the scatter plot is that diamonds of a higher
  ##carat tend to be more expensive.

#5
qplot(carat, price, geom="point", data=diamonds, 
      main='Price by Carat', xlab='Carat', ylab='Price',
      color=cut, alpha='0.1') + scale_x_log10() + scale_y_log10() + 
      geom_jitter() + geom_smooth(method='lm')
## The trend is that there is a positive linear relationship between 
  ## the log of price and carat variables.
