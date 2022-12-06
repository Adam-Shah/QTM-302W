library(tidyverse)
library(lubridate)
help(package = lubridate)

dt <- '2021-02-22'
class(dt)
str(dt)
today() - dt  #error because dt is a character, not date

dt <- as.Date(dt)
class(dt)
str(dt)
today()- dt

x <- 'May 26, 2020'
str(x)

str(mdy(x))

x <- "2010 September 20th"
ymd(x)
y <- "02.01.2010"
dmy(y)
z <- "Sep, 12th 2010 14:00"
mdy_hm(z)
a <- "2016-07-08 12:34:56"
ymd_hms(a)

b<-'2010-01-22'
ymd(b)
str(ymd(b))

c<- "12-02-1234 12:30:25"
str(mdy_hms(c))

d<- "May 18, 1933"
str(mdy(d))

dmy("1st June 2012")
str(dmy(e))

x<- c('May 18, 1933', '1st June 2012')
x
parse_date_time(x, order = c('dmy', 'mdy'))

x <- c("April 13, 2003", "17 April 2005")
y <- "January 10, 2020 at 23:30:35"

parse_date_time(x, order=c('mdy', 'dmy'))
parse_date_time(y, order='mdyHMS')
?parse_date_time #to check arguments in order

make_date(year = 2012, month = 3, day = 27)
make_datetime(year = 1234, month = 5, day = 12, 
              hour = 1, min = 23, sec = 45)

yrs <- c(1234,1222,2020)
mos <- c(4,2,12)
dys <- c(12,12,20)

make_date(year = yrs, month= mos, day= dys)

x<- c("2010-01-22", "12-02-1234 12:30:25")
x
y<-parse_date_time(x, order=c('ymd', 'mdyHMS'))

year(y)
month(y)
hour(y)
quarter(y)
wday(y)

x<-mdy_hms("12-02-1234 12:30:25")
round_date(x, unit='minutes')
round_date(x, unit='hours')
floor_date(x, unit='hours')
ceiling_date(x, unit='days')

d <- ymd_hms("1234-04-03 07:13:28 UTC")
floor_date(d, unit = 'day')
# Round to nearest 5 minutes
round_date(d, unit = '5 minutes')
# Round up to week 
ceiling_date(d, unit = 'week')
# Fun
ceiling_date(d, unit = 'hour') - floor_date(d, unit = 'hour')

d <- ymd_hms("1981-08-18 18:08:28 UTC")
round_date(d, unit='15 minutes')
floor_date(d, unit='weekly')
ceiling_date(d, unit='year')

difftime(today(), mdy("May 20, 2020"), units = 'days')
difftime(now(), mdy_hms("May 20, 2020 22:12:22"), units = 'secs')

#How many seconds I've been alive
difftime(now(), mdy_hms("May 1, 2001 01:30:00"), units='secs')


today() - years(1)
today() + month(1)
days(2)
ddays(2)
now() + seconds(20)
2*days()

x <- dmy("2 January 1998") %--% dmy("30 March 2018")
y <- dmy("2 January 2017") %--% dmy("30 March 2020")
int_start(x)
int_end(x)
int_length(x)
as.period(x)
dmy("5 January 1998") %within% x
dmy("5 January 1998") %within% y
int_overlaps(x, y)

b<- dmy('1st May 2001') %--% today()
int_length(b)        
as.period(b)
mdy('January 20, 2000') %within% b
