library(tidyverse)

table1<-tibble(
  `country`=c("Afghanistan","Brazil","China"),
  `1999`=c(19987071, 172006362, 1272915272),
  `2000`=c(20595360, 174504898, 1280428583)
)
table1

# case table
table2<-tibble(
  `country` = c("Afghanistan","Brazil","China"),
  `1999` = c(745, 37737, 212258),
  `2000` = c(2666, 80488, 213766)
)
table2

table3<- tibble(
  `country` = c("Afghanistan", "Afghanistan", "Afghanistan", "Afghanistan", "Brazil", "Brazil", "Brazil", "Brazil","China", "China","China", "China"),
  `year` = c(1999,1999,2000,2000,1999,1999,2000,2000,1999,1999,2000,2000),
  `type` = c("case", "population","case", "population","case", "population","case", "population","case", "population","case", "population"),
  `count` = c(745, 19987071, 2666, 20595360, 37737, 172006362, 80488, 174504898, 212258, 1272915272, 213766, 1280428583)
)
table3

table4<-tibble(
  `country` = c("Afghanistan", "Afghanistan", "Brazil", "Brazil","China", "China"),
  `year` = c(1999,2000,1999,2000,1999,2000),
  `rate`= c("745/19987071", "2666/20595360", "37737/172006362", "80488/174504898", "212258/1272915272", "213766/1280428583")
)
table4

# population table
tbl1<-tibble(
  `county`=c("DeKalb","Fulton","Cobb"),
  `2010`=c(691961, 920581, 690688),
  `2011`=c(693961, 921581, 691688)
)
tbl1

# veterans table
tbl2<-tibble(
  `county` = c("DeKalb","Fulton","Cobb"),
  `2010` = c(36189, 42448, 41345),
  `2011` = c(36389, 42648, 41545)
)
tbl2

tbl3<- tibble(
  `country` = c("DeKalb", "DeKalb", "DeKalb", "DeKalb", "Fulton", "Fulton", "Fulton", "Fulton","Cobb", "Cobb","Cobb", "Cobb"),
  `year` = c(2010,2010,2011,2011,2010,2010,2011,2011,2010,2010,2011,2011),
  `type` = c("veterans", "population","veterans", "population","veterans", "population","veterans", "population","veterans", "population","veterans", "population"),
  `count` = c(36189, 691961, 36389, 693961, 42448, 920581, 42648, 921581, 41345, 690688, 41545, 691688)
)
tbl3

tbl4<- tibble(
  `country` = c("DeKalb", "DeKalb", "Fulton", "Fulton", "Cobb", "Cobb"),
  `year` = c(2010,2011,2010,2011,2010,2011),
  `prop` = c("36189/691961", "36389/693961", "42448/920581", "42648/921581", "41345/690688", "41545/691688")
)
tbl4

stocks1 <-tibble(
  year = rep(c(2015,2016), each=4),
  qtr = rep(c(1,2,3,4), 2),
  return = c(1.88, 0.59, 0.35, NA, NA, 0.92,0.17, 2.66)
) %>% na.omit()

table1
gather(table1, '1999', '2000', key='year', value='population')

table2
gather(table2, '1999', '2000', key='year', value='cases')

table22<- data.frame(table2)
table22
gather(table22, 'X1999', 'X2000', key='year', value='cases')

tbl1
tbl12<- gather(tbl1, '2010':'2011', key='year', value='pop')
tbl2
tbl22<- gather(tbl2, '2010':'2011', key='year', value='cases')

long_tbl<- full_join(tbl12, tbl22)
paste(long_tbl$county,long_tbl$year, sep='_')

table3
spread(table3, key='type', value='count')

tbl3
spread(tbl3, key='type', value='count')

table4
separate(table4, rate, into=c('cases', 'pop'), sep='/')

tbl4
separate(tbl4, col= 'prop', into=c('var1', 'var2'), sep='/')

tbl44 <- separate(tbl4, col = 'prop', into = c('v1', 'v2'), sep = '/')
tbl44$newvar <- c('1&2&3','2&3','&2&3','1&&3','1&2&3','1&2')
tbl44
separate(tbl44, newvar, into=c('c1', 'c2', 'c3'), sep='&')

table_sep<-separate(table4, year, into=c('century', 'decade'), sep=2)
table_sep

unite(table_sep, col= 'year', century, decade, sep='')

table5
unite(table5, year, century, year, sep='')

df <- tibble(
  group = c(1:2, 1), item_id = c(1:2, 2),
  item_name = c("a", "b", "b"),
  value1 = 1:3, value2 = 4:6
)
df

df %>% complete(group, nesting(item_id, item_name))

stocks1
complete(stocks1, year, qtr)

