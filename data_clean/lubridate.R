#lubridate:日期时间处理的包
#is.Date(x)，该函数用来判断对象是否为日期型数据。
#now()，返回系统的日期时间
#Sys.Date()，这是base包中的函数，返回系统日期
#parse_date_time()这是一个重磅函数，可以将格式各样的日期时间字符转换为日期时间类型的数据，
 #其中函数中有一个重要的参数，即orders，通过该参数指定可能的日期格式顺序，如年-月-日或月-日-年等顺序。
library(lubridate)
x <- c('20131113','120315','12/17/1996','09-01-01','2015 12 23','2009-1, 5','Created on 2013 4 6')
parse_date_time(x, orders = c('Ymd','mdy','dmY','ymd'))

round_date()floor_date()ceiling_date()
#这三个函数实际上是截断函数，即将日期或日期时间型数据取整到不同的单位，如年、季、月、日、时等。
#区别在于第一个是四舍五入取整，第二个是向下取整，第三个是向上取整。
x <- as.POSIXct("2016-03-16 12:34:59")
#四舍五入取整
round_date(x,'hour')
#向下取整
floor_date(x,'hour')
#向上取整
ceiling_date(x,'hour')

year()quarter()month()week()day()wday()hour()minute()second()
#这组函数可以返回所属日期的年、月、日、时、分、秒等

days_in_month()
#返回所属月份的最大天数
x <- as.Date('2015-01-01')
y <- x + months(0:11)
days_in_month(y)

%m + %
#由于每个月的最后天数不一样，如果直接在某个月份的最后一天加上指定的月数就会出现错误，这时就得考虑使用%m+%函数了
x <- as.Date('2015-01-31')
y <- x + months(0:11)
y2 <- x %m+% months(0:11)

time_length()
#该函数可以非常方便的计算两个日期之间的间隔，包括年、月、日、时、分、秒的间隔。
time1 <- ymd_hms('1989-07-17 17:33:21')
time2 <- now()
#设置两个时间的区间
time_interval <- interval(time1,time2)
time_length(time_interval,'year')









