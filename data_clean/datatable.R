#在正式介绍data.table包之前，这里先模拟生成1000W行和7列的数据集，产生的csv文件有817M。脚本如下：
set.seed(1234)
x1 <- 1:10000000
x2 <- rnorm(10000000)
x3 <- rt(10000000,2)
x4 <- rf(10000000,2,3)
x5 <- rpois(10000000,5)
x6 <- sample(letters,10000000,replace = TRUE)
x7 <- rchisq(10000000,3)
my.df <- data.frame(x1 = x1, x2 = x2, x3 = x3, 
                    x4 = x4, x5 = x5, x6 = x6,
                    x7 = x7)
write.csv(my.df, 'test.csv', row.names = FALSE)

#一、读取数据
#通过使用data.table包中的fread()函数可以快速的读取大数据集
#案例对比：
#使用read.table()函数读取
system.time(read.table(file = file.choose(), 
                       head = TRUE, sep = ','))
#使用read.csv()函数读取
system.time(read.csv(file = file.choose(), 
                     head = TRUE, sep = ','))
#使用data.table包中的fread()函数读取
library(data.table)
system.time(fread(input = file.choose()))
#从中可以看出，fread()函数在读取大文本文件时，速度上有显著的提升，要比read.table()快了近6倍，比read.csv()也快了近5倍。


#二、数据排重
#data.table包中与数据排重相关的有两个重要函数：
#1）duplicated()函数返回数据行是否有重复的逻辑值，TRUE表示某行重复，FALSE表示某行不存在重复
#2）unique()函数返回所有唯一的数据行
#这两个函数可以灵活的指定某些列是否重复。
#案例对比：排除数据集my.df中x5和x6两列的重复值
df <- my.df
dt <- data.table(my.df)
library(sqldf)
system.time(sqldf('select distinct x5,x6 from df'))
library(data.table)
system.time(unique(dt[,list(x5,x6)], by = c('x5','x6')))
#同样的排重目的，read.table包中的unique函数比SQL快了近70倍！


#三、取子集
#提取数据集的子集一般使用subset函数，但函数的参数x为数据框或data.table格式的数据时，其运算速度是存在差异的。
#案例对比：
system.time(subset(x = df, x2 >= 1 & x3 <= 0.5 & x6 == 'c' | x7 > 1.2))
system.time(subset(x = dt, x2 >= 1 & x3 <= 0.5 & x6 == 'c' | x7 > 1.2))
#对1000W行的数据集，差异虽然不是很明显，但如果有更大的数据集的话，这种差异一定会显得非常明显。


#四、排序
#排序操作经常会碰到，但是数据量一大的话，性能往往会下降很多，如果使用data.table包中的setorder函数的话，速度会有一个明显的提升。
#案例对比：
library(plyr)
system.time(arrange(my.df, x2, desc(x3), x6))
library(data.table)
system.time(setorder(dt, x2, -x3, x6))
#通过使用plyr包中的arrange()函数和data.table包中的setorder()函数对比，发现后者比前者的速度快了123倍！



#五、分组计算
#一般对于明细数据都需要聚合操作，下面比较两种方式，就可以得知read.table包的速度了。
#案例对比：
library(sqldf)
system.time(sqldf('select x6, avg(x1) A, min(x2) B, 
                  max(x3) C ,max(x5)-min(x5) D
                  from df group by x6'))

library(data.table)
system.time(dt[, list(A = mean(x1), B = min(x2), C = max(x3),
                      D = diff(range(x5))), by = x6])
#为达到同样的聚合效果，data.table包所显示的速度是另一种方法的72倍！

#通过以上几个案例的对比分析，我们可以看见read.table包中的函数应用比较简单（代码较少）而且运行速度也是非常快速的。
#除此之外，该包中还包括重命名变量名函数setnames()、数据整形函数melt()和dcast()、数据合并函数merge()和rbind()、rbindlist()等。
#详细信息可以查看相应的帮助文件。