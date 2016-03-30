#plyr包最让我兴奋的地方是可以方便的实现数据结构之间的转换
#ply结尾，前面两个字母代表“输入+输出数据结构的首字母”

#aaply,adply,alply,a_ply
#daply,ddply,dlply,d_ply
#laply,ldply,llply,l_ply

#a*ply函数格式
aaply(.data = ,.margins = ,.fun = ,...,.progress = 'none',.inform = FALSE)#输入数组，输出数组
adply(.data = ,.margins = ,.fun = ,...,.progress = 'none',.inform = FALSE)#输入数组，输出数据框
alply(.data = ,.margins = ,.fun = ,...,.progress = 'none',.inform = FALSE)#输入数组，输出列表
a_ply(.data = ,.margins = ,.fun = ,...,.progress = 'none',.inform = FALSE)#输入数组，无返回结果
#.data可以是数组也可以是矩阵；
#.margins指定要分析的数组或矩阵的维度，即行维(.margins = 1)还是列维(.margins = 2)；
#.fun为行维或列维指定需要处理的函数，可以是R自带的函数，如sum()，mean()等，也可以是自定义函数；
#...为指定函数的其他参数；
#.progress指定以什么样的方式展示程序运行的进度，默认不显示进度，还可以选择"text"(文本进度条), "tk"(tk进度条), 和"win"(Windows系统自带的进度条)；
#.inform是否指定报错信息，默认不指定，因为设为TRUE的话，将降低程序的执行效率，但该参数对Bug处理是有帮助的。

library(plyr)
a <- array(data=1:500000,dim=c(100000,5))
#对每一行求均值，不显示进度条
test1 <- aaply(.data = a,.margins = 1,.fun = mean, .progress = 'none')
head(test1)
#对每一行求标准差，以文本形式显示进度条
test2 <- adply(.data = a,.margins = 1,.fun = sd, .progress = 'text')
head(test2)
#对每一列求和，以tk形式显示进度条
a2 <- array(rnorm(10000000), dim = c(1000,10000))
test3 <- alply(.data = a2,.margins = 2,.fun = sum, .progress = 'tk')
#对每一列求最大值，以Windows自带进度条显示进度
a3 <- array(rnorm(10000000), dim = c(100,100000))
test4 <- a_ply(.data = a3,.margins = 2,.fun = max, .progress = 'win')


#d*ply函数格式
daply(.data = ,.variables = ,.fun = ,...,.progress = 'none',.inform = FALSE)
ddply(.data = ,.variables = ,.fun = ,...,.progress = 'none',.inform = FALSE)
dlply(.data = ,.variables = ,.fun = ,...,.progress = 'none',.inform = FALSE)
d_ply(.data = ,.variables = ,.fun = ,...,.progress = 'none',.inform = FALSE)
#.data指定为数据框数据；
#.variables指定数据框中的分组变量；
#.fun基于分组变量，可对数据框中的其余变量指定某种函数，可以是R自带的函数，如sum()，mean()等，也可以是自定义函数，类似于聚合分析；
#.progress和.inform与a*ply函数参数一致。

#例子：
#构建自定义函数
fun <- function(data) apply(data,2,mean)

daply(.data = iris[,1:4],.variables = .(iris$Species),.fun=fun)
ddply(.data = iris[,1:4],.variables = .(iris$Species),.fun=fun)
dlply(.data = iris[,1:4],.variables = .(iris$Species),.fun=fun)
d_ply(.data = iris[,1:4],.variables = .(iris$Species),.fun=fun)



#l*ply函数格式
laply(.data = ,.fun = ,...,.progress = 'none',.inform = FALSE)
ldply(.data = ,.fun = ,...,.progress = 'none',.inform = FALSE)
llply(.data = ,.fun = ,...,.progress = 'none',.inform = FALSE)
l_ply(.data = ,.fun = ,...,.progress = 'none',.inform = FALSE)
#.data可以指定为列表数据，也可以指定为向量数据；
#其余参数与a*ply()函数和d*lpy()函数参数一致。

#例子：
x1 <- 1:100
x2 <- seq(from = 100,to = 1000 ,by = 2)
x3 <- runif(150,min = 10,max = 100)
#列表由向量构成
l1 <- list(x1 = x1,x2 = x2,x3 = x3)

laply(.data = l1,.fun = mean)
ldply(.data = l1,.fun = summary)
llply(.data = l1,.fun = quantile)
l_ply(.data = l1,.fun = summary)

#构建数据框d11
y11 <- rnorm(n = 100,mean = 10,sd = 5)
y12 <- rt(n = 100,df = 3)
y13 <- rf(n = 100,df1 = 2,df2 = 3)
y14 <- factor(x = c('low','potential','high'),ordered = T)
y15 <- sample(y14,size = 100,replace = TRUE)
d11 <- data.frame(y1 = y11,y2 = y12,y3 = y13,y5 = y15)
head(d11)

#构建数据框d21
y21 <- 1:100
y22 <- seq(from = 1,to = 2,length = 100)
y23 <- rchisq(n = 100,df = 8)
y24 <- factor(x = c('A','B','C','D'),ordered = T)
y25 <- sample(y24,size = 100,replace = TRUE)
d21 <- data.frame(y21 = y21,y22 = y22,y23 = y23,y25 = y25)
head(d21)

#列表由数据框组成
l2 <- list(first = d11,second = d21)

library(psych)
fun <- function(data) describeBy(data[,1:3],group = data[,4])
llply(.data = l2,.fun = fun,.progress = 'none')
llply(.data = l2,.fun = fun,.progress = 'text')

#plyr包中还有很多其他函数，如rename()、arrange()、join()等函数，
#其功能与dplyr包中的rename()、arrange()、left_join()一致