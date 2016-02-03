#一、绘制单条折线图：
library(ggplot2)
#有关时间序列的折线图
library(lubridate)    #处理日期时间相关的R包，非常有用，强烈推荐
Year <- year(seq(from = as.Date('2006-01-01'), to = as.Date('2015-01-01'), by = 'year'))
Weight <- c(23,35,43,57,60,62,63,66,61,62)
df <- data.frame(Year = Year, Weight = Weight)
ggplot(data = df, mapping = aes(x = factor(Year), y = Weight, group = 1)) + geom_line() + xlab('Year')

#有关离散变量的折线图
type <- c('A','B','C','D','E')
quanlity <- c(1,1.1,2.1,1.5,1.7)
df <- data.frame(type = type, quanlity = quanlity)
ggplot(data = df, mapping = aes(x = type, y = quanlity, group = 1)) + geom_line()

#有关连续变量的折线图
set.seed(1234)
times <- 1:15
value <- runif(15,min = 5,max = 15)
df <- data.frame(times = times, value = value)
ggplot(data = df, mapping = aes(x = times, y = value)) + geom_line()

#善于发现的你，可能会注意到上面三段代码有一个重要的不同之处，那就是第一段和第二段代码中含有‘group = 1’的设置。
#这样做是因为横坐标的属性设置为了因子，即将连续型的年份和离散型的字符转换为因子，如果不添加‘group = 1’这样的条件，
#绘图将会报错。故务必需要记住这里的易犯错误的点！

#往折线图中添加标记（点）
#当数据点密度比较小或采集分布(间隔)不均匀时，为折线图做上标记将会产生非常好的效果。处理的方法非常简单，
#只需在折线图的基础上再加上geom_point()函数即可。
set.seed(1234)
year <- c(1990,1995,2000,2003,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015)
value <- runif(15, min = 10, max = 50)
df <- data.frame(year = year, value = vlaue)
ggplot(data = df, mapping = aes(x = year, y = value)) + geom_line() + geom_point()




#二、绘制多条折线图
#上面绘制的都是单条这折线图，对于两个或两个以上的折线图该如何绘制呢？
#也很简单，只需将其他离散变量赋给诸如colour(线条颜色)和linetype(线条形状)的属性即可，具体参见下文例子。

#基于颜色的多条折线图
set.seed(1234)
year <- rep(1990:2015, times = 2)
type <- rep(c('A','B'),each = 26)
value <- c(runif(26),runif(26, min = 1,max = 1.5))
df <- data.frame(year = year, type = type, value = value)
ggplot(data = df, mapping = aes(x = year, y = value, colour = type)) + geom_line()
#基于形状的多条折线图
ggplot(data = df, mapping = aes(x = year, y = value, linetype = type)) + geom_line()
#注：同样需要注意的是，在绘制多条折线图时，如果横坐标为因子，必须还得加上‘group=分组变量’的参数，否则报错或绘制出错误的图形。



#以上绘制的折线图，均采用默认格式，不论是颜色、形状、大小还是透明度，均没有给出自定义的格式。其实ggplot2包也是允许用户根据自己的想法设置这些属性的。
#自定义线条或点的颜色--scale_color_manual()
#自定义线条类型--scale_linetype_manual()
#自定义点的形状--scale_shape_manual()
#自定义点的大小或线条的宽度--scale_size_manual()
#自定义透明度--scale_alpha_manual()
ggplot(data = df, mapping = aes(x = year, y = value, linetype = type, colour = type, 
                                shape = type, fill = type)) + geom_line() + geom_point()+ 
  scale_linetype_manual(values = c(1,2)) + 
  scale_color_manual(values = c('steelblue','darkred')) + 
  scale_shape_manual(values = c(21,23)) + 
  scale_fill_manual(values = c('red','black'))


#三、绘制堆积面积图
#绘制堆叠的面积图只需要geom_area()函数再加上一个离散变量映射到fill就可以轻松实现，先忙咱小试牛刀一下。
set.seed(1234)
year <- rep(1990:2015, times = 2)
type <- rep(c('A','B'),each = 26)
value <- c(runif(26),runif(26, min = 1,max = 1.5))
df <- data.frame(year = year, type = type, value = value)
ggplot(data = df, mapping = aes(x = year, y = value, fill = type)) + geom_area()
#堆叠的顺序与图例的顺序相同，加入guides()函数
ggplot(data = df, mapping = aes(x = year, y = value, fill = type)) + geom_area() + 
  guides(fill = guide_legend(reverse = TRUE))
#为每一块面积图的顶部加上一条直线
ggplot(data = df, mapping = aes(x = year, y = value, fill = type)) + 
  geom_area(colour = 'black', size =1, alpha = 0.7) + 
  guides(fill = guide_legend(reverse = TRUE))
#其中，colour设置面积图边框的颜色；size设置边框线的粗细；alpha设置面积图和边框线的透明度。

ggplot(data = df, mapping = aes(x = year, y = value, fill = type)) + 
  geom_area(alpha = 0.6) + 
  geom_line(colour = 'black', size = 1, position = 'stack', alpha = 0.6) + 
  guides(fill = guide_legend(reverse = TRUE))
#该方法是通过添加堆叠线条（必须设置geom_line()中position参数为‘stack’，否则只是添加了两条线，无法与面积图的顶部重合）。
#这两幅图的区别在于第二种方式没有绘制面积图左右边框和底边框。在实际应用中，建议不要在面积图中绘制边框线，因为边框的存在可能产生误导

#四、绘制百分比堆积面积图
set.seed(1234)
year <- rep(1990:2015, times = 4)
type <- rep(c('A','B','C','D'),each = 26)
value <- c(runif(26),runif(26, min = 1,max = 1.5), runif(26, min = 1.5,max = 2), 
           runif(26, min = 2,max = 2.5))
df <- data.frame(year = year, type = type, value = value)
ggplot(data = df, mapping = aes(x = year, y = value, fill = type)) + 
  geom_area(position = 'fill', alpha = 0.6) + 
  guides(fill = guide_legend(reverse = TRUE))
#但通过这种方式(（设置面积图的positon='fill'）存在一点点小缺陷，即无法绘制出百分比堆积面积图顶部的线条，
#该如何实现呢？这里只需要对原始数据集做一步汇总工作，让后按部就班的绘制面积图即可

library(dplyr)
df_by_type <- group_by(.data = df, year)
df_summarize <- mutate(.data = df_by_type, value2 = value/sum(value))
ggplot(data = df_summarize, mapping = aes(x = year, y = value2, fill = type)) + 
  geom_area(alpha = 0.6) + 
  geom_line(colour = 'black', size = 1, position = 'stack', alpha = 0.6) + 
  guides(fill = guide_legend(reverse = TRUE))