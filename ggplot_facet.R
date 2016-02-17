#通过facet_grid()和facet_wrap()函数将分组数据横向或纵向或横纵向排列
library(ggplot2)
#创建模拟数据集
set.seed(1234)
M <- c('一月','二月','三月','四月','五月','六月','七月','八月','九月','十月','十一月','十二月')
Month <- rep(M, each = 5)
Region <- rep(c('East','South','West','North','Center'), times = 12)
Amount <- round(runif(n = 60, min = 500, max = 5000))
df <- data.frame(Month = Month, Region = Region, Amount = Amount)

#facet_grid()函数绘制横向或纵向分面图
#绘制横向的分面图
ggplot(data = df, mapping = aes(x = Region, y = Amount, fill = Region)) + 
  geom_bar(stat = 'identity') + facet_grid(. ~ Month) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
#绘制纵向的分面图
ggplot(data = df, mapping = aes(x = Region, y = Amount, fill = Region)) + 
  geom_bar(stat = 'identity') + facet_grid(Month ~ .) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
#重新构建一组数据以绘制纵向的分面图：
set.seed(1234)
Year <- rep(seq(from = 2001, to = 2015),times = 4)
Type <- rep(c('A','B','C','D'), each = 15)
Value <- round(runif(60, min = 10, max = 100))
df2 <- data.frame(Year = Year, Type = Type, Value = Value)
#绘制纵向的分面图
ggplot(data = df2, mapping = aes(x = factor(Year), y = Value, group = 1)) + 
  geom_line(colour = 'blue') + xlab('Year') + facet_grid(Type ~ .)


#使用facet_wrap()函数绘制分面图：
#绘制横向的分面图
ggplot(data = df, mapping = aes(x = Region, y = Amount, fill = Region)) + 
  geom_bar(stat = 'identity') + facet_wrap(~ Month) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
#注：facet_wrap()函数的语法不能写成.~ Month格式
#使用facet_wrap()函数不能使用Month ~语法，只能是类似于~a + b或('a','b')的形式。
#facet_grid()函数和facet_wrap()函数的区别如下：
#1）facet_grid()函数会严格按照用户指定的方向分面，即横向分面必须是.~x的格式，纵向分面必须是y.~的格式，当然也可以y~x表示纵横两个维度的方向进行分面绘图；facet_wrap()函数不存在横向或纵向或横纵向的分面，其实他就像按照从左到右，从上到下的顺序摆放每一个分面图。
#2）语法上有显著的区别，facet_grid()函数必须是y~.或.~x或y~x的格式，而facet_wrap()函数只能是~x的格式，与之等价的是加引号的分面变量名称，即'x'。
#3）其实facet_wrap()函数可以自由排版分面行方向的个数和列方向的个数，通过nrow=和ncol=参数实现，而facet_grid()函数只能是一根筋的下来，即要么全在行方向上，要么全在列方向上，要么就在组合方向上。

#例子：
ggplot(data = df, mapping = aes(x = Region, y = Amount, fill = Region)) + 
  geom_bar(stat = 'identity') + facet_wrap(~Month, ncol = 3) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

#定义字符变量的因子顺序
df$Month <- factor(df$Month, levels = c('一月','二月','三月','四月','五月','六月','七月','八月','九月','十月','十一月','十二月'))
#通过facet_wrap()函数绘制分面图
ggplot(data = df, mapping = aes(x = Region, y = Amount, fill = Region)) + 
  geom_bar(stat = 'identity') + facet_wrap(~Month, ncol = 3) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
ggplot(data = df, mapping = aes(x = Region, y = Amount, fill = Region)) + 
  geom_bar(stat = 'identity') + facet_grid(. ~ Month) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#二、分面图的微调
#关于分面图的微调，这里就说明三项常用的微调手段，即：
#非固定的坐标轴
set.seed(1234)
height <- c(runif(100, min = 60, max = 195), runif(80, min = 45, max = 175))
weight <- 1.2*height + rnorm(180, mean = 10, sd = 20)
sex = rep(c('M','F'), times = c(100, 80))
df3 <- data.frame(sex = sex, height = height, weight = weight)
ggplot(data = df3, mapping = aes(x = height, y = weight)) + geom_point(colour = 'blue', size = 3) + 
  facet_grid(.~sex)
#默认情况下，分面图的纵坐标和横坐标的范围是一致的，可加入scales = 'free_x'和scales = 'free_y'
#注：横向分面只能控制各自的x轴是否自由设定，纵向分面只能控制各自的y轴是否自由设定，纵横交错的分面可以同时设定两轴是否自由。
ggplot(data = df3, mapping = aes(x = height, y = weight)) + 
  geom_point(colour = 'blue', size = 3) + facet_grid(.~sex, scales = 'free_x')

#修改分面的文本标签，只需将设置labeller = label_both就可以实现功能
ggplot(data = df3, mapping = aes(x = height, y = weight)) + 
  geom_point(colour = 'blue', size = 3) + facet_grid(.~sex, labeller = label_both)

levels(df3$sex)[levels(df3$sex)=='F'] <- 'Female'
levels(df3$sex)[levels(df3$sex)=='M'] <- 'Male'
ggplot(data = df3, mapping = aes(x = height, y = weight)) + 
  geom_point(colour = 'blue', size = 3) + facet_grid(.~sex, labeller = label_both)

#修改分面标签外观。通过主题theme()函数实现
ggplot(data = df3, mapping = aes(x = height, y = weight)) + 
  geom_point(colour = 'blue', size = 3) + facet_grid(.~sex, labeller = label_both) + 
  theme(strip.text = element_text(colour = 'red', face = 'bold', size = rel(1.5)), 
        strip.background = element_rect(fill = 'white', colour = 'brown', size = rel(2), linetype = 2))
#注：参数strip.text设置分面标签的颜色、大小、字体等；参数strip.background设置分面标签背景的填充色、线框色、线型等；rel()设置字体大小或线宽为原主题的倍数。

#参考资料：https://yunpan.cn/cr4KAGsPhG8fS  访问密码 2c75；R语言_ggplot2：数据分析与图形艺术；R数据可视化手册





