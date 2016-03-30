#一、为图形添加注释
#有时在图形中添加适量的注释，将会使阅读者更快速的了解图形表达的含义。
#ggplot2包中的annotate()函数帮助用户给图形的指定位置添加注释，一般注释可以从点、线和面的角度进行修饰，
#对应的geom参数可以是text/segment/pointrange/rect。下面就举几个例子，让大家看看注释带来的好处。

library(ggplot2)
library(lubridate)    #处理日期时间相关的R包，非常有用，强烈推荐
#SH_GDP <- read.csv(file = file.choose())
Year <- year(seq(from = as.Date('1995-01-01'), to = as.Date('2014-01-01'), by = 'year'))
GDP <- c(2499.43,2957.55,3438.79,3801.09,4188.73,4771.17, 5210.12,5741.03,6694.23,8072.83,9247.66,
10572.24,12494.01,14069.86,15046.45,17165.98,19195.69,20181.72,21818.15,23567.7)
df<- data.frame(Year = Year, GDP = GDP)

#不作任何修饰的折线图
p0 <- ggplot(data = df, mapping = aes(x = Year, y = GDP)) + 
  geom_line(colour = 'blue', size = 1) + 
  geom_point(colour = 'red', size = 2)
#一般统计图形至少包括4个元素，即图形标题、坐标轴标签、图例和数据来源
#可以使用labs()函数或ggtitle()函数为图形添加标题
p1 <- p0 + labs(title = '上海近20年GDP走势')#或者p1 + ggtitle(label = '上海近20年GDP走势')
#使用annotate()函数为图形添加数据来源信息
p2 <- p1 + annotate(geom = 'text', x = 2012, y = 2500, label = '数据来源:中国统计局', colour = 'brown')
#为2008年GDP上升缓慢添加注释:
p3 <- p2 + annotate(geom = 'rect', xmin = 2008, ymin = 12500, xmax = 2009, ymax = 17500, alpha = 0.4) + 
  annotate('segment', x = 2010, y = 10000, xend = 2008.5, yend = 14500, size = 1.2, arrow = arrow()) + 
  annotate('text', x = 2010, y = 10000, label = '受2008年金融危机影响', colour = 'red')
#为了比较前10年与后10年上海GDP总量变化情况，可以添加如下形式的注释：
p4 <- p3 + geom_vline(xintercept = 2004.5, colour = 'orange', size = 1, linetype = 2) + 
  annotate('text', x = 2008, y = 20000, label = '后10年GDP约为前10年GDP总和的3.5倍') 

#虽然上面的图有些画蛇添足的意思，但这其中包含了如下4中注释：
#1）添加阴影矩阵(rect)，框出重点关注区域2）通过带箭头的射线(segment)，指出需要注释的地方
#3）在指定点的位置添加文本型(text)注释内容


#尽管annotate()函数和geom_text()函数都可以问生成的图形添加文本，但其工作原理是有很大差异的。
#annotate()函数只会向图中添加一个单独的文本对象，而geom_text()函数却会根据数据创建许多的文本对象
#（同样是一条文本对象，但数据中有多少点（观测）就会重叠添加文本对象多少次）。例如：
ggplot(data = df, mapping = aes(x = Year, y = GDP)) + 
  geom_line(colour = 'blue', size = 1) + geom_point(colour = 'red', size = 2) + 
  annotate('text', x = 2000, y = 20000, label = '测试annotate注释', alpha = 0.5)
ggplot(data = df, mapping = aes(x = Year, y = GDP)) + 
  geom_line(colour = 'blue', size = 1) + geom_point(colour = 'red', size = 2) +
  geom_text(x = 2000, y = 20000, label = '测试annotate注释', alpha = 0.5)
#注：同样为0.5的透明度，一个浅，一个深，深是因为该文本对象'测试geom_text注释'重叠了20次。

#添加数学表达式，只需两步：
#1）了解R中是如何表示数学函数的，可通过?plotmath查看数学表达式如何在R中显示
#2）annotate()函数将parse设为TRUE即可
#标准正态分布曲线
x <- seq(from = -5, to = 5, length = 1000)
y <- dnorm(x)
ggplot(data = NULL, mapping = aes(x = x, y = y)) + 
  geom_line(colour = 'blue', size = 2) + 
  annotate('text', x = 0, y = 0.1, label = 'f(x) == frac(1,sqrt(2*pi))*e^(-frac(x^2,2))',
           parse = TRUE, size = 5, colour = 'red')

#二、为图形添加参考线
#geom_abline()、geom_hline()和geom_vline()函数可以为图形添加斜线、水平线和垂直线，
#必要时这样的参考线还是值得使用的，例如添加一条平均水平或垂直线。
set.seed(1234)
type <- c('A','B','C','D','E','F','G')
value <- runif(7, min = 10, max = 100)
df <- data.frame(type = type, value = value)

ggplot(data = NULL, mapping = aes(x = type, y = value)) + 
  geom_bar(stat = 'identity', fill = 'steelblue', colour = 'black') + 
  geom_hline(yintercept = mean(df$value), linetype = 2, col = 'red', size = 1) +
  annotate('text', x = 'A', y = mean(df$value) + 3, label = paste('平均值:',round(mean(df$value),2)))

#三、旋转坐标轴
#如果我想让条形图垂直放置（比较建议这样做，因为水平放置一般是跟时间趋势相关的），添加coord_flip()函数
ggplot(data = NULL, mapping = aes(x = reorder(type, value), y = value)) + 
  geom_bar(stat = 'identity', fill = 'steelblue', colour = 'black') + 
  geom_hline(yintercept = mean(df$value), linetype = 2, col = 'red', size = 1) + 
  annotate('text', x = 'A', y = mean(df$value) + 3, label = paste('平均值:',round(mean(df$value),2))) +
  xlab('type') +
  coord_flip()

#四、修改刻度标签的内容
#如果坐标轴标签值非常大，如3000000如何改写为'3M'呢？或如何将千、万、亿值中加入千分位逗号？
library(scales)
set.seed(1234)
x <- runif(1000,min = 1000, max = 10000)
y <- runif(1000, min = 1000000, max = 10000000)
#坐标轴标签不加任何处理
ggplot(data = NULL, mapping = aes(x = x, y = y)) + geom_point()
#注意，使用comma函数时，必须先加载scales包。
#将x轴处理成含千分位逗号的格式，将y轴处理为M格式的
ggplot(data = NULL, mapping = aes(x = x, y = y)) + geom_point() + 
  scale_x_continuous(breaks = seq(2500, 10000, 2500), labels = comma(seq(2500, 10000, 2500))) + 
  scale_y_continuous(breaks = seq(2.5e+06, 1.0e+07, 2.5e+06), 
                     labels = paste(seq(2.5, 10, 2.5), 'M', sep = ''))

#有时会遇到一种尴尬的情况：类别型变量水平的名称太长，导致刻度标签显示非常难看，该如何解决呢？
set.seed(1234)
level_labels <- c('good morning', 'the weather is good', 'this is my teacher', 'my name is snake', 
                  'what is your name', 'r language is easy to learn')
type <- sample(level_labels, size = 1000, replace = TRUE)
values <- c(rnorm(250), rt(250,3), rf(250,2,3), runif(250,3,5))
#不作任何调整的刻度标签
ggplot(data = NULL, mapping = aes(x = type, y = values)) + geom_boxplot()
#对于这种情况，可以选择刻度标签的角度，使其看起来比较舒服
ggplot(data = NULL, mapping = aes(x = type, y = values)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 30, vjust = 0.8))


#五、修改坐标轴标签的内容
#默认情况下，坐标轴标签会根据ggplot()中aes()属性值自动为坐标轴附上标签，
#可以通过labs()函数、xlab()函数、ylab()函数、scale_x_*函数、scale_y_*函数和theme()函数实现。
#这里重点讲一下scale_x_*函数、scale_y_*函数和theme()函数的搭配使用，该函数非常的灵活，可以对坐标轴标签、刻度标签和刻度标记等进行修饰。
ggplot(data = NULL, mapping = aes(x = type, y = values)) + geom_boxplot() + 
  scale_x_discrete(name = 'Long level') + 
  theme(axis.text.x = element_text(angle = 30, vjust = .8, colour = 'red', size = 10), 
        axis.title.x = element_text(colour = 'blue', size = 16))


#六、修改图形标题的外观
#讲完了图形注释、坐标轴和刻度的修饰，最后再来讲讲如何修改图形标题的外观，这里仍然使用的是them()函数，可见该函数有多强大了。
ggplot(data = df, mapping = aes(x = Year, y = GDP)) + 
  geom_line(colour = 'blue', size = 1) + 
  geom_point(colour = 'red', size = 2) + 
  labs(title = '上海近20年GDP走势') + 
  theme(plot.title = element_text(colour = 'brown', size = 20, face = 'bold'))






















