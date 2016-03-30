#ggplot2并没有类似于geom_pie()这样的函数实现饼图的绘制，但ggplot2有一个理念，就是通过极坐标变换绘制饼图
library(ggplot2)
type <- c('A','B','C','D','E','F','G')
nums <- c(10,23,8,33,12,40,60)
df <- data.frame(type = type, nums = nums)

#绘制条形图
p <- ggplot(data = df, mapping = aes(x = 'Content', y = nums, fill = type)) + 
  geom_bar(stat = 'identity', position = 'stack')

#堆叠的条形图绘制完后，接下来就需要进行极坐标变换了，ggplot2中coord_polar()函数可以非常方便的实现极坐标变换。
p + coord_polar(theta = 'y')
#注：此时饼图中间有一个空心圆，只需将原条形图的宽度设置为1。

#绘制条形宽度为1的条形图
p <- ggplot(data = df, mapping = aes(x = 'Content', y = nums, fill = type)) + 
  geom_bar(stat = 'identity', position = 'stack', width = 1)#两边的空隙不见了
#绘制无空心点的饼图
p + coord_polar(theta = 'y')
#通过labs()函数清除坐标轴的标签。
p + coord_polar(theta = 'y') + labs(x = '', y = '', title = '')
#可以通过theme()的方法清除掉x轴和y轴的刻度值及刻度标记。
p + coord_polar(theta = 'y') + labs(x = '', y = '', title = '') + 
  theme(axis.text = element_blank())+ theme(axis.ticks = element_blank())

#添加百分比
#第一种方法，将百分比直接显示在图例中，这种方式适合分类较多的情况。
#百分比（10.2%）表示出来
label_value <- paste('(', round(df$nums/sum(df$nums) * 100, 1), '%)', sep = '')
#为这些百分比值对应到各个组。
label <- paste(df$type, label_value, sep = '')
#接下来就是将这些百分比标签放到图例中。
p + coord_polar(theta = 'y') + 
  labs(x = '', y = '', title = '') + 
  theme(axis.text = element_blank()) + 
  theme(axis.ticks = element_blank()) + 
  scale_fill_discrete(labels = label)
                                                                                                                                                                 
                                                                                                                                                                 
#第二种方法，直接将百分比放到各自的饼区中。
#首先去掉图例
p + coord_polar(theta = 'y') + labs(x = '', y = '', title = '') + 
  theme(axis.text = element_blank()) + theme(axis.ticks = element_blank()) + 
  theme(legend.position = "none") 
#通过计算极坐标中x和y轴的位置，将标签贴在相应的地方。
p + coord_polar(theta = 'y') + labs(x = '', y = '', title = '') + 
  theme(axis.text = element_blank()) + theme(axis.ticks = element_blank()) +
  theme(legend.position = "none") + 
  geom_text(aes(y = df$nums/2 + c(0, cumsum(df$nums)[-length(df$nums)]), 
                x = sum(df$nums)/150, label = label)) 

#注：x = sum(df$nums)/150是需要不断调整的，分母越小，标签离饼图越远，分母越大，标签月挤到一起。
p + coord_polar(theta = 'y') + labs(x = '', y = '', title = '') + 
  theme(axis.text = element_blank()) + theme(axis.ticks = element_blank()) + 
  theme(legend.position = "none") + 
  geom_text(aes(y = df$nums/2 + c(0, cumsum(df$nums)[-length(df$nums)]),
                x = sum(df$nums)/50, label = label)) 


#绘制环形图，只需调整原条形图的宽度width
ggplot(data = df, mapping = aes(x = 'Content', y = nums, fill = type)) + 
  geom_bar(stat = 'identity', position = 'stack', width = 0.5) + coord_polar(theta = 'y') +
  labs(x = '', y = '', title = '') + theme(axis.text = element_blank()) + 
  theme(axis.ticks = element_blank()) + theme(legend.position = "none") + 
  geom_text(aes(y = df$nums/2 + c(0, cumsum(df$nums)[-length(df$nums)]), 
                x = sum(df$nums)/180, label = label))

#参考资料：http://www.bubuko.com/infodetail-1036150.html,刘顺祥:每天进步一点点2015 







