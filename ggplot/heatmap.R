#热力图是将两个变量（一般是离散变量）的交叉汇总信息以颜色的形式展现出来，而映射给颜色变量的是连续型数值变量。
#热力图可以通过stats包的heatmap()函数绘制，也可以通过ggplot2包中的geom_tile()函数或geom_raster()函数绘制
#模拟数据集
set.seed(123)
Year <- rep(2006:2015, each = 4)
Quater <- rep(c('Q1','Q2','Q3','Q4'), times = 10)
Counts <- round(runif(40, min = 10, max = 200))
df <- data.frame(Year = Year, Quater = Quater, Counts = Counts)


library(ggplot2)
#使用geom_tile()函数
ggplot(data = df, mapping = aes(x = Year, y = Quater, fill = Counts)) + geom_tile()
#或者使用geom_raster()函数
ggplot(data = df, mapping = aes(x = Year, y = Quater, fill = Counts)) + geom_raster()
#注：两者绘制的结果区别不大，一般geom_raster()效率更高，且更适合打印

#question：
#1）横坐标年出现小数 2）一般认为颜色越深代表的值越大，这里恰好相反3）季度坐标从上到下正好是反的季度顺序
#answer：
#1）将横坐标年份离散化，改为因子2）只需将默认颜色颠倒一下即可：scale_fill_continuous(low = '#56B1F7', high = '#132B43')
#3）对于离散变量可以使用scale_y_discrete(limits=c('Q4','Q3','Q2','Q1'))方法实现颠倒，对于连续变量可以直接使用scale_y_reverse()实现刻度的颠倒
ggplot(data = df, mapping = aes(x = factor(Year), y = Quater, fill = Counts)) + geom_tile() + 
  scale_fill_continuous(low = '#56B1F7', high = '#132B43') + 
  scale_y_discrete(limits=c('Q4','Q3','Q2','Q1')) + 
  xlab('Year')

#长形表转宽型表
library(tidyr)
spread(data = df, key = Quater, value = Counts)

#网络图一般用于描述关系强弱或路径分析等，通过网络图可以非常直观的发现数据之间的关联。
#R中igraph包中的graph()或data.frame.graph()函数实现网络图的绘制
#使用gcookbook包中的madmen数据集
library(gcookbook)
head(madmen)
#加载igraph包
library(igraph)
opar <- par(no.readonly = TRUE)
par(mar = c(0,0,0,0))
#选择layout.fruchterman.reingold布局，绘制有方向的网络图
g <- graph.data.frame(madmen, directed = TRUE)
plot(g, layout = layout.fruchterman.reingold, vertex.size = 8, edge.arrow.size = 0.5, vertex.label = NA)
par(opar)

#圆形布局绘制网络图
opar <- par(no.readonly = TRUE)
par(mar = c(0,0,0,0))
#选择layout.circle布局，绘制无方向的网络图
g <- graph.data.frame(madmen, directed = FALSE)
plot(g, layout = layout.circle, vertex.size = 8, vertex.label = NA)
par(opar)

#添加标签
V(g)$name
#绘制带标签的节点
opar <- par(no.readonly = TRUE)
par(mar = c(0,0,0,0))
g <- graph.data.frame(madmen, directed = FALSE)
plot(g, layout = layout.fruchterman.reingold, vertex.size = 8, vertex.label = V(g)$name,
     vertex.label.cex = 0.6, vertex.label.dist = 0.5)
par(opar)


#绘制带标签的节点
opar <- par(no.readonly = TRUE)
par(mar = c(0,0,0,0))
g <- graph.data.frame(madmen, directed = FALSE)
#这里使用另外一种形式来修改连线之间的属性，即E(g)格式：
#将所有连线设置为黑色
E(g)$color = 'black'
#将指定连线设置为红色
E(g)[c(2,8,10)]$color = 'red'
#将指定连线的变迁设置为L1,L2,L3
E(g)[c(2,8,10)]$label = c('L1','L2','L3')
#设置标签属性和连续属性
plot(g, layout = layout.fruchterman.reingold, vertex.size = 8, vertex.label = V(g)$name, 
     vertex.label.cex = 0.6, vertex.label.color = 'black', vertex.label.dist = 0.5, 
     edge.label.cex = 0.8, edge.label.color = 'blue')
par(opar)