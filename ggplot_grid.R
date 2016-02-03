#一、散点图
library(ggplot2)
p1 <- ggplot(data = iris, mapping = aes(x = Petal.Length, y = Petal.Width))
#不作任何处理的散点图
p1 + geom_point()
#行多列的分面图
p1 + geom_point() + facet_grid(.~Species)
#一列多行的分面图
p1 + geom_point() + facet_grid(Species~.)
#也可以实现多行对列的分面，一般将因子水平数目最大的变量按列排放
p2 <- ggplot(data = diamonds, mapping = aes(x = carat, y = price))
p2 + geom_point() + facet_grid(cut~color)


#二、直方图
p3 <- ggplot(data = diamonds, mapping = aes(x = carat, y = ..density..))
p3 + geom_histogram(binwidth = 0.1) + facet_grid(cut~.)


#三、边际图
#还可以为分面图再添加一项边际图，实现功能只需设置参数margins=TRUE。
p4 <- ggplot(data = diamonds, mapping = aes(x = carat, y = price))
p4 + geom_point() + facet_grid(cut~color, margins=TRUE)
p5 <- ggplot(data = diamonds, mapping = aes(x = carat, y = ..density..))
p5 + geom_histogram(binwidth = 0.1) + facet_grid(cut~. , margins=TRUE)
