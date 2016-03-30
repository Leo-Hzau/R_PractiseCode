library(ggplot2)
ggplot(mtcars) + geom_point(aes(wt, mpg), color='red') +
  geom_text(aes(wt, mpg, label=rownames(mtcars)))
#文本标签重叠很难看清楚。使用ggrepel可以使绘图区域内的文本标签相互分离。
library(ggrepel)
ggplot(mtcars) + geom_point(aes(wt, mpg), color='red') +
  geom_text_repel(aes(wt, mpg, label=rownames(mtcars)))
#要为数据点的文本标签加上框可以使用geom_label_repel()
ggplot(mtcars) + geom_point(aes(wt, mpg), color='red') +
  geom_label_repel(aes(wt, mpg, label=rownames(mtcars)))