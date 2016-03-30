library(corrplot)
corrplot(corr,
         method = c("circle", "square", "ellipse", "number", "shade", "color", "pie"),
         type = c("full", "lower", "upper"), add = FALSE,
         col = NULL, bg = "white", title = "",  is.corr = TRUE,		
         diag = TRUE, outline = FALSE, mar = c(0,0,0,0),
         addgrid.col = NULL, addCoef.col = NULL, addCoefasPercent = FALSE, 
         order = c("original", "AOE", "FPC", "hclust", "alphabet"),
         hclust.method = c("complete", "ward", "single", "average", "mcquitty", "median", "centroid"),
         addrect = NULL, rect.col = "black", rect.lwd = 2,
         tl.pos = NULL, tl.cex = 1,
         tl.col = "red", tl.offset = 0.4, tl.srt = 90,
         cl.pos = NULL, cl.lim = NULL,
         cl.length = NULL, cl.cex = 0.8, cl.ratio = 0.15, 
         cl.align.text = "c",cl.offset = 0.5,
         addshade = c("negative", "positive", "all"),
         shade.lwd = 1, shade.col = "white",
         p.mat = NULL, sig.level = 0.05,
         insig = c("pch","p-value","blank", "n"),
         pch = 4, pch.col = "black", pch.cex = 3,
         plotCI = c("n","square", "circle", "rect"),
         lowCI.mat = NULL, uppCI.mat = NULL, ...)
#corr：需要可视化的相关系数矩阵
#method：指定可视化的方法，可以是圆形、方形、椭圆形、数值、阴影、颜色或饼图形
#type：指定展示的方式，可以是完全的、下三角或上三角
#col：指定图形展示的颜色，默认以均匀的颜色展示
#bg：指定图的背景色
#title：为图形添加标题
#is.corr：是否为相关系数绘图，默认为TRUE，同样也可以实现非相关系数的可视化，只需使该参数设为FALSE即可
#diag：是否展示对角线上的结果，默认为TRUE
#outline：是否绘制圆形、方形或椭圆形的轮廓，默认为FALSE
#mar：具体设置图形的四边间距
#addgrid.col：当选择的方法为颜色或阴影时，默认的网格线颜色为白色，否则为灰色
#addCoef.col：为相关系数添加颜色，默认不添加相关系数，只有方法为number时，该参数才起作用
#addCoefasPercent：为节省绘图空间，是否将相关系数转换为百分比格式，默认为FALSE
#order：指定相关系数排序的方法，可以是原始顺序(original)、特征向量角序(AOE)、第一主成分顺序(FPC)、层次聚类顺序(hclust)和字母顺序，一般”AOE”排序结果都比”FPC”要好
#hclust.method：当order为hclust时，该参数可以是层次聚类中ward法、最大距离法等7种之一
#addrect：当order为hclust时，可以为添加相关系数图添加矩形框，默认不添加框，如果想添加框时，只需为该参数指定一个整数即可
#rect.col：指定矩形框的颜色
#rect.lwd：指定矩形框的线宽
#tl.pos：指定文本标签(变量名称)的位置，当type=full时，默认标签位置在左边和顶部(lt)，当type=lower时，默认标签在左边和对角线(ld)，当type=upper时，默认标签在顶部和对角线，d表示对角线，n表示不添加文本标签
#tl.cex：指定文本标签的大小
#tl.col：指定文本标签的颜色
#cl.pos：图例（颜色）位置，当type=upper或full时，图例在右表(r)，当type=lower时，图例在底部，不需要图例时，只需指定该参数为n
#addshade：只有当method=shade时，该参数才有用，参数值可以是negtive/positive和all，分表表示对负相关系数、正相关系数和所有相关系数添加阴影。注意：正相关系数的阴影是45度，负相关系数的阴影是135度
#shade.lwd：指定阴影的线宽
#shade.col：指定阴影线的颜色

corr <- cor(mtcars[,1:7])
#参数全部默认情况下的相关系数图
corrplot(corr = corr)
#指定数值方法的相关系数图
corrplot(corr = corr, method="number", col="black", cl.pos="n")
#按照特征向量角序(AOE)排序相关系数图
corrplot(corr = corr, order = 'AOE')
#同时添加相关系数值
corrplot(corr = corr, order ="AOE", addCoef.col="grey")
#选择方法为color
corrplot(corr = corr, method = 'color', order ="AOE", addCoef.col="grey")

#绘制圆形轮廓相关系数图
corrplot(corr = corr, col = NULL ,order="AOE", outline=TRUE, cl.pos="n")

#自定义背景色
corrplot(corr = corr, col = NULL, bg="gold2",  order="AOE", cl.pos="n")

#混合方法之上三角为圆形，下三角为数字
corrplot(corr = corr,order="AOE",type="upper",tl.pos="d")
corrplot(corr = corr,add=TRUE, type="lower", method="number",order="AOE",diag=FALSE,tl.pos="n", cl.pos="n")
#混合方法之上三角为圆形，下三角为方形
corrplot(corr = corr,order="AOE",type="upper",tl.pos="d")
corrplot(corr = corr,add=TRUE, type="lower", method="square",order="AOE",diag=FALSE,tl.pos="n", cl.pos="n")
#混合方法之上三角为圆形，下三角为黑色数字
corrplot(corr = corr,order="AOE",type="upper",tl.pos="tp")
corrplot(corr = corr,add=TRUE, type="lower", method="number",order="AOE", col="black",diag=FALSE,tl.pos="n",
         cl.pos="n")


#以层次聚类法排序
corrplot(corr = corr, order="hclust")
#以层次聚类法排序，并绘制3个矩形框
corrplot(corr = corr, order="hclust", addrect = 3, rect.col = "black")








