#plotrix包中的twoord.plot()函数和twoord.stackplot()函数可以实现双坐标轴图形
#twoord.plot()函数语法及参数含义：
twoord.plot(lx,ly,rx,ry,data=NULL,main="",
            
            xlim=NULL,lylim=NULL,rylim=NULL,
            
            mar=c(5,4,4,4),lcol=1,rcol=2,
            
            xlab="",lytickpos=NA,ylab="",
            
            ylab.at=NA,rytickpos=NA,rylab="",
            
            rylab.at=NA,lpch=1,rpch=2,
            
            type="b",xtickpos=NULL,
            
            xticklab=NULL,halfwidth=0.4,
            
            axislab.cex=1,do.first=NULL,...)

#lx,ly,rx,ry：分别指定左坐标轴和右坐标轴的值，必须是连续的值
#data：需要绘制双轴图形的数据框
#main：为图形指定标题
#xlim：限制横坐标值的范围
#lylim,rylim：限制左右纵坐标值的范围
#mar：设置图形边界距，默认值为(5,4,4,4)
#lcol,rcol：设置左右坐标轴的颜色，这样可以起到图例的作用
#xlab：设置横坐标轴标签
#lytickpos：设置左坐标轴刻度标签的位置
#ylab：设置左坐标轴标签
#ylab.at：设置左坐标轴标签位置
#rytickpos：设置右坐标轴刻度标签的位置
#rylab：设置右坐标轴标签
#rylab.at：设置右坐标轴标签位置
#lpch,rpch：设置左右坐标轴图形的外观
#type：指定图形类型
#xtickpos：设置横坐标轴刻度标签位置
#xticklab：设置横坐标轴刻度标签
#halfwidth：设置用户给定条形图宽度的一半
#axislab.cex：设置坐标轴标签和刻度标签的大小
#do.first：通过该参数可以往图形中添加背景色或网格线

#绘制双轴的两个线图
library(plotrix)
#数据准备
Date <- seq(from = as.Date('2015-01-01'), to = as.Date('2015-12-01'), by = 'month')
Consumers <- c(100,80,120,153,200,188,220,322,300,321,282,304)
Amount <- c(1000,840,1458,1844,2045,2000,2548,5081,5000,5200,4800,4971)
df1 <- data.frame(Date = Date, Consumers=Consumers, Amount = Amount)

twoord.plot(lx = df1$Date, ly = df1$Consumers, rx = df1$Date, ry = df1$Amount, main = '双轴的两条线图', 
            xlab = '月份', ylab = '会员人数', rylab = '总消费额', type = c('line','line'))

#1）通过xticklab参数重新设置横坐标轴的刻度标签
#2）通过rytickpos参数重新设置刻度标签
#3）通过do.first参数给图形添加背景色和网格线

twoord.plot(lx=df1$Date,ly=df1$Consumers,rx= df1$Date,ry=df1$Amount,main = '双轴的两条线图', 
            xlab = '月份', ylab = '会员人数', rylab = '总消费额', type = c('line','line'),
            xtickpos = as.numeric(df1$Date),xticklab=as.character(df1$Date),rytickpos = seq(500,5000,by = 1500), 
            do.first = 'plot_bg(col = \'gray\'); grid(col = \'white\', lty = 2)')

#将type参数设置为('bar','line'),用线条表示
twoord.plot(lx = df1$Date, ly = df1$Consumers, rx = df1$Date, ry = df1$Amount, lcol = 'steelblue', 
            main = '双轴的两条线图', xlab = '月份', ylab = '会员人数', rylab = '总消费额', 
            type = c('bar','line'), xtickpos=as.numeric(df1$Date), xticklab = as.character(df1$Date),
            rytickpos = seq(500,5000,by = 1500),  
            do.first = 'plot_bg(col = \'gray\'); grid(col = \'white\', lty = 2)')

#调整halfwidth参数的大小，变成柱状图
twoord.plot(lx = df1$Date, ly = df1$Consumers, rx = df1$Date, ry = df1$Amount, lcol = 'steelblue', 
            main = '双轴的两条线图', xlab = '月份', ylab = '会员人数', rylab = '总消费额', 
            type = c('bar','line'), xtickpos=as.numeric(df1$Date), xticklab = as.character(df1$Date), 
            rytickpos = seq(500,5000,by = 1500), halfwidth = 8, 
            do.first = 'plot_bg(col = \'gray\'); grid(col = \'white\', lty = 2)')


#直方图+核密度图
set.seed(1234)
x = rnorm(1000,10,3)
h <- hist(x, breaks = 50)
#绘制直方图和核密度图
hist(x, breaks = 50, col = 'steelblue', freq = FALSE)
lines(density(x), col = 'red', lwd=2)
x1 <- h$mids
y1 <- h$counts
x2 <- seq(min(x), max(x), by = 0.01)
y2 <- dnorm(seq(min(x), max(x), by = 0.01),10,3)
twoord.plot(lx = x1, ly = y1, rx = x2, ry = y2, type=c('bar','l'), lcol = 'steelblue',
            rcol = 'red', ylab = 'Counts', rylab = 'Density', 
            main = 'Histogram and density curve', halfwidth=0.2, 
            lylim = c(0,max(y1)+1), rylim = c(0,0.2),lwd=2)

#帕累托图形
type <- 1:7
absolute <- c(12,15,20,28,11,5,7)
cum_per <- cumsum(absolute)/sum(absolute)
twoord.plot(lx = type, ly = absolute, rx = type, ry = cum_per, type=c('bar','l'), 
            lcol = 'steelblue', rcol = 'red', ylab = '总数', rylab = '累计百分比%', 
            main = '帕累托图', xtickpos=type, xticklab = c('A','B','C','D','E','F','G'))


#twoord.stackplot()函数与twoord.plot()的不同之处在于，其可以绘制堆叠图
twoord.stackplot(lx, rx, ldata, rdata, 
                 
                 lcol, rcol, ltype, rtype,
                 
                 border, rylab, lylab, xlab,
                 
                 ..., incrylim=NULL,halfwidth=0.4,
                 
                 leftfront=FALSE, 
                 
                 mar = c(5, 4, 4, 4))

#lx,rx：指定左右横坐标轴的值
#ldata,rdata：指定左右纵坐标轴的值
#lcol, rcol：指定左右坐标轴的颜色
#ltype, rtype：指定左右坐标轴线的类型
#border：指定条形图边框颜色
#rylab,lylab：指定左右纵坐标轴标签
#xlab：指定横坐标轴标签
#incrylim：增加坐标轴值的范围
#halfwidth：设置用户给定条形图宽度的一半
#leftfront：如果leftfront设置为TRUE的话，则左坐标轴将置于顶层
#mar：设置图形边界距，默认值为(5,4,4,4)


set.seed(1111)
Date <- 1:12
Old <- round(runif(12, 100,300))
New <- round(runif(12, 50,120))
Ratio <- New/(New+Old)
twoord.stackplot(lx=Date, rx=Date, ldata=cbind(Old, New), rdata=Ratio,  
                 lcol=c('steelblue','orange'), rcol='red', ltype="bar", rtype='l', border="grey80", 
                 lylab = '人数', rylab = '新客比例', xlab='月份', main='新老客占比', incrylim=0.1)
#添加图例
#扩展绘图区域并添加图层
par(xpd=TRUE)
par(new=TRUE)
#在原来图形的基础上绘制一张空图
plot(0:1, 0:1, type="n", xlab="",ylab="", axes=FALSE)
#添加左坐标轴轴图例
legend(0, 1.3, leg=c('老客', '新客'), fill=c('steelblue','orange'), bty = 'n') 
#添加右坐标轴轴图例
legend(-0.03, 1.1, leg='新客比例', col='red', lty = 1, bty = 'n') 
par(xpd=FALSE, new=FALSE)