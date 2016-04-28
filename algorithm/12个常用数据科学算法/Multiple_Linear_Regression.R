#http://tjo-en.hatenablog.com/entry/2016/04/18/190000
#Multiple Regression (Linear Models)多元回归分析（线性模型）
data<-read.csv('https://raw.githubusercontent.com/ozt-ca/tjo.hatenablog.samples/master/r_samples/public_lib/DM_sampledata/ch4_3_2.txt',header=T,sep=' ')
head(data)
#数据解释：
#“收入”(每天的啤酒销量)作为因变量与“CM”,“Temp”(温度)和“烟花”(分类变量指示是否有烟花表演在区)作为独立的变量。
data.lm<-lm(Revenue~.,data)
summary(data.lm)
#模型诊断，即显著性检验。
par(mfrow = c(2,2))    # 设置画图为 2x2 的格式 
plot(data.lm,which=c(1:4)) 
# 画出 lm1 中对应于模型检验的 4 张图， 包括残差图（自变量随机误差项是否同方差）、
 #QQ图（检验随机误差项是否正态分布）和Cook距离图（检验异常值）
#19，12， 23为异常值，应该剔除

data1<-data[c(-19,-29,-23),]
data.lm1<-lm(Revenue~.,data1)
summary(data.lm1)
par(mfrow = c(2,2))   
plot(data.lm1,which=c(1:4))

# Plotting
matplot(cbind(data$Revenue,predict(data.lm,newdata=data[,-1])),type='l',lwd=c(2,3),lty=1,col=c(1,2))
legend('topleft',legend=c('Data','Predicted'),lwd=c(2,3),lty=1,col=c(1,2),ncol=1)
#根据结果可以看出温度和是否有烟花表演是重要的，predict()函数进行预测。 

matplot(cbind(data1$Revenue,predict(data.lm1,newdata=data1[,-1])),type='l',lwd=c(2,3),lty=1,col=c(1,2))
legend('topleft',legend=c('Data','Predicted'),lwd=c(2,3),lty=1,col=c(1,2),ncol=1)


#http://blog.sina.com.cn/s/blog_6ee39c3901017fpd.html
#http://blog.sina.com.cn/s/blog_70f632090101bp8u.html