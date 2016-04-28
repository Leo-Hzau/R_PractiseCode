#http://tjo-en.hatenablog.com/entry/2016/04/18/190000
#t-test:通常t检验用于样本含量较小（例如n<30），总体标准差σ未知的正态分布资料，用t分布理论来推论差异发生的概率，从而比较两个平均数的差异是否显著
d1<-read.csv('https://raw.githubusercontent.com/ozt-ca/tjo.hatenablog.samples/master/r_samples/public_lib/DM_sampledata/ch3_2_2.txt',header=T,sep=' ')
head(d1)
boxplot(d1) 
t.test(d1$DB1,d1$DB2)
#p < 0.05接受原假设，DB1 比DB2速度快

#Chi-squared test:卡方检验
#卡方检验就是统计样本的实际观测值与理论推断值之间的偏离程度，实际观测值与理论推断值之间的偏离程度就决定卡方值的大小，
   #卡方值越大，越不符合；卡方值越小，偏差越小，越趋于符合，若两个值完全相等时，卡方值就为0，表明理论值完全符合。
d2<-matrix(c(25,117,16,32),ncol=2,byrow=T)
chisq.test(d2)
#p<0.05，两组比率差别显著。

#ANOVA(Analysis of Variance)方差分析：又称“变异数分析”，用于两个及两个以上样本均数差别的显著性检验。 
d3<-data.frame(cnt=c(210,435,130,720,320,470,250,380,290,505,180,320,310,390,410,510),pr=c(rep(c('F','Y'),8)),category=rep(c('a','a','b','b'),4))
d3.aov<-aov(cnt~.^2,d3) # aov() function for ANOVA
summary(d3.aov)
#促销pr其P值<0.05有显著影响
#我们可以得出结论,促销的主要作用是促进销量显著增加或减少。另一方面,类别和促销没有区别或类别和促销之间没有相互作用。