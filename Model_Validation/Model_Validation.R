#分类模型是数据挖掘中应用非常广泛的算法之一，常用的分类算法有Logistic模型、决策树、随机森林、神经网络、Boosting等。
 #针对同一个数据集，可以有许多算法进行分析，那么可以采用混淆矩阵、ROC曲线、提升度、增益法和KS统计量进行模型验证和模型评估。

#一、混淆矩阵
#通过混淆矩阵可以算出模型预测精度((a+d)/(a+b+c+d))、正例覆盖率(b/(c+d))、负例覆盖率(a/(a+b))等。
#通过这么些指标综合考虑模型的预测准确率。

#二、ROC曲线
#在讲解ROC曲线之前，我们先看看几个定义：
#Sensitivity:正确预测到的正例数/实际正例总数,即b/(c+d)
#Specificity:正确预测到的负例数/实际负例总数,即a/(a+b)
#ROC曲线就是根据这两个指标值绘制出来的，其中x轴为1-Specificity，y轴为Sensitivity。
#通过比较ROC曲线与45°直线可以直观的反映模型的好坏，但并不能从定量的角度反馈模型好是好到什么程度或模型差是差到什么程度。
 #那么就引申出了AUC的概念，即ROC曲线下的面积。当曲线偏离45°直线越远，则AUC越大，模型相应就会越好。一般认为AUC在0.75以上，模型就可以接受了。


#三、提升度Lift
#在讲解提升度曲线之前，我们先看看几个定义：
#Pi：测试集中正例的比例，即(c+d)/(a+b+c+d)
#Ptp：正确预测到的正例个数占总观测值的比例，即d/a+b+c+d=Pi1* Sensitivity
#Pfp：把负例错误地预测成正例的个数占总数的比例，即b/a+b+c+d=(1-Pi1)*(1- Specificity) 
#Depth：预测成正例的比例，即b+d/a+b+c+d=Ptp+Pfp
#PV_Plus:正确预测到的正例数/预测正例总数，即d/(b+d)=Ptp/depth
#提升度Lift=(d/b+d)/(c+d/a+b+c+d)=PV_plus/Pi1
#Lift曲线就是根据Depth和Lift两个指标绘制而成，它反映了预测正例的正真准确率。

#四、增益法Gain
#其实增益法Gain与提升度是一个事物的两种说法，从公式中就可以看出：
#Gain=d/(b+d)=PV_plus
#Gain与提升度相比并没有除以Pi值。

#五、K-S统计量
#统计学中，对于单样本的K-S检验就是利用样本数据来推断其是否服从某种分布，对于两样本的K-S检验主要推测的是两个样本是否具有相同的分布，
#对于模型的评估，希望正例的累积概率分布与负例的累积概率分布存在显著差异。
#所以我们使用K-S统计量刻画模型的优劣，即使正例与负例的累积概率差达到最大。通常要求模型KS值在0.4以上。

#实例：
#读取数据
dmagecr <- read.table(file = file.choose(), head = TRUE, sep = '')
#数据结构
str(dmagecr)
#注：其中，二分变量good_bad为目标变量，Logistic模型默认将good水平作为感兴趣的水平，很显然对于客户是否为优质客户的问题，
   #这里选择good作为关注对象是错误的，下面指定bad水平为兴趣水平

#指定感兴趣的水平为bad
dmagecr$good_bad <- factor(dmagecr$good_bad, levels = c('good','bad'),ordered = TRUE)
#创建训练集和测试集
set.seed(1234)
index <- sample(c(1,2), size = nrow(dmagecr), replace = TRUE, prob = c(0.7,0.3))
train <- dmagecr[index == 1,]
test <- dmagecr[index == 2,] 
#构建Logistic模型
model <- glm(formula = good_bad ~ checking+history+duration+savings+property, 
             family = binomial(link = "logit"), data = train)
#模型结果查看
summary(model)#P检验变量显著
#模型的显著性检验
anova(object = model, test = 'Chisq')
#从第一个变量到最后一个变量，逐步加入模型后，模型的偏差检验均为显著，即认为整个模型是通过检验的。
  #下面我们再看看模型的拟合优度如何，即模型的预测与实际情况是否吻合或相近，这里使用H-L检验：

#模型的拟合优度检验--HL检验
library(sjmisc)
HL_test <- hoslem_gof(x = model)
HL_test
#H-L的P值显著大于0.05，即接受实际值与预测值相吻合的原假设，再次说明模型是比较理想的。
 #接下来我们就用这个训练集得到的模型来预测测试集
#模型预测
probility <- predict(object = model, newdata = test[,-21], type = 'response')
predict <- ifelse(probility > 0.5, 'bad', 'good')
#转型为因子
predict <- factor(predict, levels = c('good','bad'), order = TRUE)
#模型评估混淆矩阵
Freq <- table(test[,21], predict)
#预测精度
Accuracy <- sum(diag(Freq))/sum(Freq)
Freq;Accuracy
#从模型的预测精度来看，准确率为74.2%，模型预测并不理想。
 #除了使用混淆矩阵来评估模型，还可以使用ROC曲线下的面积AUC、提升度Lift、增益法Gain和K-S统计量。

#ROC曲线
library(pROC)
roc_curve <- roc(test[,21],probility)
names(roc_curve)
Specificity <- roc_curve$specificities
Sensitivity <- roc_curve$sensitivities
library(ggplot2)
p <- ggplot(data = NULL, mapping = aes(x= 1-Specificity, y = Sensitivity))
p + geom_line(colour = 'red') +geom_abline(intercept = 0, slope = 1)+ 
  annotate('text', x = 0.4, y = 0.5, label=paste('AUC=',round(roc_curve$auc,2)))+ 
  labs(x = '1-Specificity',y = 'Sensitivity', title = 'ROC Curve')
#结果显示，AUC为0.79，相比于0.75，模型马马虎虎还能说的过去

#Lift曲线
Pi <- table(test$good_bad)[2]/sum(table(test$good_bad))
Ptp <- Pi*Sensitivity
Pfp <- (1-Pi)*(1-Specificity)
Depth <- Ptp + Pfp
PV_Plus <- Ptp/Depth
Lift <- PV_Plus/Pi
p <- ggplot(data = NULL, mapping = aes(x= Depth, y = Lift))
p + geom_line(colour = 'blue') + labs(x = 'Depth',y = 'Lift', title = 'Lift Curve')
#提升度一般是这样使用的：如果某项营销活动受成本的限制，又想使营销活动取得非常成功，一般通过Lift曲线进行人员的筛选，
 #即给定某个Lift阈值，反过来确定Depth值。如提升度相比于不作任何模型，使其达到2倍以上的响应，需要设置Depth在前25%以内。
 #同样，我们还可以绘制Gain曲线：
#Gain曲线
p <- ggplot(data = NULL, mapping = aes(x= Depth, y = PV_Plus))
p + geom_line(colour = 'blue')  + labs(x = 'Depth',y = 'PV_Plus', title = 'Gain Curve')

#实际上，Lift曲线与Gain曲线长的一模一样，只不过是纵坐标不同而已。
#胡江堂的基于SAS模型评估系列文章中没有涉及到K-S统计量的讲解，本文就对其作一个拓展，
 #R中还没有找到直接绘制两个连续变量的K-S曲线统计量函数，故这里自定义绘制曲线所需数据的函数：

#准备K-S数据
ks_data <- as.data.frame(cbind(good_bad=test[,21], probility))
good_ks <- ks_data[which(ks_data$good_bad==1),'probility']
bad_ks <- ks_data[which(ks_data$good_bad==2),'probility']
#自定义计算累计分布函数值
KS_Data <- function(x, y){
  gaps_x <- seq(min(x), max(x), length=1000)
  cauculate_x <- numeric()
  for(i in 1:length(gaps_x)){
    cauculate_x[i] <- sum(x<=gaps_x[i])/length(x)
  }
  gaps_x <- sort((gaps_x-min(gaps_x))/(max(gaps_x)-min(gaps_x)))
  gaps_y <- seq(min(y), max(y), length=1000)
  cauculate_y <- numeric()
  for(i in 1:length(gaps_y)){
    cauculate_y[i] <- sum(y<=gaps_y[i])/length(y)
  }
  gaps_y <- sort((gaps_y-min(gaps_y))/(max(gaps_y)-min(gaps_y)))
  return(list(df = data.frame(rbind(data.frame(Gaps = gaps_x,Cauculate = cauculate_x,Type = 'Positive'),
                                    data.frame(Gaps = gaps_y,Cauculate = cauculate_y,Type = 'Negtive'))), 
              KS = max(abs(cauculate_y-cauculate_x)), x = gaps_y[which.max(abs(cauculate_y-cauculate_x))],
              y = abs(cauculate_x[which.max(abs(cauculate_y-cauculate_x))]-cauculate_y[which.max(abs(cauculate_y+cauculate_x))])/2))
}
#绘制K-S曲线
ggplot(data = KS_Data(bad_ks,good_ks)$df, mapping = aes(x = Gaps, y = Cauculate, colour = Type)) + 
  geom_line() + theme(legend.position='none') + 
  annotate(geom = 'text', x = KS_Data(bad_ks,good_ks)$x, y = KS_Data(bad_ks,good_ks)$y, 
           label = paste('K-S Value: ', round(KS_Data(bad_ks,good_ks)$KS,2))) + 
  labs(x = 'Probility', y = 'CDF')
#结果显示，K-S统计量的值为0.43，根据传统的评价准则，也说明该模型还是基本行得通的。

#参考：
#http://mp.weixin.qq.com/s?__biz=MzIxNjA2ODUzNg==&mid=403532662&idx=1&sn=58b0c281e680c407b6924f9ee0080f95#rd
#有关本文的脚本和数据请至下面的链接下载：https://yunpan.cn/cYdyfIWsAfn4e  访问密码 43d5
#胡江堂在统计之都上的三篇文章，即
#《分类模型的性能评估——以SAS Logistic回归为例(1): 混淆矩阵》
#《分类模型的性能评估——以SAS Logistic回归为例(2): ROC和AUC》
#《分类模型的性能评估——以SAS Logistic回归为例(3): Lift和Gain》

