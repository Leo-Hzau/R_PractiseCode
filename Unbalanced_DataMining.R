#对于不平衡数据集，分析得出的结论也一定是有偏的，因此SMOTE算法(合成少数过采样技术)，目前是处理非平衡数据的常用手段
#SMOTE算法核心思想：1）采样最邻近算法，计算出每个少数类样本的K个近邻，
 #2）从K个近邻中随机挑选N个样本进行随机线性插值
 #3）构造新的少数类样本
 #4）将新样本与原数据合成，产生新的训练集
#DMwR包中的SMOTE()函数就可以实现这样的功能，具体函数的语法和参数含义如下：
SMOTE(form, data, perc.over = 200, k = 5, perc.under = 200, learner = NULL, ...)
#form：为一个公式形式，类似于y~x1+x2+x3
#data：为不平衡的数据框
#perc.over：定义过采样的抽样次数，即对于少数类样本点，需要为每个点重新构造多少个点。
    #默认值为200，即重新为每个少数类样本点构造200/100=2个点。
#k：指定K邻近算法中的参数K值，即选取K个最邻近的点，默认值为5
#perc.under：定义欠采样的抽样次数，即从多数类样本中选择perc.under倍于新生成的样本数量，
   #默认为200，即从多数类样本中选择200/100=2倍于新生成样本的数量


#下面就使用实例数据来验证一下非平衡数据处理前后的模型结果，数据来源于C50包中的客户流失数据：
library(C50)
data(churn)
train <- churnTrain
test <- churnTest
#查看一下训练集和测试集数据的平衡状态
table(train$churn)
prop.table(table(train$churn))
table(test$churn)
prop.table(table(test$churn))

#构建C5.0决策树算法
model1 <- C5.0(formula = churn ~ ., data = train)
#在测试集中做预测
predict1 <- predict(object = model1, newdata = test[,-20])
#构造模型评估的混淆矩阵
Freq1 <- table(test[,20], predict1)
Freq1
#计算预测精度
Accuracy1 <- sum(diag(Freq1))/sum(Freq1)
Accuracy1
#虽然模型的准确率近95%，但模型的准确率更偏向于非流失客户，而流失客户的预测准确率并不高，仅146/(146+78)=65%。

#模型预测的ROC曲线
library(pROC)
library(ggplot2)
roc_curve01 <- roc(as.numeric(test$churn), as.numeric(predict1))
print(roc_curve01)
#绘制ROC曲线
x1 <- 1-roc_curve01$specificities
y1 <- roc_curve01$sensitivities
p <- ggplot(data = NULL, mapping = aes(x= x1, y = y1))
p + geom_line(colour = 'red', size = 2) +geom_abline(intercept = 0, slope = 1)+ 
  annotate('text', x = 0.4, y = 0.5, label=paste('AUC=',round(roc_curve01$auc,2)), size = 6) + 
  labs(x = '1-specificities',y = 'sensitivities', title = 'ROC Curve')

#接下来用SMOTE对非平衡数据做平衡化处理，即采用SMOTE算法实现数据集的平衡：
set.seed(1234)
library(DMwR)
train2 <- SMOTE(form = churn ~ ., data = train, perc.over = 200, k = 5, perc.under = 150)
#查看数据的平衡状态
table(train2$churn)
prop.table(table(train2$churn))


#再次构建C5.0决策树算法
model2 <- C5.0(formula = churn ~ ., data = train2)
#在测试集中做预测
predict2 <- predict(object = model2, newdata = test[,-20])
#构造模型评估的混淆矩阵
Freq2 <- table(test[,20], predict2)
Freq2
#计算预测精度
Accuracy2 <- sum(diag(Freq2))/sum(Freq2)
Accuracy2
#流失样本的预测精度有了大幅提高，即162/(162+62)=72%

library(pROC)
library(ggplot2)
roc_curve02 <- roc(as.numeric(test$churn), as.numeric(predict2))
print(roc_curve02)
#绘制ROC曲线
x2 <- 1-roc_curve02$specificities
y2 <- roc_curve02$sensitivities
p <- ggplot(data = NULL, mapping = aes(x= x2, y = y2))
p_SMOTE <-p + geom_line(colour = 'red', size = 2) +geom_abline(intercept = 0, slope = 1)+ 
  annotate('text', x = 0.4, y = 0.5, label=paste('AUC=',round(roc_curve02$auc,2)), size = 6) + 
  labs(x = '1-specificities',y = 'sensitivities', title = 'ROC Curve')


#参考资料：
#http://www.itnose.net/detail/6185647.html
#http://www.doc88.com/p-7314389829640.html
#https://www.jair.org/media/953/live-953-2037-jair.pdf

#方法二：Rose包处理非平衡数据集
#http://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/
#通常处理非平衡数据集的方法：Undersampling采样过疏，Oversampling采样过密，
#Synthetic Data Generation人工合成（SMOTE即采用人工合成的方法），#Cost Sensitive Learning代价敏感学习
library(ROSE)
library(C50)
data(churn)
train <- churnTrain
test <- churnTest
#查看一下训练集和测试集数据的平衡状态
table(train$churn)
prop.table(table(train$churn))
table(test$churn)
prop.table(table(test$churn))

#例子一
#构建C5.0决策树算法
model_C <- C5.0(formula = churn ~ ., data = train)
#在测试集中做预测
pred.model_C <- predict(object = model_C, newdata = test[,-20])
accuracy.meas(test[,20], pred.model_C)
roc.curve(test[,20], pred.model_C, plotit = F)
#注：ROSE包中accuracy.meas, 计算精度回测F值等重要指标；roc.curve计算最终预测精度
#over sampling
data_balanced_over <- ovun.sample(churn ~ ., data = train, method = "over",N = 5700)$data
#N为negative观测值*2 table(train$churn)结果no:2850，yes:483因此为5700
table(data_balanced_over$churn)#结果显示no:2850，yes:2850平衡
#接下来对采样过疏样本进行平衡under sampling
data_balanced_under <- ovun.sample(churn ~ ., data = train, method = "under", N = 966, seed = 1)$data
table(data_balanced_under$churn)#结果显示no:483，yes:483平衡
#现在数据集是平衡，但是经过两个欠采样和采样过密不平衡数据抽样有些重要的信息可能会丢失，
#这可以通过使用方法”both“。在这种情况下,少数类是采样过量替换和多数类undersampled没有更换
data_balanced_both <- ovun.sample(churn ~ ., data = train, method = "both", p=0.5,N=3333, seed = 1)$data
#p是指积极类新生成的样本的概率
table(data_balanced_both$churn)#结果no:1721,yes:1612基本平衡
#以上利用过采样技术的数据生成预期数量的重复观测。从采样数据生成剥夺了原始数据的重要信息，这可能会对结果造成影响
#使用ROSE方法是更能好的选择
data.rose <- ROSE(churn ~ ., data = train, seed = 1)$data
table(data.rose$churn)#结果no:1721,yes:1612基本平衡
#构建决策树模型build decision tree models
tree.rose <- rpart(churn ~ ., data = data.rose)
tree.over <- rpart(churn ~ ., data = data_balanced_over)
tree.under <- rpart(churn ~ ., data = data_balanced_under)
tree.both <- rpart(churn ~ ., data = data_balanced_both)

#对测试集预测
pred.tree.rose <- predict(tree.rose, newdata = test)
pred.tree.over <- predict(tree.over, newdata = test)
pred.tree.under <- predict(tree.under, newdata = test)
pred.tree.both <- predict(tree.both, newdata = test)
#预测精度
#AUC ROSE
temp.pred.tree.rose<-ifelse(pred.tree.rose[,1]>0.5,'yes','no')
p_ROSE<-roc.curve(test$churn, temp.pred.tree.rose)
#AUC Oversampling
temp.pred.tree.over <- ifelse(pred.tree.rose[,1]>0.5,'yes','no')
p_over<-roc.curve(test$churn, temp.pred.tree.over,add=TRUE, col=2,lwd=2, lty=2,pch =1)
#AUC Undersampling
temp.pred.tree.under <- ifelse(pred.tree.under[,1]>0.5,'yes','no')
p_under<-roc.curve(test$churn, temp.pred.tree.under,add=TRUE, col=3,lwd=2, lty=3,pch =2)
#AUC Both
temp.pred.tree.both <- ifelse(pred.tree.both[,1]>0.5,'yes','no')
p_both<-roc.curve(test$churn, temp.pred.tree.both,add=TRUE, col=4,lwd=2, lty=4,pch =3)

#这个包还提供了holdout和baggin方法来检查模型精度。这能确保我们的合成预测不遭受高方差影响。
ROSE.holdout <- ROSE.eval(churn ~ ., data = train, learner = rpart, method.assess = "holdout", 
                          extr.pred = function(obj)obj[,2], seed = 1)
ROSE.holdout

#SMOTE方法构建平衡数据集
set.seed(1234)
library(DMwR)
train2 <- SMOTE(form = churn ~ ., data = train, perc.over = 200, k = 5, perc.under = 150)
#查看数据的平衡状态
table(train2$churn)
prop.table(table(train2$churn))
#构建决策树模型build decision tree models
tree.SMOTE <- rpart(churn ~ ., data = train2)
#对测试集预测
pred.tree.SMOTE <- predict(tree.SMOTE, newdata = test)
#AUC SMOTE
temp.pred.tree.SMOTE <- ifelse(pred.tree.SMOTE[,1]>0.5,'yes','no')
p_SMOTE<-roc.curve(test$churn, temp.pred.tree.SMOTE,add=TRUE, col=5,lwd=2, lty=5,pch =4)

legend("bottomright", c("ROSE:0.822", "over:0.822","under:0.801","both:0.85","SMOTE:0.767"), 
       col=1:5, lty=1:5, lwd=2)



#例子二
library(rpart)
library(ROSE)
data(hacide)
treeimb <- rpart(cls ~ ., data = hacide.train)
pred.treeimb <- predict(treeimb, newdata = hacide.test)
accuracy.meas(hacide.test$cls, pred.treeimb[,2])
roc.curve(hacide.test$cls, pred.treeimb[,2], plotit = F)
#over sampling
data_balanced_over <- ovun.sample(cls ~ ., data = hacide.train, method = "over",N = 1960)$data
table(data_balanced_over$cls)
#under sampling
data_balanced_under <- ovun.sample(cls ~ ., data = hacide.train, method = "under",N = 40,seed=1)$data
table(data_balanced_under$cls)
#both
data_balanced_both <- ovun.sample(cls ~ ., data = hacide.train, method = "both", p=0.5, N=1000, seed = 1)$data
table(data_balanced_both$cls)
#ROSE
data.rose <- ROSE(cls ~ ., data = hacide.train, seed = 1)$data
table(data.rose$cls)
#build decision tree models
tree.rose <- rpart(cls ~ ., data = data.rose)
tree.over <- rpart(cls ~ ., data = data_balanced_over)
tree.under <- rpart(cls ~ ., data = data_balanced_under)
tree.both <- rpart(cls ~ ., data = data_balanced_both)

#make predictions on unseen data
pred.tree.rose <- predict(tree.rose, newdata = hacide.test)
pred.tree.over <- predict(tree.over, newdata = hacide.test)
pred.tree.under <- predict(tree.under, newdata = hacide.test)
pred.tree.both <- predict(tree.both, newdata = hacide.test)
#AUC ROSE
roc.curve(hacide.test$cls, pred.tree.rose[,2], col=2,lwd=2, lty=2)
#AUC Oversampling
roc.curve(hacide.test$cls, pred.tree.over[,2],add=TRUE,col=3,lwd=2, lty=3)
#AUC Undersampling
roc.curve(hacide.test$cls, pred.tree.under[,2],add=TRUE,col=4,lwd=2, lty=4)
#AUC Both
roc.curve(hacide.test$cls, pred.tree.both[,2],add=TRUE,col=5,lwd=2, lty=5)
legend("bottomright", c("ROSE:0.989", "over:0.798","under:0.867","both:0.798"), 
       col=2:5, lty=2:5, lwd=2)
ROSE.holdout <- ROSE.eval(cls ~ ., data = hacide.train, learner = rpart, method.assess = "holdout",
                          extr.pred = function(obj)obj[,2], seed = 1)
ROSE.holdout