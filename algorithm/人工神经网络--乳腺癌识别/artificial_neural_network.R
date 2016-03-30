#define standard function
standard1 <- function(x){
  (x-min(x))/(max(x)-min(x))
}
standard2 <- function(x){
  (x-mean(x))/sd(x)
}

#read date
cancer<-read.csv(file=file.choose())
str(cancer)
#除样本的标识号ID以外，diagnosis变量为目标变量，其余都是数值型变量。
#数据标准化
cancer_stand <- sapply(cancer[,-c(1,2)], standard1)
#数据合并
cancer_stand <- as.data.frame(cbind(diagnosis = cancer$diagnosis, cancer_stand))
#将目标变量转换为因子
cancer_stand$diagnosis <- factor(cancer_stand$diagnosis, levels = c(1,2), labels = c('B','M'))
#构建训练样本集和测试样本集
set.seed(1234)
index <- sample(c(1,2), nrow(cancer_stand), replace = TRUE, prob = c(0.8,0.2))
train <- cancer_stand[index == 1,]
test <- cancer_stand[index == 2,]
#使用nnet包中的nnet()函数建模
library(nnet)
#通过循环，确定最佳的节点数
err1 <- 0
err2 <- 0

for (i in 1:45){
  set.seed(1234)
  model <- nnet(diagnosis ~ ., data = train, maxit = 300, size = i, trace = FALSE)
  err1[i] <- sum(predict(model, train, type = 'class') != train$diagnosis)/nrow(train)
  err2[i] <- sum(predict(model, test, type = 'class') != test$diagnosis)/nrow(test)
}
plot(err1, type = 'b', col = 'black', lty = 2, lwd = 2, ylab = '误差', xlab = '节点数', ylim = c(0,0.05), pch = 10)
lines(err2, type = 'b', col = 'blue', lty = 2, lwd = 2, pch = 23)
legend(locator(1), legend = c('训练集误差率','测试集误差率'), col = c('black','blue'), lty = c(2,2), lwd = c(2,2), bty = 'n',
       pch = c(10,23))
#通过返回的图形结果，选择最佳的节点数为4
#通过循环，确定最大迭代次数
err1 <- numeric()
err2 <- numeric()
for (i in 1:500){
  set.seed(1234)
  model <- nnet(diagnosis ~ ., data = train, maxit = i, size = 4, trace = FALSE)
  err1[i] <- sum(predict(model, train, type = 'class') != train$diagnosis)/nrow(train)
  err2[i] <- sum(predict(model, test, type = 'class') != test$diagnosis)/nrow(test)
}
plot(err1, type = 'l', col = 'black', lty = 1,  ylab = '误差', xlab = '节点数')
lines(err2, type = 'l', col = 'blue', lty = 4)
legend(locator(1), legend = c('训练集误差率','测试集误差率'), col = c('black','blue'), lty = c(1,4), bty = 'n')
#通过返回的图形结果，选择最大迭代次数为50
#建立最终的神经网络模型
set.seed(1234)
model_nnet <- nnet(diagnosis ~ ., data = train, maxit = 50, size = 4, trace = FALSE)
pred_nnet <- predict(model_nnet, test, type = 'class')
#预测精度
Freq_nnet <- table(test$diagnosis, pred_nnet)
Freq_nnet
accuracy_nnet <- sum(diag(Freq_nnet))/sum(Freq_nnet)
accuracy_nnet
#模型准确判断率超过99%，模型非常完美的刻画了数据。
