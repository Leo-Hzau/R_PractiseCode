#cross-validation
library(plyr)
library(dplyr)
library(randomForest)
data <- iris
glimpse(data)
#交叉验证，使用rf预测sepal.length
k = 5

data$id <- sample(1:k, nrow(data), replace = TRUE)
list <- 1:k
# 每次迭代的预测用数据框，测试用数据框
# the folds
prediction <- data.frame()
testsetCopy <- data.frame()
# 写一个进度条，用来了解CV的进度
progress.bar <- create_progress_bar("text")
progress.bar$init(k)
#k层的函数
for(i in 1:k){
  # 删除id为i的行，创建训练集
  # 选id为i的行，创建训练集
  trainingset <- subset(data, id %in% list[-i])
  testset <- subset(data, id %in% c(i))
  
  #运行一个随机森林模型
  mymodel <- randomForest(trainingset$Sepal.Length ~ ., data = trainingset, ntree = 100)
  
  #去掉回应列1, Sepal.Length
  temp <- as.data.frame(predict(mymodel, testset[,-1]))
  
  # 将迭代出的预测结果添加到预测数据框的末尾
  prediction <- rbind(prediction, temp)
  
  # 将迭代出的测试集结果添加到测试集数据框的末尾
  # 只保留Sepal Length一列
  testsetCopy <- rbind(testsetCopy, as.data.frame(testset[,1]))
  
  progress.bar$step()
}
# 将预测和实际值放在一起
result <- cbind(prediction, testsetCopy[, 1])
names(result) <- c("Predicted", "Actual")
result$Difference <- abs(result$Actual - result$Predicted)
# 用误差的绝对平均值作为评估 
summary(result$Difference)




#gbm
library(gbm)
data(PimaIndiansDiabetes2,package='mlbench')
# 将响应变量转为0-1格式
data <- PimaIndiansDiabetes2
data$diabetes <- as.numeric(data$diabetes)
data <- transform(data,diabetes=diabetes-1)
# 使用gbm函数建模
model <- gbm(diabetes~.,data=data,shrinkage=0.01,distribution='bernoulli',cv.folds=5, n.trees=3000,verbose=F)
# 用交叉检验确定最佳迭代次数
best.iter <- gbm.perf(model,method='cv')
# 观察各解释变量的重要程度
summary(model,best.iter)
# 变量的边际效应
plot.gbm(model,1,best.iter)
#用caret包观察预测精度
library(caret)
data <- PimaIndiansDiabetes2
fitControl <- trainControl(method = "cv", number = 5,returnResamp = "all")
model2 <- train(diabetes~., data=data,method='gbm',distribution='bernoulli',
                trControl = fitControl,verbose=F,
                tuneGrid = data.frame(.n.trees=best.iter,.shrinkage=0.01,.interaction.depth=1))
model2
