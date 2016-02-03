#data from http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening
#预计的准确率应该是:xgboost > GBM > logit > knn
#数据准备
dataset <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data",
                     sep = ",",
                     header = F,
                     na.strings = "?")
head(dataset)
## save.csv(dataset,file = "creditCard.csv")
sapply(dataset,function(x) sum(is.na(x)))
sapply(dataset,class)

#分割数据的训练集和测试集,这里set.seed(123),设定70%的训练集,30%的测试集.
set.seed(123)
dataset <- na.omit(dataset)
n <- dim(dataset)[1]
index <- sample(n,round(0.7*n))
train <- dataset[index,]
test <- dataset[-index,]

#Change the variable into dummy variables
#有时候,需要转化变量为哑变量,因为在一些挖掘场合,数据不能直接使用因子型的数据:
#knn，glmnet，svm，xgboost
#有些挖掘方法是可以使用因子变量的,比如:
#logistic regression，raprt，GBM，randomforest
dataset2 <- dataset
library(plyr)
into_factor <- function(x){
  
  if(class(x) == "factor"){
    n <- length(x)
    data.fac <- data.frame(x = x,y = 1:n)
    output <- model.matrix(y~x,data.fac)[,-1]
    ## Convert factor into dummy variable matrix
  }else{
    output <- x
    ## if x is numeric, output is x
  }
  output
  
}
dataset2 <- colwise(into_factor)(dataset2)
dataset2 <- do.call(cbind,dataset2)
dataset2 <- as.data.frame(dataset2)


#使用logistic回归来进行测试建模和预测,使用的函数是glm
logit.model <- glm(V16~.,data = train,family = "binomial")
logit.response <- predict(logit.model,test,type = "response")
logit.predict <- ifelse(logit.response>0.5,"+","-")
table(logit.predict,test$V16)
accurancy1 <- mean(logit.predict == test$V16)

#使用GBM方法来进行预测,这里用的是caret,repeat-cv来选择最优树
library(caret)
ctrl <- trainControl(method = "repeatedcv",
                    number = 5, repeats = 5)
set.seed(300)
m_gbm <- train(V16 ~ ., data=train, method = "gbm",
              metric = "Kappa", trControl = ctrl)
gbm.predict <- predict(m_gbm,test)
table(gbm.predict,test$V16)
accurancy2 <- mean(gbm.predict == test$V16)

#knn method for classification knn set k = 5
#This is a model without cross-validation
#首先测试一个knn模型,不做CV,不做标准化,不做数据类型转换得到的结果,
#这里,不转换数据类型会把因子类型的变量舍弃,仅保留数值变量
library(caret)
knn.model1 <- knn3(V16 ~ .,data = train, k = 5)  
knn.response1 <- predict(knn.model1,test,class = "response") 
knn.predict1 <- ifelse(knn.response1[,1]<0.5,"+","-")
table(knn.predict1,test$V16)
accurancy3 <- mean(knn.predict1 == test$V16)
#knn after scale After scaling and convert into dummy variables:经过标准化和数据转换之后的准确率:
knn.dataset <- cbind(
  colwise(scale)(dataset2[,-38]),
  V16 = as.factor(dataset2$V16)
)
set.seed(123)
index <- sample(n,round(0.7*n))
train.knn <- knn.dataset[index,]
test.knn <- knn.dataset[-index,]
knn.model1 <- knn3(V16 ~ .,data = train.knn, k = 5)  
knn.predict1 <- predict(knn.model1,test.knn,type = "class") 
table(knn.predict1,test.knn$V16)
accurancy4 <- mean(knn.predict1 == test.knn$V16)
#knn CV for k
library(class)
cv.knn <- function(data,n=5,k){
  index <- sample(1:5,nrow(data),replace = T)
  acc <- 0
  for ( i in 1:5){
    ind <- index == i
    train <- data[-ind,]
    test <- data[ind,]
    knn.model1 <- knn3(V16 ~ .,data = train, k = k)  
    knn.predict <- predict(knn.model1,test,type = "class") 
    acc[i]  <- mean(knn.predict == test$V16)
  }
  mean(acc)
}

cv.knn(train.knn,3,5)
k <- 2:20
set.seed(123)
acc <- sapply(k,function(x) cv.knn(train.knn,3,x))

plot(k,acc,type = "b")

k.final <- which.max(acc)

knn.model.f <- knn3(V16 ~ .,data = train.knn, k = k.final)  
knn.predict.f <- predict(knn.model.f,test.knn,type = "class") 
table(knn.predict.f,test.knn$V16)

accurancy5 <- mean(knn.predict.f == test.knn$V16)

library(caret)
fitControl <- trainControl(method = "cv", 
                           number = 10)

knnTune <- train(x = dataset2[1:37], y = dataset2[,38],
                 method = "knn",
                 preProc = c("center", "scale"),
                 tuneGrid = data.frame(.k = 1:20),
                 trControl = fitControl)

#直接train,test来看:效果是k=5最好
knn_train_test <- function(train,test,k =5){
knn.model.f <- knn3(V16 ~ .,data = train, k = k)  
knn.predict.f <- predict(knn.model.f,test,type = "class") 
mean(knn.predict.f == test$V16)
}

x <- 1:20
result <- 
  sapply(x,
         function(x) knn_train_test(train.knn,test.knn,k = x))  


plot(x,result,type = "b")
k.final <- which.max(result)
accurancy6 <- knn_train_test(train.knn,test.knn,k = k.final)


#xgboost
require(xgboost)
require(methods)
require(plyr)

set.seed(123)

set.seed(123)

index = sample(n,round(0.7*n))
train.xg = dataset2[index,]
test.xg = dataset2[-index,]

label <- as.matrix(train.xg[,38,drop =F])

data <- as.matrix(train.xg[,-38,drop =F])

data2 <-  as.matrix(test.xg[,-38,drop =F])
label2 =  as.matrix(test.xg[,38,drop =F])
# weight <- as.numeric(dtrain[[32]]) * testsize / length(label)

xgmat <- xgb.DMatrix(data, label = label, missing = -10000)
param <- list("objective" = "binary:logistic",
              "bst:eta" = 1,
              "bst:max_depth" = 2,
              "eval_metric" = "logloss",
              "silent" = 1,
              "nthread" = 16 ,
              "min_child_weight" =1.45
)

nround <- 275

bst <- xgb.train(param, xgmat, nround )

res1 <- predict(bst,data2)
pre1 <- ifelse(res1>0.5,1,0)
table(pre1,label2)
accurancy6 <- mean(pre1 ==label2)