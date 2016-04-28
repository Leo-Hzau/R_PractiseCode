#http://tjo-en.hatenablog.com/entry/2016/04/18/190000
#http://dmlc.ml/rstats/2015/11/03/training-deep-net-with-R.html
#accracy Deep Learning>XGboost>Random Forest
#read data
train<-read.csv('https://github.com/ozt-ca/tjo.hatenablog.samples/raw/master/r_samples/public_lib/jp/mnist_reproduced/short_prac_train.csv')
test<-read.csv('https://github.com/ozt-ca/tjo.hatenablog.samples/raw/master/r_samples/public_lib/jp/mnist_reproduced/short_prac_test.csv')
train$label<-as.factor(train$label)
test$label<-as.factor(test$label)
#Random Forest
library(randomForest)
#选取随机森林mtry值
n <- length(names(train))
set.seed(1234)
for (i in 1:(n-1)){
  model <- randomForest(label~., data = train, mtry = i)
  err <- mean(model$err.rate)
  print(err)
}#当mtry=45时，模型内平均误差最小，故确定参数mtry=45

#选取ntree值
set.seed(1234)
model <- randomForest(label~., data = train, mtry = 45, ntree=1000)
plot(model)#ntree在600左右时，模型内误差基本稳定，故取ntree=600

set.seed(1234)
fit <- randomForest(label~., data = train, mtry = 45, ntree= 600, importance = TRUE,proximity=TRUE)
pred<-predict(fit,newdata=test[,-1])
freq<-table(test$label,pred)
sum(diag(freq))/nrow(test) # Accuracy 95.1 %

#可以尝试画图
par(mfrow=c(3,4))
for(i in 1:10){
  image(t(apply(matrix(as.vector(as.matrix(train[(i-1)*500+50,-1])),ncol=28,nrow=28,byrow=T),2,rev)),col=grey(seq(0,1,length.out=256)))
}

#XGBoost
library(xgboost)
library(Matrix)
train.mx<-sparse.model.matrix(label~., train)
test.mx<-sparse.model.matrix(label~., test)
dtrain<-xgb.DMatrix(train.mx, label=train$label)
dtest<-xgb.DMatrix(test.mx, label=test$label)
train.gdbt<-xgb.train(params=list(objective="multi:softmax", 
                                  num_class=12, 
                                  eval_metric="mlogloss", 
                                  eta=0.3, 
                                  max_depth=5, 
                                  subsample=1, 
                                  colsample_bytree=0.5), 
                      data=dtrain, 
                      nrounds=70, 
                      watchlist=list(train=dtrain,test=dtest))
table(test$label,predict(train.gdbt,newdata=dtest))
sum(diag(table(test$label,predict(train.gdbt,newdata=dtest))))/nrow(test)
 # Accuracy 94.9%
#速度明显快于随机森林

#Advanced functionality of xgboost
# Lets start with finding what the actual tree looks like
model <- xgb.dump(train.gdbt, with.stats = T)
model[1:10] #This statement prints top 10 nodes of the model
# Get the feature real names
names <- dimnames(data.matrix(train[,-1]))[[2]]
# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = train.gdbt)
# Nice graph
xgb.plot.importance(importance_matrix[1:10,])
#In case last step does not work for you because of a version issue, you can try following :
barplot(importance_matrix[,1])

#Deep Learning
install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
install.packages("mxnet")
library(mxnet)
# Data preparation
train<-read.csv('https://github.com/ozt-ca/tjo.hatenablog.samples/raw/master/r_samples/public_lib/jp/mnist_reproduced/short_prac_train.csv')
test<-read.csv('https://github.com/ozt-ca/tjo.hatenablog.samples/raw/master/r_samples/public_lib/jp/mnist_reproduced/short_prac_test.csv')
train<-data.matrix(train)
test<-data.matrix(test)
train.x<-train[,-1]
train.y<-train[,1]
train.x<-t(train.x/255)
test_org<-test
test<-test[,-1]
test<-t(test/255)

devices <- mx.cpu()
mx.set.seed(0)
data <- mx.symbol.Variable("data")

# first conv
conv1 <- mx.symbol.Convolution(data=data, kernel=c(5,5), num_filter=20)
tanh1 <- mx.symbol.Activation(data=conv1, act_type="relu")
pool1 <- mx.symbol.Pooling(data=tanh1, pool_type="max",kernel=c(2,2), stride=c(2,2))
drop1 <- mx.symbol.Dropout(data=pool1,p=0.5)
# second conv
conv2 <- mx.symbol.Convolution(data=drop1, kernel=c(5,5), num_filter=50)
tanh2 <- mx.symbol.Activation(data=conv2, act_type="relu")
pool2 <- mx.symbol.Pooling(data=tanh2, pool_type="max",kernel=c(2,2), stride=c(2,2))
drop2 <- mx.symbol.Dropout(data=pool2,p=0.5)
# first fullc
flatten <- mx.symbol.Flatten(data=drop2)
fc1 <- mx.symbol.FullyConnected(data=flatten, num_hidden=500)
tanh4 <- mx.symbol.Activation(data=fc1, act_type="relu")
drop4 <- mx.symbol.Dropout(data=tanh4,p=0.5)
# second fullc
fc2 <- mx.symbol.FullyConnected(data=drop4, num_hidden=10)
# loss
lenet <- mx.symbol.SoftmaxOutput(data=fc2)
train.array <- train.x
dim(train.array) <- c(28, 28, 1, ncol(train.x))
test.array <- test
dim(test.array) <- c(28, 28, 1, ncol(test))
mx.set.seed(0)
tic <- proc.time()
model <- mx.model.FeedForward.create(lenet, X=train.array, y=train.y,
                                     ctx=devices, num.round=60, array.batch.size=100,
                                     learning.rate=0.05, momentum=0.9, wd=0.00001,
                                     eval.metric=mx.metric.accuracy,
                                     epoch.end.callback=mx.callback.log.train.metric(100))
print(proc.time() - tic)
preds <- predict(model, test.array)
pred.label <- max.col(t(preds)) - 1
table(test_org[,1],pred.label)
sum(diag(table(test_org[,1],pred.label)))/1000
# Accuracy 98.7%
