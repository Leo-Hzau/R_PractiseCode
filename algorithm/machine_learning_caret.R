#抽查(Spot checking)机器学习算法是指如何找出最适合于给定数据集的算法模型。将介绍八个常用于抽查的机器学习算法
#尝试混合算法(如事件模型和树模型)
#尝试混合不同的学习算法(如处理相同类型数据的不同算法)
#尝试混合不同类型的模型(如线性和非线性函数或者参数和非参数模型)


#接下来将会尝试经常用于抽查处理的线性和非线性算法，但是并不包括类似于boosting和bagging的集成算法。每个算法都会从两个视角进行呈现：
#常规的训练和预测方法，和caret包的用法
#可以利用caret包的预处理、算法评估和参数调优的能力高效地评估算法的精度

#本文中将用到两个标准的数据集：
#回归模型：BHD(Boston Housing Dataset)
#分类模型: PIDD(Pima Indians Diabetes Dataset)

#本文中的算法将被分成两组进行介绍：
#1.线性算法：简单、较大的偏倚、运算速度快
#2.非线性算法：复杂、较大的方差、高精确度

#线性算法，这类方法对模型的函数形式有严格的假设条件，虽然这些方法的运算速度快，但是其结果偏倚较大。
#这类模型的最终结果通常易于解读，因此如果线性模型的结果足够精确，那么没有必要采用较为复杂的非线性模型。
#线性回归模型
#stat包中lm()函数利用最小二乘法拟合线性回归模型
#load library and data
library(mlbench)
data(BostonHousing)
#fit model
fit<-lm(medv~.,BostonHousing)
print(fit)
#make predictions
predictions<-predict(fit,BostonHousing)
#summarize accuracy
mse<- mean((BostonHousing$medv - predictions)^2)
print(mse)
#caret
library(caret)
library(mlbench)
data(BostonHousing)
#train
set.seed(7)
control<-trainControl(method = "cv",number = 5)
fit.lm<-train(medv~.,data=BostonHousing,method="lm",metric = "RMSE",
              preProc=c("center","scale"),trControl=control)
print(fit.lm)

#logistic模型
#stat包中glm()函数可以用于拟合广义线性模型，可以用于拟合处理二元分类的逻辑回归模型
library(mlbench)
data("PimaIndiansDiabetes")
fit<-glm(diabetes~.,data = PimaIndiansDiabetes,family = binomial(link ='logit'))
print(fit)
#make predictions
probilities<-predict (fit, PimaIndiansDiabetes[,1:8],type = 'response')
predictions<-ifelse(probilities>0.5,'pos','neg')
#summarize accuracy
table(predictions,PimaIndiansDiabetes$diabetes)

#caret
library(caret)
library(mlbench)
data("PimaIndiansDiabetes")
set.seed(7)
control <- trainControl(method="cv", number=5)
fit.glm <- train(diabetes~., data=PimaIndiansDiabetes, method="glm", metric="Accuracy", preProc=c("center", "scale"), trControl=control)
# summarize fit
print(fit.glm)


#线性判别分析
#MASS包中的lda()函数用于拟合线性判别分析模型
library(MASS)
library(mlbench)
data(PimaIndiansDiabetes)
#fit model
fit<-lda(diabetes~.,data=PimaIndiansDiabetes)
print(fit)
#make predictions
predictions<-predict(fit,PimaIndiansDiabetes[,1:8])$class
table(predictions,PimaIndiansDiabetes$diabetes)

#caret
library(caret)
library(mlbench)
data(PimaIndiansDiabetes)
#train
set.seed(7)
control<-trainControl(method = "cv",number = 5)
fit.lda<-train(diabetes~.,data=PimaIndiansDiabetes,method="lda", metric="Accuracy", preProc=c("center", "scale"), trControl=control)
# summarize fit
print(fit.lda)


#glmnet包中的glmnet()函数可以用于拟合正则化分类或回归模型。
#分类模型：
# load the library
library(glmnet)
library(mlbench)
# load data
data(PimaIndiansDiabetes)
x <- as.matrix(PimaIndiansDiabetes[,1:8])
y <- as.matrix(PimaIndiansDiabetes[,9])
# fit model
fit <- glmnet(x, y, family="binomial", alpha=0.5, lambda=0.001)
# summarize the fit
print(fit)
# make predictions
predictions <- predict(fit, x, type="class")
# summarize accuracy
table(predictions, PimaIndiansDiabetes$diabetes)

# caret
# load libraries
library(caret)
library(mlbench)
library(glmnet)
# Load the dataset
data(PimaIndiansDiabetes)
# train
set.seed(7)
control <- trainControl(method="cv", number=5)
fit.glmnet <- train(diabetes~., data=PimaIndiansDiabetes, method="glmnet", metric="Accuracy", preProc=c("center", "scale"), trControl=control)
# summarize fit
print(fit.glmnet)

#回归模型：
# load the libraries
library(glmnet)
library(mlbench)
# load data
data(BostonHousing)
BostonHousing$chas <- as.numeric(as.character(BostonHousing$chas))
x <- as.matrix(BostonHousing[,1:13])
y <- as.matrix(BostonHousing[,14])
# fit model
fit <- glmnet(x, y, family="gaussian", alpha=0.5, lambda=0.001)
# summarize the fit
print(fit)
# make predictions
predictions <- predict(fit, x, type="link")
# summarize accuracy
mse <- mean((y - predictions)^2)
print(mse)

# caret
# load libraries
library(caret)
library(mlbench)
library(glmnet)
# Load the dataset
data(BostonHousing)
# train
set.seed(7)
control <- trainControl(method="cv", number=5)
fit.glmnet <- train(medv~., data=BostonHousing, method="glmnet", metric="RMSE", preProc=c("center", "scale"), trControl=control)
# summarize fit
print(fit.glmnet)


#非线性算法对模型函数形式的限定较少，这类模型通常具有高精度和方差大的特点。具体如下：
#k近邻,caret包中的knn3()函数并没有建立模型，而是直接对训练集数据作出预测。它既可以用于分类模型也可以用于回归模型。
#分类模型
# knn direct classification

# load the libraries
library(caret)
library(mlbench)
# Load the dataset
data(PimaIndiansDiabetes)
# fit model
fit <- knn3(diabetes~., data=PimaIndiansDiabetes, k=3)
# summarize the fit
print(fit)
# make predictions
predictions <- predict(fit, PimaIndiansDiabetes[,1:8], type="class")
# summarize accuracy
table(predictions, PimaIndiansDiabetes$diabetes)

# caret
# load libraries
library(caret)
library(mlbench)
# Load the dataset
data(PimaIndiansDiabetes)
# train
set.seed(7)
control <- trainControl(method="cv", number=5)
fit.knn <- train(diabetes~., data=PimaIndiansDiabetes, method="knn", metric="Accuracy", preProc=c("center", "scale"), trControl=control)
# summarize fit
print(fit.knn)

#回归模型
# load the libraries
library(caret)
library(mlbench)
# load data
data(BostonHousing)
BostonHousing$chas <- as.numeric(as.character(BostonHousing$chas))
x <- as.matrix(BostonHousing[,1:13])
y <- as.matrix(BostonHousing[,14])
# fit model
fit <- knnreg(x, y, k=3)
# summarize the fit
print(fit)
# make predictions
predictions <- predict(fit, x)
# summarize accuracy
mse <- mean((BostonHousing$medv - predictions)^2)
print(mse)

# caret
# load libraries
library(caret)
data(BostonHousing)
# Load the dataset
data(BostonHousing)
# train
set.seed(7)
control <- trainControl(method="cv", number=5)
fit.knn <- train(medv~., data=BostonHousing, method="knn", metric="RMSE", preProc=c("center", "scale"), trControl=control)
# summarize fit
print(fit.knn)

#朴素贝叶斯算法,e1071 包中的 naiveBayes() 函数可用于拟合分类问题中的朴素贝叶斯模型。
# load the libraries
library(e1071)
library(mlbench)
# Load the dataset
data(PimaIndiansDiabetes)
# fit model
fit <- naiveBayes(diabetes~., data=PimaIndiansDiabetes)
# summarize the fit
print(fit)
# make predictions
predictions <- predict(fit, PimaIndiansDiabetes[,1:8])
# summarize accuracy
table(predictions, PimaIndiansDiabetes$diabetes)

# caret
# load libraries
library(caret)
library(mlbench)
# Load the dataset
data(PimaIndiansDiabetes)
# train
set.seed(7)
control <- trainControl(method="cv", number=5)
fit.nb <- train(diabetes~., data=PimaIndiansDiabetes, method="nb", metric="Accuracy", trControl=control)
# summarize fit
print(fit.nb)

#支持向量机算法,kernlab包中的ksvm()函数可用于拟合分类和回归问题中的支持向量机模型。
#分类模型：
# Classification Example:
# load the libraries
library(kernlab)
library(mlbench)
# Load the dataset
data(PimaIndiansDiabetes)
# fit model
fit <- ksvm(diabetes~., data=PimaIndiansDiabetes, kernel="rbfdot")
# summarize the fit
print(fit)
# make predictions
predictions <- predict(fit, PimaIndiansDiabetes[,1:8], type="response")
# summarize accuracy
table(predictions, PimaIndiansDiabetes$diabetes)

# caret
# load libraries
library(caret)
library(mlbench)
# Load the dataset
data(PimaIndiansDiabetes)
# train
set.seed(7)
control <- trainControl(method="cv", number=5)
fit.svmRadial <- train(diabetes~., data=PimaIndiansDiabetes, method="svmRadial", metric="Accuracy", trControl=control)
# summarize fit
print(fit.svmRadial)

#回归模型：
# Regression Example:
# load the libraries
library(kernlab)
library(mlbench)
# load data
data(BostonHousing)
# fit model
fit <- ksvm(medv~., BostonHousing, kernel="rbfdot")
# summarize the fit
print(fit)
# make predictions
predictions <- predict(fit, BostonHousing)
# summarize accuracy
mse <- mean((BostonHousing$medv - predictions)^2)
print(mse)

# caret
# load libraries
library(caret)
library(mlbench)
# Load the dataset
data(BostonHousing)
# train
set.seed(7)
control <- trainControl(method="cv", number=5)
fit.svmRadial <- train(medv~., data=BostonHousing, method="svmRadial", metric="RMSE", trControl=control)
# summarize fit
print(fit.svmRadial)

#分类和回归树,rpart包中的rpart()函数可用于拟合CART分类树和回归树模型。
#分类模型：
# load the libraries
library(rpart)
library(mlbench)
# Load the dataset
data(PimaIndiansDiabetes)
# fit model
fit <- rpart(diabetes~., data=PimaIndiansDiabetes)
# summarize the fit
print(fit)
# make predictions
predictions <- predict(fit, PimaIndiansDiabetes[,1:8], type="class")
# summarize accuracy
table(predictions, PimaIndiansDiabetes$diabetes)

# caret
# load libraries
library(caret)
library(mlbench)
# Load the dataset
data(PimaIndiansDiabetes)
# train
set.seed(7)
control <- trainControl(method="cv", number=5)
fit.rpart <- train(diabetes~., data=PimaIndiansDiabetes, method="rpart", metric="Accuracy", trControl=control)
# summarize fit
print(fit.rpart)

#回归模型：
# load the libraries
library(rpart)
library(mlbench)
# load data
data(BostonHousing)
# fit model
fit <- rpart(medv~., data=BostonHousing, control=rpart.control(minsplit=5))
# summarize the fit
print(fit)
# make predictions
predictions <- predict(fit, BostonHousing[,1:13])
# summarize accuracy
mse <- mean((BostonHousing$medv - predictions)^2)
print(mse)

# caret
# load libraries
library(caret)
library(mlbench)
# Load the dataset
data(BostonHousing)
# train
set.seed(7)
control <- trainControl(method="cv", number=2)
fit.rpart <- train(medv~., data=BostonHousing, method="rpart", metric="RMSE", trControl=control)
# summarize fit
print(fit.rpart)


#http://mp.weixin.qq.com/s?__biz=MzA3MTM3NTA5Ng==&mid=402288640&idx=2&sn=0be81009b8ba4eeeef6785851417c102&scene=5&srcid=0316td6umiAidi7lpaxHPlRc#rd