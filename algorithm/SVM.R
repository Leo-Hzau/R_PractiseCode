#支持向量机是现在被广泛用于解决多类非线性分类问题和回归问题。
#传递给函数svm()的关键参数是kernel、cost和gamma。
#Kernel指的是支持向量机的类型，它可能是线性SVM、多项式SVM、径向SVM或Sigmoid SVM。
#Cost是违反约束时的成本函数，gamma是除线性SVM外其余所有SVM都使用的一个参数。
#还有一个类型参数，用于指定该模型是用于回归、分类还是异常检测。
#但是这个参数不需要显式地设置，因为支持向量机会基于响应变量的类别自动检测这个参数，响应变量的类别可能是一个因子或一个连续变量。所以对于分类问题，一定要把你的响应变量作为一个因子。

#例子一参考：http://blog.jobbole.com/84714/
#例子一：使用支持向量机实现二元分类器，使用的数据是来自MASS包的cats数据集。
 #在本例中你将尝试使用体重和心脏重量来预测一只猫的性别。我们拿数据集中20%的数据点，用于测试模型的准确性（在其余的80%的数据上建立模型）。
library(e1071)
data(cats, package="MASS")
inputData <- data.frame(cats[, c (2,3)], response = as.factor(cats$Sex)) # response as factor
# linear SVM 线性SVM
svmfit <- svm(response ~ ., data = inputData, kernel = "linear", cost = 10, scale = FALSE) # linear svm, scaling turned OFF
print(svmfit)
plot(svmfit, inputData)
compareTable <- table (inputData$response, predict(svmfit))  # tabulate
mean(inputData$response != predict(svmfit)) # 19.44% misclassification error

# radial SVM
#注：径向基函数作为一个受欢迎的内核函数，可以通过设置内核参数作为“radial”来使用。当使用一个带有“radial”的内核时，结果中的超平面就不需要是一个线性的了。
#通常定义一个弯曲的区域来界定类别之间的分隔，这也往往导致相同的训练数据，更高的准确度。
svmfit <- svm(response ~ ., data = inputData, kernel = "radial", cost = 10, scale = FALSE) # radial svm, scaling turned OFF
print(svmfit)
plot(svmfit, inputData)
compareTable <- table (inputData$response, predict(svmfit))  # tabulate
mean(inputData$response != predict(svmfit)) # 18.75% misclassification error

#可以使用tune.svm()函数，来寻找svm()函数的最优参数。
### Tuning
# Prepare training and test data
set.seed(100) # for reproducing results
rowIndices <- 1 : nrow(inputData) # prepare row indices
sampleSize <- 0.8 * length(rowIndices) # training sample size
trainingRows <- sample (rowIndices, sampleSize) # random sampling
trainingData <- inputData[trainingRows, ] # training data
testData <- inputData[-trainingRows, ] # test data
tuned <- tune.svm(response ~., data = trainingData, gamma = 10^(-6:-1), cost = 10^(1:2)) # tune
summary (tuned) # to select best gamma and cost当cost为100，gamma为0.001时产生最小的错误率

#cost=100,gamma=0.00,kernal=radial
svmfit <- svm (response ~ ., data = trainingData, kernel = "radial", cost = 100, gamma=0.001, scale = FALSE) # radial svm, scaling turned OFF
print(svmfit)
plot(svmfit, trainingData)
compareTable <- table (testData$response, predict(svmfit, testData))  # comparison table
mean(testData$response != predict(svmfit, testData)) # 13.79% misclassification error

#网格图
# Grid Plot
n_points_in_grid = 60 # num grid points in a line
x_axis_range <- range (inputData[, 2]) # range of X axis
y_axis_range <- range (inputData[, 1]) # range of Y axis
X_grid_points <- seq (from=x_axis_range[1], to=x_axis_range[2], length=n_points_in_grid) # grid points along x-axis
Y_grid_points <- seq (from=y_axis_range[1], to=y_axis_range[2], length=n_points_in_grid) # grid points along y-axis
all_grid_points <- expand.grid (X_grid_points, Y_grid_points) # generate all grid points
names (all_grid_points) <- c("Hwt", "Bwt") # rename
all_points_predited <- predict(svmfit, all_grid_points) # predict for all points in grid
color_array <- c("red", "blue")[as.numeric(all_points_predited)] # colors for all points based on predictions
plot (all_grid_points, col=color_array, pch=20, cex=0.25) # plot all grid points
points (x=trainingData$Hwt, y=trainingData$Bwt, col=c("red", "blue")[as.numeric(trainingData$response)], pch=19) # plot data points
points (trainingData[svmfit$index, c (2, 1)], pch=5, cex=2) # plot support vectors



#例子二 参考《数据挖掘 R语言实战》 采用鸢尾花作为数据集
library(e1071)
data(iris)						# 获取数据集iris
###第一种格式建立模型
model <- svm(Species~.,data=iris)				# 建立svm模型
###第二种格式建立模型
x <- iris[,-5]						# 提取iris数据中除第5列以外的数据作为特征变量
y <- iris[,5]						# 提取iris数据中的第5列数据作为结果变量(即响应变量)
model <- svm(x,y,kernel ="radial",gamma =if(is.vector(x)) 1 else 1/ncol(x))   # 建立svm模型

###对模型进行预测
x <- iris[,1:4]									  # 确认需要进行预测的样本特征矩阵
pred <- predict(model,x)								# 根据模型model对x数据进行预测
pred[sample(1:150,8)]								 # 随机挑选8个预测结果进行展示
table(pred,y)								# 模型预测精度展示

###实际建模过程中完整操作
attach(iris)					# 将数据iris按列单独确认为向量
x <- subset(iris,select = -Species)		# 确定特征变量为数据iris中除去Species的其他项
y <- Species				# 确定结果变量为数据iris中的Species项
type <- c("C-classification","nu-classification","one-classification")# 确定将要适用的分类方式
kernel <- c("linear","polynomial","radial","sigmoid")				#确定将要适用的核函数
pred <- array(0,dim=c(150,3,4))		#初始化预测结果矩阵的三维长度分别为150，3，4
accuracy <- matrix(0,3,4)						#初始化模型精准度矩阵的两维分别为3，4
yy <- as.integer(y)					#为方便模型精度计算，将结果变量数量化为1，2，3
for(i in 1:3)								#确认i影响的维度代表分类方式
{
  for(j in 1:4)							#确认j影响的维度代表核函数
  {
    pred[,i,j]=predict(svm(x,y,type=type[i],kernel=kernel[j]),x)   #对每一模型进行预测
    if(i>2)
    {
      accuracy[i,j]=sum(pred[,i,j]!=1)
    }
    else
    {
      accuracy[i,j]=sum(pred[,i,j]!=yy)
    }
  }
}
dimnames(accuracy)=list(type,kernel)					#确定模型精度变量的列名和行名
table(pred[,1,3],y)							# 模型预测精度展示

###模型可视化
plot(cmdscale(dist(iris[,-5])),col=c("lightgray","black","gray")[as.integer(iris[,5])],pch= c("o","+")[1:150 %in% model$index + 1])                # 绘制模型分类散点图
legend(2,-0.8,c("setosa","versicolor","virginica"),col=c("lightgray","black","gray"),lty=1)		# 标记图例

data(iris)										#读入数据iris
model=svm(Species~., data = iris)							#利用公式格式建立模型
plot(model,iris,Petal.Width~Petal.Length,fill=FALSE,symbolPalette=c("lightgray","black","grey"),svSymbol="+")
#绘制模型类别关于花萼宽度和长度的分类情况
legend(1,2.5,c("setosa","versicolor","virginica"),col=c("lightgray","black","gray"),lty=1)		#标记图例

###模型进一步优化
wts=c(1,1,1)							# 确定模型各个类别的比重为1：1：1
names(wts)=c("setosa","versicolor","virginica")			#确定各个比重对应的类别
model1=svm(x,y,class.weights=wts)				#建立模型
wts=c(1,100,100)						# 确定模型各个类别的比重为1：100：100
names(wts)=c("setosa","versicolor","virginica")			#确定各个比重对应的类别
model2=svm(x,y,class.weights=wts)				#建立模型
pred2=predict(model2,x)						#根据模型进行预测
table(pred2,y)							#展示预测结果
wts=c(1,500,500)						# 确定模型各个类别的比重为1：500：500
names(wts)=c("setosa","versicolor","virginica")			#确定各个比重对应的类别
model3=svm(x,y,class.weights=wts)				#建立模型
pred3=predict(model3,x)						#根据模型进行预测
table(pred3,y)				 			#展示预测结果