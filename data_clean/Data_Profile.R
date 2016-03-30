#探索性分析:查看数据是否存在问题，如缺失值数量、是否存在明显的异常值、数据是如何分布的、数据的集中趋势和离散趋势等。

#探索性分析一般包括三大部分，即数据的分布情况、数据的集中与离散趋势和数据的分布形态：
#首先关于数据分布情况的探索性分析。一般统计中通过5数就可以大致了解数据的分布，他们是最小值、下四分位数、中位数、上四分位数和最大值。
#其次,数据的集中趋势和离散趋势，通过集中趋势可以了解数据的中心值或代表值，通过离散趋势可以了解数据远离中心的程度。
    #关于集中趋势，一般可使用均值、众数、中位数来衡量，离散趋势一般通过标准差、极差和四分位差来体现。
#最后,数据的分布形态，数据的分布形态无非是相比于正态分布而言，即偏度和峰度。偏度是数据分布形态呈现左偏或右偏；峰度是数据分布形态呈现尖瘦或矮胖。
   #对于偏度和峰度需要说明的是：若偏度=0，则无偏；若偏度>0，则有偏；若偏度<0，则左偏；若峰度=0，则陡峭程度与正态分布一致；如峰度>0，则分布陡峭；若峰度<0，则分布平缓。

#从定量和定性的角度看观察数据的探索性分析过程：
#自定义函数describe_statistics，函数返回变量的观测数目、缺失值数目、最小值、下四分位数、中位数、上四分位数、最大值、均值、众数、标准差、极差、四分位差、偏度和峰度

describe_statistics <- function(x){
  options(digits = 3)
  require(timeDate);
  N = length(x);
  Nmiss = sum(is.na(x));
  Min = min(x, na.rm = TRUE);
  Q1 = quantile(x, probs = 0.25, na.rm = TRUE);
  Median = median(x, na.rm = TRUE);
  Q3 = quantile(x, probs = 0.75, na.rm = TRUE);
  Max = max(x, na.rm = TRUE);
  Mean = mean(x, na.rm = TRUE);
  Mode = as.numeric(names(table(x)))[which.max(table(x))];
  Sd = sd(x, na.rm = TRUE);
  Range = abs(diff(range(x)));
  QRange = IQR(x, na.rm = TRUE);
  Skewness = skewness(x, na.rm = TRUE);
  Kurtosis = kurtosis(x, na.rm = TRUE);
  #返回函数结果
  return(data.frame(N = N, Nmiss = Nmiss, Min = Min, Q1 = Q1, Median = Median, 
                    Q3 = Q3, Max = Max, Mean = Mean, Mode = Mode, Sd = Sd, Range = Range, 
                    QRange = QRange, Skewness = Skewness, Kurtosis = Kurtosis))
}

#自定义函数，返回数据框中所有数值型数据的字段
Value_Variables <- function(df){
  Vars <- names(df)[sapply(df,class) == 'integer' | sapply(df,class) == 'numeric']
  return(Vars)
}


#以R中自带的iris数据集测试：
vars <- Value_Variables(iris)
res <- sapply(iris[,vars], describe_statistics)
res


#以C50包中的churnTrain数据集测试：
library(C50)
data(churn)
vars <- Value_Variables(churnTrain)
res <- sapply(churnTrain[,vars], describe_statistics)
res

#转置
t(res)

#以上是从定量的角度来探索数据的分布、集中趋势、离散趋势和分布形态，下面我们简单介绍一下定性的方法。
#从定性角度，即通过可视化来进行数据的探索性分析，使用GGally包中的ggpairs()函数，该函数将绘制两两变量的相关系数、散点图，同时也绘制出单变量的密度分布图：
library(GGally)
vars <- Value_Variables(iris)
ggpairs(iris[,vars])
#上图不仅仅反映了数据的分布情况、还得出两两变量间的散点图和相关系数，可为下一步分析做铺垫。
#数据的探索性分析过程中，通过定量和定性方法的搭配，可使分析者快速的了解数据的结构、分布及内在关系。





