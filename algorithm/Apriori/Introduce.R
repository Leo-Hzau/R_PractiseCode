#Supp(X) = P(X)
#Conf(X>=Y) = P(Y|X)
#Lift(X>=Y) =  CONF(X>=Y)/SUPP(Y) = P(X and Y)/P(X)(Y)
#Lift表示不平衡数据的偏差性，Lift越大，数据质量越好；Lift越小，数据越不平衡
library(arulesViz)
library(arules)
library(RColorBrewer)
data("Groceries")
summary(Groceries)
class(Groceries)
rules<-apriori(Groceries,parameter = list(support = 0.001, confidence = 0.5))
inspect(head(sort(rules,by = "lift"),3))
#散点图
plot(rules)
head(quality(rules))
plot(rules,measure=c("support","lift"),shading = "confidence")
plot(rules, shading = "order",control=list(main = "Two-Key plot"))
sel<-plot(rules, measure=c("support","lift"),shading = "confidence",interactive = T)

#基于分组矩阵的可视化
plot(rules,method = "grouped")
#基于图的可视化
subrules2<-head(sort(rules,by= "lift"),10)
plot(subrules2, method = "graph")
plot(subrules2, method = "graph",control = list(type = "items"))
#平行坐标图
#参考，Visualizing Association Rules-Introduction to the R-extension Package arulesViz