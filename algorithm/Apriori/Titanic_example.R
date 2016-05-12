#数据准备
setInternet2(TRUE)
con<-url("http://www.rdatamining.com/data/titanic.raw.rdata")
load(con)
close(con)
str(titanic.raw)

#关联分析
library(arules)
rules<-apriori(titanic.raw)
inspect(rules)

#只保留结果中包含生存变量的关联规则
rules<-apriori(titanic.raw,parameter = list(minlen=2,support=0.005,confidence=0.8), 
               appearance = list(rhs=c("Survived=No","Survived=Yes"),default="lhs"), 
               control = list(verbose=F))
rules.sorted<-sort(rules,by="lift")
inspect(rules.sorted)

#去除冗余的规则
#find redundant rules
subset.matrix<-is.subset(rules.sorted,rules.sorted)
subset.matrix[lower.tri(subset.matrix,diag = T)]<-NA
redundant<-colSums(subset.matrix,na.rm = T)>=1
which(redundant)
#remove redundant rules
rules.pruned<-rules.sorted[!redundant]
inspect(rules.pruned)
#结果看表面上是妇女和儿童优先，减小最小支持率和置信度的阀值，继续观察
rules<-apriori(titanic.raw,parameter = list(minlen=3,support=0.002,confidence=0.2), 
               appearance = list(rhs=c("Survived=Yes"),lhs=c("Class=1st","Class=2nd","Class=3rd","Age=Child","Age=Adult"),
                                 default="none"), control = list(verbose=F))
rules.sorted<-sort(rules,by="confidence")
inspect(rules.sorted)
#规则3和规则5以及之前的规则2和贵则可以发现获得优先救援的主要是头等舱和二等舱的妇孺

#可视化visualize rules
library(arulesViz)
plot(rules)
plot(rules,method="graph",control = list(type = "items"))
plot(rules,method="paracoord",control = list(record = T))