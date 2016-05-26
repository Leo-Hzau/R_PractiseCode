#PCA
library(psych)
fa.parallel(Harman23.cor$cov,n.obs = 302,fa="pc",nn.iter = 100,show.legend = F,
            main = "Scree plot with parallel analysis")
#碎石图、特征值大于1准则和100次模拟的平行分析可以看到保留2个主成分
Pc<-principal(Harman23.cor$cov,nfactors = 2,rotate = "none")
#结果不太容易建立理论解释，旋转主成分使之容易解释
rc<-principal(Harman23.cor$cov,nfactors = 2,rotate = "varimax")
round(unclass(rc$weights),2)#主成分得分系数


#EFA探索性因子分析
covariances<-ability.cov$cov #协方差矩阵
correlations<-cov2cor(covariances) #转化为相关系数矩阵，数据集不能有缺失值
fa.parallel(correlations,n.obs = 112,fa = "both", n.iter = 100,main="Screeplots")
#根据碎石图提取两个公因子
fa<-fa(correlations,nfactors = 2,rotate = "none",fm="pa")
#使用主轴迭代法fm="pa"提取为旋转的因子，两个因子解释了60%的方差，因子载荷阵不太好解释，使用因子旋转
fa.varimax<-fa(correlations,nfactors = 2,rotate = "varimax",fm="pa")
#用正交旋转rotate = "varimax"提取公因子，正交旋转强制两个因子不相关

fa.promax<-fa(correlations,nfactors = 2,rotate = "promax",fm="pa")
#用斜交旋转提取因子rotate = "promax"，查看因子的相关性
fsm<-function(oblique){
  if(class(oblique)[2]=="fa" & is.null(oblique$Phi)){
    warning("Object dosen't look like oblique EFA")
  }else{
    P <- unclass(oblique$loading)
    F <-P %*% oblique$Phi
    colnames(F) <- c("PA1","PA2")
    return(F)
  }
}
fsm(fa.promax)#变量与因子间的相关系数

factor.plot(fa.promax,labels=rownames(fa.promax$loadings))
fa.diagram(fa.promax,simple = F)
fa.promax$weights#因子得分


#fa.24tests<-fa(Harman74.cor$cov,nfactors= 4,rotate = "promax")