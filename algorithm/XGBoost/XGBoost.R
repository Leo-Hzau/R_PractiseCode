library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
set.seed(100)
# load data
df_train = read_csv("train.csv")
df_test = read_csv("test.csv")
# Loading labels of train data
#labels = df_train['labels']
df_train = df_train[-grep('Loan_Status', colnames(df_train))]
# combine train and test data
df_all = rbind(df_train,df_test)

# clean Variables :  here I clean people with age less than 14 or more than 100
df_all[df_all$age < 14 | df_all$age > 100,'age'] <- -1
df_all$age[df_all$age < 0] <- mean(df_all$age[df_all$age > 0])
# one-hot-encoding categorical features
ohe_feats = c('gender', 'education', 'employer')
dummies <- dummyVars(~ Gender +  Education + Self_Employed, data = df_all)
df_all_ohe <- as.data.frame(predict(dummies, newdata = df_all))
df_all_combined <- cbind(df_all[,-c(which(colnames(df_all) %in% ohe_feats))],df_all_ohe)
df_all_combined$agena <- as.factor(ifelse(df_all_combined$age < 0,1,0))
df_all_combined <- df_all_combined[,c('id',features_selected)] 
# split train and test
X = df_all_combined[df_all_combined$id %in% df_train$id,]
y <- recode(labels$labels,'True'=1; 'False'=0)
X_test = df_all_combined[df_all_combined$id %in% df_test$id,]

xgb <- xgboost(data = data.matrix(X[,-1]), 
 label = y, 
 eta = 0.1,
 max_depth = 15, 
 nround=25, 
 subsample = 0.5,
 colsample_bytree = 0.5,
 seed = 1,
 eval_metric = "merror",
 objective = "multi:softprob",
 num_class = 12,
 nthread = 3
)

# predict values in test set
y_pred <- predict(xgb, data.matrix(X_test[,-1]))


#Advanced functionality of xgboost
# Lets start with finding what the actual tree looks like
model <- xgb.dump(xgb, with.stats = T)
model[1:10] #This statement prints top 10 nodes of the model
# Get the feature real names
names <- dimnames(data.matrix(X[,-1]))[[2]]
# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = xgb)
# Nice graph
xgb.plot.importance(importance_matrix[1:10,])
#In case last step does not work for you because of a version issue, you can try following :
barplot(importance_matrix[,1])
