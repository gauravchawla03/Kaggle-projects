library(caTools)
tr=read.csv("train.csv",header=TRUE)
 split=sample.split(tr$label,SplitRatio=0.7)
 train=subset(tr,split=TRUE)
 test=subset(tr,split=FALSE)

library(rpart)
cart=rpart(label~.,data=train,method='class')
cartpredict=predict(cart,newdata=test,type='class')
cartTable=table(test$label,cartpredict)
table(cartpredict==test$label)
prop.table(table(cartpredict==test$label))
#FALSE      TRUE 
#0.3628333 0.6371667


library(randomForest)
train$label=factor(train$label)
test$label=factor(test$label)
model=randomForest(label~.,data=train,nodesize=10,ntree=50)
modelpredict=predict(model,newdata=test)
head(modelpredict)
table(modelpredict==test)
table(modelpredict==test$label)
prop.table(table(modelpredict==test$label))
##FALSE        TRUE 
##0.004142857 0.995857143


library(kernlab)
svm=ksvm(label~.,data=train,kernel="rbfdot")
svmpredict<-predict(svm,test)
table(svmpredict==test$label)
prop.table(table(svmpredict==test$label))

##FALSE       TRUE 
##0.01385714 0.98614286

library(FNN)
predict<-knn(train,test,train$label,k=10,algorithm="cover_tree")
table(predict==test$label)
prop.table(table(predict==test$label))

#FALSE       TRUE 
#0.02719048 0.97280952

library(nnet)
model<-nnet(label~.,data=train,size=1)
predict<-predict(model,test)
table(predict==test$label)
prop.table(table(predict==test$label))
#FALSE      TRUE 
#0.8884762 0.1115238 


