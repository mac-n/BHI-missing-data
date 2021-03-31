#ALL LOOCV TESTS
require(randomForest)
require(e1071)
require(caret)
fitControl <- trainControl(
  method = "boot",
  number = 3,sampling = "down",preProcOptions= c("center", "scale"))
thedata<-basedataset
N<-nrow(thedata)
thedata$CDRSB[(thedata$CDRSB==3)]<-2
#
thedata$CDRSB<-as.factor(thedata$CDRSB)
#
thedata$PTGENDER<-as.factor(thedata$PTGENDER)
resultsmatrixcomplete=matrix(nrow=N,ncol=3)
timescomplete=list()
colnames(resultsmatrix)=LETTERS[seq( from = 1, to =16 )]
set.seed(100)
#A - RF
ptm <- proc.time()
for (i in 1:N){
  #nothing yet
  print(paste("A ",i))
  temptest<-thedata[i,]
  temptrain<-thedata[-i,]

model1<-train(CDRSB ~.,temptrain,method="rf",trControl=fitControl)
model1predict<-predict(model1,temptest)
 
  resultsmatrixcomplete[i,1]<-model1predict
}

ptm <- proc.time()-ptm
timescomplete[[1]]<-ptm

#B- SVM
ptm <- proc.time()
for (i in 1:N){
  #nothing yet
  print(paste("B ",i))
  temptest<-thedata[i,]
  temptrain<-thedata[-i,]
  
 model1<-train(CDRSB ~.,temptrain,method="svmLinear",trControl=fitControl)
                model1predict<-predict(model1,temptest)
                
                resultsmatrixcomplete[i,2]<-model1predict
}
ptm <- proc.time()-ptm
timescomplete[[2]]<-ptm


#C - naive Bayes
ptm <- proc.time()
for (i in 1:N){
  #nothing yet
  print(paste("C ",i))
  temptest<-thedata[i,]
  temptrain<-thedata[-i,]
  model1<-naiveBayes(CDRSB ~.,temptrain)
  
  model1predict<-predict(model1,temptest)
  
  resultsmatrixcomplete[i,3]<-model1predict
}
ptm <- proc.time()-ptm
timescomplete[[3]]<-ptm

