#this implements the workflows shown in Table II in the paper

#The Random Forest and Mice imputations have already been done at this stage
#As LOOCV is being used here 
#they have been done nrow(dataset) times to avoid double-dipping

require(caret)
thedata<-dataset[[1]]
require(mice)
require(e1071)
N<-nrow(thedata)
thedata$CDRSB[(thedata$CDRSB==3)]<-2
#
thedata$CDRSB<-as.factor(thedata$CDRSB)
#
thedata$PTGENDER<-as.factor(thedata$PTGENDER)
resultsmatrix=matrix(nrow=N,ncol=16)
times=list()
models=list()
colnames(resultsmatrix)=LETTERS[seq( from = 1, to =16 )]
set.seed(100)
fitControl <- trainControl(
  method = "boot",
  number = 3,sampling = "down",preProcOptions= c("center", "scale"))
#A - mean - mean - RF. Subtract already known mean-imputation time to get classification time
models[[1]]=list()
ptm <- proc.time()

for (i in 1:N){
  
  print (c("A",i))
  temptest<-thedata[i,]
  temptrain<-thedata[-i,]
  temptrainmean<-temptrain
  temptestmean<-temptest
  for(j in 3:ncol(temptrain)) {
    temptrainmean[ , j][is.na(temptrainmean[ , j])] <- mean(temptrainmean[ , j], na.rm = TRUE)
    temptestmean[ , j][is.na(temptestmean[ , j])] <- mean(temptrainmean[ , j], na.rm = TRUE)
  }
  
  model1<-train(CDRSB ~.,temptrainmean,method="rf",trControl=fitControl)
  model1predict<-predict(model1,temptestmean)
  models[[1]][[i]]=model1
  resultsmatrix[i,1]<-model1predict
}
ptm <- proc.time()-ptm
times[[1]]<-ptm

#B class mean -reduced feature -RF. Subtract known class mean imputation time to get classification time
ptm <- proc.time()
models[[2]]=list()
for (i in 1:N){
 
  print (c("B",i))
  temptest<-thedata[i,]
  temptrain<-thedata[-i,]
  temptrainmean<-temptrain
  reducedtest<-temptest[,colnames(temptest)[!is.na(temptest)]]
  reducedtrain<-temptrain[,names(reducedtest)]
  temptrainmean<-reducedtrain
  for(j in 3:ncol(temptrainmean)) {
    temptrainmean[temptrainmean$CDRSB==1, j][is.na(temptrainmean[temptrainmean$CDRSB==1, j])] <- mean(temptrainmean[temptrainmean$CDRSB==1, j], na.rm = TRUE)
    temptrainmean[temptrainmean$CDRSB==2, j][is.na(temptrainmean[temptrainmean$CDRSB==2, j])] <- mean(temptrainmean[temptrainmean$CDRSB==2, j], na.rm = TRUE)
    temptrainmean[temptrainmean$CDRSB==0, j][is.na(temptrainmean[temptrainmean$CDRSB==0, j])] <- mean(temptrainmean[temptrainmean$CDRSB==0, j], na.rm = TRUE)
  }
  
  model1<-train(CDRSB ~.,temptrainmean,method="rf",trControl=fitControl)
  model1predict<-predict(model1,reducedtest)
  models[[2]][[i]]=model1
  resultsmatrix[i,2]<-model1predict
}
ptm <- proc.time()-ptm
times[[2]]<-ptm
#C RF-RF-RF. RF imputation already done
ptm <- proc.time()
models[[3]]=list()
for (i in 1:N){
  print (c("C",i))
  
  model1<-train(CDRSB ~.,forestlist1[[i]]$ximp,method="rf",trControl=fitControl)
  model1predict<-predict(model1,forestlist1[[i]]$ximptest)
  models[[3]][[i]]=model1
  resultsmatrix[i,3]<-model1predict
}
ptm <- proc.time()-ptm
times[[3]]<-ptm
#D mean - reduced feature - RF. Subtract mean imputation time for classification time
models[[4]]=list()
ptm <- proc.time()
for (i in 1:N){
  #nothing yet
  print (c("D",i))
  temptest<-thedata[i,]
  temptrain<-thedata[-i,]
  temptrainmean<-temptrain
  temptestmean<-temptest
  reducedtest<-temptest[,colnames(temptest)[!is.na(temptest)]]
  reducedtrain<-temptrain[,names(reducedtest)]
  temptrainmean<-reducedtrain
  for(j in 3:ncol(temptrainmean)) {
    temptrainmean[ , j][is.na(temptrainmean[ , j])] <- mean(temptrainmean[ , j], na.rm = TRUE)
    temptestmean[ , j][is.na(temptestmean[ , j])] <- mean(temptrainmean[ , j], na.rm = TRUE)
  }
  model1<-train(CDRSB ~.,temptrainmean,method="rf",trControl=fitControl)
  model1predict<-predict(model1,reducedtest)
  models[[4]][[i]]=model1
  resultsmatrix[i,4]<-model1predict
}
ptm <- proc.time()-ptm
times[[4]]<-ptm


#E RF- reduced feature -RF
models[[5]]=list()
ptm <- proc.time()
for (i in 1:N){
  print (c("E",i))
  temptest<-thedata[i,]
  reducedtest<-temptest[,colnames(temptest)[!is.na(temptest)]]
  #ximptest<-forestlist1[[i]]$ximptest
  reducedtrain<-forestlist1[[i]]$ximp[,names(reducedtest)]
  #model1<-randomForest(CDRSB ~.,reducedtrain)
  model1<-train(CDRSB ~.,reducedtrain,method="rf",trControl=fitControl)
  model1predict<-predict(model1,reducedtest)
  models[[5]][[i]]=model1
  resultsmatrix[i,5]<-model1predict
}
ptm <- proc.time()-ptm
times[[5]]<-ptm



b#F mice5, mice5, modal imputation
models[[6]]=list()
for(i in 1:N) {
  print (c("F",i))
  temp<-micelist1[[i]]
  tempvec<-vector(length=5)
  tempvecpred<-vector(length=5)
  for (j in 1:5){
    
    tempvec[j]<-complete(temp,j)[i,1]
  }
  #getmode instantiated in Step 3
  resultsmatrix[i,6]<-getmode(tempvec)
}
ptm <- proc.time()-ptm


times[[6]]<-ptm

#G mice5, mice5, RF ensemble
models[[7]]=list()
      ptm <- proc.time()
      for(i in 1:N) {
        print (c("G",i))
        temp<-micelist1[[i]]
        tempvec<-vector(length=5)
        tempvecpred<-vector(length=5)
        for (j in 1:5){
          tempvec[j]<-complete(temp,j)[i,1]
          
          tempj<-complete(temp,j)
          
          temptrain<-tempj[-i,]
          temptest<-tempj[i,]
          
          model1<-train(CDRSB ~.,temptrain,method="rf",trControl=fitControl)
          
          tempvecpred[j]<-predict(model1,temptest)
        }
        
        resultsmatrix[i,7]<-getmode(tempvecpred)
      }
ptm <- proc.time()-ptm
times[[7]]<-ptm

#H na/na/Naive Bayes
models[[8]]=list()
ptm <- proc.time()
for (i in 1:N){
  #nothing yet
  print (c("H",i))
  temptest<-thedata[i,]
  temptrain<-thedata[-i,]
  model1<-train(CDRSB ~.,temptrain,method="nb",trControl=fitControl)
  model1predict<-predict(model1,temptest)
  models[[8]][[i]]=model1
  resultsmatrix[i,8]<-model1predict
}
ptm <- proc.time()-ptm
times[[8]]<-ptm
#save.image()

#I mice5 - reduced feature- RF ensemble
models[[9]]=list()
ptm <- proc.time()
for(i in 1:N) {
  print (c("I",i))
  temp<-micelist1[[i]]
  tempvec<-vector(length=5)
  tempvecpred<-vector(length=5)
  temptest<-thedata[i,]
  reducedtest<-temptest[,colnames(temptest)[!is.na(temptest)]]
  for (j in 1:5){
    tempvec[j]<-complete(temp,j)[i,1]
    
    tempj<-complete(temp,j)
    tempj$CDRSB<-as.factor(thedata$CDRSB)
    #
    tempj$PTGENDER<-as.factor(thedata$PTGENDER)
    temptrain<-tempj[-i,]
    reducedtrain<-temptrain[,names(reducedtest)]
    model1<-train(CDRSB ~.,reducedtrain,method="rf",trControl=fitControl)
    tempvecpred[j]<-predict(model1,reducedtest)
  }
  
  
  resultsmatrix[i,9]<-getmode(tempvecpred)
}
ptm<-proc.time()-ptm
times[[9]]<-ptm
      
#J mean - mean -SVM
models[[10]]=list()
sumptmclassify<-0
sumptmimpute<-0
for (i in 1:N){
  #nothing yet
  print (c("J",i))
  ptmimpute<-proc.time()
  temptest<-thedata[i,]
  temptrain<-thedata[-i,]
  temptrainmean<-temptrain
  temptestmean<-temptest
  for(j in 3:ncol(temptrain)) {
    temptrainmean[ , j][is.na(temptrainmean[ , j])] <- mean(temptrainmean[ , j], na.rm = TRUE)
    temptestmean[ , j][is.na(temptestmean[ , j])] <- mean(temptrainmean[ , j], na.rm = TRUE)
  }
  ptmimpute<-proc.time()-ptmimpute
  sumptmimpute<-sumptmimpute+ptmimpute
  ptmclassify<-proc.time()
  #model1<-svm(CDRSB ~.,temptrainmean)
  model1<-train(CDRSB ~.,temptrainmean,method="svmLinear",trControl=fitControl)
  models[[10]][[i]]=model1
  model1predict<-predict(model1,temptestmean)
  ptmclassify<-proc.time()-ptmclassify
  sumptmclassify<-sumptmclassify+ptmclassify
  resultsmatrix[i,10]<-model1predict
}
times[[10]]=sumptmclassify
#K RF - RF  - SVM
models[[11]]=list()
ptm <- proc.time()
for (i in 1:N){
  print (c("K",i))
  
  model1<-train(CDRSB ~.,forestlist1[[i]]$ximp,method="svmLinear",trControl=fitControl)
  model1predict<-predict(model1,forestlist1[[i]]$ximptest)
  models[[11]][[i]]=model1
  resultsmatrix[i,11]<-model1predict
}
ptm <- proc.time()-ptm
times[[11]]<-ptm

#L RF - reduced feature - SVM
models[[12]]=list()
ptm <- proc.time()
for (i in 1:N){
  print (c("L",i))
  temptest<-thedata[i,]
  reducedtest<-temptest[,colnames(temptest)[!is.na(temptest)]]
  ximp<-forestlist1[[i]]$ximp
  reducedtrain<-ximp[,names(reducedtest)]
  #model1<-svm(CDRSB ~.,reducedtrain)
  model1<-train(CDRSB ~.,reducedtrain,method="svmLinear",trControl=fitControl)
  
  model1predict<-predict(model1,reducedtest)
  models[[12]][[i]]=model1
  resultsmatrix[i,12]<-model1predict
}
ptm <- proc.time()-ptm
times[[12]]<-ptm
#M averaged over mice15 imputations for training and test set, random forest classifier
models[[13]]=list()

ptm <- proc.time()
for (i in 1:N){
  print (c("M",i))
  #model1<-svm(CDRSB ~.,mice15toclassify[[i]][-i,])
  model1<-train(CDRSB ~.,mice15toclassify[[i]][-i,],method="rf",trControl=fitControl)
  
  model1predict<-predict(model1,mice15toclassify[[i]][i,])
  models[[13]][[i]]=model1
  resultsmatrix[i,13]<-model1predict
}
ptm <- proc.time()-ptm
times[[13]]<-ptm

#N averaged over mice15 imputations for training set, reduced feature, random forest classifier
models[[14]]=list()
ptm <- proc.time()
for (i in 1:N){
  print (c("N",i))
  temptest<-thedata[i,]
  reducedtest<-temptest[,colnames(temptest)[!is.na(temptest)]]
  temptrain=mice15toclassify[[i]][-i,names(reducedtest)]
  
  #model1<-svm(CDRSB ~.,temptrain)
  model1<-train(CDRSB ~.,temptrain,method="rf",trControl=fitControl)
  
  model1predict<-predict(model1,reducedtest)
  models[[14]][[i]]=model1
  resultsmatrix[i,14]<-model1predict
}

ptm <- proc.time()-ptm
times[[14]]<-ptm
#save.image()
#0 averaged over mice15 imputations for training set, and test set,  SVM classifier

models[[15]]=list()
#this is workflow o - check this.
ptm <- proc.time()
for (i in 1:N){
  print (c("O",i))
  #model1<-randomForest(CDRSB ~.,mice15toclassify[[i]][-i,])
  model1<-train(CDRSB ~.,mice15toclassify[[i]][-i,],method="svmLinear",trControl=fitControl)
  
  model1predict<-predict(model1,mice15toclassify[[i]][i,])
  models[[15]][[i]]=model1
  resultsmatrix[i,15]<-model1predict
}
ptm <- proc.time()-ptm
times[[15]]<-ptm

#P averaged over mice15 imputations for training set, reduced feature, SVM classifier
models[[16]]=list()
ptm <- proc.time()
for (i in 1:N){
  print (c("P",i))
  temptest<-thedata[i,]
  reducedtest<-temptest[,colnames(temptest)[!is.na(temptest)]]
  
  temptrain=mice15toclassify[[i]][-i,names(reducedtest)]
  
  #model1<-randomForest(CDRSB ~.,temptrain)
  model1<-train(CDRSB ~.,temptrain,method="svmLinear",trControl=fitControl)
  
  model1predict<-predict(model1,mice15toclassify[[i]][i,])
 
  models[[16]][[i]]=model1
  resultsmatrix[i,16]<-model1predict
}

ptm <- proc.time()-ptm3
times[[16]]<-ptm3

beepr::beep(3)