require(missForest)
require(mice)
thedata<-dataset[[1]]
thedata[1,]<-as.factor(thedata[1,])
thedata[2,]<-as.factor(thedata[2,])
N<-nrow(thedata)
forestlist1<-list()
micelist1<-list()
#random forest
ptm<-proc.time()
for (i in 1:N){
  print(i)
  temp<-thedata
  #avoid double dipping when it comes to classification
  #by deleting the outcome variable for the test row
  #not the whole row, so that missing values in the test row are still imputed
  temp[i,1]<-NA
  #that's why we have to do this N times
  set.seed(100)
  forestlist1[[i]]<-missForest(data.matrix(temp))
  
}
forestimputationtime<-proc.time()-ptm

#create mice 5 list for testing classification accuracy with ensemble
for (i in 1:N){
  print(i)
  temp<-thedata
  temp<-data.matrix(temp)
  #avoid double dipping when it comes to classification
  #by deleting the outcome variable for the test row
  temp[i,1]<-NA
  #that's why we have to do this N times
  set.seed(100)
  micelist1[[i]]<-mice(temp, m = 5, seed = i+2)
  
}


#create average over 15 MICE imputations for classification testing
micelist15=list()
ptmmice15 <- proc.time()
for(i in 1:N) {
  
  print(i)
  temp<-thedata
  temp<-data.matrix(temp)
  temp[i,1]<-NA
  micelist15[[i]]<- mice(temp, m = 15, seed = i+7)
}
ptmmice15 <- proc.time() -ptm

mice15toclassify=list()
for (i in 1:N){
  for (j in 1:15){
    tempmat<-as.matrix(complete(micelist15[[i]],j)[,3:11])
    summat<-summat+tempmat
  }
  summat<-summat/15
  
  mice15toclassify[[i]]<-cbind(thedata[,1:2],data.frame(summat))
  colnames(mice15toclassify[[i]])<-colnames(thedata)
}
ptmmice15 <- proc.time() -ptm

