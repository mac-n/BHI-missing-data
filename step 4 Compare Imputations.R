#this code runs the comparereg function on each imputed dataset

#the comparereg function regresses the values of the imputed columns
#on the values of the same columns in the base dataset
# and returns a vector with mean rsquared value for all the columns, plus the rsquared value for each column

comparereg<-function(temp){
  temp1<-basedataset
  tempvec<-vector(length=8)
  for(i in 4:ncol(temp)){
    (templm<-lm(temp[,i]~basedataset[,i]))
    summary(templm)
    tempvec[i-2]<-summary(templm)$r.squared
  }
  tempvec[1]<-mean(tempvec[2:8])
  return(tempvec)
}

#the rest of the code loops through all the lists of imputed datasets
#and applies comparereg to each in turn.

#the output matrices have individual column rsquared values and the mean of all column rsquared values
#the mean of the means was used in Figure 4 along with imputation times recorded in Step 3. 

meanmatrix<-matrix(nrow=10,ncol=9)
colnames(meanmatrix)<-c("mean",colnames(basedataset)[4:11])
for (i in 1:10){
  meanmatrix[i,]<-comparereg(meanimputed[[i]])
}

medianmatrix<-matrix(nrow=10,ncol=9)
colnames(medianmatrix)<-c("mean",colnames(basedataset)[4:11])
for (i in 1:10){
  medianmatrix[i,]<-comparereg(medianimputed[[i]])
}
RFmatrix<-matrix(nrow=10,ncol=9)
colnames(RFmatrix)<-c("mean",colnames(basedataset)[4:11])
for (i in 1:10){
  RFmatrix[i,]<-comparereg(RFimputed[[i]])
}
byclassmeanmatrix<-matrix(nrow=10,ncol=9)
colnames(byclassmeanmatrix)<-c("mean",colnames(basedataset)[4:11])
for (i in 1:10){
  byclassmeanmatrix[i,]<-comparereg(byclassmeanimputed[[i]])
}
mice1matrix<-matrix(nrow=10,ncol=9)
colnames(mice1matrix)<-c("mean",colnames(basedataset)[4:11])
for (i in 1:10){
  mice1matrix[i,]<-comparereg(completedmice1[[i]])
}
mice5matrix<-matrix(nrow=10,ncol=9)
colnames(mice5matrix)<-c("mean",colnames(basedataset)[4:11])
for (i in 1:10){
  mice5matrix[i,]<-comparereg(completedmice5[[i]])
}
mice10matrix<-matrix(nrow=10,ncol=9)
colnames(mice10matrix)<-c("mean",colnames(basedataset)[4:11])
for (i in 1:10){
  mice10matrix[i,]<-comparereg(completedmice10[[i]])
}
mice15matrix<-matrix(nrow=10,ncol=9)
colnames(mice15matrix)<-c("mean",colnames(basedataset)[4:11])
for (i in 1:10){
  mice15matrix[i,]<-comparereg(completedmice15[[i]])
}
mice50matrix<-matrix(nrow=10,ncol=9)
colnames(mice50matrix)<-c("mean",colnames(basedataset)[4:11])
for (i in 1:10){
  mice50matrix[i,]<-comparereg(completedmice50[[i]])
}

bpcamatrix<-matrix(nrow=10,ncol=9)
colnames(bpcamatrix)<-c("mean",colnames(basedataset)[4:11])
for (i in 1:10){
  bpcamatrix[i,]<-comparereg(pcimputed[[i]])
}
