imputecolmed<-function(x) {
  z <- median(x, na.rm = TRUE)
  x[is.na(x)] <- z
  return(x)
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
require(mice)
require(missForest)
require(pcaMethods)

#mean
ptmmean<-proc.time()
meanimputed<-list()
for (i in 1:10){
  thedata<- dataset[[i]]
  
  thedata$CDRSB[(thedata$CDRSB==3)]<-2
  thedata$CDRSB<-as.factor(thedata$CDRSB)

  thedata$PTGENDER<-as.factor(thedata$PTGENDER)
  for(j in 3:ncol(thedata)) {
    thedata[ , j][is.na(thedata[ , j])] <- mean(thedata[ , j], na.rm = TRUE)
  }
  meanimputed[[i]]<-thedata
}

ptmmean<-proc.time()-ptmmean
#median
ptmmedian<-proc.time()
medianimputed<-list()
for (i in 1:10){
  thedata<- dataset[[i]]
  
  thedata$CDRSB[(thedata$CDRSB==3)]<-2
  thedata$CDRSB<-as.factor(thedata$CDRSB)
  
  thedata$PTGENDER<-as.factor(thedata$PTGENDER)
  for(j in 3:ncol(thedata)) {
    thedata[ , j]<-imputecolmed(thedata[,j])
  }
  medianimputed[[i]]<-thedata
}

ptmmedian<-proc.time()-ptmmedian

#missForest

require(missForest)
ptmforest<-proc.time()
for (i in 1:10){
  temp<- dataset[[i]]
  temp<-data.matrix(temp)
  temp<-missForest(temp)
  RFimputed[[i]]<-temp$ximp
  forests[[i]]<-temp
}


ptmforest<-proc.time()-ptmforest
#meanbyclass
ptmbyclassmean<-proc.time()
byclassmeanimputed<-list()


for (i in 1:10){
  thedata<- dataset[[i]]
  
  thedata$CDRSB[(thedata$CDRSB==3)]<-2
  thedata$CDRSB<-as.factor(thedata$CDRSB)
  
  thedata$PTGENDER<-as.factor(thedata$PTGENDER)
  for(j in 3:ncol(thedata)) {
    thedata[thedata$CDRSB==1, j][is.na(thedata[thedata$CDRSB==1, j])] <- mean(thedata[thedata$CDRSB==1, j], na.rm = TRUE)
    thedata[thedata$CDRSB==2, j][is.na(thedata[thedata$CDRSB==2, j])] <- mean(thedata[thedata$CDRSB==2, j], na.rm = TRUE)
    thedata[thedata$CDRSB==0, j][is.na(thedata[thedata$CDRSB==0, j])] <- mean(thedata[thedata$CDRSB==0, j], na.rm = TRUE)
  }
  byclassmeanimputed[[i]]<-thedata
}
ptmbyclassmean<-proc.time()-ptmbyclassmean
#mice1
require(mice)
ptmmice1<-proc.time()

imputedmice1<-list()
completedmice1<-list()

for (i in 1:10){
  thedata<- dataset[[i]]
  
  thedata$CDRSB[(thedata$CDRSB==3)]<-2
  thedata$CDRSB<-as.factor(thedata$CDRSB)
  
  thedataPTGENDER<-as.factor(thedata$PTGENDER)
  imputedmice1[[i]]<-mice(thedata, m = 1, seed = i+2)
  completedmice1[[i]]<-complete(imputedmice1[[i]],1)
}
ptmmice1<-ptmmice1-proc.time()
#mice5

ptmmice5<-proc.time()

imputedmice5<-list()
completedmice5<-list()
for (i in 1:10){
  thedata<- dataset[[i]]
  
  thedata$CDRSB[(thedata$CDRSB==3)]<-2
  thedata$CDRSB<-as.factor(thedata$CDRSB)
  
  thedata$PTGENDER<-as.factor(thedata$PTGENDER)
  imputedmice5[[i]]<- mice(thedata, m = 5, seed = i+2)
  summat<-matrix(0,1185,9)
  for (j in 1:5){
    tempmat<-as.matrix(complete(imputedmice5[[i]],j)[,3:11])
    summat<-summat+tempmat
  }
  summat<-summat/5
  completedmice5[[i]]<-cbind(thedata[,1:2],data.frame(summat))
  colnames(completedmice5[[i]])<-colnames(thedata)
}
ptmmice5<-proc.time()-ptmmice5

ptmmice10<-proc.time()

imputedmice10<-list()
completedmice10<-list()
for (i in 1:10){
  thedata<- dataset[[i]]
  
  thedata$CDRSB[(thedata$CDRSB==3)]<-2
  thedata$CDRSB<-as.factor(thedata$CDRSB)
  
  thedata$PTGENDER<-as.factor(thedata$PTGENDER)
  imputedmice10[[i]]<- mice(thedata, m = 10, seed = i+2)
  summat<-matrix(0,1185,9)
  for (j in 1:10){
    tempmat<-as.matrix(complete(imputedmice10[[i]],j)[,3:11])
    summat<-summat+tempmat
  }
  summat<-summat/10
  completedmice10[[i]]<-cbind(thedata[,1:2],data.frame(summat))
  colnames(completedmice10[[i]])<-colnames(thedata)
}
ptmmice10<-proc.time()-ptmmice10
#mice15

ptmmice15<-proc.time()

imputedmice15<-list()
completedmice15<-list()
for (i in 1:10){
  thedata<- dataset[[i]]
  
  thedata$CDRSB[(thedata$CDRSB==3)]<-2
  thedata$CDRSB<-as.factor(thedata$CDRSB)
  
  thedata$PTGENDER<-as.factor(thedata$PTGENDER)
  imputedmice15[[i]]<- mice(thedata, m = 15, seed = i+2)
  summat<-matrix(0,1185,9)
  for (j in 1:15){
    tempmat<-as.matrix(complete(imputedmice15[[i]],j)[,3:11])
    summat<-summat+tempmat
  }
  summat<-summat/15
  completedmice15[[i]]<-cbind(thedata[,1:2],data.frame(summat))
  colnames(completedmice15[[i]])<-colnames(thedata)
}
ptmmice15<-proc.time()-ptmmice15
#mice % missing data

ptmmice50<-proc.time()

imputedmice50<-list()
completedmice50<-list()
for (i in 1:10){
  thedata<- dataset[[i]]
  
  thedata$CDRSB[(thedata$CDRSB==3)]<-2
  thedata$CDRSB<-as.factor(thedata$CDRSB)
  
  thedata$PTGENDER<-as.factor(thedata$PTGENDER)
  imputedmice50[[i]]<- mice(thedata, m = 50, seed = i+2)
  summat<-matrix(0,1185,9)
  for (j in 1:50){
    tempmat<-as.matrix(complete(imputedmice50[[i]],j)[,3:11])
    summat<-summat+tempmat
  }
  summat<-summat/50
  completedmice50[[i]]<-cbind(thedata[,1:2],data.frame(summat))
  colnames(completedmice50[[i]])<-colnames(thedata)
}
ptmmice50<-proc.time()-ptmmice50



ptmpca<-proc.time()
pcimputed<-list()
for (i in 1:10){
pc <- pca(data.frame(sapply(dataset[[i]],as.numeric)), nPcs=3, method="bpca")
pcimputed[[i]]<-completeObs(pc)
}
ptmpca=proc.time()-ptmpca

