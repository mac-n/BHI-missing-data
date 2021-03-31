output<-matrix(nrow=10,ncol=15)
for (i in 1:10){
  output[i,1:3]<-comparereg(meanimputed[[i]],basedataset)
  output[i,4:6]<-comparereg(medianimputed[[i]],basedataset)
  output[i,7:9]<-comparereg(imputedmice[[i]],basedataset)
  output[i,10:12]<-comparereg(imputedmice1[[i]],basedataset)  
  output[i,13:15]<-comparereg(RFimputed[[i]],basedataset)
}
### this will be a way to tell if the tests are statistically significant. 

output2=(nrow=10,ncol=40)
for (i in 1:10){
  for(j in 1:10{
  output[i,i*j]<-comparereg(meanimputed[[i]][folds[[i]][[j]],],basedataset[folds[[i]][[j]],])[1])
  output[i,i*j+1]<-comparereg(medianimputed[[i]][folds[[i]][[j]],],basedataset[folds[[i]][[j]],])[1])
  output[i,i*j+2]<-comparereg(medianimputed[[i]][folds[[i]][[j]],],basedataset[folds[[i]][[j]],])[1])
  output[i,i*j+3]<-comparereg(imputedmice1[i]][folds[[i]][[j]],],basedataset[folds[[i]][[j]],])[1])
  output[i,i*j+4]<-comparereg(RFimputed[i]][folds[[i]][[j]],],basedataset[folds[[i]][[j]],])[1])
  }
}

output<-rbind(output,colMeans(output))

for (i in c(2,5,8,11,14)){
  output[11,i]<-min(output[,i])
}

for (i in c(3,6,9,12,15)){
  output[11,i]<-max(output[,i])
}

forexcel<-matrix(nrow=5,ncol=3)
for (i in 0:4){
  y=i*3+1
  forexcel[i+1,1]<-output[11,y]
  forexcel[i+1,2]<-output[11,y]-output[11,y+1]
  forexcel[i+1,3]<-output[11,y+2]-output[11,y]
}

rownames(forexcel)<-c("mean","median","PMM1","PMM5","RF")