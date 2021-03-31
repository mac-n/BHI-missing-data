#impute everything

require(Hmisc)
require(HotDeckImputation)
require(imputeMissings)
#require(bnstruct)#for KNN imputation 

meanimputed=list()
numonly=list()
medianimputed=list()
mlimputed=list()
forests=list()
RFimputed=list()

for (i in 1:10){
 temp<-sapply(dataset[[i]][,4:11],as.numeric)

  temp<-HotDeckImputation::impute.mean(temp)
  meanimputed[[i]]<- cbind(dataset[[i]][,1:3],temp)
}
for (i in 1:10){
  medianimputed[[i]]<- imputeMissings::impute(dataset[[i]],method="median/mode")
}


# for (i in 3:12){
#   for (j in 1:10){
#     temp<-dataset[[j]]
#     temp<-as.matrix(temp)
#     KNNimputed[[i]][[j]]<-bnstruct::knn.impute(temp,k=i)
#     KNNimputed[[i]][[j]]<-as.matrix(KNNimputed[[i]][[j]])
#   }
# }
require(missForest)
for (i in 1:10){
  temp<- dataset[[i]]
  temp[,2]<-as.numeric(temp[,2])
  temp<-as.matrix(temp)
  temp<-missForest(temp)
  RFimputed[[i]]<-temp$ximp
  forests[[i]]<-temp
}

require(mice)
imputedmice<-list()
for (i in 1:10){
  temp<-dataset[[i]]
  temp[,2]<-as.numeric(temp[,2])
  temp<-as.matrix(temp)
  temp<- mice(temp, m = 1, seed = i+2)
  imputedmice1[[i]]<-complete(temp)
}

# require(MissMech)
# imputedML<-list()
# for (i in 1:10){
#   temp<-dataset[[i]]
#   temp[,2]<-as.numeric(temp[,2])
#   temp<-as.matrix(temp)
#   temp<- Impute(temp, mu = NA, sig = NA, imputation.method = "Normal", resid = NA)
#   imputedML[[i]]<-temp$yimp
# }

require(sjmisc)
imputedmice1<-list()
for (i in 1:10){
  temp<-dataset[[i]]
  temp[,2]<-as.numeric(temp[,2])
  temp<-as.matrix(temp)
  temp1<- mice(temp, m = 5, seed = i+2)
  temp<-data.frame(temp)
  imputedmice1[[i]]<-merge_imputations(temp,temp1)
  imputedmice1[[i]]<-cbind(temp[1:3],imputedmice1[[i]])
}
