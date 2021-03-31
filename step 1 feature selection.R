library(FSelector)

numorfac<-function(coll){
  return(is.numeric(coll)||is.factor(coll))
}
df<-data.frame(basedataset)
dfnum<-df[sapply(df, numorfac)]
# dfnum$CDRSB.bl<-NULL
# cfs(CDRSB ~ ., dfnum)
information.gain(CDRSB ~ ., dfnum)
weights<-information.gain(CDRSB ~ ., dfnum)
 subset <- cutoff.k(weights, 40)
#get rid of baseline only data
collist<-c("CDRSB","PTGENDER","AGE","EcogSPTotal","EcogSPMem","LDELTOTAL","EcogSPLang","MOCA","EcogSPPlan","EcogSPVisspat","EcogPtTotal","MMSE")
#list1<-collist[1:5]
fulldf<-df[,collist  ]
basedataset<-na.omit(fulldf)
summary(basedataset$CDRSB)
basedataset$CDRSB[basedataset$CDRSB==3]<-2
basedataset$CDRSB<-droplevels(basedataset$CDRSB)
