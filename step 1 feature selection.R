library(FSelector)

#this function is used to get rid of string columns
numorfac<-function(coll){
  return(is.numeric(coll)||is.factor(coll))
}

dfnum<-df[sapply(df, numorfac)]
#the mutual information feature selection algorithm
information.gain(CDRSB ~ ., dfnum)
weights<-information.gain(CDRSB ~ ., dfnum)
 subset <- cutoff.k(weights, 40)
#examine the selected features and then choose the most relevant CFA features
#get rid of baseline only data
collist<-c("CDRSB","PTGENDER","AGE","EcogSPTotal","EcogSPMem","LDELTOTAL","EcogSPLang","MOCA","EcogSPPlan","EcogSPVisspat","EcogPtTotal","MMSE")

#

fulldf<-df[,collist  ]
#create the base dataset that will be used for all continued work
basedataset<-na.omit(fulldf)

basedataset$CDRSB<-as.factor(basedataset$CDRSB)

basedataset$CDRSB<-droplevels(basedataset$CDRSB)
