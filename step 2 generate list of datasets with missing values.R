require(readr)
lookuptable <- read_csv("lookuptable.csv",col_types = cols(Addenbrook = col_skip(),X4 = col_skip(), X5 = col_skip()))
colnames(lookuptable)[2]<-"Add"

newdf<-basedataset

newdf[,4:12]<-sapply(newdf[,4:12],as.numeric)

#newdf[,1:2]<-sapply(newdf[,1:2],as.numeric)
newdf$PTGENDER<-as.factor(newdf$PTGENDER)
newdf$CDRSB<-as.factor(newdf$CDRSB)
newdf$Add<-lookuptable$Add[match(newdf$MMSE, lookuptable$MMSE)]
newdf$Add <-scale(newdf$Add)
newdf$pmiss<-0.48+0.06*newdf$Add
newdf<-na.omit(newdf)

dataset<-list()
for (i in 1:10){
  dataset[[i]]<-newdf

for (j in 4:11){
  set.seed(j+i)
  dataset[[i]][j] = ifelse(as.logical(rbinom(1185, size=1, prob=dataset[[i]]$pmiss)), NA, dataset[[i]][,j])
}
  dataset[[i]]$pmiss<-NULL
  dataset[[i]]$Add<-NULL
  dataset[[i]]$MMSE<-NULL
  #dataset[[i]]<-sapply(dataset[[i]],as.numeric)
}
basedataset<-newdf
basedataset$pmiss<-NULL
basedataset$Add<-NULL
basedataset$MMSE<-NULL
