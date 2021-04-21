
#generate missing values based on MMSE/Addenbrooke's scores (translating the missingness structure from clinical data)
require(readr)
lookuptable <- read_csv("BHI-missing-data/lookuptable.csv",
col_types = cols(Addenbrook = col_number(),
MMSE = col_number(), `Midpoint rounded down` = col_number()))


colnames(lookuptable)[1]<-"Add"
#the lookuptable is used for converting MMSE scores to Addenbrooke's scores.
#see https://www.cambridge.org/core/journals/international-psychogeriatrics/article/conversion-between-addenbrookes-cognitive-examination-iii-and-minimental-state-examination/4FCFF17328AAE98AE2AC5307F769DEE1/core-reader
newdf<-basedataset

#generate addenbrooke's score
newdf$Add<-lookuptable$Add[match(newdf$MMSE, lookuptable$MMSE)]
newdf$Add <-scale(newdf$Add)
#probability of missingness 
newdf$pmiss<-0.48+0.06*newdf$Add
newdf<-na.omit(newdf)

#Here we are generating 10 missing data with probabilitiees based on the Addenbrooke's score
dataset<-list()
for (i in 1:10){
  dataset[[i]]<-newdf
#for each column in this row set its value to NA with probability pmiss
for (j in 4:11){
  set.seed(j+i)
  dataset[[i]][j] = ifelse(as.logical(rbinom(nrow(basedataset), size=1, prob=dataset[[i]]$pmiss)), NA, dataset[[i]][,j])
}
#drop the Addenbrooke's, MMSE and probability of missingness columns in the missing datasets

  dataset[[i]]$pmiss<-NULL
  dataset[[i]]$Add<-NULL
  dataset[[i]]$MMSE<-NULL
  #dataset[[i]]<-sapply(dataset[[i]],as.numeric)
}
basedataset<-newdf
#drop the Addenbrooke's, MMSE and probability of missingness columns in the full dataset
basedataset$pmiss<-NULL
basedataset$Add<-NULL
basedataset$MMSE<-NULL
