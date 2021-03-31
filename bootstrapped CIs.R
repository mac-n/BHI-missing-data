
i=1:N
fc <- function( multidata, i){
  d2 <- multidata[i,1:2]
  m<-multiclass.roc(d2[,1], d2[,2],
                 levels=base::levels(as.factor(d2[,1])))
  return(m$auc)
}

cimatrix<-matrix(nrow=2,ncol=16)
colnames(cimatrix)<-LETTERS[1:16]
for (i in 1:16){
multidata=cbind(as.numeric(answermatrix[,1]),as.numeric(resultsmatrix[,i]))
set.seed(1000)
bootcorr <- boot(multidata, fc, R=500)
c<-boot.ci(boot.out = bootcorr, type = c("basic"))
cimatrix[1,i]<-c$basic[4]
cimatrix[2,i]<-c$basic[5]
}


# aucmeasures=list()
# for (i in 1:16){
#   predictor=resultsmatrix[,i]
#   aucmeasures[[i]]<-multiclass.roc(response, predictor,
#                                    levels=base::levels(as.factor(response)))
# }
