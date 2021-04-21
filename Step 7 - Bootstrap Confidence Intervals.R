require(boot)
require(pROC)
#fc is the function we are bootstrapping - in this case it returns the multiclass AUC
#confidence intervals shown on Figure 5 of the paper. 
i=1:nrow(thedata)
fc <- function( multidata, i){
  d2 <- multidata[i,1:2]
  m<-multiclass.roc(d2[,1], d2[,2],
    levels=base::levels(ordered(d2[,1])))
  return(m$auc)
}
# a matrix to hold the confidence intervals
cimatrix<-matrix(nrow=2,ncol=16)

colnames(cimatrix)<-LETTERS[1:16]
#bootstrap confidence intervals for each workflow in turn
for (i in 1:16){
#for sending to function
multidata=cbind(as.numeric(thedata$CDRSB),as.numeric(resultsmatrix[,i]))
set.seed(1000)
#bootstrap 500 times
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
