#this script recodes age and CDRSB variables from the dataframe

require(ADNIMERGE)
data(adnimerge)

bl_adni<-adnimerge
#adjust age variable from baseline
rnum <- nrow(bl_adni) 
 for(i in 1:rnum){
   if (bl_adni$VISCODE[[i]] == "m06") bl_adni$AGE[[i]] <- bl_adni$AGE[[i]] + 0.5
   if (bl_adni$VISCODE[[i]] == "m12") bl_adni$AGE[[i]] <- bl_adni$AGE[[i]] + 1
   if (bl_adni$VISCODE[[i]] == "m18") bl_adni$AGE[[i]] <- bl_adni$AGE[[i]] + 1.5
   if (bl_adni$VISCODE[[i]] == "m24") bl_adni$AGE[[i]] <- bl_adni$AGE[[i]] + 2
   if (bl_adni$VISCODE[[i]] == "m30") bl_adni$AGE[[i]] <- bl_adni$AGE[[i]] + 2.5
   if (bl_adni$VISCODE[[i]] == "m36") bl_adni$AGE[[i]] <- bl_adni$AGE[[i]] + 3
   if (bl_adni$VISCODE[[i]] == "m42") bl_adni$AGE[[i]] <- bl_adni$AGE[[i]] + 3.5
   if (bl_adni$VISCODE[[i]] == "m48") bl_adni$AGE[[i]] <- bl_adni$AGE[[i]] + 4
   if (bl_adni$VISCODE[[i]] == "m54") bl_adni$AGE[[i]] <- bl_adni$AGE[[i]] + 4.5
   if (bl_adni$VISCODE[[i]] == "m60") bl_adni$AGE[[i]] <- bl_adni$AGE[[i]] + 5
   if (bl_adni$VISCODE[[i]] == "m66") bl_adni$AGE[[i]] <- bl_adni$AGE[[i]] + 5.5
   if (bl_adni$VISCODE[[i]] == "m72") bl_adni$AGE[[i]] <- bl_adni$AGE[[i]] + 6
   if (bl_adni$VISCODE[[i]] == "m78") bl_adni$AGE[[i]] <- bl_adni$AGE[[i]] + 6.5
   if (bl_adni$VISCODE[[i]] == "m84") bl_adni$AGE[[i]] <- bl_adni$AGE[[i]] + 7
   if (bl_adni$VISCODE[[i]] == "m90") bl_adni$AGE[[i]] <- bl_adni$AGE[[i]] + 7.5
   if (bl_adni$VISCODE[[i]] == "m96") bl_adni$AGE[[i]] <- bl_adni$AGE[[i]] + 8
   if (bl_adni$VISCODE[[i]] == "m102") bl_adni$AGE[[i]] <- bl_adni$AGE[[i]] + 8.5
   if (bl_adni$VISCODE[[i]] == "m108") bl_adni$AGE[[i]] <- bl_adni$AGE[[i]] + 9
   if (bl_adni$VISCODE[[i]] == "m114") bl_adni$AGE[[i]] <- bl_adni$AGE[[i]] + 9.5
   if (bl_adni$VISCODE[[i]] == "m120") bl_adni$AGE[[i]] <- bl_adni$AGE[[i]] + 10
   end 
 }


df<-bl_adni
df$CDRSB1[df$CDRSB==0] <- 0
df$CDRSB1[(df$CDRSB>=0.5 & df$CDRSB<=4.0)]<- 1
df$CDRSB1[(df$CDRSB>=4.5 & df$CDRSB<=9.0)] <- 2
df$CDRSB1[(df$CDRSB>=9.5 & df$CDRSB<=15.5)] <- 3
df$CDRSB1[(df$CDRSB>=16 & df$CDRSB<=18)] <- 3
df$CDRSB[(df$CDRSB==3)]<-2
df<-df[!is.na(df$CDRSB),]
df$CDRSB<-df$CDRSB1
df$CDRSB1<-NULL
df<-df[df$VISCODE== "bl",]


