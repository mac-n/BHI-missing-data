#sensitivity and specificity calculations
#correct answers in answermatrix
#predicted answers in resultsmatrix


#boolean matrices
answersare1<-answermatrix==1
answersare2<-answermatrix==2
answersare3<-answermatrix==3

resultsare1<-resultsmatrix==1
resultsare2<-resultsmatrix==2
resultsare3<-resultsmatrix==3

resultsare12<-resultsare1 | resultsare2

resultsare23<-resultsare2 | resultsare3

answersare23<-answersare2 | answersare3
answersare12<-answersare1 |answersare2

#sensitivity - positive class 3. True positives/total sick
sens3<-colSums(resultsare3 & answersare3)/colSums(answersare3)
#specificity - positive class 3. True negatives/total well
spec3<-colSums(resultsare12 & answersare12)/colSums(answersare12)

#sensitivity - positive class 2/3. True positives/total sick
sens23x<-colSums(resultsare23 & answersare23)/colSums(answersare23)
#specificity - positive class 2/3. True negatives/total well
spec23x<-colSums(resultsare1 & answersare1)/colSums(answersare1)
