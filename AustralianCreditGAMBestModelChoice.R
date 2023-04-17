library(data.table)
library(gam)
library(caret)
library(dplyr)
library(e1071)
library(car)
library(mgcv)

cat("\014")

creditApproval<-fread("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/australian/australian.dat", sep = ' ', header = FALSE)

cor(creditApproval,creditApproval$V15)

ccardApp<-creditApproval[creditApproval$V15==0,]

ccardRef<-creditApproval[creditApproval$V15==1,]

subApp.idx<-sample(1:nrow(ccardApp), nrow(ccardRef))

subApp<-ccardApp[subApp.idx,]

subComplete<-rbind(subApp,ccardRef)

subComplete.idx<-sample(1:nrow(subComplete), nrow(subComplete))

subComplete<-subComplete[subComplete.idx,]

train.idx<-sample(1:nrow(subComplete), as.integer(0.7*nrow(subComplete)))

train<-subComplete[train.idx,c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15")]

test<-subComplete[-train.idx,c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15")]

fullGam.object <- gam(V15 ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14, data=train, family="binomial")

prevs <- (predict(fullGam.object, type="response", test) > 0.5)

resp <- (test$V15 > 0.5)

confusionMatrix(data=as.factor(resp), as.factor(prevs), mode="everything")

imp <- as.data.frame(varImp(fullGam.object))
imp <- data.frame(overall = imp$Overall, names   = rownames(imp))
imp[order(imp$overall,decreasing = T),]

summary(fullGam.object)

#----------------------------------

reducedGam.object <- gam(V15 ~ V4 + V5 + V7 + V8 + V9 + V14, data=train, family="gaussian")

prevs <- (predict(reducedGam.object, type="response", test) > 0.5)

resp <- (test$V15 > 0.5)

confusionMatrix(data=as.factor(resp), as.factor(prevs), mode="everything")

summary(reducedGam.object)

#-----------------------------

AIC(reducedGam.object, fullGam.object)

BIC(reducedGam.object, fullGam.object)



