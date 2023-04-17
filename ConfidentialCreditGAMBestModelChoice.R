library(data.table)
library(gam)
library(caret)
library(dplyr)
library(e1071)
library(car)
library(mgcv)
library(dplyr)

cat("\014")

creditApproval<-fread("https://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data", sep = ',', header = FALSE)

creditApproval<-filter_all(creditApproval,all_vars(. != '?'))

creditN<-data.matrix(creditApproval[,c("V1","V4","V5","V6","V7","V9","V10","V11","V12","V13","V16")])

creditApproval$V1<-creditN[,"V1"]
creditApproval$V2<-as.numeric(unlist(creditApproval[,"V2"]))
creditApproval$V3<-creditApproval[,"V3"]
creditApproval$V4<-creditN[,"V4"]
creditApproval$V5<-creditN[,"V5"]
creditApproval$V6<-creditN[,"V6"]
creditApproval$V7<-creditN[,"V7"]
creditApproval$V9<-creditN[,"V9"]
creditApproval$V10<-creditN[,"V10"]
creditApproval$V11<-creditN[,"V11"]
creditApproval$V12<-creditN[,"V12"]
creditApproval$V13<-creditN[,"V13"]
creditApproval$V14<-as.numeric(unlist(creditApproval[,"V14"]))
creditApproval$V15<-creditApproval[,"V15"]
creditApproval$V16<-creditN[,"V16"] - 1

cor(creditApproval,creditApproval$V16)

ccardApp<-creditApproval[creditApproval$V16==0,]

ccardRef<-creditApproval[creditApproval$V16==1,]

subApp.idx<-sample(1:nrow(ccardApp), nrow(ccardRef))

subApp<-ccardApp[subApp.idx,]

subComplete<-rbind(subApp,ccardRef)

subComplete.idx<-sample(1:nrow(subComplete), nrow(subComplete))

subComplete<-subComplete[subComplete.idx,]

train.idx<-sample(1:nrow(subComplete), as.integer(0.7*nrow(subComplete)))

train<-subComplete[train.idx,c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16")]

test<-subComplete[-train.idx,c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16")]

fullGam.object <- gam(V16 ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15, data=train, family="binomial")

prevs <- (predict(fullGam.object, type="response", test) > 0.5)

resp <- (test$V16 > 0.5)

confusionMatrix(data=as.factor(resp), as.factor(prevs), mode="everything")

imp <- as.data.frame(varImp(fullGam.object))
imp <- data.frame(overall = imp$Overall, names   = rownames(imp))
imp[order(imp$overall,decreasing = T),]

summary(fullGam.object)

#----------------------------------

reducedGam.object <- gam(V16 ~ V4 + V5 + V8 + V9 + V10 + V12, data=train, family="gaussian")

prevs <- (predict(reducedGam.object, type="response", test) > 0.5)

resp <- (test$V16 > 0.5)

confusionMatrix(data=as.factor(resp), as.factor(prevs), mode="everything")

summary(reducedGam.object)

#-----------------------------

AIC(reducedGam.object, fullGam.object)

BIC(reducedGam.object, fullGam.object)

