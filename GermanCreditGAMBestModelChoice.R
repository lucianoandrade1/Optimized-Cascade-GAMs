library(data.table)
library(gam)
library(caret)
library(dplyr)
library(e1071)
library(car)
library(mgcv)

cat("\014")

creditApproval<-fread("https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data-numeric", sep = ' ', header = FALSE)

cor(creditApproval,creditApproval$V25)

creditApproval$V25 <- creditApproval$V25 - 1

ccardApp<-creditApproval[creditApproval$V25==0,]

ccardRef<-creditApproval[creditApproval$V25==1,]

subApp.idx<-sample(1:nrow(ccardApp), nrow(ccardRef))

subApp<-ccardApp[subApp.idx,]

subComplete<-rbind(subApp,ccardRef)

subComplete.idx<-sample(1:nrow(subComplete), nrow(subComplete))

subComplete<-subComplete[subComplete.idx,]

train.idx<-sample(1:nrow(subComplete), as.integer(0.7*nrow(subComplete)))

train<-subComplete[train.idx,c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20", "V21", "V22", "V23", "V24", "V25")]

test<-subComplete[-train.idx,c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20", "V21", "V22", "V23", "V24", "V25")]

fullGam.object <- gam(V25 ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24, data=train, family="gaussian")

prevs <- (predict(fullGam.object, type="response", test) > 0.5)

resp <- (test$V25 > 0.5)

confusionMatrix(data=as.factor(resp), as.factor(prevs), mode="everything")

imp <- as.data.frame(varImp(fullGam.object))
imp <- data.frame(overall = imp$Overall, names   = rownames(imp))
imp[order(imp$overall,decreasing = T),]

summary(fullGam.object)

#----------------------------------

reducedGam.object <- gam(V25 ~ V1 + V2 + V3 + V5 + V6 + V11 + V14 + V16 + V18 + V19, data=train, family="binomial")

prevs <- (predict(reducedGam.object, type="response", test) > 0.5)

resp <- (test$V25 > 0.5)

confusionMatrix(data=as.factor(resp), as.factor(prevs), mode="everything")

summary(reducedGam.object)

#-----------------------------

AIC(reducedGam.object, fullGam.object)

BIC(reducedGam.object, fullGam.object)

