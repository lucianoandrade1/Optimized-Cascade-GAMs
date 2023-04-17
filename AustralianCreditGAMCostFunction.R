library(data.table)
library(gam)
library(caret)
library(dplyr)
library(e1071)
library(car)
library(mgcv)
library(pROC)

cat("\014")

creditApproval<-fread("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/australian/australian.dat", sep = ' ', header = FALSE)

cost <- function(x){
  
  ccardApp<-creditApproval[creditApproval$V15==0,]
  
  ccardRef<-creditApproval[creditApproval$V15==1,]
  
  gamList<-list()
  cutOffList<-list()
  
  for(ind in 1:round(x[1])){
    
    subApp.idx<-sample(1:nrow(ccardApp), nrow(ccardRef))
    
    subApp<-ccardApp[subApp.idx,]
    
    subComplete<-rbind(subApp,ccardRef)
    
    subComplete.idx<-sample(1:nrow(subComplete), nrow(subComplete))
    
    subComplete<-subComplete[subComplete.idx,]
    
    train.idx<-sample(1:nrow(subComplete), as.integer(0.7*nrow(subComplete)))
    
    train<-subComplete[train.idx,c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15")]
    
    test<-subComplete[-train.idx,c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15")]
    
    TPR<-x[2]/100  #0.72
    
    errDist_LinkFunc = c("binomial", "quasibinomial")[round(x[3])]
    
    pDmin<--1
    
    for (pCut in c(0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.11, 0.12, 0.13, 0.14, 0.15, 0.16, 0.17, 0.18, 0.19, 0.2, 0.21, 0.22, 0.23, 0.24,
                   0.25, 0.26, 0.27, 0.28, 0.29, 0.3, 0.31, 0.32, 0.33, 0.34, 0.35, 0.36, 0.37, 0.38, 0.39, 0.4, 0.41, 0.42, 0.43, 0.44, 0.45, 0.46, 0.47, 0.48, 0.49,
                   0.40, 0.41, 0.42, 0.43, 0.44, 0.45, 0.46, 0.47, 0.48, 0.49, 0.5, 0.51, 0.55, 0.56, 0.57, 0.58, 0.59, 0.6, 0.61, 0.62, 0.63, 0.64, 0.65, 0.66, 0.67, 
                   0.68, 0.69, 0.7, 0.71, 0.72, 0.73, 0.74, 0.75, 0.76, 0.77, 0.78, 0.79, 0.8, 0.81, 0.82, 0.83, 0.84, 0.85, 0.86, 0.87, 0.88, 0.89, 0.9, 0.91, 0.92, 
                   0.93, 0.94, 0.95)){
      
      reducedGam.object <- gam(V15 ~ V4 + V5 + V7 + V8 + V9 + V14, data=train, family=errDist_LinkFunc)
      
      prevs <- ifelse(predict(reducedGam.object, type="response", test) > pCut, 1, 0)
      
      test$prevs<-prevs
      
      respDmaj<-test[test$prevs==0,]
      
      respDmin<-test[test$prevs==1,]
      
      pDmaj<-sum(respDmaj$V15==respDmaj$prevs)/nrow(respDmaj)
      
      pDmin<-sum(respDmin$V15==respDmin$prevs)/nrow(respDmin)
      
      if (pDmin>=TPR) {
        print("break")
        break
      }
      
    }
    
    gamList[[ind]] <- reducedGam.object
    cutOffList[[ind]] <- pCut
    
  }
  
  subApp.idx<-sample(1:nrow(ccardApp), nrow(ccardRef))
  
  subApp<-ccardApp[subApp.idx,]
  
  subComplete<-rbind(subApp,ccardRef)
  
  subComplete.idx<-sample(1:nrow(subComplete), nrow(subComplete))
  
  subComplete<-subComplete[subComplete.idx,]
  
  test.idx<-sample(1:nrow(subComplete), as.integer(0.3*nrow(subComplete)))
  
  test<-subComplete[test.idx,c("V4", "V5", "V7", "V8", "V9", "V14", "V15")]
  
  respMaj<-NULL
  
  for(ind in 1:round(x[1])){ #3
    
    print(ind)
    
    reducedGam.object = gamList[[ind]]
    
    pCut <- cutOffList[[ind]]
    
    prevs <- ifelse(predict(reducedGam.object, type="response", test) > pCut, 1, 0)
    
    test$prevs<-prevs
    
    respDmaj<-test[test$prevs==0,]
    
    test<-test[test$prevs==1,]
    
    respMaj<-rbind(respMaj,respDmaj)
    
    respMin<-test
    
  }
  
  pDmaj<-sum(respMaj$V15==respMaj$prevs)/nrow(respMaj)
  
  pDmin<-sum(respMin$V15==respMin$prevs)/nrow(respMin)
  
  allResp<-rbind(respMin,respMaj)
  
  cm<-confusionMatrix(data=as.factor(allResp$V15), as.factor(allResp$prevs), mode="everything")
  
  f1<-cm$byClass['Pos Pred Value']
  
  f2<-cm$byClass['Neg Pred Value']
  
  f3<-cm$byClass['Precision']
  
  f4<-cm$byClass['Recall']
  
  f5<-cm$byClass['Sensitivity']
  
  f6<-cm$byClass['Specificity']
  
  f7<-cm$overall['Accuracy']
  
  f8<-cm$byClass['F1']
  
  f9<-auc(as.numeric(allResp$prevs),as.numeric(allResp$V15))
  
  return(c(f1,f2,f3,f4,f5,f6,f7,f8,f9))
}


