library(mopsocd)

## Set Arguments
varcount <- 3
fncount <- 9
lbound <- c(2,75,1)
ubound <- c(5,85,2)
optmax <- 1
optmin <- 0

## Run Solver
mopsoCredit<-mopsocd(cost,varcnt=varcount,fncnt=fncount,
                     lowerbound=lbound,upperbound=ubound,opt=optmax,popsize = 20,maxgen = 2)

## Access Pareto Object Fields
print(mopsoCredit$numsols)
print(mopsoCredit$paramvalues)
print(mopsoCredit$objfnvalues)


## Plot 
plot(mopsoCredit$objfnvalues[,1],mopsoCredit$objfnvalues[,2], 
     main='Soluções encontradas pelo MO-PSO (Confidential Credit Data Set)',
     xlab='PPV',
     ylab='NPV')


