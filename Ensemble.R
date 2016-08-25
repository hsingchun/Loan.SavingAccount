
# ================== 1.Load data ==================
mydata <- read.csv("cip_loan_top29.csv",stringsAsFactors = F, header = TRUE)
loan<-mydata
loan$TA_LOAN <-as.factor(loan$TA_LOAN)
# ================== 2.Split (training0.5 valid0.3 test0.2)==================
#install.packages("caret")
library(caret)
set.seed(1234)
splitIndex <- createDataPartition(loan$TA_LOAN, p = .50,list = FALSE, times = 1)
trainSplit <- loan[ splitIndex,]

restSplit <-loan[ -splitIndex,]

splitIndex1 <- createDataPartition(restSplit$TA_LOAN, p = .60,list = FALSE, times = 1)
validSplit <- restSplit[splitIndex1,]

testSplit <-  restSplit[-splitIndex1,]

rm(restSplit)
rm(splitIndex)
rm(splitIndex1)


# ====== Set testSplit=======
true.test<-testSplit
testSplit<-trainSplit[,2:29]
# ====== set batch ===
batch <- 50000
# ======= Collect Result (customer.test) =======
customer.test <- matrix(0,nrow(testSplit),3)
colnames(customer.test)<-c("ID","SMO+RF","SMO+XGB")

# ============== $$ SMOTE+ $$==============
library(DMwR)
#table(trainSplit$TA_LOAN)
#str(trainSplit)
set.seed(1234)
newData <- SMOTE(TA_LOAN ~ .-ROW_ID,data=trainSplit, perc.over = 40,perc.under=350)
#table(newData$TA_LOAN)
train.bal<-newData

# ====== SMO+RF ======
library(randomForest)
train.bal<-newData
rf.test <- randomForest(TA_LOAN ~ .-ROW_ID, data=train.bal,type=1,min_sample_leaf=30
                        , ntree=80, proximity=TRUE,xtest=train.bal[,-c(1,2)],
                        ytest=train.bal$TA_LOAN,keep.forest=TRUE)
#==batch for auto==
for (j in  1: ceiling(nrow(testSplit)/batch) ) {
  rm(db)
  start <- (j-1)*batch+1
  if(j == ceiling(nrow(testSplit)/batch)){
    end <- nrow(testSplit)
  }else{
    end <- (j*batch)
  } 
  
  db<- testSplit[start:end,]  #get batch for this round
  
  Pred.rf1.v <- predict(rf.test, db,type = "prob")
  customer.test[start:end,2] <-Pred.rf1.v[,2]
}

# ====== SMO+XGB ======
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
train.bal$TA_LOAN<-as.character(train.bal$TA_LOAN)
train.bal$TA_LOAN<-as.numeric(train.bal$TA_LOAN)

#---change data type for XGB----
gg=matrix(round(unlist(train.bal),3),length(train.bal[,1]),ncol(loan))
colnames(gg)=names(train.bal)
#head(train.bal,2)
#head(train.bal[,1])

TA<-gg[,1]
TAlist<-unlist(TA)
ggdata<-gg[,-c(1,2)]  # TA_LOAN;ROW_ID

trainList<-list(data=ggdata,label=TAlist)
dtrain<-xgb.DMatrix(data=trainList$data,label=trainList$label)

xgbCorrect<- xgboost(data=dtrain, eta = 0.2,
                     max_depth = 40,
                     gamma = 9,
                     nround=30, 
                     subsample = 0.6,
                     max_delta_step =5, #1
                     colsample_bytree = 0.9,
                     objective =  "binary:logistic",
                     nthread = 6,
                     verbose = 0
)

# ==for auto-batch==
for (j in  1: ceiling(nrow(testSplit)/batch) ) {
  rm(db)
  start <- (j-1)*batch+1
  if(j == ceiling(nrow(testSplit)/batch)){
    end <- nrow(testSplit)
  }else{
    end <- (j*batch)
  } 
  
  db<- testSplit[start:end,]  #get batch for this round
  gg.v=matrix(round(unlist(db),3),nrow(db),ncol(db))
  
  colnames(gg.v)=names(testSplit)
  ggdata.v<-gg.v[,-c(1)]      #take out ROW_ID
  validList<-list(data=ggdata.v)
  x.pred.v <- predict(xgbCorrect, as.matrix(validList$data))
  
  #customer.test[start:end,1]<- gg.v[,1]
  customer.test[start:end,3]<-x.pred.v
}

        
        # ===$$$ Ensemble Count====
        new <- matrix(NA,nrow = nrow(customer.test),ncol = 1)
        colnames(new)<-"TA_LOAN"
        ensem <- cbind(customer.test,new)
        ensem[,4]<- as.numeric(as.character(trainSplit[,1]) ) 
        # ===ensemble algo.=====
        ensem <- as.data.frame(ensem)
        #fit <- glm(TA_LOAN~.-ID,data=ensem,family=binomial())
        fit <- glm(TA_LOAN~.-ID,data=ensem)
        summary(fit) # display results
        confint(fit) # 95% CI for the coefficients
        #exp(coef(fit)) # exponentiated coefficients


# =========$$$true.test=============
testSplit <- validSplit[,2:29]
# ====== set batch ===
batch <- 50000
# ======= Collect Result (customer.test) =======
customer.t <- matrix(0,nrow(testSplit),3)
colnames(customer.t)<-c("ID","SMO+RF","SMO+XGB")

# ============== $$ SMOTE+ $$==============

#==(test) (rf) batch for auto==
for (j in  1: ceiling(nrow(testSplit)/batch) ) {
  rm(db)
  start <- (j-1)*batch+1
  if(j == ceiling(nrow(testSplit)/batch)){
    end <- nrow(testSplit)
  }else{
    end <- (j*batch)
  } 
  
  db<- testSplit[start:end,]  #get batch for this round
  
  Pred.rf1.v <- predict(rf.test, db,type = "prob")
  customer.t[start:end,2] <-Pred.rf1.v[,2]
}

# ====== SMO+XGB ======
# == (test) for auto-batch==
for (j in  1: ceiling(nrow(testSplit)/batch) ) {
  rm(db)
  start <- (j-1)*batch+1
  if(j == ceiling(nrow(testSplit)/batch)){
    end <- nrow(testSplit)
  }else{
    end <- (j*batch)
  } 
  
  db<- testSplit[start:end,]  #get batch for this round
  gg.v=matrix(round(unlist(db),3),nrow(db),ncol(db))
  
  colnames(gg.v)=names(testSplit)
  ggdata.v<-gg.v[,-c(1)]      #take out ROW_ID
  validList<-list(data=ggdata.v)
  x.pred.v <- predict(xgbCorrect, as.matrix(validList$data))
  
  #customer.test[start:end,1]<- gg.v[,1] 
  # -----------------------///need change---------------------------
  customer.t[start:end,1]<-validSplit[start:end,2] #ID
  customer.t[start:end,3]<-x.pred.v
}



# ===$$$ Ensemble Count====
new <- matrix(NA,nrow = nrow(customer.t),ncol = 1)
colnames(new)<-"TA_LOAN"
ensem.t <- cbind(customer.t,new)
# -----------------------------------///need change----------------------------
ensem.t[,4]<- as.numeric(as.character(validSplit[,1]) ) 

# ===ensemble.test =====
ensem.t <- as.data.frame(ensem.t)
#fit <- glm(TA_LOAN~.-ID,data=ensem,family=binomial())
predict(fit,newdata=ensem.t)

# =====ensemble pred===== 
en.res <- matrix(NA,nrow = nrow(customer.t),ncol = 3)
colnames(en.res)<-c("ID","En.Score","TA_LOAN")
en.res[,1]<-ensem.t[,1]
en.res[,3]<-ensem.t[,4] #TA_LOAN
en.res[,2]<-predict(fit,ensem.t)   # predicted values
#residuals(fit, type="deviance") # residuals
#en.res[,2] <-((ensem[,2]+ensem[,3]+ensem[,4] )/3)


library(ggplot2)
library(plyr)
library(dplyr)

a<-as.integer(nrow(ensem.t)*0.05)
b<-as.integer(nrow(ensem.t)*0.10)
c<-as.integer(nrow(ensem.t)*0.15)
d<-as.integer(nrow(ensem.t)*0.20)
e<-as.integer(nrow(ensem.t)*0.25)
f<-as.integer(nrow(ensem.t)*0.30)
g<-as.integer(nrow(ensem.t)*0.35)
h<-as.integer(nrow(ensem.t)*0.40)
i<-as.integer(nrow(ensem.t)*0.45)
j<-as.integer(nrow(ensem.t)*0.50)

md1 <- as.data.frame(en.res)
# -----Continuous-----
CntR<-function(data,top){
  ar <-arrange(data,desc(En.Score))
  sub <- ar[1:top,]
  table(sub$TA_LOAN)
  rate<-table(sub$TA_LOAN)[2]/top
  return(rate)
}

TopRank<-matrix(NA,nrow = 1,ncol = 10)
colnames(TopRank)<-c("5%","10%","15%","20%","25%","30%","35%","40%","45%","50%")
rownames(TopRank)<-c("En.Score")

#=====top 5%======
TopRank[1,1]<-CntR(md1,a)
#=====top 10%======
TopRank[1,2]<-CntR(md1,b)
#======top 15%======
TopRank[1,3]<-CntR(md1,c)
#======top 20%======
TopRank[1,4]<-CntR(md1,d)
#=====top 25%======
TopRank[1,5]<-CntR(md1,e)
#======top 30%======
TopRank[1,6]<-CntR(md1,f)
#=====top 35%======
TopRank[1,7]<-CntR(md1,g)
#======top 40%======
TopRank[1,8]<-CntR(md1,h)
#======top 45%======
TopRank[1,9]<-CntR(md1,i)
#======top 50%======
TopRank[1,10]<-CntR(md1,j)


# ======TopRankC=====
TopRankC<-matrix(NA,nrow = 1,ncol = 10)
colnames(TopRankC)<-c("5%","10%","15%","20%","25%","30%","35%","40%","45%","50%")
rownames(TopRankC)<-c("En.Score")
TopRankC<-TopRank

write.table(TopRankC,file = "EntopRankC.csv",sep = ",")

########################################################################################

# -----Not Cont.-----
CntR<-function(data,top){
  xx <- top-(nrow(ensem.t)*0.05)+1
  ar <-arrange(data,desc(En.Score))
  sub <- ar[(xx:top),]
  table(sub$TA_LOAN)
  rate<-table(sub$TA_LOAN)[2]/ (top-xx)
  return(rate)
}


# ======TopRank=====
TopRank<-matrix(NA,nrow = 1,ncol = 10)
colnames(TopRank)<-c("5%","10%","15%","20%","25%","30%","35%","40%","45%","50%")
rownames(TopRank)<-c("En.Score")

#======top 5%======
TopRank[1,1]<-CntR(md1,a)
#======top 10%======
TopRank[1,2]<-CntR(md1,b)
#======top 15%======
TopRank[1,3]<-CntR(md1,c)
#======top 20%======
TopRank[1,4]<-CntR(md1,d)
#======top 25%======
TopRank[1,5]<-CntR(md1,e)
#======top 30%======
TopRank[1,6]<-CntR(md1,f)
#======top 35%======
TopRank[1,7]<-CntR(md1,g)
#======top 40%======
TopRank[1,8]<-CntR(md1,h)
#=====top 45%======
TopRank[1,9]<-CntR(md1,i)
#======top 50%======
TopRank[1,10]<-CntR(md1,j)

write.table(TopRank,file = "EntopRank.csv",sep = ",")
