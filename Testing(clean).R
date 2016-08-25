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
   # rm(validSplit)

# ====== Set testSplit=======
    true.test<-testSplit
    testSplit<-testSplit[,2:29]
# ====== set batch ===
    batch <- 5000
# ======= Collect Result (customer.test) =======
#==batch for auto==
    customer.test <- matrix(0,nrow(testSplit),4)
    colnames(customer.test)<-c("ID","R+XGB","SMO+RF","SMO+XGB")
# ============== $$ ROSE+ $$==============
    library(ROSE)
    set.seed(123)
    R.train.bal <- ovun.sample(TA_LOAN ~ ., data =trainSplit, method = "both",N = 4000)$data
    #table(R.train.bal$TA_LOAN)
    #str(R.train.bal)
    
    # ====== R+XGB ======
    library(xgboost)
    library(readr)
    library(stringr)
    library(caret)
    library(car)
    R.train.bal$TA_LOAN<-as.character(R.train.bal$TA_LOAN)
    R.train.bal$TA_LOAN<-as.numeric(R.train.bal$TA_LOAN)
    
    # ---change data type for XGB----
    gg=matrix(round(unlist(R.train.bal),3),length(R.train.bal[,1]),ncol(loan))
    colnames(gg)=names(R.train.bal)

    TA<-gg[,1]
    TAlist<-unlist(TA)
    # TA_LOAN;ROW_ID
    ggdata<-gg[,-c(1,2)]
    
    trainList<-list(data=ggdata,label=TAlist)
    dtrain<-xgb.DMatrix(data=trainList$data,label=trainList$label)
    
    xgbCorrect<- xgboost(data=dtrain, eta = 0.1,
                         max_depth = 3, #default=6 (3-10)
                         gamma = 1,
                         nround=50, 
                         subsample = 0.8, #(0.5-1) # 8,9,10
                         max_delta_step = 30, # 30,40
                         colsample_bytree = 0.3, #
                         objective =  "binary:logistic",
                         nthread = 1,verbose = 0
    )
    
    # ==for auto-batch==
    for (j in  1: ceiling(nrow(testSplit)/batch) ) {
      
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
      
      customer.test[start:end,1]<- gg.v[,1]
      customer.test[start:end,2]<-x.pred.v
      rm(db)
    }
    
    
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
    # ==batch for auto==
    for (j in  1: ceiling(nrow(testSplit)/batch) ) {
    
      start <- (j-1)*batch+1
      if(j == ceiling(nrow(testSplit)/batch)){
        end <- nrow(testSplit)
      }else{
        end <- (j*batch)
      } 
      
      db<- testSplit[start:end,]  #get batch for this round
      
      Pred.rf1.v <- predict(rf.test, db,type = "prob")
      customer.test[start:end,3] <-Pred.rf1.v[,2]
      rm(db)
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
      customer.test[start:end,4]<-x.pred.v
      
      rm(db)
    }
    
    
    write.table(customer.test,file = ("3scores.csv"),sep = ",")