# ============== $$ 0+ $$==============

# ================== 1.Load data ==================
mydata <- read.csv("cip_loan_top33.csv",stringsAsFactors = F, header = TRUE)
loan<-mydata
loan$TA_LOAN <-as.factor(loan$TA_LOAN)
# ================== 3.Split (training0.5 valid0.3 test0.2)==================
#install.packages("caret")
library(caret)
set.seed(1234)
splitIndex <- createDataPartition(loan$TA_LOAN, p = .50,list = FALSE, times = 1)
trainSplit <- loan[ splitIndex,]
print(table(trainSplit$TA_LOAN))

# small test splitTmp
splitTmp <- createDataPartition(trainSplit$TA_LOAN, p = .04,list = FALSE, times = 1)
trainSplit.test <- trainSplit[ splitTmp,]
print(table(trainSplit.test$TA_LOAN))


restSplit <-loan[ -splitIndex,]
print(table(restSplit$TA_LOAN))

splitIndex1 <- createDataPartition(restSplit$TA_LOAN, p = .60,list = FALSE, times = 1)
validSplit <- restSplit[splitIndex1,]
print(table(validSplit$TA_LOAN))

testSplit<-  restSplit[-splitIndex1,]
print(table(testSplit$TA_LOAN))

rm(restSplit)
rm(splitIndex)
rm(splitIndex1)

# ====== 0+C50 ======
library(C50)
t4.1 <- C5.0(TA_LOAN ~ .-ROW_ID, data =trainSplit , rules = TRUE,noGlobalPruning=TRUE)
t4.1

    #t4.1 test
validSplit$TA_LOAN<-as.factor(validSplit$TA_LOAN)
    pred4.1.v.p <-predict(t4.1, validSplit, type = "prob")
    head(pred4.1.v.p,3)
    #---Result matrix----
    res.tree<-matrix(data = NA,nrow = nrow(validSplit),ncol = 5)
    colnames(res.tree)=c("ID","TA_LOAN","PredictProb","Cutoff","Class")
    #------SET CUTOFF----
    Cutoff<-0.4
    prediction<-as.numeric(pred4.1.v.p[,2]>Cutoff)
    res.tree[,1]<-validSplit[,2]
    res.tree[,2]<-as.numeric(as.character(validSplit[,1]))
    res.tree[,3]<-pred4.1.v.p[,2]
    res.tree[,4]<-Cutoff
    res.tree[,5]<-prediction
    
    head(res.tree,10)
    
    #prediction<-as.numeric(pred4.1.v.p[,2]>Cutoff)
    #confusionMatrix(prediction,validSplit$TA_LOAN,positive = "1")
    
    write.table(res.tree,file = "0+C50.csv",sep = ",")

# ====== 0+RF ======
    library(caret)
    library(randomForest)
    # need to be data.frame
    rf.test <- randomForest(TA_LOAN ~ .-ROW_ID, data=trainSplit.test,type=2,min_sample_leaf=30
                            , ntree=80, proximity=TRUE,xtest=trainSplit.test[,-c(1,2)],
                            ytest=trainSplit.test$TA_LOAN,keep.forest=TRUE)
    rf.test
    # rf.test test   
    Pred.rf1.v <- predict(rf.test, validSplit,type = "prob")
    head(Pred.rf1.v,10)
    
    #---Result matrix----
    res.rf<-matrix(data = NA,nrow = nrow(validSplit),ncol = 5)
    colnames(res.rf)=c("ID","TA_LOAN","PredictProb","Cutoff","Class")
    #------SET CUTOFF----
    Cutoff<-0.3
    prediction<-as.numeric(Pred.rf1.v[,2]>Cutoff)
    res.rf[,1]<-validSplit[,2]
    res.rf[,2]<-as.numeric(as.character(validSplit[,1]))
    res.rf[,3]<-Pred.rf1.v[,2]
    res.rf[,4]<-Cutoff
    res.rf[,5]<-prediction
    
    head(res.rf,2)
    
    prediction<-as.numeric(Pred.rf1.v[,2]>Cutoff)
    confusionMatrix(prediction,validSplit$TA_LOAN,positive = "1")
    
    write.table(res.rf,file = "0+RF.csv",sep = ",")
    
# ====== 0+XGB ======
    table(validSplit$TA_LOAN)
    trainSplit$TA_LOAN<-as.character(trainSplit$TA_LOAN)
    trainSplit$TA_LOAN<-as.numeric(trainSplit$TA_LOAN)
    
    table(validSplit$TA_LOAN)
    validSplit$TA_LOAN<-as.character(validSplit$TA_LOAN)
    validSplit$TA_LOAN<-as.numeric(validSplit$TA_LOAN)
    
    library(xgboost)
    library(readr)
    library(stringr)
    library(caret)
    library(car)
    head(trainSplit,2)
    class(trainSplit)
    str(trainSplit)
    
    #=======correct data format for XGB======
    gg=matrix(round(unlist(trainSplit),3),length(trainSplit[,1]),ncol(loan))
    colnames(gg)=names(trainSplit)
    head(trainSplit,2)
    class(trainSplit)
    table(trainSplit[,1])
    
    TA<-gg[,1]
    TAlist<-unlist(TA)
    class(TAlist)
    # TA_LOAN;ROW_ID
    ggdata<-gg[,-c(1,2)]
    class(ggdata)
    trainList<-list(data=ggdata,label=TAlist)
    dtrain<-xgb.DMatrix(data=trainList$data,label=trainList$label)
    
    xgbCorrect<- xgboost(data=dtrain, eta = 0.1,
                         max_depth = 6, #default=6 (3-10)
                         gamma = 1,
                         nround=100, 
                         subsample = 0.8, #(0.5-1) # 8,9,10
                         max_delta_step = 40, # 30,40
                         colsample_bytree = 0.4, #
                         objective =  "binary:logistic",
                         nthread = 10, #6,10
                         scale_pos_weight =4, #1,47
                         verbose = 0)
    
    #xgb.plot.tree(trainList$data@TA_LOAN, model = xgbCorrect)
    
    # predict values in test set (test)
    gg.v=matrix(round(unlist(validSplit),3),length(validSplit[,1]),ncol(loan))
    colnames(gg.v)=names(validSplit)
    head(gg.v,2)
    table(gg.v[,1])
    
    TA.v<-gg.v[,1]
    TAlist.v<-unlist(TA.v)
    # TA_LOAN;ROW_ID
    ggdata.v<-gg.v[,-c(1,2)]
    #ggdata.v<-gg.v
    validList<-list(data=ggdata.v,label=TAlist.v)
    #dvalid<-xgb.DMatrix(data=validList$data,label=validList$label)
    
    #-------------$$$$$$$$$$$$$$$$$$$$$  Tuning2---------------
    x.pred.v <- predict(xgbCorrect, as.matrix(validList$data))
    head(x.pred.v,2)
    #---Result matrix----
    res.XGB<-matrix(data = NA,nrow = nrow(validSplit),ncol = 5)
    colnames(res.XGB)=c("ID","TA_LOAN","PredictProb","Cutoff","Class")
    #------SET CUTOFF----
    Cutoff<-0.3
    prediction<-as.numeric(x.pred.v>Cutoff)
    res.XGB[,1]<-gg.v[,2]
    res.XGB[,2]<-gg.v[,1]
    res.XGB[,3]<-x.pred.v
    res.XGB[,4]<-Cutoff
    res.XGB[,5]<-prediction
    head(res.XGB,2)

    write.table(res.XGB,file = "0+XGB.csv",sep = ",")
    
# ============== $$ ROSE+ $$==============
    # ================== 1.Load data ==================
    loan<-mydata
    loan$TA_LOAN <-as.factor(loan$TA_LOAN)
    # ================== 3.Split (training0.5 valid0.3 test0.2)==================
    #install.packages("caret")
    library(caret)
    set.seed(1234)
    splitIndex <- createDataPartition(loan$TA_LOAN, p = .50,list = FALSE, times = 1)
    trainSplit <- loan[ splitIndex,]
    print(table(trainSplit$TA_LOAN))
    
    # small test splitTmp
    splitTmp <- createDataPartition(trainSplit$TA_LOAN, p = .04,list = FALSE, times = 1)
    trainSplit.test <- trainSplit[ splitTmp,]
    print(table(trainSplit.test$TA_LOAN))
    
    restSplit <-loan[ -splitIndex,]
    print(table(restSplit$TA_LOAN))
    
    splitIndex1 <- createDataPartition(restSplit$TA_LOAN, p = .60,list = FALSE, times = 1)
    validSplit <- restSplit[splitIndex1,]
    print(table(validSplit$TA_LOAN))
    
    testSplit <-  restSplit[-splitIndex1,]
    print(table(testSplit$TA_LOAN))
    
    rm(restSplit)
    rm(splitIndex)
    rm(splitIndex1)
    # ====== R+C50 ======
    library(ROSE)
    set.seed(123)
    train.bal <- ovun.sample(TA_LOAN ~ ., data =trainSplit, method = "both",N = 4000)$data
    table(train.bal$TA_LOAN)
    str(train.bal)
    
    library(C50)
    t5.1 <- C5.0(TA_LOAN ~.-ROW_ID, data =train.bal ,noGlobalPruning=TRUE)
    t5.1
    
    validSplit$TA_LOAN<-as.factor(validSplit$TA_LOAN)
    pred5.1 <-predict(t5.1, validSplit, type = "prob")
    head(pred5.1,3)
    #---Result matrix----
    res.tree<-matrix(data = NA,nrow = nrow(validSplit),ncol = 5)
    colnames(res.tree)=c("ID","TA_LOAN","PredictProb","Cutoff","Class")
    #------SET CUTOFF----
    Cutoff<-0.4
    prediction<-as.numeric(pred5.1[,2]>Cutoff)
    res.tree[,1]<-validSplit[,2]
    res.tree[,2]<-as.numeric(as.character(validSplit[,1]))
    res.tree[,3]<-pred5.1[,2]
    res.tree[,4]<-Cutoff
    res.tree[,5]<-prediction
    
    head(res.tree,2)
    
    #prediction<-as.numeric(pred5.1[,2]>Cutoff)
    #confusionMatrix(prediction,validSplit$TA_LOAN,positive = "1")
    
    write.table(res.tree,file = "R+C50.csv",sep = ",")
    
    # ====== R+RF ======
    rf.test <- randomForest(TA_LOAN ~ .-ROW_ID, data=train.bal,type=2,min_sample_leaf=100
                            , ntree=80, proximity=TRUE,xtest=train.bal[,-c(1,2)],
                            ytest=train.bal$TA_LOAN,keep.forest=TRUE)
    rf.test
    # rf.test 
    Pred.rf1.v <- predict(rf.test, validSplit,type = "prob")
    head(Pred.rf1.v,10)
    #---Result matrix----
    res.rf<-matrix(data = NA,nrow = nrow(validSplit),ncol = 5)
    colnames(res.rf)=c("ID","TA_LOAN","PredictProb","Cutoff","Class")
    #------SET CUTOFF----
    Cutoff<-0.4
    prediction<-as.numeric(Pred.rf1.v[,2]>Cutoff)
    res.rf[,1]<-validSplit[,2]
    res.rf[,2]<-as.numeric(as.character(validSplit[,1]))
    res.rf[,3]<-Pred.rf1.v[,2]
    res.rf[,4]<-Cutoff
    res.rf[,5]<-prediction
    
    head(res.rf,2)
    
    prediction<-as.numeric(Pred.rf1.v[,2]>Cutoff)
    confusionMatrix(prediction,validSplit$TA_LOAN,positive = "1")
    
    write.table(res.rf,file = "R+RF.csv",sep = ",")
    # ====== R+XGB ======
    train.bal$TA_LOAN<-as.character(train.bal$TA_LOAN)
    train.bal$TA_LOAN<-as.numeric(train.bal$TA_LOAN)
    
    table(validSplit$TA_LOAN)
    validSplit$TA_LOAN<-as.character(validSplit$TA_LOAN)
    validSplit$TA_LOAN<-as.numeric(validSplit$TA_LOAN)
    
    # ---change data type for XGB----
    gg=matrix(round(unlist(train.bal),3),length(train.bal[,1]),ncol(loan))
    colnames(gg)=names(train.bal)
    head(train.bal,2)
    table(train.bal[,1])
    
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
                         #type="response" 
    )
    #testing
    gg.v=matrix(round(unlist(validSplit),3),length(validSplit[,1]),ncol(loan))
    colnames(gg.v)=names(validSplit)
    head(gg.v,2)
    table(gg.v[,1])
    
    TA.v<-gg.v[,1]
    TAlist.v<-unlist(TA.v)
    # TA_LOAN;ROW_ID
    ggdata.v<-gg.v[,-c(1,2)]
    #ggdata.v<-gg.v
    validList<-list(data=ggdata.v,label=TAlist.v)
    #dvalid<-xgb.DMatrix(data=validList$data,label=validList$label)
    x.pred.v <- predict(xgbCorrect, as.matrix(validList$data))
    #---Result matrix----
    res.XGB<-matrix(data = NA,nrow = nrow(validSplit),ncol = 5)
    colnames(res.XGB)=c("ID","TA_LOAN","PredictProb","Cutoff","Class")
    #------SET CUTOFF----
    Cutoff<-0.3
    prediction<-as.numeric(x.pred.v>Cutoff)
    res.XGB[,1]<-gg.v[,2]
    res.XGB[,2]<-gg.v[,1]
    res.XGB[,3]<-x.pred.v
    res.XGB[,4]<-Cutoff
    res.XGB[,5]<-prediction
    
    head(res.XGB,2)
    
    prediction<-as.numeric(x.pred.v>Cutoff)
    confusionMatrix(prediction,validSplit$TA_LOAN,positive = "1")
    table(validList$label)
    
    write.table(res.XGB,file = "R+XGB.csv",sep = ",")
    
# ============== $$ SMOTE+ $$==============
    # ================== 1.Load data ==================
    loan<-mydata
    loan$TA_LOAN <-as.factor(loan$TA_LOAN)
    # ================== 3.Split (training0.5 valid0.3 test0.2)==================
    #install.packages("caret")
    library(caret)
    set.seed(1234)
    splitIndex <- createDataPartition(loan$TA_LOAN, p = .50,list = FALSE, times = 1)
    trainSplit <- loan[ splitIndex,]
    print(table(trainSplit$TA_LOAN))
    
    # small test splitTmp
    splitTmp <- createDataPartition(trainSplit$TA_LOAN, p = .04,list = FALSE, times = 1)
    trainSplit.test <- trainSplit[ splitTmp,]
    print(table(trainSplit.test$TA_LOAN))
    
    restSplit <-loan[ -splitIndex,]
    print(table(restSplit$TA_LOAN))
    
    splitIndex1 <- createDataPartition(restSplit$TA_LOAN, p = .60,list = FALSE, times = 1)
    validSplit <- restSplit[splitIndex1,]
    print(table(validSplit$TA_LOAN))
    
    testSplit <-  restSplit[-splitIndex1,]
    print(table(testSplit$TA_LOAN))
    
    rm(restSplit)
    rm(splitIndex)
    rm(splitIndex1)

    #SMOTE
    library(DMwR)
    ## checking the class distribution of this artificial data set
    table(trainSplit$TA_LOAN)
    ## now using SMOTE to create a more "balanced problem"
    str(trainSplit)
    set.seed(1234)
    newData <- SMOTE(TA_LOAN ~ .-ROW_ID,data=trainSplit, perc.over = 40,perc.under=350)
    table(newData$TA_LOAN)
    
    train.bal<-newData
    # ====== SMO+C50 ======
    classTree <- SMOTE(TA_LOAN ~ .-ROW_ID, data=train.bal, perc.over = 40,perc.under=350,
                       learner='rpartXse',se=0.5)
    # tclassTree  test
    predS<-predict(classTree,validSplit, type = "prob")
    head(predS,3)
    #--Result matrix----
    res.tree<-matrix(data = NA,nrow = nrow(validSplit),ncol = 5)
    colnames(res.tree)=c("ID","TA_LOAN","PredictProb","Cutoff","Class")
    #------SET CUTOFF----
    Cutoff<-0.5
    prediction<-as.numeric(pred5.1[,2]>Cutoff)
    res.tree[,1]<-validSplit[,2]
    res.tree[,2]<-as.numeric(as.character(validSplit[,1]))
    res.tree[,3]<-predS[,2]
    res.tree[,4]<-Cutoff
    res.tree[,5]<-prediction
    
    head(res.tree,2)
    
    #prediction<-as.numeric(predS[,2]>Cutoff)
    #confusionMatrix(prediction,validSplit$TA_LOAN,positive = "1")
    
    write.table(res.tree,file = "SMO+C50.csv",sep = ",")
    
    # ====== SMO+RF ======
    library(randomForest)
    rf.test <- randomForest(TA_LOAN ~ .-ROW_ID, data=train.bal,type=1,min_sample_leaf=30
                            , ntree=80, proximity=TRUE,xtest=train.bal[,-c(1,2)],
                            ytest=train.bal$TA_LOAN,keep.forest=TRUE)
   
    

    
    Pred.rf1.v <- predict(rf.test, validSplit,type = "prob")
    head(Pred.rf1.v,10)
    
    #---Result matrix----
    res.rf<-matrix(data = NA,nrow = nrow(validSplit),ncol = 5)
    colnames(res.rf)=c("ID","TA_LOAN","PredictProb","Cutoff","Class")
    #------SET CUTOFF----
    Cutoff<-0.5
    prediction<-as.numeric(Pred.rf1.v[,2]>Cutoff)
    res.rf[,1]<-validSplit[,2]
    res.rf[,2]<-as.numeric(as.character(validSplit[,1]))
    res.rf[,3]<-Pred.rf1.v[,2]
    res.rf[,4]<-Cutoff
    res.rf[,5]<-prediction
    
    head(res.rf,2)
    
    prediction<-as.numeric(Pred.rf1.v[,2]>Cutoff)
    confusionMatrix(prediction,validSplit$TA_LOAN,positive = "1")
    
    write.table(res.rf,file = "SMO+RF.csv",sep = ",")
    
    # ====== SMO+XGB ======
    library(xgboost)
    library(readr)
    library(stringr)
    library(caret)
    library(car)
    train.bal<-newData
    train.bal$TA_LOAN<-as.character(train.bal$TA_LOAN)
    train.bal$TA_LOAN<-as.numeric(train.bal$TA_LOAN)
    
    table(validSplit$TA_LOAN)
    validSplit$TA_LOAN<-as.character(validSplit$TA_LOAN)
    validSplit$TA_LOAN<-as.numeric(validSplit$TA_LOAN)
    
    #---change data type for XGB----
    gg=matrix(round(unlist(train.bal),3),length(train.bal[,1]),ncol(loan))
    colnames(gg)=names(train.bal)
    head(train.bal,2)
    head(train.bal[,1])
    
    TA<-gg[,1]
    TAlist<-unlist(TA)
    # TA_LOAN;ROW_ID
    ggdata<-gg[,-c(1,2)]
    
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
    #testing
    gg.v=matrix(round(unlist(validSplit),3),length(validSplit[,1]),ncol(loan))
    colnames(gg.v)=names(validSplit)
    head(gg.v,2)
    table(gg.v[,1])
    
    TA.v<-gg.v[,1]
    TAlist.v<-unlist(TA.v)
    # TA_LOAN;ROW_ID
    ggdata.v<-gg.v[,-c(1,2)]
    #ggdata.v<-gg.v
    validList<-list(data=ggdata.v,label=TAlist.v)
    #dvalid<-xgb.DMatrix(data=validList$data,label=validList$label)
    x.pred.v <- predict(xgbCorrect, as.matrix(validList$data))
    #--Result matrix----
    res.XGB<-matrix(data = NA,nrow = nrow(validSplit),ncol = 5)
    colnames(res.XGB)=c("ID","TA_LOAN","PredictProb","Cutoff","Class")
    #------SET CUTOFF----
    Cutoff<-0.3
    prediction<-as.numeric(x.pred.v>Cutoff)
    res.XGB[,1]<-gg.v[,2]
    res.XGB[,2]<-gg.v[,1]
    res.XGB[,3]<-x.pred.v
    res.XGB[,4]<-Cutoff
    res.XGB[,5]<-prediction
    
    head(res.XGB,2)
    
    #prediction<-as.numeric(x.pred.v>Cutoff)
    #confusionMatrix(prediction,validSplit$TA_LOAN,positive = "1")
    table(validList$label)
    
    write.table(res.XGB,file = "SMO+XGB.csv",sep = ",")
    
    #xgb.plot.tree(model = xgbCorrect, feature_names = validList$label)
    