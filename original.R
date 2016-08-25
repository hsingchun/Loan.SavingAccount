# ============ ***SECTION 1=============
mydata <- read.csv("cip_loan_top55.csv", header = TRUE)

#install.packages("dplyr")
#https://cran.r-project.org/web/packages/dplyr/dplyr.pdf
library(dplyr)
loan<-tbl_df(mydata)
#loan<- select(loan,-CTA_IND,-RP_IND)


# ==================Find Missing Value ==================
#(count mv by col)
colSums(is.na(loan))
loan2 <-select(loan, -year_income)
colSums(is.na(loan))
substr(loan$EDUCATION_CODE,1,1)
loan.new <- mutate(loan,EDU=substr(loan$EDUCATION_CODE,1,1))
loan.new <-select(loan.new,-EDUCATION_CODE)
write.table(loan.new,file = "cip_loan_cln_EDU.csv",sep = ",")

loan <- loan.new
select(loan,EDU)
rm(loan.new)

#(remove all missing value row)
loan3 <-filter(loan,!is.na(L1_CNT),!is.na(L2_CNT),!is.na(year_income),!is.na(marital_status),
       !is.na(children_cnt),!is.na(GENDER_TYPE_CODE),!is.na(AGE))
#(remove year_income and all the other mv)
loan4 <-filter(loan,!is.na(L1_CNT),!is.na(L2_CNT),!is.na(marital_status),
               !is.na(children_cnt),!is.na(GENDER_TYPE_CODE),!is.na(AGE))
colSums(is.na(loan4))

# ================== split (training0.5 valid0.3 test0.2)==================
#install.packages("caret")
library(caret)
set.seed(1234)
splitIndex <- createDataPartition(loan4$TA_LOAN, p = .50,
                                  list = FALSE, times = 1)
trainSplit <- loan4[ splitIndex,]
#write.table(trainSplit,file = "cip_loan_train.csv",sep = ",")
print(table(trainSplit$TA_LOAN))
select(trainSplit,EDU)
colSums(is.na(trainSplit))

restSplit <-loan4[ -splitIndex,]
#write.table(restSplit,file = "cip_loan_rest.csv",sep = ",")
print(table(restSplit$TA_LOAN))

splitIndex1 <- createDataPartition(restSplit$TA_LOAN, p = .60,
                                   list = FALSE, times = 1)
validSplit <- restSplit[splitIndex1,]
print(table(validSplit$TA_LOAN))
#write.table(validSplit,file = "cip_loan_valid.csv",sep = ",")

testSplit <-  restSplit[-splitIndex1,]
print(table(testSplit$TA_LOAN))
#write.table(testSplit,file = "cip_loan_test.csv",sep = ",")

# ================= sampling (none)/ Tree C5.0 =================
#https://cran.r-project.org/web/packages/C50/C50.pdf
install.packages("C50")
library(C50)
ruleModel <- C5.0(TA_LOAN ~ ., data =trainSplit , rules = TRUE)
ruleModel
summary(ruleModel)
predict(ruleModel, head(validSplit))
#pred <-predict(ruleModel, validSplit, type = "prob")
pred <-predict(ruleModel, validSplit, type = "class")
confusion.matrix=table(Type=validSplit$TA_LOAN,predict=pred)
confusion.matrix

# ============ ***SECTION 2=============

# ============ Feature Select(Boruta) ================
#https://www.analyticsvidhya.com/blog/2016/03/select-important-variables-boruta-package/
#use loan4 > trainSplit contains year_income (missing values)
library(Boruta)
str(trainSplit)
summary(trainSplit)

# =================( let go of missing values)==================
#(remove year_income and all the other mv)
loan5 <-filter(loan,!is.na(L1_CNT),!is.na(L2_CNT),!is.na(marital_status),
               !is.na(year_income),!is.na(children_cnt),!is.na(GENDER_TYPE_CODE),!is.na(AGE))

# ================== split (training0.5 valid0.3 test0.2)==================
#install.packages("caret")
library(caret)
set.seed(1234)
splitIndex5 <- createDataPartition(loan5$TA_LOAN, p = .50,
                                  list = FALSE, times = 1)
trainSplit5 <- loan5[ splitIndex,]
print(table(trainSplit$TA_LOAN))
select(trainSplit,EDU)
colSums(is.na(trainSplit))

restSplit5 <-loan5[ -splitIndex,]
print(table(restSplit$TA_LOAN))

splitIndex5.1 <- createDataPartition(restSplit$TA_LOAN, p = .60,
                                   list = FALSE, times = 1)
validSplit5 <- restSplit[splitIndex5.1,]
print(table(validSplit$TA_LOAN))

testSplit5 <-  restSplit[-splitIndex5.1,]
print(table(testSplit$TA_LOAN))

# ======boruta(feature selection)=======
set.seed(123)
#   boruta.train <- Boruta(TA_LOAN~.-ROW_ID, data = trainSplit5, doTrace = 2)
print(boruta.train)


plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
       at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

getSelectedAttributes(final.boruta, withTentative = F)

boruta.df <- attStats(final.boruta)
class(boruta.df)
boruta.df

#step2
#library(caret)
#library(randomForest)
#set.seed(123)
#control <- rfeControl(functions=rfFuncs, method="cv", number=10)

# =====Top55 imp=====
loan6 <-select(loan, TA_LOAN,ROW_ID,ULN_CLA_USG_004
               ,CUS_RWB_TAC_001
               ,SAC_TRA_TRA_001
               ,Ass_PB_BAL_CHANGE_RATE
               ,PB_AMT
               ,ULN_BAL_BAL_004
               ,Y1_CHNL_Mybank
               ,OTH_Curr_Revol_BAL
               ,PB_TXN_CNT
               ,CUS_CHU_CHU_063
               ,AUM
               ,CUS_RWB_RWB_008
               ,BAL_VAR
               ,LIA_BAL_TBL_020
               ,recency
               ,PERIOD_BANK
               ,AGE
               ,SAC_WIT_WIT_002
               ,num_Mybank_Cnt
               ,NUM_Mobibank_Cnt
               ,R12Mth_Mybank_NTD_QRY_Cnt
               ,Y1_CHNL_L
               ,deduct_cnt
               ,Ass_AVG_STOCK_BAL
               ,PB_CNT
               ,Ass_Mth_F_DP_All_Bal
               ,LIA_BAL_TBL_021
               ,CUS_RWB_TAC_003
               ,EMAIL_Flag
               ,Active_Product_Count
               ,AVG_PAYMENT_AMT
               ,Curr_CREDIT_LOAN_BAL
               ,CUS_CDG_CDG_010
               ,MYBANK_LOAN1
               ,PBFX_TXN_CNT
               ,Credit_Loan_Bank_Cnt
               ,Mortgage_AMT
               ,Mortgage_Ratio
               ,calife_fx3_amt_twd
               ,CUS_RWB_BAL_002
               ,CUS_RWB_BAL_004
               ,num_sec_cnt
               ,ifx_txn_cnt
               ,Ass_INSURANCE_BAL
               ,TRF_SHOPPING
               ,ULN_CLA_LAM_001
               ,Personal_LB_Closed_Ind
               ,ALL_FEE
               ,Ass_Year_TW_TD_Avg_Bal
               ,Y1_SECUTIRY_TRACE
               ,CUS_PDH_LIB_013
               ,TRF_RLOAN
               ,CUS_PDH_LIB_012
)

write.table(loan6,file = "cip_loan_top55.csv",sep = ",")
loan<-read.csv("cip_loan_top55.csv",header = T)

# =====Top imp=====
loan7 <-select(loan, TA_LOAN,ROW_ID,ULN_CLA_USG_004
               ,CUS_RWB_RWB_008
               ,BAL_VAR
               ,AUM
               ,LIA_BAL_TBL_020
               ,PB_AMT
               ,Ass_PB_BAL_CHANGE_RATE
               ,CUS_CHU_CHU_063
               ,PB_TXN_CNT
               ,recency
               ,PERIOD_BANK
               ,AGE
               ,SAC_WIT_WIT_002
               ,num_Mybank_Cnt
               ,NUM_Mobibank_Cnt
               ,R12Mth_Mybank_NTD_QRY_Cnt
               ,Y1_CHNL_L
               ,Y1_CHNL_Mybank
               ,CUS_RWB_TAC_001
               ,deduct_cnt
               ,Ass_AVG_STOCK_BAL
               ,PB_CNT
               ,Ass_Mth_F_DP_All_Bal
               ,LIA_BAL_TBL_021
               ,SAC_TRA_TRA_001
               ,CUS_RWB_TAC_003
               ,EMAIL_Flag
               ,Active_Product_Count
)

write.table(loan7,file = "cip_loan_top30.csv",sep = ",")


# =====Top imp=====
loan8 <-select(loan, TA_LOAN,ROW_ID
               ,CUS_RWB_RWB_008
               ,AUM
               ,Ass_PB_BAL_CHANGE_RATE
               ,CUS_CHU_CHU_063
               ,recency
               ,AGE
               ,num_Mybank_Cnt
               ,NUM_Mobibank_Cnt
               ,Y1_CHNL_L
               ,PB_CNT
               ,CUS_RWB_TAC_003
               ,EMAIL_Flag
               ,Active_Product_Count
               ,MYBANK_LOAN1
               ,Credit_Loan_Bank_Cnt
               ,Mortgage_Ratio
               ,calife_fx3_amt_twd
               ,ifx_txn_cnt
               ,Personal_LB_Closed_Ind
               ,ULN_CLA_USG_004
               ,Y1_SECUTIRY_TRACE
               ,CUS_PDH_LIB_013
               ,TRF_RLOAN
)

write.table(loan8,file = "cip_loan_top25.csv",sep = ",")


# =====Top imp=====
loan8 <-select(loan, TA_LOAN,ROW_ID
               ,CUS_RWB_RWB_008
               ,BAL_VAR
               ,AUM
               ,LIA_BAL_TBL_020
               ,PB_AMT
               ,Ass_PB_BAL_CHANGE_RATE
               ,CUS_CHU_CHU_063
               ,PB_TXN_CNT
               ,recency
               ,PERIOD_BANK
               ,AGE
               ,SAC_WIT_WIT_002
               ,num_Mybank_Cnt
               ,NUM_Mobibank_Cnt
               ,R12Mth_Mybank_NTD_QRY_Cnt
               ,Y1_CHNL_L
               ,Y1_CHNL_Mybank
               ,CUS_RWB_TAC_001
               ,deduct_cnt
               ,Ass_AVG_STOCK_BAL
               ,PB_CNT
               ,Ass_Mth_F_DP_All_Bal
               ,LIA_BAL_TBL_021
               ,SAC_TRA_TRA_001
               ,CUS_RWB_TAC_003
               ,EMAIL_Flag
)

write.table(loan8,file = "cip_loan_top28.csv",sep = ",")
