mydata <- read.csv("cip_loan_top29.csv",stringsAsFactors = F, header = TRUE)
loan<-mydata
loan$TA_LOAN <-as.numeric(loan$TA_LOAN)

library(dplyr)
tb_loan <- tbl_df(loan)

head(tb_loan,4)


# ====boostrap=====
  d0<-filter(tb_loan,TA_LOAN==0)
  d1<-filter(tb_loan,TA_LOAN==1)
  
  d0<-as.data.frame(d0)
  colnames(d0)<-names(d1)
  d1<-as.data.frame(d1)
  colnames(d1)<-names(d0)
  
  
  P.value<-matrix(data = NA,nrow=1,ncol=ncol(d0))
  
  for (i in 3:ncol(d0)) {
     res <-t.test((d0[,i]),(d1[,i]))
     P.value[i]<-res[3]
  }
  
  P.value <- as.data.frame(P.value)
  colnames(P.value)<-names(d0)
  add <- matrix(NA,nrow = 4,ncol = (ncol(d0))
  colnames(add)<-names(d0)
  P.value<-rbind(P.value,add)
  rownames(P.value)<-c("pvalue","a/2","Sig.","Group0Mean","Group1Mean")
  # ===set CI==
  a<-0.05
  sig<-a/2
  P.value[2,]<-sig
  
  for (i in 3:ncol(d0)) {
    if (P.value[2,i]-P.value[1,i]>0){
      P.value[3,i]<-1  #"Sig."
    }else{
      P.value[3,i]<-0  #"No. Sig."
    }
  }
  
  P.value[4,]<- colMeans(d0)
  P.value[5,]<- colMeans(d1)
  
  str(P.value)
  
  write.table(P.value,file = "P.value.csv",sep = ",")
  
  