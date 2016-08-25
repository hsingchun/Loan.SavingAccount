library(ggplot2)
library(plyr)
library(dplyr)

md1<-read.csv("0+C50.csv",header = T)
md2<-read.csv("0+RF.csv",header = T)
md3<-read.csv("0+XGB.csv",header = T)
md4<-read.csv("R+C50.csv",header = T)
md5<-read.csv("R+RF.csv",header = T)
md6<-read.csv("R+XGB.csv",header = T)
md7<-read.csv("SMO+C50.csv",header = T)
md8<-read.csv("SMO+RF.csv",header = T)
md9<-read.csv("SMO+XGB.csv",header = T)


a<-as.integer(80006*0.05)
b<-as.integer(80006*0.10)
c<-as.integer(80006*0.15)
d<-as.integer(80006*0.20)
e<-as.integer(80006*0.25)
f<-as.integer(80006*0.30)
g<-as.integer(80006*0.35)
h<-as.integer(80006*0.40)
i<-as.integer(80006*0.45)
j<-as.integer(80006*0.50)

# -----Continuous-----
CntR<-function(data,top){
  ar <-arrange(data,desc(PredictProb))
  sub <- ar[1:top,]
  table(sub$TA_LOAN)
  rate<-table(sub$TA_LOAN)[2]/top
  return(rate)
}

TopRank<-matrix(NA,nrow = 9,ncol = 10)
colnames(TopRank)<-c("5%","10%","15%","20%","25%","30%","35%","40%","45%","50%")
rownames(TopRank)<-c("0+C50","0+RF","0+XGB","R+C50","R+RF","R+XGB","SMO+C50","SMO+RF","SMO+XGB")

# ======top 5%======
TopRank[1,1]<-CntR(md1,a)
TopRank[2,1]<-CntR(md2,a)
TopRank[3,1]<-CntR(md3,a)
TopRank[4,1]<-CntR(md4,a)
TopRank[5,1]<-CntR(md5,a)
TopRank[6,1]<-CntR(md6,a)
TopRank[7,1]<-CntR(md7,a)
TopRank[8,1]<-CntR(md8,a)
TopRank[9,1]<-CntR(md9,a)

# ======top 10%======
TopRank[1,2]<-CntR(md1,b)
TopRank[2,2]<-CntR(md2,b)
TopRank[3,2]<-CntR(md3,b)
TopRank[4,2]<-CntR(md4,b)
TopRank[5,2]<-CntR(md5,b)
TopRank[6,2]<-CntR(md6,b)
TopRank[7,2]<-CntR(md7,b)
TopRank[8,2]<-CntR(md8,b)
TopRank[9,2]<-CntR(md9,b)

# ======top 15%======
TopRank[1,3]<-CntR(md1,c)
TopRank[2,3]<-CntR(md2,c)
TopRank[3,3]<-CntR(md3,c)
TopRank[4,3]<-CntR(md4,c)
TopRank[5,3]<-CntR(md5,c)
TopRank[6,3]<-CntR(md6,c)
TopRank[7,3]<-CntR(md7,c)
TopRank[8,3]<-CntR(md8,c)
TopRank[9,3]<-CntR(md9,c)

# ======top 20%======
TopRank[1,4]<-CntR(md1,d)
TopRank[2,4]<-CntR(md2,d)
TopRank[3,4]<-CntR(md3,d)
TopRank[4,4]<-CntR(md4,d)
TopRank[5,4]<-CntR(md5,d)
TopRank[6,4]<-CntR(md6,d)
TopRank[7,4]<-CntR(md7,d)
TopRank[8,4]<-CntR(md8,d)
TopRank[9,4]<-CntR(md9,d)
# ======top 25%======
TopRank[1,5]<-CntR(md1,e)
TopRank[2,5]<-CntR(md2,e)
TopRank[3,5]<-CntR(md3,e)
TopRank[4,5]<-CntR(md4,e)
TopRank[5,5]<-CntR(md5,e)
TopRank[6,5]<-CntR(md6,e)
TopRank[7,5]<-CntR(md7,e)
TopRank[8,5]<-CntR(md8,e)
TopRank[9,5]<-CntR(md9,e)

# ======top 30%======
TopRank[1,6]<-CntR(md1,f)
TopRank[2,6]<-CntR(md2,f)
TopRank[3,6]<-CntR(md3,f)
TopRank[4,6]<-CntR(md4,f)
TopRank[5,6]<-CntR(md5,f)
TopRank[6,6]<-CntR(md6,f)
TopRank[7,6]<-CntR(md7,f)
TopRank[8,6]<-CntR(md8,f)
TopRank[9,6]<-CntR(md9,f)

# ======top 35%======
TopRank[1,7]<-CntR(md1,g)
TopRank[2,7]<-CntR(md2,g)
TopRank[3,7]<-CntR(md3,g)
TopRank[4,7]<-CntR(md4,g)
TopRank[5,7]<-CntR(md5,g)
TopRank[6,7]<-CntR(md6,g)
TopRank[7,7]<-CntR(md7,g)
TopRank[8,7]<-CntR(md8,g)
TopRank[9,7]<-CntR(md9,g)

# ======top 40%======
TopRank[1,8]<-CntR(md1,h)
TopRank[2,8]<-CntR(md2,h)
TopRank[3,8]<-CntR(md3,h)
TopRank[4,8]<-CntR(md4,h)
TopRank[5,8]<-CntR(md5,h)
TopRank[6,8]<-CntR(md6,h)
TopRank[7,8]<-CntR(md7,h)
TopRank[8,8]<-CntR(md8,h)
TopRank[9,8]<-CntR(md9,h)
# ======top 45%======
TopRank[1,9]<-CntR(md1,i)
TopRank[2,9]<-CntR(md2,i)
TopRank[3,9]<-CntR(md3,i)
TopRank[4,9]<-CntR(md4,i)
TopRank[5,9]<-CntR(md5,i)
TopRank[6,9]<-CntR(md6,i)
TopRank[7,9]<-CntR(md7,i)
TopRank[8,9]<-CntR(md8,i)
TopRank[9,9]<-CntR(md9,i)
# ======top 50%======
TopRank[1,10]<-CntR(md1,j)
TopRank[2,10]<-CntR(md2,j)
TopRank[3,10]<-CntR(md3,j)
TopRank[4,10]<-CntR(md4,j)
TopRank[5,10]<-CntR(md5,j)
TopRank[6,10]<-CntR(md6,j)
TopRank[7,10]<-CntR(md7,j)
TopRank[8,10]<-CntR(md8,j)
TopRank[9,10]<-CntR(md9,j)

# ======TopRankC=====
TopRankC<-matrix(NA,nrow = 9,ncol = 10)
colnames(TopRankC)<-c("5%","10%","15%","20%","25%","30%","35%","40%","45%","50%")
rownames(TopRankC)<-c("0+C50","0+RF","0+XGB","R+C50","R+RF","R+XGB","SMO+C50","SMO+RF","SMO+XGB")
TopRankC<-TopRank

write.table(TopRankC,file = "NtopRankC.csv",sep = ",")

########################################################################################

# -----Not Cont.-----
CntR<-function(data,top){
  xx <- top-(80006*0.05)+1
  ar <-arrange(data,desc(PredictProb))
  sub <- ar[(xx:top),]
  table(sub$TA_LOAN)
  rate<-table(sub$TA_LOAN)[2]/ (top-xx)
  return(rate)
}

# ======TopRank=====
TopRank<-matrix(NA,nrow = 9,ncol = 10)
colnames(TopRank)<-c("5%","10%","15%","20%","25%","30%","35%","40%","45%","50%")
rownames(TopRank)<-c("0+C50","0+RF","0+XGB","R+C50","R+RF","R+XGB","SMO+C50","SMO+RF","SMO+XGB")

# ======top 5%======
TopRank[1,1]<-CntR(md1,a)
TopRank[2,1]<-CntR(md2,a)
TopRank[3,1]<-CntR(md3,a)
TopRank[4,1]<-CntR(md4,a)
TopRank[5,1]<-CntR(md5,a)
TopRank[6,1]<-CntR(md6,a)
TopRank[7,1]<-CntR(md7,a)
TopRank[8,1]<-CntR(md8,a)
TopRank[9,1]<-CntR(md9,a)

# ======top 10%======
TopRank[1,2]<-CntR(md1,b)
TopRank[2,2]<-CntR(md2,b)
TopRank[3,2]<-CntR(md3,b)
TopRank[4,2]<-CntR(md4,b)
TopRank[5,2]<-CntR(md5,b)
TopRank[6,2]<-CntR(md6,b)
TopRank[7,2]<-CntR(md7,b)
TopRank[8,2]<-CntR(md8,b)
TopRank[9,2]<-CntR(md9,b)

# ======top 15%======
TopRank[1,3]<-CntR(md1,c)
TopRank[2,3]<-CntR(md2,c)
TopRank[3,3]<-CntR(md3,c)
TopRank[4,3]<-CntR(md4,c)
TopRank[5,3]<-CntR(md5,c)
TopRank[6,3]<-CntR(md6,c)
TopRank[7,3]<-CntR(md7,c)
TopRank[8,3]<-CntR(md8,c)
TopRank[9,3]<-CntR(md9,c)

# ======top 20%======
TopRank[1,4]<-CntR(md1,d)
TopRank[2,4]<-CntR(md2,d)
TopRank[3,4]<-CntR(md3,d)
TopRank[4,4]<-CntR(md4,d)
TopRank[5,4]<-CntR(md5,d)
TopRank[6,4]<-CntR(md6,d)
TopRank[7,4]<-CntR(md7,d)
TopRank[8,4]<-CntR(md8,d)
TopRank[9,4]<-CntR(md9,d)
# ======top 25%======
TopRank[1,5]<-CntR(md1,e)
TopRank[2,5]<-CntR(md2,e)
TopRank[3,5]<-CntR(md3,e)
TopRank[4,5]<-CntR(md4,e)
TopRank[5,5]<-CntR(md5,e)
TopRank[6,5]<-CntR(md6,e)
TopRank[7,5]<-CntR(md7,e)
TopRank[8,5]<-CntR(md8,e)
TopRank[9,5]<-CntR(md9,e)

# ======top 30%======
TopRank[1,6]<-CntR(md1,f)
TopRank[2,6]<-CntR(md2,f)
TopRank[3,6]<-CntR(md3,f)
TopRank[4,6]<-CntR(md4,f)
TopRank[5,6]<-CntR(md5,f)
TopRank[6,6]<-CntR(md6,f)
TopRank[7,6]<-CntR(md7,f)
TopRank[8,6]<-CntR(md8,f)
TopRank[9,6]<-CntR(md9,f)

# ======top 35%======
TopRank[1,7]<-CntR(md1,g)
TopRank[2,7]<-CntR(md2,g)
TopRank[3,7]<-CntR(md3,g)
TopRank[4,7]<-CntR(md4,g)
TopRank[5,7]<-CntR(md5,g)
TopRank[6,7]<-CntR(md6,g)
TopRank[7,7]<-CntR(md7,g)
TopRank[8,7]<-CntR(md8,g)
TopRank[9,7]<-CntR(md9,g)

# ======top 40%======
TopRank[1,8]<-CntR(md1,h)
TopRank[2,8]<-CntR(md2,h)
TopRank[3,8]<-CntR(md3,h)
TopRank[4,8]<-CntR(md4,h)
TopRank[5,8]<-CntR(md5,h)
TopRank[6,8]<-CntR(md6,h)
TopRank[7,8]<-CntR(md7,h)
TopRank[8,8]<-CntR(md8,h)
TopRank[9,8]<-CntR(md9,h)
# ======top 45%======
TopRank[1,9]<-CntR(md1,i)
TopRank[2,9]<-CntR(md2,i)
TopRank[3,9]<-CntR(md3,i)
TopRank[4,9]<-CntR(md4,i)
TopRank[5,9]<-CntR(md5,i)
TopRank[6,9]<-CntR(md6,i)
TopRank[7,9]<-CntR(md7,i)
TopRank[8,9]<-CntR(md8,i)
TopRank[9,9]<-CntR(md9,i)
# ======top 50%======
TopRank[1,10]<-CntR(md1,j)
TopRank[2,10]<-CntR(md2,j)
TopRank[3,10]<-CntR(md3,j)
TopRank[4,10]<-CntR(md4,j)
TopRank[5,10]<-CntR(md5,j)
TopRank[6,10]<-CntR(md6,j)
TopRank[7,10]<-CntR(md7,j)
TopRank[8,10]<-CntR(md8,j)
TopRank[9,10]<-CntR(md9,j)

write.table(TopRank,file = "NtopRank.csv",sep = ",")

# =======PLOTS =======
#TRC<-read.csv("TopRankC.csv",header = T)
#TRC$percent<-as.character(TRC$percent)
#TRC$percent<-as.numeric(TRC$percent)
#TRC$rate<-as.numeric(TRC$rate)
#ggplot( data=TRC,aes(x = percent,y = rate,colour=method))+ geom_line(size=1.3)

#TR<-read.csv("TopRank.csv",header = T)
#TR$percent<-as.numeric(TR$percent)
#TR$rate<-as.numeric(TR$rate)
#ggplot( data=TR,aes(x = percent,y = rate,colour=method))+ geom_line(size=1.3)

