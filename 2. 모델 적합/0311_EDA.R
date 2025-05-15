setwd("D:/24_1/2024 고용조사패널 공모전")
library(ggplot2)
library(reshape2)

total_df<-read.csv("final_df_0331.csv")
#1. <표1> 연도에 따른 취업자의 비율
##취업유무, 성별
num_table<-table(total_df$성별,total_df$취업유무,total_df$조사연도)

a<-table(total_df$조사연도)
b<-rep(a,each=4)
total_table<-array(b,dim=c(2,2,5))

prob_table<-round(num_table/total_table*100,1) #비율 표

total_table2<-matrix(0:19,nrow=10,ncol=2)

row<-c(1,2,1,2,3,4,3,4,5,6,5,6,7,8,7,8,9,10,9,10)
col<-c(1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2)
for (i in 1:20){
    total_table2[row[i],col[i]]<-paste0(num_table[i]," (",prob_table[i],")")
    }

write.csv(total_table2,file="table1.csv",fileEncoding="cp949")

row<-c(1,1,2,2,3,3,4,4,5,5)
col<-rep(c(1,2),5)
total_table3<-matrix(0:9,nrow=5,ncol=2)

num=0;prob=0
for (i in 1:20){
  #짝수일 때
  if (i%%2==0) {
    
    num=num+num_table[i]
    prob=prob+prob_table[i]
    i=i/2
    total_table3[row[i],col[i]]<-paste0(num," (",prob,")")
    num=0
    prob=0
    }
  else{
    num=num+num_table[i]
    prob=prob+prob_table[i]
  }
  }
write.csv(total_table3,file="table1_1.csv",fileEncoding="cp949")

#2. <표2>각 변수의 비율
year_table<-table(total_df$조사연도)


total_table<-matrix(ncol=5)
total_table_num<-matrix(ncol=5)

colnames(total_table)<-c(2016,2017,2018,2019,2020)
colnames(total_table_num)<-c(2016,2017,2018,2019,2020)

make_table<-function(total_table,var,opt){
  if (opt=="prop"){
    prop<-as.data.frame.matrix(table(total_df[,var], total_df$조사연도)/c(year_table))
    
    a<-round(prop,3)*100
    b<-paste0(var,rownames(a))
    rownames(a)<-b
    
    total_table<-rbind(total_table,a)
    return(total_table)
  }
  
  else{
    a<-as.data.frame.matrix(table(total_df[,var], total_df$조사연도))
    
    b<-paste0(var,rownames(a))
    rownames(a)<-b
    
    total_table<-rbind(total_table,a)
    return(total_table)
  }
 
}

var_list<-c("성별","최종학력","직계친척","만6세미만아이수")
for (var in var_list){
  total_table<-make_table(total_table,var,"prop")
  total_table_num<-make_table(total_table_num,var,"num")
}


total_table2<-data.frame(total_table)

for (i in 2:nrow(total_table)){
  for (j in 1:ncol(total_table)){
    total_table2[i,j]<-paste0(total_table_num[i,j]," (",total_table[i,j],")")
  }
}

colnames(total_table2)<-c("10차년(2016)",	"11차년(2017)",	"12차년(2018)",	"13차년(2019)",	"14차년(2020)")

write.csv(total_table2,file="total_table2.csv",fileEncoding="cp949")

#3.<그림1> 성별, 취업 여부 테이블
a<-table(total_df$성별,total_df$취업유무,total_df$조사연도,dnn=c("성별","취업유무")) #도수분포표
b<-table(total_df$성별, total_df$조사연도)

c<-rep(b,each=2)
d<-array(c,dim=c(2,2,5))
e<-aperm(d,c(2,1,3))

f<-round(a/e,digits=3)*100
prob_job<-f[,2,] #취업자

prob_job[1,] #남성
prob_job[2,] #여성

m<-melt(prob_job)
colnames(m)<-c("성별","연도","취업자비율")
m$성별<-as.factor(m$성별)
#geom_col 설명: https://m.blog.naver.com/regenesis90/222210090304
ggplot(m,aes(x=연도,y=취업자비율,group=성별))+
  geom_col(aes(fill=성별),position="dodge")+
  scale_fill_manual(values=c("blue","red"),labels=c("남성","여성"))+
  geom_text(aes(label=취업자비율),
            position=position_dodge(0.9),
            vjust=-0.5,size=3)+
  ylab("취업자 비율")
  
#4. <그림2-1> 남성 대상 취업유무/ 친척 양육 여부
make_family_gender_df<-function(total_df, value){
  total_df1<-total_df[total_df$성별==value,]
  a<-table(total_df1$취업유무,total_df1$직계친척,total_df1$조사연도,dnn=c("취업유무","직계친척")) #도수분포표
  b<-table(total_df1$직계친척, total_df1$조사연도)
  
  c<-rep(b,each=2)
  d<-array(c,dim=c(2,2,5))
  
  f<-round(a/d,digits=3)*100 #2016년의 직계친척 없는사람의 비율끼리 더하면 1이 됨. 
  #f[2,,] #취업자들의 직계친척 유무 비율
  prob<-f[2,,]
  m<-melt(prob)
  colnames(m)<-c("직계친척","연도","취업자비율")
  m$직계친척<-as.factor(m$직계친척)
  return(m)
}



#geom_col 설명: https://m.blog.naver.com/regenesis90/222210090304
m1<-make_family_gender_df(total_df,0)
ggplot(m1,aes(x=연도,y=취업자비율/100,group=직계친척))+
  geom_col(aes(fill=직계친척),position="dodge")+
  scale_fill_manual(values=c("blue","red"),labels=c("없음","있음"))+
  geom_text(aes(label=취업자비율),
            position=position_dodge(0.9),
            vjust=-0.5,size=3)+
  coord_cartesian(ylim = c(0,1))+
  ylab("취업자 비율")

#5. <그림2-2>여성 대상 취업유무/ 친척 양육 여부
m2<-make_family_gender_df(total_df,1)
ggplot(m2,aes(x=연도,y=취업자비율/100,group=직계친척))+
  geom_col(aes(fill=직계친척),position="dodge")+
  scale_fill_manual(values=c("blue","red"),labels=c("없음","있음"))+
  geom_text(aes(label=취업자비율),
            position=position_dodge(0.9),
            vjust=-0.5,size=3)+
  coord_cartesian(ylim = c(0,1))+
  ylab("취업자 비율")

#6. <그림3-1> 남성 대상 취업유무/만6세 미만 아이 유무 여부
make_child_gender_df<-function(total_df, value){
  total_df1<-total_df[total_df$성별==value,]
  a<-table(total_df1$취업유무,total_df1$만6세미만아이수,total_df1$조사연도,dnn=c("취업유무","만6세미만아이유무")) #도수분포표
  b<-table(total_df1$만6세미만아이수, total_df1$조사연도)
  
  c<-rep(b,each=2)
  d<-array(c,dim=c(2,2,5))
  
  f<-round(a/d,digits=3)*100 #2016년의 직계친척 없는사람의 비율끼리 더하면 1이 됨. 
  #f[2,,] #취업자들의 직계친척 유무 비율
  prob<-f[2,,]
  m<-melt(prob)
  colnames(m)<-c("만6세미만아이유무","연도","취업자비율")
  m$만6세미만아이유무<-as.factor(m$만6세미만아이유무)
  return(m)
}


#geom_col 설명: https://m.blog.naver.com/regenesis90/222210090304
m1<-make_child_gender_df(total_df,0)
ggplot(m1,aes(x=연도,y=취업자비율/100,group=만6세미만아이유무))+
  geom_col(aes(fill=만6세미만아이유무),position="dodge")+
  scale_fill_manual(values=c("blue","red"),labels=c("없음","있음"),name="미취학 자녀 유무")+
  geom_text(aes(label=취업자비율),
            position=position_dodge(0.9),
            vjust=-0.5,size=3)+
  coord_cartesian(ylim = c(0,1))+
  ylab("취업자 비율")

#7. <그림3-2>여성 대상 취업유무/만6세미만아이 여부
m2<-make_child_gender_df(total_df,1)
ggplot(m2,aes(x=연도,y=취업자비율/100,group=만6세미만아이유무))+
  geom_col(aes(fill=만6세미만아이유무),position="dodge")+
  scale_fill_manual(values=c("blue","red"),labels=c("없음","있음"),name="미취학 자녀 유무")+
  geom_text(aes(label=취업자비율),
            position=position_dodge(0.9),
            vjust=-0.5,size=3)+
  coord_cartesian(ylim = c(0,1))+
  ylab("취업자 비율")
