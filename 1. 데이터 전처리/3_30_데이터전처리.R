setwd("D:/24_1/2024 고용조사패널 공모전")

library(readxl)
library(dplyr)
#0. 데이터 불러오기 
df2<-read_excel("ypdata_w10.xlsx",col_types = 'guess',guess_max = 10000) #2016
df3<-read_excel("ypdata_w11.xlsx",col_types = 'guess',guess_max = 10000)
df4<-read_excel("ypdata_w12.xlsx",col_types = 'guess',guess_max = 10000)
df5<-read_excel("ypdata_w13.xlsx",col_types = 'guess',guess_max = 10000)
df6<-read_excel("ypdata_w14.xlsx",col_types = 'guess',guess_max = 10000)
#모두 10206개의 obs인 이유..! 몇몇 변수는 생성되었기 때문에...그 해에 답하지 않았어도 
#기존에 답했다면 기존에 답한 값이 데이터셋에 있어서 그 obs도 남아있음. . 깜놀.

#1. 필요한 변수만 select
select_var<-function(df,num,year){
  #응답자ID, 출생연도, 성별(1: 남자, 2: 여자)
  var_0<-c("sampid","yob","gender") #변수명 변경 없이 그대로 사용할 것.1),2),3)
  
  #최종학력(1: 고졸 미만, 2: 고졸, 3: 전문대졸, 4: 대졸, 5: 석사학위이상), 응답자 유형(5: 취업자)
  var_1<-c("edu","type","")#8),9)
  
  #직계친척이 양육에 도움을 주는가, 6세 미만 아이 수
  var_2<-c("g500","g505","g506")
  
  #raw data에 있는 변수명 만들기
  w_var<-paste0("w",num,var_1) #8), 9)
  y_var<-paste0("y",num,c(var_2)) #4), 5), 6), 7)
  new_df<-df[,c(var_0,w_var,y_var)] 
  new_df$year<-year
  #변수명 통일(연도 빼기)
  colnames(new_df)<-c(var_0,var_1,var_2,"year")
  return (new_df)
}

#new_df1<-select_var(df1,"09",2015)
new_df2<-select_var(df2,10,2016)
new_df3<-select_var(df3,11,2017)
new_df4<-select_var(df4,12,2018)
new_df5<-select_var(df5,13,2019)
new_df6<-select_var(df6,14,2020)

total_df<-rbind(new_df2,new_df3,new_df4,new_df5,new_df6)
write.csv(total_df,"total_df.csv",row.names=FALSE)
write.csv(new_df2,"new_df2.csv",row.names=FALSE)

total_df<-read.csv("total_df.csv")
new_df2<-read.csv("new_df2.csv")

head(total_df) #""는 각 연도에 답했는지 여부. 
colnames(total_df)<-c("sampid", "yob","gender", "edu", "type","response","g500", "g505", "g506", "year")  #"" 대신 response로 바꿈. 

#1. 각 회차에 응답한 표본만 남기기. 
total_df1<-total_df%>%filter(response==1) #16320개 삭제됨. 

#2. 아이가 있는 (g500==1) 표본만 남기기. 
sum(is.na(total_df1$g500)) #결측치는 없음 
table(total_df1$g500) #응답거절 2명, 모름 1명
total_df2<-total_df%>%filter(g500 %in% c(1))
table(total_df2$g500) #1만 남음. 처리 잘 됨.

##데이터 범주 확인. 왜 남자표본이 여자표본보다 적은지 확인하는 코드(그냥 남자 응답자 자체가 적어서 그런 것)
# total_df$g500[is.na(total_df$g500)]<-"NA"
# table(total_df$gender,total_df$g500)
# 
# sum(df6[,c("w10","w11","w12","w13","w14")]-1) #각 회차에 응답했는지 유무. 0은 참여, 1은 미참여(원래 1,2인데 바꿈)
# sum(is.na(total_df$type)) #정확히 16320개. 즉, g500 결측치들은 모두 그냥 응답을 안한것. ㅋ
# 
# sum(is.na(total_df$g500)) #16320개의 결측치
# 
# male<-df6%>%filter(gender %in% c(1)) #남자
# female<-df6%>%filter(gender %in% c(2)) #여자
# 
# male1<-male%>%mutate(no_response=w10+w11+w12+w13+w14-5)
# table(male1$no_response)#남자는 1336명이 2016년부터 쭉 아예 답을 하지 않음.
# 
# female1<-female%>%mutate(no_response=w10+w11+w12+w13+w14-5)
# table(female1$no_response)#여자는 1245명이 2016년부터 쭉 아예 답을 하지 않음.

#3. 그외 변수들의 결측치를 확인하고 유효한 표본만 남기기. 
##1) 식별키: 생략
##2) 출생연도
table(total_df2$yob) #데이터값 종류 확인
sum(is.na(total_df2$yob)) #결측치 없음

##3) 성별 
table(total_df2$gender) #데이터값 종류 확인
sum(is.na(total_df2$gender)) #결측치 없음
##성별을 0, 1로 바꾸기(남자:0, 여자:1)
total_df2$gender<-total_df2$gender-1

##4) 취업여부
table(total_df2$type) #데이터값 종류 확인. 
sum(is.na(total_df2$type)) #결측치 없음.

##5) 직계가족
total_df2[is.na(total_df2["g505"]),]
table(total_df2$g505)
total_df3<-total_df2%>%filter(g505 %in% c(1,2)) #결측치 1개, 응답거절5개 및 모름1개  총 7개 삭제
total_df3[total_df3$g505==2,]$g505<-0 #원래는 2가 "없다"인데, 이를 0으로 바꿈. 

table(total_df3$g505) #최종 테이블

##6)6세 미만 아동 수
table(total_df3$g506) #데이터값 종류 확인. 응답거절 46개. 
sum(is.na(total_df3$g506)) #결측치 없음
total_df4<-total_df3%>%filter(g506 %in% c(0,1,2,3))
total_df4$g506<-as.numeric(total_df4$g506!=0) #g506이 0이 아니면 1, 0이면 0인 것으로 변환. 


##7)최종학력
table(total_df4$edu) #데이터값 종류 확인. 응답거절 5개
sum(is.na(total_df4$edu)) #결측치 없음.

##고졸, 고졸미만 같은 데이터로 남기기. 
total_df4$edu[total_df4$edu==1]<-2
total_df4$edu<-total_df4$edu-2 #0: 고졸 및 고졸 미만, 1: 대졸, 2: 석사졸, 3: 박사졸 이상
total_df5<-total_df4%>%filter(edu %in% c(0,1,2,3)) #유의미한 값만 남김. 

##8) 취업여부
table(total_df5$type) #데이터값 종류 확인. 
sum(is.na(total_df5$type)) #결측치 없음.

table(total_df5$type,total_df5$gender)
total_df6<-total_df5%>%filter(type %in% c(5,6))
total_df6[total_df6$type!=5,]$type<-0 #취업 안함
total_df6[total_df6$type==5,]$type<-1 #취업함

table(total_df6$type) #최종 테이블

#id값을 factor로
total_df6$sampid<-as.factor(total_df6$sampid)
#id변수 정렬하기
total_df6<-total_df6[order(total_df6$sampid),]

#최종 데이터셋 저장
final_df_0331 <- subset(total_df6, select = -c(response,g500))

colnames(final_df_0331)<-c("id","출생연도","성별","최종학력","취업유무","직계친척","만6세미만아이수","조사연도")


write.csv(final_df_0331,"final_df_0331.csv",row.names=FALSE)


##결측치 및응답거절 데이터 확인
strange<-total_df2%>%filter( 
                   (!g505 %in% c(1,2)) |
                   (!g506 %in% c(0,1,2,3)) |
                   (!edu %in% c(1,2,3,4,5))
                   ) #총 58개. 
table(strange$gender)
s1<-strange%>%filter(g506 %in% c(9090908))
table(s1$gender)
table(s1$type)



nrow(final_df_0331%>%filter(조사연도 %in% c(2020)))
