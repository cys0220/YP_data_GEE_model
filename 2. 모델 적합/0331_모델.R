setwd("D:/24_1/2024 고용조사패널 공모전")
final_df<-read.csv("final_df_0331.csv")

colnames(final_df)
length(unique(final_df$id))

#1. 주변 모형 사용. 반응변수에 대해 분포 가정 X. 
library(geepack)
#모델 돌리기

ff1<-formula(취업유무~출생연도+최종학력+직계친척+만6세미만아이수+성별+성별*직계친척+성별*만6세미만아이수)

fit11<-geeglm(ff1,id=id,family=binomial,data=final_df,corstr="independence",scale.fix=TRUE)
summary(fit11)
summary(final_df)
table(final_df$최종학력)

#2. 
library(lme4)
f1<-formula(취업유무~출생연도+최종학력+직계친척+X6세미만아이수+조사연도+성별+성별*직계친척+성별*X6세미만아이수+(1|id))
fit1=glmer(f1,data=final_df,family=binomial)
summary(fit1)
