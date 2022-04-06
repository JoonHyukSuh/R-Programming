rm(list=ls())
getwd()
setwd("C:/r_workspace")
pit = read.csv('C:/r_workspace/test/2021pitch.csv')
head(pit)
str(pit)

table(pit$팀)
#팀 이름 보기 쉽게 변경, 이적 선수는 이적 후 팀으로, 21제거
pit$팀 = gsub('21','',pit$팀)
pit$팀
pit[pit$팀 == 'K','팀'] ='KT'
pit[pit$팀 == 'K롯','팀'] ='롯데'
pit[pit$팀 == 'L','팀'] ='LG'
pit[pit$팀 == 'L키','팀'] ='키움'
pit[pit$팀 == 'N','팀'] ='NC'
pit[pit$팀 == 'N두','팀'] ='두산'
pit[pit$팀 == 'N롯','팀'] ='롯데'
pit[pit$팀 == 'S','팀'] ='SSG'
pit[pit$팀 == '기','팀'] ='기아'
pit[pit$팀 == '두','팀'] ='두산'
pit[pit$팀 == '롯','팀'] ='롯데'
pit[pit$팀 == 'k롯','팀'] ='롯데'
pit[pit$팀 == '삼','팀'] ='삼성'
pit[pit$팀 == '키','팀'] ='키움'
pit[pit$팀 == '한','팀'] ='한화'
table(pit$팀)

# X, 비율 1~4 지우기
pit$X = NULL
pit$비율.1 = NULL
pit$비율.2 = NULL
pit$비율.3 = NULL
pit$비율.4 = NULL
str(pit)
#중복된 값 확인
sum(duplicated(pit))
#결측치 확인
sum(is.na(pit))

#투수들의 이닝 수 분포
range(pit$이닝)
hist(pit$이닝, breaks = seq(0,190,10), main = '이닝 수 분포', xlab = '이닝 수',
     labels = TRUE,xlim =c(0,200),ylim=c(0,100))

#10이닝 미만을 던진 투수 제외
pit = pit[pit$이닝>10,]
str(pit)

#팀 별 평균자책점 비교 
library(lattice)
bwplot(팀 ~ 자책점, data = pit, xlab = '팀 별 평균 자책점 비교', notch=TRUE)

#볼넷 비율 열 추가
pit$볼넷비율 = pit$볼넷 / pit$이닝

#팀 별 볼넷 비율
histogram(~볼넷비율 | 팀, data = pit, type='percent', nint = 12,col='skyblue',
          main='팀 별 투수들의 볼넷 비율')

#피장타율 열 추가
pit$피장타율 = (pit$X2타 + pit$X3타 + pit$홈런) / pit$이닝

t1 = bwplot(팀~볼넷비율, data = pit, nprch=TRUE)
t2 = bwplot(팀 ~ 피장타율, data = pit, nprch=TRUE)

plot(t1, split=c(1,1,2,1))
plot(t2, split=c(2,1,2,1), newpage = FALSE)

#팀별 투수들의 피홈런 비교 
library(lattice)
library(ggplot2)
ggplot(pit,aes(x=팀,y=홈런))+ 
  geom_violin(fill='honeydew2') + geom_boxplot(fill='skyblue',width=0.2)

#팀별 총 피홈런 비교
team.homerun = aggregate(홈런~팀,pit,sum) 
team.homerun = team.homerun[c(order(-team.homerun$홈런)),]
ggplot(team.homerun, aes(reorder(팀,홈런), y=홈런)) + geom_bar(stat='identity') 

#WAR 4 넘어가는 투수 수
range(pit$WAR)
hist(pit$WAR, breaks = seq(-2,8,0.5),main='WAR 분포',labels=TRUE,
     xlim=c(-2,8),ylim=c(0,70),col='skyblue')
abline(v=4,col='red')

#WAR 정렬
subset(pit,select=c(이름,팀,WAR), subset=(WAR >= 4.0))


library(ggthemes)
library(dplyr)
library(tidyverse)
library(ggplot2)

#WAR 정렬
pit.war = head(pit[c(order(-pit$WAR)),],10)
ggplot(data = pit.war, aes(reorder(이름,WAR),y=WAR,fill=WAR)) +
  geom_bar(stat='identity') +
  coord_flip()+
  scale_fill_viridis_c(option='plasma',begin=0.5,direction = -1)+
  labs(title='2021 시즌 투수 WAR TOP10') +
  geom_text(aes(y=1,label=팀),hjust=3.5)+
  geom_text(aes(label=format(WAR,digits=3)),hjust=1.2)


#WAR로 선수 등급 구분 
war.cut = cut(pit$WAR, breaks= c(-3,0,1,4,5,10),
              c('대체선수급','후보선수','주전선수','올스타','MVP'),include.lowest=T)

pit$등급 = war.cut
table(war.cut)

#롯데와 LG 투수 비교 - 제구력
lotte.lg = pit[c(pit$팀 == '롯데' | pit$팀 == 'LG'),]
b1 = bwplot(볼넷비율~팀, data = lotte.lg, notch = T)
b2 = densityplot(~볼넷비율, groups = 팀, data=lotte.lg, auto.key =T)
plot(b1,split=c(1,1,2,1))
plot(b2, split=c(2,1,2,1),newpage=F)

#롯데와 LG 투수 비교 - 피장타율
h1 = bwplot(피장타율~팀, data= lotte.lg, notch = T)
h2 = densityplot(~피장타율, groups = 팀, data=lotte.lg, auto.key=T)
plot(h1,split=c(1,1,2,1))
plot(h2,split=c(2,1,2,1), newpage=F)

#롯데와 LG 투수 등급 비교 
war.table = with(lotte.lg,table(팀,등급))
lotte.lg.war = margin.table(war.table,c(1,2))
lotte.lg.war
barplot(lotte.lg.war,ylim=c(0,20),beside=T,col=c('red','navy'),legend=T,
        ylab='등급 별 선수',las=1,main='롯데와 LG 투수들의 등급 비교')

# 볼넷비율과 war 산점도 plot, 좋은 투수들의 위치 
ball.net = subset(pit,select = c(볼넷비율,WAR))
plot(ball.net,xlab='볼넷비율',ylab='WAR')
good.pit = subset(pit,select=c(볼넷비율,WAR), subset=(WAR >1))
plot(ball.net,xlab='볼넷비율',ylab='WAR')
points(good.pit,col='red',pch=18)
abline(h=1,col='red')
good.pit

#적합선으로 나타내기 
ggplot(data=pit, aes(x=볼넷비율,y=WAR)) +
  geom_point() + geom_smooth(method='lm',color='red',linetype=2,size=1) +
  labs(title='볼넷비율과 WAR의 관계')

#롯데자이언츠 투수 분석
lotte.pit = subset(pit,subset=c(팀=='롯데'))
lotte.pit = lotte.pit[c(order(-lotte.pit$자책점)),]
barchart(이름~자책점, data=lotte.pit, scales=list(cex=0.6),
           par.settings=simpleTheme(col=c('navy')))

ggplot(data=lotte.pit,aes(reorder(이름,자책점),y=자책점,fill=자책점))+
  geom_bar(stat='identity')+
  coord_flip()+
  scale_fill_viridis_c(option='magma',begin=0,direction=-1)+
  labs(title= '2021 시즌 롯데 자이언츠 투수들의 평균자책점')

#송재영 투수 분석
subset(lotte.pit, select=c(이름,출장,자책점,피장타율,볼넷비율,등급),
                             subset=(이름=='송재영'))

#귀무가설 : LG 투수 평균 WAR - 롯데 투수 평균 WAR < 0 
#대립가설 : LG 투수 평균 WAR - 롯데 투수 평균 WAR > 0 
lotte.lg$팀
#등분산성 테스트
var.test(lotte.lg$WAR ~ lotte.lg$팀) # pvalue 0.05보다 높다 귀무가설 채택
t.test(lotte.lg$WAR ~ lotte.lg$팀, mu = 0, alternative = 'greater',var.equal = TRUE )
# p-value 0.05보다 작으므로 귀무가설 기각, 대립가설 채택 

# 다중회귀식으로 WAR 예측 
y = as.matrix(pit$WAR)
ones = replicate(length(pit$WAR),1)
x = as.matrix(cbind(pit[c('자책점','볼넷비율','피장타율')],ones))
library(matlib)
beta = inv(t(x)%*%x)%*%t(x)%*%y
lr = lm(WAR ~ 자책점 + 볼넷비율 + 피장타율, data = pit)
lr
plot(lr)
summary(lr)
predict = x %*% beta 
predict

MSE = sum((1/length(y)) * ((y-predict)^2))
MSE
sum((pit$WAR - predict(lr, newdata = pit))^2) / length(pit$WAR)

