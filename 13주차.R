install.packages('treemap')
library(treemap)
data("GNI2014")
head(GNI2014)
treemap(GNI2014,
        index = c('continent','iso3'),
        vSize='population',
        vColor='GNI',
        type = 'value',
        bg.labels = 'yellow',
        title = "World's GNI")
st <- data.frame(state.x77) #매트릭스를 데이터프레임으로 변환

##주의 이름 열 stname 추가
st <- data.frame(st, stname = rownames(st))

treemap(st, index=c('stname'),
        vSize = 'Area',
        vColor = 'Income',
        type = 'value',
        title = 'USA states area and income')

## LAB. 미국의 주요 도시 통계 분석
library(carData)
head(Ericksen)

ds <- subset(Ericksen, city == 'state')
ds$stname <- rownames(ds)
treemap(ds,
        index = c('stname'),
        vSize = 'poverty',
        vColor = 'crime',
        type = 'value',
        title = 'USA states poverty and crime')

ds <- read.csv('C:/Users/qwert/OneDrive/문서/R-Programming/seoul_temp_2017.csv')
dim(ds) #개수
head(ds)

##2.서울의 1년 기온 분포
summary(ds$avg_temp)
boxplot(ds$avg_temp,
        col='green',
        ylim=c(-20,40),
        xlab='서울 1년 기온',
        ylab='기온')

#(3) 월별 기온 분포
# 월별 평균기온 계산
month.avg <- aggregate(ds$avg_temp,
                       by=list(ds$month),median)[2]
month.avg

#데이터 프레임을 벡터로 변환
month.avg <- month.avg[,1]
names(month.avg) <- 1:12

#평균기온 순위 계산(내림차순)
ord <- rank(-month.avg) #왜 -를 붙이는지
ord

# 월별 기온 분포
boxplot(avg_temp~month, data=ds,
        col=heat.colors(12)[ord], # 상자의 색을 지정
        ylim=c(-20,40),
        ylab='기온',
        xlab='월',
        main='서울시 월별 기온 분포(2017)')
#상자에 동그라미가 있다는 것은 비정상적인 기온이 있었다.

head(airquality)
ds <- airquality[complete.cases(airquality),] #결측값 제거
unique(ds$Month) #월을 확인

#월별 오존농도 분호 계산
month.avg <- aggregate(ds$Ozone,
                       by=list(ds$Month),median)[2] #월별로 묶기 위해 aggregate
month.avg

#데이터프레임을 벡터로 변환
month.avg <- month.avg[,1]
names(month.avg) <- 5:9

odr <- rank(-month.avg) # -를 붙임으로써 내림차순
odr

boxplot(Ozone~Month, data = ds,
        col=heat.colors(5)[odr],
        ylim=c(0,170),
        ylab = '오존농도',
        xlab = '월',
        main='여름철 오존농도')

#8월은 오존농도 차이가 심했다.
#9월은 변화폭은 크지 않고 낮지만, 이상값이 많았다.

#가운데에 가장 높지 않을 것을 보아, 고르게 분포되지 않았다는 것

##LAB
library(carData)
ds <- SLID[complete.cases(SLID),]
head(ds)

boxplot(wages~sex, data = ds,
        main='성별 임금',
        col = c('green','steelblue')) #여성일 때와 남성일 때 색상을 다르게

boxplot(wages~language, data = ds,
        main='사용 언어 별 임금',
        col = c('green','steelblue','yellow'))

ds$edu_group <- NA #새 열 추가
ds$edu_group[ds$education<10] <- 'A'
ds$edu_group[ds$education>=10 & ds$education<13] <- 'B'
ds$edu_group[ds$education>=13 & ds$education<15] <- 'C'
ds$edu_group[ds$education>=15 & ds$education<18] <- 'D'
ds$edu_group[ds$education>=18] <- 'E'
boxplot(wages~edu_group, data=ds,
        main='교육기간별 임금',
        col=rainbow(5)) # 상자의 색을 rainbow 팔레트에서 선택

install.packages('fmsb')
library(fmsb)

# (1) 데이터 준비
score <- c(80,60,95,85,40)
max.score <- rep(100,5) # 100을 5회 반복
min.score <- rep(0,5) # 0을 5회 반복
ds <- rbind(max.score,min.score, score)
ds <- data.frame(ds) # 매트릭스를 데이터프레임으로
colnames(ds) <- c('국어','영어','수학','물리','음악')
ds
# (2) 방사형 차트
radarchart(ds)
radarchart(ds, # 데이터프레임
           pcol='dark green', # 다각형 선의 색
           pfcol=rgb(0.2,0.5,0.5,0.5), # 다각형 내부 색
           plwd=3, # 다각형 선의 두께
           cglcol='grey', # 거미줄의 색
           cglty=1, # 거미줄의 타입
           cglwd=0.8, # 거미줄의 두께
           axistype=1, # 축의 레이블 타입
           seg=4, # 축의 눈금 분할
           axislabcol='grey', # 축의 레이블 색
           caxislabels=seq(0,100,25) # 축의 레이블 값 ##4등분(seg) 했으니까 25를 넣어야함 
)

#LAB
pop <- table(WVS$country)
tmp <- subset(WVS, religion=='yes')
rel <- table(tmp$country) #국가별 종교강 있는 응답자수
stat <- rel/pop #국가별 종교가 있는 응답자수 비율
stat

#방사형 차트를 하기 위해선 최댓값, 최솟값을 지정
max.score <- rep(1,4)
min.score <- rep(0,4)
ds <- rbind(max.score, min.score, stat)
ds <- data.frame(ds)

radarchart(ds, # 데이터프레임
           pcol='dark green', # 다각형 선의 색
pfcol=rgb(0.2,0.5,0.5,0.5), # 다각형 내부색
plwd=3, # 다각형 선의 두께
cglcol='grey', # 거미줄의 색
           cglty=1, # 거미줄의 타입
           axistype=1, # 축의 레이블 표시
           axislabcol='grey', # 축의 레이블 색
caxislabels=seq(0,1,0.25), # 축의 레이블 값
title='국가별 종교인 비율' # 그래프 제목
)

library(ggplot2)
month <- c(1,2,3,4,5,6)
rain <- c(55,50,45,50,60,70)
df <- data.frame(month,rain) # 그래프를 작성할 대상 데이터
df
ggplot(df, aes(x=month,y=rain)) + # 그래프를 작성할 데이터 지정
  geom_bar(stat='identity', # 막대그래프의 형태 지정
width=0.7, # 막대의 폭 지정
fill='steelblue') # 막대의 색 지정


ggplot(df, aes(x=month, y=rain)) +
  geom_bar(stat='identity',
           width=0.7,
           fill = 'steelblue')+
  ggtitle('월별 강수량') +
  theme(plot.title = element_text(size=25, face = 'bold',
                                 colour = 'steelblue')) +
  labs(x='월',y='강수량') +
  coord_flip( ) ##가로

ggplot(iris,  aes(x=Petal.Length))+
  geom_histogram(binwidth = 0.505.)

##선그래프
year <- 1937:1960
cnt <- as.vector(airmiles)
df <- data.frame(year,cnt)
head(df)

ggplot(data=df, aes(x=year, y=cnt)) +
  geom_line(col='red')

##LAB기후변화 그래프

df <- aggregate(airquality[,'Temp'],
                by=list(month = airquality$Month), FUN=mean)
ggplot(df, aes(x= month, y=x)) +
  geom_bar(stat='identity',
           width=0,7, fill='green')

#박스
df <- airquality[complete.cases(airquality),]
ggplot(data = df, aes(x=factor(Month), y=Ozone,
                      fill = factor(Month))) +
  geom_boxplot()

#산점도
ggplot(data=df, aes(x=Temp, y=Ozone, color='orange')) +
  geom_point(size=3)

#선그래프
df.7 <- subset(df, Month==7)
ggplot(data=df.7, aes(x=Day, y= Ozone)) +
  geom_line(col='red')

##실전 분석
data(UN98)
head(UN98)
help(UN98)

df <- UN98[,c('region','tfr')]
df <- df[complete.cases(df),]
df <- aggregate(df[,'tfr'], by=list(region=df$region), FUN='mean') ##region으로 그룹핑하여 평균값을 내라

ggplot(df, aes(x=region, y=x)) +
  geom_bar(stat='identity',
           width=0.7, fill = rainbow(5))

#treemap 
df <- UN98[,c('region','lifeFemale','illiteracyFemale')]
df <- df[complete.cases(df),]
df$country <- rownames(df)

treemap(df,
        index = c('region','country'),
        vSize='lifeFemale',
        vColor = 'illiteracyFemale',
        type = 'value',
        bg.labels = 'yellow',
        title = "World's Women")
#짙을 수록 문맹률이 높음
#평균 수명이 높을 수록 타일이 큼

df <- UN98[,c('region','educationMale','educationFemale')]
df <- df[complete.cases(df),]
ggplot(data=df, aes(x=educationMale, y = educationFemale, color= region)) +
  geom_point(size = 3)+
  ggtitle('남성, 여성의 교육 수준') +
  theme(plot.title = element_text(size= 25, face = 'bold',colour = 'steelblue'))

