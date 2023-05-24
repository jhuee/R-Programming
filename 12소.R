z <- c(1,2,3,NA,5,NA,8)
sum(z) #NA이므로 정상 계산 안 됨됨
is.na(z) 
sum(is.na(z)) #NA의 개수
sum(z, na.rm=True) #NA를 remove하여 제외하고 계산함

z1 <- c(1,2,3,NA,5,NA,8)
z2 <- c(5,8,1,NA,3,NA,7)
z1[is.na(z1)] <- 0 #치환
z1
z3 <- as.vector(na.omit(z2)) ##omit()는 원래 벡터가 사라짐짐

x <- iris
x[1,2] <- NA;
x[1,3] <- NA
x[2,3] <- NA
x[3,4] <- NA
head(x)


##for문을 사용하여 열별로 결측값이 몇 개인지
for (i in 1:ncol(x)) {
  this.na <- is.na(x[,i])
  cat(colnames(x)[i], '\t', sum(this.na), '\n' )
}

##apply사용법법
col_na <- function(y) {
  return(sum(is.na(y)))
}
na_count <-apply(x, 2, FUN=col_na)
na_count


//행단위 NA 개수 
rowSums(is.na(x)) #전체 데이터프레임의 NA 개수
sum(rowSums(is.na(x))>0) #NA가 포함된 행의 개수
sum(is.na(x)) #총 데이터셋의 NA 개수

##complete.cases()는 NA를 없애는 것
x[!complete.cases(x),]   # 부정문, NA가 잇는 행들을 나타내라
y <- x[complete.cases(x),]  #NA가 없는 행들을 넣기
head(y)

##연습문제
library(carData)
install.packages("car")

str(UN)
mean(UN$lifeExpF, na.rm=T) #NA제거
tmp <- UN[,c('pctUrban','infantMortality')]
tmp <- tmp[complete.cases(tmp),] #NA 제거
tmp <- subset(UN, region =='Asia')
mean(tmp$fertility, na.rm=T)

##정렬하기
v1 <- c(1,7,6,8,4,2,3)
v1 <- sort(v1)
v1
v2 <- sort(v1, decreasing = T) #내림차순
v2

##문자열 정렬
name <- c('정대일','강재구','신현석','홍길동')
sort(name)
sort(name, decreasing = T)

##인덱스 정렬(order())
order(name)
order(name, decreasing = T)
##vector로 받기
idx <- order(name)
name[idx]

##매트릭스와 데이터프레임의 정렬
head(iris)
order(iris$Sepal.Length)
iris[order(iris$Sepal.Length),] #오름차순
iris[order(iris$Sepal.Length, decreasing = T),] #내림차순
iris.new <- iris[order(iris$Sepal.Length),] #정렬된 데이터 저장
head(iris.new)
##혼합정렬
iris[order(iris$Species, decreasing = T, iris$Petal.Length),]


##연습 문제
str(Highway1)
Highway1[order(Highway1$rate, decreasing = T),]
## 상위 10개 == 내림차순
tmp <- Highway1[order(Highway1$len,decreasing = T), 'len']
tmp
sum(tmp[1:10]) #총 길이
## adt의 하위위 == 오름차순
tmp <- Highway1[order(Highway1$adt),c('adt','rate')]
tmp
tmp[1:10,]

tmp <- Highway1[order(Highway1$slim, decreasing = T),
                c('len','adt','rate')]
tmp
tmp[1:5,]


##샘플링
x <- 1:100
y <- sample(x, size=10, replace = F) #비복원 추출
y

idx <- sample(1:nrow(iris), size = 50, replace=F)
iris.50 <- iris[idx,]
dim(iris.50)
head(iris.50)

##재현 가능한 결과가 필요할 경우 sample이전에 set.seed()실행
sample(1:20, size =5)
set.seed(100) #이 함수의 효과는 샘플링을 한 번 하면 사라짐
sample(1:20, size = 5)
sample(1:20, size = 5)
sample(1:20, size = 5)
sample

##조합(Combination)
combn(1:5,3) #5C3
x <- c("red","green","blue","black","white")
com <- combn(x,2)
com

for(i in 1:ncol(com)) {
  cat(com[,i], "\n")
}

##연습 문제
library(carData)
tot.mean <- mean(KosteckiDillon)
tot.mean

for(rate in (1:5)*0.1) {
  set.seed(100)
  idx <- sample(nrow(KosteckiDillon), nrow(KosteckiDillon)*rate)
  sam.data <- KosteckiDillon[idx,'dos']
  tmp.mean <- mean(sam.data)
  cat('Diff:', rate, tot.mean-tmp.mean, '\n')
}
cbn <- combn(1:5,3)
cbn
ncol(cbn)

##집계: 데이터의 그룹에 대해서 합계나 평균을 계산산
agg <- aggregate(iris[,-5], by=list(iris$Species), FUN=mean)
#by = list()는 집계 작업 기준의 열
#FUN = mea 은 집계작업 내용이 평균 계산이다
agg

agg <- aggregate(iris[,-5], by=list(품종=iris$Species), FUN= sd)
agg

##기준이 2개
head(mtcars)
agg <- aggregate(mtcars, by=list(cyl=mtcars$cyl,
                                 vs=mtcars$vs),FUN=max)
agg

##연습문제
data('CES11')
str(CES11)
table(CES11$abortion)
table(CES11$abortion)/nrow(CES11) #반대와 찬성의 비율

##성별에 따른 찬반
agg <- aggregate(CES11[,'abortion'],by=list(성별=CES11$gender),
                 FUN=table)
agg.2 <- agg[,2]
agg.2[1,] <- agg.2[1,]/sum(agg.2[1,])
agg.2[2,] <- agg.2[2,]/sum(agg.2[2,])
rownames(agg.2) <- agg[,1]
agg.2

agg <- aggregate(CES11[,'abortion'], by=list(지역=CES11$urban), FUN=table)
agg.2 <- agg[,2] #찬반 빈도수 부분만 추출
agg.2[1,] <- agg.2[1,]/sum(agg.2[1,])
agg.2[2,] <- agg.2[2,]/sum(agg.2[2,])
rownames(agg.2) <- agg[,1]
agg.2

##연습문제 2
sum(is.na(Chile))
ch <- Chile[complete.cases(Chile),]
set.seed(100)
idx <- sample(nrow(ch), nrow(ch)*.6) # 60% sampling
ch60 <- ch[idx,]
dim(ch60)

agg <- aggregate(ch60[,'population'], by=list(지역=ch60$region),
                 sum)
agg[order(agg$x, decreasing = T),]
table(ch60$vote)
no.people <- table(ch60$sex) # 여성, 남성 응답자수
tmp <- subset(ch60, vote=='Y') # 찬성만 추출
agg <- aggregate(tmp[,'vote'], by=list(성별=tmp$sex), length)
yes.ratio <- agg$x / no.people # 찬성 비율 계산
yes.ratio

no.region <- table(ch60$region) # 지역별 응답자수
tmp <- subset(ch60, vote=='Y') # 찬성만 추출
agg <- aggregate(tmp[,'vote'], by=list(지역=tmp$region), length)
yes.ratio <- agg$x / no.region # 찬성 비율 계산
yes.ratio
