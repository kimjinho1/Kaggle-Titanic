Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-13.0.1")

## train 데이터 불러오기
train = read.csv("titanic_train.csv",stringsAsFactor = FALSE)
dim(train) # 12열 891행 -> 변수 12개 데이터 891개
head(train)
train_length =dim(train)[1]; train_length

## test 데이터 불러오기
test = read.csv("titanic_test.csv")
test_id = test$PassengerId
dim(test) # 11열 418행 -> 변수 11개 데이터 418개 -> target 데이터가 빠짐
head(test)

# train에서 PassengerId는 필요없으니 제거, train과 test를 합치기 위해 
# test의 PassengerId와 Survived를 잠깐 분리한다.
target = data.frame(train$Survived)
colnames(target) = "Survived"; head(target)
train = train[-c(1, 2)]; colnames(train)

id = data.frame(test$PassengerId)
colnames(id) = "PassengerId"; head(id)
test = test[-1]; colnames(test)

df = rbind(train, test); dim(df)

## 데이터 설명
# PassengerId -> 번호
# Survived -> 생존 (0 = No; 1 = Yes)
# Pclass -> 계급 Class (1 = 1st; 2 = 2nd; 3 = 3rd)
# name -> 이름
# sex -> 성별
# age	-> 나이
# sibsp ->	형제자매, 배우자 수의 합
# parch	-> 부모님, 아이 수의 합
# ticket -> 티켓 번호
# fare ->	요금
# cabin -> 선실
# embarked ->	선착장(C = Cherbourg, Q = Queenstown, S = Southampton)
summary(df)


## missing value 찾기 
sapply(df, function(x) sum(is.na(x)))
# Age만 missing value가 있다고 뜨는데 잘못됬다. Age에는 NA가 
# 들어가있지만 다른 missing value는 그냥 공백이기 때문에 인식이 안되었기 때문이다.
# 아래같은 방식으로 공백에다 NA를 집어넣어 줘야함.
df = lapply(df, function(x) 
  type.convert(replace(x, grepl("^\\s*$", trimws(x)), NA), as.is = TRUE)) 
df = as.data.frame(df)
# 무사히 모든 missing value를 찾았다.
sapply(df, function(x) sum(is.na(x)))


## 데이터 전처리(변환, 삭제)
## Fare는 missing-value가 하나이기 때문에 그냥 평균을 넣는다.
# S를 채워줘도 될 것 같다.
Fare_mean = mean(df$Fare[is.na(df$Fare) == FALSE]); Fare_mean
df$Fare[is.na(df$Fare) == TRUE] = Fare_mean

  
## Embarked에서 S는 비중이 거의 70%나 되기 때문에 2개의 missing value에
Embarked_summary = sort(summary(df$Embarked)); Embarked_summary
Embarked_S = Embarked_summary[4]
Embarked_S_per = Embarked_S / sum(Embarked_summary); Embarked_S_per
# Embarked의 missing value에 S를 채움
df$Embarked[is.na(df$Embarked) == TRUE] = "S"
summary(df$Embarked)
sapply(df, function(x) sum(is.na(x)))


# Name, Ticket은 데이터로 사용하기 힘들 것 같고 Cabin은 
# missing value가 너무 많아서 사용할 수 없기 때문에 열에서 뺀다.
colnames(df)
head(df[c("Name", "Ticket")])
df = df[c("Pclass","Sex","Age","SibSp","Parch","Fare","Embarked")]
sapply(df, function(x) sum(is.na(x)))


## Sibsp 와 Parch는 굳이 2개로 나눠어져 있을 필요가 없어보인다.
# 2개를 더한 값을 Family라는 변수로 데이터에 추가한다.
df["Family_size"] = df["SibSp"] + df["Parch"]
head(df$Family_size)


## 필요없는 데이터를 열에서 삭제한다.
df = df[c("Pclass","Sex","Age","Fare","Embarked","Family_size")]
head(df)


## Age는 나름 조절을 해서 missing-value를 채워줄 수 있을 것 같다.
library(corrplot)
# missing-value가 없는 데이터만 추출하고 상관분석을 진행함
temp_df = df[is.na(df$Age) == FALSE & is.na(df$Fare) == FALSE,]; temp_df
head(temp_df)
cor(temp_df[-c(2,5)])
cor_coef = cor(temp_df[-c(2,5)])
# Age는 Pclass와 제일 강한 음의 관계를 보인다. 
corrplot(cor_coef, method="circle")
# p-value가 0.05보다 작으므로 귀무가설을 기각
# -> Age와 Pclass는 유의성이 있다.
cor.test(df$Age, df$Pclass)
# Pclass에 맞춰서 Age의 값을 채워보자
for(i in 1:3)
{
  df[df$Pclass == i & is.na(df$Age), "Age"] = 
              mean(df[df$Pclass == i, "Age"], na.rm=TRUE)
}
head(df$Age)


# 미래의 분산분석을 위해 현재 데이터 조금 잘라서 저장해 놓는다.
temp = df 

############################## 무사히 전처리를 했다 ##############################


## python에는 flatten이라는 함수를 사용했었는데 R에도 비슷한게 있는 것 같긴한데
# 잘 실행이 안되서 함수를 직접 구현해보았다.
flatten = function(v, t, n, len)
{
  for(i in 1:len)
  {
    if(df[i, v] == t)
      df[i, n] = 1
    else
      df[i, n] = 0
  }
  return(df[n])
}
## Sex를 Male과 Female로 나눠줌, Embarked도 C, Q, S로 나눠줌.
len = dim(df)[1]; len
df["Male"] = flatten("Sex", "male", "Male", len)
df["Female"] = flatten("Sex", "female", "Female", len)
df["Embarked_C"] = flatten("Embarked", "C", "Embarked_C", len)
df["Embarked_Q"] = flatten("Embarked", "Q", "Embarked_Q", len)
df["Embarked_S"] = flatten("Embarked", "S", "Embarked_S", len)
head(df)


## 불필요한 데이터(Sex, Embarked) 제거
df = df[-c(2, 5)]
head(df)


## 뒤에 할 다중회귀분석, 예측 모델 만들기, Survived와 다른 변수의 관계 분석을 하기 위해 
## 처리한 데이터를 train과 test로 다시 나눈다.
train = df[1:train_length,] 
train = cbind(target, train) 
head(train); dim(train)

test = df[(train_length+1):dim(df)[1],]
head(test); dim(test)


## 이제 Survived와 다른 변수들의 관계를 분석해보자

## 상관분석과 상자 그림(boxplot)
cor_coef = cor(train)
corrplot(cor_coef, method="circle")
# 여기서 Male은 Survived와 강한 음의 관계를, Female은 Survived와 강한 양의 관계를
# 가진다는 것을 알 수 있다. -> 이는 여성이 남성보다 생존률이 훨씬 높을 것이라는 보여준다.

check = read.csv("titanic_train.csv")
table(check$Sex)
library(ggplot2)
ggplot(data=data, aes(x=Sex, y=Survived, color=Sex)) + geom_bar(stat = "identity")
# 이는 위와 같이 통게적으로도 확인할 수 있다.
 
corrplot(cor_coef, method="circle") 
cor.test(train$Survived, train$Pclass) 
cor.test(train$Survived, train$Age) 
cor.test(train$Survived, train$Fare)
cor.test(train$Survived, train$Family_size)
cor.test(train$Survived, train$Male)
cor.test(train$Survived, train$Female)
cor.test(train$Survived, train$Embarked_C)
cor.test(train$Survived, train$Embarked_Q)
cor.test(train$Survived, train$Embarked_S)

# Family_size와 Embarked_Q를 제외하곤 전부 p-value가 0.05보다 이하이므로 귀무가설을 기각
# -> Family_size와 Embarked_Q를 제외한 나머지 변수들은 Survived와 유의성이 있다. 

# Age도 Survived와 큰 유의성을 보일줄 알았는데 그렇지 않았다.
# Data binning을 하지 않아서 그런 것 같다...
# 그래도 조금의 음의 관계를 가지고 있는 것을 보아하니 나이가 어릴수록 살 확률이 조금 더 
# 높다는 것을 알 수 있다.


# Embarked를 보면 C는 Survived와 양의 관계, S는 Survived와 음의 관계를 보이는데
# 이는 S보다 C에서 배를 탄 사람이 비교적 살 확률이 높다는 것을 보여준다.

# Pclass와 Survived가 음의 관계를 가지는 것을 보아 Pclass가 낮을수록(계급이 높을수록) 
# 생존률이 올라감을 확인할 수 있다.

# Fare와 Survived도 나름 양의 관게를 보인다. 이는 계급이 높은 사람일수록 더 비싼 좋은 자리에 
# 탑승했기 때문에 생존할 확률이 높았을 것이라고 볼 수 있다.

# Fare와 Pclass도 상ekd히 큰 음의 관계를 가지고 있다. Survived에 관한 유의성을 확인하는 것이 
# 아니기 때문에 데이터가 더 많은 temp(앞에서 저장함)데이터를 사용한다.
cor.test(temp$Fare, temp$Pclass)
# Fare와 Pclass는 유의성이 있고 나름 큰 음의 관게를 보인다.
# Pclass가 낮을수록 계급이 높으니 Fare(탑승 요금)이 더 높은 것이라고 볼 수 있다.
# 박스 상자로도 확인해 보자
temp$Pclass = factor(temp$Pclass, label = c("부유함","보통","가난함"))
temp$Pclass
boxplot(Fare~Pclass, data = temp)
# 역시나 부유할수록 Fare가 높다는 것을 확인할 수 있다.

# 앞에 Fare, Pclass, Embarked의 분석을 보면 C가 비싸고 좋은 자리이고 S는 싸고 안좋은 자리일 
# 거라는 예측이 가능한데. 이원분산분석을 통해 이 예측이 맞는지 확인해보자

## 분산분석
library(psych)
library(car)
# 정규성 검정
shapiro.test(temp$Fare) 
#shapiro.test(temp$Pclass) 
#shapiro.test(temp$Embarked) 
# 독립섭 검정
chisq.test(temp$Pclass, temp$Embarked)
#chisq.test(temp$Fare, temp$Pclass)
#chisq.test(temp$Fare, temp$Embarked)
# 등분산성 검사
leveneTest(Fare~Pclass, data = temp) 
leveneTest(Fare~Embarked, data = temp) 
leveneTest(Fare~Pclass*Embarked, data = temp) 
# 이원분산분석
aov2 = aov(Fare~Pclass + Embarked + Pclass:Embarked, data = temp)
summary(aov2)
# Plclass, Embarked, Pclass*Embarked의 p-value가 0.05 이하이므로 귀무가설 기각. 유의성이 있음

## 상호작용 그래프 그리기
interaction.plot(x.factor = temp$Pclass,
                 trace.factor = temp$Embarked,
                 response = temp$Fare, function(x) mean(x),
                 type = "b", legend=TRUE,
                 xlab="계급", ylab="비용",
                 pch=c(1,19), col = c("red", "green"))

interaction.plot(x.factor = temp$Embarked,
                 trace.factor = temp$Pclass,
                 response = temp$Fare, function(x) mean(x),
                 type = "b", legend=TRUE,
                 xlab="자리", ylab="비용",
                 pch=c(1,19), col = c("red", "green"))

# 그래프를 보아하니 꼭 C가 비싸고 좋은 자리, S가 나쁘고 싼 자리는 아니다. 가난한 사람의 
# 경우 S가 오히려 더 비용이 높았고, 보통 사람의 경우 C와 S의 비용이 큰 차이가 없다.
# 허나 부유한 사람의 경우 C, Q, S 순으로 비용이 높은 것을 보여준다. 이걸 보아 C가 S보다 
# 좋은 자리라기보다는 그냥 부유한 사람들이 C를 좀 더 선호한 것 같다. 


### 앞에서 본 분석을 바탕으로 요약하면 이 타이타닉 데이터에서 생존에       ###
### 제일 중요한 요소는 성별, 계급, 비용, 선착장, 나이, 가족의 수 순서이다. ###
### 예를 들면 나이가 어리고 계급이 높은 여성은 살 확률이 제일 높고,        ###
### 나이가 많고 계급이 낮은 남성은 생존률이 제일 낮을 것이다.              ###


### 예측 모델 만들어보기

## 다중회귀분석
# 타이타닉 문제는 분류문제라 다중회귀분석이 의미가 없지만 
# 다중회귀분석을 하면 어떻게 나올지 궁금해서 한 번 해보았습니다.
plot(train)
pairs(Survived~., data=train, panel=function(x, y) {points(x, y, col="black")
  abline(lm(y~x), col="red", lwd=2)})
lm_out = lm(Survived~., data=train)
summary(lm_out)
# 처리된 타이타닉 데이터를 사용해 다중회귀모형을 만들었다. 여기서 유의성이 있는 가중치는 
# p_value가 0.05이하여서 귀무가설이 기각된 bias, Male, Female, Age, Pclass이다.
library(QuantPsyc)
lm.beta(lm_out)


## rpart(의사결정나무)
# rpart를 라이브러리를 사용하면 쉽게 의사결정나무를 만들 수 있고 의사결정나무를 통해
# 어떤 변수가 제일 중요한지를 복잡한 분석 없이 쉽게 확인할 수 있다.
library(rpart)
rpart_fit = rpart(Survived ~ ., data = train)
library(rpart.plot)
rpart.plot(rpart_fit)
# 제일 위(뿌리)에 Male이 있는데 남성의 경우 생존률이 19%, 여성의 경우 74%임을 알 수 있다.
# 아래로 내려가면 남자일 경우에 다음으로 중요한 요소는 나이고 여자일 경우에는 계급이었다. 
# 남자의 경우 6, 7세 이상이라면 생존률이 17%지만, 6, 7세 이하(아기)의 경우 생존률이 67%나 
# 됨을 알 수 있다. 이는 사람들이 아기들은 어떻게든 살리려고 노력을 했음을 보여준다. 
# 전쟁이 났을 때 아이와 여성들을 먼저 대피시키는 것과 같은 상황인 것 같다.
# 여성의 경우 Pclass가 3이상(가난함)이면 생존률이 50퍼지만, Pclass가 3보다 작으면 생존률이
# 95%나 된다. 이를 보아 귀족들 먼저 대피시켰다는 것을 알 수 있다. 남성의 경우도 더 깊게 내려가 
# 보면 나이 다음에 중요한 요소중 계급이 있는데 여기는 더 차이가 심하다. 가난, 평범한 사람들은
# 생존률이 12%인 반면 귀족은 생존률이 36%나 된다. 의사결정나무를 전체적으로 요약했을 때 
# 생존에 제일 중요한 요소는 성별, 나이, 계급임을 알 수 있는데 이 결과는 위의 분석 결과와 같다.


# csv 파일로 저장
write.csv(rpart_pred, file="rpart_pred.csv", row.names = FALSE)

## 이제 예측모델을 만들어 보자.
# rpart 예측 모델
# target인 Survived를 범주형으로 변겅
train$Survived = factor(train$Survived, labels=c("0","1"))

# train 데이터를 사용하여 모델을 구현
rpart_fit = rpart(Survived ~ ., data = train)

# test 데이터를 사용하여 Survived를 예측
rpart_pred = predict(rpart_fit, newdata=test, type="class")
rpart_pred = data.frame(rpart_pred); head(rpart_pred)

# kaggle에 Submission을 제출하기 위해 열 이름을 Survived로 바꾸고 PassengerId를 추가해준다.
colnames(rpart_pred) = "Survived"
rpart_pred = cbind(id, rpart_pred); head(rpart_pred)

# csv 파일로 저장
write.csv(rpart_pred, file="rpart_pred.csv", row.names = FALSE)

# 잘 저장되었는지 확인
check = read.csv("rpart_pred.csv"); head(check)

# 랜덤포레스트 예측 모델
library(randomForest)
# 랜덤포레스트는 mtry라는 파라미터가 있기 때문에 반복문을 써서 제일 좋은 모델을 찾아야 한다.
# 이를 위해 train 데이터에서 또 train데이터를 만든다. 
# Kaggle의 test 데이터에는 target이 없어서 이런식으로 하게 되네요...
# mtry는 각각의 tree마다 몇 개의 변수를 사용할 것인지를 정하는 파라미터이다.
n=nrow(train)
train_set=sample(n, floor(n/2))
test_set = train[-train_set,]

# 모델의 정확도와 mtry를  담을 리스트를 생성(변수가 9개이기 때문에 1부터 9까지 반복한다)
best_rf_mse =  0
for(i in 1:9)
{
  # train의 train_set을 사용하여 모델을 구현
  rf_fit = randomForest(Survived~., data = train[train_set,], mtry = i)
  
  # train의 test_set을 사용하여 Survived를 예측
  rf_pred = predict(rf_fit, newdata=test_set, type= 'response')
  
  # 정확도 저장
  mean <- mean(rf_pred == train$Survived[-train_set])
  
  # 제일 성능이 좋은 모델을 저장하는 부분
  if(mean > best_rf_mse)
  {
    best_rf_mse = mean
    random_forest_fit = rf_fit
    best_mtry = i
  }
}


# 제일 좋은 성능을 보인 mtry는 3이고 정확도는 85.2% 정도이다. 
# 사실 train_set 때문에 위의 결과는 매 번 바뀐다.
best_mtry; best_rf_mse
#importance(random_forest_fit) 
# mtry를 best_mtry(3)로 정하고 test 데이터를 사용하여 Survived를 예측
random_forest_fit = randomForest(Survived~., data = train[train_set,], mtry = best_mtry)
random_forest_pred = predict(random_forest_fit, newdata = test)
random_forest_pred = data.frame(random_forest_pred); head(random_forest_pred)

# kaggle에 Submission을 제출하기 위해 열 이름을 Survived로 바꾸고 PassengerId를 추가해준다.
colnames(random_forest_pred) = "Survived"
random_forest_pred = cbind(id, random_forest_pred); head(random_forest_pred)

# csv 파일로 저장
write.csv(random_forest_pred, file="random_forest_pred.csv", row.names = FALSE)

# 잘 저장되었는지 확인
check = read.csv("random_forest_pred.csv"); head(check)

### Kaggle에 내가 rpart와 랜덤포레스트로 예측한 결과를 제출했다.
# rpart -> 0.77511 -> 6500 등
# 랜덤포레스트 -> 0.77551 -> 6500 등
# 왠만하면 랜덤포레스트가 제일 성농이 좋다고 생각하며 지내왔지만 같은 성능이 나와버렸다. 
# 사실 train_set이 문제일 확률이 제일 높다. 
# 전체적으로 성능을 높이기 위해서는 Age와 Fare를 binning해야 할 것 같다.
# 아니면 Age같이 많은 양의 missing value를 가지고 있는 데이터를 Pclass만 보고 채웠었는데, 
# 이 방법을 좀 더 개선한다면 성능이 향상될 수도 있을 것 같다.

# 그냥 랜덤포레스트에 train 싹다 박아보자. 왠지 더 잘 나올 것 같다.

########################## 교수님 한 학기동안 수고하셨습니다! ##########################
