# [공부용 추가] 0. 머신러닝2_인트로.pdf / 14쪽 / 상자수염그림
install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

df<-data.frame(
  group = c(rep("Group 1", 60), rep("Group 2", 60)),
  values = c(rnorm(60, mean=0, sd=1), rnorm(60, mean=2, sd =1))
)

ggplot(df, aes(x=group, y=values)) +
  geom_boxplot(fill=c("lightblue", "lightgreen"), outlier.color="red") +
  labs(title="Boxplot Example") +
  xlab("Group") +
  ylab("Values")


# [공부용 추가] 0. 머신러닝2_인트로.pdf / 48쪽 / 피어슨 상관관계
# 키와 몸무게 데이터 생성
heights<-c(160, 162, 155, 180, 170, 175, 165, 171, 177, 172)
weights<-c(55, 60, 53, 72, 70, 73, 62, 64, 69, 65)

install.packages("psych")
library(psych)
# 피어슨상관계수 도출 및 p-value
result_pearson=corr.test(heights, weights, method="pearson")
result_pearson$p #p-value
result_pearson$r #상관관계 계수



# [공부용 추가] 1. 머신러닝2_단순회귀분석.pdf / 39쪽 / 첫 번째 실습
#데이터 불러오기
train_data<-read.csv("C:/R/R_data/01.simple_train_data.csv")

#산점도
library(ggplot2)

ggplot(train_data, aes(x=thigh, y =upper_arm)) +
  geom_point(alpha = 0.7) +
  labs(title = "대퇴부 vs 상박부 산점도", 
       x = "대퇴부 둘레 (thigh, cm)",
       y = "상박부 둘레 (upper_arm, cm)" ) +
  theme_minimal()

#회귀분석 모델
model<-lm(upper_arm ~ thigh, data = train_data)
summary(model)

#잔차
residuals(model)


# [공부용 추가] 1. 머신러닝2_단순회귀분석.pdf / 42쪽 / 두 번째 실습
#데이터 불러오기
train_data<-read.csv("C:/R/R_data/01.simple_train_2.csv")

#산점도
library(ggplot2)

ggplot(train_data, aes(x=thigh, y=upper_arm)) +
  geom_point(alpha=0.7) +
  labs(title = "대퇴부 vs 상박부 산점도", 
       x = "대퇴부 둘레(thigh, cm)",
       y = "상박부 둘레(upper_arm, cm)")+
  theme_minimal()

#회귀분석 모델
model<-lm(upper_arm ~ thigh, data=train_data)
summary(model)

#잔차
residuals(model)


# [공부용 추가] 1. 머신러닝2_단순회귀분석.pdf / 60쪽 / 피어슨 상관관계
# 키와 몸무게 데이터 생성
heights<-c(160, 162, 155, 180, 170, 175, 165, 171, 177, 172)
weights<-c(55, 60, 53, 72, 70, 73, 62, 64, 69, 65)

library(psych)
# 피어슨상관계수 도출 및 p-value
result_pearson=corr.test(heights, weights, method="pearson")
result_pearson$p #p-value
result_pearson$r #상관관계 계수


# [공부용 추가] 1. 머신러닝2_단순회귀분석.pdf / 73~75쪽 / 예측과 RMSE
#데이터 불러오기
train_data<-read.csv("C:/R/R_data/01.simple_train_data.csv")
test_data<-read.csv("C:/R/R_data/01.simple_test_data.csv")

#회귀분석 모델
model<-lm(upper_arm ~ thigh, data = train_data)
summary(model)

#모델 저장
saveRDS(model, "regression_model.rds")

#모델 불러오기
loaded_model<-readRDS("regression_model.rds")

#불러온 모델을 활용한 예측
predicted<-predict(loaded_model, newdata = test_data)
head(predicted)

ggplot(train_data, aes(x=thigh, y=upper_arm)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color="red", se=FALSE) + # 회귀선 (표준오차 리본 제거)
  labs(title="대퇴부 vs 상박부 회귀분석",
       x = "대퇴부 둘레(cm)",
       y = "상박부 둘레(cm)")

# 예측 결과와 실제값 비교
results<-data.frame(Actual = test_data$upper_arm, Predicted=predicted)
#RMSE 계산
rmse<-sqrt(mean((results$Actual - results$Predicted)^2))
print(paste("RMSE:", round(rmse, 3)))

#회귀분석 모델(절편의 고정)
model<-lm(upper_arm ~ 0 + thigh, data = train_data)
summary(model)



# [공부용 추가] 2. 머신러닝2_다중회귀분석.pdf / 13쪽 / 데이터 전처리
install.packages("rstatix") #통계 테스트
install.packages("skimr") #데이터의 흐름 요약
library(rstatix)
library(skimr)

#데이터 불러오기
df<-read.csv("C:/R/R_data/02.diabetes.csv", header = TRUE, stringsAsFactors = TRUE)
skim(df)

#결측값 제거
df<-na.omit(df)

#불필요 변수 제거
df<-select(df, -c(var1)) #하나면 제거(순수한 열 이름만 적기)
df<-select(df, -c(var1, var2, var3)) #여러 개 제거
skim(df)


# [공부용 추가] 2. 머신러닝2_다중회귀분석.pdf / 14쪽 / 모델 생성과 저장
#다중회귀분석
result<-lm(df$Diabetes ~., data=df)
result1<-lm(df$Diabetes ~1, data=df) # 다른 변수 고려 X(단순 평균으로 예측)

#ANOVA분석을 활용해 두 집단의 의미있는 차이가 있는지 확인(잔차제곱합 차이의 검증)
anova_result<-anova(result1, result)
print(anova_result)

#회귀모델
summary(result)

#모델 저장
saveRDS(result, "regression_model.rds")

#모델 불러오기
loaded_model<-readRDS("regression_model.rds")


# [공부용 추가] 2. 머신러닝2_다중회귀분석.pdf / 15~16쪽 / 예측과 RMSE
#데이터를 CSV에서 불러오기
new_data<-read.csv("C:/R/R_data/02.diabetes_test.csv")

#모델에 새로운 데이터 추가
predicted<-predict(loaded_model, newdata=new_data)
head(predicted)

#예측 결과 출력
results<-data.frame(new_data, Predicted=predicted)

#RMSE 계산
rmse<-sqrt(mean((results$Diabetes - results$Predicted)^2))
print(paste("RMSE:", round(rmse,3)))


# [공부용 추가] 2. 머신러닝2_다중회귀분석.pdf / 19쪽 / 선형성
Fitted<-fitted(result) # 적합값
Residuals<-resid(result) # 잔차

library(ggplot2)

ggplot(df, aes(x=Fitted, y=Residuals))+
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted (선형성 확인)",
       x = "적합값 (Fitted Values)",
       y = "잔차 (Residuals)") +
  theme_minimal()


# [공부용 추가] 2. 머신러닝2_다중회귀분석.pdf / 22쪽 / 등분산성
StdResid<-sqrt(abs(Residuals)) # 표준화된 잔차의 크기 근사치

ggplot(df, aes(x=Fitted, y=StdResid)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Scale-Location Plot (등분산성 확인)",
       x = "적합값 (Fitted Values)",
       y = "Standardized Residuals") +
  theme_minimal()

#등분산성 : p>0.05보다 크면, 등분산성 가정을 유지 반대는 이분산성
library(lmtest)
bptest(result)


# [공부용 추가] 2. 머신러닝2_다중회귀분석.pdf / 24쪽 / 독립성
#독립성: p>0.05보다 크면 잔차의 독립성을 가짐
library(car)
durbinWatsonTest(result)


# [공부용 추가] 2. 머신러닝2_다중회귀분석.pdf / 27쪽 / 정규성 및 진단 그래프
#정규성: p>0.05보다 크면 귀무가설 채택(정규성이 있음)
shapiro_test(result$residuals)

#4개의 조건에 대한 그래프를 모두 보는 방법
opar<-par(no.readonly = TRUE) # 그래프가 들어갈 Window 생성
par(mfrow = c(2,2)) # 그래프가 들어갈 공간 생성 2X2
plot(result) # 그래프 결과 도출
par(opar) # 그래픽 매개변수 복원


# [공부용 추가] 2. 머신러닝2_다중회귀분석.pdf / 30쪽 / 이상치 검정
#이상치검정 : cook's D 값이 높으면 문제
#세 기준에서(StudRes, Hat CookD) 상위 3개를 각각 뽑아 합친 것
cand<-influencePlot(result, id = list(n = 3))
mf<-model.frame(result) # 모형에 실제로 쓰인 데이터 (결측 제외된 상태)
z<-round(scale(mf[sapply(mf, is.numeric)]),1)
rownames(z)<-rownames(mf) # 행 이름을 다시 붙임
cbind(round(cand, 2), z[rownames(cand), ])


# [공부용 추가] 2. 머신러닝2_다중회귀분석.pdf / 32쪽 / 이상치 제거
#이상치 제거
out_rows<-c("108", "121", "145", "160", "277") #확인 후 직접 입력
df2<-df[!rownames(df) %in% out_rows, ]

#이상치가 제거된 데이터로 다시 분석 및 그래프 추출
result2<-lm(Diabetes ~ ., data = df2)
cand <- influencePlot(result2, id=list(n=3))
#max Cook's D < 0.5 → 영향점 없음, 0.5~1 → 확인, > 1 → 영향점
max(cooks.distance(result2))


# [공부용 추가] 2. 머신러닝2_다중회귀분석.pdf / 35쪽 / 다중공선성
#다중공선성(변수의 중복성) : 10이상 GVIF^(1/(2*DF))
vif_values<-vif(result)
print(vif_values)
#그래프와 상관계수를 그려주는 것이 좋을거 같음
selected_predictors<-names(vif_values[vif_values>7]) #서로가 영향을 주는 변수 추출 후 임의로 제거 필요
print(selected_predictors)


# [공부용 추가] 2. 머신러닝2_다중회귀분석.pdf / 36~37쪽 / AIC 변수선택
df<-read.csv("C:/R/R_data/02.diabetes.csv", header = TRUE, na=".")
result<-lm(df$Diabetes ~., data=df)

library("MASS")
#후진소거법
result_bk=lm(df$Diabetes ~., data=df)
Df_fit_bk=stepAIC(result_bk, direction = "backward", trace = T) #trace 하나씩 빼는 과정을 보여줌

#단계적선택법
Df_fit_st= stepAIC(result_bk, direction = "both", trace = T)

#전진선택법
result_fw=lm(df$Diabetes ~ 1, data = df)
Df_fit_fk=stepAIC(result_fw, direction = "forward", scope = (backward 변수카피), trace = T)
