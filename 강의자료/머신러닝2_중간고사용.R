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
  labs(title=" ") +
  xlab(" ") +
  ylab(" ")


heights<-c(160, 162, 155, 180, 170, 175, 165, 171, 177, 172)
weights<-c(55, 60, 53, 72, 70, 73, 62, 64, 69, 65)

install.packages("psych")
library(psych)

result_pearson=corr.test(heights, weights, method="pearson")
result_pearson$p
result_pearson$r




train_data<-read.csv("C:/R studio/R_data/01.simple_train_data.csv")

library(ggplot2)

ggplot(train_data, aes(x=thigh, y =upper_arm)) +
  geom_point(alpha = 0.7) +
  labs(title = " vs ", 
       x = " (thigh, cm)",
       y = " (upper_arm, cm)" ) +
  theme_minimal()

model<-lm(upper_arm ~ thigh, data = train_data)
summary(model)

residuals(model)


train_data<-read.csv("C:/R studio/R_data/01.simple_train_2.csv")

library(ggplot2)

ggplot(train_data, aes(x=thigh, y=upper_arm)) +
  geom_point(alpha=0.7) +
  labs(title = " vs ", 
       x = " (thigh, cm)",
       y = " (upper_arm, cm)")+
  theme_minimal()

model<-lm(upper_arm ~ thigh, data=train_data)
summary(model)

residuals(model)


heights<-c(160, 162, 155, 180, 170, 175, 165, 171, 177, 172)
weights<-c(55, 60, 53, 72, 70, 73, 62, 64, 69, 65)

library(psych)

result_pearson=corr.test(heights, weights, method="pearson")
result_pearson$p
result_pearson$r


train_data<-read.csv("C:/R studio/R_data/01.simple_train_data.csv")
test_data<-read.csv("C:/R studio/R_data/01.simple_test_data.csv")

model<-lm(upper_arm ~ thigh, data = train_data)
summary(model)

saveRDS(model, "regression_model.rds")

loaded_model<-readRDS("regression_model.rds")

predicted<-predict(loaded_model, newdata = test_data)
head(predicted)

ggplot(train_data, aes(x=thigh, y=upper_arm)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color="red", se=FALSE) +
  labs(title=" vs ",
       x = " (cm)",
       y = " (cm)")

results<-data.frame(Actual = test_data$upper_arm, Predicted=predicted)

rmse<-sqrt(mean((results$Actual - results$Predicted)^2))
print(paste("RMSE:", round(rmse, 3)))

model<-lm(upper_arm ~ 0 + thigh, data = train_data)
summary(model)




install.packages("rstatix")
install.packages("skimr")
library(rstatix)
library(skimr)

df<-read.csv("C:/R studio/R_data/02.diabetes.csv", header = TRUE, stringsAsFactors = TRUE)
skim(df)

df<-na.omit(df)

df<-select(df, -c(var1))
df<-select(df, -c(var1, var2, var3))
skim(df)


result<-lm(df$Diabetes ~., data=df)
result1<-lm(df$Diabetes ~1, data=df)

anova_result<-anova(result1, result)
print(anova_result)

summary(result)

saveRDS(result, "regression_model.rds")
loaded_model<-readRDS("regression_model.rds")


new_data<-read.csv("C:/R studio/R_data/02.diabetes_test.csv")

predicted<-predict(loaded_model, newdata=new_data)
head(predicted)

results<-data.frame(new_data, Predicted=predicted)

rmse<-sqrt(mean((results$Diabetes - results$Predicted)^2))
print(paste("RMSE:", round(rmse,3)))


Fitted<-fitted(result)
Residuals<-resid(result)

library(ggplot2)

ggplot(df, aes(x=Fitted, y=Residuals))+
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = " vs ",
       x = " ( )",
       y = " ( )") +
  theme_minimal()


StdResid<-sqrt(abs(Residuals))

ggplot(df, aes(x=Fitted, y=StdResid)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "  ( )",
       x = "  ( )",
       y = " ") +
  theme_minimal()


library(lmtest)
bptest(result)

library(car)
durbinWatsonTest(result)

shapiro_test(result$residuals)


opar<-par(no.readonly = TRUE)
par(mfrow = c(2,2))
plot(result)
par(opar)


cand<-influencePlot(result, id = list(n = 3))

mf<-model.frame(result)
z<-round(scale(mf[sapply(mf, is.numeric)]),1)
rownames(z)<-rownames(mf)

cbind(round(cand, 2), z[rownames(cand), ])


out_rows<-c("108", "121", "145", "160", "277")
df2<-df[!rownames(df) %in% out_rows, ]

result2<-lm(Diabetes ~ ., data = df2)

cand <- influencePlot(result2, id=list(n=3))
max(cooks.distance(result2))


vif_values<-vif(result)
print(vif_values)

selected_predictors<-names(vif_values[vif_values>7])
print(selected_predictors)


df<-read.csv("C:/R studio/R_data/02.diabetes.csv", header = TRUE, na=".")

result<-lm(df$Diabetes ~., data=df)

library("MASS")

result_bk=lm(df$Diabetes ~., data=df)
Df_fit_bk=stepAIC(result_bk, direction = "backward", trace = T)

Df_fit_st= stepAIC(result_bk, direction = "both", trace = T)

result_fw=lm(df$Diabetes ~ 1, data = df)
Df_fit_fk=stepAIC(result_fw, direction = "forward", scope = ( ), trace = T)

#중간고사 1번문제
#추론통계와 기계학습의 차이점 2가지에 대해 서술하시오

#중간고사 2번문제
#오차와 잔차에 대해 설명하시오

#중간고사 3번문제
#exam_1.rds를 로드해 회귀식을 도출하고, exam_1.rds를 사용해 해당 데이터의 RMSE를 구하시오

#중간고사 4번문제
#exam_2.csv 데이터에 대해 회귀분석을 진행해 통계적으로 유의한 변수들만 회귀식을 도출하고, 등분산성과 독립성을 각각 검증하시오.

#중간고사 5번문제
#exam_3.csv 데이터를 신뢰수준 90%에서 통계적으로 유의한 변수들만 회귀식을 도출하고, 정규성을 검정하시오.

#중간고사 6번문제
#exam_4.csv를 사용해 통계적으로 유의한 변수들만 회귀식을 도출하고, 해당 변수들의 회귀분석 결과를 토대로 각 변수들의 다중공선성 값을 도출하시오

#중간고사 7번문제
#exam_5.csv에는 2개의 종속변수(y_A, y_B)가 존재한다. 이때, 통계적으로 유의한 변수들을 y_A에 대한 회귀식과 y_B에 대한 회귀식을 각각 도출하고 (나머지 검증 진행X), 각각의 종속변수에 가장 큰 영향을 주는 변수를 선택하시오

#중간고사 8번문제
#다중회귀 분석의 5가지 검정에 대해 각각 설명하시오

#중간고사 9번문제
#delivery_train.csv 파일을 활용해 Miuntes라는 종속변수에 대한 회귀분석을 실시해 등분산, 독립성, 정규성에 대해 검증한 결과를 도출하고, VIF값을 계산해 값이 가장 큰값이 큰 하나의 변수를 제거해 회귀식을 도출한 뒤 CookD 값이 0.5보다 큰 값의 데이터 Index값을 도출하고, 해당값을 제거한 뒤 회귀분석을 실시하고, 해당 회귀식으로 delivery_test.csv에 적용해 RMSE값을 도출하시오.
