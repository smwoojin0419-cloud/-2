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


heights<-c(160, 162, 155, 180, 170, 175, 165, 171, 177, 172)
weights<-c(55, 60, 53, 72, 70, 73, 62, 64, 69, 65)

install.packages("psych")
library(psych)

result_pearson=corr.test(heights, weights, method="pearson")
result_pearson$p
result_pearson$r




train_data<-read.csv("C:/R/R_data/01.simple_train_data.csv")

library(ggplot2)

ggplot(train_data, aes(x=thigh, y =upper_arm)) +
  geom_point(alpha = 0.7) +
  labs(title = "대퇴부 vs 상박부 산점도", 
       x = "대퇴부 둘레 (thigh, cm)",
       y = "상박부 둘레 (upper_arm, cm)" ) +
  theme_minimal()

model<-lm(upper_arm ~ thigh, data = train_data)
summary(model)

residuals(model)


train_data<-read.csv("C:/R/R_data/01.simple_train_2.csv")

library(ggplot2)

ggplot(train_data, aes(x=thigh, y=upper_arm)) +
  geom_point(alpha=0.7) +
  labs(title = "대퇴부 vs 상박부 산점도", 
       x = "대퇴부 둘레(thigh, cm)",
       y = "상박부 둘레(upper_arm, cm)")+
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


train_data<-read.csv("C:/R/R_data/01.simple_train_data.csv")
test_data<-read.csv("C:/R/R_data/01.simple_test_data.csv")

model<-lm(upper_arm ~ thigh, data = train_data)
summary(model)

saveRDS(model, "regression_model.rds")

loaded_model<-readRDS("regression_model.rds")

predicted<-predict(loaded_model, newdata = test_data)
head(predicted)

ggplot(train_data, aes(x=thigh, y=upper_arm)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color="red", se=FALSE) +
  labs(title="대퇴부 vs 상박부 회귀분석",
       x = "대퇴부 둘레(cm)",
       y = "상박부 둘레(cm)")

results<-data.frame(Actual = test_data$upper_arm, Predicted=predicted)

rmse<-sqrt(mean((results$Actual - results$Predicted)^2))
print(paste("RMSE:", round(rmse, 3)))

model<-lm(upper_arm ~ 0 + thigh, data = train_data)
summary(model)




install.packages("rstatix")
install.packages("skimr")
library(rstatix)
library(skimr)

df<-read.csv("C:/R/R_data/02.diabetes.csv", header = TRUE, stringsAsFactors = TRUE)
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


new_data<-read.csv("C:/R/R_data/02.diabetes_test.csv")

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
  labs(title = "Residuals vs Fitted (선형성 확인)",
       x = "적합값 (Fitted Values)",
       y = "잔차 (Residuals)") +
  theme_minimal()


StdResid<-sqrt(abs(Residuals))

ggplot(df, aes(x=Fitted, y=StdResid)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Scale-Location Plot (등분산성 확인)",
       x = "적합값 (Fitted Values)",
       y = "Standardized Residuals") +
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


df<-read.csv("C:/R/R_data/02.diabetes.csv", header = TRUE, na=".")

result<-lm(df$Diabetes ~., data=df)

library("MASS")

result_bk=lm(df$Diabetes ~., data=df)
Df_fit_bk=stepAIC(result_bk, direction = "backward", trace = T)

Df_fit_st= stepAIC(result_bk, direction = "both", trace = T)

result_fw=lm(df$Diabetes ~ 1, data = df)
Df_fit_fk=stepAIC(result_fw, direction = "forward", scope = (backward 변수카피), trace = T)
