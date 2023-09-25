library(readr)
diabetes = read_csv("D:/Dataset/diabetes.csv")
View(diabetes)
summary(diabetes)

#Kolmogorov Smirnov & liliefors
model_kolmogorov = lm(diabetes$Outcome~. , data = diabetes)
summary(model_kolmogorov)
fit = fitted(model_kolmogorov)
error = resid(model_kolmogorov)

library(nortest)
lillie.test(error)

library(moments)
skewness(diabetes)
kurtosis(diabetes)

# Shapiro - Wilk
model_Shapiro = lm(diabetes$Outcome~., data = diabetes)
summary(model_Shapiro)
err = resid(model_Shapiro)
shapiro.test(err)


library(car)
qqnorm(diabetes$Pregnancies) + qqline(diabetes$Pregnancies)
qqnorm(diabetes$Glucose) + qqline(diabetes$Glucose)
qqnorm(diabetes$BloodPressure) + qqline(diabetes$BloodPressure)
qqnorm(diabetes$SkinThickness) + qqline(diabetes$SkinThickness)
qqnorm(diabetes$Insulin) + qqline(diabetes$Insulin)
qqnorm(diabetes$BMI) + qqline(diabetes$BMI)
qqnorm(diabetes$DiabetesPedigreeFunction) + qqline(diabetes$DiabetesPedigreeFunction)
qqnorm(diabetes$Age) + qqline(diabetes$Age)




