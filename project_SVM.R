RNGkind(sample.kind = "Rounding")

setwd("C:/Users/reeba/OneDrive/Desktop/Reeba/Fall'20/MATH 4323/project")
insurance <- read.csv('insurance.csv', header = TRUE)
View(insurance)

#exploratory analysis of orig dataset
dim(insurance)
table(insurance$region)
table(insurance$sex)
table(insurance$smoker)

boxplot(insurance$age, main="Summary of Beneficiary Ages", ylab="Age")
barplot(table(insurance$sex), main="Summary of Gender")
boxplot(insurance$bmi, main="Summary of Body Mass Index", ylab="BMI")
barplot(table(insurance$children), main="Summary of Beneficiary Dependents", xlab="Number of Children", ylab="Frequency")
barplot(table(insurance$smoker), main="Summary of Smokers")
barplot(table(insurance$region), main="Summary of Beneficiary Residential Regions", ylab="Frequency")
boxplot(insurance$charges, main="Summary of Charges by Insurance Provider", ylab="Charge")

#label encoding for sex since levels are binary
insurance$sex <- ifelse(insurance$sex == "male", 1, 0)
table(insurance$sex)

#label encoding for smoker since levels are binary
insurance$smoker <- ifelse(insurance$smoker == "no", 1, 0)
table(insurance$smoker)

dim(insurance)

#one hot encoding for region since level=4
set.seed(2)
insurance.df <- as.data.frame(insurance)
#install caret, if first time user
library(caret)
dummy <- dummyVars("~.", data = insurance.df)
insurance.transformed <- data.frame(predict(dummy, newdata = insurance.df))
View(insurance.transformed)

#exploratory analysis of transformed dataset
dim(insurance.transformed)
table(insurance.transformed$sex)
table(insurance.transformed$smoker)
table(insurance.transformed$region.northeast)
table(insurance.transformed$region.northwest)
table(insurance.transformed$region.southeast)
table(insurance.transformed$region.southwest)

summary(insurance.transformed)

#convert charges column to indicate if higher or lower than median
insurance.transformed$chargeLevel = ifelse(insurance.transformed$charges > median(insurance.transformed$charges), 1, 0)
insurance.transformed$chargeLevel = as.factor(insurance.transformed$chargeLevel)
insurance.transformed$charges <- NULL
View(insurance.transformed)

#range(insurance.transformed$age)
#range(insurance.transformed$bmi)

#scale the data
#insurance.transformed.scale <- scale(insurance.transformed[,-10])
#View(insurance.transformed.scale)


######SVM models########
set.seed(11)
library(e1071)

n <- nrow(insurance.transformed)
train = sample(n, 0.8*n)
#y.train <- y[0.8*n]

#linear model
set.seed(1)
tune.out.linear <- tune(method= svm,
                        chargeLevel ~.,
                        data = insurance.transformed[train,],
                        kernel = "linear",
                        ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100, 1000)))
summary(tune.out.linear)
mean(predict(tune.out.linear$best.model, newdata= insurance.transformed[-train,]) != insurance.transformed[-train,]$chargeLevel)

#polynomial model
set.seed(1)
tune.out.poly <- tune(method= svm,
                      chargeLevel ~.,
                      data = insurance.transformed[train,],
                      kernel = "polynomial",
                      ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100, 1000),
                                    degree = c(2,3,4,5)))
summary(tune.out.poly)
mean(predict(tune.out.poly$best.model, newdata= insurance.transformed[-train,]) != insurance.transformed[-train,]$chargeLevel)

#radial model
set.seed(1)
tune.out.radial <- tune(method= svm,
                        chargeLevel ~.,
                        data = insurance.transformed[train,],
                        kernel = "radial",
                        ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100, 1000),
                                      gamma = c(0.5,1,2,3,4)))
summary(tune.out.radial)
mean(predict(tune.out.radial$best.model, newdata= insurance.transformed[-train,]) != insurance.transformed[-train,]$chargeLevel)

#fit the entire dataset to the radial model
set.seed(1)
tune.out.radial <- tune(method= svm,
                        chargeLevel ~.,
                        data = insurance.transformed,
                        kernel = "radial",
                        ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100, 1000),
                                      gamma = c(0.5,1,2,3,4)))
summary(tune.out.radial)
mean(predict(tune.out.radial$best.model, newdata= insurance.transformed) != insurance.transformed$chargeLevel)
