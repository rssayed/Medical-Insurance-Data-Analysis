RNGkind(sample.kind ="Rounding")
insurance<-read.csv("C:/Users/reeba/OneDrive/Desktop/Reeba/Fall'20/MATH 4323/project")
View(insurance)

#exploratory analysis
dim(insurance)
table(insurance$region)
table(insurance$sex)
table(insurance$smoker)

boxplot(insurance$age, main="Summary of Beneficiary Ages", ylab="Age")
barplot(table(insurance$sex), main="Summary of Gender")
boxplot(insurance$bmi, main="Summary of Body Mass Index", ylab="BMI")
barplot(table(insurance$children), main="Summary of Benficiary Dependents", xlab="Number of Children", ylab="Frequency")
barplot(table(insurance$smoker), main="Summary of Smokers")
barplot(table(insurance$region), main="Summary of Benficiary Residential Regions", ylab="Frequency")
boxplot(insurance$charges, main="Summary of Charges by Insurance Provider", ylab="Charge")

#label encoding for sex since levels are binary
insurance$sex<- ifelse(insurance$sex == "male", 1, 0)
table(insurance$sex)
View(insurance)

#label encoding for smoker since levels are binary
insurance$smoker <- ifelse(insurance$smoker =="no", 1, 0)
table(insurance$smoker)

dim(insurance)

library(caret)
library(class)
require(plyr)

#one hot encoding for region since level=4
set.seed(2)
insurance.df <- as.data.frame(insurance)

dummy <- dummyVars("~.", data = insurance.df)
insurance.transformed <- data.frame(predict(dummy, newdata = insurance.df))
View(insurance.transformed)

dim(insurance.transformed)
table(insurance.transformed$sex)
table(insurance.transformed$smoker)
summary(insurance.transformed)

insurance.transformed$chargeLevel = ifelse(insurance.transformed$charges >median(insurance.transformed$charges), 1, 0)
insurance.transformed$chargeLevel = as.factor(insurance.transformed$chargeLevel)
insurance.transformed$charges <- NULL
View(insurance.transformed)

#range(insurance.transformed$age)
#range(insurance.transformed$bmi)

dim(insurance.transformed)

n <- nrow(insurance.transformed)
train = sample(n, 0.8*n)

set.seed(11)
X.train <- insurance.transformed[train, -10]
X.test <- insurance.transformed[-train, -10]
y.train <- insurance.transformed$chargeLevel[train]
y.test <- insurance.transformed$chargeLevel[-train]


#in the following lines we will be getting the confusion matrix for k=1,3,7,10
#we get the predictions, get the mean of all the predictions, and print a table of the predictions
#k=1
set.seed(11)
knn.pred <- knn(train = X.train,
                test = X.test,
                cl = y.train,
                k=1)
mean(knn.pred != y.test)
table(knn.pred, y.test)


#k=3
set.seed(11)
knn.pred <- knn(train = X.train,
                test = X.test,
                cl = y.train,
                k=3)
mean(knn.pred != y.test)
table(knn.pred, y.test)

#k=7
set.seed(11)
knn.pred <- knn(train = X.train,
                test = X.test,
                cl = y.train,
                k=7)
mean(knn.pred != y.test)
table(knn.pred, y.test)

#k=10
set.seed(11)
knn.pred <- knn(train = X.train,
                test = X.test,
                cl = y.train,
                k=10)
mean(knn.pred != y.test)
table(knn.pred, y.test)

kvalue<-c(1,3,7,10)
#we will use cross validation LOOCV approach
#Cross Validation
set.seed(11)
for (j in kvalue){
  
  
  set.seed(11)
  knn.cv.pred <- knn.cv(train = insurance.transformed[,-10],
                        cl = insurance.transformed$chargeLevel,
                        k=j)
  print("value of k: ")
  print(j)
  print(mean(knn.cv.pred != insurance.transformed$chargeLevel))
  print(table(knn.cv.pred,insurance.transformed$chargeLevel ))
}
#after looping through the values of k set in kvalue we output the value of k,
#the mean of the prediction error rate, and the table of the predictions made.


#Importance of scaling data
#scaling data is essential to making both training and test sets on the same scale

##scaling data 
set.seed(11)
train = sample(n, 0.8*n)

X.train <- scale(insurance.transformed[train,-10])
X.train
X.test <- scale(insurance.transformed[-train,-10],
                center = attr(X.train, "scaled:center"),
                scale = attr(X.train, "scaled:scale"))
y.train <- insurance.transformed$chargeLevel[train]
y.test <- insurance.transformed$chargeLevel[-train]
attr(X.train, "scaled:center")
attr(X.train, "scaled:scale")

#the train and the test are defined above
#after defining we get the means for each
#element in train datas 

##KNN=1,3,7,10 using scaled data
for (K in kvalue){
  set.seed(1)
  knn.pred=knn(X.train,
               X.test,
               y.train,
               k=K)
  print(table(knn.pred, y.test))
  print(mean(knn.pred != y.test))
}
#now we make the predictions for each k in kvalue as we can see from the results our scaled outputs 
#are better than the knn outputs.