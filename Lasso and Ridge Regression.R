pro=read.csv("C:/Users/Hp/Desktop/student.csv")
str(pro)
colnames(pro)

library(car)
library(caret)
library(glmnet)
sum(is.na(pro))
#there are no missing values
sum(duplicated(pro))
boxplot(pro)
#there are no outliers
cor(pro, method = "pearson")
#train-test split of data
#Train-Test Split
vif_model =lm(Performance.Index ~ Hours.Studied + Previous.Scores	+ Extracurricular.Activities+	Sleep.Hours+Sample.Question.Papers.Practiced, data = pro)
vif_values <- vif(vif_model)
set.seed=10
index=createDataPartition(pro$Performance.Index,p=0.7,list=F)
train=pro[index,]
test=pro[-index,]
str(train)
str(test)
dim(train)
dim(test)
model=caret::train(Performance.Index~.,'glmnet',data=train,set.seed=10,tune.grid=expand.grid(alfa=0,lambda=0.5))
#predict on test data
prediction=predict(model,test)
#R2 value for lasso regression 
caret::R2(prediction,test$Performance.Index)
#ridge regression
model2=caret::train(Performance.Index~.,'glmnet',data=train,tune.grid=expand.grid(alfa=1,lambda=0.6,set.seed=10))
#predicting r2 square
pred=predict(model2,test)
#R2 square
caret::R2(pred,test$Performance.Index)

