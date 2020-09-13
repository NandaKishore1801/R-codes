library(C50)
Fraudcheck<-read.csv(file.choose())
View(Fraudcheck)
summary(Fraudcheck)
str(Fraudcheck)
range(Fraudcheck$Taxable.Income)

#Converting the taxable income <30000 has "Risky" and others are "Good"
income = ifelse(Fraudcheck$Taxable.Income<30000, "Risky", "Good")
FC = data.frame(Fraudcheck,income)

#Data partion
train = FC[1:300,]
test = FC[301:600,]

View(train)
View(test)

# Building model on training data 
library(tree)
library(gmodels)
library(caret)


train1 <- C5.0(train[,-6],train$income)
train1
summary(train1)
plot(train1) # Tree graph

#using five iterations of boosting
tree_boost <- C5.0(x = train[, -6], y = train$income, trials = 5)
summary(tree_boost)

#Prediction and Condusion matrix on train data
pred1 = predict(train1,train)
head(pred1)

mean(train$income==pred1)

confusionMatrix(pred1, train$income)

# predicting on test data
test1<- as.data.frame(predict(train1,newdata=test)) 
test1["final"] <- NULL
pred_test_df <- predict(train1,newdata=test)

mean(pred_test_df==test$income)

confusionMatrix(pred_test_df,test$income)

# Cross tables
CrossTable(test$income,pred_test_df)

##### Using tree function 
cd_tree <- tree(income~.,data=train)
summary(cd_tree)

plot(cd_tree)
text(cd_tree,pretty = 0)

# Predicting the test data using the model
pred_tree <- as.data.frame(predict(cd_tree,newdata=test))
pred_tree["final"] <- NULL
pred_test_df <- predict(cd_tree,newdata=test)


pred_tree$final <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]

pred_tree$final <- as.factor(pred_tree$final)
summary(pred_tree$final)

summary(test$income)

confusionMatrix(pred_tree$final,test$income)

CrossTable(test$income,pred_tree$final)
