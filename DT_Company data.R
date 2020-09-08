##install.packages("C50")
install.packages("C50")
library(C50)
company<-read.csv(file.choose())
View(company)
summary(company)
str(company)
#Converting the sales into categorical variable, we will consider <10 has "No"
High = ifelse(company$Sales<10, "No", "Yes")
CD = data.frame(company, High)
View(CD)
mydata = CD[,2:12]
View(mydata)

#Data Partioning
train = mydata[1:200,]
test = mydata[201:400,]
View(train)
View(test)

# Building model on training data 
library(tree)
library(gmodels)
library(caret)

train1 <- C5.0(train[,-12],train$High)
train1
summary(train1)
plot(train1) # Tree graph

#using five iterations of boosting
tree_boost <- C5.0(x = train[, -12], y = train$High, trials = 5)
summary(tree_boost)


#Prediction and Condusion matrix on train data
pred1 = predict(train1,train)
head(pred1)

mean(train$High==pred1)

confusionMatrix(pred1, train$High)

# predicting on test data
test1<- as.data.frame(predict(train1,newdata=test)) 
test1["final"] <- NULL
pred_test_df <- predict(train1,newdata=test)

mean(pred_test_df==test$High)

confusionMatrix(pred_test_df,test$High)

# Cross tables
CrossTable(test$High,pred_test_df)

##### Using tree function 
cd_tree <- tree(High~.,data=train)
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

summary(test$High)

confusionMatrix(pred_tree$final,test$High)
