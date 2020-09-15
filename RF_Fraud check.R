# Using Random Forest
#install.packages("randomForest")
library(randomForest)
fraudcheck = read.csv(file.choose())
View(fraudcheck)
class(fraudcheck$Taxable.Income)
str(fraudcheck)



# if greater than or equal to 30000 then high sales
highincome = ifelse(fraudcheck$Taxable.Income<=30000, "Risky","Good")
fc = data.frame(fraudcheck[1:6],highincome)
str(fc)
table(fc$highincome)

# Data partition
set.seed(123)
ind = sample(2, nrow(fc), replace = TRUE, prob = c(0.7,0.3))
train = fc[ind==1,]
test = fc[ind==2,]
set.seed(213)
# Description of the random forest with no of trees
rf = randomForest(highincome~., data = train)
rf

#Prediction and Condusion matrix on train data
pred1 = predict(rf,train)
head(pred1)

head(train$highincome)

library(caret)

confusionMatrix(pred1, train$highincome)


# more than 95% CI
#Prediction with test data


pred2 = predict(rf,test)
confusionMatrix(pred2, test$highincome)

plot(rf)

# Visualization 
MDSplot(rf, cd$highsales)

# at 200 there is a constant line and it doesnot vary after 200 trees

# Tune Random Forest Model mtry 
tune <- tuneRF(train[,-6], train[,6], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)

rf1 <- randomForest(highincome~., data=train, ntree = 200, mtry = 2, importance = TRUE,
                    proximity = TRUE)
rf1


pred1 <- predict(rf1, train)
confusionMatrix(pred1, train$highincome) 


#The randomForest() function can be used to perform both random forests and bagging
# Bagging 

set.seed(1)
train=sample(1:nrow(fc),nrow(fc)/2)
fc.test=fc[-train,]
Bag=randomForest(highincome~.,data=fc,subset=train,mtry=6,importance=TRUE)#mtry=6 is bagging
Bag

yhat=predict(Bag,newdata=fc.test)
y=fc[-train,"highincome"]
plot(y,yhat)
abline(0,1,col=2)

mean((y-yhat)^2)

importance(Bag)

Bag=randomForest(highincome~.,data=fc,subset=train,mtry=6,ntree=25)#No.of trees=25,default=500
Bag

#Boosting
set.seed(1)
train=sample(1:nrow(fc),nrow(fc)/2)
fc.test=fc[-train,]
boost.fc=gbm(highincome~.,data=fc[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost.fc)

##################################
# Splitting data into training and testing. 
# splitting the data based on species 
iris_setosa<-iris[iris$Species=="setosa",] # 50
iris_versicolor <- iris[iris$Species=="versicolor",] # 50
iris_virginica <- iris[iris$Species=="virginica",] # 50
iris_train <- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,])
iris_test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,])

# Building a random forest model on training data 
fit.forest <- randomForest(Species~.,data=iris_train, na.action=na.roughfix,importance=TRUE)
# Training accuracy 
mean(iris_train$Species==predict(fit.forest,iris_train)) # 100% accuracy 

# Prediction of train data
pred_train <- predict(fit.forest,iris_train)
library(caret)


# Confusion Matrix
confusionMatrix(iris_train$Species, pred_train)


# Predicting test data 
pred_test <- predict(fit.forest,newdata=iris_test)
mean(pred_test==iris_test$Species) # Accuracy = 94.6 % 


# Confusion Matrix 

confusionMatrix(iris_test$Species, pred_test)

# Visualization 
plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:4)


############ WBCD #################
# Read the dataset
wbcd <- read.csv(file.choose())
View(wbcd)
#First colum in dataset is id which is not required so we will be taking out
wbcd <- wbcd[-1]

#table of diagonis B <- 357 and M <- 212
table(wbcd$diagnosis)

# Replace B with Benign and M with Malignant. Diagnosis is factor with 2 levels that is B and M. We also replacing these two entery with Benign and Malignat
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B","M"), labels = c("Benign","Malignant"))

# table or proportation of enteries in the datasets. What % of entry is Bengin and % of entry is Malignant
round(prop.table(table(wbcd$diagnosis))*100,1)
summary(wbcd[c("radius_mean","texture_mean","perimeter_mean")])
#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
#test normalization
norm(c(1,2,3,4,5))
norm(c(10,20,30,40,50))
#Apply the normalization function to wbcd dataset
wbcd_n <- as.data.frame(lapply(wbcd[2:31], norm))
View(wbcd_n)
wbcd_n["diagnosis"] <- wbcd$diagnosis
# Building a random forest model on training data 
wbcd_forest <- randomForest(diagnosis~.,data=wbcd_n,importance=TRUE)
plot(wbcd_forest)
legend("topright",colnames(wbcd_forest$err.rate),col=1:3,cex=0.8,fill=1:3)

acc_wbcd <- mean(wbcd_n$diagnosis==predict(wbcd_forest))
acc_wbcd
varImpPlot(wbcd_forest)
