library(forecast)
library(fpp)
library(smooth)
library(readxl)
Airlines=read_excel(file.choose())
View(Airlines)
plot(Airlines$Passengers,type="o")

# So creating 12 dummy variables 
X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )
View(X)

colnames(X)<-month.abb # Assigning month names 
View(X)
AirlinesData<-cbind(Airlines,X)
View(AirlinesData)
colnames(AirlinesData)

AirlinesData["t"]<- 1:96
View(AirlinesData)
AirlinesData["log_Passenger"]<-log(AirlinesData["Passengers"])
AirlinesData["t_square"]<-AirlinesData["t"]*AirlinesData["t"]
attach(AirlinesData)

# Data Partition
require(caTools)
set.seed(123)
sample=sample.split(AirlinesData,SplitRatio=0.80)
train=subset(AirlinesData, sample==TRUE)
test=subset(AirlinesData, sample==FALSE)

########################### LINEAR MODEL #############################
linear_model<-lm(Passengers~t,data=train)
summary(linear_model)

linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Passengers-linear_pred$fit)^2,na.rm = T))
rmse_linear

######################### Exponential #################################

expo_model<-lm(log_Passenger~t,data=train)
summary(expo_model)

expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Passengers-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo 

######################### Quadratic ####################################

Quad_model<-lm(Passengers~t+t_square,data=train)
summary(Quad_model)

Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Passengers-Quad_pred$fit)^2,na.rm=T))
rmse_Quad 

######################### Additive Seasonality #########################

sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)

sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)

Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Passengers-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)

Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_Passenger~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)

multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_Passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 

multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Multiplicative Seasonality Linear trend  has least RMSE value

new_model<-lm(log_Passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = AirlinesData)
new_model_pred<-data.frame(predict(new_model,newdata=AirlinesData,interval='predict'))
new_model_fin <- exp(new_model$fitted.values)

View(new_model_fin)

pred_res<- predict(arima(log_Passenger,order=c(1,0,0)),n.ahead = 12)
Month <- as.data.frame(Airlines$Month)

Final <- as.data.frame(cbind(Month,AirlinesData$Passengers,new_model_fin))
colnames(Final) <-c("Month","Passengers","New_Pred_Value")
Final <- as.data.frame(Final)
View(Final)

plot(Final$New_Pred_Value,type="o")

## smoothing techniques
# Simple Exponential Smoothing
fit1A <- ses(Airlines, alpha=0.2, initial="simple", h=3)
fit2A <- ses(Airlines, alpha=0.6, initial="simple", h=3)
fit3A <- ses(Airlines, alpha=0.89, initial="simple", h=3)


plot(fit1A, plot.conf=FALSE, ylab="Sales",
     xlab="Month", main="", fcol="white", type="o")
lines(fitted(fit1A), col="blue", type="o")
lines(fitted(fit2A), col="red", type="o")
lines(fitted(fit3A), col="green", type="o")
lines(fit1A$mean, col="blue", type="o")
lines(fit2A$mean, col="red", type="o")
lines(fit3A$mean, col="green", type="o")
legend("topleft",lty=1, col=c(1,"blue","red","green"), 
       c("data", expression(alpha == 0.2), expression(alpha == 0.6),
         expression(alpha == 0.89)),pch=1)

## Insights ##
## (Based on the above analysis Multiplicative Seasonality Linear trend has least RMSE value. 
##     So i would like to use this trend for Forecasting)


# Ref

#   model                  RMSE
#1	rmse_linear	      -   34.44124
#2	rmse_expo	       -    33.59133
#3	rmse_Quad	        -   33.32199
#4	rmse_sea_add	    -   81.15428
#5	rmse_Add_sea_Quad	-   16.14047
#6	rmse_multi_sea	  -   82.60800
#7	rmse_multi_add_sea - 	11.22028
