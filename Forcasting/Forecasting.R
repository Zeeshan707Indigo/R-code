library(readr)
Amtrak<-read.csv(file.choose()) # read the Amtrack data
View(Amtrak) # Seasonality 12 months 
# So creating 12 dummy variables 
X<- data.frame(outer(rep(month.abb,length = 159), month.abb,"==") + 0 )# Creating dummies for 12 months
colnames(X)<-month.abb # Assigning month names 
View(X)
trakdata<-cbind(Amtrak,X)
View(trakdata)
colnames(trakdata)[2]<-"Ridership"
colnames(trakdata)
trakdata["t"]<-c(1:159)
View(trakdata)
trakdata["log_rider"]<-log(trakdata["Ridership"])
trakdata["t_square"]<-trakdata["t"]*trakdata["t"]
attach(trakdata)
train<-trakdata[1:147,]
test<-trakdata[148:159,]

########################### LINEAR MODEL #############################

linear_model<-lm(Ridership~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
rmse_linear<-sqrt(mean((test$Ridership-linear_pred$fit)^2,na.rm = T))
rmse_linear

######################### Exponential #################################

expo_model<-lm(log_rider~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Ridership-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo

######################### Quadratic ####################################

Quad_model<-lm(Ridership~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Ridership-Quad_pred$fit)^2,na.rm=T))
rmse_Quad

######################### Additive Seasonality #########################

sea_add_model<-lm(Ridership~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Ridership-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Ridership~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Ridership-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_rider~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Ridership-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea

######################## Multiplicative Additive Seasonality ##########################

multi_add_sea_model<-lm(log_rider~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model)
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Ridership-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Additive seasonality with Quadratic has least RMSE value

#write.csv(trakdata,file="trakdata.csv",col.names = F,row.names = F)

####################### Predicting new data #############################

#test_data<-read.xlsx(file.choose(),1)
#View(test_data)
pred_new<-predict(Add_sea_Quad_model,newdata=trakdata,interval = 'predict') #newdata= test_data
pred_new

