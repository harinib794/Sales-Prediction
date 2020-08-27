
#Install required packages
  
#if(!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,cowplot, dplyr, caTools, leaps,caret,glmnet,MASS,regclass,jtools)

data_train<-read.csv("Bigmart_train.csv")#read data
str(data_train)
#We can see there are 4 numeric and 7 categorical variables.              

#Univariate analysis**       

ggplot(data_train)+geom_histogram(mapping=aes(x=Item_Outlet_Sales),binwidth = 100,fill="red")

#We can see the variable is right skewed and we would have to apply transformation to treat the skewness.   

ggplot(data_train)+geom_histogram(mapping=aes(x=Item_Weight),binwidth=0.5,fill="red")
ggplot(data_train)+geom_histogram(mapping=aes(x=Item_Visibility),binwidth=0.005,fill="red")
ggplot(data_train)+geom_histogram(mapping=aes(x=Item_MRP),binwidth=1,fill="red")

#There are lot of observations with Item_Visbility 0 which need to be imputed with a practical value as item cannot have zero visibility.         
#Item_Visibility is right-skewed and should be transformed to curb its skewness.        
#We can clearly see 4 different distributions for Item_MRP.
barplot(table(data_train$Item_Fat_Content))

#Change LF and low fat to Low Fat and reg to regular.       

data_train<-data_train %>% mutate(Item_Fat_Content=as.character(Item_Fat_Content))%>% mutate(Item_Fat_Content = replace(Item_Fat_Content,Item_Fat_Content=="LF"|Item_Fat_Content=="low fat","Low Fat")) %>% mutate(Item_Fat_Content = replace(Item_Fat_Content,Item_Fat_Content=="reg","Regular"))

p1=ggplot(data = data_train) + 
  geom_bar(mapping = aes(x = Item_Fat_Content))
p2=ggplot(data = data_train) + 
  geom_bar(mapping = aes(x = Item_Type))
p3=ggplot(data = data_train) + 
  geom_bar(mapping = aes(x = Outlet_Identifier))
p4=ggplot(data = data_train) + 
  geom_bar(mapping = aes(x = Outlet_Size))
p5=ggplot(data = data_train) + 
  geom_bar(mapping = aes(x = Outlet_Type))
p6=ggplot(data = data_train) + 
  geom_bar(mapping = aes(x = Outlet_Establishment_Year))
plot_grid(p1,p2,ncol=1)
plot_grid(p3,p4,ncol=1)
plot_grid(p5,p6,ncol=1)

#1.We can see outlet type for around 4000 observations is missing.We will use imputation techniques to substitute the size for these outlets.      
#2.Supermarket type 1 seems to be most popular.      

#Bivariate Analysis
ggplot(data_train) + 
  geom_point(aes(Item_Weight, Item_Outlet_Sales))

sapply(data_train,function(x)sum(is.na(x)))

#Impute missing Item_weights with mean weight of corresponding Item_Identifier.      
missing_index<-which(is.na(data_train$Item_Weight))
for(i in missing_index){
  Item<-data_train$Item_Identifier[i]
  data_train$Item_Weight[i]=mean(data_train$Item_Weight[data_train$Item_Identifier == Item],na.rm=T)
}
sum(is.na(data_train$Item_Weight))

#Impute 0 values of visibility with mean value of visibility for corresponding Item_Identifier.        
zero_index<-which(data_train$Item_Visibility == 0)
for(i in zero_index){
  Item<-data_train$Item_Identifier[i]
  data_train$Item_Visibility[i]=mean(data_train$Item_Visibility[data_train$Item_Identifier == Item],na.rm=T)
}
ggplot(data_train) + geom_histogram(aes(Item_Visibility), bins = 100)

data_train$Item_Type<-as.character(data_train$Item_Type)
IS_NA<-which(is.na(data_train$Item_Weight))
for(i in IS_NA)
{
  R<-data_train$Item_Type[i]
  data_train$Item_Weight[i]<-mean(data_train$Item_Weight[data_train$Item_Type==R],na.rm=T)
}
table(data_train$Outlet_Type,data_train$Outlet_Size)
data_train$Outlet_Size[data_train$Outlet_Size==""]<-"Small"

#Feature Engineering
  
#Item_New_Type = classify Item type into perishable and non perishable.      
#Item_Category = Categories based on Item_Identifier(DR for drinks,FD for food,NC fo non consumable).    
#Outlet_Years = Number of years outlet was open.    
#Price_per_unit_weight = Item_MRP/Item_weight.    

perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene","Household", "Soft Drinks")
data_train$Item_New_Type<-ifelse(data_train$Item_Type %in% perishable,"perishable","non-perishable")

table(data_train$Item_Type, substr(data_train$Item_Identifier, 1, 2))

data_train$Item_Category<-substr(data_train$Item_Identifier, 1, 2)
data_train$Outlet_Years<-2019-data_train$Outlet_Establishment_Year
data_train$Price_per_unit_weight<-data_train$Item_MRP/data_train$Item_Weight

data_train<-data_train %>% mutate(Outlet_Size = if_else(Outlet_Size=="Small",0,if_else(Outlet_Size=="Medium",1,2))) 
data_train<-data_train %>% mutate(Outlet_Location_Type = if_else(Outlet_Location_Type=="Tier 1",0,if_else(Outlet_Location_Type=="Tier 2",1,2)))

#Checking correlations       
cor(data_train$Outlet_Establishment_Year,data_train$Outlet_Years)
cor(data_train$Item_Weight,data_train$Price_per_unit_weight)
cor(data_train$Item_MRP,data_train$Price_per_unit_weight)

#Removing outlet_establishment_year and Item_MRP due to high correlations.       

data_final<-data_train[,-c(1,8,6,7)]

#Data pre-processing.      

ggplot(data_final)+geom_histogram(mapping=aes(x=Item_Visibility),binwidth=0.005,fill="red")
ggplot(data_final)+geom_histogram(mapping=aes(x=Price_per_unit_weight),binwidth=0.5,fill="red")

#Since item_visibility and Price_per_unit_weight are right skewed we will apply log transformation to scale them.      

data_final$Item_Visibility<-(data_final$Item_Visibility)^(1/3)
data_final$Price_per_unit_weight= log(data_final$Price_per_unit_weight)
ggplot(data_final)+geom_histogram(mapping=aes(x=Item_Visibility),binwidth=0.005,fill="red")
ggplot(data_final)+geom_histogram(mapping=aes(x=Price_per_unit_weight),binwidth=0.5,fill="red")

##split into test and train
split <- sample.split(data_final,SplitRatio = 0.8)
train.set <- subset(data_final,split=="TRUE")
test.set <- subset(data_final,split=="FALSE")

#Fitting a simple linear regression model.       
linear<-lm(Item_Outlet_Sales ~ ., data = train.set)
summary(linear)
MSE<-mean(linear$residuals^2)
RMSE<-sqrt(MSE)
RMSE
AIC(linear)
plot(linear$fitted.values,linear$residuals)
plot(linear,2)

#The simple linear regression model for Item_Outlet_Size gave R2 value of 0.54 and RMSE of 1163.28.We can see that there are a lot of insignificant predictors with high p values.We will use subset selection to find the best predictors.      

#Compute Variance Inflation Factors (VIF) for the numeric predictors.      
data_num<-train.set[,c(1,3,8,11,12)]
multi_check<-lm(Item_Outlet_Sales~.,data=data_num)
VIF(multi_check)

#VIF<5.No significant multicollinearity present.     
#As the fitted values increases,the variance of the residuals also increases.The residual plot is cone shaped which indicates heteroscedasticity.    
#We will use transformation of dependent variable Item_outlet_sales to remove heteroscedasticity.We will use sqrt and log to see which is better.     

linear1<-lm(log(Item_Outlet_Sales) ~ ., data = train.set)
summary(linear1)
MSE<-mean(linear1$residuals^2)
RMSE<-sqrt(MSE)
RMSE
AIC(linear1)
plot(linear1$fitted.values,linear1$residuals)
plot(linear1,2)
#Heteroscedasticity is reduced by taking log transformation of dependent variable, Item_Outlet_Sales. Regression model using log transformed Item_Outlet_Size gave R2 value of 0.73 and RMSE of 0.51 and AIC of 9784.412.As we can see there are a lot of insignificant predictors.Hence,we can perform subset selection to determine the best predictors that explain Item Outlet Sales.        

#Best subset selection
train.set$Item_Outlet_Sales<-log(train.set$Item_Outlet_Sales)
smallest <- Item_Outlet_Sales ~ 1
biggest <- Item_Outlet_Sales ~ Item_Weight+Item_Fat_Content+Item_Visibility +Item_Type+ Outlet_Size  +Outlet_Location_Type + Outlet_Type +Item_New_Type+Item_Category +Outlet_Years+Price_per_unit_weight
m <- lm(Item_Outlet_Sales ~ Item_Weight, data=train.set)
stats::step(m, scope=list(lower=smallest, upper=biggest)) 
lm_step<-lm(Item_Outlet_Sales ~ Item_Weight + Outlet_Type + Price_per_unit_weight + Item_Fat_Content + Outlet_Location_Type, data = train.set)
summary(lm_step)
MSE<-mean(lm_step$residuals^2)
RMSE<-sqrt(MSE)
RMSE
AIC(lm_step)

#Step wise selection method was used to select the best subset of predictors using AIC criteria. The resulting best model has 5 variables which are Outlet_Type,Price_per_unit_weight,Item_Weight,Item_Fat_Content and Outlet_location_type.The model has R2 value of 0.74 and RMSE of 0.51 and AIC has reduced from 9784.412 to 9767.42.     

#Testing the model on test set.      
pred<-predict(lm_step,test.set[,-8])
test.set$Item_Outlet_Sales<-log(test.set$Item_Outlet_Sales)
residuals<-test.set$Item_Outlet_Sales-pred
RSS_test<-sum(residuals^2)
TSS_test<-sum((test.set$Item_Outlet_Sales-mean(test.set$Item_Outlet_Sales))^2)
R2_test<-1-(RSS_test/TSS_test)
MSE_test<-mean(residuals^2)
RMSE_test<-sqrt(MSE_test)
R2_test##R-square of train dataset
RMSE_test##residual mean standard error of train data set.

#The model has R2 value of 0.71 and RMSE of 0.54 on the test dataset.       

#Build Random Forest Model.      
control <- trainControl(method = 'cv',number = 5)
seed <- 1234
tgrid = expand.grid(mtry = c(3:10))
set.seed(seed)
fit_rf_grid <- train(Item_Outlet_Sales~.,data=train.set,method='rf',tuneGrid = tgrid,trControl=control)
fit_rf_grid
plot(fit_rf_grid)
varImp(fit_rf_grid)

#Used cross validation approach to select the best mtry value.mtry=10 gives model with lowest RMSE of 0.54 and highest R2 of 0.71.Hence selected as the final model.     

#Test random forest model on test dataset.       
predictions <- predict(fit_rf_grid,test.set[,-8])
residuals<-test.set$Item_Outlet_Sales-predictions
RSS_test<-sum(residuals^2)
TSS_test<-sum((test.set$Item_Outlet_Sales-mean(test.set$Item_Outlet_Sales))^2)
R2_test<-1-(RSS_test/TSS_test)
MSE_test<-mean(residuals^2)
RMSE_test<-sqrt(MSE_test)
R2_test##R-square of train dataset
RMSE_test##