library(data.table)
library(RSQLite)
library(ggplot2)
#install.packages('cowplot')
library(cowplot)
library(dplyr)
library(caret)
library(plotly)
#install.packages('xgboost')
library(xgboost)
#install.packages("e1071")
library(e1071)
#install.packages("corrplot")
library(corrplot)

rm(list = ls())
#-----------------------------------------------------------------------------------------------------------------------------------

train_data <- read.csv("train.csv")
test_data <- read.csv("test.csv")
data <- merge(train_data,test_data, all = TRUE)

str(train_data) #display structure of the data. 

#-----------------------------------------------------------------------------------------------------------------------------------

#Analysing the Sales in train_data 
ggplot(aes(train_data$Item_Outlet_Sales), data = train_data)+geom_histogram(binwidth = 120, fill="red")
#Analysing other variables
p1 <- ggplot(aes(data$Item_Weight), data = data)+geom_histogram(binwidth = 0.5, fill="blue")
p2 <- ggplot(aes(data$Item_Visibility), data = data)+geom_histogram(binwidth = 0.02, fill="blue")
p3 <- ggplot(aes(data$Item_MRP), data = data)+geom_histogram(binwidth = 1.5, fill="blue")
plot_grid(p1, p2, p3, nrow=1)

#Analysing the categorical variables.

#fat content 
ggplot(aes(data$Item_Fat_Content), data = data)+geom_bar(fill="red")
#changing the low fats
data$Item_Fat_Content[data$Item_Fat_Content=="LF"] <- "low fat"
data$Item_Fat_Content[data$Item_Fat_Content=="Low Fat"] <- "low fat"
#changing the regulars. 
data$Item_Fat_Content[data$Item_Fat_Content=="reg"] <- "Regular"

#item type 
ggplot(aes(data$Item_Type), data = data)+ geom_bar(fill="red")
table(data$Item_Type)
#Outlet Identifier
ggplot(aes(data$Outlet_Identifier), data = data)+ geom_bar(fill="red")
#Outlet Size - too many missing values
ggplot(aes(data$Outlet_Size), data = data)+ geom_bar(fill="red")
table(data$Outlet_Size)
#Establishment Year 
ggplot(aes(as.factor(data$Outlet_Establishment_Year)), data = data)+ geom_bar(fill="red")
#Outlet Type 
ggplot(aes(data$Outlet_Type), data = data)+ geom_bar(fill="red")


#BIVARIATE ANALYSIS:
#Item Weight vs Sales : 
p1 <- ggplot(aes(data$Item_Weight, data$Item_Outlet_Sales), data = data)+geom_point(alpha=0.3, color="red")
#Item Visiblity vs Sales: 
p2 <- ggplot(aes(data$Item_Visibility, data$Item_Outlet_Sales), data = data)+geom_point(alpha=0.3, color="red")
#Item MRP VS Sales 
p3 <- ggplot(aes(data$Item_MRP, data$Item_Outlet_Sales), data = data)+geom_point(alpha=0.3, color="red")
scndrw <- plot_grid(p2,p3, ncol = 2)
plot_grid(p1, scndrw, nrow = 2)


#We can see that many points have visiblity as 0, doesnt make sense 
#We can also see four distinct segments in MRP graph, this can be used to extract features and create new variable later 

#Now Categorical variables: 
#Fatcontent vs sales: 
ggplot(aes(data$Item_Fat_Content, data$Item_Outlet_Sales), data = data)+geom_violin(fill="red")
#Item Type vs Sales: 
p1 <- ggplot(aes(Item_Type,Item_Outlet_Sales), data=train_data)+geom_violin(fill="red")+
  theme(axis.text.x =  element_text(angle = 60, hjust = 1),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 8.5))
#Fat content vs sales 
p2 <- ggplot(aes(data$Item_Fat_Content,Item_Outlet_Sales), data=data)+geom_violin(fill="red")
#Outlet Identifier vs sales 
p3 <- ggplot(aes(data$Outlet_Identifier,Item_Outlet_Sales), data=data)+geom_violin(fill="red")
second <- plot_grid(p2, p3, ncol=2)
plot_grid(p1, second, nrow=2)


#Outlet no 10 and 9 are very similar and very different with other outlets. 

#outlet size violin: 
ggplot(aes(data$Outlet_Size, data$Item_Outlet_Sales), data = data)+geom_violin(fill="blue")
# we can see that the distribution of missing values are similar to dist of "Small"
#Outlet type vs sales violin 
ggplot(aes(data$Outlet_Type, data$Item_Outlet_Sales), data = data)+geom_violin(fill="blue")
#gricery store has most points on the lower tally of sales. 
#Outlet tier vs sales 
ggplot(aes(data$Outlet_Location_Type, data$Item_Outlet_Sales), data = data)+geom_violin(fill="blue")
#tier 1 is very closely similar to tier 3. 

#-----------------------------------------------------------------------------------------------------------------------------------

#MISSING VALUES TREATMENT: 

colSums(is.na(data))
# Item sales and item weight has missing values. Sales can be ignored -> they are test data. 

#imputing Item weight: 
ggplot(aes(data$Item_Weight, data$Item_Outlet_Sales), data = data)+geom_line()
#Theres no skew in the data, so we can impute the mean of weight to the missing value
mean_val <- mean(na.omit(data$Item_Weight))
data$Item_Weight[is.na(data$Item_Weight)] <- mean_val
data$Item_Weight <- format(round(data$Item_Weight,2))
table(data$Item_Weight)

#changing Item_Visiblity from 0
temp<- subset(data, data$Item_Visibility!=0)
data$Item_Visibility[data$Item_Visibility==0] <- mean(temp$Item_Visibility)
summary(data$Item_Visibility)

#-----------------------------------------------------------------------------------------------------------------------------------

#creating new features that arent gviven in the data: 

#Item_type_new - classify as perishables, non perishables and not sure 

perishables <- c("Bread", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishables <- c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", 
                     "Health and Hygiene","Household", "Soft Drinks")
data$Item_Type_new <- ifelse(is.element(data$Item_Type, perishables), "perishables",
                             ifelse(is.element(data$Item_Type, non_perishables),"non_perishables","not_sure"))

#Exploring the Item_identifier with Item_Type 
table(data$Item_Type, substr(data$Item_Identifier,1, 2))
# Here we can see that DR means Drinks, FD = food, NC=Non Consumable: We can create a new variable called Categories 
data$Item_Category <- substr(data$Item_Identifier,1,2)

#But the Non-Consumable products also has Fat values. this should be corrected. 
#change Fat for these items to Non-Consumable
data$Item_Fat_Content <- as.character(data$Item_Fat_Content)
data$Item_Fat_Content[data$Item_Category=="NC"] <- "Non_edible"
data$Item_Fat_Content <- as.factor(data$Item_Fat_Content)

#Outlet_Years - years of operation
data$Outlet_Years <- 2018-data$Outlet_Establishment_Year
data$Outlet_Years <- as.factor(data$Outlet_Years)


#Price per Unit 
data$price_per_unit_weight <- data$Item_MRP/as.numeric(data$Item_Weight)

#Item_sales had four distinct chunks, lets split them based on the values and create MRP_Clusters
ggplot(aes(data$Item_MRP, data$Item_Outlet_Sales), data = data)+geom_point(alpha=0.3, color="red")
data$Item_MRP_Clusters <- ifelse(data$Item_MRP <= 69,"1st", 
                                 ifelse(data$Item_MRP>69 & data$Item_MRP<=136,"2nd",
                                        ifelse(data$Item_MRP>136 & data$Item_MRP<=203,"3rd","4th")))


#-----------------------------------------------------------------------------------------------------------------------------------

#Label Encoding - Assign numbers to categorical variables: 
#Outlet_Size 
data$Outlet_Size_num <- ifelse(data$Outlet_Size=="Small",0, 
                           ifelse(data$Outlet_Size=="Medium",1,2))

data$Outlet_Location_Type_num <- ifelse(data$Outlet_Location_Type=="Tier 3",0,
                                    ifelse(data$Outlet_Location_Type=="Tier 2",1,2))

#Remove the categorical variables now. 
data <- data %>% select(-Outlet_Size,-Outlet_Location_Type)



ohe = dummyVars("~.", data = data%>% select(-Item_Identifier,-Outlet_Establishment_Year,-Item_Type,-Item_Weight), fullRank = T)
ohe_df = data.table(predict(ohe, data%>% select(-Item_Identifier,-Outlet_Establishment_Year,-Item_Type)))
newdata = cbind(data%>%select(Item_Identifier, Item_Weight), ohe_df)

#-----------------------------------------------------------------------------------------------------------------------------------

#Checking for skewness in Item Visiblity and Price per unit weight 
newdata$Item_Visibility <- log(newdata$Item_Visibility+1)
newdata$price_per_unit_weight <- log(newdata$price_per_unit_weight+1)

#Scaling all the numerical data in the dataset 
numvars <- which(sapply(newdata, is.numeric))
numnames <- names(numvars) # names of all numeric columns 
newdata_num = newdata[,setdiff(numnames, "Item_Outlet_Sales")]
prep_num = preProcess(newdata_num, method=c("center", "scale"))
newdata_numeric_norm = predict(prep_num, newdata_num)

newdata[,setdiff(numnames, "Item_Outlet_Sales")] <- NULL # removing numeric independent variables
newdata = cbind(newdata, newdata_numeric_norm)

#-----------------------------------------------------------------------------------------------------------------------------------

#splitting back train and test dataset 
test <- subset(newdata, is.na(newdata$Item_Outlet_Sales))
train <-subset(newdata, !is.na(newdata$Item_Outlet_Sales))
test$Item_Outlet_Sales <- NULL

#Finding correlation in the data 
train$Item_Weight <- as.numeric(train$Item_Weight)
train_sub <- train%>% select(-Item_Identifier)
cor_train <- cor(train_sub, use="pairwise.complete.obs")
corrplot(cor_train, method = "pie", type = "lower", tl.cex = 0.9)
# we can see some correlation between price per unit and item weight. 
# also between price per unit weight and MRP 
# THis is becuase price per unit was created using these two vars. 

#-----------------------------------------------------------------------------------------------------------------------------------

#Modeling 
#Linear, Lasso,Ridge, RandomForest, Xgboost. 

#Linear Regression: 
set.seed(1234)
my_control = trainControl(method = "cv", number = 5)
linear_reg_model <- train(x=train%>%select(-Item_Identifier,-Item_Outlet_Sales), y=train$Item_Outlet_Sales, method='glmnet',trControl=my_control)
test_sub <- data.matrix(test%>%select(-Item_Identifier))
submission$Item_Outlet_sales <- predict(linear_reg_model,test_sub)
test$Lin_sales <- predict(linear_reg_model,test_sub)
write.csv(submission, "linearRegFile.csv")

#-----------------------------------------------------------------------------------------------------------------------------------

#Lasso Regression: 
set.seed(1235)
my_control <- trainControl(method = "cv", number = 5)
Grid <- expand.grid(alpha=1,lambda = seq(0.001,0.1, by=0.0002))
lasso_model <- train(x=train%>%select(-Item_Identifier, -Item_Outlet_Sales), y=train$Item_Outlet_Sales,
                     method='glmnet',trControl=my_control,tuneGrid=Grid)
submission$Item_Outlet_sales <- predict(lasso_model,test_sub)
write.csv(submission,"lassopred.csv")
test$Lasso_sales <- predict(lasso_model,test_sub)

#-----------------------------------------------------------------------------------------------------------------------------------

#Ridge Regression - lasso with alpha =0
set.seed(1236)
my_control <- trainControl(method = "cv", number = 5)
Grid <- expand.grid(alpha=0,lambda = seq(0.001,0.1, by=0.0002))
ridge_model <- train(x=train%>%select(-Item_Identifier, -Item_Outlet_Sales), y=train$Item_Outlet_Sales,
                     method='glmnet',trControl=my_control,tuneGrid=Grid)
submission$Item_Outlet_sales <- predict(ridge_model,test_sub)
write.csv(submission,"ridgepred.csv")
test$ridge_sales <- predict(lasso_model,test_sub)

#-----------------------------------------------------------------------------------------------------------------------------------

#Random Forest: 

set.seed(1237)
my_control = trainControl(method="cv", number=5)
tgrid = expand.grid(
  .mtry = 5,# num of variables sampled at each split
  .splitrule = "variance",
  .min.node.size = 20
)
rf_mod = train(x = train%>%select(-Item_Identifier, -Item_Outlet_Sales), 
               y = train$Item_Outlet_Sales,
               method='ranger', 
               trControl= my_control, 
               tuneGrid = tgrid,
               num.trees = 400,
               importance = "permutation")
submission <- predict(rf_mod,test_sub)
write.csv(submission,"randompred.csv")
test$random_sales <- predict(rf_mod,test_sub)

#-----------------------------------------------------------------------------------------------------------------------------------

#XGBoost Model - 3 phase: General Paramerter (which booster we use), Booster Param (depends on the booster), Task Param (regression or other)

param_list = list(
  
  objective = "reg:linear", # its a regular linear reg model 
  eta=0.01,  # learning rate, higher eta is more penalty
  gamma = 1, # higher gamma is more conservative model 
  max_depth=6, # Max depth of the decision tree
  subsample=0.8, # WHat portion of sample to consider for the xgboost
  colsample_bytree=0.5 #ratio of variables randomly chosen to build the tree
)
#COnverting data to matrix form for xgBoost 

dtrain = xgb.DMatrix(data = as.matrix(train%>% select(-Item_Identifier, -Item_Outlet_Sales, -Outlet_Identifier.OUT027, -Outlet_Identifier.OUT018, -Outlet_Identifier.OUT049)), label= train$Item_Outlet_Sales)
dtest = xgb.DMatrix(data = as.matrix(test_sub))

# the model 
set.seed(112)
xgbcv = xgb.cv(params = param_list, 
               data = dtrain, 
               nrounds = 1000, 
               nfold = 5, 
               print_every_n = 10, 
               early_stopping_rounds = 30, 
               maximize = F)
# we can see that the best iteration is at 392. 

xgmodel <- xgb.train(data=dtrain, params=param_list, nrounds=392)
test$xg_sales <- predict(xgmodel,test_sub)

#-----------------------------------------------------------------------------------------------------------------------------------

#Calculating Variable Importance: 

var_imp = xgb.importance(feature_names = setdiff(names(train), c("Item_Identifier", "Item_Outlet_Sales")), 
                         model = xgmodel)
xgb.plot.importance(var_imp)
