install.packages("dplyr") # To bind rows or columns for data-set with different no. of columns.
library(dplyr)

##combined_data <- bind_rows(Property_Price_Train,Property_Price_Test)

sum(colSums(is.na(combined_data)))
class (combined_data)

# temp_data <- combined_data[,-81]
# sum(is.na(temp_data))
# nrow(temp_data)*ncol(temp_data)
# (13960/233440)*100


train_set <- read.csv(file = "D:/Imarticus lecture Files/Linear Regression/Property_Price_Train.csv", na.strings = c(" ","NA"))
test_set <- read.csv(file = "D:/Imarticus lecture Files/Linear Regression/Property_Price_Test.csv", na.strings = c(" ","NA"))
View(train_set)
View(test_set)


dim(train_set)

dim(test_set)

# September 26


##dim(Property_Price_Train)
# 1459   81
##dim(Property_Price_Test)
# 1459   80

library(dplyr)
combined_data <- bind_rows(train_set,test_set)
dim(combined_data) 
# 2918   81
View(combined_data)
# Impute missing values using CT 
colsum_ms <- colSums(is.na(combined_data))
dim(Property_Price_Train)
#1459   81
dim(Property_Price_Test)
# 1459   80

library(dplyr)
combined_data<-bind_rows(Property_Price_Train,Property_Price_Test)
dim(combined_data) # 2918   81

# Impute missing values using CT 
colsum_ms<-colSums(is.na(combined_data))
# if any column has more than 50 % of missing values we can remove that column 
colsum_ms<-as.data.frame(colsum_ms)
head(colsum_ms)
colsum_ms$column_name<-names(combined_data)
head(colsum_ms)
fix(colsum_ms)
sub1<-subset(colsum_ms,colsum_ms$missing_entries>1459,select=column_name)
sub1
combined_data$Lane_Type<-NULL
combined_data$Pool_Quality<-NULL
combined_data$Fence_Quality<-NULL
combined_data$Miscellaneous_Feature<-NULL
colSums(is.na(combined_data))
combined_data$Fireplace_Quality<-NULL

colsum_ms<-colSums(is.na(combined_data))
colsum_ms<-as.data.frame(colsum_ms)
colsum_ms$column_name<-names(combined_data)
fix(colsum_ms)
sub2<-subset(colsum_ms,colsum_ms$missing_entries>0,select=column_name)
sub2
vect1<-sub2$column_name
vect1<-vect1[1:29]
# Knn imputations 
install.packages("VIM")
library(VIM)
imputed_data<-kNN(combined_data,variable = vect1)
dim(imputed_data)
View(imputed_data)
final_data<-imputed_data[,1:76]
write.csv(final_data,file="C:/personal/dsp55/Exercise 2 - Property Price Prediction/final_data.csv")

str(final_data)
library(dplyr)
dim(final_data)
summary(final_data)
final_data$X<-NULL
final_data$Id<-NULL
num_variables<-select_if(final_data,is.numeric)
categorical_variables<-select_if(final_data,is.factor)
dim(num_variables) # 2918   37
dim(categorical_variables) #2918   38

# 1. create cor matrix 
num_variables$Id<-NULL
num_variables<-num_variables[1:1459,]
cor_matrix<-cor(num_variables)
cor_df<-as.data.frame(cor_matrix)
class(cor_df)
cor_var<-cor_df$Sale_Price
head(cor_df)
cor_var
colnames_num<-names(num_variables)
cor_num_df<-data.frame(colnames_num,cor_var)
cor_num_df
sig_var<-subset(cor_num_df,(cor_num_df$cor_var>0.15 & cor_num_df$cor_var<1) | 
                  (cor_num_df$cor_var< 0 & cor_num_df$cor_var < -0.15))
dim(sig_var)
sig_var

# factor variables 
dim(categorical_variables) # 2918   38
categorical_variables<-categorical_variables[1:1459,]
summary(categorical_variables)
# Road_Type,Utility_Type,Condition2,Roof_Quality,Heating_Type
final_data$Road_Type<-NULL
final_data$Utility_Type<-NULL
final_data$Condition2<-NULL
final_data$Roof_Quality<-NULL
final_data$Heating_Quality<-NULL

categorical_variables<-select_if(final_data,is.factor)
categorical_variables<-categorical_variables[1:1459,]
summary(categorical_variables)

#Lot_Configuration
#FR3P = FR2P 
final_data$Lot_Configuration<-as.character(final_data$Lot_Configuration)
final_data$Lot_Configuration[final_data$Lot_Configuration=="FR3P"]<-"FR2P"
final_data$Lot_Configuration<-as.factor(final_data$Lot_Configuration)

#Roof_Design
#Shed = Gable
final_data$Roof_Design<-as.character(final_data$Roof_Design)
final_data$Roof_Design[final_data$Roof_Design=="Shed"]<-"Gable"
final_data$Roof_Design<-as.factor(final_data$Roof_Design)

#Exterior_Condition
#Ex,Po:TA
final_data$Exterior_Condition<-as.character(final_data$Exterior_Condition)
final_data$Exterior_Condition[final_data$Exterior_Condition=="EX"|
                                final_data$Exterior_Condition=="Po"]<-"TA"
final_data$Exterior_Condition<-as.factor(final_data$Exterior_Condition)


#Foundation_Type:
#S,W =PC
final_data$Foundation_Type<-as.character(final_data$Foundation_Type)
final_data$Foundation_Type[final_data$Foundation_Type=="S"|
                                final_data$Foundation_Type=="W"]<-"PC"
final_data$Foundation_Type<-as.factor(final_data$Foundation_Type)


#Basement_Condition:
# Po:TA
final_data$Basement_Condition<-as.character(final_data$Basement_Condition)
final_data$Basement_Condition[final_data$Basement_Condition=="Po"]<-"TA"
final_data$Basement_Condition<-as.factor(final_data$Basement_Condition)


final_data$Heating_Type<-NULL

#Electrical_System
#FuseP,Mix  = SBrkr
final_data$Electrical_System<-as.character(final_data$Electrical_System)
final_data$Electrical_System[final_data$Electrical_System=="FuseP"|
                             final_data$Electrical_System=="Mix"]<-"SBrkr"
final_data$Electrical_System<-as.factor(final_data$Electrical_System)


#Functional_Rate
#MajD1,MajD2 = MajD
#MD,MD1,MD2 = MD
#Mod,MS,SD,Sev= TF

final_data$Functional_Rate<-as.character(final_data$Functional_Rate)
final_data$Functional_Rate[final_data$Functional_Rate=="MajD1"|
                             final_data$Functional_Rate=="MajD2"]<-"MAjD"

final_data$Functional_Rate[final_data$Functional_Rate=="MD"|
                             final_data$Functional_Rate=="MD1"|
                             final_data$Functional_Rate=="MD2"]<-"MD"

final_data$Functional_Rate[final_data$Functional_Rate=="Mod"|
                             final_data$Functional_Rate=="MS"|
                             final_data$Functional_Rate=="SD"|
                             final_data$Functional_Rate=="Sev"]<-"TF"
final_data$Functional_Rate<-as.factor(final_data$Functional_Rate)
 
#Garage:
#2TFes,2Types=Attchd
final_data$Garage<-as.character(final_data$Garage)
final_data$Garage[final_data$Garage=="2TFes"|
                    final_data$Garage=="2Types"]<-"Attchd"
final_data$Garage<-as.factor(final_data$Garage)

#Garage_Quality:
 # Ex,Po=TA
final_data$Garage_Quality<-as.character(final_data$Garage_Quality)
final_data$Garage_Quality[final_data$Garage_Quality=="Ex"|
                    final_data$Garage_Quality=="Po"]<-"TA"
final_data$Garage_Quality<-as.factor(final_data$Garage_Quality)


#Garage_Condition
#Ex,Po=TA
final_data$Garage_Condition<-as.character(final_data$Garage_Condition)
final_data$Garage_Condition[final_data$Garage_Condition=="Ex"|
                            final_data$Garage_Condition=="Po"]<-"TA"
final_data$Garage_Condition<-as.factor(final_data$Garage_Condition)


# Sale_Type
#Con ConLD ConLI ConLw   CWD, Oth  = Others
final_data$Sale_Type<-as.character(final_data$Sale_Type)
final_data$Sale_Type[final_data$Sale_Type=="Con"|
                       final_data$Sale_Type=="ConLD"|
                       final_data$Sale_Type=="ConLI"|
                       final_data$Sale_Type=="CWD"|
                       final_data$Sale_Type=="Oth"]<-"others"
final_data$Sale_Type<-as.factor(final_data$Sale_Type)

#AbnoRMDl  Abnorml=Abnorml
#AdjLand   Alloca=  Alloca
#Normal  NoRMDal= Normal  
final_data$Sale_Condition<-as.character(final_data$Sale_Condition)
final_data$Sale_Condition[final_data$Sale_Condition=="AbnoRMDl"|
                            final_data$Sale_Condition=="Abnorml" ]<-"Abnorml"
final_data$Sale_Condition[final_data$Sale_Condition=="AdjLand"|
                            final_data$Sale_Condition=="Alloca" ]<-"Alloca"
final_data$Sale_Condition[final_data$Sale_Condition=="Normal"|
                            final_data$Sale_Condition=="NoRMDal" ]<-"Normal"

final_data$Sale_Condition<-as.factor(final_data$Sale_Condition)
colSums(is.na(final_data))
library(dplyr)
categorical_variables<-dplyr::select_if(final_data,is.factor)
summary(final_data)

sig_var_names<-sig_var$colnames_num
sig_var_names
sub_sig_num<-subset(final_data,select = sig_var_names)
head(sub_sig_num)
dim(sub_sig_num)
categorical_variables<-dplyr::select_if(final_data,is.factor)
dim(categorical_variables)
house_data<-cbind(sub_sig_num,categorical_variables)
dim(house_data)
write.csv(house_data,file = "C:/personal/dsp55/Exercise 2 - Property Price Prediction/house_data.csv")
colSums(is.na(house_data))
house_data$X<-NULL
summary(house_data$Functional_Rate)
Train_house_data<-house_data[1:1459,]
Test_house_data<-house_data[1460:2918,]
summary(Train_house_data)
Train_house_data$Sale_Price<-final_data$Sale_Price[1:1459]
summary(Train_house_data)
dim(Train_house_data)
# full model on the given data
# checking outliers 
boxplot(Train_house_data$Sale_Price)
quantile(Train_house_data$Sale_Price)
IQR(Train_house_data$Sale_Price)
UW<- 214000 + 1.5*(84050);UW #340075
Train_house_data$Sale_Price[Train_house_data$Sale_Price>UW]<-UW
summary(Train_house_data$Sale_Price)

set.seed(200)
index<-sample(1459,0.75*1459)
head(index)
length(index)
train_data<-Train_house_data[index,]
test_data<-Train_house_data[-index,]
dim(train_data)
dim(test_data)
names(train_data)
# model on train data 
colSums(is.na(train_data))
house_model<-lm(Sale_Price~.,data = train_data)
house_model_null<-lm(Sale_Price~1,data = train_data)
summary(house_model)
# stepwise regression 
step(house_model_null,direction = "forward",scope = 
       list(lower=house_model_null,upper=house_model))

rev_house_model<-lm(formula = Sale_Price ~ Overall_Material + Neighborhood + Grade_Living_Area + 
                      Garage_Size + House_Type + BsmtFinType1 + Kitchen_Quality + 
                      Basement_Height + Fireplaces + Remodel_Year + Exterior1st + 
                      Underground_Full_Bathroom + Lot_Size + Lot_Extent + Condition1 + 
                      Land_Outline + Property_Slope + Functional_Rate + House_Design + 
                      Rooms_Above_Grade + Sale_Condition + Exposure_Level + Basement_Condition + 
                      Garage_Quality + BsmtFinSF1 + Lot_Configuration + Roof_Design + 
                      Bedroom_Above_Grade + Brick_Veneer_Type + Brick_Veneer_Area + 
                      Exterior_Condition + Foundation_Type + Total_Basement_Area + 
                      Exterior_Material + Zoning_Class + Sale_Type, data = train_data)

step(house_model,direction = "backward",
     scope = list(lower=house_model_null,upper=house_model))


rev_house_model_back<-lm(formula = Sale_Price ~ Lot_Extent + Lot_Size + Overall_Material + 
                           Remodel_Year + Brick_Veneer_Area + Total_Basement_Area + 
                           First_Floor_Area + Second_Floor_Area + Underground_Full_Bathroom + 
                           Bedroom_Above_Grade + Rooms_Above_Grade + Fireplaces + Garage_Size + 
                           Zoning_Class + Land_Outline + Lot_Configuration + Property_Slope + 
                           Neighborhood + Condition1 + House_Type + House_Design + Roof_Design + 
                           Exterior1st + Brick_Veneer_Type + Exterior_Material + Exterior_Condition + 
                           Foundation_Type + Basement_Height + Basement_Condition + 
                           Exposure_Level + BsmtFinType1 + Kitchen_Quality + Functional_Rate + 
                           Garage_Quality + Sale_Type + Sale_Condition, data = train_data)

library(car)
vif(rev_house_model)

# removing Neighborhood
rev_house_model_nb<-lm(formula = Sale_Price ~ Overall_Material  + Grade_Living_Area + 
                      Garage_Size + House_Type + BsmtFinType1 + Kitchen_Quality + 
                      Basement_Height + Fireplaces + Remodel_Year + Exterior1st + 
                      Underground_Full_Bathroom + Lot_Size + Lot_Extent + Condition1 + 
                      Land_Outline + Property_Slope + Functional_Rate + House_Design + 
                      Rooms_Above_Grade + Sale_Condition + Exposure_Level + Basement_Condition + 
                      Garage_Quality + BsmtFinSF1 + Lot_Configuration + Roof_Design + 
                      Bedroom_Above_Grade + Brick_Veneer_Type + Brick_Veneer_Area + 
                      Exterior_Condition + Foundation_Type + Total_Basement_Area + 
                      Exterior_Material + Zoning_Class + Sale_Type, data = train_data)

vif(rev_house_model_nb)

# Sale_Condition 
rev_house_model_SL<-lm(formula = Sale_Price ~ Overall_Material  + Grade_Living_Area + 
                         Garage_Size + House_Type + BsmtFinType1 + Kitchen_Quality + 
                         Basement_Height + Fireplaces + Remodel_Year + Exterior1st + 
                         Underground_Full_Bathroom + Lot_Size + Lot_Extent + Condition1 + 
                         Land_Outline + Property_Slope + Functional_Rate + House_Design + 
                         Rooms_Above_Grade  + Exposure_Level + Basement_Condition + 
                         Garage_Quality + BsmtFinSF1 + Lot_Configuration + Roof_Design + 
                         Bedroom_Above_Grade + Brick_Veneer_Type + Brick_Veneer_Area + 
                         Exterior_Condition + Foundation_Type + Total_Basement_Area + 
                         Exterior_Material + Zoning_Class + Sale_Type, data = train_data)

vif(rev_house_model_SL)

# House_Design

rev_house_model_HD<-lm(formula = Sale_Price ~ Overall_Material  + Grade_Living_Area + 
                         Garage_Size + House_Type + BsmtFinType1 + Kitchen_Quality + 
                         Basement_Height + Fireplaces + Remodel_Year + Exterior1st + 
                         Underground_Full_Bathroom + Lot_Size + Lot_Extent + Condition1 + 
                         Land_Outline + Property_Slope + Functional_Rate  + 
                         Rooms_Above_Grade  + Exposure_Level + Basement_Condition + 
                         Garage_Quality + BsmtFinSF1 + Lot_Configuration + Roof_Design + 
                         Bedroom_Above_Grade + Brick_Veneer_Type + Brick_Veneer_Area + 
                         Exterior_Condition + Foundation_Type + Total_Basement_Area + 
                         Exterior_Material + Zoning_Class + Sale_Type, data = train_data)

vif(rev_house_model_HD)


# Exterior1st
rev_house_model_Ex1<-lm(formula = Sale_Price ~ Overall_Material  + Grade_Living_Area + 
                         Garage_Size + House_Type + BsmtFinType1 + Kitchen_Quality + 
                         Basement_Height + Fireplaces + Remodel_Year  + 
                         Underground_Full_Bathroom + Lot_Size + Lot_Extent + Condition1 + 
                         Land_Outline + Property_Slope + Functional_Rate  + 
                         Rooms_Above_Grade  + Exposure_Level + Basement_Condition + 
                         Garage_Quality + BsmtFinSF1 + Lot_Configuration + Roof_Design + 
                         Bedroom_Above_Grade + Brick_Veneer_Type + Brick_Veneer_Area + 
                         Exterior_Condition + Foundation_Type + Total_Basement_Area + 
                         Exterior_Material + Zoning_Class + Sale_Type, data = train_data)

vif(rev_house_model_Ex1)

#Exterior_Material 
rev_house_model_EM<-lm(formula = Sale_Price ~ Overall_Material  + Grade_Living_Area + 
                          Garage_Size + House_Type + BsmtFinType1 + Kitchen_Quality + 
                          Basement_Height + Fireplaces + Remodel_Year  + 
                          Underground_Full_Bathroom + Lot_Size + Lot_Extent + Condition1 + 
                          Land_Outline + Property_Slope + Functional_Rate  + 
                          Rooms_Above_Grade  + Exposure_Level + Basement_Condition + 
                          Garage_Quality + BsmtFinSF1 + Lot_Configuration + Roof_Design + 
                          Bedroom_Above_Grade + Brick_Veneer_Type + Brick_Veneer_Area + 
                          Exterior_Condition + Foundation_Type + Total_Basement_Area + 
                            Zoning_Class + Sale_Type, data = train_data)
vif(rev_house_model_EM)

#BsmtFinType1 
rev_house_model_BFT<-lm(formula = Sale_Price ~ Overall_Material  + Grade_Living_Area + 
                         Garage_Size + House_Type  + Kitchen_Quality + 
                         Basement_Height + Fireplaces + Remodel_Year  + 
                         Underground_Full_Bathroom + Lot_Size + Lot_Extent + Condition1 + 
                         Land_Outline + Property_Slope + Functional_Rate  + 
                         Rooms_Above_Grade  + Exposure_Level + Basement_Condition + 
                         Garage_Quality + BsmtFinSF1 + Lot_Configuration + Roof_Design + 
                         Bedroom_Above_Grade + Brick_Veneer_Type + Brick_Veneer_Area + 
                         Exterior_Condition + Foundation_Type + Total_Basement_Area + 
                         Zoning_Class + Sale_Type, data = train_data)
vif(rev_house_model_BFT)

#Grade_Living_Area
rev_house_model_GLA<-lm(formula = Sale_Price ~ Overall_Material   + 
                          Garage_Size + House_Type  + Kitchen_Quality + 
                          Basement_Height + Fireplaces + Remodel_Year  + 
                          Underground_Full_Bathroom + Lot_Size + Lot_Extent + Condition1 + 
                          Land_Outline + Property_Slope + Functional_Rate  + 
                          Rooms_Above_Grade  + Exposure_Level + Basement_Condition + 
                          Garage_Quality + BsmtFinSF1 + Lot_Configuration + Roof_Design + 
                          Bedroom_Above_Grade + Brick_Veneer_Type + Brick_Veneer_Area + 
                          Exterior_Condition + Foundation_Type + Total_Basement_Area + 
                          Zoning_Class + Sale_Type, data = train_data)
vif(rev_house_model_GLA)

# Basement_Height
rev_house_model_BH<-lm(formula = Sale_Price ~ Overall_Material   + 
                          Garage_Size + House_Type  + Kitchen_Quality + 
                           Fireplaces + Remodel_Year  + 
                          Underground_Full_Bathroom + Lot_Size + Lot_Extent + Condition1 + 
                          Land_Outline + Property_Slope + Functional_Rate  + 
                          Rooms_Above_Grade  + Exposure_Level + Basement_Condition + 
                          Garage_Quality + BsmtFinSF1 + Lot_Configuration + Roof_Design + 
                          Bedroom_Above_Grade + Brick_Veneer_Type + Brick_Veneer_Area + 
                          Exterior_Condition + Foundation_Type + Total_Basement_Area + 
                          Zoning_Class + Sale_Type, data = train_data)
vif(rev_house_model_BH)
house_model_rev<-rev_house_model_BH

Sale_Price_pred<-predict(house_model_rev,train_data)
train_res<-train_data$Sale_Price - Sale_Price_pred
RMSE_train<-sqrt(mean(train_res^2));RMSE_train # 25101.31 

test_data$X<-NULL
Sale_Price_pred_test<-predict(house_model_rev,test_data)
test_res<-test_data$Sale_Price - Sale_Price_pred_test
RMSE_test<-sqrt(mean(test_res^2));RMSE_test # 27322.56
 
# Predict on TEST DATA 
summary(Test_house_data)
Test_house_data$Sale_Price<-predict(house_model_rev,Test_house_data)
summary(Train_house_data$Condition1)
summary(Test_house_data$Condition1)

Test_house_data$Condition1<-as.character(Test_house_data$Condition1)
Test_house_data$Condition1[Test_house_data$Condition1=="NoRMD"]<-"Norm" 
Test_house_data$Condition1<-as.factor(Test_house_data$Condition1)
Test_house_data$Sale_Price<-predict(house_model_rev,Test_house_data)
head(Test_house_data)




# autocor 
# correlation with itself : 
# cor of residues with its 1st lag 

head(train_res,15)
# DW test 
# DW = 2(1-r)
# r [-1 to +1] is coef of cor between res and its first lag 
# ho : data is not autocor 
# h1 : data is auto cor 

# r = 0  , DW =2
# r = -1 , DW =4
# r = +1 , DW =0

durbinWatsonTest(house_model_rev)
# randomizing values 
ind<-sample(nrow(train_data),nrow(train_data))
train_data_random<-train_data[ind,]
dim(train_data_random)
rev_house_model_random<-lm(formula = Sale_Price ~ Overall_Material   + 
                         Garage_Size + House_Type  + Kitchen_Quality + 
                         Fireplaces + Remodel_Year  + 
                         Underground_Full_Bathroom + Lot_Size + Lot_Extent + Condition1 + 
                         Land_Outline + Property_Slope + Functional_Rate  + 
                         Rooms_Above_Grade  + Exposure_Level + Basement_Condition + 
                         Garage_Quality + BsmtFinSF1 + Lot_Configuration + Roof_Design + 
                         Bedroom_Above_Grade + Brick_Veneer_Type + Brick_Veneer_Area + 
                         Exterior_Condition + Foundation_Type + Total_Basement_Area + 
                         Zoning_Class + Sale_Type, data = train_data_random)
durbinWatsonTest(rev_house_model_random)


