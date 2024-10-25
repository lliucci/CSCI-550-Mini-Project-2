# Loading packages
library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(glmnet)
library(lubridate)
library(mosaic)
library(ggthemes)

conflicted::conflicts_prefer(dplyr::filter(),dplyr::lag(), base::sample(), base::mean())

## Reading Data
data = read_csv("cook_county_train_val.csv")

### Cleaning Data

# removing first column, just a rownname 

data <- data |> select(-...1)

## Descriptions Variable fixing

# Extracting important information from descriptions
sell_date = as.numeric(0)
rooms = as.numeric(0)
bedrooms = as.numeric(0)
baths = as.numeric(0)

# Takes a minute to run, not too bad though
for(i in 1:nrow(data)){
    sell_date[i] = str_split(str_split(data$Description[i], "sold on ")[[1]][2], ", is a")[[1]][1]
    rooms[i] = str_extract_all(str_split(data$Description[i], "total of ")[[1]][2], "\\d")[[1]][1]
    bedrooms[i] = str_extract_all(str_split(data$Description[i], "total of ")[[1]][2], "\\d")[[1]][2]
    baths[i] = str_split(str_split(data$Description[i], "bedrooms, and ")[[1]][2], " of which are bathrooms")[[1]][1]
}

# Adding features of descriptions, removing descriptions
data = data %>%
    mutate(
      Sell_Date = mdy(sell_date),
      Rooms = as.numeric(rooms),
      Bedrooms = as.numeric(bedrooms),
      Baths = as.numeric(baths)
    ) %>%
    select(-Description)

# Removing rows where num bathrooms/bedrooms exceeds number of rooms
data = data %>%
    filter(Bedrooms < Rooms | Baths < Rooms)


# Removing rows with at least 1 missing value
data = data %>%
    drop_na()

# Filtering extreme observations
is_outlier = function(x){
    result = abs(x - mean(x)) > 3*sd(x)
    return(result)
}

# Cleaning up the environment
rm(baths)
rm(bedrooms)
rm(i)
rm(rooms)
rm(sell_date)


# Removing Spaces
data = data %>%
  rename(Property_Class = `Property Class`,
        Neighborhood_Code = `Neighborhood Code`,
        Land_Square_Feet = `Land Square Feet`,
        Town_Code = `Town Code`,
        Wall_Material = `Wall Material`,
        Roof_Material = `Roof Material`,
        Basement_Finish = `Basement Finish`,
        Central_Heating = `Central Heating`,
        Other_Heating = `Other Heating`,
        Central_Air = `Central Air`,
        Attic_Type = `Attic Type`,
        Attic_Finish = `Attic Finish`,
        Design_Plan = `Design Plan`,
        Cathedral_Ceiling = `Cathedral Ceiling`,
        Construction_Quality = `Construction Quality`,
        Site_Desirability = `Site Desirability`,
        Garage_1_Size = `Garage 1 Size`,
        Garage_1_Material = `Garage 1 Material`,
        Garage_1_Attachment = `Garage 1 Attachment`,
        Garage_1_Area = `Garage 1 Area`,
        Garage_2_Size = `Garage 2 Size`,
        Garage_2_Material = `Garage 2 Material`,
        Garage_2_Attachment = `Garage 2 Attachment`,
        Garage_2_Area = `Garage 2 Area`,
        Other_Improvements = `Other Improvements`,
        Building_Square_Feet = `Building Square Feet`,
        Repair_Condition  = `Repair Condition`,
        Multi_Code = `Multi Code`,
        Number_of_Commercial_Units = `Number of Commercial Units`,
        Estimate_Land = `Estimate (Land)`,
        Estimate_Building = `Estimate (Building)`,
        Deed_No = `Deed No.`,
        Sale_Price = `Sale Price`,
        Census_Tract = `Census Tract`,
        Multi_Property_Indicator = `Multi Property Indicator`,
        Modeling_Group = `Modeling Group`,
        OHare_Noise = `O'Hare Noise`,
        Road_Proximity = `Road Proximity`,
        Sale_Year = `Sale Year`,
        Sale_Quarter = `Sale Quarter`,
        Sale_Half_Year = `Sale Half-Year`,
        Sale_Quarter_of_Year = `Sale Quarter of Year`,
        Sale_Month_of_Year = `Sale Month of Year`,
        Sale_Half_of_Year = `Sale Half of Year`,
        Most_Recent_Sale = `Most Recent Sale`,
        Age_Decade = `Age Decade`,
        Pure_Market_Filter = `Pure Market Filter`,
        Garage_Indicator = `Garage Indicator`,
        Neighborhood_Code_map = `Neigborhood Code (mapping)`,
        Town_and_Neighborhood = `Town and Neighborhood`,
        Lot_Size = `Lot Size`)

# Removing outliers for variables where max() is greater than q3()
data = data %>%
    filter(!is_outlier(Sale_Price),
           !is_outlier(Land_Square_Feet),
           !is_outlier(Baths),
           !is_outlier(Lot_Size),
           !is_outlier(Town_and_Neighborhood),
           !is_outlier(Age_Decade),
           !is_outlier(Age),
           !is_outlier(Estimate_Land ),
           !is_outlier(Estimate_Building),
           !is_outlier(Building_Square_Feet))


# Save cleaned data
write_csv(data, "data_cleaned.csv")

# Read in cleaned data, so we don't have to run stuff again

data <- read_csv("data_cleaned.csv")

# data no factors, removing predictors that will not have any predictive power/will not work with model.matrix

data_nofactors <- data |> select(-c(Modeling_Group,Use,PIN))

# Making some of the variable factors

# Converting Columns into factors with levels
data <- data |> 
  mutate(Property_Class = factor(Property_Class),
         # Neighborhood_Code = factor(Neighborhood_Code),
         # Town_Code = factor(Town_Code),
         Apartments = factor(Apartments),
         Wall_Material = factor(Wall_Material),
         Roof_Material = factor(Roof_Material),
         Basement = factor(Basement),
         Basement_Finish = factor(Basement_Finish),
         Central_Heating = factor(Central_Heating),
         Other_Heating = factor(Other_Heating),
         Central_Air = factor(Central_Air),
         Attic_Type = factor(Attic_Type),
         Attic_Finish = factor(Attic_Finish),
         Design_Plan = factor(Design_Plan),
         Cathedral_Ceiling = factor(Cathedral_Ceiling),
         Construction_Quality = factor(Construction_Quality),
         Site_Desirability = factor(Site_Desirability),
         Garage_1_Size = factor(Garage_1_Size),
         Garage_1_Material = factor(Garage_1_Material),
         Garage_1_Attachment = factor(Garage_1_Attachment),
         Garage_1_Area = factor(Garage_1_Area),
         Garage_2_Size = factor(Garage_2_Size),
         Garage_2_Material = factor(Garage_2_Material),
         Garage_2_Attachment = factor(Garage_2_Attachment),
         Garage_2_Area = factor(Garage_2_Area),
         Porch = factor(Porch),
         Other_Improvements = factor(Other_Improvements),
         Repair_Condition = factor(Repair_Condition),
         Multi_Code = factor(Multi_Code),
         Multi_Property_Indicator = factor(Multi_Property_Indicator),
         Modeling_Group = factor(Modeling_Group),
         Use = factor(Use),
         OHare_Noise = factor(OHare_Noise),
         Floodplain = factor(Floodplain),
         Road_Proximity = factor(Road_Proximity),
         # Sale_Year = factor(Sale_Year),
         # Sale_Quarter = factor(Sale_Quarter),
         # Sale_Half_Year = factor(Sale_Half_Year),
         Sale_Quarter_of_Year = factor(Sale_Quarter_of_Year),
         # Sale_Month_of_Year = factor(Sale_Month_of_Year),
         Sale_Half_of_Year = factor(Sale_Half_of_Year),
         Most_Recent_Sale = factor(Most_Recent_Sale),
         Pure_Market_Filter = factor(Pure_Market_Filter),
         Garage_Indicator = factor(Garage_Indicator)
         # Town_and_Neighborhood = factor(Town_and_Neighborhood)
  )


# removing data that only has one level  (and PIN)
data <- data |> select(-c(Use, Modeling_Group ,PIN)) # removing PIN because it seems like just a different row name variable



############################################################### Modeling ########################################################

# Splitting data into test and training sets 

set.seed(42069)
train = sample(1:dim(data)[1], 0.8*dim(data)[1])
test <- -train
data.train <- data[train,]
data.test <- data[test,]

# Simple Linear Regression -----------------------------
lm_slr = lm(Sale_Price ~ Age, data = data)
summary(lm_slr)
lm_slr_land_size <- lm(Sale_Price~ Land_Square_Feet, data = data)
summary(lm_slr_land_size)
lm_slr_build_size <- lm(Sale_Price~Land_Square_Feet, data = data)
summary(lm_slr_build_size)


# Multiple Linear Regression ---------------------------
lm_mlr = lm(Sale_Price ~ Age + Baths * Bedrooms, data = data)
summary(lm_mlr)

# MLR with longitude and lattitude
lm_longlat<- lm(Sale_Price ~ Longitude + Latitude, data = data)
summary(lm_longlat)

# Stepwise Regression - Backward Selection and Hybrid Selection

lm_step_back <- lm(Sale_Price ~. , data = data)
lm_step_back <- step(lm_step_back, direction = "back", trace = 0)
summary(lm_step_back)

lm_step_hybrid <- lm(Sale_Price ~., data = data)
lm_step_hybrid <- step(lm_step_hybrid, direction = "both", trace = 0)
summary(lm_step_hybrid)

# PCA --------------------------------------------------
numeric_data = data %>% select_if(is.numeric)
PCs = prcomp(numeric_data, scale = T)

PCs$sdev^2/sum(PCs$sdev^2)

# Non-Linear Models ------------------------------------
lm_nonlinear = lm(Sale_Price ~ poly(Age, 3), data = data)
summary(lm_nonlinear)

# Comparing Models -------------------------------------
AIC(lm_slr, lm_mlr, lm_nonlinear)


# Lasso and Ridge Regression ---------------------------

data.train.mat <- model.matrix(Sale_Price ~. -Sell_Date , data = data.train)[,-1]
data.test.mat <- model.matrix(Sale_Price ~. -Sell_Date, data = data.test)[,-1]

# Fitting Ridge Regression model

data.fit.ridge <- glmnet(data.train.mat, data.train$Sale_Price, alpha = 0)
data.cv.ridge <- cv.glmnet(data.train.mat, data.train$Sale_Price, alpha = 0) 
data.bestlam.ridge <- data.cv.ridge$lambda.min
data.bestlam.ridge

data.pred.ridge <- predict(data.fit.ridge, s = data.bestlam.ridge, newx = data.test.mat)
## Coefficients of Ridge Regression
predict(data.fit.ridge, s = data.bestlam.ridge, type = "coefficients")

## Calculate MSE
mean((data.pred.ridge - data.test$Sale_Price)^2)

# Fitting Lasso Regression model

data.fit.lasso <- glmnet(data.train.mat, data.train$Sale_Price, alpha = 1)
data.cv.lasso <- cv.glmnet(data.train.mat, data.train$Sale_Price, alpha = 1)
data.bestlam.lasso <- data.cv.lasso$lambda.min
data.bestlam.lasso

data.pred.lasso <- predict(data.fit.lasso, s = data.bestlam.lasso, newx = data.test.mat)
## Coefficients of Lasso Regression
predict(data.fit.lasso, s = data.bestlam.lasso, type = "coefficients")

## Calculate MSE

mean((data.pred.lasso - data.test$Sale_Price)^2)


