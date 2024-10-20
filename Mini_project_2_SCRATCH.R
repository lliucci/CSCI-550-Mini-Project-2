## Mini Project 2 Scratch Work

library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(glmnet)
library(lubridate)
library(mosaic)
conflicted::conflicts_prefer(dplyr::filter(),dplyr::lag(), base::sample(), base::mean())


# Reading Data
data = read_csv("cook_county_train_val.csv")

# Removing first column as it is row ID
data = data %>%
  select(-`...1`)

# Extracting important information from descriptions
sell_date = as.numeric(0)
rooms = as.numeric(0)
bedrooms = as.numeric(0)
baths = as.numeric(0)

for(i in 1:nrow(data)){
  sell_date[i] = str_split(str_split(data$Description[i], "sold on ")[[1]][2], ", is a")[[1]][1]
  rooms[i] = str_extract_all(str_split(data$Description[i], "total of ")[[1]][2], "\\d")[[1]][1]
  bedrooms[i] = str_extract_all(str_split(data$Description[i], "total of ")[[1]][2], "\\d")[[1]][2]
  baths[i] = str_split(str_split(data$Description[i], "bedrooms, and ")[[1]][2], " of which are bathrooms")[[1]][1]
}

# Adding features of descriptions, removing descriptions
data = data %>%
  mutate(Sell_Date = mdy(sell_date),
         Rooms = as.numeric(rooms),
         Bedrooms = as.numeric(bedrooms),
         Baths = as.numeric(baths)) %>%
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

apply(data, 2, favstats)

# Removing outliers for variables where max() is greater than q3()
data = data %>%
  filter(!is_outlier(`Sale Price`),
         !is_outlier(`Land Square Feet`),
         !is_outlier(Baths),
         !is_outlier(`Lot Size`),
         !is_outlier(`Town and Neighborhood`),
         !is_outlier(`Age Decade`),
         !is_outlier(`Age`),
         !is_outlier(`Estimate (Land)`),
         !is_outlier(`Estimate (Building)`),
         !is_outlier(`Building Square Feet`),
         !is_outlier(`Other Improvements`))


# Converting Columns into factors with levels
data <- data |> 
  mutate(`Property Class` = factor(`Property Class`),
         `Neighborhood Code` = factor(`Neighborhood Code`),
         `Town Code` = factor(`Town Code`),
         Apartments = factor(Apartments),
         `Wall Material` = factor(`Wall Material`),
         `Roof Material` = factor(`Roof Material`),
         Basement = factor(Basement),
         `Basement Finish` = factor(`Basement Finish`),
         `Central Heating` = factor(`Central Heating`),
         `Other Heating` = factor(`Other Heating`),
         `Central Air` = factor(`Central Air`),
         `Attic Type` = factor(`Attic Type`),
         `Attic Finish` = factor(`Attic Finish`),
         `Design Plan` = factor(`Design Plan`),
         `Cathedral Ceiling`= factor(`Cathedral Ceiling`),
         `Construction Quality` = factor(`Construction Quality`),
         `Site Desirability` = factor(`Site Desirability`),
         `Garage 1 Size` = factor(`Garage 1 Size`),
         `Garage 1 Material` = factor(`Garage 1 Material`),
         `Garage 1 Attachment` = factor(`Garage 1 Attachment`),
         `Garage 1 Area` = factor(`Garage 1 Area`),
         `Garage 2 Size` = factor(`Garage 2 Size`),
         `Garage 2 Material` = factor(`Garage 2 Material`),
         `Garage 2 Attachment` = factor(`Garage 2 Attachment`),
         `Garage 2 Area` = factor(`Garage 2 Area`),
         Porch = factor(Porch),
         `Other Improvements` = factor(`Other Improvements`),
         `Repair Condition` = factor(`Repair Condition`),
         `Multi Code` = factor(`Multi Code`),
         `Multi Property Indicator` = factor(`Multi Property Indicator`),
         `Modeling Group` = factor(`Modeling Group`),
         Use = factor(Use),
         `O'Hare Noise` = factor(`O'Hare Noise`),
         Floodplain = factor(Floodplain),
         `Road Proximity` = factor(`Road Proximity`),
         `Sale Year` = factor(`Sale Year`),
         `Sale Quarter` = factor(`Sale Quarter`),
         `Sale Half-Year` = factor(`Sale Half-Year`),
         `Sale Quarter of Year` = factor(`Sale Quarter of Year`),
         `Sale Month of Year` = factor(`Sale Month of Year`),
         `Sale Half of Year` = factor(`Sale Half of Year`),
         `Most Recent Sale` = factor(`Most Recent Sale`),
         `Pure Market Filter` = factor(`Pure Market Filter`),
         `Garage Indicator` = factor(`Garage Indicator`),
         `Town and Neighborhood` = factor(`Town and Neighborhood`)
         )


# Checking which columns will throw errors when trying to fit models
problems <- double(ncol(data))
for (i in 1:ncol(data)){
  print(i)
  print(unique(data[,i]))
  print(nrow(unique(data[,i])))
  if (nrow(unique(data[,i])) < 2){
    problems[i] <- i
  }
}

## From Code book the following variables are not useful for analysis
# Construction Quality
# Site Desirability
# Other Improvements
# Deed No. (I think we don't need to include this)
# Neighborhood Code (used for mapping)
# Modeling Group
# Use

## Removing problematic/useless columns

data <- data |> select(-c(`Construction Quality`,
                          `Site Desirability`,
                          `Other Improvements`,
                          `Deed No.`,
                          `Neigborhood Code (mapping)`,
                          `Modeling Group`,
                          Use))


write_csv(data, "data_cleaned.csv")

# Fitting Models

data <- read_csv("data_cleaned.csv")

### Lasso and Ridge Regression (Need to do a better job of subsetting variables)

# Fits are not very good at the moment, Might need to recode some variables TOO many variables are categorical (factors)
# so it is trying to fit models with like a shit ton of levels.

# Splitting data into training and testing sets
set.seed(123)
train = sample(1:dim(data)[1], 0.8*dim(data)[1])
test <- -train
data.train <- data[train,]
data.test <- data[test,]

# Fitting Lasso and Ridge Regression models

data.train.mat <- model.matrix(`Sale Price` ~. -Sell_Date , data = data.train)[,-1]
data.test.mat <- model.matrix(`Sale Price` ~. -Sell_Date, data = data.test)[,-1]

# Fitting Ridge Regression model

data.fit.ridge <- glmnet(data.train.mat, data.train$`Sale Price`, alpha = 0)
data.cv.ridge <- cv.glmnet(data.train.mat, data.train$`Sale Price`, alpha = 0) 
data.bestlam.ridge <- data.cv.ridge$lambda.min
data.bestlam.ridge

data.pred.ridge <- predict(data.fit.ridge, s = data.bestlam.ridge, newx = data.test.mat)
## Coefficients of Ridge Regression
predict(data.fit.ridge, s = data.bestlam.ridge, type = "coefficients")

## Calculate MSE
mean((data.pred.ridge - data.test$`Sale Price`)^2)

# Fitting Lasso Regression model

data.fit.lasso <- glmnet(data.train.mat, data.train$`Sale Price`, alpha = 1)
data.cv.lasso <- cv.glmnet(data.train.mat, data.train$`Sale Price`, alpha = 1)
data.bestlam.lasso <- data.cv.lasso$lambda.min
data.bestlam.lasso

data.pred.lasso <- predict(data.fit.lasso, s = data.bestlam.lasso, newx = data.test.mat)
## Coefficients of Lasso Regression
predict(data.fit.lasso, s = data.bestlam.lasso, type = "coefficients")

## Calculate MSE

mean((data.pred.lasso - data.test$`Sale Price`)^2)


