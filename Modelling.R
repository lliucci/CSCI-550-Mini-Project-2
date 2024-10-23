library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(glmnet)
library(lubridate)
library(mosaic)
library(ggthemes)

conflicted::conflicts_prefer(dplyr::filter(),dplyr::lag(), base::sample(), base::mean())
 
data = readr::read_csv("data_cleaned.csv")

data = data %>%
    mutate(Baths = factor(Baths),
            Rooms = factor(Rooms),
            Bedrooms = factor(Bedrooms),
            Sale_Year = factor(`Sale_Year`),
            `Property_Class` = factor(`Property_Class`),
            Apartments = factor(Apartments),
            Basement = factor(Basement),
            `Attic_Type` = factor(`Attic_Type`),
            `Design_Plan` = factor(`Design_Plan`),
            `Cathedral_Ceiling` = factor(`Cathedral_Ceiling`),
            `Garage 1 Size`) |>
    select(-c(Use, `Modeling Group`))



# Simple Linear Regression -----------------------------
lm_slr = lm(`Sale Price` ~ Age, data = data)
summary(lm_slr)


# Multiple Linear Regression ---------------------------
lm_mlr = lm(`Sale Price` ~ Age + Baths * Bedrooms, data = data)
summary(lm_mlr)

# Stepwise Regression - Backward Selection and Hybrid Selection

lm_step_back <- lm(Sale_Price ~. , data = data)
lm_step_back <- step(lm_step_back, direction = "back", trace = 0)
summary(lm_step_back)

lm_step_hybrid <- lm(Sale_Price ~., data = data)
lm_step_hybrid <- step(lm_step_back, direction = "both", trace = 0)
summary(lm_step_hybrid)

# PCA --------------------------------------------------
numeric_data = data %>% select_if(is.numeric)
PCs = prcomp(numeric_data)

PCs$sdev^2/sum(PCs$sdev^2)

# Non-Linear Models ------------------------------------
lm_nonlinear = lm(`Sale Price` ~ poly(Age, 3), data = data)
summary(lm_nonlinear)

# Comparing Models -------------------------------------
AIC(lm_slr, lm_mlr, lm_nonlinear)

# Cross-Validation -------------------------------------

# define number of folds
k = 5

# select k-1/k ids for training
train_ids = sample(1:nrow(data), size = round((nrow(data)*(k-1))/k,0), replace = FALSE)

# make training data
train_data = data[train_ids,]
test_data = data[-train_ids,]

nrow(test_data) + nrow(train_data) == nrow(data)

lm_cv = lm(`Sale Price` ~ Age, data = train_data)
lm_cv_preds = predict(lm_cv, newdata = test_data)

Metrics::mse(predicted = cv_preds, actual = test_data$`Sale Price`)
