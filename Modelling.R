library(tidyverse)
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(glmnet)
library(lubridate)
library(mosaic)
library(ggthemes)
library(MuMIn)

conflicted::conflicts_prefer(dplyr::filter(),dplyr::lag(), base::sample(), base::mean(), dplyr::select())
 
data = readr::read_csv("data_cleaned.csv")

# Converting Columns into factors with levels
data <- data |> 
  mutate(Property_Class = factor(Property_Class),
         Neighborhood_Code = factor(Neighborhood_Code),
         Town_Code = factor(Town_Code),
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
        #  Use = factor(Use),
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
         Garage_Indicator = factor(Garage_Indicator),
         Fireplaces = factor(Fireplaces),
         Number_of_Commercial_Units = factor(Number_of_Commercial_Units),
         Area = factor(Area),
         Sub_Area = factor(Sub_Area),
         Block = factor(Block),
         Parcel = factor(Parcel),
         Multicode = factor(Multicode)
         # Town_and_Neighborhood = factor(Town_and_Neighborhood)
  )

# Subset of data for modelling variable
Model_Data = data %>%
    dplyr::select(-c(`...1`,
                        PIN,
                        Other_Improvements,
                        Multi_Code,
                        Deed_No,
                        Census_Tract,
                        Multi_Property_Indicator,
                        Sale_Half_Year,
                        Sale_Month_of_Year,
                        Sale_Half_of_Year,
                        Most_Recent_Sale,
                        Pure_Market_Filter,
                        Neighborhood_Code_Mapping,
                        Town_and_Neighborhood,
                        Sale_Quarter,
                        Age_Decade,
                        Neighborhood_Code,
                        Modeling_Group,
                        OHare_Noise,
                        Floodplain,
                        Road_Proximity,
                        Use,
                        Garage_1_Size,
                        Garage_1_Material,
                        Garage_1_Area,
                        Garage_2_Size,
                        Garage_2_Material,
                        Garage_2_Attachment,
                        Garage_2_Area,
                        Sell_Date,
                        Multicode,
                        Block,
                        Parcel,
                        Sub_Area,
                        Longitude,
                        Latitude,
                        Town_Code,
                        Area,
                        Attic_Finish,
                        Construction_Quality))

str(Model_Data)

Model_Data = Model_Data %>%
    mutate(Land_Square_Feet = scale(Land_Square_Feet),
            Building_Square_Feet = scale(Building_Square_Feet),
            Estimate_Land = scale(Estimate_Land),
            Estimate_Building = scale(Estimate_Building),
            Age = scale(Age),
            Lot_Size = scale(Lot_Size)
            )

# Simple Linear Regression -----------------------------
lm_slr = lm(Sale_Price ~ Age, data = Model_Data)
summary(lm_slr)
lm_slr_land_size <- lm(Sale_Price~ Land_Square_Feet, data = Model_Data)
summary(lm_slr_land_size)
lm_slr_build_size <- lm(Sale_Price~Land_Square_Feet, data = Model_Data)
summary(lm_slr_build_size)


# Multiple Linear Regression ---------------------------
lm_mlr = lm(`Sale Price` ~ Age + Baths * Bedrooms, data = Model_Data)
summary(lm_mlr)

# MLR with longitude and lattitude
lm_longlat<- lm(Sale_Price ~ Longitude + Latitude, data = Model_Data)
summary(lm_longlat)
lm_longlat$residuals


plot(data$Longitude~lm_longlat$residuals)
plot(data$Latitude~lm_longlat$residuals)

# Dredge for auto selection -----------------------------

# defining full model to be model will all variables
lm.full = lm(Sale_Price ~ ., data = Model_Data)

options(na.action = "na.fail")

# Searching all single parameter models and ranking on AIC
dredging = dredge(lm.full, rank = "AIC", m.lim = c(1,1))

# Show top models that are indistinguishable
subset(dredging, delta < 3)

# Select top model
top.model = get.models(dredging, subset = 1)[[1]]

summary(top.model)

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
