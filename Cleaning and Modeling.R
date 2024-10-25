# Loading packages
library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(glmnet)
library(lubridate)
library(mosaic)
library(ggthemes)
library(leaps)
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

data_nofactors <- data |> select(-c(Modeling_Group,Use,PIN, Other_Improvements))

# Making some of the variable factors

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



############################################################### Modeling ########################################################

# Splitting data into test and training sets 

data <- Model_Data

set.seed(42069)
train = sample(1:dim(data)[1], 0.8*dim(data)[1], replace = FALSE)
test <- -train
data.train <- data[train,]
data.test <- data[test,]

# Simple Linear Regression -----------------------------
lm_slr = lm(Sale_Price ~ Age, data = data)
summary(lm_slr)
lm_slr_land_size <- lm(Sale_Price~ Land_Square_Feet, data = data)
summary(lm_slr_land_size)
lm_slr_build_size <- lm(Sale_Price~Building_Square_Feet, data = data)
summary(lm_slr_build_size)


# Multiple Linear Regression ---------------------------
lm_mlr = lm(Sale_Price ~ Age + Baths * Bedrooms, data = data)
summary(lm_mlr)

# MLR with longitude and lattitude
lm_longlat<- lm(Sale_Price ~ Longitude + Latitude  + Longitude * Latitude, data = data)
summary(lm_longlat)

# Best subsets
# has to fit approx 2^62 models


lm_best_subset =  regsubsets(Sale_Price ~ . ,data = data)
summary(lm_best_subset)

# Stepwise Regression - Backward Selection and Hybrid Selection
# has to fit approx 62^2 different models

lm_step_back <- lm(Sale_Price ~. , data = data)
lm_step_back <- step(lm_step_back, direction = "back", trace = 0)
summary(lm_step_back)

lm_step_hybrid <- lm(Sale_Price ~., data = data)
lm_step_hybrid <- step(lm_step_hybrid, direction = "both", trace = 0)
summary(lm_step_hybrid)

##   OUTPUT HYBRID STEPWISE REGRESSION
#Call:
#lm(formula = Sale_Price ~ Property_Class + Neighborhood_Code + 
#     Land_Square_Feet + Town_Code + Apartments + Wall_Material + 
#     Roof_Material + Basement + Basement_Finish + Central_Heating + 
#     Other_Heating + Central_Air + Fireplaces + Attic_Type + Attic_Finish + 
#     Design_Plan + Cathedral_Ceiling + Construction_Quality + 
#     Site_Desirability + Garage_1_Size + Garage_1_Material + Garage_1_Attachment + 
#     Garage_1_Area + Garage_2_Size + Garage_2_Material + Garage_2_Attachment + 
 #    Garage_2_Area + Porch + Other_Improvements + Building_Square_Feet + 
#     Repair_Condition + Multi_Code + Number_of_Commercial_Units + 
#     Estimate_Land + Estimate_Building + Deed_No + Longitude + 
#     Latitude + Census_Tract + Multi_Property_Indicator + Age + 
#     OHare_Noise + Floodplain + Road_Proximity + Sale_Year + Sale_Quarter + 
#     Sale_Quarter_of_Year + Sale_Month_of_Year + Most_Recent_Sale + 
#     Pure_Market_Filter + Town_and_Neighborhood + Sell_Date + 
#     Rooms + Bedrooms + Baths, data = data)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-713253  -58037   -7095   48590 1038450 

#Coefficients: (5 not defined because of singularities)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                 8.106e+06  2.114e+07   0.383 0.701459    
#Property_Class203          -6.707e+03  7.995e+02  -8.389  < 2e-16 ***
#  Property_Class204          -1.188e+04  1.697e+03  -7.005 2.48e-12 ***
#  Property_Class205          -3.330e+03  1.211e+03  -2.749 0.005979 ** 
#  Property_Class206           1.638e+04  2.319e+03   7.063 1.63e-12 ***
#  Property_Class207          -1.653e+04  1.676e+03  -9.863  < 2e-16 ***
#  Property_Class208           1.303e+04  1.006e+04   1.295 0.195180    
#Property_Class209           5.782e+04  6.137e+04   0.942 0.346072    
#Property_Class278           6.567e+03  2.016e+03   3.257 0.001125 ** 
#  Neighborhood_Code          -6.875e+00  3.676e+00  -1.870 0.061496 .  
#Land_Square_Feet           -2.737e+00  8.059e-02 -33.969  < 2e-16 ***
#  Town_Code                  -2.778e+02  1.981e+01 -14.021  < 2e-16 ***
#  Apartments5                -1.301e+04  3.983e+04  -0.327 0.743907    
#Apartments6                 3.136e+04  7.482e+04   0.419 0.675114    
#Wall_Material2             -1.370e+04  6.691e+02 -20.479  < 2e-16 ***
#  Wall_Material3             -1.260e+04  7.554e+02 -16.678  < 2e-16 ***
#  Wall_Material4              1.584e+04  1.865e+03   8.494  < 2e-16 ***
#  Roof_Material2              2.682e+04  1.876e+03  14.295  < 2e-16 ***
#  Roof_Material3              8.065e+03  4.753e+03   1.697 0.089736 .  
#Roof_Material4              2.030e+04  4.147e+03   4.895 9.86e-07 ***
#  Roof_Material5              8.753e+03  3.014e+03   2.904 0.003681 ** 
#  Roof_Material6              4.926e+04  3.777e+03  13.043  < 2e-16 ***
#  Basement2                  -6.421e+03  8.406e+02  -7.639 2.20e-14 ***
#  Basement3                  -7.855e+03  7.855e+02 -10.000  < 2e-16 ***
#  Basement4                  -1.555e+04  1.047e+03 -14.856  < 2e-16 ***
#  Basement_Finish3           -8.328e+03  6.135e+02 -13.573  < 2e-16 ***
#  Central_Heating1           -7.483e+03  7.398e+03  -1.012 0.311773    
#Central_Heating2           -4.197e+03  7.420e+03  -0.566 0.571671    
#Other_Heating5              9.564e+02  1.324e+03   0.722 0.470174    
#Central_Air1                2.148e+03  6.107e+02   3.517 0.000437 ***
  #Fireplaces                  1.119e+04  6.296e+02  17.777  < 2e-16 ***
  #ttic_Type2                 1.153e+03  8.215e+02   1.403 0.160631    
#Attic_Type3                -1.261e+04  2.535e+04  -0.497 0.618884    
#Attic_Finish1              -1.604e+04  2.536e+04  -0.633 0.527053    
#Attic_Finish3              -1.002e+04  2.534e+04  -0.396 0.692405    
#Design_Plan1               -9.166e+03  1.921e+03  -4.770 1.84e-06 ***
#  Design_Plan2               -3.899e+03  6.832e+02  -5.708 1.15e-08 ***
#  Cathedral_Ceiling1          9.762e+03  1.724e+03   5.663 1.49e-08 ***
#  Cathedral_Ceiling2          1.506e+04  6.624e+02  22.729  < 2e-16 ***
# Construction_Quality2      -4.197e+02  3.470e+03  -0.121 0.903712    
#Construction_Quality3       5.617e+03  9.519e+03   0.590 0.555116    
#Site_Desirability2          1.699e+04  4.360e+03   3.896 9.78e-05 ***
#  Site_Desirability3         -1.023e+04  7.733e+03  -1.323 0.185997    
#Garage_1_Size1              2.444e+04  6.155e+04   0.397 0.691370    
#Garage_1_Size2              1.929e+04  6.155e+04   0.313 0.754050    
#Garage_1_Size3              2.073e+04  6.155e+04   0.337 0.736275    
#Garage_1_Size4              1.879e+04  6.156e+04   0.305 0.760150    
#Garage_1_Size5              2.572e+04  6.157e+04   0.418 0.676160    
#Garage_1_Size6              1.781e+04  6.167e+04   0.289 0.772752    
#Garage_1_Size7              3.257e+04  6.206e+04   0.525 0.599764    
#Garage_1_Material1          8.835e+03  1.219e+05   0.072 0.942213    
#Garage_1_Material2          1.273e+04  1.219e+05   0.104 0.916793    
##Garage_1_Material4          1.736e+03  1.220e+05   0.014 0.988650    
#Garage_1_Attachment1        2.567e+05  1.225e+05   2.096 0.036090 *  
#  Garage_1_Attachment2        2.683e+05  1.225e+05   2.190 0.028489 *  
#  Garage_1_Area1             -3.078e+05  1.611e+05  -1.910 0.056097 .  
#Garage_1_Area2             -3.013e+05  1.611e+05  -1.870 0.061468 .  
#Garage_2_Size2             -4.057e+04  4.235e+04  -0.958 0.338076    
#Garage_2_Size3             -3.238e+04  2.662e+04  -1.217 0.223768    
#Garage_2_Size4             -6.320e+04  8.439e+04  -0.749 0.453875    
#Garage_2_Size5             -9.229e+03  7.754e+04  -0.119 0.905262    
#Garage_2_Size7             -3.756e+04  4.151e+04  -0.905 0.365557    
#Garage_2_Material1         -1.264e+04  3.548e+04  -0.356 0.721518    
#Garage_2_Material2                 NA         NA      NA       NA    
#Garage_2_Attachment1        6.144e+03  3.339e+04   0.184 0.853997    
#Garage_2_Attachment2               NA         NA      NA       NA    
#Garage_2_Area1                     NA         NA      NA       NA    
#Garage_2_Area2                     NA         NA      NA       NA    
#Garage_2_Area4             -6.257e+03  7.450e+03  -0.840 0.400986    
#Porch2                      5.262e+02  2.326e+03   0.226 0.821040    
#Porch3                      1.297e+03  7.074e+02   1.833 0.066811 .  
#Other_Improvements1        -2.509e+04  3.040e+04  -0.825 0.409274    
#Other_Improvements2        -4.462e+04  2.483e+04  -1.797 0.072326 .  
#Other_Improvements3         2.688e+04  3.724e+04   0.722 0.470362    
#Other_Improvements4         7.642e+03  2.416e+04   0.316 0.751784    
#Other_Improvements5        -4.289e+04  3.175e+04  -1.351 0.176725    
#Other_Improvements6         5.021e+02  2.483e+04   0.020 0.983864    
#Other_Improvements7        -2.335e+04  2.066e+04  -1.130 0.258275    
#Other_Improvements8        -2.287e+04  4.299e+04  -0.532 0.594750    
#Other_Improvements9         3.482e+04  3.040e+04   1.145 0.252059    
#Other_Improvements10        3.402e+04  2.298e+04   1.480 0.138789    
#Other_Improvements11       -7.093e+02  3.333e+04  -0.021 0.983020    
#Other_Improvements12       -2.539e+04  2.719e+04  -0.934 0.350417    
#Other_Improvements13        2.320e+04  2.921e+04   0.794 0.427018    
#Other_Improvements14        8.800e+04  2.921e+04   3.012 0.002591 ** 
#  Other_Improvements15        4.931e+04  2.482e+04   1.987 0.046974 *  
#  Other_Improvements16        5.543e+03  3.042e+04   0.182 0.855404    
#Other_Improvements17       -1.154e+05  3.331e+04  -3.465 0.000530 ***
#  Other_Improvements18        1.843e+04  2.355e+04   0.783 0.433740    
#Other_Improvements19        3.675e+04  2.921e+04   1.258 0.208359    
#Other_Improvements20       -2.066e+04  3.040e+04  -0.680 0.496803    
#Other_Improvements21        4.177e+04  4.709e+04   0.887 0.375025    
#Other_Improvements22        1.165e+04  2.245e+04   0.519 0.603880    
#Other_Improvements23        1.561e+04  3.176e+04   0.491 0.623142    
#Other_Improvements24        6.070e+03  3.331e+04   0.182 0.855389    
#Other_Improvements25       -2.576e+04  2.106e+04  -1.223 0.221420    
#Other_Improvements26       -6.150e+04  3.515e+04  -1.750 0.080195 .  
#Other_Improvements27       -3.067e+04  3.041e+04  -1.009 0.313155    
#Other_Improvements28        3.970e+04  4.300e+04   0.923 0.355813    
#Other_Improvements29        4.253e+04  6.083e+04   0.699 0.484446    
#Other_Improvements30        1.911e+04  2.065e+04   0.925 0.354820    
#Other_Improvements31        2.492e+04  2.633e+04   0.946 0.343958    
#Other_Improvements32        1.067e+04  2.920e+04   0.365 0.714866    
#Other_Improvements33        9.541e+03  2.066e+04   0.462 0.644159    
#Other_Improvements34       -8.960e+04  5.265e+04  -1.702 0.088753 .  
#Other_Improvements35        5.931e+03  2.299e+04   0.258 0.796411    
#Other_Improvements36        4.913e+04  2.719e+04   1.807 0.070756 .  
#Other_Improvements37        1.075e+04  2.245e+04   0.479 0.632239    
#Other_Improvements38        2.534e+04  2.921e+04   0.868 0.385651    
#Other_Improvements39       -2.863e+04  4.710e+04  -0.608 0.543299    
#Other_Improvements40        3.428e+04  2.632e+04   1.302 0.192828    
#Other_Improvements41       -5.105e+03  1.505e+04  -0.339 0.734435    
#Other_Improvements42        2.317e+04  2.815e+04   0.823 0.410480    
#Other_Improvements43        1.610e+04  3.723e+04   0.432 0.665389    
#Other_Improvements44        1.531e+04  3.176e+04   0.482 0.629688    
#Other_Improvements45       -2.035e+04  2.150e+04  -0.946 0.343968    
#Other_Improvements46       -9.617e+03  1.956e+04  -0.492 0.622993    
#Other_Improvements47        1.687e+04  3.510e+04   0.481 0.630788    
#Other_Improvements48        3.085e+04  2.921e+04   1.056 0.290892    
#Other_Improvements49       -7.258e+03  2.196e+04  -0.331 0.740997    
#Other_Improvements50        9.309e+03  1.891e+04   0.492 0.622574    
#Other_Improvements51        2.609e+04  2.027e+04   1.287 0.197979    
#Other_Improvements52       -4.156e+04  1.834e+04  -2.266 0.023423 *  
#  Other_Improvements53        4.018e+04  3.330e+04   1.207 0.227566    
#Other_Improvements54        6.081e+03  2.554e+04   0.238 0.811833    
#Other_Improvements55       -7.997e+04  5.265e+04  -1.519 0.128743    
#Other_Improvements56       -2.835e+04  2.067e+04  -1.371 0.170295    
#Other_Improvements57       -1.198e+04  3.330e+04  -0.360 0.719122    
#Other_Improvements58        4.484e+04  2.921e+04   1.535 0.124693    
#Other_Improvements59       -2.702e+04  4.298e+04  -0.629 0.529585    
#Other_Improvements60        1.187e+04  2.355e+04   0.504 0.614245    
#Other_Improvements61       -4.930e+04  2.416e+04  -2.041 0.041292 *  
#  Other_Improvements62        1.099e+02  1.665e+04   0.007 0.994734    
#Other_Improvements63        3.966e+04  3.330e+04   1.191 0.233680    
#Other_Improvements64        1.760e+04  1.349e+04   1.305 0.191934    
#Other_Improvements65        5.708e+04  2.483e+04   2.299 0.021517 *  
#  Other_Improvements66       -6.285e+04  3.042e+04  -2.066 0.038794 *  
#  Other_Improvements67        4.097e+03  1.806e+04   0.227 0.820559    
#@Other_Improvements68       -4.304e+04  3.722e+04  -1.156 0.247571    
#Other_Improvements69       -2.758e+03  3.330e+04  -0.083 0.933990    
#Other_Improvements70        2.957e+04  2.356e+04   1.255 0.209555    
#Other_Improvements71        9.400e+03  3.040e+04   0.309 0.757128    
#Other_Improvements72        9.695e+03  3.510e+04   0.276 0.782368    
#Other_Improvements73        3.630e+03  4.709e+04   0.077 0.938557    
#Other_Improvements74        1.490e+04  3.511e+04   0.424 0.671329    
#Other_Improvements75       -3.232e+03  1.755e+04  -0.184 0.853916    
#Other_Improvements76       -1.805e+04  1.209e+04  -1.493 0.135445    
#Other_Improvements77       -7.667e+02  2.814e+04  -0.027 0.978264    
#Other_Improvements78       -8.908e+03  2.482e+04  -0.359 0.719680    
#Other_Improvements79       -4.607e+04  5.264e+04  -0.875 0.381467    
#Other_Improvements80        1.553e+04  3.040e+04   0.511 0.609354    
#Other_Improvements81        1.326e+04  3.040e+04   0.436 0.662571    
#Other_Improvements82       -4.635e+04  3.041e+04  -1.524 0.127417    
#Other_Improvements83       -3.454e+03  3.042e+04  -0.114 0.909602    
#Other_Improvements84        4.693e+04  2.033e+04   2.308 0.020990 *  
#  Other_Improvements85       -2.758e+04  3.980e+04  -0.693 0.488381    
#Other_Improvements86        9.508e+03  3.510e+04   0.271 0.786492    
#Other_Improvements87        3.438e+04  2.383e+04   1.442 0.149181    
#Other_Improvements88       -2.803e+04  3.980e+04  -0.704 0.481196    
#Other_Improvements89       -6.189e+03  2.298e+04  -0.269 0.787697    
#Other_Improvements90        4.233e+04  2.482e+04   1.705 0.088145 .  
#Other_Improvements91       -3.784e+04  3.980e+04  -0.951 0.341695    
#Other_Improvements92        7.534e+03  1.372e+04   0.549 0.582802    
#Other_Improvements93       -1.453e+04  1.833e+04  -0.792 0.428213    
#Other_Improvements94        2.058e+04  3.723e+04   0.553 0.580357    
#Other_Improvements95       -1.256e+04  3.330e+04  -0.377 0.705983    
#Other_Improvements96       -1.490e+04  1.606e+04  -0.927 0.353705    
#Other_Improvements97       -4.301e+04  3.330e+04  -1.292 0.196497    
#Other_Improvements98        9.521e+04  2.721e+04   3.500 0.000466 ***
#  Other_Improvements99        3.754e+03  2.355e+04   0.159 0.873357    
#Other_Improvements100      -1.120e+04  1.433e+04  -0.782 0.434432    
#Other_Improvements101       3.566e+03  4.710e+04   0.076 0.939646    
#Other_Improvements102       6.981e+03  2.150e+04   0.325 0.745377    
#Other_Improvements103      -1.770e+03  4.299e+04  -0.041 0.967159    
#Other_Improvements104      -1.619e+04  3.723e+04  -0.435 0.663667    
#Other_Improvements105       3.891e+04  2.027e+04   1.920 0.054871 .  
#Other_Improvements106      -7.198e+04  5.264e+04  -1.367 0.171531    
#Other_Improvements107       4.756e+02  1.688e+04   0.028 0.977526    
#Other_Improvements108       2.239e+04  3.040e+04   0.736 0.461523    
##Other_Improvements110      -7.546e+03  2.245e+04  -0.336 0.736826    
#Other_Improvements111       2.998e+04  4.298e+04   0.697 0.485508    
#Other_Improvements112      -1.693e+04  2.107e+04  -0.804 0.421476    
#Other_Improvements113       1.278e+04  3.723e+04   0.343 0.731436    
#Other_Improvements114       1.415e+05  7.445e+04   1.901 0.057290 .  
#Other_Improvements115      -1.532e+04  1.505e+04  -1.018 0.308555    
#Other_Improvements116      -6.146e+03  3.510e+04  -0.175 0.861005    
#Other_Improvements117      -3.463e+04  3.330e+04  -1.040 0.298370    
#Other_Improvements118      -2.384e+04  3.510e+04  -0.679 0.496986    
#Other_Improvements119      -1.086e+04  6.078e+04  -0.179 0.858189    
#Other_Improvements120       7.594e+03  1.521e+04   0.499 0.617505    
#Other_Improvements121      -1.100e+04  2.027e+04  -0.543 0.587263    
#Other_Improvements122       1.747e+04  1.755e+04   0.995 0.319531    
#Other_Improvements123      -7.389e+04  3.981e+04  -1.856 0.063435 .  
#Other_Improvements124      -1.731e+04  4.299e+04  -0.403 0.687200    
#Other_Improvements125       1.879e+03  2.298e+04   0.082 0.934834    
#Other_Improvements126      -7.633e+03  2.150e+04  -0.355 0.722540    
#Other_Improvements127      -3.385e+04  3.175e+04  -1.066 0.286421    
#Other_Improvements128      -1.500e+04  1.395e+04  -1.075 0.282487    
#[ reached getOption("max.print") -- omitted 757 rows ]
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 105300 on 191221 degrees of freedom
#Multiple R-squared:  0.7071,	Adjusted R-squared:  0.7056 
#F-statistic: 485.3 on 951 and 191221 DF,  p-value: < 2.2e-16

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

data.train.mat <- model.matrix(Sale_Price ~. , data = data.train)[,-1]
data.test.mat <- model.matrix(Sale_Price ~., data = data.test)[,-1]

# Fitting Ridge Regression model

data.fit.ridge <- glmnet(data.train.mat, data.train$Sale_Price, alpha = 0)
data.cv.ridge <- cv.glmnet(data.train.mat, data.train$Sale_Price, alpha = 0, nfolds = 5) 
data.bestlam.ridge <- data.cv.ridge$lambda.min
data.bestlam.ridge

data.pred.ridge <- predict(data.fit.ridge, s = data.bestlam.ridge, newx = data.test.mat)
## Coefficients of Ridge Regression
predict(data.fit.ridge, s = data.bestlam.ridge, type = "coefficients")

## Calculate MSE
mean((data.pred.ridge - data.test$Sale_Price)^2)

# Fitting Lasso Regression model

data.fit.lasso <- glmnet(data.train.mat, data.train$Sale_Price, alpha = 1)
data.cv.lasso <- cv.glmnet(data.train.mat, data.train$Sale_Price, alpha = 1, nfolds = 5)
data.bestlam.lasso <- data.cv.lasso$lambda.min
data.bestlam.lasso

data.pred.lasso <- predict(data.fit.lasso, s = data.bestlam.lasso, newx = data.test.mat)
## Coefficients of Lasso Regression
predict(data.fit.lasso, s = data.bestlam.lasso, type = "coefficients")

## Calculate MSE

mean((data.pred.lasso - data.test$Sale_Price)^2)


# Fitting models using caret
library(caret)
library(glmnet)

train_control <- trainControl(method = "cv", 
                              number = 5)

data.train.mat <- model.matrix(Sale_Price ~. , data = data.train)[,-1]
data.test.mat <- model.matrix(Sale_Price ~., data = data.test)[,-1]


# fitting ridge regression model 
tune.grid.ridge = expand.grid(alpha = 0,
                           lambda = glmnet(data.train.mat,
                                    data.train$Sale_Price, 
                                    alpha = 0)$lambda)
fit.ridge <- train(Sale_Price ~ ., data = data.train,
  method = 'glmnet',
  trControl = train_control,
  tuneGrid = tune.grid.ridge)

print(fit.ridge)

# fitting the model on the test data
ridge_pred <- predict(fit.ridge, data.test)
print(paste("MSE from Test Set: ", Metrics::mse(ridge_pred, data.test$Sale_Price)))
# model coefficients
coef(fit.ridge$finalModel, s = fit.ridge$bestTune$lambda)



# fitting lasso regression model
tune.grid.lasso = expand.grid(alpha = 1,
                           lambda = glmnet(data.train.mat,
                                    data.train$Sale_Price,
                                    alpha = 1)$lambda)
fit.lasso <- train(Sale_Price ~., data = data.train,
  method = 'glmnet',
  trControl = train_control,
  tuneGrid = tune.grid.lasso)

print(fit.lasso)
# Fitting the model on our test data
lasso_pred <- predict(fit.lasso, data.test)
print(paste("MSE from Test Set: ", Metrics::mse(lasso_pred, data.test$Sale_Price)))
# model coefficients
coef(fit.lasso$finalModel, s = fit.lasso$bestTune$lambda)









## Comparisons with caret and glmnet for fitting the two models.

data.train.mat <- model.matrix(Sale_Price ~. , data = data.train)[,-1]
data.test.mat <- model.matrix(Sale_Price ~., data = data.test)[,-1]


train_control <- trainControl(method = "cv", 
                              number = 5)

grid = 10 ^ seq(5, -2, length = 100)

#### Ridge - Caret

# fitting ridge regression model 
tune.grid.ridge = expand.grid(lambda = grid, alpha = 0)
fit.ridge <- train(Sale_Price ~ ., data = data.train,
  method = 'glmnet',
  trControl = train_control,
  tuneGrid = data.frame(alpha = 0))

fit.ridge$bestTune

fit.ridge <- train(Sale_Price ~ ., data = data.train,
  method = 'glmnet',
  trControl = train_control,
  tuneGrid = expand.grid(lambda = fit.ridge$finalModel$lambdaOpt, alpha = 0))

print(fit.ridge)

ridge_pred <- predict(fit.ridge, data.test)
print(paste("MSE from Test Set: ", Metrics::mse(ridge_pred, data.test$Sale_Price)))

#### Ridge - glmnet

# Fitting Ridge Regression model

data.fit.ridge <- glmnet(data.train.mat, data.train$Sale_Price, alpha = 0)
data.cv.ridge <- cv.glmnet(data.train.mat, data.train$Sale_Price, alpha = 0, nfolds = 5) 
data.bestlam.ridge <- data.cv.ridge$lambda.min
data.bestlam.ridge

data.pred.ridge <- predict(data.fit.ridge, s = data.bestlam.ridge, newx = data.test.mat)
## Coefficients of Ridge Regression
predict(data.fit.ridge, s = data.bestlam.ridge, type = "coefficients")

## Calculate MSE
mean((data.pred.ridge - data.test$Sale_Price)^2)
#### Comparison with best lambda choice and mse

## Caret
fit.ridge$bestTune$lambda
print(paste("MSE from Test Set: ", Metrics::mse(ridge_pred, data.test$Sale_Price)))

## glmnet
data.bestlam.ridge
mean((data.pred.ridge - data.test$Sale_Price)^2)


### direct comparison
fit.ridge$bestTune$lambda == data.bestlam.ridge

Metrics::mse(ridge_pred, data.test$Sale_Price) == mean((data.pred.ridge - data.test$Sale_Price)^2)




## Lasso

#### Lasso caret
tune.grid.lasso = expand.grid(lambda = grid, alpha = 1)
fit.lasso <- train(Sale_Price ~., data = data.train,
  method = 'glmnet',
  trControl = train_control,
  tuneGrid = tune.grid.lasso)

fit.lasso <-train(Sale_Price ~ ., data = data.train,
    method = 'glmnet',
    trControl = train_control,
    tuneGrid = expand.grid(lambda = fit.lasso$finalModel$lambda, alpha = 1))

print(fit.lasso)

lasso_pred <- predict(fit.lasso, data.test)
print(paste("MSE from Test Set: ", Metrics::mse(lasso_pred, data.test$Sale_Price)))

#### Lasso glmnet

data.fit.lasso <- glmnet(data.train.mat, data.train$Sale_Price, alpha = 1)
data.cv.lasso <- cv.glmnet(data.train.mat, data.train$Sale_Price, alpha = 1, nfolds = 5)
data.bestlam.lasso <- data.cv.lasso$lambda.min
data.bestlam.lasso

data.pred.lasso <- predict(data.fit.lasso, s = data.bestlam.lasso, newx = data.test.mat)
## Coefficients of Lasso Regression
predict(data.fit.lasso, s = data.bestlam.lasso, type = "coefficients")

## Calculate MSE

mean((data.pred.lasso - data.test$Sale_Price)^2)

### Comparison of best lambda choice and mse

##### Caret

fit.lasso$bestTune$lambda
print(paste("MSE from Test Set: ", Metrics::mse(lasso_pred, data.test$Sale_Price)))

##### glmnet
data.bestlam.lasso
mean((data.pred.lasso - data.test$Sale_Price)^2)

###### Direct comparison

fit.lasso$bestTune$lambda == data.bestlam.lasso
mean((data.pred.lasso - data.test$Sale_Price)^2) == Metrics::mse(lasso_pred, data.test$Sale_Price)
