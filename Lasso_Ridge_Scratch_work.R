## Mini Project 2 Scratch Work

library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(glmnet)
library(lubridate)
library(mosaic)
library(ggthemes)

conflicted::conflicts_prefer(dplyr::filter(),dplyr::lag(), base::sample(), base::mean())


# Doing Extra Cleaning if needed

data <- read_csv("data_cleaned.csv")


# data no factors, removing predictors that will not have any predictive power/will not work with model.matrix

data_nofactors <- data |> select(-c(Construction_Quality,
                          Site_Desirability,
                          Other_Improvements,
                          `Deed No.`,
                          `Neigborhood Code (mapping)`,
                          Modeling_Group,
                          Use,
                          PIN))

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
         `Construction Quality` = factor(`Construction Quality`),
         `Site Desirability` = factor(`Site Desirability`),
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
                          Use,
                          PIN))


# If we think the data has been cleaned enough

write_csv(data, "data_cleaned.csv")

# Fitting Models

data <- read_csv("data_cleaned.csv")

### Lasso and Ridge Regression (Need to do a better job of subsetting variables)

# Fits are not very good at the moment, Might need to recode some variables TOO many variables are categorical (factors)
# so it is trying to fit models with like a shit ton of levels.

# Splitting data into training and testing sets
set.seed(42069)
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

# fitting models on data without any predictors recoded as factors

# Splitting data into training and testing sets
set.seed(42069)
train = sample(1:dim(data_nofactors)[1], 0.8*dim(data_nofactors)[1])
test <- -train
data.train <- data_nofactors[train,]
data.test <- data_nofactors[test,]

# Fitting Lasso and Ridge Regression models

data.train.mat <- model.matrix(Sale_Price ~. -Sell_Date , data = data.train)[,-1]
data.test.mat <- model.matrix(Sale_Price ~. -Sell_Date, data = data.test)[,-1]

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
data.cv.lasso <- cv.glmnet(data.train.mat, data.train$`Sale Price`, alpha = 1, nfolds = 5)
data.bestlam.lasso <- data.cv.lasso$lambda.min
data.bestlam.lasso

data.pred.lasso <- predict(data.fit.lasso, s = data.bestlam.lasso, newx = data.test.mat)
## Coefficients of Lasso Regression
predict(data.fit.lasso, s = data.bestlam.lasso, type = "coefficients")

## Calculate MSE

mean((data.pred.lasso - data.test$Sale_Price)^2)


## Putting Stuff on the map
library(sf)
library(maps)
library(ggthemes)
library(ggdark)

cook_map <- map_data('county', 'illinois') |> filter(subregion == 'cook')
map1 <- ggplot(cook_map, aes(long, lat)) +
  geom_polygon(fill = "white", colour = "black") + 
  geom_point(data = data, aes(x = Longitude, y = Latitude, color = factor(Property_Class)), size = 0.025)+
  coord_quickmap()
map2<- ggplot(cook_map, aes(long, lat)) +
  geom_polygon(fill = "white", colour = "black") + 
  geom_point(data = data, aes(x = Longitude, y = Latitude, color = `Sale Price`), size = 0.025)+
  scale_color_gradient(low = "#69f04e", high = "#ff0845")+
  coord_quickmap()+
  theme_solid()

cook_map2 <- read_sf('Congressional_District.geojson')
map3 <- ggplot(cook_map2) +
  geom_sf()+
  geom_point(data = data, aes(x = Longitude, y = Latitude, color = `Sale Price`), size = 0.025, alpha = 0.25)+
  scale_color_gradient(low = "#6fe7f7", high = "#890000")+
  coord_sf()+
  theme_solid()

map3

## Trying Something Crazy here (This is really cool stuff)
library(leaflet)
library(leaflet.providers)
cook_overlay <- st_transform(cook_map2)

County_Overlay <- leaflet() |>
  addTiles() |>
  #addProviderTiles(providers$Esri.WorldImagery) |>
  setView(lng = mean(data$Longitude), lat = mean(data$Latitude), zoom = 10) |>
  addPolygons(data = cook_overlay, color = "blue", stroke = 1, opacity = 0.25) #|>
  addMarkers(lng = data$Longitude, lat = data$Latitude, group = 'Properties')


County_Overlay


addMarkers(map = County_Overlay, lng = data$Longitude[1:100], lat = data$Latitude[1:100], group = 'Properties')
addMarkers(map = County_Overlay, lng = data$Longitude[1:(length(data$Longitude)/10)], lat = data$Latitude[1:(length(data$Latitude)/10)])

addCircleMarkers(map = County_Overlay,lng = data$Longitude[1:(length(data$Longitude)/10)], lat = data$Latitude[1:(length(data$Latitude)/10)], radius = 0.1, group = 'Properties')

Sat_Map_Sale_Price_Cook_Overlay <- ggmap(Map, darken = c(0.1, "white")) +
    geom_polygon(data = cook_map, aes(x = long, y = lat), fill = NA, color = "orange")+
    #geom_sf(data = cook_map2, inherit.aes = FALSE)+
    geom_point(data = data, 
                aes(x = Longitude, 
                    y = Latitude, 
                    color = Sale_Price), 
                size = 0.02, 
                alpha = 0.75) +
    scale_color_gradient(low = "#6fe7f7", high = "#890000") +
    labs(x = "Longitude", y = "Latitude", color = "Sale Price ($)")


Sat_Map_Sale_Price_Cook_Overlay


csf <- coord_sf()
csf$default <- TRUE
