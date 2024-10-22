# Loading packages
library(tidyverse)
library(lubridate)
library(mosaic)

## Reading Data
data = read_csv("cook_county_train_val.csv")


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


## Other modifications

# Removing rows with at least 1 missing value
data = data %>%
    drop_na()

# Filtering extreme observations
is_outlier = function(x){
    result = abs(x - mean(x)) > 3*sd(x)
    return(result)
}

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


# Removing columns that are not needed
# Label "NA" meaning codebook said not to include it
data <- data %>% 
  select(-c(
    `...1`, # Just the row number
    PIN, # Identifier for row, not necessary for regression
    `Construction Quality`, # NA
    `Site Desirability`, # NA
    `Other Improvements`, # NA
    `Repair Condition`, ## NA maybe, Idk if we should keep it
    `Deed No.`, # NA
    Longitude, # NA
    Latitude, # NA
    `Census Tract`, ## NA, maybe. Could be used for plotting/clustering
    `Modeling Group`, # NA
    Use, # NA
    `Sale Quarter`, # This variable doesn't make sense to me. `Sale Quarter of Year` makes more sense to keep
    `Sale Half-Year`, # This variable doesn't make sense to me. `Sale Half of Year` makes more sense to keep
    `Sale Half of Year`, # Variable information captured in `Sale Quarter of Year`
    `Neigborhood Code (mapping)`, ## NA, maybe. Could be used for plotting/clustering
    `Town and Neighborhood`, ## NA unless we treat it as a random variable, it has 800 levels
    `Age Decade` # This variable is better captured in `Age`
  ))

# Mutating variables to make sure they are of the right type
# Also scaling occurs in here
data <- data %>% 
  mutate(
    # `Repair Condition` = as.factor(`Repair Condition`),
    # `Census Tract` = as.factor(`Census Tract`),
    # `Neighborhood Code (mapping)` = as.factor(`Neigborhood Code (mapping)`),
    # `Town and Neighborhood` = as.factor(`Town and Neighborhood`),
    `Property Class` = as.factor(`Property Class`),
    `Land Square Feet` = as.numeric(`Land Square Feet`),
    `Town Code` = as.factor(`Town Code`),
    Apartments = as.numeric(Apartments),
    `Wall Material` = as.factor(`Wall Material`),
    `Roof Material` = as.factor(`Roof Material`),
    Basement = as.factor(Basement),
    `Basement Finish` = as.factor(`Basement Finish`),
    `Central Heating` = as.factor(`Central Heating`),
    `Other Heating` = as.factor(`Other Heating`),
    `Central Air` = as.factor(`Central Air`),
    Fireplaces = as.factor(Fireplaces),
    `Attic Type` = as.factor(`Attic Type`),
    `Attic Finish` = as.factor(`Attic Finish`),
    `Design Plan` = as.factor(`Design Plan`),
    `Cathedral Ceiling` = as.factor(`Cathedral Ceiling`),
    `Garage 1 Area` = as.factor(`Garage 1 Area`),
    `Garage 1 Size` = as.factor(`Garage 1 Size`),
    `Garage 1 Material` = as.factor(`Garage 1 Material`),
    `Garage 1 Attachment` = as.factor(`Garage 1 Attachment`), 
    `Garage 2 Area` = as.factor(`Garage 2 Area`),
    `Garage 2 Size` = as.factor(`Garage 2 Size`),
    `Garage 2 Material` = as.factor(`Garage 2 Material`),
    `Garage 2 Attachment` = as.factor(`Garage 2 Attachment`),
    Porch = as.factor(Porch),
    `Building Square Feet` = as.numeric(`Building Square Feet`),
    `Multi Code` = as.factor(`Multi Code`),
    `Number of Commercial Units` = as.numeric(`Number of Commercial Units`),
    `Estimate (Land)` = as.numeric(`Estimate (Land)`),
    `Estimate (Building)` = as.numeric(`Estimate (Building)`),
    `Sale Price` = as.numeric(`Sale Price`),
    # Add a scale for Sale Price here?
    `Multi Property Indicator` = as.factor(`Multi Property Indicator`),
    Age = as.numeric(Age),
    `O'Hare Noise` = as.factor(`O'Hare Noise`),
    Floodplain = as.factor(Floodplain),
    `Road Proximity` = as.factor(`Road Proximity`),
    `Sale Year` = as.numeric(`Sale Year`),
    `Sale Year` = `Sale Year` - 2013, # Scaling Sale Year to start at 0
    `Sale Quarter of Year` = as.factor(`Sale Quarter of Year`),
    `Sale Month of Year` = as.factor(`Sale Month of Year`),
    `Most Recent Sale` = as.factor(`Most Recent Sale`),
    `Pure Market Filter` = as.factor(`Pure Market Filter`),
    `Garage Indicator` = as.factor(`Garage Indicator`),
    `Neigborhood Code` = as.factor(`Neighborhood Code`),
    `Lot Size` = as.numeric(`Lot Size`),
  )

# Print out the summary statistics for each variable
#apply(data, 2, favstats)

write_csv(data, "data_cleaned.csv")
