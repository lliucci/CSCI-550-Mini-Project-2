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
        Garage_1_Size = `Garage 1 Size`,
        Garage_1_Material = `Garage 1 Material`,
        Garage_1_Attachment = `Garage 1 Attachment`,
        Garage_1_Area = `Garage 1 Area`,
        Garage_2_Size = `Garage 2 Size`,
        Garage_2_Material = `Garage 2 Material`,
        Garage_2_Attachment = `Garage 2 Attachment`,
        Garage_2_Area = `Garage 2 Area`,
        Building_Square_Feet = `Building Square Feet`,
        Multi_Code = `Multi Code`,
        Number_of_Commercial_Units = `Number of Commercial Units`,
        Estimate_Land = `Estimate (Land)`,
        Estimate_Building = `Estimate (Building)`,
        Sale_Price = `Sale Price`,
        Multi_Property_Indicator = `Multi Property Indicator`,
        OHare_Noise = `O'Hare Noise`,
        Road_Proximity = `Road Proximity`,
        Sale_Year = `Sale Year`,
        Sale_Quarter_of_Year = `Sale Quarter of Year`,
        Sale_Month_of_Year = `Sale Month of Year`,
        Most_Recent_Sale = `Most Recent Sale`,
        Pure_Market_Filter = `Pure Market Filter`,
        Garage_Indicator = `Garage Indicator`,
        Lot_Size = `Lot Size`,
        Construction_Quality = `Construction Quality`,
        Site_Desirability = `Site Desirability`,
        Other_Improvements = `Other Improvements`,
        Repair_Condition = `Repair Condition`,
        Deed_No = `Deed No.`,
        Census_Tract = `Census Tract`,
        Modeling_Group = `Modeling Group`,
        Sale_Half_of_Year = `Sale Half of Year`,
        Age_Decade = `Age Decade`,
        Neighborhood_Code_Mapping = `Neigborhood Code (mapping)`,
        Town_and_Neighborhood = `Town and Neighborhood`,
        )

# Save cleaned data
write_csv(data, "data_cleaned.csv")
