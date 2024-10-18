# Loading packages
library(tidyverse)
library(lubridate)
library(mosaic)

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

write_csv(data, "data_cleaned.csv")
