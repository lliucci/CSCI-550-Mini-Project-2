library(tidyverse)
library(ggmap)
library(sf)
theme_set(theme_bw())

data = read_csv("data_cleaned.csv")

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
            `Garage 1 Size`)

# Sale_Price against Age and Number of Rooms
data %>%
    ggplot(aes(x = Age, y = `Sale_Price`, color = `Rooms`)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "lm", se = F) +
    facet_wrap(~`Rooms`)

# Sale_Price against Number of Bathrooms
SP_vs_Bathrooms = data %>%
    ggplot() +
    geom_boxplot(aes(x = Baths, y = `Sale_Price`))

ggsave(SP_vs_Bathrooms,
    filename = "Figures/SP_vs_Bathrooms.png",
    width = 1200,
    height = 800,
    units = 'px',
    scale = 2)

# Sale_Price against Lot Size
SP_vs_LS = data %>%
    ggplot(aes(x = `Lot_Size`, y = `Sale_Price`)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = 'lm', se = F)

ggsave(SP_vs_LS,
    filename = "Figures/SP_vs_Lot_Size.png",
    width = 1200,
    height = 800,
    units = 'px',
    scale = 2)

# Sale_Price against Lot Size and Building Sq. Footage
SP_vs_Lot_Building_Size = data %>%
    ggplot(aes(x = `Building_Square_Feet`, 
                y = `Land_Square_Feet`, 
                color = `Sale_Price`)) +
    geom_point(size = 2)

ggsave(SP_vs_Lot_Building_Size,
    filename = "Figures/SP_vs_Lot_and_Building_Size.png",
    width = 1200,
    height = 800,
    units = 'px',
    scale = 2)

# Spatial Map

register_google(key = "AIzaSyCnq3okTIRjxfQ0wHWGyu08HGCrFtIQo4M", write = TRUE)

Map <- get_googlemap(center = c(long = -87.8,lat = 41.8), maptype = "satellite", zoom = 9)

cook_map2 <- read_sf('Congressional_District.geojson')
cook_map_overlay = st_transform(cook_map2)

Sat_Map = ggmap(Map, darken = c(0.1, "white")) +
    geom_point(data = data, 
                aes(x = Longitude, 
                    y = Latitude, 
                    color = Sale_Price), 
                size = 0.02, 
                alpha = 0.75) +
    scale_color_gradient(low = "#6fe7f7", high = "#890000") +
    labs(x = "Longitude", y = "Latitude", color = "Sale Price ($)")

ggsave(Sat_Map,
    filename = "Figures/Sat_Map.png",
    height = 1200,
    width = 1200,
    units = 'px',
    scale = 2)

# Plotting Against Garage Variables
Garage_Vars = data %>%
    select(Sale_Price, 
           Garage_1_Area, 
           Garage_1_Size, 
           Garage_1_Attachment, 
           Garage_1_Material, 
           Garage_2_Area, 
           Garage_2_Size, 
           Garage_2_Attachment, 
           Garage_2_Material) %>%
    pivot_longer(cols = 2:9, names_to = "Variable", values_to = "Value") %>%
    ggplot(aes(x = factor(Value), y = Sale_Price, group = Value)) +
    geom_boxplot() +
    facet_wrap(~Variable, scales = "free_x") +
    labs(x = " ")

ggsave(Garage_Vars,
    filename = "Figures/SP_vs_Garage_Vars.png",
    height = 800,
    width = 1200,
    units = 'px',
    scale = 2)

# Plotting Against Property Variables
Property_Vars = data %>%
    select(`Sale_Price`, 
            `Property_Class`,
            `Apartments`,
            `Basement`,
            `Attic_Type`,
            `Design_Plan`,
            `Cathedral_Ceiling`,
            Fireplaces) %>%
    pivot_longer(cols = 2:8, names_to = "Variable", values_to = "Value") %>%
    ggplot(aes(x = factor(Value), y = Sale_Price, group = Value)) +
    geom_boxplot() +
    facet_wrap(~Variable, scales = "free_x") +
    labs(x = " ")

ggsave(Property_Vars,
    filename = "Figures/SP_vs_Property_Vars.png",
    height = 800,
    width = 1200,
    units = 'px',
    scale = 2)

# Plotting Against Room Variables
Room_Vars = data %>%
    select(`Sale_Price`, 
            `Bedrooms`,
            `Baths`,
            `Rooms`) %>%
    pivot_longer(cols = 2:4, names_to = "Variable", values_to = "Value") %>%
    ggplot(aes(x = factor(Value), y = Sale_Price, group = Value)) +
    geom_boxplot() +
    facet_wrap(~Variable, scales = "free_x") +
    labs(x = " ")

ggsave(Room_Vars,
    filename = "Figures/SP_vs_Room_Vars.png",
    height = 800,
    width = 1200,
    units = 'px',
    scale = 2)

# Plotting Against Lot Variables
Room_Vars = data %>%
    select(`Sale_Price`, 
            Land_Square_Feet,
            Building_Square_Feet,
            ) %>%
    pivot_longer(cols = 2:4, names_to = "Variable", values_to = "Value") %>%
    ggplot(aes(x = factor(Value), y = Sale_Price, group = Value)) +
    geom_boxplot() +
    facet_wrap(~Variable, scales = "free_x") +
    labs(x = " ")

ggsave(Room_Vars,
    filename = "Figures/SP_vs_Room_Vars.png",
    height = 800,
    width = 1200,
    units = 'px',
    scale = 2)