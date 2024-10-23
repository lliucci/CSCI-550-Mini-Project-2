library(tidyverse)
theme_set(theme_bw())

data = read_csv("data_cleaned.csv")

data = data %>%
    mutate(Baths = factor(Baths),
            Rooms = factor(Rooms),
            Bedrooms = factor(Bedrooms),
            `Sale Year` = factor(`Sale Year`),
            `Property Class` = factor(`Property Class`),
            Apartments = factor(Apartments),
            Basement = factor(Basement),
            `Attic Type` = factor(`Attic Type`),
            `Design Plan` = factor(`Design Plan`),
            `Cathedral Ceiling` = factor(`Cathedral Ceiling`),
            `Garage 1 Size`)

# Sale Price against Age and Number of Rooms
data %>%
    ggplot(aes(x = Age, y = `Sale Price`, color = `Rooms`)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "lm", se = F) +
    facet_wrap(~`Rooms`)

# Sale Price against Number of Bathrooms
SP_vs_Bathrooms = data %>%
    ggplot() +
    geom_boxplot(aes(x = Baths, y = `Sale Price`))

ggsave(SP_vs_Bathrooms,
    filename = "Figures/SP_vs_Bathrooms.png",
    width = 1200,
    height = 800,
    units = 'px',
    scale = 2)

# Sale Price against Lot Size
SP_vs_LS = data %>%
    ggplot(aes(x = `Lot Size`, y = `Sale Price`)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = 'lm', se = F)

ggsave(SP_vs_LS,
    filename = "Figures/SP_vs_Lot_Size.png",
    width = 1200,
    height = 800,
    units = 'px',
    scale = 2)

# Sale Price against Lot Size and Building Sq. Footage
SP_vs_Lot_Building_Size = data %>%
    ggplot(aes(x = `Building Square Feet`, 
                y = `Land Square Feet`, 
                color = `Sale Price`)) +
    geom_point(size = 2)

ggsave(SP_vs_Lot_Building_Size,
    filename = "Figures/SP_vs_Lot_and_Building_Size.png",
    width = 1200,
    height = 800,
    units = 'px',
    scale = 2)

# Sale Price against Design Variables
Design_Var_GGPairs = data %>%
    dplyr::select(`Sale Price`, 
                `Property Class`,
                `Apartments`,
                `Basement`,
                `Attic Type`,
                `Design Plan`,
                `Cathedral Ceiling`) %>%
    GGally::ggpairs()

ggsave(Design_Var_GGPairs,
    filename = "Figures/Design_Variable_GGPairs.png",
    width = 1200,
    height = 1200,
    units = 'px',
    scale = 2)

# Sale Price against Garage Variables
Garage_GGPairs = data %>%
    dplyr::select(`Sale Price`,
                `Garage 1 Size`,
                `Garage 1 Material`,
                `Garage 1 Attachment`,
                `Garage 1 Area`,
                `Garage 2 Size`,
                `Garage 2 Material`,
                `Garage 2 Attachment`,
                `Garage 2 Area`) %>%
    GGally::ggpairs()

ggsave(Garage_GGPairs,
    filename = "Figures/Garage_GGPairs.png",
    width = 1200,
    height = 1200,
    units = 'px',
    scale = 2)


data %>%
    select(`Sale Price`, )