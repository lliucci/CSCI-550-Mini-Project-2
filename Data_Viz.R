library(tidyverse)

data = read_csv("data_cleaned.csv")

data = data %>%
    mutate(Baths = factor(Baths),
            Rooms = factor(Rooms),
            Bedrooms = factor(Bedrooms),
            `Sale Year` = factor(`Sale Year`))

data %>%
    ggplot(aes(x = Age, y = `Sale Price`, color = `Rooms`)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "lm", se = F) +
    facet_wrap(~`Rooms`)

data %>%
    ggplot() +
    geom_boxplot(aes(x = Baths, y = `Sale Price`))
