library(dplyr)
library(tidyverse)
library(ggplot)

df <- read.csv("data/raw/accidents.csv")
nrow(df)
df$date <- as.Date(df$date, format = "%Y-%m-%d")

df <- df %>% 
  complete(date = seq(min(date), max(date), by = "day"))

nrow(df)

# df %>%
#   filter(is.na(drunk_drivers))

write.csv(df, "data/clean/accidents.csv")



