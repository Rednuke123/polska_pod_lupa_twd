df <- load.csv("data/clean/accidents.csv")


table <- df %>%
  mutate(
    day_of_the_year = format(date, "%m-%d"),
    year = format(date, "%Y")
  ) %>%
  filter(format(date, "%m-%d") >= "12-22", year >= 2022)


ggplot(table, aes(
  x = day_of_the_year,
  y = drunk_drivers,
  group = year,
  color = year,
  fill = year
)) +
  geom_area(alpha = 0.2, position = "identity") +
  geom_line(linewidth = 1) +
  theme_minimal()