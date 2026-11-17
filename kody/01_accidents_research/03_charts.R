df <- load.csv("data/clean/accidents.csv")


df <- df %>%
  mutate(
    day = format(date, "%d"),
    day_and_month = format(date, "%m-%d"),
    year = format(date, "%Y")
  )

table <- df %>%
  filter(day_and_month >= "12-22", year >= 2022)

avg <- df %>%
  group_by(year) %>%
  summarise(mean_wanted_arrested = mean(wanted_arrested, na.rm = TRUE)) %>%
  filter(year >= 2022)

year_labels <- setNames(
  paste0(avg$year, " (śr. ", round(avg$mean_wanted_arrested), ")"),
  avg$year
)

ggplot(table, aes(
  x = day,
  y = wanted_arrested,
  group = year,
  color = year,
  fill = year
)) +
  geom_area(alpha = 0.2, position = "identity") +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(title="Zatrzymani w Wigilię") + 
  xlab("Dzień w grudniu") + 
  ylab("Ile zatrzymanych") +
  scale_color_discrete(labels = year_labels) +
  guides(fill = "none")
