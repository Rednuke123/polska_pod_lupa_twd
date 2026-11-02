
### wczytanie danych

library(dplyr)
library(tidyr)
library(ggplot2)

retail <- read.csv("data/raw/retail_raw.csv",
                   sep = ";",
                   fileEncoding = "UTF-8")


kategorie <- c("furniture, radio, TV and household appliances", 
               "food, beverages and tobacco products",
               "newspapers, books, other sale in specialized stores", 
               "solid, liquid and gaseous fuels")


# usuwamy 2020 i 2026 rok, wybieramy potrzebne kolumny, wybieramy 4 kategorii dla 4 podwykresow,
# usuwamy schowane dane
retail <- retail %>% filter(Year %in% c(2021:2025)) %>% 
      select(Name, Months, Activity.types, Year, Value) %>%
      filter(Activity.types %in% kategorie) %>%
      filter(Value != 0)

cala_polska <-retail %>% 
  group_by(Activity.types, Year, Months) %>%
  summarise(Value = median(Value), .groups = "drop" )
  
wynik <- cala_polska %>%
  group_by(Ativity.types, Months) %>%
  summarise(mediana = median(Value) - 100,
            min = min(Value) - 100,
            max = max(Value) - 100,
            .groups = "drop" )
wynik <- wynik %>%
  mutate(Months = factor(Months, levels = month.name))

wynik <- wynik %>%
  mutate(Activity.types = recode(Activity.types,
                                 "food, beverages and tobacco products"                = "Żywność i napoje",
                                 "newspapers, books, other sale in specialized stores" = "Prezenty (książki, zabawki)",
                                 "furniture, radio, TV and household appliances"       = "Meble, RTV, AGD",
                                 "solid, liquid and gaseous fuels"                     = "Paliwa"))


ggplot(wynik, aes(x = Months, y = mediana, fill = Months == "December")) +
  geom_col(width = 0.7) +
  facet_wrap(~ Activity.types, nrow = 1) +
  geom_linerange(aes(ymin = min, ymax = max), colour = "grey40", linewidth = 0.4)+
  geom_hline(yintercept = 0, colour = "grey30") +
  scale_fill_manual(values = c("FALSE" = "#C5D0C0", "TRUE" = "#7A1E2B"),
                 guide = "none")+
  scale_x_discrete(labels = c("I","II","III","IV","V","VI",
                                "VII","VIII","IX","X","XI","XII")) +
  labs(title    = "Na co Polacy wydają w grudniu?",
         subtitle = "Zmiana sprzedaży względem poprzedniego miesiąca (mediana 2021–2025). Kreska – rozrzut między latami.",
         x = "Miesiąc",
         y = "Zmiana sprzedaży, %",
         caption = "Źródło: GUS, Bankw Danych Lokalnych")+
          
  theme_minimal()
  



podglad <- retail %>%
  pivot_wider(names_from = Year, values_from = Value)
