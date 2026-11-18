
### wczytanie danych

library(dplyr)
library(tidyr)
library(ggplot2)

KARTA <- "#F5EEE0"

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
  group_by(Activity.types, Months) %>%
  summarise(mediana = median(Value) - 100,
            min = min(Value) - 100,
            max = max(Value) - 100,
            .groups = "drop")
wynik <- wynik %>%
  mutate(Months = factor(Months, levels = month.name))

wynik <- wynik %>%
  mutate(Activity.types = recode(Activity.types,
                                 "food, beverages and tobacco products"                = "Żywność i napoje",
                                 "newspapers, books, other sale in specialized stores" = "Książki, prasa i prezenty",
                                 "furniture, radio, TV and household appliances"       = "Meble, RTV, AGD",
                                 "solid, liquid and gaseous fuels"                     = "Paliwa"))
wynik <- wynik %>%
  mutate(wyroznienie = ifelse(Months == "December", "Grudzień", "Pozostałe miesiące"))

wynik <- wynik %>%
  mutate(Activity.types = factor(Activity.types,
                                 levels = c("Książki, prasa i prezenty", "Żywność i napoje", "Meble, RTV, AGD", "Paliwa")))

p <- ggplot(wynik, aes(x = Months, y = mediana, fill = wyroznienie)) +
  geom_col(width = 0.7) +
  facet_wrap(~ Activity.types, nrow = 1) +
  geom_linerange(aes(ymin = min, ymax = max), colour = "#5C4A3D", linewidth = 0.4)+
  geom_hline(yintercept = 0, colour = "grey30") +
  scale_fill_manual(values = c("Grudzień" = "#7A1E2B", "Pozostałe miesiące" = "#C5D0C0"),
                    breaks = c("Grudzień", "Pozostałe miesiące"),
                    name = NULL) +
  scale_x_discrete(labels = c("I","II","III","IV","V","VI",
                                "VII","VIII","IX","X","XI","XII", "")) +
  geom_vline(data = data.frame(Activity.types = c("Książki, prasa i prezenty",
                                                  "Meble, RTV, AGD",
                                                  "Paliwa")),
             aes(xintercept = 13),
             colour = "#B3A48D", linewidth = 0.4) +
  labs(title    = "Na co wydajemy więcej przed świętami?",
         subtitle = "Zmiana sprzedaży względem poprzedniego miesiąca (słupek – mediana). Kreska – od wartości minimalnej do maksymalnej w okresie 2021–2025.",
         x = "Miesiąc",
         y = "Zmiana sprzedaży, %",
         )+
    #caption = "Źródło: GUS, Bank Danych Lokalnych"      
  theme_minimal()+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.background  = element_rect(fill = KARTA, colour = NA),
    panel.background = element_rect(fill = KARTA, colour = NA),
    plot.subtitle = element_text(colour = "#A8323E"),
    legend.position = "top",
    legend.justification = "left",
    panel.grid.major.y = element_line(colour = "#E6DDCB"),  
    axis.text  = element_text(colour = "#5C4A3D"),          
    axis.title = element_text(colour = "#5C4A3D"),          
    strip.text = element_text(colour = "#2E1F17"),
    plot.title = element_text(family = "Helvetica", face = "bold",
                              size = 34.2, colour = "#A8323E",
                              hjust = 0, lineheight = 0.9,
                              margin = margin(b = 6, unit = "pt")),
    plot.title.position = "plot",
    
    )
  
ggsave("~/Desktop/wykres_zakupy.png", plot = p,
       width = 44, height = 22, units = "cm", dpi = 300, bg = "transparent")

podglad <- retail %>%
  pivot_wider(names_from = Year, values_from = Value)
