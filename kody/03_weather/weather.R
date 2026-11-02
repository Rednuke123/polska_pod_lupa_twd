library(dplyr)

library(ggplot2)

wigilia <- read.csv("data/raw/wigilia_snieg.csv", fileEncoding = "UTF-8")



gory <- c(349190650,   # Kasprowy Wierch
          350150510,   # Śnieżka
          349200628)   # Hala Gąsienicowa

wigilia <- wigilia %>%
  filter(!kod %in% gory,          # bez stacji wysokogórskich
         !is.na(snieg_cm))        # bez braków pomiaru (status 8)


wigilia <- wigilia %>%
  mutate(biala = snieg_cm >= 1)

wigilia <- wigilia %>%
  select(-snieg_status)

lata <- wigilia %>%
  group_by(rok) %>%
  summarise(stacje = n(),                         # ile stacji miało pomiar
            biale  = sum(biala),                  # ile z nich miało śnieg
            proc   = round(biale / stacje * 100), # % białych
            .groups = "drop")

lata <- lata %>%
  arrange(desc(proc))

write.csv(lata, "data/clean/wigilia_lata.csv", row.names = FALSE)


# --- paleta plakatu ---
BG     <- "#EFE6D8"
KARTA  <- "#F7F1E8"
BORDO  <- "#7A1E2B"
BEZ    <- "#E9DCCB"   # jasny koniec skali (zielona Wigilia)
TEKST  <- "#2B2523"
TEKST2 <- "#574F48"   # ciemniejszy – lepiej czytelny na wydruku
SIATKA <- "#E3D9CA"

# --- dane ---
lata <- read.csv("data/clean/wigilia_lata.csv") %>%
  arrange(rok)

# średnie dla dekad
dekady <- lata %>%
  mutate(dekada = case_when(rok <= 2005 ~ "1996–2005",
                            rok <= 2015 ~ "2006–2015",
                            TRUE        ~ "2016–2025")) %>%
  group_by(dekada) %>%
  summarise(od      = min(rok) - 0.45,
            do      = max(rok) + 0.45,
            srednia = mean(proc),
            .groups = "drop")

# --- wykres ---
ggplot(lata, aes(x = rok, y = proc)) +
  geom_col(aes(fill = proc), width = 0.8) +
  geom_segment(data = dekady,
               aes(x = od, xend = do, y = srednia, yend = srednia),
               colour = TEKST, linewidth = 0.8, linetype = "dashed",
               inherit.aes = FALSE) +
  geom_label(data = dekady,
             aes(x = (od + do) / 2, y = srednia,
                 label = paste0("średnio ", round(srednia), "%")),
             fill = KARTA, colour = TEKST, size = 4.5, label.size = 0,
             vjust = -0.3, inherit.aes = FALSE) +
  geom_text(data = filter(lata, proc == 0),
            aes(label = "0%"), vjust = -0.5, size = 3.8, colour = TEKST2) +
  scale_x_continuous(breaks = 1996:2025) +
  scale_fill_gradient(low = BEZ, high = BORDO, limits = c(0, 100), guide = "none") +
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 25),
                     labels = function(x) paste0(x, "%")) +
  labs(title    = "Czy Wigilia jest jeszcze biała?",
       subtitle = "Odsetek stacji IMGW z pokrywą śnieżną (min. 1 cm) rano 24 grudnia, 1996–2025.\nPrzerywana linia – średnia dla dekady.",
       x = "Rok",
       y = "Stacje ze śniegiem [%]",
       caption = "Źródło: IMGW-PIB, dane synoptyczne dobowe. Bez stacji wysokogórskich (Kasprowy Wierch, Śnieżka, Hala Gąsienicowa).") +
  theme_minimal(base_size = 14) +
  theme(
    plot.background    = element_rect(fill = BG, colour = NA),
    panel.background   = element_rect(fill = KARTA, colour = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = SIATKA),
    plot.title    = element_text(colour = BORDO, face = "bold", size = 22),
    plot.subtitle = element_text(colour = TEKST2),
    plot.caption  = element_text(colour = TEKST2, size = 10, hjust = 0),
    axis.text     = element_text(colour = TEKST2),
    axis.title    = element_text(colour = TEKST2),
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)
    )

ggsave("~/Desktop/wigilia_lata.png", width = 12, height = 6, dpi = 300, bg = BG)

