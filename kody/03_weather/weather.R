library(dplyr)
library(tidyr)
library(ggplot2)

swieta <- read.csv("data/raw/swieta_snieg.csv", fileEncoding = "UTF-8")

# --- paleta---
KARTA   <- "#F5EEE0"   # tło wykresu
CZERWON <- "#A8323E"   # tytuł i podtytuł
BORDO   <- "#7A1E2B"   # Wigilia (ten sam kolor co grudzień w wydatkach)
LOW     <- "#A8BFA0"   # Nowy Rok
CYNAMON <- "#5C4A3D"   # osie, kreski, linie dekad
ESPRESSO<- "#2E1F17"   # przerywana linia, podpisy średnich
SIATKA  <- "#E6DDCB"

gory <- c(349190650,   # Kasprowy Wierch
          350150510,   # Śnieżka
          349200628)   # Hala Gąsienicowa

swieta <- swieta %>%
  filter(!kod %in% gory,          # bez stacji wysokogórskich
         !is.na(snieg_cm))        # bez braków pomiaru (status 8)


swieta <- swieta %>%
  mutate(biala = snieg_cm >= 1)


sezony <- swieta %>%
  group_by(sezon, dzien) %>%
  summarise(stacje = n(),                         
            biale  = sum(biala),                  
            proc   = round(biale / stacje * 100),
            .groups = "drop")

write.csv(sezony, "data/clean/swieta_sezony.csv", row.names = FALSE)


gantele <- sezony %>%
  select(sezon, dzien, proc) %>%
  pivot_wider(names_from = dzien, values_from = proc)


# --- dane ---
lata <- read.csv("data/clean/wigilia_lata.csv") %>%
  arrange(rok)

# średnie dla dekad
dekady <- sezony %>%
  mutate(dekada = case_when(sezon <= 2005 ~ "1996–2005",
                            sezon <= 2015 ~ "2006–2015",
                            TRUE        ~ "2016–2025")) %>%
  group_by(dekada) %>%
  summarise(od      = min(sezon) - 0.45,
            do      = max(sezon) + 0.45,
            srednia = mean(proc),
            .groups = "drop")

# --- wykres ---
ggplot() +
  geom_vline(xintercept = c(2005.5, 2015.5),
             colour = CYNAMON, linewidth = 0.5) +
  geom_segment(data = dekady,
                aes(x = od, xend = do, y = srednia, yend = srednia),
                colour = ESPRESSO, linewidth = 0.8, linetype = "dashed") +
  geom_segment(data = gantele,
               aes(x = sezon, xend = sezon, y = Wigilia, yend = `Nowy Rok`),
               colour = CYNAMON, linewidth = 0.6)+
  geom_point(data = sezony,
             aes(x = sezon, y = proc, colour = dzien), size = 3.5)+
  scale_colour_manual(values = c("Wigilia" = BORDO, "Nowy Rok" = LOW),
                      breaks = c("Wigilia", "Nowy Rok"),
                      labels = c("Wigilia (24.12)", "Nowy Rok (01.01)"),
                      name = NULL)+
  scale_x_continuous(breaks = 1996:2025, 
                     labels = sprintf("%02d", 1996:2025 %% 100),
                     expand = expansion(add = 0.2))+
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 10),
                     labels = function(x) paste0(x, "%"))+
  geom_label(data = dekady,
             aes(x = (od + do) / 2, y = 100,
                 label = paste0("średnio ", round(srednia), "%")),
             fill = KARTA, colour = ESPRESSO, size = 4.5, label.size = 0, vjust = -0.3)+
  labs(title    = "Czy święta są jeszcze białe?",
       subtitle = "Odsetek stacji IMGW z pokrywą śnieżną (min. 1 cm) rano w Wigilię i w Nowy Rok, sezony 1996/97–2025/26.\nKreska łączy oba dni tych samych świąt. Przerywana linia – średnia dla 10 lat.",
       x = "Rok (Wigilia danego roku i Nowy Rok następnego)",
       y = "Stacje ze śniegiem, %",
       caption = "Bez stacji wysokogórskich (Kasprowy Wierch, Śnieżka, Hala Gąsienicowa).\nOd 2023 r. dane na 1 stycznia z ok. 44 stacji (wcześniej ok. 55).") +
  #"Źródło: IMGW-PIB, dane synoptyczne dobowe. 
  theme_minimal()+
  theme(
   plot.background  = element_rect(fill = KARTA, colour = NA),
    panel.background = element_rect(fill = KARTA, colour = NA),
    plot.title    = element_text(colour = CZERWON, face = "bold", size = 20),
    plot.subtitle = element_text(colour = CZERWON),
    plot.caption  = element_text(colour = CYNAMON, hjust = 0),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(colour = CYNAMON),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = SIATKA),
    axis.text  = element_text(colour = CYNAMON),
    axis.title = element_text(colour = CYNAMON),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
 
  )
  
  
  
ggsave("~/Desktop/pogoda.png", width = 12, height = 6, dpi = 300, bg = BG)  
  
  


