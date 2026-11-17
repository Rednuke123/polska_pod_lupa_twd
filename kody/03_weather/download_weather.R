# Pobieranie danych IMGW (synop, dobowe) –  Wigilia (24.12) i Nowy Rok (1.01), sezony 1996–2025
library(dplyr)

baza    <- "https://danepubliczne.imgw.pl/data/dane_pomiarowo_obserwacyjne/dane_meteorologiczne/dobowe/synop/"
foldery <- c("1996_2000", 2001:2026)

wynik <- list()

for (f in foldery) {
  url_folderu <- paste0(baza, f, "/")
  strona <- tryCatch(readLines(url_folderu, warn = FALSE), error = function(e) NULL)
  if (is.null(strona)) { message("Brak folderu: ", f); next }
  zipy   <- unique(regmatches(strona, regexpr("[^\"/>]+\\.zip", strona)))
  
  for (z in zipy) {
    tmp_zip <- tempfile(fileext = ".zip")
    tmp_dir <- tempfile()
    download.file(paste0(url_folderu, z), tmp_zip, mode = "wb", quiet = TRUE)
    unzip(tmp_zip, exdir = tmp_dir)
    
    # tylko pliki s_d_ (bez s_d_t_)
    pliki <- list.files(tmp_dir, pattern = "^s_d_[0-9].*\\.csv$", full.names = TRUE)
    
    for (p in pliki) {
      dane <- read.csv(p, header = FALSE, fileEncoding = "CP1250")
      wynik[[length(wynik) + 1]] <- dane %>%
        filter((V4 == 12 & V5 == 25) | (V4 == 1 & V5 == 1))%>%
        select(kod = V1, stacja = V2, rok = V3,  miesiac = V4, temp = V10,
               snieg_cm = V17, snieg_status = V18) %>%
        # ujednolicamy typy kolumn (puste kolumny R czyta jako logiczne)
        mutate(across(c(kod, rok, temp, snieg_cm, snieg_status), as.numeric),
               stacja = as.character(stacja)) %>%
        # status 9 = zjawisko nie wystąpiło -> brak śniegu = 0 cm
        mutate(snieg_cm = ifelse(is.na(snieg_cm) & snieg_status == 9, 0, snieg_cm))
    }
    unlink(c(tmp_zip, tmp_dir), recursive = TRUE)
  }
  message("Gotowe: ", f)
}

swieta <- bind_rows(wynik) %>%      
  distinct() %>%
  mutate(dzien = ifelse(miesiac == 12, "Wigilia", "Nowy Rok"),
         sezon = ifelse(miesiac == 12, rok, rok - 1)) %>%
  filter(sezon >= 1996, sezon <= 2025)

write.csv(swieta, "data/raw/swieta_snieg.csv", row.names = FALSE, fileEncoding = "UTF-8")