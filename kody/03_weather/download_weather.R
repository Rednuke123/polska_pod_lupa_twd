# Pobieranie danych IMGW (synop, dobowe) – tylko 24 grudnia, lata 1996–2025
library(dplyr)

baza    <- "https://danepubliczne.imgw.pl/data/dane_pomiarowo_obserwacyjne/dane_meteorologiczne/dobowe/synop/"
foldery <- c("1996_2000", 2001:2025)

wynik <- list()

for (f in foldery) {
  url_folderu <- paste0(baza, f, "/")
  strona <- readLines(url_folderu, warn = FALSE)
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
        filter(V4 == 12, V5 == 24) %>%
        select(kod = V1, stacja = V2, rok = V3, temp = V10,
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

wigilia <- bind_rows(wynik) %>%
  filter(rok >= 1996, rok <= 2025) %>%
  distinct()

write.csv(wigilia, "data/raw/wigilia_snieg.csv", row.names = FALSE, fileEncoding = "UTF-8")