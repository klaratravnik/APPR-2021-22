# 2. faza: Uvoz podatkov

library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(tidyverse)

sl <- locale("sl", decimal_mark=",", grouping_mark=".")


#####################1. tabela##################################################

#file.choose("zemljisca_regije")

zemljisca <- read_csv("regije.csv", skip = 2,
                      locale = locale(encoding = "Windows-1250"),
                      col_names=TRUE, col_types = cols(
                        .default = col_guess(),
                        "2003 Površina [ha]" = col_double(), 
                        "2005 Površina [ha]" = col_double(), 
                        "2007 Površina [ha]" = col_double(), 
                        "2010 Površina [ha]" = col_double(), 
                        "2013 Površina [ha]" = col_double(), 
                        "2016 Površina [ha]" = col_double()))
                        
                        
                   
zemljisca <- pivot_longer(zemljisca,
                                cols = colnames(zemljisca)[-c(1,2)],
                                names_to = "leto", 
                                values_to = "povrsina" )


colnames(zemljisca)[1] <- 'regija'
colnames(zemljisca)[2] <- 'vrsta.zemljisca'

  
    
zemljisca <- zemljisca %>%
  tidyr::extract(
    col = leto,
    into = c("stevilka", "crke"),
    regex = "^(\\d{4})\\s+(.*)$"
  ) 

zemljisca <- zemljisca %>% select(regija, vrsta.zemljisca, stevilka, povrsina)

colnames(zemljisca)[3] <- 'leto'

 
imena.regij = tibble(
  regija = c(
    "Gorenjska",
    "Goriška",
    "Jugovzhodna Slovenija",
    "Koroška",
    "Obalno-kraška",
    "Osrednjeslovenska",
    "Podravska",
    "Pomurska",
    "Posavska",
    "Primorsko-notranjska",
    "Savinjska",
    "Zasavska",
    "SLOVENIJA"
  ),
  oznaka = c(
    "kr", "ng", "nm", "sg", "kp", "lj", "mb", "ms", "kk", "po", "ce", "za", "slo"
  )
)



  

zemljisca[2] <- zemljisca$vrsta.zemljisca %>% str_replace_all("\\.+", '')


#preimenujem grda imena

zemljisca[2] <- zemljisca$vrsta.zemljisca %>% str_replace_all("[:alpha:]{5} [:alpha:]{9} - [:alpha:]{6}", "skupaj") %>% str_replace_all("[:alpha:]{9} [:alpha:]{9}", "kmetijska.zemljisca") 

#left join za regije
zemljisca <- merge(x = zemljisca, y = imena.regij, by = "regija", all.x = TRUE) %>% select(regija = oznaka, leto, vrsta.zemljisca, povrsina)

vrste_zemljisc = tibble(
  vrsta.zemljisca = c(
    "Zelena krma z njiv",
    "Žita",
    "Gozd",
    "Trajni nasadi",
    "Trajni travniki in pašniki",
    "kmetijska.zemljisca",
    "skupaj",
    "Njive",
    "Nerodovitno"
  ),
  lepse = c(
    "zelena.krma",
    "zita",
    "gozd",
    "trajni.nasadi",
    "travniki.in.pasniki",
    "kmetijska.zemljisca",
    "skupaj",
    "njive",
    "nerodovitno"
  )
)


#left join za vrste zemljisc
zemljisca <- merge(x = zemljisca, y = vrste_zemljisc, by = "vrsta.zemljisca", all.x = TRUE) %>% select(regija, leto, vrsta.zemljisca = lepse, povrsina)

#izbrisem vrstice z NA v povrsini
zemljisca <- zemljisca %>% dplyr::filter(!is.na(povrsina))




file.choose("povprecni_pridelek_poregijah")

pridelek <- read_csv("povprecni_pridelek_poregijah.csv", skip = 2,
                      locale = locale(encoding = "Windows-1250"),
                      col_names=TRUE, col_types = cols(
                        .default = col_double(),
                        "KMETIJSKE KULTURE" = col_character(), 
                        ))

pridelek <- pivot_longer(pridelek,
                          cols = colnames(pridelek)[-c(1)],
                          names_to = "leto", 
                          values_to = "povprecni.pridelek" )

colnames(pridelek)[1] <- 'kmetijske.kulture'

pridelek <- pridelek %>%
  tidyr::extract(
    col = leto,
    into = c("stevilka", "crke"),
    regex = "^(\\d{4})\\s+(.*)$"
  ) 

pridelek <- pridelek %>% select(regija = crke, leto = stevilka, kmetijske.kulture, povprecni.pridelek)

#left join za regije
pridelek <- merge(x = pridelek, y = imena.regij, by = "regija", all.x = TRUE) %>% select(regija = oznaka, leto, kmetijske.kulture, povprecni.pridelek)

