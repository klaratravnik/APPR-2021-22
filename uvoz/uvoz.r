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


#left join za regije
zemljisca <- merge(x = zemljisca, y = imena.regij, by = "regija", all.x = TRUE) %>% select(regija = oznaka, leto, vrsta.zemljisca, povrsina)

vektor_zemljisc <- unique(zemljisca$vrsta.zemljisca) %>% sort()


vrste_zemljisc = tibble(
  vrsta.zemljisca = vektor_zemljisc,
  lepse = c(
    "kmetijska.zemljisca",
    "skupaj",
    "travniki.in.pasniki",
    "gozd",
    "nerodovitno",
    "zita",
    "njive",
    "trajni.nasadi",
    "zelena.krma"
  )
)


#left join za vrste zemljisc
zemljisca <- merge(x = zemljisca, y = vrste_zemljisc, by = "vrsta.zemljisca", all.x = TRUE) %>% select(regija, leto, vrsta.zemljisca = lepse, povrsina)

#izbrisem vrstice z NA v povrsini
zemljisca <- zemljisca %>% dplyr::filter(!is.na(povrsina))


############################################################

#file.choose("povprecni_pridelek_poregijah")

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

#izbrišem oklepaje
pridelek$kmetijske.kulture <- pridelek$kmetijske.kulture %>% str_replace_all("\\(([^)]+)\\)", '')  


vektor_kultur <- unique(pridelek$kmetijske.kulture) %>% sort()
                   
vrste_kultur <- data.frame(
  kmetijske.kulture = vektor_kultur,
  lepse = c(
    "trave",
    "trave",
    "travniki.in.pasniki",
    "psenica.in.pira",
    "koruza",
    "oljna.ogrscica.in.repica",
    "belo.zelje",
    "breskve.in.nektarine",
    "buce",
    "detelja",
    "grozdje",
    "hmelj",
    "jabolka",
    "jecmen",
    "koruza",
    "krompir",
    "lucerna",
    "oljke",
    "oves",
    "trave",
    "tritikala", "rz.in.sorzica"
  )
)

pridelek <- pridelek %>% left_join(vrste_kultur, by = c("kmetijske.kulture"))
 

pridelek <- pridelek %>% select(regija, leto, kmetijske.kulture = lepse, povprecni.pridelek)
pridelek <- pridelek %>% filter(!is.na(povprecni.pridelek))


# joinat morem se tabelo zemljisca pa pridelek by = c("regija", "leto")

test <- zemljisca %>% left_join(pridelek, by = c("regija", "leto"))
#nevem ce bi zdruzevala

####################2. tabela##################################################

zivina <- read_csv("zivina_.csv", skip = 2,
                     locale = locale(encoding = "Windows-1250"),
                     col_names=TRUE, col_types = cols(
                       .default = col_character()
                         ))

zivina <- pivot_longer(zivina,
                         cols = colnames(zivina)[-c(1)],
                         names_to = "leto", 
                         values_to = "stevilo.zivine" )

colnames(zivina)[1] <- 'vrsta.zivine'
zivina <- zivina %>% filter(vrsta.zivine != "Kmetijska gospodarstva - SKUPAJ") %>% select(vrsta.zivine, leto, stevilo.zivine)

vektor_zivine <- unique(zivina$vrsta.zivine) %>% sort()

vrsta_zivine <- data.frame(
  vrsta.zivine = vektor_zivine,
  lepse = c("skupaj", "prasici", "govedo", "jelenjad", "konji", "koze", "kunci", "ovce", "perutnina", "cebelje.druzine")
)

zivina <- merge(x = zivina, y = vrsta_zivine, by = "vrsta.zivine", all.x = TRUE) %>% select(vrsta.zivine = lepse, leto, stevilo.zivine)

#shranim preurejeno tabelo

zivina %>% write_csv("zivina.csv")


#################3. tabela #####################################################

#potrosnja je v kg
potrosnja <- read_csv("potrosnja_kmetijskihpridelkov.csv", skip = 2,
                   locale = locale(encoding = "Windows-1250"),
                   col_names=TRUE, col_types = cols(
                     .default = col_character()
                   ))

colnames(potrosnja)[1] <- 'leto'

potrosnja <- pivot_longer(potrosnja,
                       cols = colnames(potrosnja)[-c(1)],
                       names_to = "pridelek", 
                       values_to = "potrosnja" )
ime_pridelka <- tibble(
  pridelek = c("Žita", "Meso", "Jajca", "Krompir", "Zelenjava", "Med", "Riž"),
  lepse = c("zita", "meso", "jajca", "krompir", "zelenjava", "med", "riz")
)

potrosnja <- merge(x = potrosnja, y = ime_pridelka, by = "pridelek", all.x = TRUE) %>% select(leto, pridelek = lepse, potrosnja)


#####################################################
#prodaja v kg 
prodaja <- read_csv("prodaja_kmetijskihpridelkov.csv", skip = 2,
                    locale = locale(encoding = "Windows-1250"),
                    col_names=TRUE, col_types = cols(
                      .default = col_character()
                    ))

prodaja <- pivot_longer(prodaja,
                          cols = colnames(prodaja)[-c(1)],
                          names_to = "leto", 
                          values_to = "prodaja" )
colnames(prodaja)[1] <- "pridelek"

#locim leta

prodaja <- prodaja %>%
  tidyr::extract(
    col = leto,
    into = c("stevilka", "crke"),
    regex = "^(\\d{4})\\s+(.*)$")

prodaja <- prodaja %>% select(leto = stevilka, pridelek, prodaja)

#znebim se oklepajev pri pridelkih

prodaja$pridelek <- prodaja$pridelek %>% str_replace_all("\\(([^)]+)\\)", '')

#pri količinah dam NA namesto -
prodaja$prodaja <- prodaja$prodaja %>% str_replace_all("\\-", NA_character_)

vektor_pridelek <- unique(prodaja$pridelek) %>% sort()

prodaja_pridelek <- tibble(
  pridelek = vektor_pridelek,
  lepse = c("mleko", "zelenjava", "meso", "hruske", "gobe", "visnje", "lesniki", 
            "fige", "moka", "borovnice", "zelenjava", "slive.in.cesplje", "cesnje", "fizol", 
            "zelenjava", "breskve.in.nektarine", "zelenjava", "lesniki", "zelenjava", "zelenjava", "orehi", "zelenjava",
            "zelenjava", "zelenjava", "zelenjava", "grozdje", "jagode", "jajca", 
            "zelenjava", "kajmak", "kaki", "zelenjava", "zelenjava", "zelenjava", "zelenjava",
            "moka", "kostanj", "krompir", "zelenjava", "zelenjava",
            "lubenice", "maline", "marelice", "med", "zelenjava", "jabolka", "zelenjava", "olje",
            "orehi", "zelenjava", "zelenjava", "meso", "zelenjava", "zelenjava",
            "zelenjava", "sir", "smetana", "fige", "slive", "maslo", "zelenjava", "zelenjava")
)

prodaja <- prodaja %>% left_join(prodaja_pridelek, by = c("pridelek")) 


prodaja <- prodaja %>% select(leto, pridelek = lepse, prodaja)
prodaja <- prodaja %>% filter(!is.na(prodaja))



#zdruzit moram se tabeli potrosnja in prodaja po letu in zivilih



##################################4. tabela#######################################################

#poraba na clana gospodinjstva v kg
doma.porabljeni <- read_csv("kolicina_doma_porabljenih_zivil.csv", skip = 2,
                      locale = locale(encoding = "Windows-1250"),
                      col_names=TRUE, col_types = cols(
                        .default = col_guess()
                       
                      ))

colnames(doma.porabljeni)[1] <- 'zivila'

doma.porabljeni <- pivot_longer(doma.porabljeni,
                        cols = colnames(doma.porabljeni)[-c(1)],
                        names_to = "leto", 
                        values_to = "poraba")

#locim leta

doma.porabljeni <- doma.porabljeni %>%
  tidyr::extract(
    col = leto,
    into = c("stevilka", "crke"),
    regex = "^(\\d{4})\\s+(.*)$")

doma.porabljeni <- doma.porabljeni %>% select(leto = stevilka, zivila, poraba)

#znebim se oklepajev

doma.porabljeni$zivila <- doma.porabljeni$zivila %>% str_replace_all("\\(([^)]+)\\)", '') %>% str_replace_all("\\[([^)]+)\\]", "")


vektor_zivil <- unique(doma.porabljeni$zivila) %>% sort()
vektor_lepse <- c("zgano", "grah", "paradiznik", "marmelada", "hruske", "margarina",
                  "riz", "agrumi", "banane", "brezalk.pijace",
                  "cesen.cebula", "fizol", "meso", "cokolada", "meso", "mleko", "meso", "grozdje",
                  "jabolka", "jajca", "olje", "jogurt", "kakav", "kava", "zelenjava",
                  "zelenjava", "krompir", "kruh", "marelice.breskve", "maslo", "med",
                  "voda", "mleko", "zita", "zelenjava", "meso", "pivo", "ribe", "sirup",
                  "sok", "sir.skuta", "sladkor", "sladoled", "slive", "smetana", "meso",
                  "testenine", "vino", "zelenjava", "zelenjava", "pecivo")

ista_zivila <- tibble(
  zivila = vektor_zivil, lepse = vektor_lepse
)

doma.porabljeni <- doma.porabljeni %>% left_join(ista_zivila, by = "zivila") %>% dplyr::select(leto, zivila = lepse, poraba)

doma.porabljeni$leto <- as.double(doma.porabljeni$leto)

#stopnja samooskrbe
samooskrba <- read_csv("stopnja_samooskrbe.csv", skip = 2,
                            locale = locale(encoding = "Windows-1250"),
                            col_names=TRUE, col_types = cols(
                              .default = col_guess()
                              
                            ))
colnames(samooskrba)[1] <- 'leto'

samooskrba$leto <- as.double(samooskrba$leto)

samooskrba <- pivot_longer(samooskrba,
                                cols = colnames(samooskrba)[-c(1)],
                                names_to = "zivila", 
                                values_to = "stopnja.samooskrbe")

zivila <- tibble(
  zivila = c("Žita", "Meso", "Jajca", "Krompir", "Zelenjava", "Med", "Riž"),
  lepse = c("zita", "meso", "jajca", "krompir", "zelenjava", "med", "riz")
)

samooskrba <- samooskrba %>% left_join(zivila, by = "zivila") %>% dplyr::select(leto, zivila = lepse, stopnja.samooskrbe)

primerjava <- samooskrba$zivila

doma.porabljeni<- doma.porabljeni %>% filter(zivila %in% primerjava)

samooskrba.in.poraba <- samooskrba %>% left_join(doma.porabljeni, by = c("leto", "zivila"))

##transmutat stolpec 
#ce smo se zmozni preskrbeti glede na podatke o porabi in stopnji samooskrbe
