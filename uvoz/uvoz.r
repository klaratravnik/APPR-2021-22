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


pridelek[3] <- pridelek$kmetijske.kulture %>% str_replace_all("\\š", "s") %>% str_replace_all("\\č", "c") %>% str_replace_all("\\ž", "z") %>% str_replace_all("(^[:alpha:]{8} [:alpha:]{6} [:alpha:]{8})", "trave") %>% str_replace_all("(^[:alpha:]{6} [:alpha:]{8} [:alpha:]{8})", "trave") %>% str_replace_all("^([:alpha:]{5})$","trave")

kulture <- c("Deteljno travne mešanice", "Trave", "Belo zelje", "Lucerna", "Travno deteljne mešanice", "Breskve in nektarine v intenzivnih sadovnjakih",
             "Grozdje", "Trajni travniki in pašniki, vključno s skupnimi travniki in pašniki",
             "Oljke", "Koruza za zrnje", "Oves", "Jabolka v intenzivnih sadovnjakih",
             "Pšenica in pira", "Buče za olje", "Hmelj", "Tritikala", "Rž in soržica", "Ječmen", "Oljna ogrščica in repica",
             "Krompir", "Silažna koruza", "Detelja")
`Encoding<-`(kulture, "UTF-8")

vrste_kultur <- data.frame(
  kmetijske.kulture = c(
    "trave",
    "Trave",
    "trave",
    "Belo zelje",
    "Lucerna",
    "Breskve in nektarine v intenzivnih sadovnjakih",
    "Grozdje",
    "Trajni travniki in pasniki, vkljucno s skupnimi travniki in pasniki",
    "Oljke",
    "Koruza za zrnje",
    "Oves",
    "Jabolka v intenzivnih sadovnjakih",
    "Detelja",
    "Psenica in pira",
    "Buce za olje",
    "Hmelj",
    "Rz in sorzica",
    "Tritikala",
    "Krompir",
    "Silazna koruza",
    "Jecmen",
    "Oljna ogrscica in repica"
    
    
  ),
  lepse = c(
    "trave",
    "trave",
    "trave",
    "belo.zelje",
    "lucerna",
    "breskve.in.nektarine",
    "grozdje",
    "travniki.in.pasniki",
    "oljke",
    "koruza.za.zrnje",
    "oves",
    "jabolka",
    "detelja",
    "psenica.in.pira",
    "buce",
    "hmelj",
    "rz.in.sorzica",
    "tritikala",
    "krompir",
    "silazna.koruza",
    "jecmen",
    "oljna.ogrscica.in.repica"
  )
)



#left join za kmetijske kulture
test3 <- merge(x = pridelek, y = vrste_kultur, by = "kmetijske.kulture", all.x = TRUE) #%>% select(regija, leto, kmetijske.kulture = lepse, povprecni.pridelek)
###### zakaj noce vseh trav mergat??????????
test4 <- pridelek %>% left_join(vrste_kultur, by = "kmetijske.kulture")

# joinat morem se tabelo zemljisca pa pridelek by = c("regija", "leto")


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

zivina[1] <- zivina$vrsta.zivine %>% str_replace_all("\\č|\\Č", "c") %>% str_replace_all("\\š|\\Š", "s") %>% str_replace_all("\\ž|\\Ž", "z")

vrsta_zivine <- data.frame(
  vrsta.zivine = c(
    "zivina - SKUPAJ",
    "Govedo", "Prasici", "Perutnina", "Konji", "Ovce", "Koze", "Kunci", "cebelje druzine", "Jelenjad"),
  lepse = c("skupaj", "govedo", "prasici", "perutnina", "konji", "ovce", "koze", "kunci", "cebelje.druzine", "jelenjad")
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

prodaja[2] <- prodaja$pridelek %>% str_replace_all("\\č|\\Č", "c") %>% str_replace_all("\\š|\\Š", "s") %>% str_replace_all("\\ž|\\Ž", "z")    
  
prodaja_pridelek <- tibble(
  pridelek = c("Krompir, jedilni", "Fizol, v zrnju", "Fizol, strocji", "Grah, strocji", "cebula", "cesen", "Por",
               "Belo zelje", "Rdece zelje", "Kitajski kapus", "Ohrovt", "Cvetaca in brokoli", "Korencek", "Paradiznik",
               "Paprika", "Jajcevec", "spinaca", "Zelena solata", "Endivija", "Radic, vse vrste", "Motovilec", "Kumare, solatne",
               "Kumare, za vlaganje", "Bucke", "Rdeca pesa", "Lubenice in dinje", "sparglji in belusi", "Sveze slive in cesplje",
               "Suhe slive", "Namizna jabolka", "Hruske", "cesnje", "Visnje", "Marelice", "Breskve in nektarine",
               "Celi orehi", "Orehova jedrca", "Celi lesniki in mandlji", "Lesnikova in mandljeva jedrca", "Kostanj",
               "Jagode", "Maline", "Ameriske borovnice", "Sveze fige", "Suhe fige", "Kaki", "Grozdje", "Kokosi in piscanci, meso",
               "Purani, meso", "Jajca, konzumna", "Sveze mleko", "Surovo maslo", "Kajmak", "Siri vseh vrst",
               "Smetana", "Oljcno olje", "Kislo zelje", "Kisla repa", "Sveze gobe", "Med", "Psenicna moka",
               "Koruzna moka"),
  lepse = c("krompir", "fizol", "fizol", "grah", "cebula", "cesen", "por", 
            "belo.zelje", "rdece.zelje", "kitajski.kapus", "ohrovt", "cvetaca.in.brokoli", "korenje", "paradiznik", 
            "paprika", "jajcevec", "spinaca", "zelena.solata", "endivija", "radic", "motovilec", "kumare",
            "kumare", "bucke", "rdeca.pesa", "lubenice", "sparglji.in.belusi", "slive.in.cesplje", 
            "suhe.slive", "jabolka", "hruske", "cesnje", "visnje", "marelice", "breskve.in.nektarine",
            "orehi", "orehi", "lesniki.in.mandlji", "lesniki.in.mandlji", "kostanj",
            "jagode", "maline", "ameriske.borovnice", "fige", "fige", "kaki", "grozdje", "piscancje.meso",
            "puranje.meso", "jajca", "mleko", "maslo", "kajmak", "sir",
            "smetana", "oljcno.olje", "kislo.zelje", "kisla.repa", "gobe", "med", "moka", "moka")
)

#spet da NA ??????????????????? kaj je narobe?
test <- merge(x = prodaja, y = prodaja_pridelek, by = "pridelek", all.x = TRUE)# %>% select(leto, pridelek = lepse, potrosnja)

test4 <- prodaja %>% left_join(prodaja_pridelek, by = "pridelek")

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




#stopnja samooskrbe
samooskrba <- read_csv("stopnja_samooskrbe.csv", skip = 2,
                            locale = locale(encoding = "Windows-1250"),
                            col_names=TRUE, col_types = cols(
                              .default = col_guess()
                              
                            ))
colnames(samooskrba)[1] <- 'leto'

samooskrba <- pivot_longer(samooskrba,
                                cols = colnames(samooskrba)[-c(1)],
                                names_to = "zivila", 
                                values_to = "stopnja.samooskrbe")

zivila <- tibble(
  zivila = c("Žita", "Meso", "Jajca", "Krompir", "Zelenjava", "Med", "Riž"),
  lepse = c("zita", "meso", "jajca", "krompir", "zelenjava", "med", "riz")
)

samooskrba <- samooskrba %>% left_join(zivila, by = "zivila") 
samooskrba <- samooskrba %>% select(leto, zivila = lepse, stopnja.samooskrbe)


##uredit moram se tabelo doma.porabljeni in potem zdruzit s samooskrbo ter transmutat stolpec 
#ce smo se zmozni preskrbeti glede na podatke o porabi in stopnji samooskrbe
