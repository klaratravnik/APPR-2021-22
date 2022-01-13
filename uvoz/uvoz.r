# 2. faza: Uvoz podatkov

source("lib/libraries.r")


sl <- locale("sl", decimal_mark=",", grouping_mark=".")


#####################1. tabela##################################################

# v hektarih

zemljisca <- read_csv("podatki/regije.csv" , skip = 2,
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

zemljisca <- zemljisca %>% dplyr::select(regija, vrsta.zemljisca, stevilka, povrsina)

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
zemljisca <- merge(x = zemljisca, y = imena.regij, by = "regija", all.x = TRUE) %>% dplyr::select(regija = oznaka, leto, vrsta.zemljisca, povrsina)



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
zemljisca <- merge(x = zemljisca, y = vrste_zemljisc, by = "vrsta.zemljisca", all.x = TRUE) %>% dplyr::select(regija, leto, vrsta.zemljisca = lepse, povrsina)

#izbrisem vrstice z NA v povrsini
zemljisca <- zemljisca %>% dplyr::filter(!is.na(povrsina))

zemljisca$leto <- as.integer(zemljisca$leto)
zemljisca$regija <- as.factor(zemljisca$regija)
zemljisca$vrsta.zemljisca <- as.factor(zemljisca$vrsta.zemljisca)


vektor_let <- unique(zemljisca$leto) %>% sort()


# sestejem stevilo zemljisc glede na leto in regijo

zemljisca.skupno <- zemljisca %>% filter(vrsta.zemljisca != "skupaj") %>% 
  group_by(leto, regija) %>% mutate(skupno.stevilo.zemljisc = sum(povrsina)) %>%
  dplyr::select(regija, leto, skupno.stevilo.zemljisc)
zemljisca.skupno <- distinct(zemljisca.skupno)
zemljisca.skupno$leto <- as.integer(zemljisca.skupno$leto)

###################PRIDELEK###################################################

# v tonah na hektar

pridelek <- read_csv("podatki/povprecni_pridelek_poregijah.csv", skip = 2,
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

pridelek <- pridelek %>% dplyr::select(regija = crke, leto = stevilka, kmetijske.kulture, povprecni.pridelek)

#left join za regije
pridelek <- merge(x = pridelek, y = imena.regij, by = "regija", all.x = TRUE) %>% dplyr::select(regija = oznaka, leto, kmetijske.kulture, povprecni.pridelek)

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

pridelek <- pridelek %>% left_join(vrste_kultur, by = c("kmetijske.kulture")) %>% dplyr::select(regija, leto, kmetijske.kulture = lepse, povprecni.pridelek)
pridelek <- pridelek %>% filter(!is.na(povprecni.pridelek))

pridelek$regija <- as.factor(pridelek$regija)
pridelek$leto <- as.integer(pridelek$leto)
pridelek$kmetijske.kulture <- as.factor(pridelek$kmetijske.kulture)


pridelek.skupno <- pridelek %>% filter(leto %in% vektor_let) %>% group_by(leto, regija) %>% mutate(skupni.povprecni.pridelek = sum(povprecni.pridelek)) %>%
  dplyr::select(regija, leto, skupni.povprecni.pridelek)
#pobrisem iste vrstice
pridelek.skupno <- distinct(pridelek.skupno)
pridelek.skupno$leto <- as.integer(pridelek.skupno$leto)
# zdruzim z zemljisca.skupno

zemljisca.in.pridelek <- zemljisca.skupno %>% left_join(pridelek.skupno, by = c("regija", "leto")) %>%
  filter(!is.na(skupni.povprecni.pridelek))


####################2. tabela##################################################

zivina3 <- read_csv("podatki/zivina2.csv", skip = 2,
                   locale = locale(encoding = "Windows-1250"),
                   col_names=TRUE, col_types = cols(
                     .default = col_character(),
                     "2003 Število živali" = col_double(),
                     "2005 Število živali" = col_double(),
                     "2007 Število živali" = col_double(),
                     "2010 Število živali" = col_double(),
                     "2013 Število živali" = col_double(),
                     "2016 Število živali" = col_double(),
                   ))

zivina3 <- pivot_longer(zivina3,
                       cols = colnames(zivina3)[-c(1, 2)],
                       names_to = "leto", 
                       values_to = "stevilo.zivine" )

colnames(zivina3)[1] <- 'regija'
colnames(zivina3)[2] <- 'vrsta.zivine'

zivina3 <- merge(x = zivina3, y = imena.regij, by = "regija", all.x = TRUE) %>% 
  dplyr::select(regija = oznaka, vrsta.zivine, leto, stevilo.zivine)

zivina3 <- zivina3 %>%
  tidyr::extract(
    col = leto,
    into = c("stevilka", "crke"),
    regex = "^(\\d{4})\\s+(.*)$"
  ) 
zivina3 <- zivina3 %>% rename(leto = stevilka) %>% dplyr::select(!crke)
zivina3$stevilo.zivine <- zivina3$stevilo.zivine %>% str_replace_all("[N, z]", NA_character_)

vektor_zivine3 <- unique(zivina3$vrsta.zivine) %>% sort()

vrsta_zivine3 <- data.frame(
  vrsta.zivine = vektor_zivine3,
  lepse = c("govedo", "konji", "koze", "ovce", "perutnina", "velika.zivina", "prasici")
)

zivina3 <- zivina3 %>% left_join(vrsta_zivine3, by = c("vrsta.zivine")) 
zivina3 <- zivina3 %>% dplyr::filter(!is.na(stevilo.zivine)) %>% dplyr::select(regija, vrsta.zivine = lepse, leto, stevilo.zivine)

zivina3$stevilo.zivine <- as.integer(zivina3$stevilo.zivine)
zivina3$regija <- as.factor(zivina3$regija)
zivina3$vrsta.zivine <- as.factor(zivina3$vrsta.zivine)
zivina3$leto <- as.integer(zivina3$leto)


# sestejem stevilo zivine glede na leto in regijo

zivina.skupno <- zivina3 %>% filter(leto %in% vektor_let) %>% group_by(leto, regija) %>% mutate(skupno.stevilo.zivine = sum(stevilo.zivine)) %>%
  dplyr::select(regija, leto, skupno.stevilo.zivine)
#pobrisem iste vrstice
zivina.skupno <- distinct(zivina.skupno)
zivina.skupno$leto <- as.integer(zivina.skupno$leto)

###########join zemljisc in zivine#########################################

zemljisca.in.zivina <- zemljisca.skupno %>% left_join(zivina.skupno, by = c("regija", "leto"))
#koliko je živine na hektar zemljišča
zemljisca.in.zivina <- zemljisca.in.zivina %>% mutate(zivina.na.ha = skupno.stevilo.zivine / skupno.stevilo.zemljisc)

#################3. tabela #####################################################

#potrosnja je v kg NA PREBIVALCA !!!!
potrosnja <- read_csv("podatki/potrosnja_kmetijskihpridelkov.csv", skip = 2,
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

potrosnja <- merge(x = potrosnja, y = ime_pridelka, by = "pridelek", all.x = TRUE) %>% dplyr::select(leto, pridelek = lepse, potrosnja)

potrosnja$potrosnja <- as.double(potrosnja$potrosnja)
potrosnja$leto <- as.integer(potrosnja$leto)
potrosnja$pridelek <- as.factor(potrosnja$pridelek)

#potrosnjo moram pretvoriti iz potrosnje na prebivalca > množim z dvema miljonoma
potrosnja <- potrosnja %>% mutate(potrosnja = potrosnja *2000000)
#sestejem skupne faktorje
potrosnja <- potrosnja %>% group_by(leto,pridelek) %>% mutate(potrosnja = sum(potrosnja))


###########################################################################
#prodaja v kg 
prodaja <- read_csv("podatki/prodaja_kmetijskihpridelkov.csv", skip = 2,
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

prodaja <- prodaja %>% dplyr::select(leto = stevilka, pridelek, prodaja)

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

prodaja <- prodaja %>% left_join(prodaja_pridelek, by = c("pridelek")) %>% 
  filter(!is.na(prodaja)) %>% dplyr::select(leto, pridelek = lepse, prodaja)

prodaja$prodaja <- as.double(prodaja$prodaja)
prodaja$leto <- as.integer(prodaja$leto)
prodaja$pridelek <- as.factor(prodaja$pridelek)

prodaja <- prodaja %>% group_by(leto, pridelek) %>% mutate(prodaja = sum(prodaja)) %>%
  distinct(prodaja) %>% filter(pridelek %in% potrosnja$pridelek)


potrosnja <- potrosnja %>% filter(pridelek %in% prodaja$pridelek)
#zdruzenje tabeli potrosnja in prodaja po letu in zivilih

prodaja.in.potrosnja <- prodaja %>% left_join(potrosnja, by = c("leto", "pridelek")) %>% 
  mutate(razmerje = potrosnja / prodaja)
# koliko več porabimo kot prodamo, razmerje


##################################4. tabela#######################################################

#poraba na clana gospodinjstva v kg
doma.porabljeni <- read_csv("podatki/kolicina_doma_porabljenih_zivil.csv", skip = 2,
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

doma.porabljeni <- doma.porabljeni %>% dplyr::select(leto = stevilka, zivila, poraba)

#znebim se oklepajev

doma.porabljeni$zivila <- doma.porabljeni$zivila %>% str_replace_all("\\(([^)]+)\\)", '') %>% str_replace_all("\\[([^)]+)\\]", "")


vektor_zivil <- unique(doma.porabljeni$zivila) %>% sort()
vektor_lepse <- c("zgano", "grah", "paradiznik", "marmelada", "hruske", "margarina",
                  "riž", "agrumi", "banane", "brezalk.pijace",
                  "cesen.cebula", "fizol", "meso", "cokolada", "meso", "mleko", "meso", "grozdje",
                  "jabolka", "jajca", "olje", "jogurt", "kakav", "kava", "zelenjava",
                  "zelenjava", "krompir", "kruh", "marelice.breskve", "maslo", "med",
                  "voda", "mleko", "žita", "zelenjava", "meso", "pivo", "ribe", "sirup",
                  "sok", "sir.skuta", "sladkor", "sladoled", "slive", "smetana", "meso",
                  "testenine", "vino", "zelenjava", "zelenjava", "pecivo")

ista_zivila <- tibble(
  zivila = vektor_zivil, lepse = vektor_lepse
)

doma.porabljeni <- doma.porabljeni %>% left_join(ista_zivila, by = "zivila") %>% dplyr::select(leto, zivila = lepse, poraba)
doma.porabljeni$zivila <- as.factor(doma.porabljeni$zivila)
doma.porabljeni$leto <- as.integer(doma.porabljeni$leto)

#doma.porabljeni <- doma.porabljeni %>% group_by(leto) %>% mutate(skupna.poraba = sum(poraba)) %>%
 # dplyr::select(leto, zivila, poraba = skupna.poraba)
zemljisca.skupno <- distinct(zemljisca.skupno)
#stopnja samooskrbe v %
samooskrba <- read_csv("podatki/stopnja_samooskrbe.csv", skip = 2,
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
  lepse = c("žita", "meso", "jajca", "krompir", "zelenjava", "med", "riž")
)

samooskrba <- samooskrba %>% left_join(zivila, by = "zivila") %>% dplyr::select(leto, zivila = lepse, stopnja.samooskrbe)
samooskrba$zivila <- as.factor(samooskrba$zivila)

primerjava <- samooskrba$zivila

doma.porabljeni<- doma.porabljeni %>% filter(zivila %in% primerjava)

samooskrba.in.poraba <- samooskrba %>% left_join(doma.porabljeni, by = c("leto", "zivila")) %>% 
  filter(leto %in% doma.porabljeni$leto) %>% group_by(leto, zivila) %>% 
  mutate(poraba = mean(poraba), preskrba = stopnja.samooskrbe / poraba) 
samooskrba.in.poraba <- distinct(samooskrba.in.poraba)


#ce smo se zmozni preskrbeti glede na podatke o porabi in stopnji samooskrbe

#funkcija za izračun zmožnosti samooskrbe z določenim živilom
Zmoznost <- function(preskrba) {
  case_when(
    preskrba <= 0.5 ~ "NE",
    preskrba <= 30 ~ "DELNO",
    preskrba > 30 ~ "DA",
    TRUE ~ "neznan vhod"
  )
}

samooskrba.in.poraba <-samooskrba.in.poraba %>% mutate(zmoznost = Zmoznost(preskrba))


#shranim urejene tabele v mapo podatki
write.csv(zemljisca,"podatki/zemljisca_urejeno.csv")
write.csv(pridelek, "podatki/pridelek_urejeno.cvs")
write.csv(zivina3, "podatki/zivina3_urejeno.csv")
write.csv(zemljisca.in.zivina, "podatki/zemljisca.in.zivina_urejeno.csv")
write.csv(prodaja, "podatki/prodaja_urejeno.csv")
write.csv(potrosnja, "podatki/potrosnja_urejeno.csv")
write.csv(prodaja.in.potrosnja, "podatki/prodaja.in.potrosnja_urejeno.csv")
write.csv(doma.porabljeni, "podatki/doma.porabljeni_urejeno.csv")
write.csv(zemljisca.in.pridelek, "podatki/zemljisca.in.pridelek_urejeno.csv")


write.csv(samooskrba.in.poraba, "podatki/samooskrba.in.poraba_urejeno.csv")

