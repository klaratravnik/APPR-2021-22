# 3. faza: Vizualizacija podatkov

source("lib/libraries.r")

#1. graf:Povprečni pridelek glede na število hektarjev \n kmetijskih površin po regijah
g1 <- ggplot(zemljisca.in.pridelek %>% filter(regija != "slo")) + 
  aes(x = leto, y = skupni.povprecni.pridelek, size = skupno.stevilo.zemljisc) +
  labs(title = "Povprečni pridelek glede na število hektarjev \n kmetijskih površin po regijah",
    x = "Leto",
    y = "Povprečni pridelek") +
  labs(size = "Skupno število zemljišč") +
  geom_point(color="darkblue") +
  theme(legend.title = element_text(color = "darkblue", size = 10),
        axis.line = element_line(colour = "darkblue", 
                                 size = 1, linetype = "solid"),
        legend.background = element_rect(fill = "lightblue", linetype="solid", 
                                         colour ="darkblue"), plot.title = element_text(color = "darkblue", hjust=0.5), strip.background=element_rect(colour="black",fill="white"),
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  facet_wrap(.~regija)




#2. graf: Primerjava povprečnih pridelkov v letih 2010 in 2020 za določene kulture
podatki2 <- pridelek %>% filter(regija != "slo", kmetijske.kulture %in% c("koruza", "krompir", "jecmen", "psenica.in.pira"), 
                                leto %in% c(2010, 2020))
g2 <- ggplot(podatki2) + 
  aes(x = kmetijske.kulture, y = povprecni.pridelek) + geom_bar(stat = "identity", fill = "lightblue")+
  xlab("Vrsta pridelka")+ ylab("Povprečni pridelek") + 
  labs(title = "Primerjava povprečnih pridelkov v letih 2010 in 2020")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.title.x = element_text(vjust = 0), 
  plot.title = element_text(color = "darkblue", hjust=0.5), strip.background=element_rect(colour="black",fill="white")) + 
  facet_wrap(.~leto)



#3. graf:Število živine glede na  površino  kmetijskih zemljišč večjih slovenskih regij
data <- zemljisca.in.zivina %>% filter(regija %in% c("lj", "nm", "mb", "ms", "kp", "kr"))
ggplot(data) + aes(x = skupno.stevilo.zemljisc, y = skupno.stevilo.zivine, color = leto, size = zivina.na.ha) +
  labs(title = "Število živine glede na  površino \n kmetijskih zemljišč večjih slovenskih regij") + xlab("Površina zemljišč") + ylab("Število živine") +
  labs(size="Živina na ha", color="Leto") + 
  geom_point() + 
  theme(plot.title = element_text(color = "darkblue", hjust=0.5, size=15),
        axis.line = element_line(colour = "darkblue", 
                                 size = 1, linetype = "solid"),
        legend.background = element_rect(fill = "lightblue", linetype="solid", 
                                         colour ="darkblue"))


g3 <- ggplot(zemljisca.in.zivina %>% filter(regija != "slo")) +
  aes(x = skupno.stevilo.zemljisc, y = skupno.stevilo.zivine, size = zivina.na.ha, color = leto,
      text = paste(
        "Regija: ", regija, "\n",
        "Leto: ", leto,
        sep = ""
      )) + ggtitle("Število živine glede na površino kmetijskih zemljišč") + xlab("Površina zemljišč") + ylab("Število živine") + 
  geom_point() + labs(color="Leto")
ggplotly(g3, tooltip = "text")




#4. graf
#tortni diagram

podatki3 <- prodaja.in.potrosnja %>% filter(leto == 2020)
g4 <- ggplot(podatki3, aes(x="", y=razmerje, fill=pridelek)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) + theme_void() +
  scale_fill_hue() + 
  labs(title = "Razmerje med potrošnjo in prodajo živil v letu 2020") +
  labs(fill = "Pridelek") + 
  theme(plot.title = element_text(color = "darkblue", hjust=0.5, size=15, vjust = -8),
        legend.title = element_text(color = "darkblue", size = 12),
        legend.background = element_rect(fill = "lightblue", linetype="solid", 
                                         colour ="darkblue")) 


#5. graf:

g5 <- ggplot(samooskrba.in.poraba) + aes(x = leto, y = stopnja.samooskrbe,color = zmoznost) + 
  labs(title = "Samooskrba in preskrba z živili") + xlab("Leto") + ylab("Stopnja samooskrbe (v %)") + 
  theme(legend.position = "right") + labs(color = "Zmožnost samooskrbe") +
  geom_point(size=3.5) + facet_wrap(.~zivila) + 
  theme(plot.title = element_text(color = "darkblue", hjust=0.5, size=15)) + 
  theme(strip.background=element_rect(colour="black",fill="white"), legend.title = element_text(color = "darkblue", size = 10),
        legend.background = element_rect(fill = "lightblue", linetype="solid", colour ="darkblue"), axis.text.x = element_text(angle = 45, hjust = 1, colour = "darkblue")) + 
  scale_color_manual(values=c("lightgreen", "orange", "red"))


#6. graf:
g6 <- ggplot(samooskrba.in.poraba, aes(poraba, zivila)) + geom_boxplot(fill = "lightblue") + xlab("Poraba") + 
  ylab("Živila") + 
  theme(axis.line = element_line(colour = "darkblue", 
                                 size = 1, linetype = "solid"),
        axis.text.y = element_text(colour = "darkblue")) + xlim(0, 130)



########################################## ZEMLJEVIDI #########################################################


zemljevid <- readOGR("podatki/SLO1/gadm36_SVN_1.shp", "gadm36_SVN_1")
zemljevid <- zemljevid %>% spTransform(CRS("+proj=longlat +datum=WGS84")) # pretvorimo v ustrezen format

loc <- locale(encoding="UTF-8")
for (col in names(zemljevid)) {
  if (is.character(zemljevid[[col]])) {
    zemljevid[[col]] <- zemljevid[[col]] %>% parse_character(locale=loc)
  }
}

zemljevid@data
zemljevid.regije <- zemljevid
names(zemljevid.regije)
zemljevid.regije$NAME_1



zemljevid.regije$NAME_1 <- factor(zemljevid.regije$NAME_1)

lvls <- levels(zemljevid.regije$NAME_1)

# spremenim imena regij, da se ujemajo z zemljevidom, kratice spremenim nazaj v dolga imena

imena.regij.nazaj = tibble(
  oznaka = c(
    "Gorenjska",
    "Goriška",
    "Jugovzhodna Slovenija",
    "Koroška",
    "Obalno-kraška",
    "Osrednjeslovenska",
    "Podravska",
    "Pomurska",
    "Spodnjeposavska",
    "Notranjsko-kraška",
    "Savinjska",
    "Zasavska",
    "SLOVENIJA"
  ),
  regija = c(
    "kr", "ng", "nm", "sg", "kp", "lj", "mb", "ms", "kk", "po", "ce", "za", "slo"
  )
)

#Primerjava števila goveda v regijah leta 2003 in 2016
tmap_mode("plot")
zivina3.za.zemljevid <- merge(x = zivina3, y = imena.regij.nazaj, by = "regija", all.x = TRUE) %>% 
  dplyr::select(regija = oznaka, vrsta.zivine, leto, stevilo.zivine) %>% filter(regija != "SLOVENIJA")


zivina3.za.zemljevid.2003 <- zivina3.za.zemljevid %>% filter(leto == 2003, vrsta.zivine == "govedo") %>% group_by(regija, leto) 
graf.zemljevid0 <- merge(zemljevid.regije, zivina3.za.zemljevid.2003, by.x = "NAME_1", by.y = "regija")
zem1 <- tm_shape(graf.zemljevid0) + tm_polygons("stevilo.zivine" , popup.vars = c("Število goveda: " = "stevilo.zivine"),
                                       style = "pretty", palette="Blues",title="Število goveda") + 
  tm_layout(
    "Število goveda v letu 2003",
    legend.title.size=1,
    legend.text.size = 0.8,
    legend.position = c("right","bottom"),
    legend.bg.color = "white",
    legend.bg.alpha = 1)


zivina3.za.zemljevid.2016 <- zivina3.za.zemljevid %>% filter(leto == 2016, vrsta.zivine == "govedo") %>% group_by(regija, leto) 
graf.zemljevid <- merge(zemljevid.regije, zivina3.za.zemljevid.2016, by.x = "NAME_1", by.y = "regija")
zem2 <- tm_shape(graf.zemljevid) + tm_polygons("stevilo.zivine" , popup.vars = c("Število goveda: " = "stevilo.zivine"),
                                       style = "pretty", palette="Blues",title="Število goveda") +
  tm_layout(
    "Število goveda v letu 2016",
    legend.title.size=1,
    legend.text.size = 0.8,
    legend.position = c("right","bottom"),
    legend.bg.color = "white",
    legend.bg.alpha = 1)



#Primerjava hektarjev gozdov leta 2003 in 2016
zemljisca.za.zemljevid <- merge(x = zemljisca, y = imena.regij.nazaj, by = "regija", all.x = TRUE) %>%
  dplyr::select(regija = oznaka, leto, vrsta.zemljisca, povrsina) 

zemljisca.2003 <- zemljisca.za.zemljevid %>% filter(regija != "SLOVENIJA", vrsta.zemljisca == "gozd", leto == 2003)

graf.zemljevid2 <- merge(zemljevid.regije, zemljisca.2003, by.x = "NAME_1", by.y = "regija")

zem3 <- tm_shape(graf.zemljevid2) + tm_polygons("povrsina" , popup.vars = c("Število hektarjev gozdov " = "povrsina"),
                                                     style = "pretty", palette="Blues",
                                                    title="Število hektarjev gozda") +
  tm_layout(
    "Število hektarjev gozdov v 2003",
    legend.title.size=1,
    legend.text.size = 0.8,
    legend.position = c("right","bottom"),
    legend.bg.color = "white",
    legend.bg.alpha = 1)

zemljisca.2016 <- zemljisca.za.zemljevid %>% filter(regija != "SLOVENIJA", vrsta.zemljisca == "gozd", leto == 2016)
graf.zemljevid3 <- merge(zemljevid.regije, zemljisca.2016, by.x = "NAME_1", by.y = "regija")

zem4 <- tm_shape(graf.zemljevid3) + tm_polygons("povrsina" , popup.vars = c("Število hektarjev gozdov " = "povrsina"),
                                        style = "pretty", palette="Blues",
                                        title="Število hektarjev gozda") + 
  tm_layout("Število hektarjev gozdov v 2016",
    legend.title.size=1,
    legend.text.size = 0.8,
    legend.position = c("right","bottom"),
    legend.bg.color = "white",
    legend.bg.alpha = 1)



