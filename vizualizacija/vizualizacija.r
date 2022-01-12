# 3. faza: Vizualizacija podatkov
library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(xml2)
library(rvest)

library(ggplot2)
library(dplyr)
library(plotly)
library(sp)
library(rgdal)


#1. graf:Povprečni pridelek glede na število hektarjev \n kmetijskih površin po regijah
ggplot(zemljisca.in.pridelek %>% filter(regija != "slo")) + 
  aes(x = leto, y = skupni.povprecni.pridelek, size = skupno.stevilo.zemljisc) +
  labs(title = "Povprečni pridelek glede na število hektarjev \n kmetijskih površin po regijah") + xlab("Leto") + ylab("Povprečni pridelek") +
  labs(size = "Skupno število zemljišč") +
  geom_point(color="darkblue") + facet_wrap(.~regija) +
  theme(legend.title = element_text(color = "darkblue", size = 10),
        axis.line = element_line(colour = "darkblue", 
                                 size = 1, linetype = "solid"),
        legend.background = element_rect(fill = "lightblue", linetype="solid", 
                                         colour ="darkblue"), plot.title = element_text(color = "darkblue", hjust=0.5), strip.background=element_rect(colour="black",fill="white")) 



#2. graf: Primerjava povprečnih pridelkov v letih 2010 in 2020 za določene kulture
podatki2 <- pridelek %>% filter(regija != "slo", kmetijske.kulture %in% c("koruza", "krompir", "jecmen", "psenica.in.pira"), 
                                leto %in% c(2010, 2020))
ggplot(podatki2) + 
  aes(x = kmetijske.kulture, y = povprecni.pridelek) + geom_bar(stat = "identity", fill = "lightblue")+
  xlab("Vrsta pridelka")+ ylab("Povprečni pridelek") + 
  labs(title = "Primerjava povprečnih pridelkov v letih 2010 in 2020")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.title.x = element_text(vjust = 0)) + facet_wrap(.~leto) + 
  theme(plot.title = element_text(color = "darkblue", hjust=0.5), strip.background=element_rect(colour="black",fill="white")) 



#2. graf:Število živine glede na  površino  kmetijskih zemljišč večjih slovenskih regij
ggplot(zemljisca.in.zivina %>% filter(regija %in% c("lj", "nm", "mb", "ms", "kp", "kr"))) + aes(x = skupno.stevilo.zemljisc, y = skupno.stevilo.zivine, color = leto, size = zivina.na.ha) +
  labs(title = "Število živine glede na  površino \n kmetijskih zemljišč večjih slovenskih regij") + xlab("Površina zemljišč") + ylab("Število živine") +
  labs(size="Živina na ha", color="Leto") + 
  geom_point() + 
  theme(plot.title = element_text(color = "darkblue", hjust=0.5, size=15),
        axis.line = element_line(colour = "darkblue", 
                                 size = 1, linetype = "solid"),
        legend.background = element_rect(fill = "lightblue", linetype="solid", 
                                         colour ="darkblue"))


graf2 <- ggplot(zemljisca.in.zivina %>% filter(regija != "slo")) +
  aes(x = skupno.stevilo.zemljisc, y = skupno.stevilo.zivine, size = zivina.na.ha, color = leto,
      text = paste(
        "Regija: ", regija, "\n",
        "Leto: ", leto,
        sep = ""
      )) + ggtitle("Število živine glede na površino kmetijskih zemljišč") + xlab("Površina zemljišč") + ylab("Število živine") + 
  geom_point() + labs(color="Leto")
ggplotly(graf2, tooltip = "text")



#3. graf:
#ggplot(prodaja.in.potrosnja) + aes(x = leto, y = razmerje) +
 # xlab("Leto") + ylab("Razmerje potrošnja : prodaja") +
 # geom_col(fill="darkred") + facet_wrap(.~pridelek) + 
 # labs(title = "Razmerje med potrošnjo in prodajo živil") +
 # theme(plot.title = element_text(color = "darkred", hjust=0.5, size=15), legend.position="none",
   #     strip.background=element_rect(colour="black", fill="white"))


#tortni diagram

ggplot(prodaja.in.potrosnja %>% filter(leto == 2020), aes(x="", y=razmerje, fill=pridelek)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) + theme_void() +
  scale_fill_brewer(palette="Set1") + 
  labs(title = "Razmerje med potrošnjo in prodajo živil v letu 2020") +
  labs(fill = "Pridelek") + 
  theme(plot.title = element_text(color = "darkred", hjust=0.5, size=15, vjust = -8),
        legend.title = element_text(color = "darkred", size = 12),
        legend.background = element_rect(fill = "white", linetype="solid", 
                                         colour ="darkred"))
  



#4. graf:

ggplot(samooskrba.in.poraba) + aes(x = leto, y = stopnja.samooskrbe,color = zmoznost) + 
  labs(title = "Samooskrba in preskrba z živili") + xlab("Leto") + ylab("Stopnja samooskrbe (v %)") + 
  theme(legend.position = "right") + labs(color = "Zmožnost samooskrbe") +
  geom_line(size=2) + facet_wrap(.~zivila) + 
  theme(plot.title = element_text(color = "darkred", hjust=0.5, size=15)) + 
  theme(strip.background=element_rect(colour="black",fill="white"), legend.title = element_text(color = "darkred", size = 10),
        legend.background = element_rect(fill = "white", linetype="solid", colour ="darkred"), axis.text.x = element_text(angle = 45, hjust = 1, colour = "darkred")) + 
  scale_color_manual(values=c("lightgreen", "orange", "red"))




#graf6 = ggplot(samooskrba.in.poraba) + 
 # aes(x = leto, y = stopnja.samooskrbe, size = preskrba, color = zivila, 
    #  text = paste(
    #    "Živilo: ", zivila, "\n",
    #    "Zmožnost samooskrbe: ", zmoznost,
    #    sep = ""
   #   )) + ggtitle("Samooskrba in preskrba z živili") + xlab("Leto") + ylab("Stopnja samooskrbe (v %)") + 
 # theme(legend.position = "bottom") + labs(size="Preskrba (velikost)", color = "Živila") +
 # geom_point()
#ggplotly(graf6, tooltip = "text")


#5. graf:
podatki <- zivina3 %>% filter(regija != "slo") %>% group_by(vrsta.zivine, leto) %>% mutate(st.zivine = sum(stevilo.zivine)) %>%
  select(leto, vrsta.zivine, st.zivine)
podatki <- distinct(podatki)

ggplot(podatki) + 
  aes(x = vrsta.zivine, y = st.zivine) + 
  geom_point() + facet_wrap(.~leto)


########################################## ZEMLJEVIDI #########################################################


zemljevid <- readOGR("podatki/SLO/gadm36_SVN_1.shp", encoding = "UTF-8")
zemljevid <- zemljevid %>% spTransform(CRS("+proj=longlat +datum=WGS84")) # pretvorimo v ustrezen format

loc <- locale(encoding="Windows-1250")
for (col in names(zemljevid)) {
  if (is.character(zemljevid[[col]])) {
    zemljevid[[col]] <- zemljevid[[col]] %>% parse_character(locale=loc)
  }
}

zemljevid@data
zemljevid.regije <- zemljevid
names(zemljevid.regije)
zemljevid.regije$NAME_1

source("lib/uvozi.zemljevid.r")

zemljevid2 <- uvozi.zemljevid("http://biogeo.ucdavis.edu/data/gadm2.8/shp/SVN_adm_shp.zip",
                              "SVN_adm1", encoding = "UTF-8") %>% fortify()


