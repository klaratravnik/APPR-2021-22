# 3. faza: Vizualizacija podatkov

library(ggplot2)
library(dplyr)
library(plotly)

#1. graf: 

ggplot(zemljisca.skupno %>% filter(regija != "slo")) + aes(x = leto, y = skupno.stevilo.zemljisc, color = regija) + 
  ggtitle("Število hektarov kmetijskih površin v slovenskih regijah skozi leta") + xlab("Leto") + ylab("Hektarji zemljišč") +
  geom_line(size = 1.5) 


#2. graf:
ggplot(pridelek) + aes(x = kmetijske.kulture, y = povprecni.pridelek, fill = leto) + 
  ggtitle("Povprečni pridelek kmetijskih kultur") + xlab("Kultura") + ylab("Povprečni pridelek") +
  geom_col()


#3. graf:
ggplot(zemljisca.in.pridelek %>% filter(regija != "slo")) + 
  aes(x = regija, y = skupni.povprecni.pridelek, color = leto, size = skupno.stevilo.zemljisc) +
  ggtitle("Povprečni pridelek glede na število hektarjev kmetijskih površin po regijah") + xlab("Leto") + ylab("Povprečni pridelek") +
  geom_point() 

#4. graf:
ggplot(zemljisca.in.zivina %>% filter(regija != "slo")) + aes(x = skupno.stevilo.zemljisc, y = skupno.stevilo.zivine, size = zivina.na.ha, color = leto) +
  ggtitle("Število živine glede na površino kmetijskih zemljišč") + xlab("Površina zemljišč") + ylab("Število živine") + 
  geom_point()


graf4 = ggplot(zemljisca.in.zivina %>% filter(regija != "slo")) +
  aes(x = skupno.stevilo.zemljisc, y = skupno.stevilo.zivine, size = zivina.na.ha, color = leto,
      text = paste(
        "Regija: ", regija,
        sep = ""
      )) + ggtitle("Število živine glede na površino kmetijskih zemljišč") + xlab("Površina zemljišč") + ylab("Število živine") + 
  geom_point()
ggplotly(graf4, tooltip = "text")


#5. graf:
ggplot(prodaja.in.potrosnja) + aes(x = leto, y = razmerje, fill = pridelek) +
  ggtitle("Razmerje med potrošnjo in prodajo živil") + xlab("Leto") + ylab("Razmerje potrošnja : prodaja") + 
  theme(legend.position = "right") + labs(fill="Pridelek") +
  geom_col()


#6. graf:

ggplot(samooskrba) + aes(x = leto, y = stopnja.samooskrbe, color = zivila) + 
  geom_point(size = 3)


ggplot(samooskrba.in.poraba) + aes(x = leto, y = stopnja.samooskrbe, size = preskrba, color = zivila, shape = zmoznost) + 
  ggtitle("Samooskrba in preskrba z živili", title) + xlab("Leto") + ylab("Stopnja samooskrbe (v %)") + 
  theme(legend.position = "bottom") + labs(size="Preskrba", color = "Živila", shape = "Zmožnost samooskrbe") +
  geom_point()
