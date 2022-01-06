# 3. faza: Vizualizacija podatkov

library(ggplot2)
library(dplyr)

#1. graf: 

ggplot(zemljisca.skupno %>% filter(regija != "slo")) + aes(x = leto, y = skupno.stevilo.zemljisc, color = regija) + 
  ggtitle("Število hektarov kmetijskih površin v slovenskih regijah skozi leta") + xlab("Leto") + ylab("Hektarji zemljišč") +
  geom_line(size = 1.5) 


#2. graf:
ggplot(pridelek) + aes(x = kmetijske.kulture, y = povprecni.pridelek, color = leto) + 
  ggtitle("Povprečni pridelek kmetijskih kultur") + xlab("Kultura") + ylab("Povprečni pridelek") +
  geom_col() + scale_fill_manual(values = c("blue", "red", "yellow"))

  