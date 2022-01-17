# 4. faza: Napredna analiza podatkov

source("lib/libraries.r")

#####################################LINEARNA REGRESIJA#######################################################

data.za.analizo <- zemljisca.in.zivina %>% filter(regija %in% c("lj", "nm", "mb", "ms", "kp", "kr"))

g <- ggplot(data.za.analizo, aes(x=skupno.stevilo.zemljisc, y=skupno.stevilo.zivine)) + geom_point() + 
  labs(title = "Število živine glede na  površino \n kmetijskih zemljišč večjih slovenskih regij") + xlab("Površina zemljišč") + ylab("Število živine") + 
  theme(plot.title = element_text(hjust=0.5, size=15),
        axis.line = element_line(colour = "purple", 
                                 size = 1, linetype = "solid"))

lin <- lm(data=data.za.analizo, skupno.stevilo.zivine ~ skupno.stevilo.zemljisc)
lin
napovedi <- predict(lin, data.frame(skupno.stevilo.zemljisc=seq(0, 250000, 5000)))

g + geom_line(data = data.frame(skupno.stevilo.zemljisc=seq(0, 250000, 5000), napovedi = napovedi),
              aes(x = skupno.stevilo.zemljisc, y = napovedi), col = "violet")

kv <- lm(data=data.za.analizo, skupno.stevilo.zivine ~ I(skupno.stevilo.zemljisc^2))
kv
g + geom_smooth(method="lm", formula = y ~ x + I(x^2), color="violet")

z <- lowess(data.za.analizo$skupno.stevilo.zemljisc, data.za.analizo$skupno.stevilo.zivine)
g + geom_line(data=as.data.frame(z), aes(x=x, y=y), color="violet")

mls <- loess(data=data.za.analizo, skupno.stevilo.zivine ~ skupno.stevilo.zemljisc)
g + geom_smooth(method="loess", color = "violet")

sapply(list(lin, kv, mls), function(x) mean((x$residuals^2)))  

#Ugotovim kater model ima najmanjšo napako in tega potem izberem.
which.min(sapply(list(lin, kv, mls), function(x) mean((x$residuals^2))))


analiziran.graf <- g + geom_smooth(method="loess", formula = y ~ x, color = "violet")
print(analiziran.graf)



#######################################RAZVRŠČANJE V SKUPINE ######################################################
## Z METODO VODITELJEV

set.seed(123)


data.za.razvrscanje <- merge(x = zemljisca.in.zivina, y = imena.regij.nazaj, by = "regija", all.x = TRUE) %>% 
  dplyr::select(regija = oznaka, leto, skupno.stevilo.zemljisc, skupno.stevilo.zivine, zivina.na.ha) %>% 
  filter(regija != "SLOVENIJA", leto == 2007) %>% dplyr::select(-leto)


 
data.za.razvrscanje.norm <- data.za.razvrscanje %>% dplyr::select(-regija) %>% scale()
rownames(data.za.razvrscanje.norm) <- data.za.razvrscanje$regija


skupine <- kmeans(data.za.razvrscanje.norm, 5, nstart=1000)
skupine$tot.withinss


skupine.zemljevid <- data.frame(regija = data.za.razvrscanje$regija, 
                                Skupina = factor(skupine$cluster))

tmap_mode("plot")

clustering.zemljevid <- tm_shape(merge(zemljevid.regije, skupine.zemljevid, by.x = "NAME_1", by.y = "regija")) + tm_polygons("Skupina", popup.vars = c("Skupina " = "Skupina"), title="Skupina") + 
  tm_layout(
    "Razvrstitev regij v skupine glede na št. zemljišč \n in  živine v 2007 ",
    legend.title.size=1,
    legend.text.size = 0.8,
    legend.position = c("right","bottom"),
    legend.bg.color = "white",
    legend.bg.alpha = 1)

print(clustering.zemljevid)




