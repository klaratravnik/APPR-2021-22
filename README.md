# Analiza podatkov s programom R - 2021/22

Vzorčni repozitorij za projekt pri predmetu APPR v študijskem letu 2021/22. 

## Analiza slovenskega kmetijstva in samooskrbe

V sklopu projekta bom analizirala sestavo kmetijstva v Sloveniji in zmožnost samooskrbe z doma pridelano hrano. Opazovala bom števila različnih vrst kmetijskih zemljišč po statističnih regijah, povprečne pridelke na teh zemljiščih ter števila kmetij, ki imajo posamezno vrsto živine. Analizirala bom prodajo kmetijskih pridelkov iz lastne pridelave na živilskih trgih ter potrošnjo teh proizvodov na prebivalca (v kg). Primerjala bom tudi stopnjo samooskrbe (v %) po vrstah kmetijskih proizvodov ter količino doma porabljenih živil na člana gospodinjstva ter izračunala, kako je Slovenija sposobna samooskrbe glede na te podatke.

Viri podatkov: 
* https://pxweb.stat.si/SiStat/sl/Podrocja/Index/85/kmetijstvo-gozdarstvo-in-ribistvo

## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Potrebne knjižnice so v datoteki `lib/libraries.r`
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).
