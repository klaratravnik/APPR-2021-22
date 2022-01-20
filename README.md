# Analiza podatkov s programom R - 2021/22

Vzorčni repozitorij za projekt pri predmetu APPR v študijskem letu 2021/22. 

## Analiza slovenskega kmetijstva in samooskrbe

V sklopu projekta bom analizirala sestavo kmetijstva v Sloveniji in zmožnost samooskrbe z doma pridelano hrano. Opazovala bom števila različnih vrst kmetijskih zemljišč po statističnih regijah, povprečne pridelke na teh zemljiščih ter števila kmetij, ki imajo posamezno vrsto živine. Analizirala bom prodajo kmetijskih pridelkov iz lastne pridelave na živilskih trgih ter potrošnjo teh proizvodov na prebivalca (v kg). Primerjala bom tudi stopnjo samooskrbe (v %) po vrstah kmetijskih proizvodov ter količino doma porabljenih živil na člana gospodinjstva ter izračunala, kako je Slovenija sposobna samooskrbe glede na te podatke.

Viri podatkov: 
* https://pxweb.stat.si/SiStat/sl/Podrocja/Index/85/kmetijstvo-gozdarstvo-in-ribistvo


Tabele:
1. **zemljisca** - tabela števila hektarjev posamezne vrste zemljišča v regiji, po letih
- `regija` - regija popisa podatkov
- `leto` - leto meritve
- `vrsta.zemljisca` - tip vrste zemljišča
- `povrsina` - število hektarjev

2. **pridelek** - tabela povprečnih pridelkov kmetijskih kultur v tonah na hektar
- `regija` - ime regije
- `leto` - leto popisa
- `kmetijske.kulture` - tip pridelka
- `povprecni.pridelek` - število povprečnega pridelka

3. **zemljisca.in.pridelek** - skupno število hektarjev, povprečni pridelek vseh seštetih kultur po regijah in letih ter šteto število živine po regijah in letih ne glede na vrsto in izračunan delež živine na število hektarjev zemljišča 
- `regija` - ime regije
- `leto` - leto popisa
- `skupno.stevilo.zemljisc` - število vseh vrst zemljišč skupaj
- `skupni.povprecni.pridelek` - seštet povprečni pridelek


4. **zivina3** - število različnih vrst glav živine po regijah in letih
- `regija` - ime regije
- `vrsta.zivine` - ime živine
- `leto` - leto popisa
- `stevilo.zivine` - število živali

5. **zemljisca.in.zivina** - sešteto število živine po regijah in letih ne glede na vrsto in izračunan delež živine na število hektarjev zemljišča 
- `regija` - ime regije
- `leto` - leto popisa
- `skupno.stevilo.zemljisc` - število vseh vrst zemljišč skupaj
- `skupno.stevilo.zivine` - število vseh živali skupaj
- `zivina.na.ha` - razmerje števila živine in hektarjev zemljišč

6. **prodaja.in.potrosnja**- število prodanih živil slovenskih kmetov in končna potrošnja prebivalcev glede na vrsto živila in leto
- `leto` - leto popisa
- `pridelek` - vrsta živila
- `prodaja` - število prodanega živila v kilogramih
- `potrosnja` - število porabljenega živila v kilogramih
- `razmerje` - razmerje med potrošenim in prodanim živilom

7. **samooskrba.in.poraba** - stopnja samooskrbe določenega živila in določena zmožnost samooskrbe z le-tem
- `leto` - leto popisa
- `zivila` - ime živila
- `stopnja.samooskrbe` - stopnja samooskrbe v odstotkih
- `poraba` - poraba živila na prebivalca v kilogramih
- `preskrba` - število kilogramov na prabivalca, ki smo jih zmožni pridelati sami
- `zmoznost` - s funkcijo zapisana zmožnost samooskrbe glede na povprečno število porabljenih kilogramov živila ter število samopreskrbljenih kilogramov

 
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


## Potrebni paketi za R

Za zagon tega vzorca je potrebno namestiti sledeče pakete za R:

* `knitr` - za izdelovanje poročila
* `rmarkdown` - za prevajanje poročila v obliki RMarkdown
* `shiny` - za prikaz spletnega vmesnika
* `readr` - za branje podatkov
* `rvest` - za pobiranje spletnih strani
* `tidyr` - za preoblikovanje podatkov v obliko *tidy data*
* `dplyr` - za delo s podatki
* `gsubfn` - za delo z nizi (čiščenje podatkov)
* `ggplot2` - za izrisovanje grafov
* `tmap` - za risanje na zemljevide
* `tidyverse` - za delo s podatki oblike *tidy data*
* `dbplyr` - za delo s podatki
* `maps` - za uvoz zamljevidov
* `viridis` - za barvne lestvice pri risanju grafov
* `stringr` - za delo z regularnimi izrazi
* `readxl` - za uvoz iz Excel-ove datoteke
* `ggplotly` - za interaktivno delo z grafi
