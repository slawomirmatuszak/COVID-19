library(maptools)
library(rgeos)
library(rgdal)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(ggthemes)
library(scales)
library(tidyr)
library(dplyr)

####################################################################################################
# skrypt do map Covid na Ukrainie
####################################################################################################

## wgrywamy mapę
shp1 <- readOGR("E:/R/UA_spis/data/mapa", layer = "obwod_pl")
#zmieniamy format danych
shp1f <- fortify(shp1, region = "ADM1_PCODE")


# dzienne dane z obwodów i szpitali
load(file = "E:/R/COVID-19/Ukraina.dane/obwody.lista.Rda")
obwody.lista <- obwody %>%
  select(1:3,7)
rm(obwody)

load(file = "E:/R/COVID-19/Ukraina.dane/obwody_dzienne.Rda")
obwody <- obwody %>%
  select(-c(7,8))%>%
  left_join(obwody.lista, by="Kod")%>%
  mutate(lat = as.numeric(lat), long = as.numeric(long))

load(file = "E:/R/COVID-19/Ukraina.dane/szpitale.lozka.Rda")
szpitale.lozka <- szpitale.lozka %>% 
  select(-c(6,7))%>%
  left_join(obwody.lista, by="Kod")%>%
  mutate(lat = as.numeric(lat), long = as.numeric(long))

load(file = "E:/R/COVID-19/Ukraina.dane/szpitale.oblozenie.Rda")
szpitale.lozka <- szpitale.oblozenie %>%
  select(-c(3,4))%>%
  left_join(obwody.lista, by="Kod")%>%
  mutate(lat = as.numeric(lat), long = as.numeric(long))


### przykładowa mapa ze spisu_UA
ggplot() + 
  geom_map(data=szpitale.lozka, aes(map_id=Kod, fill=lozka.10tys), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.001) + 
  coord_map(projection = "mercator") + 
  #geom_text(data=szpitale.lozka, aes(x=long_s, y=lat_s), label = szpitale.lozka$lozka.10tys)
  scale_fill_gradient(low = "deepskyblue2", high = "blue4") +
  # można też geom_label, ale nie ma check_overlap label=paste(round(zmiana$proc_m, 1), "%")
  geom_label(data=szpitale.lozka, aes(x=long, y=lat), label=paste(round(szpitale.lozka$lozka.10tys, 1)))


  # próbujemy dodać tytuł do wykresu. 
  labs(fill = "Ludność\nw mln") +
  
  temat 
