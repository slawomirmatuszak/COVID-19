library(maptools)
library(rgeos)
library(rgdal)
library(ggplot2)
library(stringr)
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

## pobieramy dane o obwodach
obwody <- read.csv("E:/R/UA_spis/data/obwody.csv", encoding = "UTF-8")
names(obwody)[1] <- "id"
names(obwody)[2] <- "Область"
# usuwamy niepotrzebne kolumny
obwody <- tbl_df(obwody)
obwody <- select(obwody, -c(11:17))



### przykładowa mapa ze spisu_UA
ggplot() + 
  ggtitle("Ilość mieszkańców wg. spisu Dubileta") +
  geom_map(data=spis, aes(map_id=id, fill=r.2020.dubilet/1000000), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.01) + 
  coord_map(projection = "mercator") + 
  scale_fill_gradient(low = "deepskyblue2", high = "blue4") +
  
  # można też geom_label, ale nie ma check_overlap label=paste(round(zmiana$proc_m, 1), "%")
  geom_label(data=spis, aes(x=long, y=lat), label=paste(round(spis$`r.2020.dubilet`/1000000,2), "mln"), nudge_y=-0.1, size=5) +
  # próbujemy dodać tytuł do wykresu. 
  labs(fill = "Ludność\nw mln") +
  
  temat 
