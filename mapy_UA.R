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
  # trzeba poprawic współrzędne Kijowa, bo nie widać go na mapie
  select(1:3,7)
obwody.lista[13,1] <- 50.777435
obwody.lista[13,2] <- 30.167475
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
szpitale.oblozenie <- szpitale.oblozenie %>%
  select(-c(3,4))%>%
  left_join(obwody.lista, by="Kod")%>%
  mutate(lat = as.numeric(lat), long = as.numeric(long))

# procent zapełnienia łóżek
# do poprawienia
a <- szpitale.oblozenie %>%
  group_by(Obwód)%>%
  summarise(
    lozka = sum(filter(stan=="wolne łóżka"))+sum(filter(stan=="chorzy"))
  )

### liczba łóżek na 10 tys
ggplot() + 
  geom_map(data=szpitale.lozka, aes(map_id=Kod, fill=lozka.10tys), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.001) + 
  coord_map(projection = "mercator") + 
  scale_fill_gradient(low = "deepskyblue2", high = "blue4") +
  labs(fill= "liczba \nłóżek", title = "Liczba przystosowanych łóżek na 10 tysięcy mieszkańców",
       subtitle = "stan na 26 marca") +
  geom_label(data=szpitale.lozka, aes(x=long, y=lat), label=paste(round(szpitale.lozka$lozka.10tys, 1))) +
  theme_bw()+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
         axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = c(0.1, 0.2),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# liczba chorych
a <- obwody %>%
  filter(data== max(data)) %>%
  group_by(Obwód, Kod, data)%>%
  summarise(
    liczba = sum(liczba),
    data2 = max(data)+1
  )

ggplot() + 
  geom_map(data=a, aes(map_id=Kod, fill=liczba), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.001) + 
  coord_map(projection = "mercator") + 
  scale_fill_gradient(low = "white", high = "orange") +
  labs(fill= "liczba \nzarażeń", title = "Liczba potwierdzonych przypadków Covid-19",
       subtitle =  paste( "stan na", format(as.Date(a$data2), "%d/%m/%Y"))) +
  geom_label(data=szpitale.lozka, aes(x=long, y=lat), label=a$liczba) +
  theme_bw()+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = c(0.1, 0.2),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# liczba zgonóW
a <- obwody %>%
  filter(data== max(data)) %>%
  filter(skumulowane == "zgony") %>%
  group_by(Obwód, Kod, data)%>%
  summarise(
    liczba = sum(liczba),
    data2 = max(data)+1
  )

ggplot() + 
  geom_map(data=a, aes(map_id=Kod, fill=liczba), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.001) + 
  coord_map(projection = "mercator") + 
  scale_fill_gradient(low = "white", high = "red") +
  labs(fill= "liczba \nzgonów", title = "Liczba zgonów z powodu Covid-19",
       subtitle =  paste( "stan na", format(as.Date(a$data2), "%d/%m/%Y")),
       caption = "Źródło - Ministerstwo Zdrowia Ukrainy") +
  geom_label(data=szpitale.lozka, aes(x=long, y=lat), label=a$liczba) +
  theme_bw()+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = c(0.1, 0.2),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# nowe przypadki
a <- obwody %>%
  filter(data== max(data)) %>%
  group_by(Obwód, Kod, data)%>%
  summarise(
    ilosc = sum(ilosc),
    data2 = max(data)+1
  )

ggplot() + 
  geom_map(data=a, aes(map_id=Kod, fill=ilosc), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.001) + 
  coord_map(projection = "mercator") + 
  scale_fill_gradient(low = "white", high = "orange") +
  labs(fill= "liczba nowych\nprzypadków", title = "Liczba nowych przypadków Covid-19 w ciągu ostatniej doby",
       subtitle =  paste( "stan na", format(as.Date(a$data2), "%d/%m/%Y")),
       caption = "Źródło - Ministerstwo Zdrowia Ukrainy") +
  geom_label(data=szpitale.lozka, aes(x=long, y=lat), label=a$ilosc) +
  theme_bw()+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = c(0.1, 0.2),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

## liczba przypadków na 100 tys mieszkańców
a <- obwody %>%
  filter(data== max(data)) %>%
  group_by(Obwód, Kod, data, population)%>%
  summarise(
    liczba = sum(liczba),
    data2 = max(data)+1
  ) %>%
  mutate(zach.100 = liczba*100000/population)%>%
  mutate(zach.100 = round(zach.100, 2))

png("100tys.png", units="in", width=11, height=6, res=600)
ggplot() + 
  geom_map(data=a, aes(map_id=Kod, fill=zach.100), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="black", size=0.001) + 
  coord_map(projection = "mercator") + 
  scale_fill_gradient(low = "white", high = "orange") +
  labs(fill= "liczba\nzarażonych", 
       title = "Liczba zarażonych Covid-19 na 100 tys. mieszkańców",
       subtitle =  paste( "stan na", format(as.Date(a$data2), "%d/%m/%Y")),
       caption = "Źródło - Ministerstwo Zdrowia Ukrainy, liczba ludności wg tzw. spisu Dubileta") +
  geom_label(data=szpitale.lozka, aes(x=long, y=lat), label=a$zach.100) +
  theme_bw()+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = c(0.1, 0.2),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
dev.off()
