library(gsheet)
library(tidyverse)
library(lubridate)
library(readxl)



# pobór danych ------------------------------------------------------------

dane <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1Tv6jKMUYdK6ws6SxxAsHVxZbglZfisC8x_HZ1jacmBM/edit#gid=1169869581")

# dane o ludnosci w wojewodztwach i powiatach -----------------------------

lud.woj <- read_xlsx("./Ukraina.dane/POLSKA/powierzchnia_i_ludnosc.xlsx", sheet = 2) %>%
  select(1:3,5)%>%
  rename(Kod=1, wojewodztwo=2, powierzchnia=3, ludnosc=4)%>%
  slice(-c(1:5))%>%
  mutate(wojewodztwo=toupper(wojewodztwo))

nazwy <- lud.woj%>%
  select(wojewodztwo)%>%
  pull()

lud.powiat <- read_xlsx("./Ukraina.dane/POLSKA/powierzchnia_i_ludnosc.xlsx", sheet = 3)%>%
  select(1:3,5)%>%
  rename(Kod=1, wojewodztwo=2, powierzchnia=3, ludnosc=4)%>%
  slice(-c(1:6))%>%
  filter(!is.na(Kod))
  #mutate(Kod2 = if_else(str_detect(Kod, pattern="^0"), str_sub(Kod, start=2, end = 4), Kod))%>%
  #mutate(Kod2 = if_else(nchar(Kod2)==3, paste0(Kod2, "0"), Kod2))

# obróbka danych ----------------------------------------------------------

PL <- dane %>%
  filter(!is.na(Kod))

wojewodztwa <- PL %>%
  filter(Nazwa%in%nazwy)%>%
  mutate_all(as.character)%>%
  pivot_longer(cols = c(4:length(names(PL))), names_to = "data", values_to = "zakazeni")%>%
  mutate(data=paste0(data,".2020"))%>%
  mutate(data=dmy(data))%>%
  rename(wojewodztwo=Nazwa)%>%
  left_join(lud.woj, by="wojewodztwo")%>%
  mutate(zakazeni=as.numeric(zakazeni),
         ludnosc=as.numeric(ludnosc))%>%
  mutate(zach.100.cum=zakazeni*1e5/ludnosc)

powiaty <- PL %>%
  filter(!Nazwa%in%nazwy)%>%
  mutate_all(as.character)%>%
  slice(-1)%>%
  rename(Kod2 = X2)%>%
  mutate(Kod = if_else(nchar(Kod)==6, paste0("0", Kod), Kod))%>%
  mutate(Kod = str_sub(Kod, start=1, end=4))%>%
  pivot_longer(cols = c(4:length(names(PL))), names_to = "data", values_to = "zakazeni")%>%
  mutate(data=paste0(data,".2020"))%>%
  mutate(data=dmy(data))%>%
  left_join(lud.powiat, by="Kod")%>%
  mutate(zakazeni=as.numeric(zakazeni),
         ludnosc=as.numeric(ludnosc))%>%
  mutate(zach.100.cum=zakazeni*1e5/ludnosc)

test <- powiaty %>%
  filter(!is.na(zach.100.cum))%>%
  group_by(Kod)%>%
  filter(data==max(data))%>%
  filter(str_detect(Nazwa, pattern="m[.]")&zakazeni==0)

a <- powiaty %>%
  mutate(zach.100.cum=if_else(Kod=="1062", filter(powiaty, Kod=="1010"&data==max(data))%>%ungroup()%>%select(zach.100.cum)%>%pull(), zach.100.cum))


# case łodzi --------------------------------------------------------------

oficjalne <- 211+173
ludnosc <- 685285
zgony <- 6
odsetek <- oficjalne/ludnosc
smiertelnosc <- zgony/oficjalne

roznica <- 0.137/odsetek
smiertelnosc2 <- zgony/(oficjalne*roznica)

# odsetek*roznica = 13,7

# założenie 2% i 10 razy większa ilość zmarłych

oficjalne <- 211+173
ludnosc <- 685285
zgony <- 6*5
odsetek <- oficjalne/ludnosc
smiertelnosc <- zgony/oficjalne

roznica <- 0.02/odsetek
smiertelnosc2 <- zgony/(oficjalne*roznica)


# case Polski - model -----------------------------------------------------

oficjalne <- 18885
ludnosc <- 38e6
zgony <- 9360
odsetek <- oficjalne/ludnosc
smiertelnosc <- zgony/oficjalne

roznica <- 0.1/odsetek
smiertelnosc2 <- zgony/(oficjalne*roznica)

# mapa powiatów -----------------------------------------------------------

library(maptools)
library(rgeos)
library(rgdal)
library(ggplot2)
library(stringr)
library(gridExtra)
library(tidyverse)
library(ggthemes)
library(scales)

## wgrywamy mapę
shp1 <- readOGR("E:/R/wybory_powiaty/data/Powiaty", layer = "Powiaty")
#zmieniamy format danych
shp1f <- fortify(shp1, region = "JPT_KOD_JE")

#dalsza obróbka

a <- powiaty %>%
  filter(!is.na(zach.100.cum))%>%
  group_by(Kod)%>%
  filter(data==max(data))

piotrków <- filter(a, Kod=="1010"&data==max(data))%>%ungroup()%>%select(zach.100.cum)%>%pull()
skierniewice <- filter(a, Kod=="1015"&data==max(data))%>%ungroup()%>%select(zach.100.cum)%>%pull()
opole <- filter(a, Kod=="1609"&data==max(data))%>%ungroup()%>%select(zach.100.cum)%>%pull()
kielce <- filter(a, Kod=="2604"&data==max(data))%>%ungroup()%>%select(zach.100.cum)%>%pull()
koszalin <-  filter(a, Kod=="3209"&data==max(data))%>%ungroup()%>%select(zach.100.cum)%>%pull()
  
a <- a %>%
  mutate(zach.100.cum=if_else(Kod=="1062", piotrków, zach.100.cum))%>%
  mutate(zach.100.cum=if_else(Kod=="1063", skierniewice, zach.100.cum))%>%
  mutate(zach.100.cum=if_else(Kod=="1661", opole, zach.100.cum))%>%
  mutate(zach.100.cum=if_else(Kod=="2661", kielce, zach.100.cum))%>%
  mutate(zach.100.cum=if_else(Kod=="3261", koszalin, zach.100.cum))%>%
    mutate(mapa = case_when(zach.100.cum>0&zach.100.cum<10 ~ "poniżej 10 osób",
                          zach.100.cum==0 ~ "brak",
                          zach.100.cum>=10&zach.100.cum<50 ~ "10-50 osób",
                          zach.100.cum>=50&zach.100.cum<100 ~ "50-100 osób",
                          zach.100.cum>=100&zach.100.cum<150 ~ "100-150 osób",
                          zach.100.cum>=150&zach.100.cum<300 ~ "150-300 osób",
                          TRUE ~ "powyżej 300"))%>%
  #mutate(mapa= if_else((str_detect(Nazwa, pattern="m[.]")&zakazeni==0), paste("brak danych"),mapa))%>%
  mutate(mapa=as.factor(mapa))

a$mapa <- ordered(a$mapa, levels = c("brak", "poniżej 10 osób", "10-50 osób", "50-100 osób", "100-150 osób","150-300 osób", "powyżej 300"))

png("Polska.Powiaty1.png", units="in", width=9, height=9, res=600)
ggplot(a)+ 
  geom_map(data=a, aes(map_id=Kod, fill=zach.100.cum), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey20", size=0.01) + 
  coord_map(projection = "mercator") + 
  scale_fill_gradient(low = "white", high = "red") +
  labs(fill = "zakażenia\nna 100 tys.")+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = c(0.1, 0.15),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), plot.background = element_rect(colour = "grey", size = 0.5), 
        plot.caption = element_text(size = 8))
dev.off()

png("Polska.Powiaty2.png", units="in", width=9, height=9, res=600)
ggplot(a)+ 
  geom_map(data=a, aes(map_id=Kod, fill=mapa), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey11", size=0.1) + 
  coord_map(projection = "mercator") + 
  scale_fill_brewer(palette = "Spectral", direction = -1)+
  labs(fill = "zakażenia\nna 100 tys.")+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = c(0.1, 0.15),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), plot.background = element_rect(colour = "grey", size = 0.5), 
        plot.caption = element_text(size = 8))
dev.off()


# Powiaty tuckey

test <- a %>%
  ungroup()%>%
  mutate(Kod=str_sub(Kod, start=1, end=2))%>%
  left_join(select(lud.woj, c(1,2)), by="Kod")%>%
  rename(wojewodztwo=wojewodztwo.y)%>%
  mutate(wojewodztwo=tolower(wojewodztwo))%>%
  group_by(wojewodztwo)%>%
  mutate(woj.mediana = median(zach.100.cum))

png("./wykresy/woj.powiaty.png", units="in", width=9, height=5, res=600)
ggplot(test, aes(x=reorder(wojewodztwo, woj.mediana), y=zach.100.cum))+
  geom_boxplot(color="blue")+
  geom_point(color="blue", alpha=0.2)+
  coord_flip()+
  labs(x="", y="poziom zakażeń w powiatach na 100 tys. mieszkańców")+
  theme_bw()
dev.off()

#zoom dla lepszego porównania
png("./wykresy/woj.powiaty.zoom.png", units="in", width=9, height=5, res=600)
ggplot(test, aes(x=reorder(wojewodztwo, woj.mediana), y=zach.100.cum))+
  geom_boxplot(color="blue")+
  geom_point(color="blue", alpha=0.2)+
  coord_flip(ylim = c(0,200))+
  labs(x="", y="poziom zakażeń w powiatach na 100 tys. mieszkańców")+
  theme_bw()
dev.off()

top.10 <- test %>%
  arrange(desc(zach.100.cum))%>%
  select(wojewodztwo, Nazwa, zach.100.cum)%>%
  head(15)

ggplot(test, aes(x=Nazwa, y=zach.100.cum))+
  geom_col()+
  facet_wrap(~wojewodztwo, scales = "free_y")+
  coord_flip()+
  geom_hline(yintercept = median(test$zach.100.cum), size=1.5, color="blue")+
  theme_bw()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

# histogram
ggplot(test, aes(x=zach.100.cum))+
  geom_histogram(binwidth = 10, color="red")+
  geom_vline(xintercept = median(test$zach.100.cum), size=1, color="blue")+
  geom_vline(xintercept = sum(test$zakazeni)*1e5/sum(test$ludnosc), size=1, color="darkgreen")+
  facet_wrap(~wojewodztwo)+
  theme_bw()

ggplot(test, aes(x=ludnosc, y=zach.100.cum))+
  geom_point(color="blue", alpha=0.4)+
  geom_smooth()+
  coord_cartesian(xlim = c(0,25e4))+
  facet_wrap(~wojewodztwo, scales = "free_y")


# mapa wojewodztw ---------------------------------------------------------

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
  
Polska <- ne_states(country = 'poland', returnclass = "sf")%>%
  rename(wojewodztwo=10)%>%
  mutate(wojewodztwo=toupper(wojewodztwo))%>%
  mutate(wojewodztwo = case_when(wojewodztwo=="SLASKIE" ~ "ŚLĄSKIE",
                             wojewodztwo=="MALOPOLSKIE" ~ "MAŁOPOLSKIE",
                             wojewodztwo=="DOLNOSLASKIE" ~ "DOLNOŚLĄSKIE",
                             wojewodztwo=="WARMINSKO-MAZURSKIE" ~ "WARMIŃSKO-MAZURSKIE",
                             wojewodztwo=="LÓDZKIE" ~ "ŁÓDZKIE",
                             wojewodztwo=="SWIETOKRZYSKIE" ~ "ŚWIĘTOKRZYSKIE",
                             TRUE ~ wojewodztwo))


a <- wojewodztwa%>%
  filter(!is.na(zach.100.cum))%>%
  group_by(wojewodztwo)%>%
  filter(data==max(data))

test= left_join(Polska,a,by="wojewodztwo")  

# ilość zakażeń  
  ggplot(data = test) +
  geom_sf(aes(fill=zakazeni)) +
  scale_fill_gradient(low = "white", high = "#FF3300")+
  geom_label(aes(x=longitude, y=latitude), label=test$zakazeni, size=3) +
  labs(fill="liczba \nzakażeń", x="", y="") +
  theme_bw()+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),panel.grid.minor = element_blank(),panel.grid.major = element_blank(), 
        plot.caption = element_text(size = 8),
        plot.background = element_rect(colour = "grey", size = 0.5), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))  ->p1

  #zakażenia na 100 tys.

  ggplot(data = test) +
    geom_sf(aes(fill=zach.100.cum)) +
    scale_fill_gradient(low = "white", high = "#FF3300")+
    geom_label(aes(x=longitude, y=latitude), label=round(test$zach.100.cum,1), size=3) +
    labs(fill="zakażenia na \n 100 tys. mieszkańców", x="", y="") +
    theme_bw()+
    theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.title.x = element_blank(), axis.title.y = element_blank(),panel.grid.minor = element_blank(),panel.grid.major = element_blank(), 
          plot.caption = element_text(size = 8),
          plot.background = element_rect(colour = "grey", size = 0.5), plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))    -> p2
  
grid.arrange(p1,p2, ncol=2)

# testy na ludność --------------------------------------------------------
library(ggrepel)
library(ggthemes)

testy <- wojewodztwa%>%
  group_by(wojewodztwo)%>%
  filter(!is.na(zakazeni))%>%
  filter(data==max(data))

liczba.testow <-  read_xlsx("./Ukraina.dane/POLSKA/testy.xlsx")%>%
  mutate(wojewodztwo=toupper(wojewodztwo))%>%
  left_join(testy, by="wojewodztwo")%>%
  mutate(wojewodztwo=tolower(wojewodztwo))%>%
  mutate(test.mieszkancy = testy*1e5/ludnosc)

png("testy.pl.png", units="in", width=10, height=8, res=600)
ggplot(liczba.testow, aes(x=zach.100.cum, y=test.mieszkancy, label=wojewodztwo))+
  geom_point()+
  geom_label_repel()+
  geom_smooth(method = "lm", se=F)+
  labs(x="zakażenia na 100 tys. mieszkańców",
       y="liczba testów na 100 tys. mieszkańców")+
  theme_economist()
dev.off()


# testy na świecie --------------------------------------------------------
a <- covid.ECDC%>%
  unite(col="id", c("ISO3", "data"), remove = F)%>%
  mutate(zach.100.cum = suma.chorych*100000/population)%>%
  mutate(zach.100 = cases*1e5/population)

testy.world <- read_csv("full-list-cumulative.csv")%>%
  mutate(Date=mdy(Date))%>%
  rename(ISO3=Code, total=4)%>%
  unite(col="id", c("ISO3", "Date"), remove = F)%>%
  left_join(a, by="id")%>%
  filter(Kontynenty=="Europa")%>%
  filter(!Państwo %in%c("Bułgaria", "Hiszpania", "Francja", "Holandia", "Niemcy", "Szwecja", "Irlandia", "Ukraina"))%>%
  filter(zach.100>=0)

kraje <- testy.world%>%
  select(Państwo)%>%
  unique()%>%
  pull()

png("krzywe.pl.png", units="in", width=10, height=8, res=600)
ggplot(testy.world, aes(y=zach.100, x=total))+
  #geom_point(color="blue", alpha=0.5)+
  geom_smooth(color="red", size=2, span=0.4, se=F)+
  facet_wrap(~Państwo, scales="free")+
  labs(x = "ilość testów na 1000 osób",
       y = "liczba dziennych nowych zakażeń na 100 tys. mieszkańców")+
  theme_minimal()->p2
dev.off()

b <- covid.ECDC%>%
  mutate(zach.100 = cases*1e5/population)%>%
  mutate(srednia= zoo::rollmean(zach.100, k=7, fill=NA, align="right"))%>%
  filter(Kontynenty=="Europa")%>%
  filter(Państwo %in%kraje)%>%
  filter(srednia>0.1)%>%
  mutate(id=row_number())
  

ggplot(b, aes(x=id, y=srednia))+
  #geom_point(color="blue", alpha=0.5)+
  geom_smooth(color="blue", size=2, span=0.4, se=F)+
  facet_wrap(~Państwo, scales="free")+
  labs(x = "ilość dni od przekroczenia poziomu 0,1 zakażenia na 100 tys. mieszkańców",
       y = "liczba dziennych nowych zakażeń na 100 tys. mieszkańców")+
  theme_minimal() ->p1

png("testy.zachorowania.png", units="in", width=15, height=12, res=600)
grid.arrange(p1,p2, ncol=2)
dev.off()
