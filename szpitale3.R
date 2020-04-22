library(data.table)
library(tidyr)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(scales)

################################################################################################## 
#skrypt do obróbki danych o szpitalach.

shp1 <- readOGR("E:/R/UA_spis/data/mapa", layer = "obwod_pl")
#zmieniamy format danych
shp1f <- fortify(shp1, region = "ADM1_PCODE")

load(file = "E:/R/COVID-19/Ukraina.dane/obwody.lista.Rda")
obwody.lista <- obwody %>%
  # trzeba poprawic współrzędne Kijowa, bo nie widać go na mapie
  select(1:3,6,7)
obwody.lista[13,1] <- 50.777435
obwody.lista[13,2] <- 30.167475
rm(obwody)

szpitale <- read.csv("https://covid19.gov.ua/csv/data.csv", encoding = "UTF-8")

szpitale <- szpitale %>%
  select(4, 5, 26:31,33:41)%>%
  rename(id=1, data=2, lozka.suma=3, infekcyjne=4, infekcyjne.zajete=5, reanimacyjne=6, reanimacyjne.zajete=7,
         lozka.zajete=8, covid=9, resp.wys=10, resp.wys.podl=11, resp.sr=12, resp.sr.podl=13, resp.dzieci=14,
         resp.dzieci.podl=15, resp.przenosne=16, resp.zepsute=17)%>%
  separate(col=data, into=c("data", NA), sep=" ")%>%
  mutate(data=dmy(data), id=gsub("м.Київ", "м. Київ", id))%>%
  filter(data!=Sys.Date())%>%
  group_by(id, data)%>%
  summarise_all(funs(sum))%>%
  arrange(id, data)%>%
  unite(col = "id2", data, id, sep="_", remove = F)

szpitale <- left_join(szpitale, obwody.lista, by="id")%>%
  ungroup()%>%
  select(-c(2:3,22))

load(file = "E:/R/COVID-19/Ukraina.dane/obwody_dzienne.Rda")
obwody <- obwody %>%
  unite(col = "id2", data, id, sep="_", remove = F)%>%
  filter(dzienne=="aktywni")

szpitale <- left_join(obwody, szpitale, by="id2")%>%
  mutate(lat = as.numeric(lat), long = as.numeric(long))

# obliczamy procent hospitalizowanych
a <- szpitale %>%
  mutate(proc.hosp=round(covid/liczba, 3))%>%
  filter(data==max(data))

ggplot() + 
  geom_map(data=a, aes(map_id=Kod, fill=proc.hosp), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.001) + 
  coord_map(projection = "mercator") + 
  scale_fill_gradient(low = "white", high = "orange", labels = percent) +
  labs(fill= "", title = "Odsetek hospitalizowanych przypadków zakażonych",
       #subtitle =  paste0( "stan na ", format(as.Date(a$data2), "%d/%m/%Y"), ", godz. 9.00"),
       caption = "Źródło - Ministerstwo Zdrowia Ukrainy") +
  geom_label(data=a, aes(x=long, y=lat), label=paste0(a$proc.hosp*100, "%"), size=3) +
  theme_bw()+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = c(0.1, 0.2),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), plot.background = element_rect(colour = "grey", size = 0.5), 
        plot.caption = element_text(size = 8))

# proc przyrost w ciągu tygodnia. 
load(file = "E:/R/COVID-19/Ukraina.dane/obwody_dzienne.Rda")
a <- obwody %>%
  filter(data== max(data)|data== max(data)-7)%>%
  group_by(Obwód, Kod, data)%>%
  summarise(
    liczba = sum(liczba)
  )%>%
  pivot_wider(names_from = data, values_from = liczba)%>%
  rename(min=3, max=4)%>%
  mutate(roznica = (max/min-1))%>%
  mutate(roznica=round(roznica, 3))%>%
  left_join(select(obwody.lista, c(1:2,5)), by="Kod")%>%
  mutate(lat = as.numeric(lat), long = as.numeric(long))

ggplot() + 
  geom_map(data=a, aes(map_id=Kod, fill=roznica), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.001) + 
  coord_map(projection = "mercator") + 
  scale_fill_gradient(low = "white", high = "orange", labels = percent) +
  labs(fill= "", title = "Przyrost zakażeń w ciągu 7 dni",
       #subtitle =  paste0( "stan na ", format(as.Date(a$data2), "%d/%m/%Y"), ", godz. 9.00"),
       caption = "Źródło - Ministerstwo Zdrowia Ukrainy") +
  geom_label(data=a, aes(x=long, y=lat), label=paste0(a$roznica*100, "%"), size=3) +
  theme_bw()+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = c(0.1, 0.2),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), plot.background = element_rect(colour = "grey", size = 0.5), 
        plot.caption = element_text(size = 8))

# proc przyrost w porównaniu do porzedniego dnia.

a <- obwody %>%
  filter(data== max(data)|data== max(data)-1)%>%
  group_by(Obwód, Kod, data)%>%
  summarise(
    liczba = sum(liczba)
  )%>%
  pivot_wider(names_from = data, values_from = liczba)%>%
  rename(min=3, max=4)%>%
  mutate(roznica = (max/min-1))%>%
  mutate(roznica=round(roznica, 3))%>%
  left_join(select(obwody.lista, c(1:2,5)), by="Kod")%>%
  mutate(lat = as.numeric(lat), long = as.numeric(long))

png("./Ukraina.dane/wykresy/wykres1.png", units="in", width=9, height=7, res=600)
ggplot() + 
  geom_map(data=a, aes(map_id=Kod, fill=roznica), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.5) + 
  coord_map(projection = "mercator") + 
  scale_fill_gradient(low = "white", high = "orange", labels = percent_format(accuracy = 1)) +
  labs(fill= "", title = "Przyrost zakażeń od wczoraj",
       #subtitle =  paste0( "stan na ", format(as.Date(a$data2), "%d/%m/%Y"), ", godz. 9.00"),
       caption = "Źródło - Ministerstwo Zdrowia Ukrainy") +
  geom_label(data=a, aes(x=long, y=lat), label=paste0(a$roznica*100, "%"), size=4) +
  theme_bw()+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = c(0.1, 0.2),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), plot.background = element_rect(colour = "grey", size = 0.5), 
        plot.caption = element_text(size = 8))
dev.off()

#Porównanie łóżek do chorych
library(forcats)

a <- szpitale %>%
  filter(data==max(data))%>%
  select(4,16,21)%>%
  mutate(Obwód=as.factor(Obwód))%>%
  mutate(Obwód=fct_reorder(Obwód, -covid))%>%
  pivot_longer(cols = c(2:3), names_to = "lozka", values_to = "liczba")%>%
  mutate(lozka=as.factor(lozka))

png("./Ukraina.dane/wykresy/wykres2.png", units="in", width=9, height=5, res=600)
ggplot(a)+
  geom_bar(aes(x=Obwód, y=liczba, fill=lozka), stat = "identity", position = position_dodge())+
  labs(x="", y="", fill="")+
  #scale_fill_discrete(labels = c("pacjenci \nz Covid", "łóżka\ninfekcyjne"))+
  scale_fill_manual(values = c("infekcyjne"="orange", "covid"="red"), labels = c("pacjenci \nz Covid", "łóżka\ninfekcyjne"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))
  dev.off()
  
a <- szpitale %>%
  filter(data==max(data))%>%
  select(4,21,22,24,26,28)%>%
  mutate(Obwód=as.factor(Obwód))%>%
  pivot_longer(cols = c(3:6), names_to = "respiratory", values_to = "liczba")
  

ggplot(a)+
  geom_bar(aes(x=reorder(x=Obwód, -liczba), y=liczba, fill=respiratory ), stat = "identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

################################################################################################
szpitale2 <- read.csv("https://raw.githubusercontent.com/VasiaPiven/covid19_ua/master/dataset.csv", encoding = "UTF-8")

a <- szpitale2 %>%
  mutate(data=ymd(zvit_date))%>%
  group_by(person_gender, person_age_group, add_conditions)%>%
  summarise_if(is.numeric, funs(sum))%>%
  mutate(smiertelnosc = new_death/new_confirm)%>%
  filter(new_death>0)
         
# zgony z podziałem na wiek i płeć
ggplot(filter(a, new_death>0))+
  geom_bar(aes(x=person_age_group, y=new_death, fill=person_gender), 
           stat="identity", position = position_dodge(preserve = 'single'))+
  geom_label(data=a, aes(x=person_age_group, y=new_death, label=new_death, 
                         fill=person_gender), position = position_dodge(width = .8), show.legend = FALSE)+
  facet_wrap(~add_conditions, ncol = 1)+
  coord_flip()+
  theme_bw()

# śmiertelność z podziałem na wiek, płeć itp. 
png("./Ukraina.dane/wykresy/wykres3.png", units="in", width=9, height=5, res=600)
ggplot(a)+
  geom_bar(aes(x=person_age_group, y=smiertelnosc, fill=person_gender), 
           stat="identity", position = position_dodge(preserve = 'single'))+
  facet_wrap(~add_conditions, ncol = 1)+
  scale_y_continuous(labels = percent_format(accuracy = 1))+
  geom_label(data=a, aes(x=person_age_group, y=smiertelnosc, label=paste0(round(smiertelnosc*100,1), "%"), 
                         fill=person_gender), position = position_dodge(width = 0.8), show.legend = FALSE)+
  coord_flip()+
  theme_bw()+
  theme(legend.position = "bottom")
dev.off()

# łóżka podejrzewani
a <- szpitale2 %>%
  mutate(data=ymd(zvit_date))%>%
  filter(edrpou_hosp !="Самоізоляція")%>%
  group_by(registration_area)%>%
  summarise_if(is.numeric, funs(sum))


ggplot(a)+
  geom_bar(aes(x=reorder(x=registration_area, -new_susp), y=new_susp), stat="identity", color="blue")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))