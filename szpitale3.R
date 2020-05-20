library(data.table)
library(tidyr)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(scales)
library(ggrepel)
library(gridExtra)

################################################################################################## 
################################################################################################## 
# skrypt do obróbki danych o szpitalach.

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
  summarise_all(sum, na.rm=T)%>%
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

save(szpitale, file = "szpitale.respiratory.Rda")

#####################################################################################################
#####################################################################################################

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

## izolacja vs szpitale
a <- szpitale.obwody %>%
  group_by(izolacja)%>%
  summarise(
    liczba = sum(new_confirm)-sum(new_recover)-sum(new_death)
  )

ggplot(a, aes(x="", y=liczba, fill=izolacja)) +
  geom_col() +
  geom_label(aes(label = liczba), 
             position = position_stack(vjust = 0.5), show.legend = FALSE, color="white", fontface='bold')+
  coord_polar(theta = "y") + 
  labs(fill="", y="",x="", title = "izolacja")+
  theme_bw()+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")


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

#png("./Ukraina.dane/wykresy/wykres1.png", units="in", width=9, height=7, res=600)
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
#dev.off()

#Porównanie łóżek do chorych
library(forcats)

a <- szpitale %>%
  filter(data==max(data))%>%
  select(4,16,21)%>%
  mutate(Obwód=as.factor(Obwód))%>%
  mutate(Obwód=fct_reorder(Obwód, -covid))%>%
  pivot_longer(cols = c(2:3), names_to = "lozka", values_to = "liczba")%>%
  mutate(lozka=as.factor(lozka))

#png("./Ukraina.dane/wykresy/wykres2.png", units="in", width=9, height=5, res=600)
ggplot(a)+
  geom_bar(aes(x=Obwód, y=liczba, fill=lozka), stat = "identity", position = position_dodge())+
  labs(x="", y="", fill="")+
  scale_fill_manual(values = c("infekcyjne"="orange", "covid"="red"), labels = c("pacjenci \nz Covid", "łóżka\ninfekcyjne"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))
  #dev.off()

#Porównanie łóżek do chorych i podejrzanych - nowi podejrzani z ostatnich 14 dni

b <- szpitale.obwody %>%
  ungroup()%>%
  filter(izolacja=="szpital")%>%
  filter(data>max(data)-4)%>%
  group_by(Obwód)%>%
  summarise(
    liczba.podejrzani = sum(new_susp)
  )%>%
  mutate(lozka="podejrzewani", Obwód = gsub("iwanofrakiwski", "iwanofrankiwski", Obwód),
         Obwód = gsub("dniepropietrowski", "dniepropetrowski", Obwód),
         Obwód = gsub("łuhański", "ługański", Obwód))

c <- a%>%
  filter(lozka=="covid")%>%
  left_join(b, by="Obwód")%>%
  mutate(liczba=liczba+liczba.podejrzani)%>%
  select(1:3)%>%
  rename(lozka=2)%>%
  mutate(Obwód=fct_reorder(Obwód, -liczba))%>%
  bind_rows(filter(a, lozka=="infekcyjne"))

ggplot(c)+
  geom_bar(aes(x=Obwód, y=liczba, fill=lozka), stat = "identity", position = position_dodge())+
  labs(x="", y="", fill="")+
  scale_fill_manual(values = c("infekcyjne"="orange", "covid"="red"), labels = c("pacjenci z Covid\ni podejrzeniem o Covid", "łóżka\ninfekcyjne"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

### respiratory
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
################################################################################################
szpitale2 <- read.csv("https://raw.githubusercontent.com/VasiaPiven/covid19_ua/master/dataset.csv", encoding = "UTF-8")

szpitale.suma <- szpitale2 %>%
  mutate(data=ymd(zvit_date))%>%
  group_by(person_gender, person_age_group, add_conditions)%>%
  summarise_if(is.numeric, funs(sum))%>%
  ungroup()%>%
  mutate(smiertelnosc = new_death/new_confirm)%>%
  mutate(add_conditions = gsub("Так", "choroby współistniejące", add_conditions))%>%
  mutate(add_conditions = gsub("Ні", "brak chorób współistniejących", add_conditions))%>%
  mutate(person_gender = gsub("Жіноча", "kobiety", person_gender))%>%
  mutate(person_gender = gsub("Чоловіча", "mężczyźni", person_gender))%>%
  mutate(person_gender = gsub("Уточнюється", "b.d.", person_gender))%>%
  mutate(person_age_group = gsub("Уточнюється", "b.d.", person_age_group))

save(szpitale.suma, file = "szpitale.suma.github.Rda")

# obróbka z podziałem na obwody

load(file = "E:/R/COVID-19/Ukraina.dane/obwody.lista.Rda")
obwody.lista <- obwody %>%
  # trzeba poprawic współrzędne Kijowa, bo nie widać go na mapie
  select(1:4,6,7)
obwody.lista[13,1] <- 50.777435
obwody.lista[13,2] <- 30.167475
rm(obwody)

szpitale.obwody <- szpitale2 %>%
  mutate(izolacja= if_else(edrpou_hosp=="Самоізоляція", paste("izolacja"), paste("szpital")))%>%
  group_by(zvit_date, registration_area, izolacja)%>%
  summarise_if(is.numeric, funs(sum))%>%
  rename(id=2)%>%
  left_join(obwody.lista, by="id")%>%
  mutate(lat = as.numeric(lat), long = as.numeric(long))%>%
  mutate(data=ymd(zvit_date))

save(szpitale.obwody, file = "szpitale.obwody.Rda")

#trzeba potem dodać pracowników medycznych
load(file = "E:/R/COVID-19/Ukraina.dane/obwody.lista.Rda")

obwody.lista <- obwody %>%
  # trzeba poprawic współrzędne Kijowa, bo nie widać go na mapie
  select(1:4,6,7)
obwody.lista[13,1] <- 50.777435
obwody.lista[13,2] <- 30.167475
rm(obwody)

szpitale.medycy <- szpitale2 %>%
  mutate(izolacja= if_else(edrpou_hosp=="Самоізоляція", paste("izolacja"), paste("szpital")))%>%
  mutate(medycy= if_else(is_medical_worker=="Так", paste("tak"), paste("nie")))%>%
  group_by(zvit_date, registration_area, izolacja, medycy)%>%
  summarise_if(is.numeric, sum)%>%
  rename(id=2)%>%
  left_join(obwody.lista, by="id")%>%
  mutate(lat = as.numeric(lat), long = as.numeric(long))%>%
  mutate(data=ymd(zvit_date))%>%
  ungroup()

save(szpitale.medycy, file = "szpitale.medycy.Rda")


################################################################################################## 
################################################################################################## 

# zgony z podziałem na wiek i płeć
a <- szpitale.suma %>%
  filter(new_death>0)
         
ggplot(a)+
  geom_bar(aes(x=person_age_group, y=new_death, fill=person_gender), 
           stat="identity", position = position_dodge(preserve = 'single'))+
  geom_label(data=a, aes(x=person_age_group, y=new_death, label=new_death, 
                         fill=person_gender), position = position_dodge(width = .8), show.legend = FALSE)+
  facet_wrap(~add_conditions, ncol = 1)+
  labs(fill="", x="", y="liczba zgonów")+
  coord_flip()+
  theme_bw()+
  theme(legend.position = "bottom")

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
  labs(fill="", y="śmiertelność", x="")+
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
  geom_bar(aes(x=reorder(x=registration_area, -new_susp), y=new_susp), stat="identity", fill="blue")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

#####################################################################################################
#porównnie zarażonych i zgonóW
a <- szpitale.suma%>%
  group_by(person_age_group)%>%
  summarise(
    zgony=sum(new_death),
    zarazeni=sum(new_confirm)
  )%>%
  filter(person_age_group!="b.d.")

kolory <- c("0-5"="red4", "06-17"="royalblue4", "18-39"="coral3", "40-64"="goldenrod4", "65+"="yellowgreen")

p1 <- ggplot(a, aes(x="", y=zarazeni, fill=person_age_group)) +
  geom_col() +
  geom_label_repel(aes(label = paste0(round(zarazeni/sum(zarazeni)*100), "%")), 
            position = position_stack(vjust = 0.5), show.legend = FALSE, color="white",fontface='bold')+
  coord_polar(theta = "y") + 
  scale_fill_manual(values = kolory)+
  labs(fill="", y="",x="", title = "Zarażeni")+
  theme_bw()+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5))

a <- szpitale.suma%>%
  group_by(person_age_group)%>%
  summarise(
    zgony=sum(new_death),
    zarazeni=sum(new_confirm)
  )%>%
  filter(person_age_group!="b.d.")%>%
  filter(zgony>0)

p2 <- ggplot(a, aes(x="", y=zgony, fill=person_age_group)) +
  geom_col() +
  geom_label_repel(aes(label = paste0(round(zgony/sum(zgony)*100), "%")), 
                   position = position_stack(vjust = 0.5), show.legend = FALSE, color="white", fontface='bold')+
  coord_polar(theta = "y") + 
  labs(fill="", y="",x="", title = "Zgony")+
  scale_fill_manual(values = kolory)+
  theme_bw()+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5))

grid.arrange(p1,p2, ncol=2)

# porównanie zgonów ze współistniejącymi
b <- szpitale.suma%>%
  group_by(add_conditions)%>%
  summarise(
    zgony=sum(new_death),
    zarazeni=sum(new_confirm)
  )

ggplot(b, aes(x="", y=zgony, fill=add_conditions)) +
  geom_col() +
  geom_label(aes(label = zgony), 
                   position = position_stack(vjust = 0.5), show.legend = FALSE, color="white", fontface='bold')+
  coord_polar(theta = "y") + 
  labs(fill="", y="",x="", title = "Zgony")+
  theme_bw()+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

### pracownicy medyczni ogółem

a <- szpitale2%>%
  group_by(is_medical_worker)%>%
  summarise_if(is.numeric, funs(sum))

ggplot(a, aes(x="", y=new_confirm, fill=is_medical_worker)) +
  geom_col() +
  geom_label(aes(label = new_confirm), 
             position = position_stack(vjust = 0.5), show.legend = FALSE, color="white", fontface='bold')+
  coord_polar(theta = "y") + 
  labs(fill="", y="",x="", title = "Zgony")+
  theme_bw()+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

# pracownicy obwody
b <- szpitale2%>%
  group_by(registration_area, is_medical_worker)%>%
  summarise_if(is.numeric, funs(sum))

png("./Ukraina.dane/wykresy/wykres3.png", units="in", width=10, height=15, res=600)
ggplot(b, aes(x=registration_area, y=new_confirm, fill=is_medical_worker)) +
  geom_col(position = position_dodge()) +
  #geom_label(aes(label = new_confirm), 
             #position = position_stack(vjust = 0.5), show.legend = FALSE, color="white", fontface='bold')+
  labs(fill="", y="",x="", title = "Zgony")+
  #facet_wrap(~registration_area, ncol=3)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90),  plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")
dev.off()

## odsetek hospitalizowanych

a <- szpitale.obwody%>%
  filter(izolacja=="szpital")%>%
  group_by(Obwód, Kod, long, lat)%>%
  summarise_if(is.numeric, funs(sum))%>%
  mutate(aktywni.szpital=new_confirm-new_death-new_recover)%>%
  select(1:4, 10)

b <- szpitale.obwody%>%
  filter(izolacja=="izolacja")%>%
  group_by(Obwód, Kod, long, lat)%>%
  summarise_if(is.numeric, funs(sum))%>%
  mutate(aktywni.iz=new_confirm-new_death-new_recover)%>%
  ungroup()%>%
  select(2, 10)%>%
  left_join(a, by="Kod")%>%
  mutate(suma=aktywni.iz+aktywni.szpital)%>%
  mutate(proc.hosp = aktywni.szpital/suma)%>%
  mutate(proc.hosp = if_else(proc.hosp<0, 0, proc.hosp))

png("./Ukraina.dane/wykresy/hospitalizacja.png", units="in", width=9, height=7, res=600)
ggplot() + 
  geom_map(data=b, aes(map_id=Kod, fill=proc.hosp), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.5) + 
  coord_map(projection = "mercator") + 
  scale_fill_gradient(low = "white", high = "orange", labels=percent_format(accuracy = 1)) +
  labs(fill= "", title = "Odsetek hospitalizowanych, wśród aktywnych przypadków SARS-CoV-2 ",
       #subtitle =  paste0( "stan na ", format(as.Date(a$data2), "%d/%m/%Y"), ", godz. 9.00"),
       caption = "Źródło - Ministerstwo Zdrowia Ukrainy") +
  geom_label(data=b, aes(x=long, y=lat), label=paste0(round(b$proc.hosp*100,1), "%"), size=4) +
  theme_bw()+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = c(0.1, 0.2),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), plot.background = element_rect(colour = "grey", size = 0.5), 
        plot.caption = element_text(size = 8))
dev.off()

##########################################################################################################
##########################################################################################################

# szpitale medycy (liczba chorych)
b <- szpitale.medycy%>%
  filter(medycy=="tak")%>%
  group_by(Obwód,Kod, medycy, long,lat)%>%
  summarise(
    aktywni = sum(new_confirm)-sum(new_recover)-sum(new_death),
    zgony = sum(new_death),
    wyleczeni = sum(new_recover)
  )%>%
  arrange(desc(aktywni))

a<- szpitale.medycy%>%
  group_by(Obwód,Kod, medycy, long,lat)%>%
  summarise(
    aktywni = sum(new_confirm)-sum(new_recover)-sum(new_death),
    zgony = sum(new_death),
    wyleczeni = sum(new_recover)
  ) %>%
  ungroup()

a$Obwód <- ordered(a$Obwód, levels = b$Obwód)

# pacjenci w obwodach - medycy vs niemedycy

ggplot(a)+
  geom_bar(aes(x=Obwód, y=aktywni, fill=medycy), stat = "identity", position = position_dodge())+
  labs(x="", y="", fill="", 
       title = "Ilość przypadków SARS-CoV-2 wśród pracowników medycznych w obwodach",
       caption = "Źródło: Ministerstwo Zdrowia Ukrainy")+
  scale_fill_manual(values = c("tak"="red4", "nie"="blue4"), labels = c("pozostali\npacjenci", "pracownicy\nmedyczni"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom", plot.title = element_text(hjust = 0.5))

#mapa z liczbą medyków

a <- filter(a, medycy=="tak")

ggplot() + 
  geom_map(data=a, aes(map_id=Kod, fill=aktywni), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.5) + 
  coord_map(projection = "mercator") + 
  scale_fill_gradient(low = "white", high = "red4") +
  labs(fill= "", title = "Liczba przypadków SARS-CoV-2 wśród pracowników medycznych",
       #subtitle =  paste0( "stan na ", format(as.Date(a$data2), "%d/%m/%Y"), ", godz. 9.00"),
       caption = "Źródło - Ministerstwo Zdrowia Ukrainy") +
  geom_label(data=a, aes(x=long, y=lat), label=a$aktywni, size=3) +
  theme_bw()+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = c(0.1, 0.2),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), plot.background = element_rect(colour = "grey", size = 0.5), 
        plot.caption = element_text(size = 8))

# wykres udziału personelu medycznego w stosunku do zarażonych


a<- szpitale.medycy%>%
  group_by(Obwód,Kod, medycy, long,lat)%>%
  summarise(
    liczba = sum(new_confirm)-sum(new_recover)-sum(new_death)
  )%>%
  ungroup()

a1 <- a %>%
  filter(medycy=="tak")
a2 <- a %>%
  filter(medycy=="nie")%>%
  select(liczba)%>%
  rename(pacjenci=liczba)

a3 <- cbind(a1, a2)

b <- a3 %>%
  mutate(suma=pacjenci+liczba)%>%
  mutate(odsetek = liczba/suma)%>%
  mutate(odsetek = round(odsetek,3))


ggplot() + 
  geom_map(data=b, aes(map_id=Kod, fill=odsetek), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.5) + 
  coord_map(projection = "mercator") + 
  scale_fill_gradient(low = "white", high = "orange", labels = percent_format(accuracy = 1)) +
  labs(fill= "", title = "Odsetek pracowników medycznych wśród przypadków SARS-CoV-2",
       #subtitle =  paste0( "stan na ", format(as.Date(a$data2), "%d/%m/%Y"), ", godz. 9.00"),
       caption = "Źródło - Ministerstwo Zdrowia Ukrainy") +
  geom_label(data=b, aes(x=long, y=lat), label=paste0(b$odsetek*100,"%"), size=3) +
  theme_bw()+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = c(0.1, 0.2),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), plot.background = element_rect(colour = "grey", size = 0.5), 
        plot.caption = element_text(size = 8))

#########################################################################################################################
# porównanie wieku w szpitalach
izolacja.szpital <- szpitale2 %>%
  mutate(data=ymd(zvit_date))%>%
  mutate(izolacja = if_else(edrpou_hosp== "Самоізоляція", paste("samoizolacja"), paste("szpital")))%>%
  group_by(person_age_group, add_conditions, izolacja)%>%
  summarise_if(is.numeric, funs(sum))%>%
  ungroup()%>%
  mutate(smiertelnosc = new_death/new_confirm)%>%
  mutate(add_conditions = gsub("Так", "choroby współistniejące", add_conditions))%>%
  mutate(add_conditions = gsub("Ні", "brak chorób współistniejących", add_conditions))%>%
  #mutate(person_gender = gsub("Жіноча", "kobiety", person_gender))%>%
  #mutate(person_gender = gsub("Чоловіча", "mężczyźni", person_gender))%>%
  #mutate(person_gender = gsub("Уточнюється", "b.d.", person_gender))%>%
  mutate(person_age_group = gsub("Уточнюється", "b.d.", person_age_group))

odsetek <- izolacja.szpital%>%
  mutate(odsetek = new_confirm/sum(izolacja.szpital$new_confirm))

# pierwszy wariant, lae mało czytelny
ggplot(odsetek, aes(x=person_age_group, y=odsetek))+
  geom_col()+
  facet_wrap(izolacja~add_conditions)

#Drugi wariant

a <- izolacja.szpital%>%
  filter(izolacja=="samoizolacja")%>%
  filter(add_conditions == "brak chorób współistniejących")%>%
  filter(person_age_group!="b.d.")

b <- izolacja.szpital%>%
  filter(izolacja=="szpital")%>%
  filter(add_conditions == "brak chorób współistniejących")%>%
  filter(person_age_group!="b.d.")

c <- izolacja.szpital%>%
  filter(izolacja=="samoizolacja")%>%
  filter(add_conditions == "choroby współistniejące")%>%
  filter(person_age_group!="b.d.")

d <- izolacja.szpital%>%
  filter(izolacja=="szpital")%>%
  filter(add_conditions == "choroby współistniejące")%>%
  filter(person_age_group!="b.d.")

kolory <- c("0-5"="red4", "06-17"="royalblue4", "18-39"="coral3", "40-64"="goldenrod4", "65+"="yellowgreen")

p1 <- ggplot(a, aes(x="", y=new_confirm, fill=person_age_group)) +
  geom_col() +
  #geom_label_repel(aes(label = paste0(round(zarazeni/sum(zarazeni)*100), "%")), 
                   #position = position_stack(vjust = 0.5), show.legend = FALSE, color="white",fontface='bold')+
  coord_polar(theta = "y") + 
  scale_fill_manual(values = kolory)+
  labs(fill="", y="",x="", title = "samoizolacja, bez chorób", caption = "")+
  theme_bw()+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

p2 <- ggplot(b, aes(x="", y=new_confirm, fill=person_age_group)) +
  geom_col() +
  #geom_label_repel(aes(label = paste0(round(zarazeni/sum(zarazeni)*100), "%")), 
  #position = position_stack(vjust = 0.5), show.legend = FALSE, color="white",fontface='bold')+
  coord_polar(theta = "y") + 
  scale_fill_manual(values = kolory)+
  labs(fill="", y="",x="", title = "szpital, bez chorób", caption = "")+
  theme_bw()+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

p3 <- ggplot(c, aes(x="", y=new_confirm, fill=person_age_group)) +
  geom_col() +
  #geom_label_repel(aes(label = paste0(round(zarazeni/sum(zarazeni)*100), "%")), 
  #position = position_stack(vjust = 0.5), show.legend = FALSE, color="white",fontface='bold')+
  coord_polar(theta = "y") + 
  scale_fill_manual(values = kolory)+
  labs(fill="", y="",x="", title = "samozolacja, z chorobami", caption = "")+
  theme_bw()+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

p4 <- ggplot(d, aes(x="", y=new_confirm, fill=person_age_group)) +
  geom_col() +
  #geom_label_repel(aes(label = paste0(round(zarazeni/sum(zarazeni)*100), "%")), 
  #position = position_stack(vjust = 0.5), show.legend = FALSE, color="white",fontface='bold')+
  coord_polar(theta = "y") + 
  scale_fill_manual(values = kolory)+
  labs(fill="", y="",x="", title = "szpital, z chorobami", caption = "")+
  theme_bw()+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

grid.arrange(p1,p2,p3,p4, ncol=2)

## porównanie proporcji w zależności od samego wieku

a <- izolacja.szpital%>%
  filter(izolacja=="samoizolacja")%>%
  group_by(person_age_group)%>%
  summarise(
    liczba  = sum(new_confirm)
  ) %>%
  filter(person_age_group!="b.d.")

b <- izolacja.szpital%>%
  filter(izolacja=="szpital")%>%
  group_by(person_age_group)%>%
  summarise(
    liczba  = sum(new_confirm)
  )%>%
  filter(person_age_group!="b.d.")


p1 <- ggplot(a, aes(x="", y=liczba, fill=person_age_group)) +
  geom_col() +
  #geom_label_repel(aes(label = paste0(round(zarazeni/sum(zarazeni)*100), "%")), 
  #position = position_stack(vjust = 0.5), show.legend = FALSE, color="white",fontface='bold')+
  coord_polar(theta = "y") + 
  scale_fill_manual(values = kolory)+
  labs(fill="", y="",x="", title = "samoizolacja", caption = "")+
  theme_bw()+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

p2 <- ggplot(b, aes(x="", y=liczba, fill=person_age_group)) +
  geom_col() +
  #geom_label_repel(aes(label = paste0(round(zarazeni/sum(zarazeni)*100), "%")), 
  #position = position_stack(vjust = 0.5), show.legend = FALSE, color="white",fontface='bold')+
  coord_polar(theta = "y") + 
  scale_fill_manual(values = kolory)+
  labs(fill="", y="",x="", title = "szpital", caption = "")+
  theme_bw()+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

grid.arrange(p1,p2, ncol=2)

# dzienne zachorowania medykóW
a <- szpitale.medycy %>%
  filter(data==max(data), medycy=="tak")

a <- szpitale.medycy%>%
  filter(medycy=="tak")%>%
  group_by(Obwód)%>%
  summarise(
    suma=sum(new_death)
  )

a <- szpitale2%>%
  filter(is_medical_worker=="Так")%>%
  group_by(registration_area, person_age_group, add_conditions)%>%
  summarise(
    suma=sum(new_death)
  )%>%
  filter(suma>0)%>%
  group_by(person_age_group, add_conditions)%>%
  summarise(
    suma=sum(suma)
  )
ggplot(a)+
  geom_col(aes(x=person_age_group, y=suma, fill=add_conditions), position = "dodge")+
  #facet_wrap(~add_conditions)+
  coord_flip()
