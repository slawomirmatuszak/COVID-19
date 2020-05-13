library(tidyverse)
library(lubridate)


# pobór i obróbka danych na poziomie szpitali -----------------------------

szpitale2 <- read.csv("https://raw.githubusercontent.com/VasiaPiven/covid19_ua/master/covid19_by_area_type_hosp_dynamics.csv", encoding = "UTF-8")

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


# miejscowości ----------------------------------------------------------

miejscowosci <- read_csv("https://raw.githubusercontent.com/VasiaPiven/covid19_ua/master/covid19_by_settlement_actual.csv")

a <- miejscowosci%>%
  filter(total_confirm>0)

ggplot(a) + 
  #geom_map(data=a, aes(map_id=Kod), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.001) + 
  coord_map(projection = "mercator") + 
  geom_point(aes(x=registration_settlement_lng, y=registration_settlement_lat, size=total_confirm), alpha=0.5, color="orange")+
  labs(size="zakażeni")+
  theme_bw()+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = c(0.1, 0.2),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), plot.background = element_rect(colour = "grey", size = 0.5), 
        plot.caption = element_text(size = 8))

a <- miejscowosci%>%
  filter(total_death>0)

ggplot(a) + 
  #geom_map(data=a, aes(map_id=Kod), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.001) + 
  coord_map(projection = "mercator") + 
  geom_point(aes(x=registration_settlement_lng, y=registration_settlement_lat, size=total_death), alpha=0.5, color="red")+
  labs(size="zmarli")+
  theme_bw()+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = c(0.1, 0.2),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), plot.background = element_rect(colour = "grey", size = 0.5), 
        plot.caption = element_text(size = 8))

a <- miejscowosci%>%
  filter(total_recover>0)

ggplot(a) + 
  #geom_map(data=a, aes(map_id=Kod), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.001) + 
  coord_map(projection = "mercator") + 
  geom_point(aes(x=registration_settlement_lng, y=registration_settlement_lat, size=total_recover), alpha=0.5, color="darkgreen")+
  labs(size="wyleczeni")+
  theme_bw()+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = c(0.1, 0.2),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), plot.background = element_rect(colour = "grey", size = 0.5), 
        plot.caption = element_text(size = 8))

a <- miejscowosci%>%
  filter(total_susp>0)

ggplot(a) + 
  #geom_map(data=a, aes(map_id=Kod), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.001) + 
  coord_map(projection = "mercator") + 
  geom_point(aes(x=registration_settlement_lng, y=registration_settlement_lat, size=total_susp), alpha=0.5, color="orange")+
  theme_bw()+
  labs(size="podejrzewani")+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = c(0.1, 0.2),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), plot.background = element_rect(colour = "grey", size = 0.5), 
        plot.caption = element_text(size = 8))
