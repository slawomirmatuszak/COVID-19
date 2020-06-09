library(tidyverse)
library(lubridate)

# miejscowości ----------------------------------------------------------
library(rgeos)
library(rgdal)
library(tidyverse)

shp1 <- readOGR("E:/R/UA_spis/data/mapa", layer = "obwod_pl")
#zmieniamy format danych
shp1f <- fortify(shp1, region = "ADM1_PCODE")

load(file = "E:/R/COVID-19/miejscowosci.Rda")

a <- miejscowosci%>%
  filter(total_confirm>0)

png("./wykresy/UA.zakażenia.png", units="in", width=9, height=6, res=300)
ggplot(a) + 
  #geom_map(data=a, aes(map_id=Kod), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.001) + 
  coord_map(projection = "mercator") + 
  geom_point(aes(x=registration_settlement_lng, y=registration_settlement_lat, size=total_confirm), alpha=0.4, color="orange")+
  labs(size="zakażeni")+
  theme_bw()+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = c(0.1, 0.2),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), plot.background = element_rect(colour = "grey", size = 0.5), 
        plot.caption = element_text(size = 8))
dev.off()

a <- miejscowosci%>%
  filter(total_death>0)

png("./wykresy/UA.zgony.png", units="in", width=9, height=6, res=300)
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
dev.off()

a <- miejscowosci%>%
  filter(total_recover>0)

png("./wykresy/UA.wyleczeni.png", units="in", width=9, height=6, res=300)
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
dev.off()

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


# miejscowosci dynamika ---------------------------------------------------

miejscowosci.timeline <- read_csv("https://raw.githubusercontent.com/VasiaPiven/covid19_ua/master/covid19_by_settlement_dynamics.csv")

# zmiana Dniepropiertowska
Dnipro <- miejscowosci.timeline %>%
  filter(registration_area=="Дніпропетровська")%>%
  filter(zvit_date==min(zvit_date))%>%
  select(registration_settlement)%>%
  pull()

# porównanie dla dużych miast
miasta <- miejscowosci.timeline %>%
  filter(registration_settlement %in% c("Київ", "Львів", "Харків", Dnipro, "Одеса", "Кривий Ріг", "Запоріжжя", "Миколаїв","Маріуполь", "Вінниця", "Херсон", "Полтава")) %>%
  group_by (registration_settlement, zvit_date)%>%
  summarise_if(is.numeric, sum)%>%
  arrange(zvit_date)%>%
  mutate(zach.cum=cumsum(new_confirm),
         zgony.cum=cumsum(new_death),
         wyleczeni.cum=cumsum(new_recover),
         aktywni=zach.cum-(wyleczeni.cum+zgony.cum))%>%
  mutate(nazwa = case_when(registration_settlement== "Київ" ~ "Kijów",
                           registration_settlement==Dnipro ~"Dnipro",
                           registration_settlement=="Львів" ~"Lwów",
                           registration_settlement=="Харків" ~"Charków",
                           registration_settlement=="Одеса" ~"Odessa",
                           registration_settlement=="Кривий Ріг" ~"Krzywy Róg",
                           registration_settlement=="Запоріжжя" ~"Zaporoże",
                           registration_settlement=="Миколаїв" ~"Mikołajów",
                           registration_settlement=="Вінниця" ~"Winnica",
                           registration_settlement=="Херсон" ~"Chersoń",
                           registration_settlement=="Полтава" ~"Połtawa",
                           registration_settlement=="Маріуполь" ~"Mariupol"))%>%
  mutate(ludnosc = case_when(registration_settlement== "Київ" ~ 2611370,
                           registration_settlement==Dnipro ~ 1065008,
                           registration_settlement=="Львів" ~ 732818,
                           registration_settlement=="Харків" ~ 1470902,
                           registration_settlement=="Одеса" ~ 1023049,
                           registration_settlement=="Кривий Ріг" ~ 668980,
                           registration_settlement=="Запоріжжя" ~ 815256,
                           registration_settlement=="Миколаїв" ~ 514136,
                           registration_settlement=="Вінниця" ~ 356665,
                           registration_settlement=="Херсон" ~ 328360,
                           registration_settlement=="Полтава" ~ 317998,
                           registration_settlement=="Маріуполь" ~ 492176))%>%
  mutate(zach.100.cum = zach.cum*1e5/ludnosc,
         zach.100 = new_confirm*1e5/ludnosc,
         srednia= zoo::rollmean(zach.100, k=7, fill=NA, align="right"),
         aktywni.100 = aktywni*1e5/ludnosc)

miasta.long <- miasta %>%
  select(nazwa, zvit_date, registration_settlement, aktywni, wyleczeni.cum, zgony.cum)%>%
  pivot_longer(cols = c(aktywni, wyleczeni.cum, zgony.cum), names_to = "nazwy", values_to = "liczby")%>%
  mutate(nazwy=gsub(".cum", "", nazwy))

png(paste0("./wykresy/miasta.", Sys.Date(),".png"), units="in", width=10, height=8, res=300)
ggplot(miasta.long, aes(x=zvit_date, y=liczby, color=nazwy))+
  geom_path(size=2)+
  facet_wrap(~nazwa, scales = "free_y", ncol=4)+
  scale_color_manual(values = c("aktywni"="orange", "wyleczeni"="darkgreen", "zgony"="red"))+
  labs(x="", y="", color="")+
  theme_bw()+
  theme(legend.position = "top")
dev.off()

ggplot(miasta, aes(x=zvit_date, y=srednia))+
  geom_path(size=2, color="orange")+
  facet_wrap(~nazwa,  ncol=4)+
  labs(x="", y="", color="")+
  theme_bw()+
  theme(legend.position = "top")

ggplot(miasta, aes(x=zvit_date, y=zach.100.cum))+
  geom_path(size=2, color="orange")+
  facet_wrap(~nazwa,  ncol=4)+
  labs(x="", y="", color="")+
  theme_bw()+
  theme(legend.position = "top")

ggplot(miasta, aes(x=zvit_date, y=aktywni.100))+
  geom_path(size=2, color="orange")+
  facet_wrap(~nazwa,  scales = "free_y", ncol=4)+
  labs(x="", y="", color="")+
  theme_bw()+
  theme(legend.position = "top")

# miejscowości rejony -----------------------------------------------------

load(file = "E:/R/COVID-19/miejscowosci.Rda")

rejony <- miejscowosci%>%
  filter(total_confirm>0)%>%
  group_by(Obwód, registration_region)%>%
  summarise(
    zgony=sum(total_death),
    wyleczeni=sum(total_recover),
    zakazeni = sum(total_confirm),
    long=mean(registration_settlement_lng),
    lat=mean(registration_settlement_lat)
  )%>%
  mutate(aktywni=zakazeni-(wyleczeni+zgony))


#sprawdzamy, które są zduplikowane
 a <- rejony%>%
   ungroup()%>%
   filter(duplicated(registration_region)==T)
   
   select(registration_region)%>%
   duplicated()%>%
   sum()

png("./wykresy/UA.rejony.zakażeni.png", units="in", width=9, height=6, res=300)
ggplot(rejony) + 
  #geom_map(data=a, aes(map_id=Kod), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.001) + 
  coord_map(projection = "mercator") + 
  geom_point(aes(x=long, y=lat, size=zakazeni), alpha=0.5, color="orange")+
  scale_size(range = c(0, 10))+
  #scale_size_binned(range = c(0, 15))+
  theme_bw()+
  labs(size="zakażeni")+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = c(0.1, 0.2),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), plot.background = element_rect(colour = "grey", size = 0.5), 
        plot.caption = element_text(size = 8))

dev.off()

png("./wykresy/UA.rejony.aktywni.png", units="in", width=9, height=6, res=300)

rejony%>%filter(aktywni>0)%>%
ggplot() + 
  #geom_map(data=a, aes(map_id=Kod), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.001) + 
  coord_map(projection = "mercator") + 
  geom_point(aes(x=long, y=lat, size=aktywni), alpha=0.5, color="orange")+
  scale_size(range = c(0, 10))+
  theme_bw()+
  labs(size="aktywni")+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = c(0.1, 0.2),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), plot.background = element_rect(colour = "grey", size = 0.5), 
        plot.caption = element_text(size = 8))

dev.off()

png("./wykresy/UA.rejony.zgony.png", units="in", width=9, height=6, res=600)

rejony%>%filter(zgony>0)%>%
  ggplot() + 
  #geom_map(data=a, aes(map_id=Kod), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.001) + 
  coord_map(projection = "mercator") + 
  geom_point(aes(x=long, y=lat, size=zgony), alpha=0.5, color="red")+
  scale_size(range = c(1, 10))+
  theme_bw()+
  labs(size="zgony")+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = c(0.1, 0.2),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), plot.background = element_rect(colour = "grey", size = 0.5), 
        plot.caption = element_text(size = 8))

dev.off()


# łączenie rajonów z kodem KOTAU ------------------------------------------

#generalnie działa, ale nie wszystko w secie Wasi da się sprowadzić do rejonóW. Czasem miasta i rajony są podzielone. Trzeba by ręcznie dłubać. 

shp1 <- readOGR("./Ukraina.dane/rejony.mapa", layer = "ukr_admbnda_adm2_q2_sspe_20171221")
#zmieniamy format danych
shp1f <- fortify(shp1, region = "ADM2_PCODE")%>%
  filter(str_detect(id, "UA73...|UA26..."))


load(file = "E:/R/COVID-19/Ukraina.dane/obwody.lista.Rda")

obwody <- obwody %>%  
  select(Obwód, id)

lista.rejonow <- read_xlsx("E:/Power BI/Listy/Lista rejonów.xlsx")%>%
  rename(id=1, rejon.miasto=2, rejon=3, powierzchnia=4, ludnosc=5, centrum=6, kod=7)%>%
  mutate(rejon.miasto=gsub("райони", "район", rejon.miasto),
         rejon.miasto=gsub("міста обласного значення", "", rejon.miasto),
         rejon.miasto=gsub("міста республіканського значення", "", rejon.miasto))%>%
  left_join(obwody, by="id")%>%
  mutate(rejon=gsub("І","І", rejon))%>%
  unite(col="rejon" , rejon, rejon.miasto, sep=" ")%>%
  unite(col="id2", Obwód, rejon, sep="_", remove = F)%>%
  mutate(id2=trimws(id2))%>%
  select(-Obwód)


a<- rejony%>%
  unite(col="id2", Obwód, registration_region, remove = F)%>%
  left_join(lista.rejonow, by="id2")%>%
  mutate(zach.100=zakazeni*1e5/ludnosc)%>%
  filter(Obwód %in% c("czerniowiecki", "iwanofrakiwski"))

ggplot() + 
  geom_map(data=a, aes(map_id=kod, fill=zach.100), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.001) + 
  coord_map(projection = "mercator") + 
  scale_fill_gradient(low = "white", high = "orange") +
  geom_label(data=a, aes(x=long, y=lat), label=paste(a$rejon, "\n", round(a$zach.100,0)), size=3) +
  theme_bw()+
  labs(size="aktywni")+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = c(0.1, 0.2),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), plot.background = element_rect(colour = "grey", size = 0.5), 
        plot.caption = element_text(size = 8))


# odsetek zachorowań i zgonów pracowników medycznych ----------------------

a <- szpitale.medycy %>%
  group_by(medycy)%>%
  summarise_if(is.numeric, sum)

proc.zakaz.medykow <- a[2,3]/sum(a[2,3]+a[1,3])%>%
  tibble::enframe(name = NULL)%>%
  pull()

smiertelnosc <- a %>%
  summarise_if(is.numeric, sum)%>%
  mutate(smiertelnosc = new_death/new_confirm)%>%
  select(smiertelnosc)%>%
  pull

smiertelnosc.medycy <- a %>%
  filter(medycy=="tak")%>%
  summarise_if(is.numeric, sum)%>%
  mutate(smiertelnosc = new_death/new_confirm)

medycy.smiertelnosc <- szpitale2 %>%
  mutate(data=ymd(zvit_date))%>%
  group_by(is_medical_worker, person_age_group, add_conditions)%>%
  summarise_if(is.numeric, sum)%>%
  ungroup()%>%
  mutate(smiertelnosc = new_death/new_confirm)%>%
  mutate(add_conditions = gsub("Так", "choroby współistniejące", add_conditions))%>%
  mutate(add_conditions = gsub("Ні", "brak chorób współistniejących", add_conditions))%>%
  mutate(person_age_group = gsub("Уточнюється", "b.d.", person_age_group))%>%
  filter(is_medical_worker=="Так",
         new_confirm>0)

a <- medycy.smiertelnosc%>%
  filter(smiertelnosc>0)

ggplot(a, aes(x=person_age_group, y=smiertelnosc, label=smiertelnosc))+
  geom_col(fill="blue")+
  facet_wrap(~add_conditions)+
  geom_label(label=paste0(round(a$smiertelnosc*100,1),"%"), hjust=-0.1, fill="blue", color="white", fontface = "bold")+
  labs(x="grupa wiekowa",
       y="śmiertelność")+
  coord_flip()+
  scale_y_continuous(labels = percent, limits = c(0,0.1))+
  theme_bw()


  
  