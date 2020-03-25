# modyfikacja skryptu z tableu. Tutaj będą tylko dane za ostatni dostępny dzień.

library(tidyr)
library(dplyr)
library(lubridate)
library(readxl)
library(directlabels)
library(ggplot2)
library(ggthemes)
library(scales)

#wgrywamy stare dane z nazwami szpitali
load(file = "E:/R/COVID-19/Ukraina.dane/szpitale.Rda")
szpitale <- szpitale %>%
  rename(wyzdrowieli=wyzrowieli)%>%
  filter(data==max(data))%>%
  select(2,3,12)%>%
  unite(id, c(1,2), remove = FALSE) %>%
  mutate(id=gsub(",", "", id))%>%
  mutate(id=gsub("[.]", "", id))%>%
  mutate(id=gsub(" ", "", id))%>%
  mutate(id=gsub('"', "", id))%>%
  mutate(id=tolower(id))%>%
  mutate(id=gsub("-", "", id))

# dane z listą obwodów
load(file = "E:/R/COVID-19/Ukraina.dane/obwody.lista.Rda")

# dodajemy najnowszy plik z danymi

nowy <- read_xlsx("./Ukraina.dane/dobowe.dane.2/2020.03.23.xlsx")
a <- nowy %>%
  rename(liczba.lozek=2, adres=5)%>%
  mutate(liczba.lozek = as.numeric(liczba.lozek)) %>% 
  fill(liczba.lozek, .direction = "down") %>%
  unite(id, c(5,4), remove = FALSE) %>%
  mutate(id=gsub(",", "", id))%>%
  mutate(id=gsub("[.]", "", id))%>%
  mutate(id=gsub(" ", "", id))%>%
  mutate(id=gsub('"', "", id))%>%
  mutate(id=tolower(id))%>%
  mutate(id=gsub("-", "", id))

## testujemy połączenie
a <- left_join(a, szpitale, by="id")%>%
  rename(adres=6)

a <- a %>%
  mutate(Obwód=if_else(adres=="с. Софіївська Борщагівка, вул. Яблунева, 26", paste("kijowski"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="м. Харків, вул. Дмитрівська, 18", paste("charkowski"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="м. Андрушівка, вул. Тітова, 34", paste("żytomierski"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="м. Бершадь, вул. Будкевича, 2", paste("winnicki"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="смт. Гусятин, вул. Б.Лепкого, 1.", paste("tarnopolski"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="м. Заліщики, вул. С.Бандери, 86", paste("tarnopolski"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="смт. Народичі, вул. Замкова, 115", paste("żytomierski"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="м. Хмільник, вул. Монастирська, 71", paste("winnicki"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="м. Хотин, вул. Б. Хмельницького, 4", paste("czerniowiecki"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="м. Коростишів, вул. Героїв Небесної Сотні, 58", paste("żytomierski"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="м. Шостка, вул. Щедріна, 1", paste("sumski"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="м. Городенка, вул. Шептицького, 24-Е", paste("iwanofrakiwski"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="м. Лисичанськ, вул. 40 років Перемоги, 12а", paste("łuhański"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="м. Могилів-Подільський, вул. Полтавська, 89/2", paste("winnicki"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="с. Софіївська Борщагівка, вул. Яблунева, 26", paste("kijowski"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="м. Київ", paste("Kijów"), paste(Obwód))) %>%
  mutate(szpital=if_else(adres=="смт. Баришівка, вул. Київський шлях, 126", paste("Баришівська центральна районна лікарня"), paste(szpital))) %>%
  mutate(szpital=if_else(adres=="'м. Макарів, вул. Б.Хмельницького, 62-А", paste("МАКАРІВСЬКА ЦЕНТРАЛЬНА РАЙОННА ЛІКАРНЯ"), paste(szpital))) %>%
  mutate(Obwód=if_else(adres=="м. Київ, вул. Кучера, 7", paste("Kijów"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="м. Київ, просп. Лобановського, 2", paste("Kijów"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="м. Буринь, вул. Кутузова, 15", paste("sumski"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="м. Запоріжжя, просп. Соборний, 88", paste("zaporoski"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="м. Охтирка, вул. Петропавлівська, 15", paste("sumski"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="м.Сокиряни, вул. Кобилянської, 43", paste("czerniowiecki"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="м. Мелітополь, вул. Індустріальна, 89", paste("zaporoski"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="смт. Баришівка, вул. Київський шлях, 126", paste("kijowski"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="м. Запоріжжя, вул. Брюллова, 6", paste("zaporoski"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="м. Бердичів, вул. Здоров’я, 1", paste("żytomierski"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="м. Вінниця, Хмельницьке шосе, 92", paste("winnicki"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="м. Глибока, вул. Шевченка, 11", paste("czerniowiecki"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="м. Жмеринка, вул. Київська, 288", paste("winnicki"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="м. Теребовля, вул. Січових Стрільців, 25", paste("tarnopolski"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="м. Запоріжжя, вул. Чумаченко, 21", paste("zaporoski"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="м. Шаргород, вул. Пархоменко, 9", paste("winnicki"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="м. Коростень, вул. М.Амосова, 8", paste("żytomierski"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="м. Буринь, вул. Кутузова,15.", paste("sumski"), paste(Obwód)))

# test NA i "NA"
c <- filter(b, Obwód=="NA")
c <- filter(b, is.na(Obwód)==TRUE)

b <- a %>%
  rename(podejrzewani=8, chorzy=9) %>%
  group_by(Obwód)%>%
  summarise(
    liczba.lozek = sum(liczba.lozek, na.rm=TRUE),
    liczba.chorych = sum(chorzy, na.rm=TRUE),
    liczba.podejrzanych = sum(podejrzewani, na.rm=T)
    ) %>%
  filter(Obwód!="NA")%>%
  filter(is.na(Obwód)==FALSE)

b <-  left_join(b, select(obwody, c(4,11)), by="Obwód") 
b <-  mutate(b, lozka.10tys = liczba.lozek/population*10000)
b <- mutate(b, wolne.lozka = liczba.lozek - liczba.chorych - liczba.podejrzanych)


############ wykresy

#łóżka na 10 tys.
ggplot(data=b)+
  geom_bar(aes(x=reorder(Obwód, -lozka.10tys), y=lozka.10tys), stat="identity", fill="blue")+
  labs(x="obwód", y="", caption = "Źródło: Departament Polityki Regionalnej i Decentralizacji Biura Prezydenta Ukrainy.") +
  ggtitle("Liczba przystosowanych łóżek na 10 tys. mieszkańców wg stanu na 23 marca") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5))

# obłożenie liczbą łóżek
c <- b %>%
  select(1, 3, 4, 7)%>%
  pivot_longer(cols = c(2:4), names_to = "stan", values_to = "liczba") %>%
  mutate(stan=gsub("wolne.lozka", "wolne łóżka", stan))%>%
  mutate(stan=gsub("liczba.podejrzanych", "podejrzewani", stan))%>%
  mutate(stan=gsub("liczba.chorych", "chorzy", stan))%>%
  mutate(stan=as.factor(stan))


colors <- c("podejrzewani"="orange", "chorzy"="red", "wolne łóżka"="blue")
ggplot(data=c)+
  geom_bar(aes(x=reorder(Obwód, -liczba), y=liczba, fill=stan), stat="identity")+
  labs(x="obwód", y="liczba łóżek", fill="",
       title = "Sytuacja w szpitalach na Ukrainie",
       subtitle = "wg stanu na 23 marca",
       caption = "Źródło: Departament Polityki Regionalnej i Decentralizacji Biura Prezydenta Ukrainy.")+
  #ggtitle("Sytuacja w szpitalach wg stanu na 23 marca")+
  scale_fill_manual(values = colors)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5) , legend.position=c(0.9,0.8), plot.caption = element_text(hjust = 0, size = 8))

lozka.10.tys <- select(b, 1,6)
write.csv(lozka.10.tys, file="wykres2.lozka.csv")

oblozenie <- c
write.csv(oblozenie, file = "wykres1.oblozenie.csv")