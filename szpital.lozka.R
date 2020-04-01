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
  filter(duplicated(adres)==FALSE)%>%
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

nowy <- read_xlsx("./Ukraina.dane/dobowe.dane.2/2020.04.01.xlsx")
a <- nowy %>%
  rename(liczba.lozek=2, adres=4)%>%
  mutate(liczba.lozek = as.numeric(liczba.lozek)) %>% 
  fill(liczba.lozek, .direction = "down") %>%
  unite(id, c(5,4), remove = FALSE) %>%
  mutate(id=gsub(",", "", id))%>%
  mutate(id=gsub("[.]", "", id))%>%
  mutate(id=gsub(" ", "", id))%>%
  mutate(id=gsub('"', "", id))%>%
  mutate(id=tolower(id))%>%
  mutate(id=gsub("-", "", id))%>%
  # to jeszcze do przetestowania później, czy nie usuwa szpitali z chorymi
  filter(duplicated(adres)==FALSE)

## zmieniam sposób łączenia, zamiast id na adres
a <- left_join(a, szpitale, by="adres")
  #rename(adres=6)

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
  mutate(Obwód=if_else(adres=="м. Буринь, вул. Кутузова,15.", paste("sumski"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Мелітополь, вул. Кізіярська,38", paste("zaporoski"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="смт. Маневичі, вул. Незалежності, 1", paste("wołyński"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="с. Липини, вул. Теремнівська, 100", paste("wołyński"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Київ, вул. Богговутівська, 1", paste("Kijów"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Скадовськ, вул. Шмідта, 24", paste("chersoński"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Київ, вул. Юрія Кондратюка, 8", paste("Kijów"), paste(Obwód))) %>%
  mutate(Obwód=if_else(adres=="м. Скадовськ, вул. Шмідта, 24", paste("chersoński"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Немирів, вул. Євдокименка, 21", paste("winnicki"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Горохів, вул. Паркова, 22", paste("wołyński"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Ківерці, вул. Філатова, 6", paste("wołyński"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Ірпінь, вул. Садова, 38", paste("kijowski"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Монастириська, вул. Шевченка, 29", paste("tarnopolski"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Генічеськ, просп. Миру, 130", paste("chersoński"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Дубно, вул. Львівська, 73", paste("równieński"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Каховка, вул. Першотравнева, 34", paste("chersoński"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Київ, вул. Відпочинку, 11", paste("Kijów"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="смт. Козова, вул. Зелена, 19", paste("tarnopolski"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Житомир, вул. Романа Шухевича, 2-А", paste("żytomierski"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="смт. Мельниця-Подільська, вул. Незалежності, 24", paste("tarnopolski"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Чернівці, вул. Героїв Майдану, 226", paste("czerniowiecki"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="с. Топорище, вул. Житомирська, 86-А", paste("żytomierski"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Нова Каховка, вул Героїв України, 33-А", paste("chersoński"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="с. Козятин, вул. Центральна, 96", paste("winnicki"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="смт. Сарата, вул. Соборна, 2", paste("odeski"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Запоріжжя, бул. Гвардійський, 142", paste("zaporoski"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="смт. Крижопіль, вул. Данила Нечая, 10", paste("winnicki"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Запоріжжя, вул. Сєдова, 3", paste("zaporoski"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Рівне, вул. Київська, 78Г", paste("równieński"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Луцьк, просп. Відродження, 13", paste("wołyński"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="с. Миколай-Поле, вул. Центральна, 46-А", paste("zaporoski"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Зборів, вул. Б. Хмельницького, 17", paste("tarnopolski"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Конотоп, вул. Миколи Амосова, 5", paste("sumski"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="смт. Доброслав, вул. Грубника, 27", paste("odeski"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Харків, вул. Гуданова, 5", paste("charkowski"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="смт. Підволочиськ, вул. Патріарха Мстислава, 102", paste("tarnopolski"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Ладижин, вул. Ентузіастів, 24", paste("winnicki"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Київ, вул. Госпітальна, 18", paste("Kijów"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Київ, вул. Братиславська, 5-А", paste("Kijów"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Кременчук, вул. Лікаря О.Богаєвського, 60/1", paste("połtawski"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Вишгород, вул. Кургузова, 1", paste("kijowski"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Київ, проспект Любомира Гузара, 3", paste("Kijów"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Почаїв, вул. Возз'єднання, 19", paste("tarnopolski"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Черкаси, вул. Чехова, 101", paste("czerkaski"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Березне, вул. Київська, 19", paste("równieński"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Київ, вул. Сковороди, 2", paste("Kijów"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Малин, вул. Бондарик, 17", paste("żytomierski"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Павлоград, вул. Дніпровська, 541", paste("dniepropietrowski"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Кропивницький, вул. Архітектора Паученка, 45/35", paste("kirowohradzki"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Івано-Франківськ, вул. Гетьмана Сагайдачного, 66", paste("iwanofrakiwski"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Київ, вул. Петра Запорожця, 26", paste("Kijów"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Берислав, вул. 1 Травня, 124", paste("chersoński"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Кропивницький, вул. Велика Перспективна, 65", paste("kirowohradzki"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Гайсин, вул. В'ячеслава Чорновола, 1", paste("winnicki"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Луцьк, просп. Президента Грушевського, 21", paste("wołyński"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="смт. Тиврів, вул. Шевченка, 2А", paste("winnicki"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Коломия, вул. І. Мазепи, 134", paste("iwanofrakiwski"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Київ, вул. Бердичівська, 1", paste("Kijów"), paste(Obwód)))%>%
  mutate(Obwód=if_else(adres=="м. Берислав, вул. 1 Травня, 124", paste("chersoński"), paste(Obwód)))
 

# test NA i "NA"
c <- filter(a, Obwód=="NA") %>%
  rename(chorzy=8, podejrzani=7)%>%
  filter(chorzy>0|podejrzani>0|liczba.lozek>0)

c <- filter(a, is.na(Obwód)==TRUE)

b <- a %>%
  rename(podejrzewani=7, chorzy=8) %>%
  group_by(Obwód)%>%
  summarise(
    liczba.lozek = sum(liczba.lozek, na.rm=TRUE),
    liczba.chorych = sum(chorzy, na.rm=TRUE),
    liczba.podejrzanych = sum(podejrzewani, na.rm=T)
    ) %>%
  filter(Obwód!="NA")%>%
  filter(is.na(Obwód)==FALSE)

b <-  b %>%
  left_join(select(obwody, c(4,7,9,10,11)), by="Obwód")%>%
  mutate(lozka.10tys = liczba.lozek/population*10000)%>%
  mutate(wolne.lozka = liczba.lozek - liczba.chorych - liczba.podejrzanych)%>%
  #poprawiamy nazwy obwodów dla OSW
  mutate(Obwód = gsub("łuhański", "ługański", Obwód))%>%
  mutate(Obwód = gsub("dniepropietrowski", "dniepropetrowski", Obwód))%>%
  mutate(Obwód = gsub("iwanofrakiwski", "iwanofrankiwski", Obwód))

szpitale.lozka <- b

save(szpitale.lozka, file = "./Ukraina.dane/szpitale.lozka.Rda")
load(file = "E:/R/COVID-19/Ukraina.dane/szpitale.lozka.Rda")

############ wykresy

#łóżka na 10 tys.
png("szpitale.10tys.png", units="in", width=9, height=5, res=300)
ggplot(data=b)+
  geom_bar(aes(x=reorder(Obwód, -lozka.10tys), y=lozka.10tys), stat="identity", fill="blue")+
  labs(x="obwód", y="", caption = "Źródło: Departament Polityki Regionalnej i Decentralizacji Biura Prezydenta Ukrainy.",
       title = "Liczba przystosowanych łóżek na 10 tys. mieszkańców",
       subtitle = "wg stanu na 25 marca") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),  plot.caption = element_text(hjust = 0, size = 8),
        plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
dev.off()

# obłożenie liczbą łóżek
#przestali publikować, ile jest łóżek
c <- b %>%
  select(1, 5:7, 3, 4, 10)%>%
  pivot_longer(cols = c(5:7), names_to = "stan", values_to = "liczba") %>%
  mutate(stan=gsub("wolne.lozka", "wolne łóżka", stan))%>%
  mutate(stan=gsub("liczba.podejrzanych", "podejrzewani", stan))%>%
  mutate(stan=gsub("liczba.chorych", "chorzy", stan))%>%
  mutate(stan=as.factor(stan))

szpitale.oblozenie <- c
save(szpitale.oblozenie, file = "./Ukraina.dane/szpitale.oblozenie.Rda")
load(file = "E:/R/COVID-19/Ukraina.dane/szpitale.oblozenie.Rda")

colors <- c("podejrzewani"="orange", "chorzy"="red", "wolne łóżka"="blue")
#png("szpitale.png", units="in", width=11, height=6, res=600)
ggplot(data=filter(c, liczba>0))+
  geom_bar(aes(x=reorder(Obwód, -liczba), y=liczba, fill=stan), stat="identity")+
  labs(x="obwód", y="liczba łóżek", fill="",
       title = "Sytuacja w szpitalach na Ukrainie",
       subtitle = "wg stanu na 30 marca",
       caption = "Źródło: Departament Polityki Regionalnej i Decentralizacji Biura Prezydenta Ukrainy.")+
  scale_fill_manual(values = colors)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5) , legend.direction = "horizontal", legend.position=c(0.8,0.9), plot.caption = element_text(hjust = 0, size = 8))
#dev.off()


lozka.10.tys <- select(b, 1,7)
write.csv(lozka.10.tys, file="wykres2.lozka.csv")

oblozenie <- c
write.csv(oblozenie, file = "wykres1.oblozenie.csv")