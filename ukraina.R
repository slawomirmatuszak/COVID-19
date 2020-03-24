library(tidyr)
library(dplyr)
library(lubridate)
library(readxl)

###########################################################################################################
obwody<- read_xlsx("./Ukraina.dane/lista obwodów2.xlsx") %>%
  select(1,2,4,7:10,17:20) %>%
  mutate(Obwód=as.character(Obwód))%>%
  filter(Obwód!="Krym"&Obwód!="Sewastopol")%>%
  rename(id=6)
obwody[13,6] <- "м. Київ"

#dodajemy dane Dubileta
dubilet <- read_xlsx("E:/R/UA_spis/data/dubilet.xlsx")%>%
  select(1,2)%>%
  filter(is.na(dubilet)==FALSE)%>%
  rename(id=1)%>%
  mutate(dubilet=dubilet*1000)

obwody <- left_join(obwody,dubilet, by="id") %>%
  select(-11)%>%
  rename(population=11)
rm(dubilet)

save(obwody, file = "./Ukraina.dane/obwody.lista.Rda")
load(file = "E:/R/COVID-19/Ukraina.dane/obwody.lista.Rda")

########################################################################################################
# obwody z csv
UA_obwody <- read.csv2("./Ukraina.dane/obwody/2020.03.18.Tablica_data.csv", encoding = "UTF-8", stringsAsFactors = F)%>%
  rename(dane=1, id=2, ilosc=3)%>%
  mutate(id=as.character(id))%>%
  mutate(data=as.Date("2020-03-18"))%>%
  select(4, 2, 1, 3)
#dodajemy iwanofrankowsk
UA_obwody[18,4] <- 1

# łączenie danych z wielu dni i wielu plików
path <- "./Ukraina.dane/obwody/dane"
merge_file_name <- "./Ukraina.dane/obwody//merged_file.xlsx"
filenames_list <- list.files(path= path, full.names=TRUE)

All <- lapply(filenames_list,function(filename){
  print(paste("Merging",filename,sep = " "))
  read_xlsx(filename)
})
UA_obwody2 <- do.call(rbind.data.frame, All)
#doajemy Iwanofrankowsk
UA_obwody2[9,6] <- 1
rm(All, path, merge_file_name, filenames_list)

UA_obwody2 <- UA_obwody2 %>%
  fill(data, .direction = "down")%>%
  rename(id=2)%>%
  filter(id!="Всього по Україні")%>%
  pivot_longer(cols = c(3:6), names_to = "dane", values_to = "ilosc")

#łączymy dane
UA_obwody <- rbind(UA_obwody, UA_obwody2)
# łączymy z obwodami
UA_obwody <- left_join(UA_obwody, obwody, by="id")
rm(UA_obwody2)

#zmieniamy jednak na szeroką, bo tak jest lepiej
UA_obwody <- UA_obwody%>%
  unite(id2, Kod, data, remove = FALSE)%>%
  mutate(dane=if_else(dane=="Захворіло", paste("liczba.chorych"),
                      if_else(dane=="Підозри", paste("podejrzewani"),
                              ifelse(dane=="Лікарні", paste("liczba.szpitali"),
                                     if_else(dane=="Пристосовані ліжкомісця", paste("liczba.lozek"),
                      paste(dane))))))
UA_obwody <- pivot_wider(UA_obwody, names_from = dane, values_from = ilosc)

#do sprawdzenia, czy się zgadza 
test <- UA_obwody %>%
  filter(data==max(data)) %>%
  select(3,17:14) 
        
save(UA_obwody, file = "./Ukraina.dane/obwody.Rda")
load(file = "E:/R/COVID-19/Ukraina.dane/obwody.Rda")

###################################################################################################
###################################################################################################
###################################################################################################
#Poniższa część do następnej linii jest już nieaktualna, bo przestali wykładać dane
#(z pliku power BI)
szpitale <- read.csv("./Ukraina.dane/dobowe.polaczone.csv", encoding = "UTF-8", stringsAsFactors = F)%>%
  unite(id2, c(2:3), sep = "_", remove = FALSE)

#przepuszczenie przez power BI
nazwy.szpitali <- read_xlsx("./Ukraina.dane/nazwy.szpitali.xlsx") %>%
  unite(id, c(2:4), sep = ", ", remove =FALSE)%>%
  unite(id2, c(2,6), sep = "_", remove = FALSE)

#próbujemy łączyć
#zostawiam, dla testów na kolejne dni
a <- left_join(szpitale, nazwy.szpitali, by="id2") 

szpitale <- left_join(szpitale, nazwy.szpitali, by="id2")

szpitale <- szpitale%>%
  select(-c(2,14:19))%>%
  rename(data=1)%>%
  mutate(data=gsub(".csv", "", data)) %>%
  mutate(data=ymd(data))%>%
  rename(adres=2, szpital=3)
  
# na tym etapie trzeba przetestować, czy są NA w obwodach

szpitale <- szpitale%>%
  mutate(obwod=if_else(adres=="м. Кременчук, вул. Павлова,16", paste("połtawski"),
                        if_else(adres=="м. Мелітополь, вул. Кізіярська,38", paste("zaporoski"),
                                if_else(adres=="м. Мелітополь, вул.Кізіярська,55", paste("zaporoski"),
                                        if_else(adres=="м. Пологи, вул. ім. Героя України Сацького В.А.,6 / вул. І.Чуберка, 90", paste("zaporoski"),
                                                if_else(adres=="м. Суми. вул. Ковпака 22,", paste("sumski"),
                                                        if_else(adres=="м. Суми. вул. Троїцька, 28", paste("sumski"),
                                                                if_else(adres=="м. Хмельницький , вул. Г.Сковороди, 17", paste("chmielnicki"),
                                                                        if_else(adres=="м. Ямпіль, вул. Пирогова,1", paste("winnicki"),
                                                                                if_else(adres=="м.Гадяч, вул.Лохвицька,1", paste("połtawski"),
                                                                                        if_else(adres=="смт. Піщанка, вул. Центральна,42", paste("winnicki"),
                                paste(obwod))))))))))))
szpitale <- szpitale%>%
  rename(Obwód=obwod, lat.szp=lat, long.szp=long)

# sprawdzić jeszcze raz NA w obwodach
  
                                           

#testujemy, czy się zgadza z danymi z Tableu
a <- szpitale %>%
  filter(data==max(data))%>%
  group_by(obwod)%>%
  summarise(
    lozka=sum(przystosowane.lozka)
  )

#łączymy z danymi obwodów
szpitale <- left_join(szpitale, obwody, by="Obwód")

# dla BI
#tu może być błąd, bo było szpitale 2, a zmieniłem na szpitale
save(szpitale, file = "./Ukraina.dane/szpitale.Rda")

###################################################################################################
# nie trzeba odpalać skryptu powyżej - wystarczy wgrać plik
load(file = "E:/R/COVID-19/Ukraina.dane/szpitale.Rda")
szpitale <- szpitale %>%
  rename(wyzdrowieli=wyzrowieli)

# łączenie danych z wielu dni i wielu plików
path <- "./Ukraina.dane/dobowe.dane.2"
merge_file_name <- "./Ukraina.dane//merged_file.xlsx"
filenames_list <- list.files(path= path, full.names=TRUE)

All <- lapply(filenames_list,function(filename){
  print(paste("Merging",filename,sep = " "))
  read_xlsx(filename)
})
szpitale2 <- do.call(rbind.data.frame, All)
rm(All, path, merge_file_name, filenames_list)


## dodajemy szpitale z nowych plików
szpitale2 <- szpitale2 %>%
  fill(1, .direction = "down")%>%
  rename(stan=2, szpital=3, adres=4, przystosowane.lozka=5, podejrzewani=6, potwierdzeni=7, wyzdrowieli=8, smiertelne.z.potwierdzonych=9) %>%
  select(1,4,3, 2, 5, 8, 6, 7,9)%>%
  unite(id, c(2,3), sep = "_", remove = FALSE)%>%
  mutate(id=gsub(",", "", id))%>%
  mutate(id=gsub("[.]", "", id))%>%
  mutate(id=gsub(" ", "", id))%>%
  mutate(id=gsub('"', "", id))%>%
  mutate(id=tolower(id))%>%
  mutate(id=gsub("-", "", id)) %>%
  #tu jest potencjalny błąd, ale 19 marca są dane z całęgo kijowa
  filter(szpital!="Всього по Києву")

#dodajemy długość i szerokość ze szpitali. 
#update: Nie ma sensu, za dużo zabawy. Same obwody.
a <- szpitale %>%
  filter(data==max(data))%>%
  select(2:3, 12)%>%
  unite(id, c(1,2), sep = "_", remove = FALSE)%>%
  select(-c(2,3))%>%
  mutate(id=gsub(",", "", id))%>%
  mutate(id=gsub("[.]", "", id))%>%
  mutate(id=gsub(" ", "", id))%>%
  mutate(id=gsub('"', "", id))%>%
  mutate(id=tolower(id))%>%
  mutate(id=gsub("-", "", id))


a <- left_join(szpitale2, a, by="id")
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
  mutate(Obwód=if_else(adres=="м. Запоріжжя, вул. Чумаченко, 21", paste("zaporoski"), paste(Obwód)))
  

  
# test NA i "NA"
b <- filter(a, Obwód=="NA")
c <- filter(a, is.na(Obwód)==TRUE)

#wyrzucamy śmieci i samoizolowanych
a <- a %>%
  filter(Obwód!="NA")

#test, czy się zgadza liczba łóżej z Tableu
b<- a %>%
  filter(data==max(data))%>%
  group_by(Obwód)%>%
  summarise(
    lozka=sum(przystosowane.lozka),
    podejrzani=sum(podejrzewani),
    zdiagnozowani=sum(potwierdzeni, na.rm=TRUE)
  )
b <- filter(a, data==max(data))
# dane o zakażonych rozjeżdżają sie z danymi obwodowymi. Może dlatego, że nie wszyscy są w szpitalach

#dodajemy listę obwodów
a<- left_join(select(a, -2), obwody, by="Obwód")%>%
  mutate(lat.szp="NA", long.szp="NA")%>%
  select(1:3, 21,22,4:20) %>%
  mutate(data=as.Date(data))

#łączymy ze szpitalami
szpitale2 <- rbind(szpitale, a)

save(szpitale2, file = "./Ukraina.dane/szpitale2.Rda")
load(file = "E:/R/COVID-19/Ukraina.dane/szpitale2.Rda")

###################################################################################################
load(file = "E:/R/COVID-19/Ukraina.dane/obwody.Rda")
load(file = "E:/R/COVID-19/Ukraina.dane/szpitale2.Rda")

# łączymy obwody, śmiertelne przypadki i wyzdrowienia
# trzeba będzie pomyśleć, jak potem zrobić skumulowane zgony i wyzdrowienia. 
zgon.zdrowi <- szpitale2 %>%
  group_by(data, Kod)%>%
  summarise(
    dzienne.zgony = sum(smiertelne.z.potwierdzonych),
    dzienne.wyzdrowienia = sum(wyzdrowieli),
  ) %>%
  group_by(Kod)%>%
  #linia poniżej jest jeszcze do weryfikacji, jak będzie więcej zgonów/wyzdrowień
  mutate(suma.zgonow=cumsum(dzienne.zgony), suma.wyzdrowien=cumsum(dzienne.wyzdrowienia)) %>%
  filter(suma.zgonow>0|suma.wyzdrowien>0) %>%
  #usuwamy daty
  filter(data>"2020-03-17")%>%
  unite(id2, Kod, data, sep="_", remove = FALSE)%>%
  ungroup()%>%
  select(-c(2,3))
#dodajemy ręcznie zmarłego z żytomierza
zgon.zdrowi[1,2] <- 1

# łączymy tabele
UA_obwody2 <- left_join(UA_obwody, zgon.zdrowi, by="id2")%>%
  #ciekawy fragment do zmiany NA na zero
  mutate_at(c(18:21), ~replace(., is.na(.), 0))

save(UA_obwody2, file = "./Ukraina.dane/obwody2.Rda")

# to jest do dopracowania i rozszerzenia
#trzeba zmienić z liczby chorych na aktywnych
a <- UA_obwody2 %>%
  group_by(Obwód) %>%
  mutate(nowe.zachorowania = liczba.chorych - lag(liczba.chorych, default = first(liczba.chorych))) %>%
  mutate(suma.aktywnych = liczba.chorych-suma.zgonow-suma.wyzdrowien) %>%
  mutate(wolne.lozka=liczba.lozek-suma.aktywnych)%>%
  mutate(lozka.10tys = liczba.lozek/population*10000)%>%
  mutate(proc.wykorzystanych.lozek = suma.aktywnych/liczba.lozek)
#wydzielnamy nowe zachorowania ze skumulowanych pierszego dnia.
pierwszy.dzien <- UA_obwody2 %>%
  filter(data==min(data))%>%
  select(14)%>%
  pull()
#dodajemy nowe zachorowania
a[1:25,22] <- pierwszy.dzien
rm(pierwszy.dzien)

a <- a %>%
  mutate(nowi.aktywni=nowe.zachorowania-dzienne.zgony-dzienne.wyzdrowienia)
# jest ujemny w Iwanofrankowsku (ale to chyba ok, bo będzie suma)
UA_obwody2 <- a
save(UA_obwody2, file = "./Ukraina.dane/obwody2.Rda")
load(file = "E:/R/COVID-19/Ukraina.dane/obwody2.Rda")

#przerabiamy na długą, dla mapy w Power BI
obwody.long <- UA_obwody2 %>%
  select(2,7,8,9, 10:13, 16,17,18,19,22,27)%>%
  pivot_longer(cols = c(11:14), names_to = "zachorowania", values_to = "liczba")%>%
  mutate(zachorowania = gsub("dzienne.zgony", "zgony", zachorowania))%>%
  mutate(zachorowania = gsub("dzienne.wyzdrowienia", "wyleczeni", zachorowania))%>%
  mutate(zachorowania = gsub("nowi.aktywni", "aktywni", zachorowania))%>%
  mutate(zachorowania = gsub("nowe.zachorowania", "zachorowania", zachorowania))

  
save(obwody.long, file = "./Ukraina.dane/obwody.long.Rda")
load(file = "E:/R/COVID-19/Ukraina.dane/obwody.long.Rda")

