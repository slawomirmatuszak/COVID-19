library(tidyr)
library(dplyr)
library(lubridate)
library(readxl)

obwody<- read_xlsx("./Ukraina.dane/lista obwodów2.xlsx") %>%
  select(1,2,4,7:10,17:20) %>%
  mutate(Obwód=as.character(Obwód))%>%
  filter(Obwód!="Krym"&Obwód!="Sewastopol")%>%
  rename(id=6)
obwody[13,6] <- "м. Київ"

# nad tym będzie trzeba popracować, pomyśleć, jak efektywnie łączyć dane z wielu dni i wielu plikóW.
UA_obwody <- read.csv2("./Ukraina.dane/obwody/2020.03.18.Tablica_data.csv", encoding = "UTF-8", stringsAsFactors = F)%>%
  rename(dane=1, id=2, ilosc=3)%>%
  mutate(id=as.character(id))

UA_obwody <- left_join(UA_obwody, obwody, by="id")

save(UA_obwody, file = "./Ukraina.dane/obwody.Rda")
load(file = "E:/R/COVID-19/Ukraina.dane/obwody.Rda")

###############################################################################
#(z pliku power BI)
szpitale <- read.csv("./Ukraina.dane/dobowe.polaczone.csv", encoding = "UTF-8", stringsAsFactors = F)%>%
  unite(id2, c(2:3), sep = "_", remove = FALSE)

save(szpitale, file = "./Ukraina.dane/szpitale.Rda")
load(file = "E:/R/COVID-19/Ukraina.dane/szpitale.Rda")

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
save(szpitale, file = "./Ukraina.dane/szpitale2.Rda")
load(file = "E:/R/COVID-19/Ukraina.dane/szpitale2.Rda")

# trzeba podoawać ilość wykorzystanych łóżek (na chorych i oddzielnie na chorych+podejrzanych)
# trzeba wgrać na wcześniejszym etapie dane spisu Dubileta, bo wydają się bardziej wiarygodne i nie mają DNR/LNR

