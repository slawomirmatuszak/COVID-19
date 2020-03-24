library(data.table)
library(tidyr)
library(dplyr)
library(lubridate)
library(readxl)

################################################################################################## 
#skrypt do obróbki danych publikowanych przez Ukraińską Prawdę. 

############################ stare dane
load(file = "E:/R/COVID-19/Ukraina.dane/obwody2.Rda")
obwody.stare <- UA_obwody2%>%
  filter(data<"2020-03-22") %>%
  select(-c(1,4,5,6,15:17,24:26))%>%
  select(1:10,13, 14, 16, 15, 11,12, 17 )%>%
  rename (suma.chorych=10, nowe.zgony=15, nowe.wyzrowienia=16)%>%
  ungroup()

#dla testu
nowe.przypadki <- obwody.stare %>%
  select(1,2,14:17)

obwody.stare <- select(obwody.stare, -c(14:17))

############################ dane z obwodów
load(file = "E:/R/COVID-19/Ukraina.dane/obwody.lista.Rda")
obwody.lista <- obwody 
rm(obwody, UA_obwody2)

############################ łączymy pliki csv

# read file path
all_paths <-
  list.files(path = "./Ukraina.dane/UP/",
             pattern = "*.csv",
             full.names = TRUE)
# read file content
all_content <-
  all_paths %>%
  lapply(read.table,
         header = TRUE,
         sep = ",",
         encoding = "UTF-8")
# read file name
all_filenames <- all_paths %>%
  basename() %>%
  as.list()
# combine file content list and file name list
all_lists <- mapply(c, all_content, all_filenames, SIMPLIFY = FALSE)
# unlist all lists and change column name
obwody <- rbindlist(all_lists, fill = T)
# change column name
names(obwody)[5] <- "File.Path"

rm(all_content, all_filenames, all_lists, all_paths)

#################### obrabiamy dane z UP
obwody <- obwody %>%
  rename(id=1, suma.chorych=2, suma.zgonow=3, suma.wyzdrowien=4, data=5)%>%
  mutate(data=gsub(".csv", "", data), id=gsub(" обл.", "", id))%>%
  mutate(data=ymd(data), suma.aktywnych=suma.chorych-suma.zgonow-suma.wyzdrowien)%>%
  left_join(obwody.lista, by="id")%>%
  select(-c(7:9)) %>%
  select(5,1,7:13,2:4,6)

# łączymy ze starymi obwodami
obwody <- rbind(obwody.stare, obwody)

obwody <- obwody %>%
  group_by(Obwód)%>%
  mutate(nowe.zachorowania = suma.chorych - lag(suma.chorych, default = first(suma.chorych)))%>%
  mutate(nowe.zgony = suma.zgonow - lag(suma.zgonow, default = first(suma.zgonow)))%>%
  mutate(nowe.wyzdrowienia = suma.wyzdrowien - lag(suma.wyzdrowien, default = first(suma.wyzdrowien)))%>%
  mutate(nowi.aktywni = suma.aktywnych - lag(suma.aktywnych, default = first(suma.aktywnych)))%>%
  unite(id2, c(1,5), sep = "_", remove = FALSE) %>%
  ungroup()
  
#wydzielamy pierwszy dzień nowych
pierwsi.chorzy <- nowe.przypadki %>%
  filter(data==min(data))%>%
  select(3)%>%
  pull()
pierwsze.zgony <- nowe.przypadki %>%
  filter(data==min(data))%>%
  select(4)%>%
  pull()
pierwsze.wyzdrowienia <- nowe.przypadki %>%
  filter(data==min(data))%>%
  select(5)%>%
  pull()
pierwsi.aktywni <- nowe.przypadki %>%
  filter(data==min(data))%>%
  select(6)%>%
  pull()

#dodajemy pierwszy dzień do obwodów
obwody[1:25,15] <- pierwsi.chorzy
obwody[1:25,16] <- pierwsze.zgony
obwody[1:25,17] <- pierwsze.wyzdrowienia
obwody[1:25,18] <- pierwsi.aktywni
rm(pierwsi.aktywni, pierwsi.chorzy, pierwsze.wyzdrowienia, pierwsze.zgony, obwody.lista, obwody.stare, nowe.przypadki)

# robimy długie dwie wersje (skumulowane i dzienne)
# do późniejszej modyfikacji - wyrzucamy na razie sumę chorych (zrobię miarę w DAX). Zostawiam zgony, wyleczonych i aktywnych
obwody1 <- obwody %>%
  select(1:10, 12:14)%>%
  pivot_longer(cols = c(11:13), names_to = "skumulowane", values_to = "liczba")
obwody2 <- obwody %>%
  select(1,16:18) %>%
  pivot_longer(cols = c(2:4), names_to = "dzienne", values_to = "ilosc")%>%
  rename(id3=1)

# to jeszcze do późniejszego sprawdzenia, czy jest prawidłowo
obwody <- cbind(obwody1, obwody2)
obwody <- obwody[,-c(1,13)]
rm(obwody1, obwody2)

#zmieniamy nazwy
obwody <- obwody %>%
  mutate(skumulowane = gsub("suma.zgonow", "zgony", skumulowane))%>%
  mutate(skumulowane = gsub("suma.wyzdrowien", "wyleczeni", skumulowane))%>%
  mutate(skumulowane = gsub("suma.aktywnych", "aktywni", skumulowane))%>%
  mutate(dzienne = gsub("nowe.zgony", "zgony", dzienne))%>%
  mutate(dzienne = gsub("nowe.wyzdrowienia", "wyleczeni", dzienne))%>%
  mutate(dzienne = gsub("nowi.aktywni", "aktywni", dzienne))

## problem w BI rozwiązała zmiana nazwy pliku

# test czy się zgadza
sum(obwody$ilosc)

a <- obwody %>%
  filter(data==max(data))
save(obwody, file = "./Ukraina.dane/obwody_dzienne.Rda")
load(file = "E:/R/COVID-19/Ukraina.dane/obwody_dzienne.Rda")
