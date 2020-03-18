library(tidyr)
library(dplyr)
library(lubridate)
library(readxl)

chorzy <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
chorzy <- chorzy %>%
  pivot_longer(cols = c(5:length(names(chorzy))), names_to = "data", values_to = "liczba.zachorowan") %>%
  mutate(data=gsub("X", "", data), data=mdy(data)) %>%
  unite(indeks, Province.State, Country.Region, data, sep = "_", remove = FALSE)

ofiary <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
ofiary <- ofiary %>%
  pivot_longer(cols = c(5:length(names(ofiary))), names_to = "data", values_to = "liczba.ofiar") %>%
  mutate(data=gsub("X", "", data), data=mdy(data))%>%
  unite(indeks, Province.State, Country.Region, data, sep = "_", remove = FALSE)
  
wyzdrowienia <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")
wyzdrowienia <- wyzdrowienia %>%
  pivot_longer(cols = c(5:length(names(wyzdrowienia))), names_to = "data", values_to = "liczba.wyzdrowien") %>%
  mutate(data=gsub("X", "", data), data=mdy(data))%>%
  unite(indeks, Province.State, Country.Region, data, sep = "_", remove = FALSE)

covid <- left_join(chorzy, select(ofiary, 1,7), by="indeks")
covid <- left_join(covid, select(wyzdrowienia, 1,7), by="indeks")

covid <- covid %>%
  mutate(nowe.zachorowania = liczba.zachorowan - lag(liczba.zachorowan, default = first(liczba.zachorowan)))%>%
  mutate(nowe.zachorowania = if_else(nowe.zachorowania<0, paste(liczba.zachorowan), paste(nowe.zachorowania))) %>%
  mutate(nowe.zachorowania = as.numeric(nowe.zachorowania)) %>%
  mutate(smiertelnosc = liczba.ofiar/liczba.zachorowan) %>%
  filter(liczba.zachorowan>0) %>%
  # fitrujemy hrabstwa w USA
  filter(grepl("County", Province.State)==FALSE)

chiny <- filter(covid, Country.Region=="China")

hubei <- filter(chiny, Province.State=="Hubei") %>%
  mutate(Country.Region="Hubei")

chiny.bez.hubei <- filter(chiny, Province.State!="Hubei")

#obrabiamy chiny bez hubei. 
chiny.bez.hubei <- chiny.bez.hubei %>%
  group_by(data) %>%
  summarise(
    Lat=33.582751,
    Long=106.270960,
    liczba.zachorowan=sum(liczba.zachorowan),
    liczba.ofiar=sum(liczba.ofiar),
    liczba.wyzdrowien=sum(liczba.wyzdrowien)
  ) %>%
  mutate(nowe.zachorowania = liczba.zachorowan - lag(liczba.zachorowan, default = first(liczba.zachorowan))) %>%
  mutate(smiertelnosc = liczba.ofiar/liczba.zachorowan) %>%
  mutate(Country.Region="China.no.hubei", Province.State="China")%>%
  unite(indeks, Province.State, Country.Region, data, sep = "_", remove = FALSE) %>%
  select(1, 11, 10, 3, 4, 2, 5, 6, 7, 8,9)

#obrabiamy chiny
chiny <- chiny %>%
  group_by(data) %>%
  summarise(
    Lat=33.582751,
    Long=106.270960,
    liczba.zachorowan=sum(liczba.zachorowan),
    liczba.ofiar=sum(liczba.ofiar),
    liczba.wyzdrowien=sum(liczba.wyzdrowien)
  ) %>%
  mutate(nowe.zachorowania = liczba.zachorowan - lag(liczba.zachorowan, default = first(liczba.zachorowan))) %>%
  mutate(smiertelnosc = liczba.ofiar/liczba.zachorowan) %>%
  mutate(Country.Region="China", Province.State="China")%>%
  unite(indeks, Province.State, Country.Region, data, sep = "_", remove = FALSE) %>%
  select(1, 11, 10, 3, 4, 2, 5, 6, 7, 8,9)

USA <- filter(covid, Country.Region=="US")
USA <- USA %>%
  group_by(data) %>%
  summarise(
    Lat=40.250331,
    Long=-99.793280,
    liczba.zachorowan=sum(liczba.zachorowan),
    liczba.ofiar=sum(liczba.ofiar),
    liczba.wyzdrowien=sum(liczba.wyzdrowien)
  ) %>%
  mutate(nowe.zachorowania = liczba.zachorowan - lag(liczba.zachorowan, default = first(liczba.zachorowan))) %>%
  mutate(smiertelnosc = liczba.ofiar/liczba.zachorowan) %>%
  mutate(Country.Region="USA", Province.State="USA")%>%
  unite(indeks, Province.State, Country.Region, data, sep = "_", remove = FALSE) %>%
  select(1, 11, 10, 3, 4, 2, 5, 6, 7, 8,9)

Canada <- filter(covid, Country.Region=="Canada")
Canada <- Canada %>%
  group_by(data) %>%
  summarise(
    Lat=60.920585,
    Long=-107.527655,
    liczba.zachorowan=sum(liczba.zachorowan),
    liczba.ofiar=sum(liczba.ofiar),
    liczba.wyzdrowien=sum(liczba.wyzdrowien)
  ) %>%
  mutate(nowe.zachorowania = liczba.zachorowan - lag(liczba.zachorowan, default = first(liczba.zachorowan))) %>%
  mutate(smiertelnosc = liczba.ofiar/liczba.zachorowan) %>%
  mutate(Country.Region="Canada", Province.State="Canada")%>%
  unite(indeks, Province.State, Country.Region, data, sep = "_", remove = FALSE) %>%
  select(1, 11, 10, 3, 4, 2, 5, 6, 7, 8,9)

Australia <- filter(covid, Country.Region=="Australia")
Australia <- Australia %>%
  group_by(data) %>%
  summarise(
    Lat=-24.116038,
    Long=134.741505,
    liczba.zachorowan=sum(liczba.zachorowan),
    liczba.ofiar=sum(liczba.ofiar),
    liczba.wyzdrowien=sum(liczba.wyzdrowien)
  ) %>%
  mutate(nowe.zachorowania = liczba.zachorowan - lag(liczba.zachorowan, default = first(liczba.zachorowan))) %>%
  mutate(smiertelnosc = liczba.ofiar/liczba.zachorowan) %>%
  mutate(Country.Region="Australia", Province.State="Australia")%>%
  unite(indeks, Province.State, Country.Region, data, sep = "_", remove = FALSE) %>%
  select(1, 11, 10, 3, 4, 2, 5, 6, 7, 8,9)


covid <- filter(covid, Country.Region!="US")
covid <- filter(covid, Country.Region!="China")
covid <- filter(covid, Country.Region!="Canada")
covid <- filter(covid, Country.Region!="Australia")

covid <- covid %>%
  mutate(Country.Region = if_else( Province.State=="Faroe Islands", paste("Faroe Islands"), 
                                   if_else(Province.State=="St Martin", paste("St Martin"),
                                           if_else(Province.State=="Saint Barthelemy", paste("Saint Barthelemy"),
                                                   if_else(Province.State=="Channel Islands", paste("Jersey"),
                                                           if_else(Province.State=="Gibraltar", paste("Gibraltar"),
                                                                   if_else(Province.State=="Gibraltar", paste("Gibraltar"),
                                                                           if_else(Province.State=="French Polynesia", paste("French Polynesia"),
                                                                                   if_else(Province.State=="Fench Guiana", paste("French Guiana"),
                                                                                           as.character(Country.Region))))))))))
covid.chiny <- bind_rows(covid, Australia, USA, Canada, hubei, chiny.bez.hubei)
save(covid.chiny, file = "covid.chiny.Rda")
rm(covid.chiny, hubei, chiny.bez.hubei)

covid <- bind_rows(covid, Australia, USA, chiny, Canada)

#wgrywamy państwa
nazwy <- read_xlsx("nazwy państw.xlsx", sheet = 1)
nazwy <- nazwy %>%
  select(1:3, 5, 9) %>%
  rename(Country.Region = Country) %>%
  mutate(Country.Region=as.factor(Country.Region))

covid <- covid %>%
  mutate(Country.Region = gsub("Iran", "Iran, Islamic Republic of", Country.Region),
         Country.Region = gsub("North Macedonia", "Macedonia, the former Yugoslav Republic of", Country.Region),
         Country.Region = gsub("Brunei", "Brunei Darussalam", Country.Region),
         Country.Region = gsub("Holy See", "Holy See (the)", Country.Region),
         Country.Region = gsub("Korea, South", "Korea, Republic of", Country.Region),
         Country.Region = gsub("Vietnam", "Viet Nam", Country.Region),
         Country.Region = gsub("Russia", "Russian Federation", Country.Region),
         Country.Region = gsub("Moldova", "Moldova, Republic of", Country.Region),
         Country.Region = gsub("St Martin", "Sint Maarten", Country.Region),
         Country.Region = gsub("Congo [(]Kinshasa[])]", "Republic of the Congo", Country.Region),
         Country.Region = gsub("Taiwan[*]", "Taiwan", Country.Region),
         Country.Region = gsub("Cote d'Ivoire", "Côte d’Ivoire", Country.Region),
         Country.Region = gsub("Reunion", "Réunion", Country.Region),
         Country.Region = gsub("Venezuela", "Venezuela, Bolivarian Republic of", Country.Region),
         Country.Region = gsub("Curacao", "Curaçao", Country.Region),
         Country.Region = gsub("USA", "United States", Country.Region),
         Country.Region = gsub("Congo [(]Brazzaville[])]", "Congo", Country.Region),
         Country.Region = gsub("The Bahamas", "Bahamas", Country.Region),
         Country.Region = gsub("The Gambia", "Gambia", Country.Region))

covid <- left_join(covid, nazwy, by="Country.Region")
covid <- covid %>% 
  select(-2) %>%
  mutate(grupa = if_else(Państwo=="Polska"|Państwo=="Słowacja"|Państwo=="Węgry"|Państwo=="Czechy", paste("V4"),
                         if_else(Państwo=="Ukraina"|Państwo=="Białoruś"|Państwo=="Mołdawia", paste("BUM"),
                                 if_else(Państwo=="Litwa"|Państwo=="Łotwa"|Państwo=="Estonia", paste("P. Bałtyckie"),
                                         if_else(Państwo=="Norwegia"|Państwo=="Szwecja"|Państwo=="Finlandia"|Państwo=="Dania"|Państwo=="Islandia", paste("Skandynawia"),
                                                 if_else(Państwo=="Chorwacja"|Państwo=="Słowenia"|Państwo=="Serbia"|Państwo=="Bośnia i Hercegowina"|Państwo=="Albania"|Państwo=="Macedonia Północna"|Państwo=="Bułgaria"|Państwo=="Rumunia"|Państwo=="Kosowo", paste("Bałkany"),
                                                         if_else(Państwo=="Turcja"|Państwo=="Gruzja"|Państwo=="Azerbejdżan"|Państwo=="Armenia", paste("Turcja i Kaukaz"),
                                                                 if_else(Państwo=="Kazachstan"|Państwo=="Turkmenistan"|Państwo=="Tadżykistan"|Państwo=="Kirgistan"|Państwo=="Uzbekistan", paste("Azja Środkowa"),
                                                                         if_else(Państwo=="Rosja", paste("Rosja"),
                                                                                if_else(Państwo=="Niemcy", paste("Niemcy"),
                                                                                        paste("Pozostałe")))))))))))

covid <- covid %>%
  mutate(nowe.zgony = liczba.ofiar - lag(liczba.ofiar, default = first(liczba.ofiar))) %>%
  mutate(nowe.zgony = if_else(nowe.zgony<0, paste(liczba.ofiar), paste(nowe.zgony))) %>%
  mutate(nowe.zgony = as.numeric(nowe.zgony)) %>% 
  #próbujemy dodać procent dziennych zachorowań
  mutate(proc.zach = (liczba.zachorowan/lag(liczba.zachorowan, default = first(liczba.zachorowan)))-1) 


#zostawiam do testowania czy są NA
a <- filter(covid, is.na(Państwo))

rm(a, chorzy, ofiary, wyzdrowienia, nazwy, Australia, USA, Canada, chiny)

# dla Power BI
save(covid, file = "covid.Rda")
load(file = "E:/R/COVID-19/covid.Rda")
