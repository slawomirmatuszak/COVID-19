library(tidyr)
library(dplyr)
library(lubridate)
library(readxl)

## należy najpierw odpalić skrypt dane_skumulowane.R

load(file = "E:/R/COVID-19/covid.chiny.Rda")

#wgrywamy państwa
nazwy <- read_xlsx("nazwy państw.xlsx", sheet = 1)
nazwy <- nazwy %>%
  select(1:3, 5, 9) %>%
  rename(Country.Region = Country) %>%
  mutate(Country.Region=as.factor(Country.Region))

covid.chiny <- covid.chiny %>%
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
         Country.Region = gsub("The Bahamas", "Bahamas", Country.Region))

covid.chiny <- left_join(covid.chiny, nazwy, by="Country.Region")
covid.chiny <- covid.chiny %>% 
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

covid.chiny <- covid.chiny %>%
  mutate(nowe.zgony = liczba.ofiar - lag(liczba.ofiar, default = first(liczba.ofiar))) %>%
  mutate(nowe.zgony = if_else(nowe.zgony<0, paste(liczba.ofiar), paste(nowe.zgony))) %>%
  mutate(nowe.zgony = as.numeric(nowe.zgony)) %>% 
  #próbujemy dodać procent dziennych zachorowań
  mutate(proc.zach = (liczba.zachorowan/lag(liczba.zachorowan, default = first(liczba.zachorowan)))-1) 

#zostawiam do testowania czy są NA
a <- filter(covid.chiny, is.na(Państwo))

rm(a, nazwy)
save(covid.chiny, file = "covid.chiny.Rda")