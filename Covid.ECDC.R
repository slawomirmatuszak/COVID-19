library(tidyverse)
library(lubridate)
library(readxl)
library(httr)


# pobór long i lat --------------------------------------------------------

# bierzemy nazwy i long lat z Hopkinsa
load(file = "E:/R/COVID-19/covid2.Rda")

kraje <- covid %>% 
  select(1:4, 10:14)%>%
  rename(countries = 2)%>%
  mutate(indeks = gsub("[0-9]", "", indeks)) %>%
  unique()

kraje.filtr <- c("United Kingdom", "Netherlands", "France", "Denmark")

oczyszczone <- filter(kraje, countries %in% kraje.filtr) %>%
  filter(indeks=="_Denmark_--"|indeks=="_France_--"|indeks=="_Netherlands_--"| indeks=="_United Kingdom_--")

kraje <- kraje %>%
  filter(!countries %in% kraje.filtr)%>%
  bind_rows(oczyszczone) %>%
  select(-1)

rm(oczyszczone, kraje.filtr, covid)


# stary kod ---------------------------------------------------------------

# fragment skryptu stąd 
# https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide

#create the URL where the dataset is stored with automatic updates every day

#url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.xlsx", sep = "")

#download the dataset from the website to a local temporary file

#GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))

#read the Dataset sheet into “R”

#covid.ECDC <- read_excel(tf)
#rm(tf, url)

#coś przestało działać, próbujemy innego rozwiązania
#covid.ECD <- read_xlsx(path="https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.xlsx")

# jeszcze inne rozwiązanie


# aktualny kod do poboru danych -------------------------------------------

#these libraries need to be loaded
library(utils)
#read the Dataset sheet into “R”. The dataset will be called "data".
data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")
# działa, ale trzeba poprawić skrypt w części z datą.

#obrabiamy dane 

covid.ECDC <- data %>%
  rename(data=1, countries=7, ISO2=8, ISO3= 9, population=10) %>%
  mutate(data=dmy(data))%>%
  mutate(countries = gsub("_", " ", countries))%>%
  mutate(population = if_else(countries=="CANADA", as.numeric(paste("37058856")),
                              as.numeric(paste(population))))  %>%
  mutate(ISO3 = if_else(countries=="CANADA", paste("CAN"), paste(ISO3)))  %>%
  # można więcej tego zrobić, ale ograniczyłem się tylko do głównych
  mutate(countries = gsub("CANADA", "Canada", countries)) %>%
  mutate(countries = gsub("Iran", "Iran, Islamic Republic of", countries),
         countries = gsub("Cases on an international conveyance Japan", "Diamond Princess", countries),
         countries = gsub("Cote dIvoire", "Côte d’Ivoire", countries),
         countries = gsub("Czech Republic", "Czechia", countries),
         countries = gsub("Democratic Republic of the Congo", "Republic of the Congo", countries),
         countries = gsub("Holy See", "Holy See (the)", countries),
         countries = gsub("Laos", "Lao People’s Democratic Republic", countries),
         countries = gsub("Libya", "Libyan Arab Jamahiriya", countries),
         countries = gsub("Moldova", "Moldova, Republic of", countries),
         countries = gsub("Russia", "Russian Federation", countries),
         countries = gsub("South Korea", "Korea, Republic of", countries),
         countries = gsub("Timor Leste", "Timor-Leste", countries),
         countries = gsub("Venezuela", "Venezuela, Bolivarian Republic of", countries),
         countries = gsub("Vietnam", "Viet Nam", countries),
         countries = gsub("North Macedonia", "Macedonia, the former Yugoslav Republic of", countries),
         countries = gsub("Syria", "Syrian Arab Republic", countries),
         countries = gsub("United States of America", "United States", countries))

#test państw na NA
a <- filter(covid.ECDC, is.na(population))

a <- left_join(covid.ECDC, kraje, by="countries") %>%
  filter(ISO3.x=="NA")

covid.ECDC <- left_join(covid.ECDC, select(kraje, -5), by="countries")

# obliczamy skumulowane przypadki i procentowy przyrost
covid.ECDC <- covid.ECDC %>%
  select(1:4, 7:16, 5,6)%>%
  arrange(countries, data) %>%
  group_by(countries)%>%
  mutate(suma.chorych=cumsum(cases), suma.zgonow=cumsum(deaths))%>%
  ungroup() %>%
  # zachorowania i zgony wobec liczby ludności
  mutate(proc.chorych = suma.chorych/population, proc.zgonow = suma.zgonow/population) %>%
  # to jeszcze do weryfikacji, ale wygląda, że data jest na dzień opóźniona
  mutate(data = data-1) %>%
  select(-c(2:4)) %>%
  group_by(countries) %>%
  mutate(proc.wzrostu.chorych = (suma.chorych/lag(suma.chorych, default = first(suma.chorych)))-1) %>%
  mutate(proc.wzrostu.zgonow = (suma.zgonow/lag(suma.zgonow, default = first(suma.zgonow)))-1)%>%
  filter(suma.chorych>0)

save(covid.ECDC, file = "covid.ECDC.Rda")
load(file = "E:/R/COVID-19/covid.ECDC.Rda")

