library(tidyr)
library(dplyr)
library(lubridate)

chorzy <- read.csv("./csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
chorzy <- chorzy %>%
  pivot_longer(cols = c(5:length(names(chorzy))), names_to = "data", values_to = "liczba.zachorowan") %>%
  mutate(data=gsub("X", "", data), data=mdy(data)) %>%
  unite(indeks, Province.State, Country.Region, data, sep = "_", remove = FALSE)

ofiary <- read.csv("./csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
ofiary <- ofiary %>%
  pivot_longer(cols = c(5:length(names(ofiary))), names_to = "data", values_to = "liczba.ofiar") %>%
  mutate(data=gsub("X", "", data), data=mdy(data))%>%
  unite(indeks, Province.State, Country.Region, data, sep = "_", remove = FALSE)
  
wyzdrowienia <- read.csv("./csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")
wyzdrowienia <- wyzdrowienia %>%
  pivot_longer(cols = c(5:length(names(wyzdrowienia))), names_to = "data", values_to = "liczba.wyzdrowien") %>%
  mutate(data=gsub("X", "", data), data=mdy(data))%>%
  unite(indeks, Province.State, Country.Region, data, sep = "_", remove = FALSE)

covid <- left_join(chorzy, select(ofiary, 1,7), by="indeks")
covid <- left_join(covid, select(wyzdrowienia, 1,7), by="indeks")

covid <- covid %>%
  mutate(nowe.zachorowania = liczba.zachorowan - lag(liczba.zachorowan, default = first(liczba.zachorowan)))%>%
  mutate(nowe.zachorowania = if_else(nowe.zachorowania<0, paste(liczba.zachorowan), paste(nowe.zachorowania))) %>%
  mutate(smiertelnosc = liczba.ofiar/liczba.zachorowan) %>%
  filter(liczba.zachorowan>0)

# dla Power BI
save(covid, file = "covid.Rda")
load(file = "E:/R/COVID-19/covid.Rda")

