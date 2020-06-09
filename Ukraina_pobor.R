library(tidyverse)
library(lubridate)


# miejscowości ----------------------------------------------------------

load(file = "E:/R/COVID-19/Ukraina.dane/obwody.lista.Rda")

obwody <- obwody%>%
  select(Obwód, id)%>%
  rename(registration_area=id)

miejscowosci <- read_csv("https://raw.githubusercontent.com/VasiaPiven/covid19_ua/master/covid19_by_settlement_actual.csv")%>%
  left_join(obwody, by="registration_area")


save(miejscowosci, file="miejscowosci.Rda")

# pobór i obróbka danych na poziomie szpitali -----------------------------

szpitale2 <- read_csv("https://raw.githubusercontent.com/VasiaPiven/covid19_ua/master/covid19_by_area_type_hosp_dynamics.csv")

szpitale.suma <- szpitale2 %>%
  mutate(data=ymd(zvit_date))%>%
  group_by(person_gender, person_age_group, add_conditions)%>%
  summarise_if(is.numeric, sum)%>%
  ungroup()%>%
  mutate(smiertelnosc = new_death/new_confirm)%>%
  mutate(add_conditions = gsub("Так", "choroby współistniejące", add_conditions))%>%
  mutate(add_conditions = gsub("Ні", "brak chorób współistniejących", add_conditions))%>%
  mutate(person_gender = gsub("Жіноча", "kobiety", person_gender))%>%
  mutate(person_gender = gsub("Чоловіча", "mężczyźni", person_gender))%>%
  mutate(person_gender = gsub("Уточнюється", "b.d.", person_gender))%>%
  mutate(person_age_group = gsub("Уточнюється", "b.d.", person_age_group))

save(szpitale.suma, file = "szpitale.suma.github.Rda")

# obróbka z podziałem na obwody

load(file = "E:/R/COVID-19/Ukraina.dane/obwody.lista.Rda")
obwody.lista <- obwody %>%
  # trzeba poprawic współrzędne Kijowa, bo nie widać go na mapie
  select(1:4,6,7)
obwody.lista[13,1] <- 50.777435
obwody.lista[13,2] <- 30.167475
rm(obwody)

szpitale.obwody <- szpitale2 %>%
  mutate(izolacja= if_else(edrpou_hosp=="Самоізоляція", paste("izolacja"), paste("szpital")))%>%
  group_by(zvit_date, registration_area, izolacja)%>%
  summarise_if(is.numeric, sum)%>%
  rename(id=2)%>%
  left_join(obwody.lista, by="id")%>%
  mutate(lat = as.numeric(lat), long = as.numeric(long))%>%
  mutate(data=ymd(zvit_date))

save(szpitale.obwody, file = "szpitale.obwody.Rda")

#trzeba potem dodać pracowników medycznych
load(file = "E:/R/COVID-19/Ukraina.dane/obwody.lista.Rda")

obwody.lista <- obwody %>%
  # trzeba poprawic współrzędne Kijowa, bo nie widać go na mapie
  select(1:4,6,7)
obwody.lista[13,1] <- 50.777435
obwody.lista[13,2] <- 30.167475
rm(obwody)

szpitale.medycy <- szpitale2 %>%
  mutate(izolacja= if_else(edrpou_hosp=="Самоізоляція", paste("izolacja"), paste("szpital")))%>%
  mutate(medycy= if_else(is_medical_worker=="Так", paste("tak"), paste("nie")))%>%
  group_by(zvit_date, registration_area, izolacja, medycy)%>%
  summarise_if(is.numeric, sum)%>%
  rename(id=2)%>%
  left_join(obwody.lista, by="id")%>%
  mutate(lat = as.numeric(lat), long = as.numeric(long))%>%
  mutate(data=ymd(zvit_date))%>%
  ungroup()

save(szpitale.medycy, file = "szpitale.medycy.Rda")
