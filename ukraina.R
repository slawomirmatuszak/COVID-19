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

UA_obwody <- read.csv2("./Ukraina.dane/obwody/2020.03.18.Tablica_data.csv", encoding = "UTF-8", stringsAsFactors = F)%>%
  rename(dane=1, id=2, ilosc=3)%>%
  mutate(id=as.character(id))

UA_obwody <- left_join(UA_obwody, obwody, by="id")

save(UA_obwody, file = "./Ukraina.dane/obwody.Rda")

load(file = "E:/R/COVID-19/Ukraina.dane/obwody.Rda")
