library(data.table)
library(tidyr)
library(dplyr)
library(lubridate)
library(readxl)

################################################################################################## 
#skrypt do obróbki danych o szpitalach.

szpitale <- read.csv("https://covid19.gov.ua/csv/data.csv", encoding = "UTF-8")

a <- szpitale %>%
  select(4, 5, 26:31,33:41)%>%
  rename(Obwod=1, data=2, lozka.suma=3, infekcyjne=4, infekcyjne.zajete=5, reanimacyjne=6, reanimacyjne.zajete=7,
         lozka.zajete=8, covid=9, resp.wys=10, resp.wys.podl=11, resp.sr=12, resp.sr.podl=13, resp.dzieci=14,
         resp.dzieci.podl=15, resp.przenosne=16, resp.zepsute=17)%>%
  separate(col=data, into=c("data", NA), sep=" ")%>%
  mutate(data=dmy(data))%>%
  filter(data!=Sys.Date())%>%
  group_by(Obwod, data)%>%
  summarise_all(funs(sum))%>%
  arrange(Obwod, data)