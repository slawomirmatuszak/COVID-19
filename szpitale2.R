# nowy skrypt dla szpitali

library(tidyr)
library(dplyr)
library(lubridate)
library(readxl)
library(directlabels)
library(ggplot2)
library(ggthemes)
library(scales)

nowy <- read_xlsx("./Ukraina.dane/dobowe.dane.2/2020.04.06.xlsx")%>%
  rename(adres = 4)%>%
  separate(adres, into = c("miasto", "ulica", "numer"), sep = ",")

miasta <- read_xlsx("E:/Power BI/Listy/miasta Ukraina.xlsx", col_names = F)%>%
  rename(rodzaj=1, miasto=2, Obwód=3)%>%
  fill(rodzaj, .direction = "down")%>%
  mutate(rodzaj2 = if_else(rodzaj=="Селища міського типу", paste("смт."), paste("м.")))%>%
  unite(miasto, rodzaj2, miasto, sep = " ")

szpitale <- left_join(nowy, miasta, by="miasto")

szpitale <- szpitale %>%
  mutate(Obwód = if_else(miasto=="м. Глибока", paste("Чернівецька"), paste(Obwód)))%>%
  mutate(Obwód = if_else(miasto=="м. Кам’янка", paste("Черкаська"), paste(Obwód)))%>%
  mutate(Obwód = if_else(miasto=="м. Макарів", paste("Київська"), paste(Obwód)))%>%
  mutate(Obwód = if_else(miasto=="м.Сокиряни", paste("Чернівецька"), paste(Obwód)))%>%
  mutate(Obwód = if_else(miasto=="м. Хмельницький ", paste("Хмельницька"), paste(Obwód)))%>%
  mutate(Obwód = if_else(miasto=="м. Суми. вул. Троїцька", paste("Сумська"), paste(Obwód)))
  
  
  
# test NA
a <-filter( szpitale, Obwód=="NA")

szpitale <- filter(szpitale, Obwód!="NA") %>%
  rename(id=13)

#lista obwodów
load(file = "E:/R/COVID-19/Ukraina.dane/obwody.lista.Rda")
obwody <- obwody %>%
  select(4:11)

szpitale <- left_join(szpitale, obwody, by="id") %>%
  select(-c(1:2))%>%
  rename(chorzy=6, podejrzani=7)

wykres1 <- szpitale %>%
  select(12,6,7)%>%
  pivot_longer(c(2:3), names_to = "stan", values_to = "liczba")

colors <- c("podejrzani"="orange", "chorzy"="red")
#png("szpitale.png", units="in", width=11, height=6, res=600)
ggplot(data=wykres1, aes(x=reorder(Obwód, -liczba), y=liczba))+
  geom_bar(aes( fill=stan), stat="identity")+
  labs(x="obwód", y="liczba łóżek", fill="",
       title = "Sytuacja w szpitalach na Ukrainie",
       subtitle = "wg stanu na 30 marca",
       caption = "Źródło: Departament Polityki Regionalnej i Decentralizacji Biura Prezydenta Ukrainy.")+
  scale_fill_manual(values = colors)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5) , legend.direction = "horizontal", legend.position=c(0.8,0.9), plot.caption = element_text(hjust = 0, size = 8))
#dev.off()
  