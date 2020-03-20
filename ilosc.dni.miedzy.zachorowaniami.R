library(tidyr)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(ggthemes)

load(file = "E:/R/COVID-19/covid.Rda")

#szukamy państw, które mają powyżej 6000 zachorowań
powyzej.5tys <- covid %>%
  filter(data==max(data), liczba.zachorowan>6000)%>%
  filter(Country.Region!="China")

#robimy wektor państw do filtrowania
nazwy <- powyzej.5tys$Państwo

roznica <- covid %>%
  filter(Państwo %in% nazwy)%>%
  # to można zmodyfikować, w zależności od wyniku Ukrainy w niedzielę
  filter(liczba.zachorowan>20) %>%
  filter(liczba.zachorowan<=6000)%>%
  group_by(Państwo)%>%
  summarise(
    ilosc.dni=max(data)-min(data)
  )%>%
  mutate(ilosc.dni=as.numeric(ilosc.dni))

ggplot()+
  geom_bar(data=roznica, aes(x=Państwo, y=ilosc.dni), stat="identity", fill="blue")+
  geom_hline(data=roznica, yintercept = median(roznica$ilosc.dni), color="red", size=2)+
  labs(x="", y="liczba dni")+
  ggtitle("Ilość dni jaka upłynęła między x i 6000 tysięcy zachorowań")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

    