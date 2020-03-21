library(tidyr)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(ggthemes)

load(file = "E:/R/COVID-19/covid.Rda")

#szukamy państw, które mają powyżej 6000 zachorowań
powyzej.5tys <- covid %>%
  filter(data==max(data), liczba.zachorowan>200)%>%
  filter(Country.Region!="China")

#robimy wektor państw do filtrowania
nazwy <- powyzej.5tys$Państwo

roznica.min <- covid %>%
  filter(Państwo %in% nazwy)%>%
  # to można zmodyfikować, w zależności od wyniku Ukrainy w niedzielę
  filter(liczba.zachorowan>20) %>%
  group_by(Państwo)%>%
  summarise(
    data.min=min(data),
    chorzy=min(liczba.zachorowan)
  )
roznica.max <- covid %>%
  filter(Państwo %in% nazwy)%>%
  # to można zmodyfikować, w zależności od wyniku Ukrainy w niedzielę
  filter(liczba.zachorowan>200) %>%
  group_by(Państwo)%>%
  summarise(
    data.max=min(data),
    chorzy=min(liczba.zachorowan)
  )

roznica <- left_join(roznica.min, roznica.max, by="Państwo")%>%
  mutate(roznica=data.max-data.min)%>%
  mutate(roznica=as.numeric(roznica))

# do ustawienia, w zależności od sytuacji w niedzielę
ggplot()+
  geom_bar(data=roznica, aes(x=Państwo, y=roznica), stat="identity", fill="blue")+
  #można dać medianę, albo średnią
  geom_hline(data=roznica, yintercept = mean(roznica$roznica), color="red", size=2)+
  labs(x="", y="liczba dni")+
  #też może być do modyfikacji
  #scale_y_continuous(breaks=seq(0,10, 2))+
  ggtitle("Ilość dni jaka upłynęła między przekroczeniem 425 i  8000 tysięcy przypadków zakażenia")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot()+
  geom_boxplot(data=roznica, aes(y=roznica))

ggplot() +
  geom_point(data=roznica, aes(x=Państwo, y=roznica)) +
  geom_hline(data=roznica, yintercept = mean(roznica$roznica), color="red", size=2)+
  geom_hline(data=roznica, yintercept = median(roznica$roznica), color="blue", size=2)+
  theme_bw()


# Polska przewidywany wzrost zachorowań w ciągu tygodnia
PL <- covid %>%
  filter(Państwo=="Polska")%>%
  filter(data>=max(data)-4)

srednia.4dni <- mean(PL$proc.zach) +1
srednia.4dni.mediana <- median(PL$proc.zach) +1

ilosc.za.tydzien <- round(bsts::GeometricSequence(11, 425, srednia.4dni),0)
ilosc.za.tydzien.median <- round(bsts::GeometricSequence(10, 425, srednia.4dni.mediana),0)

    