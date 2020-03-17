library(tidyr)
library(dplyr)
library(lubridate)
library(directlabels)
library(ggplot2)
library(ggthemes)
library(scales)

load(file = "E:/R/COVID-19/covid.chiny.Rda")

covid.1000 <- covid.chiny %>%
  filter(liczba.zachorowan>1000) %>%
  group_by(Country.Region) %>%
  mutate(id=row_number()-1, id.test=id>4)

kraje <- unique(covid.1000$Państwo)
kraje.test <- filter(covid.1000, id.test==TRUE)
kraje.test <-unique(kraje.test$Państwo)

covid.1000 <- filter(covid.1000,Państwo %in% kraje.test)
rm(kraje, kraje.test)


linia.proc1000 <- cumprod(c(1000, rep(1.33, max(covid.100$id))))
dzien <- seq(from=0,to=length(linia.proc1000)-1, by=1 )
linia.proc1000 <- data.frame(linia.proc1000, dzien)


linia.20 <- cumprod(c(1000, rep(1.2, max(covid.100$id))))
dzien <- seq(from=0,to=length(linia.20)-1, by=1 )
linia.20 <- data.frame(linia.20, dzien)
rm(dzien)

ggplot()+
  geom_path(data=covid.1000, aes(x=id, y=liczba.zachorowan, color=Państwo),size=2, alpha=0.8)+
  scale_y_continuous(trans='log10', labels = label_number()) + 
  #geom_dl(data=wybrane.panstwa, aes(x=id, y=liczba.zachorowan,label = Państwo, color=Państwo), method =  list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  xlim(0, 20) +
  labs(y = "liczba zachorowań", x="liczba dni od przekroczenia 1000 zachorowań", color=NULL) +
  geom_path(data=filter(linia.proc1000, dzien<21), aes(x=dzien, y=linia.proc1000), linetype = "dashed", size=1) +
  #annotate("text", x = 20, y = 30000, label = "zachorowalność 33% dziennie", angle = 30) + 
  theme_bw()+
  theme(legend.position=c(0.1,0.8))

#skala nielogarytmiczna
ggplot()+
  geom_path(data=covid.1000, aes(x=id, y=liczba.zachorowan, color=Państwo),size=2, alpha=0.8)+
  scale_y_continuous( labels = label_number()) + 
  #geom_dl(data=wybrane.panstwa, aes(x=id, y=liczba.zachorowan,label = Państwo, color=Państwo), method =  list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  xlim(0, 20) +
  labs(y = "liczba zachorowań", x="liczba dni od przekroczenia 1000 zachorowań", color=NULL) +
  geom_path(data=filter(linia.proc1000, dzien<21), aes(x=dzien, y=linia.proc1000), linetype = "dashed", size=1) +
  #annotate("text", x = 20, y = 30000, label = "zachorowalność 33% dziennie", angle = 30) + 
  theme_bw()+
  theme(legend.position=c(0.2,0.8))

# zachorowalność 20%
ggplot()+
  geom_path(data=covid.1000, aes(x=id, y=liczba.zachorowan, color=Państwo),size=2, alpha=0.8)+
  scale_y_continuous(trans='log10', labels = label_number()) + 
  #geom_dl(data=wybrane.panstwa, aes(x=id, y=liczba.zachorowan,label = Państwo, color=Państwo), method =  list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  xlim(0, 20) +
  labs(y = "liczba zachorowań", x="liczba dni od przekroczenia 1000 zachorowań", color=NULL) +
  geom_path(data=filter(linia.20, dzien<21), aes(x=dzien, y=linia.20), linetype = "dashed", size=1) +
  #annotate("text", x = 20, y = 30000, label = "zachorowalność 33% dziennie", angle = 30) + 
  theme_bw()+
  theme(legend.position=c(0.1,0.8))

# nielogarytmiczne
ggplot()+
  geom_path(data=covid.1000, aes(x=id, y=liczba.zachorowan, color=Państwo),size=2, alpha=0.8)+
  scale_y_continuous(labels = label_number()) + 
  #geom_dl(data=wybrane.panstwa, aes(x=id, y=liczba.zachorowan,label = Państwo, color=Państwo), method =  list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  xlim(0, 20) +
  labs(y = "liczba zachorowań", x="liczba dni od przekroczenia 1000 zachorowań", color=NULL) +
  geom_path(data=filter(linia.20, dzien<21), aes(x=dzien, y=linia.20), linetype = "dashed", size=1) +
  #annotate("text", x = 20, y = 30000, label = "zachorowalność 33% dziennie", angle = 30) + 
  theme_bw()+
  theme(legend.position=c(0.1,0.8))

# liczba zgonów
ggplot()+
  geom_path(data=covid.1000, aes(x=id, y=liczba.ofiar, color=Państwo),size=2, alpha=0.8)+
  scale_y_continuous(trans='log10',labels = label_number()) + 
  xlim(0, 20) +
  labs(y = "liczba zgonów", x="liczba dni od przekroczenia 1000 zachorowań", color=NULL) +
  theme_bw()+
  theme(legend.position=c(0.1,0.8))

# śmiertelność
ggplot()+
  geom_path(data=covid.1000, aes(x=id, y=smiertelnosc, color=Państwo),size=2, alpha=0.8)+
  geom_point(data=covid.1000, aes(x=id, y=smiertelnosc, color=Państwo), size = 3)+
  scale_y_continuous(labels = label_percent()) + 
  xlim(0, 20) +
  labs(y = "śmiertelność", x="liczba dni od przekroczenia 1000 zachorowań", color=NULL) +
  theme_bw()
  #theme(legend.position=c(0.1,0.8))

