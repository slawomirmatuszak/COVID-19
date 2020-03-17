library(tidyr)
library(dplyr)
library(lubridate)
library(directlabels)
library(ggplot2)
library(ggthemes)
library(scales)

load(file = "E:/R/COVID-19/covid.Rda")

covid.100 <- covid %>%
  filter(liczba.zachorowan>100) %>%
  group_by(Country.Region) %>%
  mutate(id=row_number()-1)


linia.proc <- cumprod(c(100, rep(1.33, max(covid.100$id))))
dzien <- seq(from=0,to=length(linia.proc)-1, by=1 )
linia.proc <- data.frame(linia.proc, dzien)
rm(dzien)

# Polska i wybrane kraje
PL.kraje.nazwy <- c("Hiszpania","Japonia", "Niemcy", "Słowacja","Ukraina", "Białoruś", 
                    "Rosja", "Litwa", "Polska", "Singapur")
PL.kraje <- covid.100 %>%
  filter(Państwo %in% PL.kraje.nazwy)
a<- filter(linia.proc, dzien<max(PL.kraje$id)+1)

#png("wykres2.png", width = 1200, height = 800)
ggplot()+
  geom_path(data=PL.kraje, aes(x=id, y=liczba.zachorowan, color=Państwo),size=2, alpha=0.8)+
  geom_point(data=PL.kraje, aes(x=id, y=liczba.zachorowan, color=Państwo),size=2, alpha=0.8) +
  scale_y_continuous(trans='log10', labels = label_number()) + 
  #geom_dl(data=sasiedzi, aes(x=id, y=liczba.zachorowan,label = Państwo, color=Państwo), method =  list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  xlim(0, 20) +
  labs(y = "liczba zachorowań", x="liczba dni od przekroczenia 100 zachorowań") +
  geom_path(data=a, aes(x=dzien, y=linia.proc), linetype = "dashed", size=1) +
  annotate("text", x = 15, y = 9000, label = "33% dziennie nowych zachorowań", angle = 27, size=6) + 
  theme_bw(base_size=25) +
  theme(legend.position=c(0.1,0.8))

#dev.off()

#Zachód
zachod.nazwy <- c("Niemcy", "Włochy", "Francja", "Hiszpania", "Szwecja", "Polska", 
                           "Wielka Brytania")
zachod <- covid.100 %>%
  filter(Państwo %in% zachod.nazwy)
a<- filter(linia.proc, dzien<max(zachod$id)+1)
ggplot()+
  geom_path(data=zachod, aes(x=id, y=liczba.zachorowan, color=Państwo),size=2, alpha=0.8)+
  scale_y_continuous(trans='log10', labels = label_number()) + 
  #geom_dl(data=wybrane.panstwa, aes(x=id, y=liczba.zachorowan,label = Państwo, color=Państwo), method =  list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  xlim(0, max(zachod$id)) +
  labs(y = "liczba zachorowań", x="liczba dni od przekroczenia 100 zachorowań", color=NULL) +
  geom_path(data=a, aes(x=dzien, y=linia.proc), linetype = "dashed", size=1) +
  #annotate("text", x = 20, y = 30000, label = "zachorowalność 33% dziennie", angle = 30) + 
  theme_bw()+
  theme(legend.position=c(0.8,0.2))

#sąsiedzi
sasiedzi.nazwy <-c("Polska","Niemcy", "Czechy", "Słowacja","Ukraina", "Białoruś", 
                   "Litwa", "Rosja")

sasiedzi <- covid.100 %>%
  filter(Państwo %in% sasiedzi.nazwy)
a<- filter(linia.proc, dzien<max(sasiedzi$id)+1)
ggplot() +
  geom_path(data=sasiedzi, aes(x=id, y=liczba.zachorowan, color=Państwo),size=2, alpha=0.8)+
  scale_y_continuous(trans='log10', labels = label_number()) + 
  #geom_dl(data=wybrane.panstwa, aes(x=id, y=liczba.zachorowan,label = Państwo, color=Państwo), method =  list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  xlim(0, max(sasiedzi$id)) +
  labs(y = "liczba zachorowań", x="liczba dni od przekroczenia 100 zachorowań", color=NULL) +
  geom_path(data=a, aes(x=dzien, y=linia.proc), linetype = "dashed", size=1) +
  #annotate("text", x = 20, y = 30000, label = "zachorowalność 33% dziennie", angle = 30) + 
  theme_bw()+
  theme(legend.position=c(0.8,0.2))
