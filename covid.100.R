library(tidyr)
library(dplyr)
library(lubridate)
library(directlabels)

load(file = "E:/R/COVID-19/covid.Rda")

covid.100 <- covid %>%
  filter(liczba.zachorowan>100) %>%
  group_by(Country.Region) %>%
  mutate(id=row_number())

sasiedzi_nazwy <- c("Niemcy", "Czechy", "Słowacja","Ukraina", "Białoruś", "Rosja", "Litwa")

sasiedzi <- covid %>%
  filter(Państwo %in% sasiedzi_nazwy)

wybrane.panstwa.nazwy <- c("Czechy","Niemcy", "Włochy", "Francja", "Hiszpania", "Szwecja", "Polska", "Wielka Brytania", "Singapur", "Japonia")

wybrane.panstwa <- covid.100 %>%
  filter(Państwo %in% wybrane.panstwa.nazwy)

linia.proc <- cumprod(c(100, rep(1.33, max(wybrane.panstwa$id))))
dzien <- seq(from=1,to=length(linia.proc), by=1 )
linia.proc <- data.frame(linia.proc, dzien)


library(scales)
# geom_text(data = "2014"), aes(label = State, colour = State, x = Inf, y = Capex), hjust = -.1)
# geom_dl(aes(label = State), method = list(dl.combine("first.points", "last.points"), cex = 0.8))
ggplot()+
  geom_path(data=wybrane.panstwa, aes(x=id, y=liczba.zachorowan, color=Państwo),size=2, alpha=0.8, show.legend = FALSE)+
  scale_y_continuous(trans='log10', labels = label_number()) + 
  geom_dl(data=wybrane.panstwa, aes(x=id, y=liczba.zachorowan,label = Państwo, color=Państwo), method =  list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  xlim(1, 25) +
  geom_path(data=linia.proc, aes(x=dzien, y=linia.proc), linetype = "dashed", size=1.5) +
  annotate("text", x = 20, y = 30000, label = "zachorowalność 33% dziennie", angle = 30) + 
  theme_bw()