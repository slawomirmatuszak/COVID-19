library(tidyverse)
library(lubridate)
library(scales)
load(file = "E:/R/COVID-19/covid.ECDC.Rda")
load(file = "E:/R/COVID-19/covid2.Rda")


# test Białoruś na tle UE
ECDC2 <- covid.ECDC%>%
  ungroup()%>%
  select(ISO3, population)%>%
  unique()

a <- covid%>%
  #filter(Państwo=="Białoruś"|Państwo=="Szwajcaria"|`Blok Ekonomiczny`=="Unia Europejska")%>%
  filter(Kontynenty=="Europa")%>%
  left_join(ECDC2, by="ISO3")%>%
  filter(population>=250000)%>%
  mutate(zach.100 = nowe.zachorowania*100000/population)%>%
  group_by(Państwo)%>%
  mutate(srednia= zoo::rollmean(zach.100, k=7, fill=NA, align="right"))%>%
  filter(srednia>0.1)%>%
  mutate(id=row_number())%>%
  mutate(by=if_else(Państwo=="Białoruś", paste("tak"), paste("nie")))

linia1 <-filter(a, Państwo=="Białoruś", id==max(id))%>%
  ungroup()%>%
  select(srednia)%>%
  pull()
id2 <- a %>%
  filter(Państwo=="Białoruś", id==max(id))%>%
  ungroup()%>%
  select(id)%>%
  pull()

kolejnosc <- a %>%
  filter(id==id2)%>%
  arrange(desc(srednia))%>%
  select(Państwo)

data.by <- a %>%
  filter(Państwo=="Białoruś", id==max(id))%>%
  ungroup()%>%
  select(data)%>%
  pull()

a$Państwo <- ordered(a$Państwo, levels = kolejnosc$Państwo)

#png("bialorus.png", units="in", width=9, height=9, res=600)
ggplot(filter(a), aes(x=id, y=srednia, color=by))+
  geom_point(aes(x=id, y=zach.100), color="orange")+
  geom_path(size=1.5, alpha=0.8, show.legend = F) +
  facet_wrap(~Państwo, ncol=8)+
  coord_cartesian(xlim=c(0,sum(a$Państwo=="Białoruś")-1), ylim=c(0,17))+
  scale_color_manual(values = c("tak"="red", "nie"="blue"))+
  geom_hline(aes(yintercept = linia1, linetype=""), color="red4")+
  labs(x="liczba dni od przekroczenia 0,1 zakażenia na 100 tys. mieszkańców", 
       y="dzienny przyrost",
       color="",
       title = "Liczba dziennych zakażeń na 100 tys. mieszkańców",
       #subtitle = paste( "stan na", format(as.Date(UA1$data), "%d/%m/%Y")),
       caption = "Źródło: Center for Systems Science and Engineering at Johns Hopkins University")+
  scale_linetype_manual(name = "", values = "longdash", labels = paste("poziom przyrostu zakażeń na Białorusi\nstan na ",format(data.by,"%d %B %Y")))+
  theme_bw()+
  theme(legend.position = "top", plot.caption = element_text( size = 8), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(colour = "grey", size = 0.5), strip.background = element_rect(fill="grey90"))
#dev.off()

#sama białoruś
a <- a %>%
  filter(Państwo=="Białoruś")

png("bialorus8 maja.png", units="in", width=7, height=7, res=600)
ggplot(filter(a), aes(x=data, y=srednia))+
  geom_point(aes(x=data, y=zach.100), color="orange")+
  geom_path(size=1.5, alpha=0.8, show.legend = F, color="blue") +
  geom_smooth(span=0.4, size=1, se=F)+
  #facet_wrap(~Państwo, ncol=8)+
  #coord_cartesian(xlim=c(0,sum(a$Państwo=="Białoruś")-1), ylim=c(0,17))+
  #scale_color_manual(values = c("tak"="red", "nie"="blue"))+
  #geom_hline(aes(yintercept = linia1, linetype=""), color="red4")+
  labs(x="", 
       y="dzienny przyrost nowych przypadków na 100 tys. mieszkańców",
       color="",
       title = "Dynamika przyrostu nowych zakażeń na Białorusi",
       #subtitle = paste( "stan na", format(as.Date(UA1$data), "%d/%m/%Y")),
       caption = "Źródło: Center for Systems Science and Engineering at Johns Hopkins University")+
  #scale_linetype_manual(name = "", values = "longdash", labels = paste("poziom przyrostu zakażeń na Białorusi\nstan na ",format(data.by,"%d %B %Y")))+
  theme_bw()+
  theme(legend.position = "top", plot.caption = element_text( size = 8), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(colour = "grey", size = 0.5), strip.background = element_rect(fill="grey90"))
dev.off()

## to samo, tylko z datą na osi x
a <- covid%>%
  filter(Państwo=="Białoruś"|Państwo=="Szwajcaria"|`Blok Ekonomiczny`=="Unia Europejska")%>%
  separate(indeks, into = "region", sep="_")%>%
  filter(region=="")%>%
  left_join(ECDC2, by="ISO3")%>%
  mutate(zach.100 = nowe.zachorowania*100000/population)%>%
  group_by(Państwo)%>%
  mutate(srednia= zoo::rollmean(zach.100, k=7, fill=NA, align="right"))%>%
  filter(srednia!=is.na(srednia))
  

ggplot(filter(a), aes(x=data, y=srednia, color=by))+
  geom_path(size=1.5, alpha=0.8) +
  facet_wrap(~Państwo, ncol=5, scales="free_y")+
  #coord_cartesian(ylim=c(0,30))+
  #geom_hline(yintercept = linia1), linetype="dashed", color="chocolate3")+
  labs(x="liczba dni od przekroczenia 0,1 zakażenia na 100 tys. mieszkańców", 
       y="dzienny przyrost",
       color="",
       title = "Liczba dziennych zakażeń na 100 tys. mieszkańców",
       #subtitle = paste( "stan na", format(as.Date(UA1$data), "%d/%m/%Y")),
       caption = "Źródło: Center for Systems Science and Engineering at Johns Hopkins University")+
  theme_bw()+
  theme(legend.position = "none", plot.caption = element_text( size = 8), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(colour = "grey", size = 0.5))

############################################################################################################################################
# Białoruś i dane ECDC
a <- covid.ECDC%>%
  filter(Kontynenty=="Europa")%>%
  #filter(population>=250000)%>%
  mutate(zach.100 = cases*100000/population)%>%
  group_by(Państwo)%>%
  mutate(srednia= zoo::rollmean(zach.100, k=7, fill=NA, align="right"))%>%
  filter(srednia>0.1)%>%
  mutate(id=row_number())%>%
  mutate(by=if_else(Państwo=="Białoruś", paste("tak"), paste("nie")))

linia1 <-filter(a, Państwo=="Białoruś", id==max(id))%>%
  ungroup()%>%
  select(srednia)%>%
  pull()

kolejnosc <- a %>%
  filter(srednia==max(srednia))%>%
  arrange(desc(srednia))%>%
  select(Państwo)%>%
  unique()

data.by <- a %>%
  filter(Państwo=="Białoruś", id==max(id))%>%
  ungroup()%>%
  select(data)%>%
  pull()

a$Państwo <- ordered(a$Państwo, levels = kolejnosc$Państwo)

png("bialorus.ECDC.3maj.png", units="in", width=12, height=8, res=600)
ggplot(filter(a), aes(x=data, y=srednia, color=by))+
  geom_path(size=1.5, alpha=0.8, show.legend=F) +
  facet_wrap(~Państwo, ncol=8, scales=)+
  scale_color_manual(values = c("tak"="red", "nie"="blue"))+
  #coord_cartesian(ylim=c(0,30))+
  geom_hline(aes(yintercept = linia1, linetype=""), color="red4")+
  labs(x="", 
       y="dzienny przyrost nowych przypadkóW",
       color="",
       title = "Liczba dziennych zakażeń na 100 tys. mieszkańców",
       #subtitle = paste( "stan na", format(as.Date(UA1$data), "%d/%m/%Y")),
       caption = "Źródło: European Centre for Disease Prevention and Control")+
  scale_linetype_manual(name = "", values = "longdash", labels = paste("poziom przyrostu zakażeń na Białorusi\nstan na ",format(data.by,"%d %B %Y")))+
  scale_x_date(date_breaks = "1 month",labels = date_format("%b"))+
  theme_bw()+
  theme(legend.position = "top", plot.caption = element_text( size = 8), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(colour = "grey", size = 0.5))
dev.off()

# to samo z id
ggplot(filter(a), aes(x=id, y=srednia, color=by))+
  geom_path(size=1.5, alpha=0.8, show.legend=F) +
  facet_wrap(~Państwo, ncol=8, scales=)+
  scale_color_manual(values = c("tak"="red", "nie"="blue"))+
  #coord_cartesian(ylim=c(0,30))+
  geom_hline(aes(yintercept = linia1, linetype=""), color="red4")+
  labs(x="liczba dni od przekroczenia poziomu 0,1 zakażenia na 100 tys. mieszkańców", 
       y="dzienny przyrost nowych przypadkóW",
       color="",
       title = "Liczba dziennych zakażeń na 100 tys. mieszkańców",
       #subtitle = paste( "stan na", format(as.Date(UA1$data), "%d/%m/%Y")),
       caption = "Źródło: European Centre for Disease Prevention and Control")+
  scale_linetype_manual(name = "", values = "longdash", labels = paste("poziom przyrostu zakażeń na Białorusi\nstan na ",format(data.by,"%d %B %Y")))+
  theme_bw()+
  theme(legend.position = "top", plot.caption = element_text( size = 8), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(colour = "grey", size = 0.5))


## scatterplot z samą białorusią
a <- covid.ECDC%>%
  filter(Państwo=="Białoruś")%>%
  mutate(zach.100 = cases*100000/population)%>%
  mutate(srednia= zoo::rollmean(zach.100, k=7, fill=NA, align="right"))%>%
  mutate(srednia.nowe= zoo::rollmean(cases, k=7, fill=NA, align="right"))%>%
  filter(srednia>0.1)

ggplot(filter(a), aes(x=data, y=cases))+
  geom_point(aes(size="dzienne nowe zakażenia"), color="blue", alpha=0.4) +
  #scale_color_manual(values = c("nowe zakażenia"="blue4", "średnia"="red4" ))+
  geom_smooth(aes(color="średnia"), size=1.5, se=T, span=0.4, level=0.95)+
  coord_cartesian(xlim = c(ymd("2020-04-01"), ymd("2020-05-07")))+
  geom_path(aes(x=data, y=srednia.nowe), color="orange", size=2)+
  #facet_wrap(~Państwo, ncol=7, scales="free_y")+
  #geom_hline(aes(yintercept = linia1, linetype=""), color="red4")+
  #geom_vline(aes(xintercept = linia2, linetype=" "),color= "red4", show.legend = F)+
  labs(x="ilość dni od przekroczenia 0,1 zakażenia na 100 tys. mieszkańców", 
       y="dzienna ilość nowych przypadków",
       color="",
       size= "",
       title = "Liczba dziennych zakażeń na 100 tys. mieszkańców w państwach europejskich",
       subtitle =  paste("Stan na", format(max(a$data), "%d/%m/%Y"), ". Oś y indywidualna dla każdego państwa."),
       caption = "Źródło: European Centre for Disease Prevention and Control")+
  #scale_linetype_manual(name = c("", " "), values = c("solid", "longdash"), labels = c(paste("poziom przyrostu zakażeń w Polsce\nstan na ",format(data.pl,"%d %B %Y") ), 
                                                                                       #"ilość dni od przekroczenia poziomu 0,1 zakażenia \nna 100 tys. mieszkancow w Polsce"))+
  theme_bw()+
  theme(legend.position = "top", plot.caption = element_text( size = 8), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(colour = "grey", size = 0.5))
