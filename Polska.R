library(tidyverse)

load(file = "E:/R/COVID-19/covid.ECDC.Rda")



a <- covid.ECDC%>%
  filter(Kontynenty=="Europa")%>%
  mutate(zach.100 = cases*100000/population)%>%
  group_by(Państwo)%>%
  mutate(srednia= zoo::rollmean(zach.100, k=7, fill=NA, align="right"))%>%
  filter(srednia>0, population>2e5)


# krzywe z datą na osi x
#png("Europa.png", units="in", width=12, height=8, res=600)
ggplot(filter(a), aes(x=data, y=srednia))+
  geom_path(color = "blue",size=1.5, alpha=0.8) +
  facet_wrap(~Państwo, ncol=6, scales="free_y")+
  #coord_cartesian(ylim=c(0,30))+
  #geom_hline(yintercept = linia1), linetype="dashed", color="chocolate3")+
  labs(x="", 
       y="dzienna ilość nowych przypadków",
       color="",
       title = "Liczba dziennych zakażeń na 100 tys. mieszkańców w państwach europejskich",
       subtitle =  paste("Średnia krocząca z 7 dni.", "Stan na", format(max(a$data), "%d/%m/%Y"), ". Oś y indywidualna dla każdego państwa."),
       caption = "Źródło: European Centre for Disease Prevention and Control")+
  theme_bw()+
  theme(legend.position = "none", plot.caption = element_text( size = 8), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(colour = "grey", size = 0.5))
#dev.off()

###############################################################################################
# krzywe z id na osi x

a <- covid.ECDC%>%
  filter(Kontynenty=="Europa")%>%
  mutate(zach.100 = cases*100000/population)%>%
  group_by(Państwo)%>%
  mutate(srednia= zoo::rollmean(zach.100, k=7, fill=NA, align="right"))%>%
  filter(srednia>0.1, population>2e5)%>%
  mutate(id=row_number())

linia1 <-filter(a, Państwo=="Polska", id==max(id))%>%
  ungroup()%>%
  select(srednia)%>%
  pull()

linia2 <- a %>%
  filter(Państwo=="Polska", id==max(id))%>%
  ungroup()%>%
  select(id)%>%
  pull()

data.pl <- a %>%
  filter(Państwo=="Polska", id==max(id))%>%
  select(data)%>%
  pull()

#png("Europa2.png", units="in", width=14, height=8, res=600)
ggplot(filter(a), aes(x=id, y=srednia))+
  geom_path(color = "blue",size=1.5, alpha=0.8) +
  facet_wrap(~Państwo, ncol=7, scales="free_y")+
  geom_hline(aes(yintercept = linia1, linetype=""), color="red4")+
  geom_vline(aes(xintercept = linia2, linetype=" "),color= "red4", show.legend = F)+
  labs(x="ilość dni od przekroczenia 0,1 zakażenia na 100 tys. mieszkańców", 
       y="dzienna ilość nowych przypadków",
       color="",
       title = "Liczba dziennych zakażeń na 100 tys. mieszkańców w państwach europejskich",
       subtitle =  paste("Średnia krocząca z 7 dni.", "Stan na", format(max(a$data), "%d/%m/%Y"), ". Oś y indywidualna dla każdego państwa."),
       caption = "Źródło: European Centre for Disease Prevention and Control")+
  scale_linetype_manual(name = c("", " "), values = c("solid", "longdash"), labels = c(paste("poziom przyrostu zakażeń w Polsce\nstan na ",format(data.pl,"%d %B %Y") ), 
                                                                                     "ilość dni od przekroczenia poziomu 0,1 zakażenia \nna 100 tys. mieszkancow w Polsce"))+
  theme_bw()+
  theme(legend.position = "top", plot.caption = element_text( size = 8), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(colour = "grey", size = 0.5))
#dev.off()

## to samo z regresją
png("Europa.regresja.7maja.png", units="in", width=14, height=8, res=600)
ggplot(filter(a), aes(x=id, y=zach.100))+
  geom_point(aes(size="dzienne nowe zakażenia"), color="blue", alpha=0.4) +
  scale_color_manual(values = c("nowe zakażenia"="blue4", "średnia"="red4" ))+
  geom_smooth(aes(color="średnia"), size=1.5, se=F)+
  facet_wrap(~Państwo, ncol=7, scales="free_y")+
  geom_hline(aes(yintercept = linia1, linetype=""), color="red4")+
  geom_vline(aes(xintercept = linia2, linetype=" "),color= "red4", show.legend = F)+
  labs(x="ilość dni od przekroczenia 0,1 zakażenia na 100 tys. mieszkańców", 
       y="dzienna ilość nowych przypadków",
       color="",
       size= "",
       title = "Liczba dziennych zakażeń na 100 tys. mieszkańców w państwach europejskich",
       subtitle =  paste("Stan na", format(max(a$data), "%d/%m/%Y"), ". Oś y indywidualna dla każdego państwa."),
       caption = "Źródło: European Centre for Disease Prevention and Control")+
  scale_linetype_manual(name = c("", " "), values = c("solid", "longdash"), labels = c(paste("poziom przyrostu zakażeń w Polsce\nstan na ",format(data.pl,"%d %B %Y") ), 
                                                                                       "ilość dni od przekroczenia poziomu 0,1 zakażenia \nna 100 tys. mieszkancow w Polsce"))+
  theme_bw()+
  theme(legend.position = "top", plot.caption = element_text( size = 8), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(colour = "grey", size = 0.5))
dev.off()
###############################################################################################
#kontytnenty
a <- covid.ECDC%>%
  filter(Kontynenty=="Ameryka Pn")%>%
  mutate(zach.100 = cases*100000/population)%>%
  group_by(Państwo)%>%
  mutate(srednia= zoo::rollmean(zach.100, k=7, fill=NA, align="right"))%>%
  filter(srednia>0, population>2e5)


ggplot(filter(a), aes(x=data, y=srednia))+
  geom_path(color = "blue",size=1.5, alpha=0.8) +
  facet_wrap(~Państwo, ncol=6, scales="free_y")+
  #coord_cartesian(ylim=c(0,30))+
  #geom_hline(yintercept = linia1), linetype="dashed", color="chocolate3")+
  labs(x="", 
       y="dzienny przyrost",
       color="",
       title = "Liczba dziennych zakażeń na 100 tys. mieszkańców",
       subtitle =  paste("Średnia krocząca z 7 dni.", "Stan na", format(max(a$data), "%d/%m/%Y")),
       caption = "Źródło: European Centre for Disease Prevention and Control")+
  theme_bw()+
  theme(legend.position = "none", plot.caption = element_text( size = 8), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(colour = "grey", size = 0.5))

####################
# dni
a <- covid.ECDC%>%
  filter(Kontynenty=="Europa")%>%
  mutate(dzien=lubridate::wday(data, label=TRUE, week_start=1, locale=Sys.setlocale("LC_TIME", "English")))%>%
  filter(Kontynenty=="Europa")%>%
  group_by(dzien, countries)%>%
  summarise(
    cases=sum(cases),
    zgony=sum(deaths)
  )

ggplot(a)+
  geom_col(aes(x=dzien, y=cases))+
  theme_bw()

ggplot(a)+
  geom_col(aes(x=dzień, y=deaths))+
  facet_wrap(~countries, scales="free_y")+
  theme_bw()

# daty 

filtr <- c("Netherlands", "Germany", "Poland", "Switzerland", "Denmark", "Sweden")

a <- covid.ECDC%>%
  filter(Kontynenty=="Europa")%>%
  mutate(dzien=lubridate::wday(data, label=TRUE, week_start=1, locale=Sys.setlocale("LC_TIME", "English")))%>%
  filter(Kontynenty=="Europa")%>%
  filter(population>5e6)%>%
  filter(deaths>0)%>%
  filter(countries %in% filtr)

#png("panstwa zgony.png", units="in", width=9, height=6, res=300)
ggplot(a)+
  geom_path(aes(x=data, y=deaths),  color="blue", size=2)+
  facet_wrap(~countries, scales="free", ncol=3)+
  theme_bw()
#dev.off()

# Ukraina
a <- covid.ECDC%>%
  filter(Kontynenty=="Europa")%>%
  mutate(dzien=lubridate::wday(data, label=TRUE, week_start=1, locale=Sys.setlocale("LC_TIME", "English")))%>%
  filter(Kontynenty=="Europa")%>%
  filter(population>5e6)%>%
  filter(deaths>0)%>%
  filter(countries == "Ukraine")

ggplot(a)+
  geom_path(aes(x=data, y=deaths),  color="blue", size=2)+
  #facet_wrap(~countries, scales="free", ncol=3)+
  theme_bw()
