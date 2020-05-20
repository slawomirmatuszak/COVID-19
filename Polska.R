library(tidyverse)

load(file = "E:/R/COVID-19/covid.ECDC.Rda")



a <- covid.ECDC%>%
  filter(Kontynenty=="Europa")%>%
  arrange(data, Państwo)%>%
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
  mutate(srednia= zoo::rollmean(zach.100, k=10, fill=NA, align="right"))%>%
  filter(srednia>0.1, population>1e5)%>%
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

png("Europa10 maja.png", units="in", width=14, height=8, res=600)
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
dev.off()

## to samo z regresją
png("Europa.regresja.8maja.png", units="in", width=14, height=8, res=600)
ggplot(filter(a), aes(x=id, y=zach.100))+
  geom_point(aes(size="dzienne nowe zakażenia"), color="blue", alpha=0.4) +
  scale_color_manual(values = c("nowe zakażenia"="blue4", "średnia"="red4" ))+
  geom_smooth(aes(color="średnia"), size=1.5, se=F, span=0.4)+
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



# skumulowany odsetek zakażonych ------------------------------------------
library(ggrepel)
library(scales)

a <- covid.ECDC %>%
  filter(Kontynenty=="Europa")%>%
  filter(data==max(data))%>%
  #filter(suma.chorych>1000)%>%
  mutate(zach.100=suma.chorych*1e5/population,
         smiertelnosc=suma.zgonow/suma.chorych,
         zgony.100=suma.zgonow*1e5/population)%>%
  arrange(desc(zgony.100))%>%
  ungroup()%>%
  select(countries, Państwo, proc.chorych, zach.100, smiertelnosc, zgony.100)

# odsetek zarażonej populacji
ggplot(a, aes(x=Państwo, y=proc.chorych))+
  geom_point()+
  geom_label_repel(aes(label=Państwo), hjust=-0.1)+
  scale_y_continuous(labels = percent)+
  theme_bw()+
  theme(legend.position = "none", axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.caption = element_text( size = 8), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(colour = "grey", size = 0.5))

#barplot
a %>%filter(!is.na(proc.chorych))%>%
ggplot(aes(x=reorder(countries, proc.chorych), y=proc.chorych))+
  geom_col()+
  scale_y_continuous(labels = percent)+
  coord_flip()+
  theme_bw()

#zgony na 100 tys.
a%>%head(20)%>%
  ggplot(aes(x=reorder(countries, zgony.100), y=zgony.100))+
  geom_col()+
  #scale_y_continuous(labels = percent)+
  coord_flip()+
  theme_bw()

#smiertelnosc vs zachorowania
a %>% filter(countries!="Diamond Princess")%>%
ggplot(aes(x=zach.100, y=smiertelnosc))+
  geom_point(color="blue", alpha=0.7)+
  scale_y_continuous(labels = percent)+
  facet_wrap(~Państwo, scales="free")

# zachorownia na 100 tys.

ggplot(a, aes(x=Państwo, y=zach.100))+
  geom_point()+
  geom_label_repel(aes(label=Państwo), hjust=-0.1)+
  scale_y_continuous()+
  theme_bw()+
  theme(legend.position = "none", axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.caption = element_text( size = 8), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(colour = "grey", size = 0.5))

ggplot(a, aes(x=reorder(Państwo, zach.100), y=zach.100))+
  geom_col()+
  #geom_label_repel(aes(label=Państwo), hjust=-0.1)+
  scale_y_continuous()+
  coord_flip()+
  theme_bw()

# ilość nowych przypadków i lockdown --------------------------------------

a <- covid.ECDC%>%
  filter(Państwo=="Polska")%>%
  mutate(zach.100 = cases*100000/population)%>%
  mutate(srednia= zoo::rollmean(zach.100, k=7, fill=NA, align="right"))%>%
  mutate(srednia.nowe.przypadki= zoo::rollmean(cases, k=7, fill=NA, align="right"))

png("./wykresy/polska.ograniczenia.zachorowania.19.maja.png", units="in", width=9, height=6, res=300)
ggplot(a, aes(x=data, y=srednia.nowe.przypadki))+
  geom_path(color="blue", size=2)+
  geom_vline(xintercept=ymd("2020-03-24"), colour="red") +
  annotate("text", x=ymd("2020-03-24"),y=0, angle=90, label="zakaz przemieszczania się bez przyczyny", color="red", hjust = 0, vjust=1.2)+
  geom_vline(xintercept=ymd("2020-04-01"), colour="red") +
  annotate("text", x=ymd("2020-04-01"),y=0, angle=90, label="zamknięcie parków i lasów", color="red", hjust = 0, vjust=1.2)+
  geom_vline(xintercept=ymd("2020-04-16"), colour="red") +
  annotate("text", x=ymd("2020-04-16"),y=0, angle=90, label="wprowadzenie obowiązkowych maseczek", color="red", hjust = 0, vjust=1.1)+
  geom_vline(xintercept=ymd("2020-04-20"), colour="darkgreen") +
  annotate("text", x=ymd("2020-04-20"),y=0, angle=90, label="otwarcie parków i lasów", color="darkgreen", hjust = 0, vjust=-0.5)+
  annotate("text", x=ymd("2020-04-20"),y=0, angle=90, label="zniesienie zakazu przemieszczania się", color="darkgreen", hjust = 0, vjust=1.2)+
  geom_vline(xintercept=ymd("2020-05-04"), colour="darkgreen") +
  annotate("text", x=ymd("2020-05-04"),y=0, angle=90, label="otwarcie galerii handlowych", color="darkgreen", hjust = 0, vjust=1.2)+
  geom_vline(xintercept=ymd("2020-05-18"), colour="darkgreen") +
  annotate("text", x=ymd("2020-05-18"),y=0, angle=90, label="otwarcie restauracji", color="darkgreen", hjust = 0, vjust=1.2)+
  labs(y="nowe dzienne zachorowania*", x="",
       caption = "*średnia ruchoma z 7 dni")+
  theme_bw()
dev.off()

# wariant z regresją
png("./wykresy/polska.ograniczenia.zachorowania.regresja.20.maja.png", units="in", width=9, height=6, res=300)
ggplot(a, aes(x=data, y=cases))+
  geom_point(color="blue", size=2, alpha=0.5)+
  geom_smooth(color="orange", size=1.5, span=0.4, se=F)+
  geom_vline(xintercept=ymd("2020-03-24"), colour="red") +
  annotate("text", x=ymd("2020-03-24"),y=0, angle=90, label="zakaz przemieszczania się bez przyczyny", color="red", hjust = 0, vjust=1.2)+
  #geom_text(aes(x=ymd("2020-03-24"), y=110),  label="zakaz przemieszczania się bez przyczyny", colour="red", angle=90, vjust = 1.2)+
  geom_vline(xintercept=ymd("2020-04-01"), colour="red") +
  annotate("text", x=ymd("2020-04-01"),y=0, angle=90, label="zamknięcie parków i lasów", color="red", hjust = 0, vjust=1.2)+
  #geom_text(aes(x=ymd("2020-04-01"), y=100),  label="zamknięcie parków i lasów", colour="red", angle=90, vjust = 1.2)+
  geom_vline(xintercept=ymd("2020-04-20"), colour="darkgreen") +
  geom_vline(xintercept=ymd("2020-04-16"), colour="red") +
  annotate("text", x=ymd("2020-04-16"),y=0, angle=90, label="wprowadzenie obowiązkowych maseczek", color="red", hjust = 0, vjust=1.2)+
  annotate("text", x=ymd("2020-04-20"),y=0, angle=90, label="otwarcie parków i lasów", color="darkgreen", hjust = 0, vjust=-0.5)+
  annotate("text", x=ymd("2020-04-20"),y=0, angle=90, label="zniesienie zakazu przemieszczania się", color="darkgreen", hjust = 0, vjust=1.2)+
  #geom_text(aes(x=ymd("2020-04-20"), y=100),  label="zniesienie zakazu przemieszczania się", colour="darkgreen", angle=90, vjust = 1.2)+
  geom_vline(xintercept=ymd("2020-05-04"), colour="darkgreen") +
  annotate("text", x=ymd("2020-05-04"),y=0, angle=90, label="otwarcie galerii handlowych", color="darkgreen", hjust = 0, vjust=1.2)+
  #geom_text(aes(x=ymd("2020-05-04"), y=100),  label="otwarcie galerii handlowych", colour="darkgreen", angle=90, vjust = 1.2)+
  geom_vline(xintercept=ymd("2020-05-18"), colour="darkgreen") +
  annotate("text", x=ymd("2020-05-18"),y=0, angle=90, label="otwarcie restauracji", color="darkgreen", hjust = 0, vjust=1.2)+
  #geom_text(aes(x=ymd("2020-05-18"), y=100),  label="otwarcie restauracji", colour="darkgreen", angle=90, vjust = 1.2)+
  labs(y="nowe dzienne zachorowania", x="")+
  theme_bw()
dev.off()
