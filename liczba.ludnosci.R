library(tidyr)
library(dplyr)
library(lubridate)
library(directlabels)
library(ggplot2)
library(ggthemes)
library(scales)
library(rnaturalearth)

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(9, 10, 35) %>%
  rename(ISO3=adm0_a3)
world$geometry <- NULL

population <- left_join(covid, world, by="ISO3")

chiny.hubei <- covid %>%
  filter(Country.Region=="China.no.hubei"|Country.Region=="Hubei") %>%
  mutate(admin="NA")%>%
  mutate(pop_est = if_else(Country.Region=="China.no.hubei", paste(1281502970), paste(57110000)))%>%
  mutate(pop_est = as.numeric(pop_est))

# chiny 1338612970
# Chiny bez hubei 1281502970
# hubei 57110000

#na tym etapie można olać, ale warto sprawdzać, czy nie ma gdzieś wzrostu
# ale to wynika, że w danych world nie ma tych państw. Może potem dodać ręcznie.
a<- filter(population, is.na(pop_est))

population <- bind_rows(population, chiny.hubei)

population <- population %>%
  filter(liczba.zachorowan>500) %>%
  group_by(Country.Region) %>%
  mutate(id=row_number()-1, proc.chorych=liczba.zachorowan/pop_est, proc.zgonow=liczba.ofiar/pop_est)

population2 <- population %>%
  group_by(Country.Region) %>%
  mutate(id=row_number()-1, proc.chorych=liczba.zachorowan/pop_est, proc.zgonow=liczba.ofiar/pop_est)%>%
  filter(proc.chorych>0.001) %>%
  filter(data==max(population$data))


test <- population%>%
  filter(data==max(population$data)) %>%
  arrange(proc.chorych)%>%
  filter(proc.chorych>0.0005)

test.no.hubei <- population%>%
  filter(data==max(population$data)) %>%
  filter(Country.Region!="China.no.hubei") %>%
  filter(Country.Region!="Hubei") %>%
  arrange(proc.chorych)

test.zachorowalnosc <- population %>%
  filter(proc.chorych>0.0001) %>%
  filter(data==max(population$data))

#chorzy i ludność
png("ludnosc.png", units="in", width=9, height=5, res=300)
ggplot(data=test, aes(x=Państwo, y=proc.chorych, label=Państwo))+
  geom_point(color="blue", size=2)+
  ggrepel::geom_label_repel(hjust=-0.1, fill="grey")+
  scale_y_continuous( labels = label_percent()) +
  labs(y="odsetek przypadków COVID-19 w stosunku do liczby ludności")+
  theme_bw()+
  #ggtitle(paste0("Procent zachorowań w stosunku do liczby ludności. Stan na", gsub("2020-03-"," ", max(test.no.hubei$data))," ",months(max(test.no.hubei$data)), "."))+  
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5))
dev.off()

#chorzy i ludność bez Hubei
ggplot(data=test.no.hubei, aes(x=Państwo, y=proc.chorych, label=Państwo))+
  geom_point(color="blue", size=2)+
  ggrepel::geom_label_repel(hjust=-0.1, fill="grey")+
  scale_y_continuous(limits = c(0, 0.00077), labels = label_percent()) +
  theme_bw()+
  ggtitle(paste0("Procent zachorowań w stosunku do liczby ludności. Stan na", gsub("2020-03-"," ", max(test.no.hubei$data))," ",months(max(test.no.hubei$data)), "."))+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5))

#chorzy i ludność logarytmicznie powyżej 0.0001 procenta zachorowania
ggplot(data=test.zachorowalnosc, aes(x=Państwo, y=proc.chorych, label=Państwo))+
  geom_point(color="blue", size=2)+
  ggrepel::geom_label_repel(hjust=-0.1, fill="grey", label=paste(test.zachorowalnosc$Państwo, as.character(round(test.zachorowalnosc$proc.chorych*100, 4)), "%"))+
  scale_y_continuous(trans='log10',labels = label_percent()) +
  theme_bw()+
  ggtitle(paste0("Procent zachorowań w stosunku do liczby ludności. Stan na", gsub("2020-03-"," ", max(test.no.hubei$data))," ",months(max(test.no.hubei$data)), "."))+  
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5))

#chorzy i ludność  powyżej 0.0001 procenta zachorowania i powyżej 200 chorych
ggplot(data=test.zachorowalnosc, aes(x=Państwo, y=proc.chorych, label=Państwo))+
  geom_point(color="blue", size=2)+
  ggrepel::geom_label_repel(hjust=-0.1, fill="grey", label=paste(test.zachorowalnosc$Państwo, as.character(round(test.zachorowalnosc$proc.chorych*100, 3)), "%"))+
  scale_y_continuous(labels = label_percent()) +
  theme_bw()+
  ggtitle(paste0("Procent zachorowań w stosunku do liczby ludności. \nPaństwa, w których zachorowało powyżej 0,01% ludności. Stan na", gsub("2020-03-"," ", max(test.no.hubei$data))," ",months(max(test.no.hubei$data)), "."))+  
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5))

#chorzy i ludność  powyżej 0.0001 procenta zachorowania
ggplot(data=population2, aes(x=Państwo, y=proc.chorych, label=Państwo))+
  geom_point(color="blue", size=2)+
  ggrepel::geom_label_repel(hjust=-0.1, fill="grey", label=paste(population2$Państwo, as.character(round(population2$proc.chorych*100, 3)), "%"))+
  scale_y_continuous(labels = label_percent()) +
  theme_bw()+
  ggtitle(paste0("Procent zachorowań w stosunku do liczby ludności. Stan na", gsub("2020-03-"," ", max(test.no.hubei$data))," ",months(max(test.no.hubei$data)), "."))+  
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5))

 
  #zgony i ludność 
ggplot(data=test, aes(x=Państwo, y=proc.zgonow, label=Państwo))+
  geom_point(color="blue", size=2)+
  ggrepel::geom_label_repel(hjust=-0.1, fill="grey")+
  scale_y_continuous(labels = label_percent()) +
  theme_bw()+
  ggtitle("Procent zgonów w stosunku do liczby ludności")+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5))

#zgony logarytmicznie
ggplot(data=test, aes(x=Państwo, y=proc.zgonow, label=Państwo))+
  geom_point(color="blue", size=2)+
  ggrepel::geom_label_repel(hjust=-0.1, fill="grey")+
  scale_y_continuous(trans='log10', labels = label_percent()) +
  theme_bw()+
  ggtitle("Procent zgonów w stosunku do liczby ludności (skala logarytmiczna)")+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5))
