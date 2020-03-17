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

chiny.hubei <- covid.chiny %>%
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
  filter(liczba.zachorowan>100) %>%
  group_by(Country.Region) %>%
  mutate(id=row_number()-1, proc.chorych=liczba.zachorowan/pop_est, proc.zgonow=liczba.ofiar/pop_est)

test <- population%>%
  filter(data==max(population$data)) %>%
  arrange(proc.chorych)

test.no.hubei <- population%>%
  filter(data==max(population$data)) %>%
  filter(Country.Region!="China.no.hubei") %>%
  filter(Country.Region!="Hubei") %>%
  arrange(proc.chorych)

#chorzy i ludność
ggplot(data=filter(test, liczba.zachorowan>300), aes(x=Państwo, y=proc.chorych, label=Państwo))+
  geom_point(color="blue", size=2)+
  ggrepel::geom_label_repel(hjust=-0.1, fill="grey")+
  scale_y_continuous(limits = c(0, 0.0012), labels = label_percent()) +
  theme_bw()+
  ggtitle("Procent chorych w stosunku do liczby ludności")+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5))

#chorzy i ludność bez Hubei
ggplot(data=test.no.hubei, aes(x=Państwo, y=proc.chorych, label=Państwo))+
  geom_point(color="blue", size=2)+
  ggrepel::geom_label_repel(hjust=-0.1, fill="grey")+
  scale_y_continuous(limits = c(0, 0.0006), labels = label_percent()) +
  theme_bw()+
  ggtitle("Procent chorych w stosunku do liczby ludności")+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5))

#zgony i ludność 
ggplot(data=test, aes(x=Państwo, y=proc.zgonow, label=Państwo))+
  geom_point(color="blue", size=2)+
  ggrepel::geom_label_repel(hjust=-0.1, fill="grey")+
  scale_y_continuous(limits = c(0, 0.00004), labels = label_percent()) +
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
  ggtitle("Procent zgonów w stosunku do liczby ludności")+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5))
