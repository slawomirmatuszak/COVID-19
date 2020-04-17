kraje <- c("Niemcy", "Włochy", "Hiszpania", "Wielka Brytania", "Holandia", "Polska", "Belgia", "Szwecja", "Szwajcaria", "Francja")

szwecja <- covid %>%
  filter(Państwo %in% kraje)%>%
  filter(liczba.zachorowan>1000)  %>%
  group_by(Państwo)%>%
  mutate(id=row_number()-1)%>%
  ungroup()

maks <- szwecja %>%
  filter(Państwo=="Szwecja")%>%
  select(id)%>%
  pull()%>%
  max()

png("szwecja.png", units="in", width=9, height=6, res=600)
ggplot(szwecja)+
  geom_path(aes(x=id, y=liczba.zachorowan, color=Państwo), size=2)+
  scale_x_continuous(limits = c(0,maks))+
  scale_y_continuous(limits = c(1000,100000))+
  labs(x="ilość dni od przekroczenia 1000 zarażeń", y="liczba potwierdzonych zarażeń")+
  theme_bw()
dev.off()

# Polska i Szwecja
kraje2 <- c("Polska", "Szwecja")

szwecja <- covid %>%
  filter(Państwo %in% kraje2)%>%
  filter(liczba.zachorowan>1000)  %>%
  group_by(Państwo)%>%
  mutate(id=row_number()-1)%>%
  ungroup()

maks <- szwecja %>%
  filter(Państwo=="Szwecja")%>%
  select(id)%>%
  pull()%>%
  max()

png("szwecja.PL.png", units="in", width=9, height=6, res=600)
kolory <- c("Polska"="red4", "Szwecja"="blue")
ggplot(szwecja)+
  geom_path(aes(x=id, y=liczba.zachorowan, color=Państwo), size=2)+
  scale_x_continuous(limits = c(0,maks))+
  scale_y_continuous(limits = c(1000,12000))+
  scale_color_manual(values = kolory)+
  labs(x="ilość dni od przekroczenia 1000 zarażeń", y="liczba potwierdzonych zarażeń")+
  theme_bw()+
  theme(legend.position = c(0.9,0.1))
dev.off()

png("szwecja.PL.zgony.png", units="in", width=9, height=6, res=600)
kolory <- c("Polska"="red4", "Szwecja"="blue")
ggplot(szwecja)+
  geom_path(aes(x=id, y=liczba.ofiar, color=Państwo), size=2)+
  scale_x_continuous(limits = c(0,maks))+
  scale_y_continuous(limits = c(0,max(szwecja$liczba.ofiar)))+
  scale_color_manual(values = kolory)+
  labs(x="ilość dni od przekroczenia 1000 zarażeń", y="liczba zgonów")+
  theme_bw()+
  theme(legend.position = c(0.9,0.1))
dev.off()