covid.1 <- covid.chiny %>%
  filter(liczba.ofiar>0) %>%
  group_by(Country.Region) %>%
  mutate(id=row_number()-1)

kraje <- unique(covid.1000$Państwo)
kraje.test <- filter(covid.1000, id.test==TRUE)
kraje.test <-unique(kraje.test$Państwo)

covid.1 <- filter(covid.1,Państwo %in% kraje.test)
rm(kraje, kraje.test)

# liczba zgonów
ggplot()+
  geom_path(data=covid.1, aes(x=id, y=liczba.ofiar, color=Państwo),size=2, alpha=0.8)+
  scale_y_continuous(trans='log10',labels = label_number()) + 
  xlim(0, 30) +
  labs(y = "liczba zgonów", x="liczba dni od pierwszego zgonu", color=NULL) +
  theme_bw()+
  theme(legend.position=c(0.1,0.8))