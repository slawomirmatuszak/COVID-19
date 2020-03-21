# dałem jako źródło covid.chiny, ale chyba może być po prostu covid?
de.it <- covid %>%
  filter(liczba.ofiar>0) 

de.it <- de.it %>%
  filter(Country.Region=="Italy"|Country.Region=="Germany") %>%
  group_by(Country.Region) %>%
  mutate(id=row_number()-1)

# liczba zgonóW we Włoszech i Niemczech
ggplot()+
  geom_path(data=de.it, aes(x=id, y=liczba.ofiar, color=Państwo),size=2, alpha=0.8)+
  xlim(0,12)+
  ylim(0,60)+
  labs(y = "liczba zgonów", x="liczba dni od pierwszego zgonu", color=NULL)+
  theme_bw()

# śmiertelność we Włoszech i Niemczech
ggplot()+
  geom_path(data=de.it, aes(x=id, y=smiertelnosc, color=Państwo),size=2, alpha=0.8)+
  geom_point(data=de.it, aes(x=id, y=smiertelnosc, color=Państwo),size=3, alpha=0.8)+
  xlim(0,19)+
  scale_y_continuous(labels = label_percent()) +
  #ylim(0,60)+
  labs(y = "śmiertelność", x="liczba dni od pierwszego zgonu", color=NULL)+
  theme_bw()

ggplot()+
  geom_point(data=de.it, aes(x=id, y=nowe.zgony, color=Państwo),size=2, alpha=0.8)