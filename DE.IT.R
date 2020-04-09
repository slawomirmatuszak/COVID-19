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
  xlim(0,30)+
  ylim(0,6000)+
  labs(y = "liczba zgonów", x="liczba dni od pierwszego zgonu", color=NULL)+
  theme_bw()

# śmiertelność we Włoszech i Niemczech
ggplot()+
  geom_path(data=de.it, aes(x=id, y=smiertelnosc, color=Państwo),size=2, alpha=0.8)+
  geom_point(data=de.it, aes(x=id, y=smiertelnosc, color=Państwo),size=3, alpha=0.8)+
  xlim(0,30)+
  scale_y_continuous(labels = label_percent()) +
  #ylim(0,60)+
  labs(y = "śmiertelność", x="liczba dni od pierwszego zgonu", color=NULL)+
  theme_bw()

ggplot()+
  geom_point(data=de.it, aes(x=id, y=nowe.zgony, color=Państwo),size=2, alpha=0.8)

duze.UE <- covid %>%
  filter(liczba.ofiar>50) 

duze.UE <- duze.UE %>%
  filter(Country.Region=="Italy"|Country.Region=="Germany"|Country.Region=="Poland"|Country.Region=="France"|Country.Region=="Spain") %>%
  group_by(Country.Region) %>%
  mutate(id=row_number()-1)

# liczba zachorowań
ggplot()+
  geom_path(data=duze.UE, aes(x=id, y=liczba.zachorowan, color=Państwo),size=2, alpha=0.8)+
  coord_cartesian(xlim=c(0,20))+
  labs(y = "liczba zachorowań", x="liczba dni od 50-tego zgonu", color=NULL)+
  theme_bw()+
  theme(legend.position = "top")->p0

# liczba zgonóW 
ggplot()+
  geom_path(data=duze.UE, aes(x=id, y=liczba.ofiar, color=Państwo),size=2, alpha=0.8)+
  coord_cartesian(xlim=c(0,20), ylim=c(0,8000))+
  labs(y = "liczba zgonów", x="liczba dni od 50-tego zgonu", color=NULL)+
  theme_bw()+
  theme(legend.position = "top")->p1

# śmiertelność 
ggplot()+
  geom_path(data=duze.UE, aes(x=id, y=smiertelnosc, color=Państwo),size=2, alpha=0.8)+
  geom_point(data=duze.UE, aes(x=id, y=smiertelnosc, color=Państwo),size=3, alpha=0.8)+
  coord_cartesian(xlim=c(0,20))+
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(y = "śmiertelność", x="liczba dni od 50-tego zgonu", color=NULL)+
  theme_bw()+
  theme(legend.position = "top") ->p2

library(gridExtra)
png("duze.ue.png", units="in", width=9, height=6, res=600)
grid.arrange(p0, p1,p2, ncol=3)
dev.off()

library(ggpubr)
png("duze.ue.png", units="in", width=9, height=6, res=600)
ggarrange(p0, p1, p2, ncol=3, nrow=1, common.legend = TRUE, legend="top")
dev.off()