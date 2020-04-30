library(tidyverse
        )
load(file = "E:/R/COVID-19/covid.ECDC.Rda")



a <- covid.ECDC%>%
  filter(Kontynenty=="Europa")%>%
  mutate(zach.100 = cases*100000/population)%>%
  group_by(Państwo)%>%
  mutate(srednia= zoo::rollmean(zach.100, k=7, fill=NA, align="right"))%>%
  filter(srednia>0, population>2e5)
  
png("Europa.png", units="in", width=12, height=8, res=600)
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
dev.off()