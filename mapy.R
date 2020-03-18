library(ggplot2)
library(ggthemes)
library("sf")
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
theme_set(theme_bw())

pribaltyka <- covid %>%
  filter(Państwo=="Litwa"|Państwo=="Łotwa"| Państwo=="Estonia")