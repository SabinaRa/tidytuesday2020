# TidyTuesday Chellenge Astronauts Dataset 2020-07-14
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-07-14/readme.md
# Contributor: Sabina Rako, Twitter: @ra_sabina

library(tidytuesdayR)
library(tidyverse)
library(skimr)
library(ggtext)
library(mdthemes)
library(DT)


# Importing data
tuesdata <- tidytuesdayR::tt_load('2020-07-14')
astronauts <- tuesdata$astronauts

#Exploration
skim(astronauts)
glimpse(astronauts)

#Q: How many astronauts did a mission from 1961 till 2019 (by year)?
astronauts_by_year <- astronauts %>% group_by(year_of_mission) %>% summarise(n = n())
astronauts_by_year

#Q: What was their gender and status (military or civilian)?
astronauts_g_mc <- count(astronauts, year_of_mission, sex, military_civilian) 
astronauts_g_mc

#Q: Can we visualize this?

p <- ggplot(astronauts_g_mc, aes(year_of_mission, n)) + 
  geom_bar(data=astronauts_by_year, stat="identity", fill="grey", alpha = 0.6, width = 0.7) + 
  geom_bar(stat="identity", fill = "#c70000", width = 0.7) + facet_grid( military_civilian ~ sex) + xlab("Year of mission") + 
  mdthemes::md_theme_minimal() + 
  labs(title = "Number of space travelers in missions from 1961 till 2019 by <span style=color:#c70000>**gender**</span> and <span style=color:#c70000>**status**</span><br>(military or civilian)", subtitle = "TidyTuesday 2020-07-14", caption = "Data source: Mariya Stavnichuk and Tatsuya Corlett article<br>Data preparation: Georgios Karamanis<br> Visualization: Sabina Rako") 

print(p)


#Adding annotation
anndata <- astronauts %>% select(year_of_mission, sex, name, military_civilian) %>% filter(year_of_mission == 1963) %>% filter(sex=="female") %>%  mutate(name=replace(name, name =="Tereshkova, Valentina", "ValentinaTereshkova")) 
anndata

p + geom_text(data = anndata, aes(label = name), x = 1960, y = 32, parse = TRUE, hjust = 0, size=3) +  
  geom_curve( data = anndata, x = 1965, y = 27, xend = 1963, yend = 3,  curvature = .3, arrow = arrow(length = unit(2, "mm")))

print(p)

#Saving
ggsave("TT2020-07-04_astronauts.png")


