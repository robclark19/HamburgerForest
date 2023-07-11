library(tidyverse)
library(janitor)
library(readxl)
library(sf)
library(raster)
library(stars)
library(tigris)
library(basemapR)
library(cowplot)

dat = read_excel("./Data/Originals/full branch data 2021.xlsx",
                 sheet = "branch basic data") %>%
  filter(action=='setup') %>%
  dplyr::select(host_plant,bird_treatment,branch_code,lat,long) %>%
  distinct() %>%
  st_as_sf(coords = c("long","lat"), crs=4326)

ct = tigris::states(cb=T,resolution="20m") %>%
  filter(STUSPS=="CT")

# Red = B, Gold = C
ggplot() +
  base_map(st_bbox(dat) %>% expand_bbox(X=100,Y=0),basemap='mapnik',increase_zoom=2,nolabels=T) +
  geom_sf(data=dat,aes(colour=bird_treatment),alpha=.8) +
  scale_colour_manual(values=c('red','gold')) +
  scale_x_continuous(limits=c(-73.542,-73.522)) + scale_y_continuous() +
  labs(caption = "Map © 2023 OpenStreetMap") +
  coord_sf() +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = 'none'
  )
ggsave(filename="map.png",path="./Figures",width=6,height=6,units='in',dpi=300)
