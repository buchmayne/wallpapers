library(tidyverse)
library(osmdata)


the_bounding_box <- getbb("Portland Oregon")

streets <- the_bounding_box %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

small_streets <- the_bounding_box %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

river <- the_bounding_box %>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

# #282828

ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "white",          
          size = .3,
          alpha = .8) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "white",          
          size = .2,
          alpha = .6) +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "white",          
          size = .2,
          alpha = .5) +
  coord_sf(xlim = c(-122.8, -122.5),
           ylim = c(45.65, 45.445),
           expand = TRUE) +
  theme_void() +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#282828"),
        panel.grid.major = element_line(colour = "transparent")
        )


# ggsave(filename = "~/Desktop/portland_map_bw.png", width = 6, height = 6)

# Manizales coords
# coord_sf(xlim = c(-75.539, -75.47),
#          ylim = c(5.03, 5.085),
#          expand = TRUE) + 