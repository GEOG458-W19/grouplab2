library(maps)
library(geosphere)
library(rworldmap)
source("key.R")

world_map <- getMap()
mapworld_df <- fortify(world_map)

us_df <- mapworld_df %>% filter(id == "United States of America")

ggplot() +
  geom_polygon(data = us_df, aes(long, lat, group = group)) +
  coord_equal()

register_google(key = google_key)
seattle_map <- qmap(location = "baylor university")
