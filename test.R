rac <- read.csv("wa_rac_S000_JT00_2015_tract.csv", stringsAsFactors = FALSE) %>% select(-X)
# wac <- read.csv("wa_wac_S000_JT00_2015_tract.csv", stringsAsFactors = FALSE) %>% select(-X)

# rac_wac <- left_join(rac, wac, by = c("rac.h_geocode" = "wac.w_geocode"), suffix = c(".rac", ".wac"))

shape_data <- st_read(dsn = "data/tl_2015_53_tract/tl_2015_53_tract.shp", stringsAsFactors = FALSE)
shape_data$geoid_new <- as.numeric(shape_data$GEOID)
rac_shapedata <- left_join(shape_data, rac, by = c("geoid_new" = "rac.h_geocode"))

tm_shape(shape_data) +
 tm_fill(col="NAME") +
 tm_borders()
