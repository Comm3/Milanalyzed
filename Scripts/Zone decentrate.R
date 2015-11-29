# library(maptools)
library(dplyr)
library(stringdist)
library(rgdal)
library(ggplot2)


# Loading data from CSV
decessi <- read.csv(file = "Dati/dataset/Morti_quartiere_2003_2014.csv", 
                    header = TRUE, 
                    sep = ";", 
                    fileEncoding = "latin1")

nascite <- read.csv(file = "Dati/dataset/Nati_quartiere_2003_2014.csv", 
                    header = TRUE, 
                    sep = ";", 
                    fileEncoding = "latin1")

# Converting to attribute to same type
nascite$Numerosità <- as.numeric(nascite$Numerosità)

# Adding missing label "Parco Sempione"
levels(decessi$Quartiere) <- c(levels(decessi$Quartiere), "Parco Sempione")
decessi$Quartiere[decessi$Quartiere == ""] <- "Parco Sempione"

levels(nascite$Quartiere) <- c(levels(nascite$Quartiere), "Parco Sempione")
nascite$Quartiere[nascite$Quartiere == ""] <- "Parco Sempione"

# Syntesis of the data
decessi_synt <- decessi %>%
  arrange(Quartiere) %>%
  filter(Luogo_decesso == "Milano")  %>%
  select(Anno_evento, Quartiere, Numerosità) %>%
  group_by(Quartiere) %>%
  summarize( tot_decessi = sum(Numerosità))
  

nascite_synt <- nascite %>%
  filter(Luogo_nascita == "Milano")  %>%
  select(Anno_evento, Quartiere, Numerosità) %>%
  group_by(Quartiere) %>%
  summarize( tot_nascite = sum(Numerosità)) %>%
  arrange(Quartiere)


# Dictionary of the different "quartieri" labels
quartieri_dict <- data_frame(id = zone_sp@data$ID_NIL,
                             quartiere = zone_sp@data$NIL) %>%
  arrange(quartiere)

# Mapping different quartieri label between different databases
quartieri_map <- data_frame(q_synt = db_synt$Quartiere,
                            quartiere = factor(
                              sapply(db_synt$Quartiere,
                                     # Custom function
                                     str_match,
                                     words = levels(quartieri_dict$quartiere))
                              )
                            )

# Finalizing the quartieri dictioanry
quartieri_dict <- quartieri_dict %>%
  inner_join(quartieri_map, by = "quartiere")

# Joined syntesis
db_synt <- inner_join(decessi_synt, nascite_synt, by = c("Quartiere")) %>%
  mutate(rap_nd = tot_nascite/tot_decessi,
         tot_ricambio = tot_nascite - tot_decessi,
         rap_nd_round = round(rap_nd))

db_synt <- quartieri_dict %>%
  select(id, Quartiere = q_synt) %>%
  inner_join(db_synt, by = "Quartiere") %>%
  mutate(id = as.character(id))


# Setting the map

milan_qt <- readOGR(dsn = "Dati/quartieri", layer = "NILZone")
milan_qt.f <- milan_qt %>%
  fortify(region = "ID_NIL") %>%
  merge(milan_qt@data, by.x = "id", by.y = "ID_NIL") %>%
  left_join(db_synt, by = "id")

milan_qt.wgs84 <- spTransform(milan_qt, CRS("+init=epsg:4326"))
milan_qt.wgs84.f <- fortify(milan_qt.wgs84, region = "ID_NIL") %>%
  merge(milan_qt.wgs84@data, by.x = "id", by.y = "ID_NIL") %>%
  left_join(db_synt,
            by = "id")

milan_zone <- readOGR(dsn = "Dati/zone_dec", layer = "ZoneDecentramento")
milan_zone.wgs84 <- spTransform(milan_zone, CRS("+init=epsg:4326"))
milan_zone.wgs84.f <- fortify(milan_zone.wgs84, region = "ZONADEC") %>%
  merge(milan_zone@data, by.x = "id", by.y = "ZONADEC")


# Simple map
# Map <- ggplot(milan_qt.f, aes(long, lat, group = group, fill = as.factor(rap_nd_round))) +
#   geom_polygon() +
#   coord_equal() +
#   labs(x = "Dist. Latitudine (m)", y = "Dist. Longitudine (m)", fill = "Nascite per decessi") + 
#   ggtitle("Tasso di natalità")
# 
# Map + scale_fill_discrete()

# First approach

library(ggmap)

# b <- bbox(milan_qt.wgs84)
b <- bbox(milan_zone.wgs84)
b[1, ] <- (b[1, ] - mean(b[1, ])) * 1.2 + mean(b[1, ])
b[2, ] <- (b[2, ] - mean(b[2, ])) * 1.2 + mean(b[2, ])
# lnd.b1 <- ggmap(get_map(location = b))



# Second approach
# lnd.stamen <- ggmap(get_map(location = b, source = "stamen", maptype = "toner", 
# crop = T))
lnd.gmaps <- ggmap(get_map(location = b, source = "google", maptype = "roadmap", 
    crop = T))
lnd.osm <- ggmap(get_map(location = b, source = "osm", maptype = "roadmap", 
                           crop = T))

# library(scales)

lnd.osm + 
  geom_polygon(data = milan_qt.wgs84.f, 
              aes(x = long, y = lat, group = group, 
                  fill = cut(rap_nd, breaks = c(0,1,2,Inf), include.lowest = TRUE, right = FALSE, labels = c("Meno di 1", "Tra 1 e 2", "Oltre 2")),
                  alpha = tot_nascite
                  ), 
              # alpha = 0.5, 
              colour = "black", size = 0.25) + 
  geom_polygon(data = milan_zone.wgs84.f,
               aes(long, lat, group = group),
               fill = NA, color = "black", size = 1
  ) +
  labs(x = "Longitudine", y = "Latitudine", fill = "Nascite per decesso",
       alpha = "Nascite 2014") + 
  ggtitle("Tasso di natalità 2003/2014") +
  # scale_fill_distiller(palette = "Greens", breaks = pretty_breaks(n = 5)) +
  guides(fill = guide_legend(reverse = TRUE))
