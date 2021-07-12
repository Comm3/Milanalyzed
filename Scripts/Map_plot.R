
library(ggmap)

# Setting the map

# Carico le figure dei quartieri di Milano
milan_qt.wgs84 <- spTransform(milan_qt, CRS("+init=epsg:4326"))
milan_qt.wgs84.f <- fortify(milan_qt.wgs84, region = "ID_NIL") %>%
  merge(milan_qt.wgs84@data, by.x = "id", by.y = "ID_NIL")

# Carico le figure delle zone di Milano
milan_zone <- readOGR(dsn = "Dati/zone_dec", layer = "ZoneDecentramento")
milan_zone.wgs84 <- spTransform(milan_zone, CRS("+init=epsg:4326"))
milan_zone.wgs84.f <- fortify(milan_zone.wgs84, region = "ZONADEC") %>%
  merge(milan_zone@data, by.x = "id", by.y = "ZONADEC")




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

# Births and deaths

lnd.osm + 
  geom_polygon(data = milan_qt.wgs84.f, 
               aes(x = long, y = lat, group = group, 
                   fill = 
                     cut(nascite/decessi,
                              breaks = c(0,1,2,Inf),
                              include.lowest = TRUE,
                              right = FALSE,
                              labels = c("Meno di 1", "Tra 1 e 2", "Oltre 2")),
                   alpha = 
                     cut((cittadini / (AreaMQ / 100)),
                         breaks = c(0,5,10,20,Inf),
                         include.lowest = TRUE,
                         right = FALSE,
                         labels = c("Meno di 5", "Tra 5 e 10", "Tra 10 e 20", "Oltre 20"))
               ), 
               # alpha = 0.5, 
               colour = "black", size = 0.25) + 
  geom_polygon(data = milan_zone.wgs84.f,
               aes(long, lat, group = group),
               fill = NA, color = "black", size = 1
  ) +
  labs(x = "Longitudine", y = "Latitudine", fill = "Nascite per decesso",
       alpha = "Densità citt./100mq 2014") + 
  ggtitle("Tasso di natalità 2003/2014") +
  # scale_fill_distiller(palette = "Greens", breaks = pretty_breaks(n = 5)) +
  guides(fill = guide_legend(reverse = TRUE))


# Nationality


lnd.osm + 
  geom_polygon(data = milan_qt.wgs84.f, 
               aes(x = long, y = lat, group = group, 
                   fill = seconda_cittadinanza,
                   alpha = (secondi_cittadini / cittadini) * 100
               ), 
               # alpha = 0.5, 
               colour = "black", size = 0.25) + 
  geom_polygon(data = milan_zone.wgs84.f,
               aes(long, lat, group = group),
               fill = NA, color = "black", size = 1
  ) +
  labs(fill = "Cittadinanza",
       alpha = "Perc. rispetto totale") + 
  ggtitle("Seconda Nazionalità del quartiere 2014") +
  # scale_fill_distiller(palette = "Greens", breaks = pretty_breaks(n = 5)) +
  guides(fill = guide_legend(reverse = TRUE))


# Giovani senza filtro su nazionalità
lnd.osm + 
  geom_polygon(data = milan_qt.wgs84.f, 
               aes(x = long, y = lat, group = group, 
                   fill = 
                     # Femmine_giovani/Uomini_giovani
                     # Femmine_giovani
                     cut(Femmine_giovani/Uomini_giovani,
                         breaks = c(0,1,2,Inf),
                         include.lowest = FALSE,
                         right = FALSE,
                         labels = c("Meno di 1", "Tra 1 e 2", "Oltre 2"))
                     ,
                   alpha = 
                     cut((Femmine_giovani / (AreaMQ / 1000)),
                         breaks = c(0,5,10,20,Inf),
                         include.lowest = TRUE,
                         right = FALSE,
                         labels = c("Meno di 5", "Tra 5 e 10", "Tra 10 e 20", "Oltre 20"))
               ), 
               # alpha = 0.5, 
               colour = "black", size = 0.25) + 
  geom_polygon(data = milan_zone.wgs84.f,
               aes(long, lat, group = group),
               fill = NA, color = "black", size = 1
  ) +
  labs(x = NULL, y = NULL, fill = "Rapporto donna/uomo",
       alpha = "Donne per KmQ") + 
  ggtitle("Donne tra i 25 e i 30 anni 2014") +
  # scale_fill_distiller(palette = "Greens", breaks = pretty_breaks(n = 5)) +
  guides(fill = guide_legend(reverse = TRUE))


# Giovani italiani

lnd.osm + 
geom_polygon(data = milan_qt.wgs84.f, 
             aes(x = long, y = lat, group = group, 
                 fill = 
                   # Femmine_giovani/Uomini_giovani
                   # Femmine_giovani
                   cut(Femmine_giovani_it/Uomini_giovani_it,
                       breaks = c(0,1,2,Inf),
                       include.lowest = FALSE,
                       right = FALSE,
                       labels = c("Meno di 1", "Tra 1 e 2", "Oltre 2"))
                 ,
                 alpha = (Femmine_giovani_it / (AreaMQ / 1000))
             ), 
             colour = "black", size = 0.25) + 
  geom_polygon(data = milan_zone.wgs84.f,
               aes(long, lat, group = group),
               fill = NA, color = "black", size = 1
  ) +
  labs(x = NULL, y = NULL, fill = "Rapporto donna/uomo",
       alpha = "Donne per KmQ") + 
  ggtitle("Donne italiane tra i 25 e i 30 anni 2014") +
  # scale_fill_distiller(palette = "Greens", breaks = pretty_breaks(n = 5)) +
  guides(fill = guide_legend(reverse = TRUE))

