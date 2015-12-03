# library(maptools)
library(dplyr)
library(stringdist)
library(rgdal)
library(ggplot2)



# Deaths ------------------------------------------------------------------


decessi <- read.csv(file = "Dati/dataset/Morti_quartiere_2003_2014.csv", 
                    header = TRUE, 
                    sep = ";", 
                    fileEncoding = "latin1")

decessi$Numerosità <- as.numeric(decessi$Numerosità)
levels(decessi$Quartiere) <- c(levels(decessi$Quartiere), "Parco Sempione")
decessi$Quartiere[decessi$Quartiere == ""] <- "Parco Sempione"

# Colonna con i nomi dei quartieri presenti in tabella
milan_qt@data$DB_ID_NIL <- sapply(as.character(zone_sp@data$NIL),
                                  # Custom function
                                  str_match,
                                  words = as.character(unique(decessi$Quartiere)))


milan_qt@data <- filter(decessi, Luogo_decesso == "Milano")  %>%
  select(Anno_evento, Quartiere, Numerosità) %>%
  group_by(Quartiere) %>%
  summarize( decessi = sum(Numerosità)) %>%
  inner_join(milan_qt@data, .,  by = c("DB_ID_NIL" = "Quartiere"))



# Births ------------------------------------------------------------------


nascite <- read.csv(file = "Dati/dataset/Nati_quartiere_2003_2014.csv", 
                    header = TRUE, 
                    sep = ";", 
                    fileEncoding = "latin1")

# Converting to attribute to same type
nascite$Numerosità <- as.numeric(nascite$Numerosità)

levels(nascite$Quartiere) <- c(levels(nascite$Quartiere), "Parco Sempione")
nascite$Quartiere[nascite$Quartiere == ""] <- "Parco Sempione"

milan_qt@data <- nascite %>%
  filter(Luogo_nascita == "Milano")  %>%
  select(Anno_evento, Quartiere, Numerosità) %>%
  group_by(Quartiere) %>%
  summarize(nascite = sum(Numerosità)) %>%
  inner_join(milan_qt@data, .,  by = c("DB_ID_NIL" = "Quartiere"))


# Suddivisione per sesso ---------------------------------------------------------------




# Rapporto tra nascite e decessi
milan_qt@data <- mutate(milan_qt@data, nascite_x_decesso = nascite/decessi)

