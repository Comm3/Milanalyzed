# library(maptools)
library(dplyr)
library(stringdist)
library(rgdal)
library(ggplot2)



# Load map ----------------------------------------------------------------

milan_qt <- readOGR(dsn = "Dati/quartieri", layer = "NILZone")


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


decessi <- filter(decessi, Luogo_decesso == "Milano")  %>%
  select(Anno_evento, Quartiere, Numerosità) %>%
  group_by(Quartiere) %>%
  summarize( decessi = sum(Numerosità))

milan_qt@data <- 
  left_join(milan_qt@data, decessi, by = c("DB_ID_NIL" = "Quartiere"))



# Births ------------------------------------------------------------------


nascite <- read.csv(file = "Dati/dataset/Nati_quartiere_2003_2014.csv", 
                    header = TRUE, 
                    sep = ";", 
                    fileEncoding = "latin1")

# Converting to attribute to same type
nascite$Numerosità <- as.numeric(nascite$Numerosità)

levels(nascite$Quartiere) <- c(levels(nascite$Quartiere), "Parco Sempione")
nascite$Quartiere[nascite$Quartiere == ""] <- "Parco Sempione"

nascite <- nascite %>%
  filter(Luogo_nascita == "Milano")  %>%
  select(Anno_evento, Quartiere, Numerosità) %>%
  group_by(Quartiere) %>%
  summarize(nascite = sum(Numerosità))


milan_qt@data <- 
  left_join(milan_qt@data, nascite, by = c("DB_ID_NIL" = "Quartiere"))


# Suddivisione per anagrafica ----------------------------

popolazione <- read.csv(file = "Dati/dataset/pop_sto_quartiere_1999-2014.csv", 
                        header = TRUE, 
                        sep = ";", 
                        fileEncoding = "latin1") %>%
  mutate(X2014 = as.numeric(X2014),
         # Quartiere = as.character(Quartiere),
         Eta = as.integer(Eta))

# Cittadinanza =====================

milan_qt@data <- 
  left_join(
    milan_qt@data,
    select(popolazione, Quartiere, Cittadinanza, X2014) %>%
      group_by(Quartiere) %>%
      summarise(cittadini = sum(X2014)),
    by = c("DB_ID_NIL" = "Quartiere")
  )
  

milan_qt@data <- 
  left_join(
    milan_qt@data,
    filter(popolazione, Cittadinanza != "Italia") %>%
      select(Quartiere, seconda_cittadinanza = Cittadinanza, X2014) %>%
      group_by(Quartiere, seconda_cittadinanza) %>%
      summarise(secondi_cittadini = sum(X2014)) %>%
      filter(secondi_cittadini == max(secondi_cittadini)) %>%
      replace(is.na(.), 0),
    by = c("DB_ID_NIL" = "Quartiere")
  )


# Popolazione per sesso ed eta ===============

milan_qt@data <- 
  left_join(milan_qt@data,
    filter(popolazione, Eta >= 25 & Eta <= 30 ) %>%
    transmute(Quartiere,
              Cittadinanza,
              Genere = ifelse(Genere == "Femmine",
                              "Femmine_giovani",
                              "Uomini_giovani"),
              X2014) %>%
    group_by(Quartiere, Genere) %>%
    summarise(pop = sum(X2014)) %>%
    spread(Genere, pop) %>%
    replace(is.na(.), 0),
  by = c("DB_ID_NIL" = "Quartiere")
  )

milan_qt@data <- 
  left_join(milan_qt@data,
            filter(popolazione, Eta >= 25 & Eta <= 30 & Cittadinanza == "Italia") %>%
              transmute(Quartiere,
                        Cittadinanza,
                        Genere = ifelse(Genere == "Femmine",
                                        "Femmine_giovani_it",
                                        "Uomini_giovani_it"),
                        X2014) %>%
              group_by(Quartiere, Genere) %>%
              summarise(pop = sum(X2014)) %>%
              spread(Genere, pop) %>%
              replace(is.na(.), 0),
            by = c("DB_ID_NIL" = "Quartiere")
  )


