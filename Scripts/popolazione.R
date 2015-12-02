# library(maptools)
library(dplyr)
library(tidyr)
library(stringdist)
library(rgdal)
library(ggplot2)

popolazione <- read.csv(file = "Dati/dataset/pop_sto_quartiere_1999-2014.csv", 
                    header = TRUE, 
                    sep = ";", 
                    fileEncoding = "latin1") %>%
  mutate(X2014 = as.numeric(X2014), Quartiere = as.character(Quartiere))

popolazione_qt <- select(popolazione, Quartiere, Cittadinanza, X2014) %>%
  group_by(Quartiere, Cittadinanza) %>%
  summarise(pop = sum(X2014)) %>%
  spread(Cittadinanza, pop) %>%
  replace(is.na(.), 0) %>%
  
  popolazione_qt <-   popolazione_qt %>% mutate(sum = rowSums(.[2:4]))
ncol(.)

popolazione_subset <- select(popolazione, Quartiere, Cittadinanza, X2014) %>%
  group_by(Quartiere, Cittadinanza) %>%
  summarise(tot = sum(X2014)) %>%
  filter(Cittadinanza != "Italia") %>%
  group_by(Quartiere) %>%
  filter(tot == max(tot)) %>%
  inner_join(popolazione_qt, by = "Quartiere") %>%
  mutate(prc = round(tot / tot_qt * 100))

popolazione_subset <- 
  quartieri_dict %>%
  mutate(Quartiere = as.character(q_synt)) %>%
  select(id, Quartiere) %>%
    inner_join(
      popolazione_subset
      , by = "Quartiere") %>%
  mutate(id = as.character(id))

write.csv2(popolazione_subset, sep = ";",row.names=FALSE, file = "nazionalita_quartiere.csv")

naz_dict <- read.csv(file = "Dati/dataset/nazionalita_quartiere.csv", header = TRUE, sep = ";")

naz_dict$Area_geografica <-  sapply(levels(naz_dict$Nazionalita),
       # Custom function
       str_match,
       words = levels(popolazione_subset$Cittadinanza)
       )

popolazione_subset <- inner_join(popolazione_subset, naz_dict, by = "Cittadinanza")


milan_qt.wgs84.f <- left_join(milan_qt.wgs84.f, popolazione_subset, by = "id")

