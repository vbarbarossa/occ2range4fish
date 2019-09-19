#Valerio Barbarossa
# stript that compiles the occurrence records from the different datasets
# ala.org.ay
library(valerioUtils)
libinv(c('dplyr','vroom'))

dir_data <- '../data/fish_databases/'

#ala
ala <- vroom(paste0(dir_data,'ala.org.au/Fishes-brief.csv'),delim = ',') %>%
  select(name = scientificName,lon = decimalLongitude,lat = decimalLatitude) %>%
  .[!is.na(.$lon) & !is.na(.$name),]
#~3M occ
#7,358 names

#boldsystems

#fishnet2
fishnet <- vroom(paste0(dir_data,'fishnet2/fishnet2.csv')) %>%
  select(name = ScientificName,lon = Longitude,lat = Latitude) %>%
  .[!is.na(.$lon) & !is.na(.$name),]
#~4M occ
#62,747 names

#gbif
gbif <- vroom(paste0(dir_data,'gbif/actinopterygii.csv')) %>%
  select(name = species,lon = decimalLongitude,lat = decimalLatitude) %>%
  .[!is.na(.$lon) & !is.na(.$name),]
#~17M occ
#62,747 names

#portalbiodiversidade.icmbio.gov.br
bra <- vroom(paste0(dir_data,'portalbiodiversidade.icmbio.gov.br/portalbio_export_17-09-2019-10-21-11.csv')) %>%
  select(name = Especie,lon = Longitude,lat = Latitude) %>%
  .[.$lon != "Acesso Restrito" & .$name != "Sem Informações",]
#~80k occ
#2,701 names

#splink.org
splink <- vroom(paste0(dir_data,'splink.org/speciesLink_all_112728_20190917101630.txt')) %>%
  select(name = scientificname,lon = longitude,lat = latitude) %>%
  .[!is.na(.$lon) & !is.na(.$name),]

# read reference names that should be used to filter the data
tab <- read.csv('proc/names_fishbase.csv',stringsAsFactors = F)

# bind and filter occurrence records
occ <- rbind(ala,fishnet,gbif,bra,splink) %>%
  filter(name %in% tab$name_synonym) %>%
  select(name_src = name,lon,lat)

o2 <- left_join(occ,tab,by=c('name_src'='name_synonym'))
# 2.2M records
#

