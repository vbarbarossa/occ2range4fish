#Valerio Barbarossa
# stript that compiles the occurrence records from the different datasets

library(valerioUtils)
libinv(c('dplyr','vroom'))

# diagnostics file
file_diag <- 'filtering_occurrence_datasets_diag.log'

diag <- function(df,name_df = '',file_out = '',append = T){
  
  if(file_out == ''){
    cat('Dataset: ',name_df,'\n',
        'No. records: ',prettyNum(nrow(df),big.mark = ','),'\n',
        'No. species: ',prettyNum(length(unique(df$name)),big.mark = ','),'\n\n\n')
  }else{
    cat('Dataset: ',name_df,'\n',
        'No. records: ',prettyNum(nrow(df),big.mark = ','),'\n',
        'No. species: ',prettyNum(length(unique(df$name)),big.mark = ','),'\n\n\n',
        file = file_out,append = append)
  }
}

# read reference names that should be used to extract species from the datasets----------------------------
# tab <- read.csv('proc/names_fishbase.csv',stringsAsFactors = F)
fishbase <- read.csv('proc/names_fishbase_and_tedesco.csv',stringsAsFactors = F)
tab <- vroom(list.files('proc/iucn_synonyms/',pattern = 'iucn_names',full.names = T)) %>%
  filter(!is.na(name_src))

#assign names that will be used for filtering
# filter_names <- unique(tab$name_synonym)
filter_names <- unique(c(tab$name_iucn,tab$name_iucn_synonym,tab$name_src,fishbase$name,fishbase$name_synonym)) %>%
  .[!is.na(.)]
cat('Using ',prettyNum(length(filter_names),big.mark = ','),' species names for records extraction\n',
    file = file_diag)

# functions to clean nomenclature--------------------------------------------------------------------------

# function that removes any trailing word with capital (e.g., Linneaus)
rm_author <- function(x){
  s <- strsplit(x,' ')
  if(length(s[[1]]) > 2){
    s3 <- strsplit(s[[1]][3],'')
    if(s3[[1]][1] %in% LETTERS){
      return(
        paste(unlist(s)[1:2],collapse=' ')
      )
    }else{
      return(x)
    }
  }else{
    return(x)
  }
}

# function that removes anything after , or ( and trailing capitals
clean_binomial <- function(names){
  cl <- lapply(strsplit(names,','),function(x) x[[1]][1]) %>% 
    unlist() %>%
    strsplit(.,'\\(') %>%
    lapply(.,function(x) x[[1]][1]) %>%
    trimws() %>%
    lapply(.,rm_author) %>%
    unlist
  return(cl)
}

# clean and filter datasets--------------------------------------------------------------------------------
# setup location of datasets
dir_data <- '../data/fish_databases/'

# ala.org.au
ala <- vroom(paste0(dir_data,'ala.org.au/Fishes-brief.csv'),delim = ',') %>%
  select(name = scientificName,lon = decimalLongitude,lat = decimalLatitude) %>%
  .[!is.na(.$lon) & !is.na(.$name),] %>%
  mutate(name = clean_binomial(name)) %>%
  filter(name %in% filter_names) %>%
  distinct()
diag(ala,'ala.org.au')
diag(ala,'ala.org.au',file_diag)

#boldsystems #not implemented

#fishnet2
fishnet <- vroom(paste0(dir_data,'fishnet2/fishnet2.csv')) %>%
  select(name = ScientificName,lon = Longitude,lat = Latitude) %>%
  .[!is.na(.$lon) & !is.na(.$name),] %>%
  mutate(name = clean_binomial(name)) %>%
  filter(name %in% filter_names) %>%
  distinct()

diag(fishnet,'fishnet2')
diag(fishnet,'fishnet2',file_diag)


#gbif
gbif_o <- vroom(paste0(dir_data,'gbif/actinopterygii.csv'))

# based on species
gbifs <- gbif_o %>%
  select(name = species,lon = decimalLongitude,lat = decimalLatitude) %>%
  .[!is.na(.$lon) & !is.na(.$name),] %>%
  mutate(name = clean_binomial(name)) %>%
  filter(name %in% filter_names)

# based on scientific name
gbifn <- gbif_o %>%
  select(name = scientificName,lon = decimalLongitude,lat = decimalLatitude) %>%
  .[!is.na(.$lon) & !is.na(.$name),] %>%
  mutate(name = clean_binomial(name)) %>%
  filter(name %in% filter_names)

gbif <- bind_rows(gbifs,gbifn) %>%
  distinct()

# checknames <- gbif_o %>%
#   mutate(species = clean_binomial(species),scientificName = clean_binomial(scientificName))
# ss <- sort(table(checknames$species),decreasing = T)
# sn <- sort(table(checknames$scientificName),decreasing = T)

diag(gbif,'gbif')
diag(gbif,'gbif',file_diag)
rm(gbif_o) # unload gbif_o from memory


#portalbiodiversidade.icmbio.gov.br
bra <- vroom(paste0(dir_data,'portalbiodiversidade.icmbio.gov.br/portalbio_export_17-09-2019-10-21-11.csv')) %>%
  select(name = Especie,lon = Longitude,lat = Latitude) %>%
  .[.$lon != "Acesso Restrito" & .$name != "Sem Informações",] %>%
  mutate(name = clean_binomial(name)) %>%
  filter(name %in% filter_names) %>%
  distinct()
diag(bra,'portalbiodiversidade.icmbio.gov.br')
diag(bra,'portalbiodiversidade.icmbio.gov.br',file_diag)


#splink.org
splink <- vroom(paste0(dir_data,'splink.org/speciesLink_all_112728_20190917101630.txt')) %>%
  select(name = scientificname,lon = longitude,lat = latitude) %>%
  .[!is.na(.$lon) & !is.na(.$name),] %>%
  mutate(name = clean_binomial(name)) %>%
  filter(name %in% filter_names) %>%
  distinct()
diag(splink,'splink.org')
diag(splink,'splink.org',file_diag)

# combine records from different datasets------------------------------------------------------------------

# bind and filter occurrence records
occ <- rbind(ala,fishnet,gbif,bra,splink) %>%
  arrange(name) %>%
  distinct()

# there are still some lat and lon coordinates with letters in there e.g., 1044730E
# difficult to convert without knowing the CRS, filter out!
# create vector to report rows with letters in latitude or longitude

# understand unique symbols to take outsbatch 
unique_symbols <- strsplit(paste0(occ$lon,occ$lat),'') %>%
  do.call('c',.) %>%
  unique(.)

# exclude everything that is not numeric (numbers,.,-)
# checkout biogeo package also for more through corrections
to_exclude <- unique_symbols[!unique_symbols %in% c(as.character(0:9),'.','-')] 
lonlat_to_exclude <- lapply(strsplit(paste0(occ$lon,occ$lat),''),
                            function(x) sum(x %in% to_exclude)> 0 ) %>%
  do.call('c',.)

# and filter occ
cat('Removing ',prettyNum(sum(lonlat_to_exclude),big.mark = ','),' records with letters in coordinates\n',
    file = file_diag,append = T)

occ <- occ %>%
  filter(!lonlat_to_exclude) %>%
  mutate(name,lon = as.numeric(lon),lat = as.numeric(lat)) %>% # transform lat lon to numeric
  filter(!is.na(lon) & !is.na(lat)) %>% #make sure there are no NAs in the coords
  filter(lon >= -180 & lon <= 180) %>% # and no coords outside allowed boundaries
  filter(lat >= -90 & lat <= 90)


# merge with fishbase synonyms----------------------------------------------------------------------------

# read fishbase + tedesco synonym tab
syn_fb <- fishbase %>%
  as_tibble() %>%
  select(name_ref = name,name = name_synonym)

# get only synonyms table and adjust columns name
syn_iucn <- tab %>%
  filter(name_src != name_iucn_synonym) %>% 
  select(name_ref = name_src,name = name_iucn_synonym)

# which have different src and iucn name?
# then consider the different iucn name as fishbase synonym
diff_iucn_fb <- tab %>%
  filter(!is.na(name_iucn)) %>%
  filter(name_iucn != name_src) %>%
  select(name_ref = name_src,name = name_iucn)

tab_ref <- bind_rows(syn_fb,syn_iucn,diff_iucn_fb) %>%
  filter(!is.na(name) & !is.na(name_ref)) %>%
  distinct()

tab_syn <- tab_ref %>%
  filter(name_ref != name)

occ_syn <- inner_join(occ,tab_syn,by='name') %>% #select only records that are synonyms
  select(name = name_ref,lon,lat) %>% # and assign the fishbase name to the name
  distinct()

occ_nosyn <- occ %>%
  filter(name %in% unique(tab_ref$name_ref))

diag(occ_nosyn,'merged occurrence records (no synonyms)',file_diag)
diag(occ_syn,'additional merged occurrence records from fishbase synonyms',file_diag)

occ_total <- bind_rows(occ_nosyn,occ_syn) %>%
  arrange(name) %>%
  distinct()

diag(occ_total,'final occurrence records cleaned and checked for synonyms',file_diag)

vroom_write(occ_total,'proc/compiled_occurrence_records.tsv.gz')
saveRDS(occ_total,'proc/compiled_occurrence_records.rds')
