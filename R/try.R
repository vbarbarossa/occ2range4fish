pacman::p_load(spocc,purrr)

spted <- read.csv('D:/fishsuit/data/Tedesco/Occurrence_Table.csv',sep = ';')

names <- unique(gsub('\\.',' ',spted$X6.Fishbase.Valid.Species.Name))

res <- occ(names[1],from = c('gbif','ecoengine'),has_coords = T,limit = 10**6)
# res
# res$gbif

library("mapr")
map_leaflet(res)


library('rgbif')
occ_count(georeferenced=TRUE)
res <- occ_search(scientificName = names[1], limit = 2000,hasCoordinate = T)
occ_download(scientificName = names[1])

key <- name_suggest(q=names[1], rank='species')$key[1]
occ_search(taxonKey=key, limit=20)

# need to do occ_download or directly from website
# spocc::occ or rgbif::occ_search are made for retrieving small number of species
# API cannot be used to download all actinoperigii fish (or loop through species)
occ_download(paste0('taxonKey = ',key), format = "SIMPLE_CSV",
             user = 'vbarbarossa', pwd = '1Rocker..11', email = 'v.barbarossa@fnwi.ru.nl')

occ_download(parsenames(ref_names))


