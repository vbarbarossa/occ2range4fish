# Valerio Barbarossa, 19 Sep 2019
# this script generates a table of freshwater fish binomial names and synonyms
# based on IUCN names 

# check with IUCN names
library(valerioUtils)
libinv(c('rredlist','dplyr'))

token <- 'd361026f05b472e57b0ffe1fa5c9a768aaf3d8391abbb464293e9efe2bbbf733'
NC = 22 #no cores to use
# read table of names retrieved from fishbase
tab <- read.csv('proc/names_fishbase.csv',stringsAsFactors = F)
names <- unique(tab$name_synonym)

# st <- Sys.time()
iucn <- parallel::mcmapply(function(i) {
  syn <- rl_synonyms(names[i],key=token)$result
  if(length(syn) > 0){
    syn$name_src = names[i]
    res <- syn %>%
      select(id_iucn = accepted_id, 
             name_iucn = accepted_name, 
             name_iucn_synonym = synonym, 
             name_src = name_src)
  }else{
    res <- data.frame(
      id_iucn = NA, 
      name_iucn = NA, 
      name_iucn_synonym = NA, 
      name_src = names[i])
  }
  return(res)
},1:length(names),SIMPLIFY = FALSE,mc.cores = NC) %>% 
  do.call('rbind',.) %>%
  distinct() # removes eventual duplicated rows
# Sys.time() - st

write.csv(iucn,'proc/iucn_names.csv',row.names = F)
