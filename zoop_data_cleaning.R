## Purpose: clean up Drake lab Carolina bay zoop data for ECOL 4000/6000 class
## What needs to be done: clean up taxonomic data

## Libraries and working directory, load data #### 
setwd("/Users/katieschroeder/Downloads")
zgroup <- read.csv("doi_10.5061_dryad.541q2__v1/data/SRS_Zoop_species_groups.csv")
zoop <- read.csv("doi_10.5061_dryad.541q2__v1/data/zoopData.csv")
library(tidyverse)
library(magrittr)

## starting to do taxonomic ID #### 
#Merge zoop group (gives clade) with zoop data
zoop2 <- merge(zgroup,zoop,by="taxa",all=TRUE)

# get unique taxa
ids <- as.data.frame(unique(zoop2$taxa)) %>% rename("taxa" = "unique(zoop$taxa)")

# mutate to get genus and species
ids <- ids %>% separate_wider_delim(cols=taxa,delim=" ", names = c("genus","species"),too_few = "debug",too_many="debug")

#a lot of cf, so make new column that has an ID certainty and get rid of "cf. "
ids <- ids %>% 
  mutate(ID_certainty = ifelse(str_detect(taxa,"cf. "),"uncertain","confident"))

#now remove cf. from each of the taxa that have it so splitting the genus and species actually works
ids <- ids %>% mutate(taxa = gsub('cf. ','',taxa)) %>%
  select(taxa, ID_certainty)

#redo separation
ids <- ids %>% 
  separate_wider_delim(cols=taxa,delim=" ", names = c("genus","species"),too_few = "debug",too_many="debug")

# deal with the debug ones - sp. A can become just sp. and rustica americana needs to become one string, NA becomes sp.
#then remove delim separate columns
ids<- ids %>% mutate(
  species = case_when(
    is.na(species) ~ "sp.",
    species == "rustica" ~ "rustica americana",
    TRUE ~ species
  )
)  %>% 
  select(-c(taxa_ok,taxa_pieces,taxa_remainder))

#some of these genera aren't actually genera (i.e. are classes)
#get a list of things ID-ed to sp. and double check
filter(ids,species=="sp.")

#change Harpacticoida (order) and Ostracoda (class) sp. to NA
ids %<>% mutate(species = case_when(
  genus=="Harpacticoida" ~ NA,
  genus=="Ostracoda" ~ NA,
  TRUE ~ species
))
  
#now add ID specificity level - sp. become to genus, other things to species, and two special cases from above become order and class
ids %<>% mutate(
  ID_specificity = case_when(
  species == "sp." ~ "genus",
  genus == "Harpacticoida" ~ "order",
  genus == "Ostracoda" ~ "class",
  TRUE ~ "species"
))

## now let's add a bunch of taxonomic info and try to use the taxize package ####
library(taxize)

gen_uniq <- unique(ids$genus)
gen_uniq_short <- gen_uniq[1:38] 

# function for pulling classification details ("phylum" in this case)
get_sys_order <- function(x){ require(taxize)
  a <- classification(get_uid(x))
  y <- data.frame(a[[1]])                        # if there are multiple results, take the first..
  z <- tryCatch(as.character(y[which(y[,2] == "order"), 1]),    # in case of any other errors put NA
                error = function(e) NA)
  z <- ifelse(length(z) != 0, z, NA)
  return(data.frame(taxa = x, order = z))
}

get_sys_class <- function(x){ require(taxize)
  a <- classification(get_uid(x))
  y <- data.frame(a[[1]])                        # if there are multiple results, take the first..
  z <- tryCatch(as.character(y[which(y[,2] == "class"), 1]),    # in case of any other errors put NA
                error = function(e) NA)
  z <- ifelse(length(z) != 0, z, NA)
  return(data.frame(taxa = x, order = z))
}

get_sys_family <- function(x){ require(taxize)
  a <- classification(get_uid(x))
  y <- data.frame(a[[1]])                        # if there are multiple results, take the first..
  z <- tryCatch(as.character(y[which(y[,2] == "family"), 1]),    # in case of any other errors put NA
                error = function(e) NA)
  z <- ifelse(length(z) != 0, z, NA)
  return(data.frame(taxa = x, order = z))
}
# call function and rbind the returned values 
result <- do.call(rbind, lapply(gen_uniq_short, get_sys_class))
result2 <- do.call(rbind, lapply(gen_uniq_short, get_sys_order))
result3 <- do.call(rbind, lapply(gen_uniq_short, get_sys_family))


## do it by hand ####
## using http://cfb.unh.edu/cfbkey/html/groups.html
ids %<>%
  mutate(family = case_when(
    str_detect(genus,"Bosmina") ~ "Bosminidae",
    str_detect(genus,"Alona|Alonella|Acroperus|Camptocercus|Chydorus|Disparalona|Eurycercus|Kurzia|Oxyurella|Dunhevedia|Ephemeroporus|Karualona|Paralona|Picripleuroxus|Pseudochydorus") ~ "Chydoridae",
    str_detect(genus,"Ceriodaphnia|Daphnia|Scapholebris|Simocephalus") ~ "Daphniidae",
    str_detect(genus,"Aglaodiaptomus|Hesperodiaptomus|Leptodiaptomus|Onychodiaptomus") ~ "Diaptomidae",
    str_detect(genus,"Diaphanosoma|Pseudosida") ~ "Sididae",
    str_detect(genus,"Acanthocyclops|Diacyclops|Ectocyclops|Eucyclops|Macrocyclops|Mesocyclops|Microcyclops|Orthocyclops|Tropocyclops|Homocyclops|Thermocyclops|Megacyclops|Paracyclops") ~ "Cyclopidae",
    str_detect(genus,"Eubranchipus") ~ "Chirocephelidae",
    str_detect(genus,"Moina|Moinodaphnia") ~ "Moinidae",
    str_detect(genus,"Harpacticoida") ~ NA,
    str_detect(genus,"lyocryptus") ~ "Ilyocryptidae",
    str_detect(genus,"Lathonura|Macrothrix|Streblocercus|Grimaldina|Acantholebris") ~ "Macrothricidae",
    str_detect(genus,"Ostracoda") ~ NA,
    str_detect(genus,"Polyphemus") ~ "Polyphemidae",
    str_detect(genus,"Lynceus") ~ "Lynceidae",
    str_detect(genus,"Streptocephalus") ~ "Streptocephalidae",
    TRUE ~ "check it"
  ),
  
  order = case_when(
    family == "Cyclopidae" ~ "Cyclopoida",
    family == "Diaptomidae" ~ "Calanoida",
    str_detect(family,"Bosminidae|Chydoridae|Daphniidae|Ilyocryptidae|Macrothricidae|Sididae|Polyphemidae|Moinidae") ~ "Cladocera",
    str_detect(family,"Chirocephelidae|Streptocephalidae") ~ "Anostraca",
    genus == "Ostracoda" ~ NA,
    family == "Lynceidae" ~ "Laevicaudata",
    genus == "Harpacticoida" ~ "Harpacticoida",
    TRUE ~ "check it"
  ),
  
  genus = ifelse(genus== "Harpacticoida",NA,genus),
  
  class = case_when(
    order == "Cladocera" ~ "Branchiopoda",
    order == "Anostraca" ~ "Branchiopoda",
    order == "Laevicaudata" ~ "Branchiopoda",
    is.na(order) ~ "Ostracoda",
    str_detect(order,"Cyclopoida|Calanoida|Harpacticoida") ~ "Copepoda"
  ))

## woooo taxonomic info has been added



## more zoop cleaning #### 
## work with original zoop df and do similar steps to get genus and species column and cleaner species
ids <- as.data.frame(unique(zoop2$taxa)) %>% rename("taxa" = "unique(zoop$taxa)")

# mutate to get genus and species
ids <- ids %>% separate_wider_delim(cols=taxa,delim=" ", names = c("genus","species"),too_few = "debug",too_many="debug")

#a lot of cf, so make new column that has an ID certainty and get rid of "cf. "
ids <- ids %>% 
  mutate(ID_certainty = ifelse(str_detect(taxa,"cf. "),"uncertain","confident"))

#now remove cf. from each of the taxa that have it so splitting the genus and species actually works
ids <- ids %>% mutate(taxa = gsub('cf. ','',taxa)) %>%
  select(taxa, ID_certainty)

#redo separation
ids <- ids %>% 
  separate_wider_delim(cols=taxa,delim=" ", names = c("genus","species"),too_few = "debug",too_many="debug")

# deal with the debug ones - sp. A can become just sp. and rustica americana needs to become one string, NA becomes sp.
#then remove delim separate columns
ids<- ids %>% mutate(
  species = case_when(
    is.na(species) ~ "sp.",
    species == "rustica" ~ "rustica americana",
    TRUE ~ species
  )
)  %>% 
  select(-c(taxa_ok,taxa_pieces,taxa_remainder))

#some of these genera aren't actually genera (i.e. are classes)
#get a list of things ID-ed to sp. and double check
filter(ids,species=="sp.")

#change Harpacticoida (order) and Ostracoda (class) sp. to NA
ids %<>% mutate(species = case_when(
  genus=="Harpacticoida" ~ NA,
  genus=="Ostracoda" ~ NA,
  TRUE ~ species
))

#now add ID specificity level - sp. become to genus, other things to species, and two special cases from above become order and class
ids %<>% mutate(
  ID_specificity = case_when(
    species == "sp." ~ "genus",
    genus == "Harpacticoida" ~ "order",
    genus == "Ostracoda" ~ "class",
    TRUE ~ "species"
  ))

# Deal with other zoops ####
z1 <- data.frame(unique(zoop$taxa)) %>% rename("taxa" = "unique.zoop.taxa.")
z2 <- data.frame(unique(zgroup$taxa)) %>% rename("taxa" = "unique.zgroup.taxa.")
z3<-rbind(z1,z2)
z3<-unique(z3$taxa)

write.csv(z3,"zoop_taxa.csv")
write.csv(ids_test,"ids.csv")

## look at samples/bay/sampling date ####
# there might be multiple samples taken from each bay. We want to clean those up and have one data frame with one pooled sample and one with each of the samples if so

#need to group by bay, sample date
zoop <- zoop[,1:18] #annoyingly has tons of excess columns so subset it

zoop %<>% filter(taxa != "") #there are a ton of empty rows, so get rid of them

# try grouping by both month and year and day,month,year
zoop %<>% mutate(sample_date = paste(month,year,sep='_'))
zoop %<>% mutate(sample_date_long = paste(month,date,year,sep='_'))

#this is summarized by month and year for each taxa in each bay
zoop_summed <- zoop %>% group_by(sample_date,bay,taxa) %>%
  summarize(abund = sum(abund,na.rm=T),
            male = sum(male,na.rm=T),
            female = sum(female,na.rm=T),
            repro = sum(repro,na.rm=T),
            eph = sum(eph,na.rm = T),
            density = sum(density,na.rm=T),
            #sample_depth = unique(sampleDepth),
            volume = sum(volume)) 

#this is summarized by sampling date (day, month, and year) for each taxa in each bay
zoop_summed_long <- zoop %>% group_by(sample_date_long,bay,taxa) %>%
  summarize(abund = sum(abund,na.rm=T),
            male = sum(male,na.rm=T),
            female = sum(female,na.rm=T),
            repro = sum(repro,na.rm=T),
            eph = sum(eph,na.rm = T),
            density = sum(density,na.rm=T),
            #sample_depth = unique(sampleDepth),
            volume = sum(volume)) 

## add soil, canopy, information ####
bay_info <- read.csv("SRS_bay_summary_table.csv")
bay_info %<>% rename(bay=Bay)


bay_info$Hydro_days <- bay_info$Hydro_days/2

#change soil to full term
bay_info$Soil <- gsub('up','upland',bay_info$Soil)
bay_info$Soil <- gsub('sh','sand',bay_info$Soil)
bay_info$Soil <- gsub('ter','terrace',bay_info$Soil)

bay_info$Canopy <- gsub('o','open',bay_info$Canopy)
bay_info$Canopy <- gsub('f','forrested',bay_info$Canopy)

## merge everything together:
zoop_combo<-merge(zoop_summed_long,bay_info,by="bay")

str(zoop_combo)
zoop_combo %<>% rename(mean_temp_C=MeanTempC,
                       temp_var=TempVariance,
                       conductivity=Cond,
                       hydro_days=Hydro_days,
                       soil=Soil,
                       canopy=Canopy,
                       area_ha=AreaHa)
zoop_combo %<>% separate_wider_delim(cols=sample_date_long,delim="_",names=c("month","day","year"))

zoop_combo$date<-as.Date(with(zoop_combo,paste(year,month,day,sep="-")), "%Y-%m-%d")

## add taxanomic info - need to clean up names
#remove cf.
zoop_combo %<>% mutate(taxa = gsub('cf. ','',taxa))
zoop_combo %<>% filter(taxa != "") 
zoop_combo %<>% mutate(taxa = case_when(
  str_detect(taxa,"Amphibia") ~ gsub('Amphibia: ','',taxa),
  str_detect(taxa,"Arachnida") ~ gsub('Arachnida: ','',taxa),
  str_detect(taxa,"Crayfish") ~ gsub('Crayfish','Decapoda',taxa),
  str_detect(taxa,"Dytiscidae: larvae") ~ gsub('Coleoptera: Dytiscidae: larvae', 'Dytiscidae larva',taxa),
  str_detect(taxa,"Coleoptera: larva") ~ gsub('Coleoptera: larva','Coleoptera larva',taxa),
  str_detect(taxa,"Coleoptera: ") ~ gsub('Coleoptera: ','',taxa),
  str_detect(taxa,"Diptera: larva") ~ gsub('Diptera: larva','Diptera larva',taxa),
  str_detect(taxa,"Diptera: ") ~ gsub('Diptera: ','',taxa),
  str_detect(taxa,"Hemiptera: larvae") ~ gsub('Hemiptera: larvae','Hemiptera larva',taxa),
  str_detect(taxa,"Hemiptera: ") ~ gsub('Hemiptera: ','',taxa),
  str_detect(taxa,"Neuroptera") ~ gsub('Neuroptera: ','',taxa),
  str_detect(taxa,"Odonata") ~ gsub('Odonata: ','',taxa),
  TRUE ~ taxa))

#time for the big merge
taxonomy <- read.csv("~/Documents/GitHub/ECOL_4000_6000/ids_taxanomic_info.csv")

big_zoop<-merge(zoop_combo,taxonomy,by="taxa")
observations<-as.data.frame(table(big_zoop$taxa))

write.csv(big_zoop,"cleaned_SRS_zoop_data.csv")
