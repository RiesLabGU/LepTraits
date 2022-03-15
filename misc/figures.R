library(tidyverse); library(cowplot); library(ggpubr);
library(sf); library(data.table); library(purrr)

###########################################################
## RESOURCES                                             ##
###########################################################
## read in the resource information
resources <- read.csv("data/book_data.csv")

## read in the record information (for matching to books)
records <- read.csv("data/record_data.csv")

## read in the WGS data
wgs_l1 <- st_read("../../../Github/wgsrpd/level1/level1.shp")
wgs_l2 <- st_read("../../../Github/wgsrpd/level2/level2.shp")

## read in the full dataset to create a backbone of accepted names
names <- fread("data/name_data.csv") %>%
  dplyr::mutate(verbatimSpecies=paste(Genus_asreported, `Species Epithet_asreported`)) %>%
  dplyr::select(NN_Family, NN_Genus, NN_Species, verbatimSpecies) %>%
  unique()

###########################################################
## RECORD-LEVEL TRAITS                                   ##
###########################################################
# Read in measurement data, remove records without information
ws_rec <- read.csv("traits/wingspan_record.csv") %>%
  dplyr::filter(!is.na(WingSpanUpper_Female) | !is.na(WingSpanLower_Female) |
                  !is.na(WingSpanUpper_Male) | !is.na(WingSpanLower_Male) |
                  !is.na(WingSpanUpper_Unspecified) | !is.na(WingSpanLower_Unspecified)) %>%
  dplyr::mutate(UniqueID=as.character(UniqueID))
fw_rec <- read.csv("traits/forewing_record.csv") %>%
  dplyr::filter(!is.na(ForewingUpper_Female) | !is.na(ForewingLower_Female) |
                  !is.na(ForewingUpper_Male) | !is.na(ForewingLower_Male) |
                  !is.na(ForewingUpper_Unspecified) | !is.na(ForewingLower_Unspecified)) %>%
  dplyr::mutate(across(where(is.logical), as.numeric),
                ForewingUpper_Unspecified=as.numeric(ForewingUpper_Unspecified),
                UniqueID=as.character(UniqueID))

# Read in phenology data, remove records without information
ph_rec <- read.csv("traits/phenology_record.csv") %>%
  dplyr::filter(Duration != 0) %>%
  dplyr::mutate(UniqueID=as.character(UniqueID))

# Read in other trait data (already pre-filtered to exclude missing data)
ow_rec <- read.csv("traits/overwintering_record.csv") %>%
  dplyr::mutate(UniqueID=as.character(UniqueID)) # overwintering stages
ov_rec <- read.csv("traits/oviposition_record.csv") %>%
  dplyr::mutate(UniqueID=as.character(UniqueID)) # oviposition styles
vo_rec <- read.csv("traits/voltinism_record.csv") %>%
  dplyr::mutate(UniqueID=as.character(UniqueID)) # voltinisms
ha_rec <- read.csv("traits/habitat_record.csv") %>%
  dplyr::mutate(UniqueID=as.character(UniqueID)) # habitat affinities

## get all unique record IDs
uniqueIDs <- c(ws_rec$UniqueID, fw_rec$UniqueID,
               ph_rec$UniqueID, ow_rec$UniqueID,
               ov_rec$UniqueID, vo_rec$UniqueID) %>% unique()

# Join all the records together for records.csv file
records.final <- records %>%
  left_join(ws_rec, by=c("UniqueID"="UniqueID")) %>%
  left_join(fw_rec, by=c("UniqueID"="UniqueID")) %>%
  left_join(ow_rec, by=c("UniqueID"="UniqueID")) %>%
  left_join(ov_rec, by=c("UniqueID"="UniqueID")) %>%
  left_join(vo_rec, by=c("UniqueID"="UniqueID")) %>%
  left_join(ph_rec, by=c("UniqueID"="UniqueID")) %>%
  left_join(ha_rec, by=c("UniqueID"="UniqueID")) %>%
  dplyr::select(UniqueID, Book, PageNumber=Page.Number, Genus=Genus.x,
                Species=Species.x, WingSpanLower_Female, WingSpanUpper_Female,
                WingSpanLower_Male, WingSpanUpper_Male, WingSpanLower_Unspecified,
                WingSpanUpper_Unspecified, ForewingLower_Female, ForewingUpper_Female,
                ForewingLower_Male, ForewingUpper_Male, ForewingLower_Unspecified,
                ForewingUpper_Unspecified, Overwintering, Oviposition, Voltinism=Final.Code,
                Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec, Duration,
                Canopy=canopy, Edge=ForestEdgeGap, Moisture=moisture, Disturbance=disturbance)
write.csv(records.final, "records.csv")

## keep only records for which we have a trait extracted, match each
## record to the book data
records_rec <- dplyr::filter(records, UniqueID %in% uniqueIDs) %>%
  left_join(resources, by=c("Book"="Title")) %>% 
  dplyr::select("Book", "Author.s.", "World.Geographic.Scheme") %>%
  unique()
  
records_rec <- dplyr::filter(records, UniqueID %in% uniqueIDs) %>%
  left_join(resources, by=c("Book"="Title")) %>% unique() %>%
  dplyr::select("UniqueID", "World.Geographic.Scheme")

records_dupe <- records_rec %>%
  dplyr::select(UniqueID, `World.Geographic.Scheme`) %>%
  dplyr::mutate(WGS=str_extract_all(`World.Geographic.Scheme`, "[0-9]{2}"))

flatten <- function(indt, cols, drop = FALSE) {
  require(data.table)
  if (!is.data.table(indt)) indt <- as.data.table(indt)
  x <- unlist(indt[, lapply(.SD, function(x) max(lengths(x))), .SDcols = cols])
  nams <- paste(rep(cols, x), sequence(x), sep = "_")
  indt[, (nams) := unlist(lapply(.SD, data.table::transpose), recursive = FALSE), .SDcols = (cols)]
  if (isTRUE(drop)) indt[, (cols) := NULL]
  indt[]
}

flattenLong <- function(indt, cols) {
  ob <- setdiff(names(indt), cols)
  x <- flatten(indt, cols, TRUE)
  mv <- lapply(cols, function(y) grep(sprintf("^%s_", y), names(x)))
  setorderv(melt(x, measure.vars = mv, value.name = cols), ob)[]
}

records_dupe <- flattenLong(records_dupe, "WGS") %>%
  dplyr::filter(!is.na(WGS)) %>%
  dplyr::mutate(WGS=as.numeric(WGS))

## create a WGS L1 map of resource count
resource_wgs1 <- records_dupe %>%
  dplyr::select("World.Geographic.Scheme", "WGS") %>%
  unique()

resource_wgs1 <- resource_wgs1 %>% group_by(WGS) %>%
  dplyr::mutate(n=n()) %>% ungroup() %>%
  dplyr::select(WGS, n) %>% unique()

resource_wgs1_sf <- wgs_l2 %>% inner_join(resource_wgs1,
                                        by=c("LEVEL2_COD"="WGS"))
ggplot()+
  geom_sf(wgs_l2, mapping=aes(), fill="grey70", color="white")+
  geom_sf(resource_wgs1_sf, mapping=aes(fill=n), color="white")+
  scale_fill_viridis_c(option="mako", end=0.8, breaks=c(1,3,5,7,9),
                       name="Number of \nResources", limits=c(1,9), direction=-1)+
  theme_map()+
  theme(legend.position=c(0.1, 0.37), legend.key.width=unit(0.75, "cm"),
        legend.key.height=unit(0.75, "cm"))
ggsave("WGS_L2Resource.png", dpi=350)

## create a WGS L1 map by record count
record_rec_wgs1 <- records_dupe %>% group_by(WGS) %>%
  dplyr::mutate(n=n()) %>% ungroup() %>%
  dplyr::select(WGS, n) %>% unique()

record_wgs1_sf <- wgs_l2 %>% inner_join(record_rec_wgs1,
                                        by=c("LEVEL2_COD"="WGS"))
ggplot()+
  geom_sf(wgs_l2, mapping=aes(), fill="grey70", color="white")+
  geom_sf(record_wgs1_sf, mapping=aes(fill=n), color="white")+
  scale_fill_viridis_c(option="mako", end=0.8, direction=-1,
                       name="Number of \nRecords", limits=c(1,6000),
                       breaks=c(1,2000,4000,6000))+
  theme_map()+
  theme(legend.position=c(0.1, 0.37), legend.key.width=unit(0.75, "cm"),
        legend.key.height=unit(0.75, "cm"))
ggsave("WGS_L2Record.png", dpi=350)


consensusTraitsFinal <- read.csv("../consensus/consensus.csv")

# Pull summary statistics for each trait for the manuscript
nrow(consensusTraitsFinal)-sum(rowSums(is.na(consensusTraitsFinal[,5:10]))==6) # wingspans
nrow(consensusTraitsFinal)-sum(rowSums(is.na(consensusTraitsFinal[,11:16]))==6) # forewing lengths
nrow(consensusTraitsFinal)-sum(rowSums(is.na(consensusTraitsFinal[,29]))==1) # adult flight phenologies
nrow(consensusTraitsFinal)-sum(rowSums(is.na(consensusTraitsFinal[,30]))==1) # diapause
nrow(consensusTraitsFinal)-sum(rowSums(is.na(consensusTraitsFinal[,31]))==1) #  voltinism
nrow(consensusTraitsFinal)-sum(rowSums(is.na(consensusTraitsFinal[,32]))==1) # oviposition
nrow(consensusTraitsFinal)-sum(rowSums(is.na(consensusTraitsFinal[,33]))==1) # canopy affinity
nrow(consensusTraitsFinal)-sum(rowSums(is.na(consensusTraitsFinal[,34]))==1) # edge affinity
nrow(consensusTraitsFinal)-sum(rowSums(is.na(consensusTraitsFinal[,35]))==1) # moisture affinity
nrow(consensusTraitsFinal)-sum(rowSums(is.na(consensusTraitsFinal[,36]))==1) # disturbance affinity
nrow(consensusTraitsFinal)-sum(rowSums(is.na(consensusTraitsFinal[,37:42]))==6) # disturbance affinity

# Pull summary statistics for each family
table(consensusTraitsFinal$Family)

# Number of represented genera
length(unique(consensusTraitsFinal$Genus))


  





