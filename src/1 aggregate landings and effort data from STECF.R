
datadir <- "w://imares/ijmuiden/afdeling/projecten/data poor mixed fisheries/robin hood/data"

#this data is downloaded from STECF effort data base 
landings <- read.csv(paste0(datadir,"/map__landings_by_rectangle_data.csv"), stringsAsFactors = F)
landings <- landings[landings$species %in% c("PLE","SOL","TUR","BLL"),]
landings[!landings$regulated.gear %in% c("BT2","TR1"),]$regulated.gear <- "OTHER"
landings <- landings[landings$regulated.gear %in% c("BT2","TR1", "OTHER"),]
lanbygearyear <- aggregate(Landings~regulated.gear + species +year, data=landings, FUN="sum")


#this data is downloaded from STECF effort data base 
effort <- read.csv(paste0(datadir,"/map__effort_by_rectangle_data.csv"), stringsAsFactors = F)
effort[!effort$regulated.gear %in% c("BT2","TR1"),]$regulated.gear <- "OTHER"
effort <- effort[effort$regulated.gear %in% c("BT2","TR1", "OTHER"),]
effbygearyear <- aggregate(Effective.Effort~regulated.gear +year, data=effort, FUN="sum")

