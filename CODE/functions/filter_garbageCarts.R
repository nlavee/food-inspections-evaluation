# Taking out rows without latitude, longitude and creation date
# Keep only rows that has either completed or open status

filter_garbageCarts <- function(garbageCarts){
    garbageCarts <- garbageCarts[!is.na(Latitude) & !is.na(Longitude) & !is.na(Creation_Date)]
    garbageCarts <- garbageCarts[Status %in% c("Completed", "Open")]
    garbageCarts
}
