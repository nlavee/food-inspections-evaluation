# This script cleans up food inspection dataset by removing entry without inspection date & license, duplicate id for inspection, license has to be different from 0 (0 indicates no license)
# Filtered dataset also only take in inspection after 2011-09-01
# Canvas inspection : the most common type of inspection performed at a frequency relative to the risk of the establishment
# Also, take out inspection with results that are either 'Out of Business', 'Business Not Located', and 'No Entry'

filter_foodInspect <- function(foodInspect){
    foodInspect <- foodInspect[!is.na(Inspection_Date) & !is.na(License)]
    foodInspect <- foodInspect[!duplicated(Inspection_ID)]
    foodInspect <- foodInspect[License != 0]
    foodInspect <- foodInspect[Inspection_Date > as.IDate("2011-09-01")]
    foodInspect <- foodInspect[Inspection_Type == "Canvass"]
    foodInspect <- foodInspect[!Results %in% c('Out of Business',
                                               'Business Not Located',
                                               'No Entry')]
    foodInspect
}
