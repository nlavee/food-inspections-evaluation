# Taking out rows without latitude, longitude and creation date
# Keep only rows that has either completed or open status

filter_sanitationComplaints <- function(sanitationComplaints) {
    sanitationComplaints <- sanitationComplaints[!is.na(Latitude) & !is.na(Longitude) & !is.na(Creation_Date)]
    sanitationComplaints <- sanitationComplaints[Status %in% c("Completed", "Open")]
    sanitationComplaints
}
