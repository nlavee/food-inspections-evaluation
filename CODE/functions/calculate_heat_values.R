
## The following fields must be present:
##    inspections$Inspection_ID
##    inspections$Inspection_Date
##    inspections$Latitude
##    inspections$Longitude
##    observed_values$Latitude
##    observed_values$Longitude
##    observed_values$Date

calculate_heat_values <- function(inspections, 
                                  observed_values,
                                  window = 90,
                                  page_limit = 500,
                                  verbose = TRUE){
    require(data.table)
    
    obs_cols <- c("Date", "Latitude", "Longitude")
    insp_cols <- c("Inspection_ID", "Inspection_Date", "Latitude", "Longitude")
    
    ## Check for required columns
    if(!all(obs_cols %in% colnames(observed_values))) {
        stop(paste0("observed_values is missing one of these columns: \n",
                    "      'Latitude', 'Longitude', 'Date'"))
    }
    if(!all(insp_cols %in% colnames(inspections))) {
        stop(paste0("inspections is missing one of these columns: \n",
                    "      'Latitude', 'Longitude', 'Date'"))
    }
    
    ## Subset
    inspections <- inspections[ , insp_cols, with=F]
    observed_values <- observed_values[ , obs_cols, with=F]
    
    ## Filter out NA values
    observed_values <- na.omit(observed_values)
    
    ## Create index values for pages
    N <- nrow(inspections) #number of row in inspection
    START_ROWS <- seq(1, N, page_limit) #go from 1 --> number of row, jump 500 numbers each time, indicating the start of a new bin of rows
    END_ROWS <- c(seq(1, N, page_limit)[-1] - 1, N) # create vector that include all the end rows of the starts row above, also include the stretch from last end row till end of data frame. The negative number in bracket excludes the specific value at that index.
    II <- mapply(`:`, START_ROWS, END_ROWS) #run c(start_row : end rows) for each start_row elements, resulting in a a bunch of vector inside vector that correlates to how we split up this dataset
    
    ret <- rbindlist(lapply(II, function(ii) {
        if(verbose){
            print(paste(sys.call()[2], "out of", length(II)))
        }
        
        foverlaps(    
            x = inspections[i = ii,
                            j = list(Inspection_ID, 
                                     Latitude, 
                                     Longitude), 
                            keyby = list(start = Inspection_Date - window, 
                                         end = Inspection_Date)],
            y = observed_values[i = TRUE, 
                                j = list(Latitude, Longitude),
                                keyby = list(start = Date,  end = Date)],
            type = "any")[ , kde(new=c(i.Latitude[1], i.Longitude[1]), 
                                 x = Latitude, 
                                 y = Longitude, 
                                 h = c(.01, .01)),
                          keyby = Inspection_ID]}))
    setkey(ret, Inspection_ID)
    setnames(ret, "V1", "heat_values")
    return(ret)
}

