
## Requires a vector of violations in text format, 
## where each element is a collapsed list of violations
## separated by |
## Requires a vector of key values to be assigned to the 
## result for merging after the function is run.
##

calculate_violation_types <- function(violation_text, ...){
    
    require(data.table)
    
    ## Tabluate voilation types
    ## 1) Split violoation description by "|"
    ## 2) use regex to extract leading digits of code number
    ## 3) create indicator matrix of code violations
    ## 4) use apply to total up each group of code violations
    vio <- strsplit(violation_text,"| ",fixed=T)
    vio_nums <- lapply(vio, 
                       function(item) regmatches(x = item, 
                                                 m = gregexpr(pattern = "^[0-9]+", 
                                                              text = item)))
    vio_mat <- geneorama::list2matrix(vio_nums, count = T) ### Converts a list of vectors into an indicator matrix that spans the universe of the list elements
    vio_mat <- vio_mat[ , order(as.numeric(colnames(vio_mat)))]
    # colnames(vio_mat)
    # range(vio_mat)
    
    criticalCount <- apply(vio_mat[ , colnames(vio_mat) %in% 1:14], 1, sum)
    seriousCount <- apply(vio_mat[ , colnames(vio_mat) %in% 15:29], 1, sum)
    minorCount <- apply(vio_mat[ , colnames(vio_mat) %in% 30:44], 1, sum)
    
    ## Extract the key from the ...'s
    key_vec <- list(...) ### Put inspection id into a list
    
    ## Check if the key is in the ...'s
    if(length(key_vec) != 1) {
        stop("A key vector is required as the second argument")
    }
    
    ## Check key length
    if(length(key_vec[[1]]) != length(violation_text)){
        stop("The length of the key must match the length of the first argument") ## Making sure that we have as many violation box as inspections. In another word, all violations are accopmanied with a specific inspection id
    }
    
    ## Construct return values
    ret <- data.table(criticalCount,
                      seriousCount,
                      minorCount)
    
    data.table::set(x = ret, 
                    j = names(key_vec), # this is inspection id list
                    value = key_vec[[1]]) # get the actual inspection value
    setkeyv(ret, names(key_vec)) # this line sorts the data table by inspection number
    return(ret)
}
