pollutantmean <- function(directory, pollutant, id = 1:332){
    summation <- 0
    numberEntries <- 0
    for(i in id){
        ## opening the file
        if(i < 10){
            file_ex <- paste("00", i, sep = "")
        }
        else if(i < 100){
            file_ex <- paste("0", i, sep = "")
        }
        else{
            file_ex <- i
        }
        filepath <- paste(directory, "/", file_ex, ".csv", sep = "")
        pollutantFile <- read.csv(filepath, header = TRUE)
        ## once the file is open
        goodEntries <- pollutantFile[!is.na(pollutantFile[, pollutant]),] # remove the NAs
        numberEntries <- numberEntries + nrow(goodEntries) #find the number of rows, 
                                                            #add it to the number of rows from previous csv's
        summation <- summation + sum(goodEntries[, pollutant]) #find pollution level sum of column, add to previous csv's
    }
    meanPollution <- summation/numberEntries
    return(meanPollution)
}
