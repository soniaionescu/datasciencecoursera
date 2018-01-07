complete <- function(directory, id = 1:332){
    ## create data file
    myDataFrame <- data.frame(id = numeric(), nobs = numeric())
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
        ## count number of NAs
        numberNAs <- sum(complete.cases(pollutantFile))
        ## add to the data frame
        myDataFrame <- rbind(myDataFrame, data.frame(id = i, nobs = numberNAs))
    }
    return(myDataFrame)
}
