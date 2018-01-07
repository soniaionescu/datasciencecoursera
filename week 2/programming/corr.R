corr <- function(directory, threshold = 0){
    myDataFrame <- data.frame(sulfate = numeric(), nitrate = numeric())
    cor_vector <- numeric()
    for(i in 1: 332){
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
        ## find files with greater than threshold
        if(complete(directory, i)[, "nobs"] >= threshold){
        ## add to dataframe of things above threshold
            goodEntries <- pollutantFile[complete.cases(pollutantFile), ]
            myDataFrame <- data.frame(sulfate = goodEntries$sulfate, nitrate = goodEntries$nitrate)
        
        ## find correlation between sulfate and nitrate
        correlation <- cor(myDataFrame$nitrate, myDataFrame$sulfate)
        cor_vector <- c(cor_vector, correlation)
        }
    }
    return(cor_vector)
}
