pollutantmean <- function(directory, pollutant, id = 1:332){
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
	
	mean_vector <- c()
	for (i in id){
		i <- sprintf("%03d", as.numeric(i))
		namefile <- paste(directory, i, sep="/")
		namefile <- paste(namefile, ".csv", sep="")
		dataframe <- read.csv(namefile, header=T, sep=",")
		subset <- dataframe[!is.na(dataframe[, pollutant]), pollutant]
 		mean_vector <- c(mean_vector, subset)
	}
	result <- mean(mean_vector)	
    return(round(result, 3)) 
}