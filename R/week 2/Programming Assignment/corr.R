corr <- function(directory, threshold = 0){
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ##  number of completely observed observations (on all 
    ##  variables) required to compute the correlation between 
    ##  nitrate and sulfate; the default value is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
	all_files <- as.character( list.files(directory))
	file_paths <- paste(directory, all_files, sep="/")
	num_files = length(file_paths)
	corr_vector <-  numeric()
	for (i in 1:num_files){
		dataframe <- read.csv(file_paths[i])
		complete_cases <- (complete(directory, i))$nobs
		if (complete_cases > threshold){
			subset <- dataframe
			V1 <- dataframe[!is.na(dataframe[,"sulfate"]) & !is.na(dataframe[,"nitrate"]), "sulfate"]
			V2 <- dataframe[!is.na(dataframe[,"nitrate"]) & !is.na(dataframe[,"sulfate"]), "nitrate"]
			val_cor <- round((cor(V1,V2)),5)
			corr_vector <- c(corr_vector, val_cor)
		}
	}
	return(corr_vector)
}