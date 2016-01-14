complete <- function(directory, id = 1:332){
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
	## 1 117
	## 2 1041
	## ....
	## where id is the monitor ID number and nobs is the
	## number of complete cases
	countCC <- data.frame("id"=numeric(), "nobs"=numeric())
	pos <- 1
	for (i in id){
		countCC[pos,1] <- i
		i <- sprintf("%03d", as.numeric(i))
		namefile <- paste(directory, i, sep="/")
		namefile <- paste(namefile, ".csv", sep="")
		dataframe <- read.csv(namefile)
		subset <- complete.cases(dataframe) 
		countCC[pos,2] <- length(subset[subset==TRUE])
		pos <- pos + 1
	}
	return(countCC)
}