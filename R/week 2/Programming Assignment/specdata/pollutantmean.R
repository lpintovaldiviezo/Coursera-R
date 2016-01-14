pollutantmean <- function(directory, pollutant, id = 1:322){
	for (i in id){
		namefile <- paste(directory, id, sep="/")
		namefile <- paste(namefile, .csv, sep="")
		print (namefile)	
	}
}