find.best <- function(data, outcome , state){
subset.state <- data[data[,7]==state,]
subset.outcome <- subset.state[,outcome]
min <- min(subset.outcome, na.rm=TRUE)
min_index <- which(subset.outcome == min)
best.hospital <- subset.state[min_index, 2]
return (best.hospital)
}


best <- function(state, outcome) {
## Read outcome data
data.outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
## Check that state and outcome are valid
valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
if (!state %in% data.outcome$State) {
	stop("invalid state")
} else if(!outcome %in% valid_outcomes) {
	stop("invalid outcome")
} else {
	if(outcome== "heart attack"){
		data.outcome[,11] <- as.numeric(data.outcome[,11])
		best.hospital <- find.best(data.outcome,11,state)
	}else if (outcome== "heart failure"){
		data.outcome[,17] <- as.numeric(data.outcome[,17])
		best.hospital <- find.best(data.outcome,17,state)
	}else{
		data.outcome[,23] <- as.numeric(data.outcome[,23])
		best.hospital <- find.best(data.outcome,23,state)
	}
## Return hospital name in that state with lowest 30-day death
return (sort(best.hospital)[1])	
}
}
