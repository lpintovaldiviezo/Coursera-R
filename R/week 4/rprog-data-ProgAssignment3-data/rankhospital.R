find.rank <- function(data, outcome , state ,rank.pos){
subset.state <- data[data[,7]==state,]
subset.outcome <- subset.state[,outcome]
len <- dim(subset.state[!is.na(subset.outcome), ])[1]
if (rank.pos == "best"){
hospital.name = subset.state[,2][order(subset.outcome, subset.state[,2])[1]]
}else if (rank.pos == "worst"){
hospital.name = subset.state[,2][order(subset.outcome, subset.state[,2])[len]]
}else if(rank.pos > len){
hospital.name = "NA"
}else{
hospital.name = subset.state[,2][order(subset.outcome, subset.state[,2])[rank.pos]]
}
return (hospital.name)
}

rankhospital <- function(state, outcome, num = "best") {
## Read outcome data
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
		hospital.name <- find.rank(data.outcome,11,state, num)
	}else if (outcome== "heart failure"){
		data.outcome[,17] <- as.numeric(data.outcome[,17])
		hospital.name <- find.rank(data.outcome,17,state, num)
	}else{
		data.outcome[,23] <- as.numeric(data.outcome[,23])
		hospital.name <- find.rank(data.outcome,23,state, num)
	}
## Return hospital name in that state with the given rank
## 30-day death rate
return (hospital.name)
}	
}