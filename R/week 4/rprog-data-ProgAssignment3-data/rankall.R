find.best <- function(data, outcome , state ,rank.pos){
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


rankall <- function(outcome, num = "best") {
## Read outcome data
data.outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
## Check that state and outcome are valid
valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
## Get the list of states
data.state <- sort(unique(data.outcome[,7]),decreasing = FALSE)
df <- data.frame(hospital=character(),
                 state=character(),  
                 stringsAsFactors=FALSE)
data.pos <- 1
if(!outcome %in% valid_outcomes) {
	stop("invalid outcome")
} else {
## For each state, find the hospital of the given rank
	for (state in data.state){
		df[data.pos,2]=state
		if (outcome == "heart attack"){
			data.outcome[,11] = as.numeric(data.outcome[,11])
			df[data.pos,1]=find.best(data.outcome,11,state,num)
		}else if(outcome == "heart failure"){
			data.outcome[,17] = as.numeric(data.outcome[,17])
			df[data.pos,1]=find.best(data.outcome,17,state,num)
		}else{
			data.outcome[,23] = as.numeric(data.outcome[,23])
			df[data.pos,1]=find.best(data.outcome,23,state,num)
		}
		data.pos <- data.pos + 1
	}
return (df)
}

## Return a data frame with the hospital names and the
## (abbreviated) state name
}