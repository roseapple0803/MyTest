	
checkTarget <- function(outcome)
{
	
	val <- 0

	if(outcome == "heart attack") 
	{
      	val <- 1
	}
      else if(outcome == "heart failure") 
	{
            val <- 2
	}

	else if (outcome == "pneumonia")
	{
		val <- 3
	}

	val
}



##  Finding the best hospital in a state
best <- function(state, outcome)
{
	## read in data as "character" type from outcome-of-care-measures.csv
	filename <- "outcome-of-care-measures.csv"
	myData <- read.csv(filename, colClasses = "character")

	## Check that state (user input) is valid
	myState <- unique(myData$State)
	if (!any(myState == state))
	{
		## typo error
		stop("invalid state")
	}

	## Check that outcome (user input) is valid
	targetlist <- c("heart attack", "heart failure", "pneumonia")
	if (!any(targetlist == outcome))
	{
		stop("invalid outcome")
	}

	##  find the target mortality rate from outcome
	stopVal <- checkTarget(outcome)
	if (stopVal == 1)
	{
		targetName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"

	}

	else if (stopVal == 2)
	{

		targetName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
		
	}

	else if (stopVal == 3)
	{

		targetName <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
	}

	good <- myData$State == state
	
	## coerce the data on "outcome" column to be numeric type
	myData[,targetName] <- as.numeric(myData[, targetName], na.rm=TRUE)

	## Return hospital name in that state with lowest 30-day death
	outData <- myData[good, c("State", targetName, "Hospital.Name")]	
	outData <- na.omit(outData)  ##  In effect the same as: outData[complete.cases(outData), ]

	## find the hospital with the lowest rate		
	bestNum <- min(outData[targetName])		
	criteria <- outData[targetName] == bestNum
	bestHospital <- outData[criteria, "Hospital.Name"]
	
	## Handling ties: sort the hospital names in alphabetical order
	sort(bestHospital)
	bestHospital[1]
}




