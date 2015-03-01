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




## Ranking hospitals by outcome in a state
rankhospital <- function(state, outcome, num = "best")
{
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


	## coerce the data on "outcome" column to be numeric type	
	myData[,targetName] <- as.numeric(myData[, targetName], na.rm=TRUE)

	## Return hospital name in that state with lowest 30-day death
	good <- myData$State == state
	outData <- myData[good, c("State", targetName, "Hospital.Name")]

	## Hospitals that have NA on a particular outcome should be excluded
	outData <- na.omit(outData)  ##  In effect the same as: outData[complete.cases(outData), ]
		
	## rank the hospitals and also sort them in alphabetical order
	outData <- outData[order(outData[targetName], outData["Hospital.Name"]), ]


	outData[ ,c("State", "Hospital.Name", targetName)]	 


############################################################################
############################################################################

	## total number of hospitals
	totalCandidates <- length(outData[ ,"Hospital.Name"])

	## "num" validity check
	if (is.numeric(num))
	{
		if (num > totalCandidates)
		{
			print("num is larger than the number of hospitals in that state")
			stop("NA")		
		}
		else
		{
			rankIdx <- num
		}
	}
	else
	{
		numlist <- c("best", "worst")
		if (!any(numlist == num))
		{
			stop("invalid num")
		}
		

		if (num == "best")
		{
			rankIdx <- 1
		}
		else if (num == "worst")
		{
			rankIdx <- totalCandidates
		}
	}

	## find hospital name with the given rank
	outData[rankIdx, "Hospital.Name"]

}







