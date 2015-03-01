## find out target mortality name
checkTarget <- function(outcome)
{
	val <- ""

	if(outcome == "heart attack") 
	{
                        val <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
	}
      else if(outcome == "heart failure") 
	{
                        val <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"

	}

	else if (outcome == "pneumonia")
	{
		val <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"

	}

	val
}




## "num" validity check
checkRankIdx <- function(num, totalCandidates)
{	
	if (is.numeric(num))
	{
		if (num > totalCandidates)
		{
			## out of bounds
			rankIdx <- 0 	
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
			## typo error
			rankIdx <- -1
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

	rankIdx
}




rankall2 <- function(outcome, num = "best")
{
	filename <- "outcome-of-care-measures.csv"
	myData <- read.csv(filename, colClasses = "character")

	myState <- unique(myData$State)
	myState <- sort(myState)
	
	
	targetMortalityName <- checkTarget(outcome)
	if (targetMortalityName== "")
	{
		stop("Invalid outcome")
	}


	myData[,targetMortalityName] <- as.numeric(myData[, targetMortalityName], na.rm=TRUE)

	## go through each state in myState vector
	
	## initialize data frame
	resultDF <- data.frame()
	resultDF.names <- c("Hospital", "State")

	##resultDF <- matrix(cols=2)


	for (i in seq_along(myState))
	{
		## stateName <- myState[i]
		good <- myData$State == myState[i]

		outData <- myData[good, c("State", targetMortalityName, "Hospital.Name")]

		##Hospitals that have NA on a particular outcome should be excluded
		outData <- na.omit(outData)  ##  In effect the same as: outData[complete.cases(outData), ]
		

		outData <- outData[order(outData[targetMortalityName], outData["Hospital.Name"]), ]

		totalCandidates <- length(outData$Hospital.Name)

		##print(totalCandidates)
		
		

		idx <- checkRankIdx(num, totalCandidates)	
		if (idx < 0)
		{
			stop("invalid num: typo error")
		}
		else if (idx == 0)
		{
			candidate <- "NA"
		}
		else 
		{
			candidate <- outData[idx, "Hospital.Name"]
		}

			
		##print(candidate)
		##print(myState[i])
	
		resultDF <- rbind(resultDF, c(candidate, myState[i]))
	}


	##colnames(resultDF) <- c("Hospital", "State")
	##resultDF
	
}

