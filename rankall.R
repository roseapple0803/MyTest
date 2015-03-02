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



rankIt <- function(x, num)
{
	##Hospitals that have NA on a particular outcome should be excluded
	outData <- na.omit(x)  
		
	## rank the hospitals, in mortality rate (from small to large) and then in alphabetical order
	##outData <- outData[order(outData[targetMortalityName], outData["Hospital.Name"]), ]
	outData <- outData[order(outData[2], outData["Hospital.Name"]), ]

	
	totalCandidates <- length(outData$Hospital.Name)

	## num validity check
	idx <- checkRankIdx(num, totalCandidates)	
	if (idx < 0)
	{
			stop("invalid num: typo error")
	}
	else if (idx == 0)
	{
			candidate <- NA_character_ ##special character: <NA>
	}
	else 
	{
			candidate <- outData[idx, "Hospital.Name"]
	}

}





doTest <- function(outcome, num = "best")
{
	filename <- "outcome-of-care-measures.csv"
	myData <- read.csv(filename, colClasses = "character")
	
	## get all the states and sort them in alphabetical order
	myState <- unique(myData$State)
	myState <- sort(myState)

	targetMortalityName <- checkTarget(outcome)
	myData[,targetMortalityName] <- as.numeric(myData[, targetMortalityName], na.rm=TRUE)
	
	myData <- myData[ , c("Hospital.Name", targetMortalityName, "State")]


	x <- split(myData, myData$State) ## x is a LIST of 54 members(i.e., States)
	result <- lapply(x, FUN="rankIt", num)

	as.matrix(result)

}




## Ranking hospitals in all states, based on an outcome name (outcome)
## and a ranking numer (num)
##
## It then returns a 2-column data frame containing the hospital in each state 
## that has the ranking specified in num.

rankall <- function(outcome, num = "best")
{
	## read in data as "character" type from outcome-of-care-measures.csv
	filename <- "outcome-of-care-measures.csv"
	myData <- read.csv(filename, colClasses = "character")
	
	## get all the states and sort them in alphabetical order
	myState <- unique(myData$State)
	myState <- sort(myState)
	
	## Check that outcome (user input) is valid
	targetMortalityName <- checkTarget(outcome)
	if (targetMortalityName== "")
	{
		stop("Invalid outcome")
	}

	## convert target mortality rate in numeric format
	myData[,targetMortalityName] <- as.numeric(myData[, targetMortalityName], na.rm=TRUE)


	## resultDF is a data frame with two columns: hospital(character type) and state(character type)
	resultDF <- data.frame(hospital = character(), state=character())
	resultDF <- data.frame() ## it also works!
	

	
	##colnames(resultDF) <- c("Hospital", "State")
	##resultDF <- matrix(nrow=0,ncol=2)
	##colnames(resultDF) <- c("Hospital", "State")


	##print(resultDF)

	## go through each state in myState vector
	for (i in seq_along(myState))
	{
		## stateName <- myState[i]
		good <- myData$State == myState[i]

		outData <- myData[good, c("State", targetMortalityName, "Hospital.Name")]

		##Hospitals that have NA on a particular outcome should be excluded
		outData <- na.omit(outData)  ##  In effect the same as: outData[complete.cases(outData), ]
		
		## rank the hospitals, in mortality rate (from small to large) and then in alphabetical order
		outData <- outData[order(outData[targetMortalityName], outData["Hospital.Name"]), ]

		totalCandidates <- length(outData$Hospital.Name)
	
		## num validity check
		idx <- checkRankIdx(num, totalCandidates)	
		if (idx < 0)
		{
			stop("invalid num: typo error")
		}
		else if (idx == 0)
		{
			candidate <- NA_character_ ##special character: <NA>
		}
		else 
		{
			candidate <- outData[idx, "Hospital.Name"]
		}

			
		## append the current "hospital" and "State" into resultDF
		##resultDF <- rbind(resultDF, c(candidate, myState[i]))
		
		resultDF <- rbind(resultDF, data.frame(hospital=candidate, state=myState[i]))

	}
	rownames(resultDF) <- resultDF$state
	resultDF
	
	
}

