library(tidyverse)

min_num_samples <- 20 

SaccDFtoList <- function(saccDF, alg, fl)
{
	# first get observer number
	m <- regexpr('[lr][0-9]+', fl)
	saccDF$person <- regmatches(fl, m)
	rm(m)

	# take samples and split into a list of saccades
	if (alg=='eyelink'){
		sacBoundaries  <- which(saccDF$SacFlagEyeLink[1:(nrow(saccDF)-1)]!=saccDF$SacFlagEyeLink[2:nrow(saccDF)])
	} else {
		sacBoundaries  <- which(saccDF$SacFlagNystrom[1:(nrow(saccDF)-1)]!=saccDF$SacFlagNystrom[2:nrow(saccDF)])
	}

	# if trial ends during a saccade, remove that saccade
	if (length(sacBoundaries) %% 2)
	{
		sacBoundaries <- sacBoundaries[-length(sacBoundaries)]
	}

	# now get extract saccade
	saccDF <- select(saccDF, person, TrialIndex, SampleTime, X, Y)
	sacList <- vector('list', length(sacBoundaries)/2)

	saccDF$SaccNumber <- 0

	saccCtr <- 0 # overall saccade coutner
	saccN   <- 0 # counts saccades per trial
	currentTrl <- 0;

	for (ii in seq(1, length(sacBoundaries), 2))
	{
		saccCtr <- saccCtr + 1
		idx <- (sacBoundaries[ii]+1):sacBoundaries[ii+1]

		# check to see if we're moving onto a new trial!
		if (saccDF$TrialIndex[idx[1]] != currentTrl)
		{
			# new trial! update currentTrl and reset saccN
			currentTrl <- saccDF$TrialIndex[idx[1]]	
			saccN <- 0
		}
		saccN <- saccN + 1

		saccDF$SaccNumber[idx] <- saccN
		sacList[[saccCtr]] <- rbind(saccDF[(sacBoundaries[ii]+1):sacBoundaries[ii+1],])
		row.names(sacList[[saccCtr]]) = NULL
		sacList[[saccCtr]]$SampleTime <-  sacList[[saccCtr]]$SampleTime - sacList[[saccCtr]]$SampleTime[1] 
	}

	# names(sacList) <- paste("trl", trl, "-sacc", 1:length(sacList), sep="")
	return(sacList)
}


GetFixDur <- function(saccDF, alg)
{
	# take samples and split into a list of saccades
	if (alg=='eyelink'){
		sacBoundaries  <- which(saccDF$SacFlagEyeLink[1:(nrow(saccDF)-1)]!=saccDF$SacFlagEyeLink[2:nrow(saccDF)])
	} else {
		sacBoundaries  <- which(saccDF$SacFlagNystrom[1:(nrow(saccDF)-1)]!=saccDF$SacFlagNystrom[2:nrow(saccDF)])
	}

	# if trial ends during a saccade, remove that saccade
	if (length(sacBoundaries) %% 2)
	{
		sacBoundaries <- sacBoundaries[-length(sacBoundaries)]
	}

	# first, get preceding fixation duration
	fixDur <- vector('numeric', length(sacBoundaries)/2)
	fixCtr <- 1
	for (ii in seq(1, length(sacBoundaries)-2, 2))
	{
		fixCtr <- fixCtr + 1
		idx <- (sacBoundaries[ii+1]+1):sacBoundaries[ii+2]

		fixDur[fixCtr] <- saccDF$SampleTime[idx[length(idx)]] - saccDF$SampleTime[idx[1]]
	}

	return(fixDur)
}

CleanData <- function(saccades, duratations)
{
	print(paste("We have ", length(saccades), " saccades before cleaning"))
	before_clean <- length(saccades)
	print('***************************************************************')

	# remove 1st saccades
	idx <- which(sapply(saccades, function(x){unique(x$SaccNumber)})==1)
	saccades <- saccades[-idx]
	durations <- durations[-idx]
	print(paste("removed", length(idx), "first saccades"))

	# remove saccades for which X or Y was outside of image
	idx <- sapply(saccades, 
		function(sacc){max((sacc$X>1024)|(sacc$Y>768)|(sacc$X<0)|(sacc$Y<0))})
	idx <- which(idx==1)
	saccades  <- saccades[-idx]
	durations <- durations[-idx]
	print(paste("removed", length(idx), "saccades for falling outside of image boundary"))

	# remove saccades that take place after a really long fixation duration > 2secs
	idx <- which(durations>2000)
	saccades <- saccades[-idx]
	durations <- durations[-idx]
	print(paste("removed", length(idx), "saccades with preceding fixation duration >2000ms"))

	# remove saccades that take place after a really short fixation!
	idx <- which(durations<50)
	saccades <- saccades[-idx]
	durations <- durations[-idx]
	print(paste("removed", length(idx), "saccades with preceding fixation duration <50ms"))

	# remove saccades with less than 5 samples
	idx <- sapply(saccades, function(x){nrow(x)<min_num_samples})
	idx <- which(idx==1)
	if (length(idx)>0){
		saccades <- saccades[-idx]
		durations <- durations[-idx]
	}
	print(paste("removed", length(idx), "saccades with less than", min_num_samples, "samples"))
	print('***************************************************************')
	print(paste("We have ", length(saccades), " saccades after cleaning"))
	after_clean <- length(saccades)
	print(paste("We removed ", round(100 - 100*after_clean/before_clean), "% of saccades"))
	return(list(saccades, durations))

}

GetSaccadeStatistics <- function(saccade)
{
	x1 <- saccade$X[1]
	x2 <- saccade$X[nrow(saccade)]

	y1 <- saccade$Y[1]
	y2 <- saccade$Y[nrow(saccade)]

	t1 <- saccade$SampleTime[1]
	t2 <- saccade$SampleTime[nrow(saccade)]

	r = sqrt((x2-x1)^2+(y2-y1)^2)

	theta = atan2(y2-y1, x2-x1)

	nSamples = nrow(saccade)
	saccDur = t2 - t1
	return(tibble(
		person = saccade$person[1],
		trlNum = saccade$TrialIndex[1],
		saccNum = saccade$SaccNumber[1],
		x1 = x1 - 1024/2, 
		y1 = y1 - 768/2, 
		x2 = x2 - 1024/2,
		y2 = y2 - 768/2,  
		dc12 = x1^2 + y1^2,
		dc22 = x2^2 + y2^2,
		r = r, 
		theta = 	theta, 
		nSample = nSamples, 
		saccDur = saccDur))
}

NormaliseSaccade <- function(saccade)
{
	# first set saccade start point at (0, 0)
	x <- saccade$X - saccade$X[1]
	y <- saccade$Y - saccade$Y[1]

	# now rotate so endpoint lands on (x, 0)
	theta = atan2(y[nrow(saccade)], x[nrow(saccade)])
	xr <- cos(-theta) * x - sin(-theta) * y
	yr <- sin(-theta) * x + cos(-theta) * y

	# now normalise so endpoint lands on (1, 0)
	xn <- 2 * (xr/xr[length(xr)]) - 1
	yn <- yr/xr[length(xr)]

	# now reflect is -ve curvature
	if (abs(min(yn))>max(yn))
	{
		yn <- -yn
	}

	# output!!
	saccade %>% 
		rename(
			trial = "TrialIndex", 
			n = "SaccNumber",
			x = "X", y = "Y",
			t = "SampleTime") %>%
		mutate(
			xn = xn, yn = yn, tn = t/max(t)) %>%
		select(person, trial, n, t, tn, x, y, xn, yn) -> saccade
	# saccade$t <- saccade$SampleTime - saccade$SampleTime[1]
	return(saccade)
}

CurvatureStats <- function(saccade)
{
	
	max_c <- max(abs(saccade$yn))
	area_c <- sum(abs(saccade$yn))

	# linear parameterised by time
	lfit_x    <- lm(xn ~ tn, saccade)
	linear_x_R2 <- as.numeric(summary(lfit_x)$r.squared)

	lfit_y    <- lm(yn ~ tn, saccade)
	linear_y_R2 <- as.numeric(summary(lfit_y)$r.squared)

	# quadratic
	qfit   <- lm(yn~tn+I(tn^2), saccade)
	quad_c <- as.numeric(abs(qfit$coefficients[3]))
	quad_R2<- as.numeric(summary(qfit)$r.squared)

	# cubic
	cfit    <- lm(y~tn+I(tn^2)+I(tn^3), saccade)
	cubic_R2 <- as.numeric(summary(cfit)$r.squared)

	return(tibble(
		person=saccade$person[1],
		trlNum=saccade$trial[1],
		saccNum=saccade$n[1],
		max_curvature=max_c, 
		area_curvature=area_c,
		quad_curvature=quad_c,		
		line_x_R2 = linear_x_R2,
		line_y_R2 = linear_y_R2,		
		quad_R2 = quad_R2,
		cube_R2 = cubic_R2
		)) 
}
