SaccDFtoList <- function(saccDF, trl, alg='eyelink')
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

	# now get extract saccade
	saccDF <- select(saccDF, SampleTime, X, Y)
	sacList <- vector('list', length(sacBoundaries)/2)

	saccDF$saccNumber <- 0
	saccDF$trlNumber  <- 0
	saccCtr <- 0

	for (ii in seq(1, length(sacBoundaries), 2))
	{
		saccCtr <- saccCtr + 1
		idx <- (sacBoundaries[ii]+1):sacBoundaries[ii+1]
		saccDF$saccNumber[idx] <- saccCtr
		saccDF$trlNumber[idx]  <- trl
		sacList[[saccCtr]] <- rbind(saccDF[(sacBoundaries[ii]+1):sacBoundaries[ii+1],])
		row.names(sacList[[saccCtr]]) = NULL
	}

	names(sacList) <- paste("trl", trl, "-sacc", 1:length(sacList), sep="")
	return(sacList)
}

GetFixDur <- function(saccDF, trl, alg='eyelink')
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

	# first, get preceeding fixation duration
	fixDur <- vector('numeric', length(sacBoundaries)/2)
	fixCtr <- 1
	fixDur[1] = NaN # first fixation duration is unreliable
	for (ii in seq(1, length(sacBoundaries)-2, 2))
	{
		fixCtr <- fixCtr + 1
		idx <- (sacBoundaries[ii+1]+1):sacBoundaries[ii+2]

		fixDur[fixCtr] <- saccDF$SampleTime[idx[length(idx)]] - saccDF$SampleTime[idx[1]]
	}

	return(fixDur)
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
	return(list(x1=x1, y1=y1, r=r, theta=theta, nSample=nSamples, saccDur=saccDur))
}

NormaliseSaccade <- function(saccade)
{
	# first set saccade start point at (0, 0)
	x <- saccade$X - saccade$X[1]
	y <- saccade$Y -saccade$Y[1]

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
	saccade[,2:3] = cbind(xn, yn)
	return(saccade)
}



CurvatureStats <- function(saccade)
{
	max_c <- max(abs(saccade$Y))
	area_c <- sum(abs(saccade$Y))

	# quadratic
	qfit   <- lm(Y~X+I(X^2), saccade)
	quad_c <- as.numeric(abs(qfit$coefficients[3]))
	quad_R2<- as.numeric(summary(qfit)$r.squared)

	return(data.frame(
		trlNumber=saccade$trlNumber[1],
		saccNumber=saccade$saccNumber[1],
		max_curvature=max_c, 
		area_curvature=area_c,
		quad_curvature=quad_c,
		quad_R2 = quad_R2
		)) 
}