SaccDFtoList <- function(saccDF, alg='eyelink')
{
	# take samples and split into a list of saccades

	if (alg=='eyelink'){
		sacBoundaries  <- which(saccDF$SacFlagEyeLink[1:(nrow(saccDF)-1)]!=saccDF$SacFlagEyeLink[2:nrow(saccDF)])
	}

	saccDF <- select(saccDF, SampleTime, X, Y)
	sacList <- vector('list', length(sacBoundaries)/2)

	saccDF$saccNumber <- 0
	saccCtr <- 0
	for (ii in seq(1, length(sacBoundaries), 2))
	{
		saccCtr <- saccCtr + 1
		idx = (sacBoundaries[ii]+1):sacBoundaries[ii+1]
		saccDF$saccNumber[idx] = saccCtr
		sacList[[saccCtr]] <- rbind(saccDF[(sacBoundaries[ii]+1):sacBoundaries[ii+1],])
		row.names(sacList[[saccCtr]]) = NULL
	}
	names(sacList) <- 1:length(sacList)
	return(sacList)
}

GetAmpAndDir <- function(saccade)
{
	x1 <- saccade$X[1]
	x2 <- saccade$X[nrow(saccade)]

	y1 <- saccade$Y[1]
	y2 <- saccade$Y[nrow(saccade)]

	r = sqrt((x2-x1)^2+(y2-y1)^2)

	theta = atan2(y2-y1, x2-x1)

	return(list(r=r, theta=theta))


}

NormaliseSaccade <- function(saccade)
{
	# first set saccade start point at (0, 0)
	x <- saccade$X - saccade$X[1]
	y <- saccade$ Y-saccade$Y[1]
	

	# now rotate so endpoint lands on (x, 0)
	theta = atan2(y[nrow(saccade)], x[nrow(saccade)])
	xr <- cos(-theta) * x - sin(-theta) * y
	yr <- sin(-theta) * x + cos(-theta) * y

	# now normalise so endpoint lands on (1, 0)
	xn <- xr/xr[length(xr)]
	yn <- yr/xr[length(xr)]

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
	quad_c <- abs(qfit$coefficients[3])


	return(list(
		max_curvature=max_c, 
		area_curvature=area_c,
		quad_curvature=quad_c)) 
}