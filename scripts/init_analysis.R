library(ggplot2)
library(dplyr)

source("myFunctions.R")


alg = 'eyelink'

trials = 1:10


saccades  <- vector('list', length(trials))
durations <- vector('list', length(trials))

for (trl in trials)
{
	# read in trial data
	dat <- read.csv(paste("../data/l19a_t", trl, "_samples.txt", sep=""), sep="\t")
	# turn dataframe into list!
	saccades[[trl]]  <- SaccDFtoList(dat, trl, alg)
	# get fixation duration gvzr
	durations[[trl]] <- GetFixDur(dat)
}

# flatten the list-of-lists into a list
saccades  <- unlist(saccades, recursive=FALSE)
durations <- unlist(durations)

# remove saccades for which X was outside of image
idx <- sapply(saccades, function(sacc){max((abs(sacc$X)>1024)|(abs(sacc$Y)>768))})
saccades  <- saccades[-which(idx==1)]
durations <- durations[-which(idx==1)]

# get basic saccade statistics
saccStats <- sapply(saccades, GetSaccadeStatistics)
saccStats <- as.data.frame(t(saccStats))
saccStats$fixDur = durations

# normalise saccades 
saccadesNormalised = lapply(saccades, NormaliseSaccade)
names(saccadesNormalised) <- 1:length(saccadesNormalised)
sacDF <- do.call(rbind.data.frame, saccadesNormalised)

#get curvature statistics
curve <-sapply(saccadesNormalised, FUN=CurvatureStats, USE.NAMES = FALSE)

curveDF <- cbind(as.data.frame((t(curve))), saccStats)

for (cc in 1:length(curveDF))
{
	curveDF[,cc] <- as.numeric(curveDF[,cc])
}

plt <- ggplot(sacDF, aes(x=X, y=Y)) + geom_point()
plt <- plt + geom_text(data=curveDF, aes(x=0,y=0, label=round(curveDF$quad_R2,2)))
plt <- plt + geom_smooth(method='lm', formula=y~x+I(x^2), se=FALSE)
plt <- plt + facet_wrap(trlNumber~saccNumber, scale="free")
ggsave(paste("../plots/sacTrace_", alg, ".pdf", sep=""),width=20, height=20)


plt <- ggplot(filter(curveDF, quad_R2>0.8), aes(x=fixDur, y=quad_curvature))
plt <- plt + geom_point()
plt <- plt + geom_smooth(method=lm)
plt