library(ggplot2)
library(dplyr)
library(rethinking)
source("myFunctions.R")

alg <- 'nystrom'

# read in data
files <- dir("../data/")

saccades  <- vector('list', length(files))
durations <- vector('list', length(files))

ctr <- 0
for (fl in files)
{
	ctr <- ctr + 1
	dat <- read.csv(paste("../data/", fl, sep=""), sep="\t")
	# turn dataframe into list!
	saccades[[ctr]]  <- SaccDFtoList(dat, alg, fl)
	# get fixation duration
	durations[[ctr]] <- GetFixDur(dat, alg)

	rm(dat)
}

# flatten the list-of-lists into a list
saccades  <- unlist(saccades, recursive=FALSE)
durations <- unlist(durations)

output <- CleanData(saccades, duratkions)
saccades <- output[[1]]
durations <- output[[2]]
rm(output)

# get basic saccade statistics
saccStats <- lapply(saccades, FUN=GetSaccadeStatistics)
saccStats <- do.call(rbind.data.frame, saccStats)
saccStats$prevFixDur <- durations
rm(durations)

# normalise saccades to [-1,0] -> [1,0]
saccadesNormalised = lapply(saccades, NormaliseSaccade)
# names(saccadesNormalised) <- 1:length(saccadesNormalised)
saccDF <- do.call(rbind.data.frame, saccadesNormalised)
rm(saccades)
names(saccDF) = c("person", "trlNum", "sampleTime", "x", "y", "saccNum")

#get curvature statistics
curve <-lapply(saccadesNormalised, FUN=CurvatureStats)
curve <- do.call(rbind.data.frame, curve)
saccStats <- merge(saccStats, curve)
rm(saccadesNormalised, curve)
	
plt <- ggplot(filter(saccDF, person==1), aes(x=x, y=y)) + geom_point()
plt <- plt + geom_text(data=filter(saccStats, person==1), aes(x=0,y=0, label=round(quad_R2,2)))
plt <- plt + geom_smooth(method='lm', formula=y~x+I(x^2), se=FALSE, colour="red")
plt <- plt + geom_smooth(method='lm', formula=y~x+I(x^2)+I(x^3), se=FALSE, colour="orange")
plt <- plt + geom_smooth(method='lm', formula=y~x+I(x^2)+I(x^3)+I(x^4), se=FALSE, colour="purple")
plt <- plt + facet_wrap(~paste(trlNum,saccNum), scale="free")
ggsave(paste("../plots/sacTrace_", alg, ".pdf", sep=""),width=30, height=30)

saccStats$r.log <- log(saccStats$r,2)
saccStats$c.log <- log(saccStats$quad_curvature,2)
m <- map(
	alist(
		c.log ~ dnorm(mu, sigma) ,
		mu <- intrcpt + beta * r.log,
		intrcpt ~ dnorm(0,1),
		beta ~ dnorm(0,1),
		sigma ~ dunif(0,10)
		),
	data=filter(saccStats, quad_R2>0.8))

plot(c.log ~ r.log, 
	data=filter(saccStats, quad_R2>0.8), col='red')

r.seq = log(seq(1,1000, by=10),2)
mu <- link(m, data=data.frame(r.log=r.seq))
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob=0.89)
lines(r.seq, mu.mean)
shade(mu.HPDI, r.seq)


curve.sim <- sim(m, data=data.frame(r.log=r.seq), n=1e4)
curve.PI  <- apply(curve.sim, 2, PI, prob=0.89)
shade(curve.PI, r.seq)

plt <- ggplot(filter(saccStats, quad_R2>0.80), aes(x=log(r,2), y=log(max_curvature,2)))
plt <- plt + geom_point()
plt <- plt + geom_smooth(method=lm)
ggsave(paste("../plots/r_curvature_", alg, ".pdf", sep=""),width=4, height=4)
