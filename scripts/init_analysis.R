library(ggplot2)

library(dplyr)

source("myFunctions.R")


dat <- read.csv("../data/l19a_t1_samples.txt", sep="\t")

# turn dataframe into list!
saccades <- SaccDFtoList(dat, 'eyelink')

saccStats <- sapply(saccades, GetAmpAndDir)

saccadesNormalised = lapply(saccades, NormaliseSaccade)
names(saccadesNormalised) <- 1:length(saccadesNormalised)



sacDF <- do.call(rbind.data.frame, saccadesNormalised)
plt <- ggplot(sacDF, aes(x=X, y=Y)) + geom_point()
plt <- plt + facet_wrap(~saccNumber, scale="free")
ggsave("../plots/sacTrace_Eyelink.pdf")




curve <-sapply(saccadesNormalised, CurvatureStats, simplify='array')


plot(saccStats[1,], curve[1,])

