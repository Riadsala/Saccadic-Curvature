library(tidyverse)

source("myFunctions.R")


# for (alg in c("eyelink", "nystrom")) {
alg = "eyelink"

	print('***************************************************************')
	print('***************************************************************')
	print(paste("Running for", alg, "data"))
	print('***************************************************************')
	print('***************************************************************')

	# read in data
	files <- dir("../data/", pattern = ".txt")

	saccades  <- vector('list', length(files))
	durations <- vector('list', length(files))
	dat <- tibble()

	ctr <- 0
	for (fl in files)
	{
		ctr <- ctr + 1
		dat <- read_delim(paste("../data/", fl, sep=""), 
			delim="\t", 
			col_types = cols(
			  TrialIndex = col_integer(),
			  SampleTime = col_integer(),
			  X = col_double(),
			  Y = col_double(),
			  SacFlagEyeLink = col_integer(),
			  SacFlagNystrom = col_integer()))

		# turn dataframe into list!
		saccades[[ctr]]  <- SaccDFtoList(dat, alg, fl)
		# get fixation duration
		durations[[ctr]] <- GetFixDur(dat, alg)
		rm(dat)
	}

	# flatten the list-of-lists into a list
	saccades  <- unlist(saccades, recursive=FALSE)
	durations <- unlist(durations)

	# clean data
	output <- CleanData(saccades, durations)
	saccades <- output[[1]]
	durations <- output[[2]]
	rm(output)

	# get basic saccade statistics
	saccStats <- lapply(saccades, FUN=GetSaccadeStatistics)
	saccStats <- do.call(rbind.data.frame, saccStats)
	saccStats$prevFixDur <- durations
	rm(durations)

	# normalise saccades to [-1,0] -> [1,0]
	saccades = lapply(saccades, NormaliseSaccade)

	write_csv(bind_rows(saccades), paste("saccade_samples_dat_", alg, ".csv", sep = ""))
	
	# get curvature statistics
	curve <-map_df(saccades, CurvatureStats)
	saccStats <- merge(saccStats, curve)	
	rm(curve)

	saccStats <- as_tibble(saccStats)
	
	# now remove short saccades
	print('')
	print(paste("Removing a further", round(100 * nrow(filter(saccStats, r <= 32))/nrow(saccStats)), "% of the saccades for being too short (<1vd)"))
	saccStats <- filter(saccStats, r > 32)

	# check range of amplitude
	ggplot(saccStats, aes(x = r)) + geom_histogram(bins = 200)
	ggsave(paste("../plots/sacc_amp_", alg, ".pdf", sep = ""), width = 8, height = 3)


	ggplot(saccStats, aes(x = quad_R2)) + geom_histogram(bins = 50)
	ggsave(paste("../plots/quad_R2_", alg, ".pdf", sep = ""), width = 4, height = 3)

	ggplot(saccStats, aes(x = line_R2)) + geom_histogram(bins = 50)
	ggsave(paste("../plots/line_R2_", alg, ".pdf", sep = ""), width = 4, height = 3)


	ggplot(saccStats, aes(x = log(prevFixDur))) + geom_histogram(bins = 50)
	ggsave(paste("../plots/prev_fix_dir_R2_", alg, ".pdf", sep = ""), width = 4, height = 3)

	print('')

	print(paste(round(100 * mean(saccStats$quad_R2 > 0.5)), "% of saccades have quadratic R2 > 0.5", sep = ""))
	print('')
	print('')
	print('')

	write_csv(saccStats, paste("saccade_dat_", alg, ".csv", sep = ""))
	
# }
