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
	sacc_stats <- lapply(saccades, FUN=GetSaccadeStatistics)
	sacc_stats <- do.call(rbind.data.frame, sacc_stats)
	sacc_stats$prevFixDur <- durations
	rm(durations)

	# normalise saccades to [-1,0] -> [1,0]
	saccades = lapply(saccades, NormaliseSaccade)

	write_csv(bind_rows(saccades), paste("saccade_samples_dat_", alg, ".csv", sep = ""))
	
	# get curvature statistics
	curve <-map_df(saccades, CurvatureStats)
	sacc_stats <- merge(sacc_stats, curve)	
	rm(curve)

	sacc_stats <- as_tibble(sacc_stats)
	
	# now remove short saccades
	print('')
	print(paste("Removing a further", round(100 * nrow(filter(sacc_stats, r <= 32))/nrow(sacc_stats)), "% of the saccades for being too short (<0.5vd)"))
	sacc_stats <- filter(sacc_stats, r > 16)

	print('')
	print('')
	print('')

	write_csv(sacc_stats, paste("saccade_dat_", alg, ".csv", sep = ""))
	
# }
