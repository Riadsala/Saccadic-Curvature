library(tidyverse)

source("myFunctions.R")


for (alg in c("eyelink", "nystrom")) {
# alg = "eyelink"

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
	saccades =lapply(saccades, NormaliseSaccade)

	# get curvature statistics
	curve <-map_df(saccades, CurvatureStats)
	sacc_stats <- merge(sacc_stats, curve)	
	rm(curve)

	sacc_stats <- as_tibble(sacc_stats)
	
	saccades <- bind_rows(saccades)
	write_csv(saccades, paste("saccade_samples_dat_", alg, ".csv", sep = ""))
	

	# now look at x(t)
	print('')
	ggplot(sacc_stats, aes(x = line_x_R2)) + 
		geom_histogram(bins = 100, fill = "slateblue") +
		theme_minimal() 
	print('')

	line_x_R2_thresh = 0.9
	print(paste(round(100*mean(sacc_stats$line_x_R2 > line_x_R2_thresh),2), "% of saccades have good x fit"))

	# give some examples of the type of eye movement we are removing!

	sacc_stats %>% 
		filter(line_x_R2 < 0.5) %>%
		sample_n(5) %>%
		mutate(key = paste(person, trlNum, saccNum)) -> ss

	saccades %>%
		mutate(key = paste(person, trial, n)) %>%
		filter(key %in% ss$key) %>%
		ggplot(aes(x = x, y = y, colour = key)) + geom_path() + 
			theme_bw() +
			theme(legend.position = "none", panel.grid = element_blank()) +
			xlim(c(1, 1024)) + ylim(c(1, 768)) 
	ggsave(paste("../plots/bad_linear_fits_", alg, ".png", sep = ""))

	ggplot(sacc_stats, aes(x = line_x_R2)) + geom_histogram(bins = 100)
	ggsave(paste("../plots/x_linear_fits_", alg, ".png", sep = ""))
	
	sacc_stats <- filter(sacc_stats, line_x_R2 > 0.9)

	print('')
	print('')

	write_csv(sacc_stats, paste("saccade_dat_", alg, ".csv", sep = ""))
	

 }
