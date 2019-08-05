library(tidyverse)


for (alg in c("eyelink", "nystrom")) 
{
	dat <- read_csv(paste("saccade_dat_", alg, ".csv", sep = ""))

	saccades <-	read_csv(paste("saccade_samples_dat_", alg, ".csv", sep = ""))

	# first, we will look at how linear y is 
	ggplot(dat, aes(x = line_y_R2)) + 
		geom_histogram(fill = "slateblue", bins = 50) +
		theme_minimal()
	ggsave(paste("../plots/y_line_R2_", alg, ".pdf", sep = ""))

	# now look at how good the quadratic fits are

	ggplot(dat, aes(x = quad_R2)) + 
		geom_histogram(fill = "slateblue", bins = 50) +
		theme_minimal() + xlim(c(0, 1))
	ggsave(paste("../plots/quad_R2_", alg, ".pdf", sep = ""))


	# give examples of poor quadratic fits
	dat %>% 
		filter(quad_R2 < 0.5) %>%
		sample_n(10) %>%
		mutate(key = paste(person, trlNum, saccNum)) -> ss

	saccades %>%
		mutate(key = paste(person, trial, n)) %>%
		filter(key %in% ss$key) %>%
		ggplot(aes(x = x, y = y, colour = key)) + geom_path() + 
			theme_bw() +
			theme(legend.position = "none", panel.grid = element_blank()) +
			xlim(c(1, 1024)) + ylim(c(1, 768)) 
	ggsave(paste("../plots/bad_quadratic_fits_", alg, ".png", sep = ""))


	saccades %>%
		mutate(key = paste(person, trial, n)) %>%
		filter(key %in% ss$key) %>%
		ggplot(aes(x = xn, y = yn, colour = key)) + geom_path() + 
			theme_bw() +
			theme(legend.position = "none", panel.grid = element_blank()) 
	ggsave(paste("../plots/bad_quadratic_fits_n_", alg, ".png", sep = ""))

rm(dat)
}
# how do the various curvature measures compare?

# ggplot(dat, aes(x = quad_curvature, y = max_curvature)) +
# 	geom_point() + geom_smooth(method = "lm") 
# ggsave("../plots/quad_max_cor.png")

# ggplot(dat, aes(x = quad_curvature, y = area_curvature)) +
# 	geom_point() + geom_smooth(method = "lm") 
# ggsave("../plots/quad_area_cor.png")

# ggplot(dat, aes(x = r, y = area_curvature)) +
# # 	geom_point() + geom_smooth(method = "lm") 

# ggplot(dat, aes( x = cube_R2 / quad_R2)) + geom_histogram(bins = 50) + 
# 	scale_x_continuous(trans = scales::log10_trans())

# ggplot(filter(dat, quad_R2 > 0.8), aes(x = log(r), y = log(quad_curvature))) +
#  	geom_point() + geom_smooth(method = "lm") 
# ggsave("../plots/quad_curve_amp.png", width = 4, height = 3)

# cor.test(log(dat$quad_curvature) ,log(dat$r))

# aggregate(data = dat, quad_R2 ~ person, FUN = median)

# ggplot(dat, aes(x = as.factor(person), y = quad_R2)) + geom_bar(stat="summary", fun.y = median)



# # plot a few random examples of bad quartic but good cubic

# saccades <-	read_csv(paste("saccade_samples_dat_eyelink.csv", sep = ""))

# dat %>% 
# 	filter(line_R2 >.5, quad_R2 > 0.9) %>%
# 	sample_n(5) %>%
# 	mutate(key = paste(person, trlNum, saccNum)) -> ss



# saccades %>%
# 	mutate(key = paste(person, trial, n)) %>%
# 	filter(key %in% ss$key) %>%
# 	ggplot(aes(x = x, y = y, colour = key)) + geom_path()
# ggsave("../plots/good_linear_fits.png")


# saccades %>%
# 	mutate(key = paste(Person, TrialIndex, SaccNumber)) %>%
# 	filter(key %in% ss$key) %>%
# 	ggplot(aes(x = x_n, y = y_n, colour = key)) + geom_path()
# ggsave("../plots/good_linear_fits_n.png")

# # good linear fits