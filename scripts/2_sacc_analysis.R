library(tidyverse)


alg = "eyelink"
dat <- read_csv("saccade_dat_eyelink.csv")


ggplot(dat, aes(x = line_R2)) + geom_histogram(bins = 50) +
	ggtitle(paste("median R2 =", round(median(dat$line_R2), 3)))
ggsave("../plots/linear_R2_hist.png", width = 4, height = 3)
ggplot(dat, aes(x = quad_R2)) + geom_histogram(bins = 50) +
	ggtitle(paste("median R2 =", round(median(dat$quad_R2), 3)))
ggsave("../plots/quadratic_R2_hist.png", width = 4, height = 3)
ggplot(dat, aes(x = cube_R2)) + geom_histogram(bins = 50) +
	ggtitle(paste("median R2 =", round(median(dat$cube_R2), 3)))
ggsave("../plots/cubic_R2_hist.png", width = 4, height = 3)


# how do the various curvature measures compare?

# ggplot(dat, aes(x = quad_curvature, y = max_curvature)) +
# 	geom_point() + geom_smooth(method = "lm") 
# ggsave("../plots/quad_max_cor.png")

# ggplot(dat, aes(x = quad_curvature, y = area_curvature)) +
# 	geom_point() + geom_smooth(method = "lm") 
# ggsave("../plots/quad_area_cor.png")

# ggplot(dat, aes(x = r, y = area_curvature)) +
# 	geom_point() + geom_smooth(method = "lm") 

ggplot(dat, aes( x = cube_R2 / quad_R2)) + geom_histogram(bins = 50) + 
	scale_x_continuous(trans = scales::log10_trans())

ggplot(filter(dat, quad_R2 > 0.8), aes(x = log(r), y = log(quad_curvature))) +
 	geom_point() + geom_smooth(method = "lm") 
ggsave("../plots/quad_curve_amp.png", width = 4, height = 3)

cor.test(log(dat$quad_curvature) ,log(dat$r))


aggregate(data = dat, quad_R2 ~ person, FUN = median)

ggplot(dat, aes(x = as.factor(person), y = quad_R2)) + geom_bar(stat="summary", fun.y = median)



# plot a few random examples of bad quartic but good cubic

saccades <-	read_csv(paste("saccade_samples_dat_eyelink.csv", sep = ""))

dat %>% 
	filter(line_R2 >.5, quad_R2 > 0.9) %>%
	sample_n(5) %>%
	mutate(key = paste(person, trlNum, saccNum)) -> ss



saccades %>%
	mutate(key = paste(person, trial, n)) %>%
	filter(key %in% ss$key) %>%
	ggplot(aes(x = x, y = y, colour = key)) + geom_path()
ggsave("../plots/good_linear_fits.png")


saccades %>%
	mutate(key = paste(Person, TrialIndex, SaccNumber)) %>%
	filter(key %in% ss$key) %>%
	ggplot(aes(x = x_n, y = y_n, colour = key)) + geom_path()
ggsave("../plots/good_linear_fits_n.png")

# good linear fits