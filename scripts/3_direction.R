library(tidyverse)
library(brms)
library(tidybayes)
library(modelr)

rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


alg <- "eyelink"

dat <- read_csv(paste("saccade_dat_", alg, ".csv", sep = ""))

n_dir <- c(4,8)
dir_labels <- list(
	c("right", "up", "left", "down"),
	c("right", "right-up", "up", "left-up", "left", "left-down", "down", "right-down"))

dd <- 2

angle_width <- 360/n_dir[dd]

dat %>% 
	mutate(
		direction = (theta * 180/pi) %% 360,
		direction = round((direction)/ angle_width) %% n_dir[dd],
		direction = factor(direction, labels =dir_labels[[dd]]),
		hori = if_else(direction %in% c("left", "right"), "yes", "no")
	) -> dat

# plotting to make sure direction categories are all working
dat %>%
	mutate(x = x2-x1, y = y2-y1) %>%
	select(direction, x, y) %>%
	ggplot(aes(x=x, y=y, colour = direction)) + geom_point() + coord_fixed()

ggplot(dat, aes(x = direction, y = area_curvature)) + geom_boxplot() +
	scale_y_log10() + ggthemes::theme_tufte()
ggsave("../plots/max_curvature_by_direction.png")

dat %>%
	filter(quad_R2 > 0.5) %>%
	ggplot(aes(x = direction, y = quad_curvature)) + geom_boxplot() +
		scale_y_log10() + ggthemes::theme_tufte()
ggsave("../plots/quad_curvature_by_direction.png")

f <- y ~ sin(x) + cos(x) + 
	sin(2*x) + cos(2*x) + 
	sin(3*x) + cos(3*x) + 
	sin(4*x) + cos(4*x) + 
	sin(5*x) + cos(5*x) + 
	sin(6*x) + cos(6*x) + 
	sin(7*x) + cos(7*x) +
	sin(8*x) + cos(8*x) + 
	sin(9*x) + cos(9*x) + 
	sin(10*x) + cos(10*x) + 
	sin(11*x) + cos(11*x) + 
	sin(12*x) + cos(12*x) +
	sin(13*x) + cos(13*x) + 
	sin(14*x) + cos(14*x) + 
	sin(15*x) + cos(15*x) + 
	sin(16*x) + cos(16*x) +

dat %>%
	filter(quad_R2 > 0.5) %>%
	ggplot(aes(x = theta, y = quad_curvature)) +
		geom_point(alpha = 0.1) + geom_smooth(colour = "aquamarine4", formula = f, method = "lm", se = F) + 
		scale_y_log10() + theme_bw() +
		coord_polar(start = pi/2) +
		scale_x_continuous(breaks = c(0, pi/2, pi, 3*pi/2), labels = c("left", "up", "right", "down"))
		ggsave("../plots/quad_curvature_by_direction_polar.png")




m <- brm(data = dat, quad_curvature ~ hori, family = "gamma")


dat %>% 
	modelr::data_grid(hori) %>%
	add_predicted_draws(m) %>%
	ggplot() +
		geom_density(data = dat, aes(x = quad_curvature, colour = hori), bins = 100, size = 2) +
  		geom_density(aes(x = .prediction, fill = hori), alpha = 0.33) +
  		scale_fill_viridis_d("horizontal?", end = 0.6) + scale_colour_viridis_d("horizontal?", end = 0.6) +
  		xlim(0, 2)
 ggsave("../plots/bayes_quad_curve_by_direction.png")




m_r <- brm(data = dat, quad_curvature ~ log(r)*hori + (1|person), family = "gamma")



dat %>% 
	data_grid(r = seq_range(r, n = 101), hori, person) %>%
	add_predicted_draws(m_r) %>%
	select(r, hori, .prediction) -> m_dat


ggplot(dat, aes(x = log(r), y = quad_curvature, colour = hori)) + 
	geom_point(alpha = 0.1) + 
	stat_lineribbon(data = m_dat, aes(y = .prediction, fill = hori), .width = c(.90), alpha = 0.33) +
	scale_y_log10() + 
	scale_colour_viridis_d(end = 0.8, option = "plasma") + 
	scale_fill_viridis_d(end = 0.8, option = "plasma") +
	theme_bw()
ggsave("../plots/bayes_quad_curve_by_amp_direction.png", width = 4, height = 4)
