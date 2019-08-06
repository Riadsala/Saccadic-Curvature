library(tidyverse)

alg <- "eyelink"

dat <- read_csv(paste("saccade_dat_", alg, ".csv", sep = ""))


n_dir = 4
angle_width = 360/n_dir

dat %>% 
	mutate(
		direction = (theta * 180/pi) %% 360,
		direction = round((direction)/ angle_width) %% n_dir,
		direction = factor(direction, labels = c("right", "up", "left", "down"))) -> dat


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

