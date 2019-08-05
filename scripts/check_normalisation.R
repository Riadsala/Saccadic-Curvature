library(tidyverse)

alg = "eyelink"
saccades <-	read_csv(paste("saccade_samples_dat_", alg, ".csv", sep = ""))
dat <- read_csv(paste("saccade_dat_", alg, ".csv", sep = ""))


dat %>% 
	sample_n(10) %>%
	mutate(key = paste(person, trlNum, saccNum)) -> ss

saccades %>%
	mutate(key = paste(person, trial, n)) %>%
	filter(key %in% ss$key) %>%
	ggplot(aes(x = x, y = y, colour = key)) + geom_path() + 
		theme_bw() +
		theme(legend.position = "none", panel.grid = element_blank()) +
		xlim(c(1, 1024)) + ylim(c(1, 768)) 
ggsave("un.png")


saccades %>%
	mutate(key = paste(person, trial, n)) %>%
	filter(key %in% ss$key) %>%
	ggplot(aes(x = xn, y = yn, colour = key)) + geom_path() + 
		theme_bw() +
		theme(legend.position = "none", panel.grid = element_blank()) +
ggsave("no.png")