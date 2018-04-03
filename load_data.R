

library(tidyverse)
library(broom)
library(glmnet)
data_path <- "~/Dropbox/Basketball/data/"
gamedata <- read_csv(file.path(data_path,"2015-16/[10-20-2015]-[06-20-2016]-combined-stats.csv"),
                     col_types = cols(date = col_date(format = "%Y-%m-%d"),
                                      play_length= col_time(),
                                      game_id = col_number()))

## Need to construct a dataset that has STINTS
## What is a stint:
## 1) time that a set of players (5 away, 5 home) are on the court together, 
## 2) and the net scoring of home team

per_stint <- gamedata %>%  arrange(game_id, period, desc(remaining_time)) %>% 
                mutate(end_stint = type == "sub" | play_id == 0) %>% 
                filter((lag(end_stint) == 0 & end_stint == 0) |
                      (end_stint == 1 & lead(end_stint) == 0))  %>% 
                mutate(new_possession = team != lag(team)) %>%
                mutate(stint_id =cumsum(end_stint)) %>% 
                select(stint_id, starts_with("a"), starts_with("h"), play_length, period, new_possession, remaining_time, team, game_id, -assist, -away, -home, event_type)

per_stint_stats <- per_stint %>%
                    group_by(game_id, stint_id) %>% 
                    summarize(net_home_score = last(home_score) - first(home_score), net_away_score = last(away_score) - first(away_score),
                              minutes_played = sum(play_length, na.rm=TRUE), num_possessions = sum(new_possession, na.rm=TRUE)) %>%
                    filter(net_home_score >= 0) %>% filter(num_possessions != 0)
  
# There's basically one outlier

per_stint_stats <- per_stint %>% select(a1:a5, h1:h5, stint_id, game_id) %>% group_by(stint_id, game_id) %>% filter(row_number()==1) %>%
                    gather(key = "player", value = "playername", -stint_id, -game_id)   %>% 
                    group_by(stint_id, game_id) %>% 
                    right_join(per_stint_stats)

stint_matrix <- per_stint_stats %>% ungroup() %>% 
                  mutate(home = (-1 + 2*(str_detect(player, "h")))) %>% mutate(plus_minus = (net_home_score-net_away_score)/ num_possessions) %>%
                  select(plus_minus, playername, stint_id, home)

matrix <- stint_matrix %>% mutate(i = row_number()) %>% group_by(stint_id) %>% spread(key = playername, value=home, fill=0)
x <- matrix %>% ungroup() %>% select(-plus_minus, -stint_id, -i) %>% data.matrix()
y <- matrix %>% ungroup() %>% select(plus_minus) %>% data.matrix()
weights <- per_stint_stats %>% ungroup() %>% select(num_possessions) %>% data.matrix()
lambdas <- 10^seq(3, -2, by = -.1)

cv_fit <- cv.glmnet(x, y, alpha = 0, lambda = lambdas, weight=weights)
opt_lambda <- cv_fit$lambda.min
fit <- cv_fit$glmnet.fit
glm_betas <- tidy(fit) %>%
  filter(term != "(Intercept)", lambda == opt_lambda) %>%
    select(player = term, coef = estimate) %>% mutate(APM = coef*100) %>%
  arrange(APM)

ggplot(data=glm_betas, aes(x=reorder(player, -coef), y=coef)) + geom_bar(stat="identity") + coord_flip()

espn_rpm_2015 <- read_csv("~/repos/rpm/espn_rpm_2015.csv")
  
espn_rpm <- espn_rpm_2015 %>% separate(NAME, c("player", "pos"), sep=",") %>% right_join(glm_betas)

