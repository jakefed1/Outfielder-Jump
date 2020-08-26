library(tidyverse)
standardize <- function(x){
 mu <- mean(x, na.rm = TRUE)
   sigma <- sd(x, na.rm = TRUE)
    return( (x - mu)/sigma )
   }
jump_2019<-read_csv(file="jump_2019.csv")
jump_2018<-read_csv(file="jump_2018.csv")
jump_2017<-read_csv(file="jump_2017.csv")
jump_2016<-read_csv(file="jump_2016.csv")
jump_2019
jump_2019<-
  jump_2019 %>%
  mutate(z_burst = standardize(burst)) %>%
  mutate(z_reaction = standardize(reaction)) %>%
  mutate(z_route = standardize(route))
jump_2019<-
  jump_2019 %>%
  mutate(OAA_per_play = outs_above_average/plays)
jump_2019 <-
  jump_2019 %>%
  mutate(z_OAA_per_play = standardize(OAA_per_play))
jump_2019 <-
  jump_2019 %>%
  mutate(z_ft_vs_avg = standardize(ft_vs_avg))
jump_2018<-
  jump_2018 %>%
  mutate(z_burst = standardize(burst)) %>%
  mutate(z_reaction = standardize(reaction)) %>%
  mutate(z_route = standardize(route))
jump_2018<-
  jump_2018 %>%
  mutate(OAA_per_play = outs_above_average/plays)
jump_2018 <-
  jump_2018 %>%
  mutate(z_OAA_per_play = standardize(OAA_per_play))
jump_2018 <-
  jump_2018 %>%
  mutate(z_ft_vs_avg = standardize(ft_vs_avg))
jump_2017<-
  jump_2017 %>%
  mutate(z_burst = standardize(burst)) %>%
  mutate(z_reaction = standardize(reaction)) %>%
  mutate(z_route = standardize(route))
jump_2017<-
  jump_2017 %>%
  mutate(OAA_per_play = outs_above_average/plays)
jump_2017 <-
  jump_2017 %>%
  mutate(z_OAA_per_play = standardize(OAA_per_play))
jump_2017 <-
  jump_2017 %>%
  mutate(z_ft_vs_avg = standardize(ft_vs_avg))
jump_2016<-
  jump_2016 %>%
  mutate(z_burst = standardize(burst)) %>%
  mutate(z_reaction = standardize(reaction)) %>%
  mutate(z_route = standardize(route))
jump_2016<-
  jump_2016 %>%
  mutate(OAA_per_play = outs_above_average/plays)
jump_2016 <-
  jump_2016 %>%
  mutate(z_OAA_per_play = standardize(OAA_per_play))
jump_2016 <-
  jump_2016 %>%
  mutate(z_ft_vs_avg = standardize(ft_vs_avg))

jump_overall<-
  jump_2019 %>%
  inner_join(jump_2018, by="last_name")
jump_overall<-
  jump_overall %>%
  inner_join(jump_2017, by="last_name")
jump_overall<-
  jump_overall %>%
  inner_join(jump_2016, by="last_name")
jump_overall
jump_18_19<-
  jump_2019 %>%
  inner_join(jump_2018, by="last_name")
jump_17_18<-
  jump_2018 %>%
  inner_join(jump_2017, by="last_name")
jump_16_17<-
  jump_2017 %>%
  inner_join(jump_2016, by="last_name")
jump_2019 <-
  jump_2019 %>%
  select(last_name, first_name, year, z_burst, z_reaction, z_route, z_OAA_per_play, z_ft_vs_avg)
jump_2018 <-
  jump_2018 %>%
  select(last_name, first_name, year, z_burst, z_reaction, z_route, z_OAA_per_play, z_ft_vs_avg)
jump_2017 <-
  jump_2017 %>%
  select(last_name, first_name, year, z_burst, z_reaction, z_route, z_OAA_per_play, z_ft_vs_avg)
jump_2016 <-
  jump_2016 %>%
  select(last_name, first_name, year, z_burst, z_reaction, z_route, z_OAA_per_play, z_ft_vs_avg)
jump_16_17 <-
  jump_16_17 %>%
  select(last_name, first_name.x, year.x, year.y, z_burst.x, z_burst.y, z_reaction.x, z_reaction.y, z_route.x, z_route.y, z_OAA_per_play.x, z_OAA_per_play.y, z_ft_vs_avg.x, z_ft_vs_avg.y)
jump_17_18 <-
  jump_17_18 %>%
  select(last_name, first_name.x, year.x, year.y, z_burst.x, z_burst.y, z_reaction.x, z_reaction.y, z_route.x, z_route.y, z_OAA_per_play.x, z_OAA_per_play.y, z_ft_vs_avg.x, z_ft_vs_avg.y)
jump_18_19 <-
  jump_18_19 %>%
  select(last_name, first_name.x, year.x, year.y, z_burst.x, z_burst.y, z_reaction.x, z_reaction.y, z_route.x, z_route.y, z_OAA_per_play.x, z_OAA_per_play.y, z_ft_vs_avg.x, z_ft_vs_avg.y)
jump_overall <-
  jump_overall %>%
  select(last_name, first_name.x, year.x, year.y, year.x.x, year.y.y, z_burst.x, z_burst.y, z_burst.x.x, z_burst.y.y, z_reaction.x, z_reaction.y, z_reaction.x.x, z_reaction.y.y, z_route.x, z_route.y, z_route.x.x, z_route.y.y, z_OAA_per_play.x, z_OAA_per_play.y, z_OAA_per_play.x.x, z_OAA_per_play.y.y, z_ft_vs_avg.x, z_ft_vs_avg.y, z_ft_vs_avg.x.x, z_ft_vs_avg.y.y)

# make ggplots of each skill vs itself year to year and then first year to last year to see which one is most skill-like

burst_1_fit <- lm(data = jump_16_17, z_burst.x ~ z_burst.y)
burst_1 <-
  ggplot(data = jump_16_17) +
  geom_point(mapping = aes(x = z_burst.y, y = z_burst.x), col = "blue") +
  geom_abline(intercept = -0.141, slope = 0.891, col = "red") +
  labs(
    x="Burst in 2016 (standardized)",
    y="Burst in 2017 (standardized)",
    title = "2016 and 2017 burst",
    subtitle = "R-squared=0.6599",
    caption="Data from Baseball Savant") +
  theme_bw()
burst_1
summarize(jump_16_17, correlation = cor(z_burst.y, z_burst.x))
summary(burst_1_fit)

burst_2_fit <- lm(data = jump_17_18, z_burst.x ~ z_burst.y)
burst_2 <-
  ggplot(data = jump_17_18) +
  geom_point(mapping = aes(x = z_burst.y, y = z_burst.x), col = "blue") +
  geom_abline(intercept = 0.02413, slope = 0.51922, col = "red") +
  labs(
    x="Burst in 2017 (standardized)",
    y="Burst in 2018 (standardized)",
    title = "2017 and 2018 burst",
    subtitle = "R-squared=0.3405",
    caption="Data from Baseball Savant") +
  theme_bw()
burst_2
summarize(jump_17_18, correlation = cor(z_burst.y, z_burst.x))
summary(burst_2_fit)

burst_3_fit <- lm(data = jump_18_19, z_burst.x ~ z_burst.y)
burst_3 <-
  ggplot(data = jump_18_19) +
  geom_point(mapping = aes(x = z_burst.y, y = z_burst.x), col = "blue") +
  geom_abline(intercept = -0.08808, slope = 0.65774, col = "red") +
  labs(
    x="Burst in 2018 (standardized)",
    y="Burst in 2019 (standardized)",
    title = "2018 and 2019 burst",
    subtitle = "R-squared=0.4782",
    caption="Data from Baseball Savant") +
  theme_bw()
burst_3
summarize(jump_18_19, correlation = cor(z_burst.y, z_burst.x))
summary(burst_3_fit)

burst_all_fit <- lm(data = jump_overall, z_burst.x ~ z_burst.y.y)
burst_all <-
  ggplot(data = jump_overall) +
  geom_point(mapping = aes(x = z_burst.y.y, y = z_burst.x), col = "blue") +
  geom_abline(intercept = -0.2715, slope = 0.6709, col = "red") +
  labs(
    x="Burst in 2016 (standardized)",
    y="Burst in 2019 (standardized)",
    title = "2016 and 2019 burst",
    subtitle = "R-squared=0.3658",
    caption="Data from Baseball Savant") +
  theme_bw()
burst_all
summarize(jump_overall, correlation = cor(z_burst.y, z_burst.x))
summary(burst_all_fit)

reaction_1_fit <- lm(data = jump_16_17, z_reaction.x ~ z_reaction.y)
reaction_1 <-
  ggplot(data = jump_16_17) +
  geom_point(mapping = aes(x = z_reaction.y, y = z_reaction.x), col = "blue") +
  geom_abline(intercept = 0.06842, slope = 0.89796, col = "red") +
  labs(
    x="Reaction in 2016 (standardized)",
    y="Reaction in 2017 (standardized)",
    title = "2016 and 2017 reaction",
    subtitle = "R-squared=0.7446",
    caption="Data from Baseball Savant") +
  theme_bw()
reaction_1
summarize(jump_16_17, correlation = cor(z_reaction.y, z_reaction.x))
summary(reaction_1_fit)

reaction_2_fit <- lm(data = jump_17_18, z_reaction.x ~ z_reaction.y)
reaction_2 <-
  ggplot(data = jump_17_18) +
  geom_point(mapping = aes(x = z_reaction.y, y = z_reaction.x), col = "blue") +
  geom_abline(intercept = 0.01848, slope = 0.82152, col = "red") +
  labs(
    x="Reaction in 2017 (standardized)",
    y="Reaction in 2018 (standardized)",
    title = "2017 and 2018 reaction",
    subtitle = "R-squared=0.6878",
    caption="Data from Baseball Savant") +
  theme_bw()
reaction_2
summarize(jump_17_18, correlation = cor(z_reaction.y, z_reaction.x))
summary(reaction_2_fit)

reaction_3_fit <- lm(data = jump_18_19, z_reaction.x ~ z_reaction.y)
reaction_3 <-
  ggplot(data = jump_18_19) +
  geom_point(mapping = aes(x = z_reaction.y, y = z_reaction.x), col = "blue") +
  geom_abline(intercept = 0.07175, slope = 0.89720, col = "red") +
  labs(
    x="Reaction in 2018 (standardized)",
    y="Reaction in 2019 (standardized)",
    title = "2018 and 2019 reaction",
    subtitle = "R-squared=0.7393",
    caption="Data from Baseball Savant") +
  theme_bw()
reaction_3
summarize(jump_18_19, correlation = cor(z_reaction.y, z_reaction.x))
summary(reaction_3_fit)

reaction_all_fit <- lm(data = jump_overall, z_reaction.x ~ z_reaction.y.y)
reaction_all <-
  ggplot(data = jump_overall) +
  geom_point(mapping = aes(x = z_reaction.y.y, y = z_reaction.x), col = "blue") +
  geom_abline(intercept = 0.007887, slope = 0.943495, col = "red") +
  labs(
    x="Reaction in 2016 (standardized)",
    y="Reaction in 2019 (standardized)",
    title = "2016 and 2019 reaction",
    subtitle = "R-squared=0.7318",
    caption="Data from Baseball Savant") +
  theme_bw()
reaction_all
summarize(jump_overall, correlation = cor(z_reaction.y.y, z_reaction.x))
summary(reaction_all_fit)

route_1_fit <- lm(data = jump_16_17, z_route.x ~ z_route.y)
route_1 <-
  ggplot(data = jump_16_17) +
  geom_point(mapping = aes(x = z_route.y, y = z_route.x), col = "blue") +
  geom_abline(intercept = -0.09139, slope = 0.96503, col = "red") +
  labs(
    x="Route in 2016 (standardized)",
    y="Route in 2017 (standardized)",
    title = "2016 and 2017 route",
    subtitle = "R-squared=0.7981",
    caption="Data from Baseball Savant") +
  theme_bw()
route_1
summarize(jump_16_17, correlation = cor(z_route.y, z_route.x))
summary(route_1_fit)

route_2_fit <- lm(data = jump_17_18, z_route.x ~ z_route.y)
route_2 <-
  ggplot(data = jump_17_18) +
  geom_point(mapping = aes(x = z_route.y, y = z_route.x), col = "blue") +
  geom_abline(intercept = 0.1011, slope = 0.8157, col = "red") +
  labs(
    x="Route in 2017 (standardized)",
    y="Route in 2018 (standardized)",
    title = "2017 and 2018 route",
    subtitle = "R-squared=0.7102",
    caption="Data from Baseball Savant") +
  theme_bw()
route_2
summarize(jump_17_18, correlation = cor(z_route.y, z_route.x))
summary(route_2_fit)

route_3_fit <- lm(data = jump_18_19, z_route.x ~ z_route.y)
route_3 <-
  ggplot(data = jump_18_19) +
  geom_point(mapping = aes(x = z_route.y, y = z_route.x), col = "blue") +
  geom_abline(intercept = -0.04481, slope = 0.84464, col = "red") +
  labs(
    x="Route in 2018 (standardized)",
    y="Route in 2019 (standardized)",
    title = "2018 and 2019 route",
    subtitle = "R-squared=0.6356",
    caption="Data from Baseball Savant") +
  theme_bw()
route_3
summarize(jump_18_19, correlation = cor(z_route.y, z_route.x))
summary(route_3_fit)

route_all_fit <- lm(data = jump_overall, z_route.x ~ z_route.y.y)
route_all <-
  ggplot(data = jump_overall) +
  geom_point(mapping = aes(x = z_route.y.y, y = z_route.x), col = "blue") +
  geom_abline(intercept = -0.1452, slope = 0.8439, col = "red") +
  labs(
    x="Route in 2016 (standardized)",
    y="Route in 2019 (standardized)",
    title = "2016 and 2019 route",
    subtitle = "R-squared=0.4794",
    caption="Data from Baseball Savant") +
  theme_bw()
route_all
summarize(jump_overall, correlation = cor(z_route.y.y, z_route.x))
summary(route_all_fit)

# make ggplots to try to determine which skill is most important/correlated to jump

burst_jump_1_fit <- lm(data = jump_2016, z_ft_vs_avg ~ z_burst)
burst_jump_1 <-
  ggplot(data = jump_2016) +
  geom_point(mapping = aes(x = z_burst , y = z_ft_vs_avg), col = "blue") +
  geom_abline(intercept = -6.33e-17 , slope = 8.97e-01 , col = "red") +
  labs(
    x="Burst in 2016 (standardized)",
    y="Jump in 2016 (standardized)",
    title = "Burst vs Total Jump 2016",
    subtitle = "R-squared=0.8026",
    caption="Data from Baseball Savant") +
  theme_bw()
burst_jump_1
summarize(jump_2016, correlation = cor(z_burst, z_ft_vs_avg))
summary(burst_jump_1_fit)

burst_jump_2_fit <- lm(data = jump_2017, z_ft_vs_avg ~ z_burst)
burst_jump_2 <-
  ggplot(data = jump_2017) +
  geom_point(mapping = aes(x = z_burst , y = z_ft_vs_avg), col = "blue") +
  geom_abline(intercept = -1.037e-17, slope = 8.994e-01, col = "red") +
  labs(
    x="Burst in 2017 (standardized)",
    y="Jump in 2017 (standardized)",
    title = "Burst vs Total Jump 2017",
    subtitle = "R-squared=0.8071",
    caption="Data from Baseball Savant") +
  theme_bw()
burst_jump_2
summarize(jump_2017, correlation = cor(z_burst, z_ft_vs_avg))
summary(burst_jump_2_fit)

burst_jump_3_fit <- lm(data = jump_2018, z_ft_vs_avg ~ z_burst)
burst_jump_3 <-
  ggplot(data = jump_2018) +
  geom_point(mapping = aes(x = z_burst , y = z_ft_vs_avg), col = "blue") +
  geom_abline(intercept = 4.178e-17, slope = 9.094e-01 , col = "red") +
  labs(
    x="Burst in 2018 (standardized)",
    y="Jump in 2018 (standardized)",
    title = "Burst vs Total Jump 2018",
    subtitle = "R-squared=0.8253",
    caption="Data from Baseball Savant") +
  theme_bw()
burst_jump_3
summarize(jump_2018, correlation = cor(z_burst, z_ft_vs_avg))
summary(burst_jump_3_fit)

burst_jump_4_fit <- lm(data = jump_2019, z_ft_vs_avg ~ z_burst)
burst_jump_4 <-
  ggplot(data = jump_2019) +
  geom_point(mapping = aes(x = z_burst , y = z_ft_vs_avg), col = "blue") +
  geom_abline(intercept = -4.478e-17 , slope = 9.357e-01 , col = "red") +
  labs(
    x="Burst in 2019 (standardized)",
    y="Jump in 2019 (standardized)",
    title = "Burst vs Total Jump 2019",
    subtitle = "R-squared=0.8742",
    caption="Data from Baseball Savant") +
  theme_bw()
burst_jump_4
summarize(jump_2019, correlation = cor(z_burst, z_ft_vs_avg))
summary(burst_jump_4_fit)

reaction_jump_1_fit <- lm(data = jump_2016, z_ft_vs_avg ~ z_reaction)
reaction_jump_1 <-
  ggplot(data = jump_2016) +
  geom_point(mapping = aes(x = z_reaction , y = z_ft_vs_avg), col = "blue") +
  geom_abline(intercept = 2.631e-18, slope = 6.627e-01, col = "red") +
  labs(
    x="Reaction in 2016 (standardized)",
    y="Jump in 2016 (standardized)",
    title = "Reaction vs Total Jump 2016",
    subtitle = "R-squared=0.4335",
    caption="Data from Baseball Savant") +
  theme_bw()
reaction_jump_1
summarize(jump_2016, correlation = cor(z_reaction, z_ft_vs_avg))
summary(reaction_jump_1_fit)

reaction_jump_2_fit <- lm(data = jump_2017, z_ft_vs_avg ~ z_reaction)
reaction_jump_2 <-
  ggplot(data = jump_2017) +
  geom_point(mapping = aes(x = z_reaction , y = z_ft_vs_avg), col = "blue") +
  geom_abline(intercept = -1.694e-17, slope = 6.598e-01, col = "red") +
  labs(
    x="Reaction in 2017 (standardized)",
    y="Jump in 2017 (standardized)",
    title = "Reaction vs Total Jump 2017",
    subtitle = "R-squared=0.4297",
    caption="Data from Baseball Savant") +
  theme_bw()
reaction_jump_2
summarize(jump_2017, correlation = cor(z_reaction, z_ft_vs_avg))
summary(reaction_jump_2_fit)

reaction_jump_3_fit <- lm(data = jump_2018, z_ft_vs_avg ~ z_reaction)
reaction_jump_3 <-
  ggplot(data = jump_2018) +
  geom_point(mapping = aes(x = z_reaction , y = z_ft_vs_avg), col = "blue") +
  geom_abline(intercept = 3.667e-17, slope = 5.410e-01, col = "red") +
  labs(
    x="Reaction in 2018 (standardized)",
    y="Jump in 2018 (standardized)",
    title = "Reaction vs Total Jump 2018",
    subtitle = "R-squared=0.2856",
    caption="Data from Baseball Savant") +
  theme_bw()
reaction_jump_3
summarize(jump_2018, correlation = cor(z_reaction, z_ft_vs_avg))
summary(reaction_jump_3_fit)

reaction_jump_4_fit <- lm(data = jump_2019, z_ft_vs_avg ~ z_reaction)
reaction_jump_4 <-
  ggplot(data = jump_2019) +
  geom_point(mapping = aes(x = z_reaction , y = z_ft_vs_avg), col = "blue") +
  geom_abline(intercept = -2.956e-17, slope = 5.963e-01, col = "red") +
  labs(
    x="Reaction in 2019 (standardized)",
    y="Jump in 2019 (standardized)",
    title = "Reaction vs Total Jump 2019",
    subtitle = "R-squared=0.3489",
    caption="Data from Baseball Savant") +
  theme_bw()
reaction_jump_4
summarize(jump_2019, correlation = cor(z_reaction, z_ft_vs_avg))
summary(reaction_jump_4_fit)

route_jump_1_fit <- lm(data = jump_2016, z_ft_vs_avg ~ z_route)
route_jump_1 <-
  ggplot(data = jump_2016) +
  geom_point(mapping = aes(x = z_route , y = z_ft_vs_avg), col = "blue") +
  geom_abline(intercept = -1.645e-17, slope = -1.304e-01, col = "red") +
  labs(
    x="Route in 2016 (standardized)",
    y="Jump in 2016 (standardized)",
    title = "Route vs Total Jump 2016",
    subtitle = "R-squared=0.007",
    caption="Data from Baseball Savant") +
  theme_bw()
route_jump_1
summarize(jump_2016, correlation = cor(z_route, z_ft_vs_avg))
summary(route_jump_1_fit)

route_jump_2_fit <- lm(data = jump_2017, z_ft_vs_avg ~ z_route)
route_jump_2 <-
  ggplot(data = jump_2017) +
  geom_point(mapping = aes(x = z_route , y = z_ft_vs_avg), col = "blue") +
  geom_abline(intercept = 2.528e-18, slope = -2.170e-01 , col = "red") +
  labs(
    x="Route in 2017 (standardized)",
    y="Jump in 2017 (standardized)",
    title = "Route vs Total Jump 2017",
    subtitle = "R-squared=0.03765",
    caption="Data from Baseball Savant") +
  theme_bw()
route_jump_2
summarize(jump_2017, correlation = cor(z_route, z_ft_vs_avg))
summary(route_jump_2_fit)

route_jump_3_fit <- lm(data = jump_2018, z_ft_vs_avg ~ z_route)
route_jump_3 <-
  ggplot(data = jump_2018) +
  geom_point(mapping = aes(x = z_route, y = z_ft_vs_avg), col = "blue") +
  geom_abline(intercept = 2.722e-17, slope = -8.131e-02, col = "red") +
  labs(
    x="Route in 2018 (standardized)",
    y="Jump in 2018 (standardized)",
    title = "Route vs Total Jump 2018",
    subtitle = "R-squared=-0.003224",
    caption="Data from Baseball Savant") +
  theme_bw()
route_jump_3
summarize(jump_2018, correlation = cor(z_route, z_ft_vs_avg))
summary(route_jump_3_fit)

route_jump_4_fit <- lm(data = jump_2019, z_ft_vs_avg ~ z_route)
route_jump_4 <-
  ggplot(data = jump_2019) +
  geom_point(mapping = aes(x = z_route , y = z_ft_vs_avg), col = "blue") +
  geom_abline(intercept = -1.293e-17, slope = -2.257e-01, col = "red") +
  labs(
    x="Route in 2019 (standardized)",
    y="Jump in 2019 (standardized)",
    title = "Route vs Total Jump 2019",
    subtitle = "R-squared=0.04105",
    caption="Data from Baseball Savant") +
  theme_bw()
route_jump_4
summarize(jump_2019, correlation = cor(z_route, z_ft_vs_avg))
summary(route_jump_4_fit)

# make ggplots to try to determine which skill is most important/correlated to/with actually making the play/OAA

burst_jump_outs_1_fit <- lm(data = jump_2016, z_OAA_per_play ~ z_burst)
burst_jump_outs_1 <-
  ggplot(data = jump_2016) +
  geom_point(mapping = aes(x = z_burst , y = z_OAA_per_play), col = "blue") +
  geom_abline(intercept = -5.869e-17, slope = 8.591e-01, col = "red") +
  labs(
    x="Burst in 2016 (standardized)",
    y="OAA per play in 2016 (standardized)",
    title = "Burst vs OAA 2016",
    subtitle = "R-squared=0.7355",
    caption="Data from Baseball Savant") +
  theme_bw()
burst_jump_outs_1
summarize(jump_2016, correlation = cor(z_burst, z_OAA_per_play))
summary(burst_jump_outs_1_fit)

burst_jump_outs_2_fit <- lm(data = jump_2017, z_OAA_per_play ~ z_burst)
burst_jump_outs_2 <-
  ggplot(data = jump_2017) +
  geom_point(mapping = aes(x = z_burst , y = z_OAA_per_play), col = "blue") +
  geom_abline(intercept = -1.585e-17, slope = 8.162e-01, col = "red") +
  labs(
    x="Burst in 2017 (standardized)",
    y="OAA per play in 2017 (standardized)",
    title = "Burst vs OAA 2017",
    subtitle = "R-squared=0.6628",
    caption="Data from Baseball Savant") +
  theme_bw()
burst_jump_outs_2
summarize(jump_2017, correlation = cor(z_burst, z_OAA_per_play))
summary(burst_jump_outs_2_fit)

burst_jump_outs_3_fit <- lm(data = jump_2018, z_OAA_per_play ~ z_burst)
burst_jump_outs_3 <-
  ggplot(data = jump_2018) +
  geom_point(mapping = aes(x = z_burst , y = z_OAA_per_play), col = "blue") +
  geom_abline(intercept = 1.161e-16, slope = 8.056e-01, col = "red") +
  labs(
    x="Burst in 2018 (standardized)",
    y="OAA per play in 2018 (standardized)",
    title = "Burst vs OAA 2018",
    subtitle = "R-squared=0.6455",
    caption="Data from Baseball Savant") +
  theme_bw()
burst_jump_outs_3
summarize(jump_2018, correlation = cor(z_burst, z_OAA_per_play))
summary(burst_jump_outs_3_fit)

burst_jump_outs_4_fit <- lm(data = jump_2019, z_OAA_per_play ~ z_burst)
burst_jump_outs_4 <-
  ggplot(data = jump_2019) +
  geom_point(mapping = aes(x = z_burst , y = z_OAA_per_play), col = "blue") +
  geom_abline(intercept = -2.177e-17, slope = 7.977e-01, col = "red") +
  labs(
    x="Burst in 2019 (standardized)",
    y="OAA per play in 2019 (standardized)",
    title = "Burst vs OAA 2019",
    subtitle = "R-squared=0.6326",
    caption="Data from Baseball Savant") +
  theme_bw()
burst_jump_outs_4
summarize(jump_2019, correlation = cor(z_burst, z_OAA_per_play))
summary(burst_jump_outs_4_fit)

reaction_jump_outs_1_fit <- lm(data = jump_2016, z_OAA_per_play ~ z_reaction)
reaction_jump_outs_1 <-
  ggplot(data = jump_2016) +
  geom_point(mapping = aes(x = z_reaction , y = z_OAA_per_play), col = "blue") +
  geom_abline(intercept = -3.844e-18, slope = 3.840e-01, col = "red") +
  labs(
    x="Reaction in 2016 (standardized)",
    y="OAA per play in 2016 (standardized)",
    title = "Reaction vs OAA 2016",
    subtitle = "R-squared=0.1389",
    caption="Data from Baseball Savant") +
  theme_bw()
reaction_jump_outs_1
summarize(jump_2016, correlation = cor(z_reaction, z_OAA_per_play))
summary(reaction_jump_outs_1_fit)

reaction_jump_outs_2_fit <- lm(data = jump_2017, z_OAA_per_play ~ z_reaction)
reaction_jump_outs_2 <-
  ggplot(data = jump_2017) +
  geom_point(mapping = aes(x = z_reaction , y = z_OAA_per_play), col = "blue") +
  geom_abline(intercept = -1.545e-17, slope = 4.824e-01, col = "red") +
  labs(
    x="Reaction in 2017 (standardized)",
    y="OAA per play in 2017 (standardized)",
    title = "Reaction vs OAA 2017",
    subtitle = "R-squared=0.2251",
    caption="Data from Baseball Savant") +
  theme_bw()
reaction_jump_outs_2
summarize(jump_2017, correlation = cor(z_reaction, z_OAA_per_play))
summary(reaction_jump_outs_2_fit)

reaction_jump_outs_3_fit <- lm(data = jump_2018, z_OAA_per_play ~ z_reaction)
reaction_jump_outs_3 <-
  ggplot(data = jump_2018) +
  geom_point(mapping = aes(x = z_reaction , y = z_OAA_per_play), col = "blue") +
  geom_abline(intercept = 1.071e-16, slope = 3.174e-01, col = "red") +
  labs(
    x="Reaction in 2018 (standardized)",
    y="OAA per play in 2018 (standardized)",
    title = "Reaction vs OAA 2018",
    subtitle = "R-squared=0.09183",
    caption="Data from Baseball Savant") +
  theme_bw()
reaction_jump_outs_3
summarize(jump_2018, correlation = cor(z_reaction, z_OAA_per_play))
summary(reaction_jump_outs_3_fit)

reaction_jump_outs_4_fit <- lm(data = jump_2019, z_OAA_per_play ~ z_reaction)
reaction_jump_outs_4 <-
  ggplot(data = jump_2019) +
  geom_point(mapping = aes(x = z_reaction , y = z_OAA_per_play), col = "blue") +
  geom_abline(intercept = -3.023e-18, slope = 3.799e-01, col = "red") +
  labs(
    x="Reaction in 2019 (standardized)",
    y="OAA per play in 2019 (standardized)",
    title = "Reaction vs OAA 2019",
    subtitle = "R-squared=0.1354",
    caption="Data from Baseball Savant") +
  theme_bw()
reaction_jump_outs_4
summarize(jump_2019, correlation = cor(z_reaction, z_OAA_per_play))
summary(reaction_jump_outs_4_fit)

route_jump_outs_1_fit <- lm(data = jump_2016, z_OAA_per_play ~ z_route)
route_jump_outs_1 <-
  ggplot(data = jump_2016) +
  geom_point(mapping = aes(x = z_route , y = z_OAA_per_play), col = "blue") +
  geom_abline(intercept = -1.529e-17, slope = -5.797e-02, col = "red") +
  labs(
    x="Route in 2016 (standardized)",
    y="OAA per play in 2016 (standardized)",
    title = "Route vs OAA 2016",
    subtitle = "R-squared=-0.006706",
    caption="Data from Baseball Savant") +
  theme_bw()
route_jump_outs_1
summarize(jump_2016, correlation = cor(z_route, z_OAA_per_play))
summary(route_jump_outs_1_fit)

route_jump_outs_2_fit <- lm(data = jump_2017, z_OAA_per_play ~ z_route)
route_jump_outs_2 <-
  ggplot(data = jump_2017) +
  geom_point(mapping = aes(x = z_route , y = z_OAA_per_play), col = "blue") +
  geom_abline(intercept = -4.684e-18, slope = -2.040e-01, col = "red") +
  labs(
    x="Route in 2017 (standardized)",
    y="OAA per play in 2017 (standardized)",
    title = "Route vs OAA 2017",
    subtitle = "R-squared=0.03214",
    caption="Data from Baseball Savant") +
  theme_bw()
route_jump_outs_2
summarize(jump_2017, correlation = cor(z_route, z_OAA_per_play))
summary(route_jump_outs_2_fit)

route_jump_outs_3_fit <- lm(data = jump_2018, z_OAA_per_play ~ z_route)
route_jump_outs_3 <-
  ggplot(data = jump_2018) +
  geom_point(mapping = aes(x = z_route, y = z_OAA_per_play), col = "blue") +
  geom_abline(intercept = 9.783e-17, slope = 9.528e-03, col = "red") +
  labs(
    x="Route in 2018 (standardized)",
    y="OAA per play in 2018 (standardized)",
    title = "Route vs OAA 2018",
    subtitle = "R-squared=-0.009809",
    caption="Data from Baseball Savant") +
  theme_bw()
route_jump_outs_3
summarize(jump_2018, correlation = cor(z_route, z_OAA_per_play))
summary(route_jump_outs_3_fit)

route_jump_outs_4_fit <- lm(data = jump_2019, z_OAA_per_play ~ z_route)
route_jump_outs_4 <-
  ggplot(data = jump_2019) +
  geom_point(mapping = aes(x = z_route , y = z_OAA_per_play), col = "blue") +
  geom_abline(intercept = 7.201e-18, slope = -1.520e-01, col = "red") +
  labs(
    x="Route in 2019 (standardized)",
    y="OAA per play in 2019 (standardized)",
    title = "Route vs OAA 2019",
    subtitle = "R-squared=0.01292",
    caption="Data from Baseball Savant") +
  theme_bw()
route_jump_outs_4
summarize(jump_2019, correlation = cor(z_route, z_OAA_per_play))
summary(route_jump_outs_4_fit)

#jump vs OAA

jump_OAA_1_fit <- lm(data = jump_2016, z_OAA_per_play ~ z_ft_vs_avg)
jump_OAA_1 <-
  ggplot(data = jump_2016) +
  geom_point(mapping = aes(x = z_ft_vs_avg , y = z_OAA_per_play), col = "blue") +
  geom_abline(intercept = -9.434e-19, slope = 8.083e-01, col = "red") +
  labs(
    x="Jump in 2016 (standardized)",
    y="OAA per play in 2016 (standardized)",
    title = "Jump and OAA 2016",
    subtitle = "R-squared=0.6499",
    caption="Data from Baseball Savant") +
  theme_bw()
jump_OAA_1
summarize(jump_2016, correlation = cor(z_ft_vs_avg, z_OAA_per_play))
summary(jump_OAA_1_fit)

jump_OAA_2_fit <- lm(data = jump_2017, z_OAA_per_play ~ z_ft_vs_avg)
jump_OAA_2 <-
  ggplot(data = jump_2017) +
  geom_point(mapping = aes(x = z_ft_vs_avg , y = z_OAA_per_play), col = "blue") +
  geom_abline(intercept = -4.751e-18, slope = 8.196e-01, col = "red") +
  labs(
    x="Jump in 2017 (standardized)",
    y="OAA per play in 2017 (standardized)",
    title = "Jump and OAA 2017",
    subtitle = "R-squared=0.6685",
    caption="Data from Baseball Savant") +
  theme_bw()
jump_OAA_2
summarize(jump_2017, correlation = cor(z_ft_vs_avg, z_OAA_per_play))
summary(jump_OAA_2_fit)

jump_OAA_3_fit <- lm(data = jump_2018, z_OAA_per_play ~ z_ft_vs_avg)
jump_OAA_3 <-
  ggplot(data = jump_2018) +
  geom_point(mapping = aes(x = z_ft_vs_avg, y = z_OAA_per_play), col = "blue") +
  geom_abline(intercept = 8.086e-17, slope = 8.040e-01, col = "red") +
  labs(
    x="Jump in 2018 (standardized)",
    y="OAA per play in 2018 (standardized)",
    title = "Jump and OAA 2018",
    subtitle = "R-squared=0.643",
    caption="Data from Baseball Savant") +
  theme_bw()
jump_OAA_3
summarize(jump_2018, correlation = cor(z_ft_vs_avg, z_OAA_per_play))
summary(jump_OAA_3_fit)

jump_OAA_4_fit <- lm(data = jump_2019, z_OAA_per_play ~ z_ft_vs_avg)
jump_OAA_4 <-
  ggplot(data = jump_2019) +
  geom_point(mapping = aes(x = z_ft_vs_avg , y = z_OAA_per_play), col = "blue") +
  geom_abline(intercept = 1.624e-17, slope = 7.915e-01, col = "red") +
  labs(
    x="Jump in 2019 (standardized)",
    y="OAA per play in 2019 (standardized)",
    title = "Jump and OAA 2019",
    subtitle = "R-squared=0.6226",
    caption="Data from Baseball Savant") +
  theme_bw()
jump_OAA_4
summarize(jump_2019, correlation = cor(z_ft_vs_avg, z_OAA_per_play))
summary(jump_OAA_4_fit)

#exposing Market inefficiency???

jump_18_19 <-
  jump_18_19 %>%
  mutate(Discrepancy.y =(z_burst.y - z_OAA_per_play.y))
jump_18_19 <-
  jump_18_19 %>%
  mutate(Discrepancy.x =(z_burst.x - z_OAA_per_play.x))
jump_18_19 <-
  jump_18_19 %>%
  mutate(avg_discrepancy = ((Discrepancy.y + Discrepancy.x)/2))

jump_18_19 %>%
  select(last_name, first_name.x, avg_discrepancy) %>%
  arrange(desc(avg_discrepancy))