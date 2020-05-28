### practice setting up the multi level model with gaussians
### instead of with betas

library(dplyr)
library(ggplot2)

n_groups <- 5

mu0 <- 0
sd0 <- 2.5

### create the group centers
set.seed(71231)
xc <- rnorm(n = n_groups, mean = mu0, sd = sd0)

### plot the cluster centers
tibble::tibble(xc = xc) %>% 
  tibble::rowid_to_column("group_id") %>% 
  ggplot(mapping = aes(x = xc, y = 0)) +
  geom_point(data = data.frame(xc = 0),
             mapping = aes(x = xc, y = 0),
             size = 6, shape = 15, color = "black", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "grey50", size = 1.25) +
  geom_point(size = 7,
             mapping = aes(color = as.factor(group_id))) +
  ggthemes::scale_color_colorblind("Group") +
  theme_bw()

### set the group standard deviations, hard code for now
sdj <- 0.5

### generate the random observations - 15 total random observations
n_obs <- 15

### need to decide which group each of the random observations belongs to

### for right now, use a random assignment, sample WITH replacement
set.seed(9871)
obs_in_groups <- sample(1:n_groups, size = n_obs, replace = TRUE)

obs_in_groups

### generate the random observations associaetd with each group
set.seed(812731)
x_obs <- rnorm(n = n_obs, mean = xc[obs_in_groups], sd = sdj)

x_obs

### visualize the random observations with the cluster centers
df_obs <- tibble::tibble(
  x = x_obs,
  group_id = obs_in_groups
)

df_obs %>% 
  ggplot(mapping = aes(x = x, y = 0)) +
  geom_point(data = tibble::tibble(x = xc) %>% tibble::rowid_to_column("group_id"),
             mapping = aes(x = x, y = 0, color = as.factor(group_id)),
             size = 7, alpha = 0.5) +
  geom_point(size = 3, shape = 16,
             mapping = aes(color = as.factor(group_id))) +
  ggthemes::scale_color_colorblind("Group") +
  theme_bw()
