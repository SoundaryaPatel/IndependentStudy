### practice generating random betas

library(dplyr)
library(ggplot2)

set.seed(123123)
x <- rbeta(n = 5000, shape1 = 0.5, shape2 = 0.5)

ggplot(data = data.frame(x = x),
       mapping = aes(x = x)) +
  geom_histogram(bins = 55,
                 mapping = aes(y = stat(density))) +
  geom_density(color = "red", size = 1.15) +
  theme_bw()


set.seed(123123)
z <- rbeta(n = 5000, shape1 = 4, shape2 = 12)

ggplot(data = data.frame(x = z),
       mapping = aes(x = x)) +
  geom_histogram(bins = 55,
                 mapping = aes(y = stat(density))) +
  geom_density(color = "red", size = 1.15) +
  theme_bw()

tibble::tibble(x = x, z = z) %>% 
  tibble::rowid_to_column() %>% 
  tidyr::gather(key = "key", value = "value", -rowid) %>% 
  ggplot(mapping = aes(x = value)) +
  geom_density(mapping = aes(color = key,
                             group = key),
               size = 1.15) +
  ggthemes::scale_color_calc("") +
  theme_bw()

### go through generating the random observations for a single dimension

n_groups <- 5

### specify the GROUP distribution hyper-parameters
a0 <- 1.2
b0 <- 1.2

### look at the distribution of the GROUP population
tibble::tibble(
  x = seq(1e-2, 0.99, length.out = 1001)
) %>% 
  mutate(beta_pdf = dbeta(x = x, shape1 = a0, shape2 = b0)) %>% 
  ggplot(mapping = aes(x = x, y = beta_pdf)) +
  geom_vline(xintercept = c(0, 1),
             color = "grey50", size = 1.2) +
  geom_hline(yintercept = 0, color = "grey50", size = 1.25) +
  geom_line(size = 1.15, color = "black") +
  theme_bw()

### specify the within-GROUP variance ratio hyperparameter
r <- 0.02/10

### GENERATE the random draws from the GROUP population
set.seed(12121)
xc <- rbeta(n = n_groups, shape1 = a0, shape2 = b0)

### take a look at the randomly distributed groups
tibble::tibble(xc = xc) %>% 
  tibble::rowid_to_column("group_id") %>% 
  ggplot(mapping = aes(x = xc, y = 0)) +
  geom_vline(xintercept = c(0, 1),
             color = "grey50", size = 1.2) +
  geom_hline(yintercept = 0, color = "grey50", size = 1.25) +
  geom_point(size = 7,
             mapping = aes(color = as.factor(group_id))) +
  ggthemes::scale_color_colorblind("Group") +
  coord_cartesian(xlim = c(-0.05, 1.05)) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  theme_bw()

### calculate the group-specific alpha and beta parameters

### start by calculating the "sample size"
vc <- 1 / r - 1

### now use the randomly generated group-specific MEANS
### and the group-specific "sample size" to calculate alpha and beta
alpha_j <- xc * vc
beta_j <- (1 - xc) * vc

alpha_j

beta_j

### check the MEAN values relative to the randomly generated xc locations
alpha_j / ( alpha_j + beta_j )

xc

### check the group-specific variance
r * (xc * (1 - xc))

### generate the random observations - 15 total random observations
n_obs <- 15

### need to decide which group each of the random observations belongs to

### for right now, use a random assignment, sample WITH replacement
set.seed(9871)
obs_in_groups <- sample(1:n_groups, size = n_obs, replace = TRUE)

obs_in_groups

### use the observation in the group level assignment to specify the
### appropriate group-specific parameters
alpha_j

alpha_j[obs_in_groups]

### generate the random observations associated with each group
set.seed(45131)
x_obs <- rbeta(n = n_obs, shape1 = alpha_j[obs_in_groups], shape2 = beta_j[obs_in_groups])

### visualize the random observations with the cluster centers
df_obs <- tibble::tibble(
  x = x_obs,
  group_id = obs_in_groups
)

df_obs %>% 
  ggplot(mapping = aes(x = x, y = 0)) +
  geom_vline(xintercept = c(0, 1),
             color = "grey50", size = 1.2) +
  geom_hline(yintercept = 0, color = "grey50", size = 1.25) +
  geom_point(data = tibble::tibble(x = xc) %>% tibble::rowid_to_column("group_id"),
             mapping = aes(x = x, y = 0, color = as.factor(group_id)),
             size = 7, alpha = 0.5) +
  geom_point(size = 3, shape = 16,
             mapping = aes(color = as.factor(group_id))) +
  ggthemes::scale_color_colorblind("Group") +
  coord_cartesian(xlim = c(-0.05, 1.05), ylim = c(-0.025, 0.025)) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  theme_bw()

### how many observations in each group?
df_obs %>% 
  count(group_id)

### generate thousands of observations in total
set.seed(9871)
obs_in_groups_big <- sample(1:n_groups, size = 1e4, replace = TRUE)

### generate the random observations associated with each group
set.seed(45131)
x_obs_big <- rbeta(n = length(obs_in_groups_big), 
                   shape1 = alpha_j[obs_in_groups_big], 
                   shape2 = beta_j[obs_in_groups_big])

df_obs_big <- tibble::tibble(
  x = x_obs_big,
  group_id = obs_in_groups_big
)

df_obs_big %>% 
  count(group_id)

### visualize the distribution of the observations per group
df_obs_big %>% 
  ggplot(mapping = aes(x = x)) +
  geom_vline(xintercept = c(0, 1),
             color = "grey50", size = 1.2) +
  geom_hline(yintercept = 0, color = "grey50", size = 1.25) +
  geom_point(data = tibble::tibble(x = xc) %>% tibble::rowid_to_column("group_id"),
             mapping = aes(x = x, y = 0, color = as.factor(group_id)),
             size = 7, alpha = 0.5) +
  geom_density(mapping = aes(color = as.factor(group_id)),
               size = 1.55) +
  ggthemes::scale_color_colorblind("Group") +
  coord_cartesian(xlim = c(-0.05, 1.05)) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  theme_bw()

### break up by group
df_obs_big %>% 
  ggplot(mapping = aes(x = x)) +
  geom_vline(xintercept = c(0, 1),
             color = "grey50", size = 1.2) +
  geom_hline(yintercept = 0, color = "grey50", size = 1.25) +
  geom_point(data = tibble::tibble(x = xc) %>% tibble::rowid_to_column("group_id"),
             mapping = aes(x = x, y = 0, color = as.factor(group_id)),
             size = 7, alpha = 0.5) +
  geom_density(mapping = aes(color = as.factor(group_id)),
               size = 1.55) +
  ggthemes::scale_color_colorblind("Group") +
  coord_cartesian(xlim = c(-0.05, 1.05)) +
  facet_wrap(~group_id, scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  theme_bw()

df_obs_big %>% 
  ggplot(mapping = aes(x = x)) +
  # geom_vline(xintercept = c(0, 1),
  #            color = "grey50", size = 1.2) +
  # geom_hline(yintercept = 0, color = "grey50", size = 1.25) +
  geom_point(data = tibble::tibble(x = xc) %>% tibble::rowid_to_column("group_id"),
             mapping = aes(x = x, y = 0, color = as.factor(group_id)),
             size = 7, alpha = 0.5) +
  geom_density(mapping = aes(color = as.factor(group_id)),
               size = 1.55) +
  ggthemes::scale_color_colorblind("Group") +
  # coord_cartesian(xlim = c(-0.05, 1.05)) +
  facet_wrap(~group_id, scales = "free") +
  # scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  theme_bw()
