### practice 1D multi-level model for generating random locations
### around group centers with group-specific means AND group-specific
### standard deviations

### define group-level MEAN population and group-level SD popluation 
### through the log-transform

library(dplyr)
library(ggplot2)

### setup the group-level popluation information

n_groups <- 9

mu0 <- 0
sd0 <- 1

psi0 <- log(0.1)
xi0 <- 1

### generate the group MEANS and the group SDs
set.seed(71231)
muj <- rnorm(n = n_groups, mean = mu0, sd = sd0)
sdj <- exp(rnorm(n = n_groups, mean = psi0, sd = xi0))

muj
sdj

### the relative group-specific sd compared to the group-population sd
sdj / sd0

### visualize the separate group densities relative to the
### group-center populations
xviz0 <- seq(mu0 - 4*sd0, mu0 + 4*sd0, length.out = 2001)
dviz0 <- dnorm(x = xviz0, mean = mu0, sd = sd0)

fviz_dist_j <- function(mj, sj, gj, nj)
{
  xj <- seq(mj - 4*sj, mj + 4*sj, length.out = nj)
  
  dj <- dnorm(x = xj, mean = mj, sd = sj)
  
  tibble::tibble(x = xj, norm_pdf = dj) %>% 
    mutate(group_id = gj,
           mu_j = mj,
           sd_j = sj)
}

viz_group_dens <- purrr::pmap_dfr(list(muj, sdj, 1:n_groups),
                                  fviz_dist_j,
                                  nj = 2001)

### compare the group-level population distribution to the
### group-specific distributions
viz_group_dens %>% 
  ggplot(mapping = aes(x = x, y = norm_pdf)) +
  # scale the population density for viewing purposes
  geom_line(data = data.frame(x = xviz0, norm_pdf = dviz0),
            color = "black", size = 1.45, alpha = 0.5,
            mapping = aes(x = x, y = 10 * norm_pdf)) +
  geom_vline(xintercept = c(mu0 - sd0, mu0 + sd0),
             color = "black", linetype = "dashed", alpha = 0.5,
             size = 1.45) +
  geom_line(mapping = aes(group = group_id,
                          color = as.factor(group_id)),
            size = 1.2) +
  # scale_color_discrete("ID", l = 45) +
  scale_color_brewer("ID", palette = "Set1") +
  theme_bw()

### compare the group-specific distributions to
### group-level summaries
viz_group_dens %>% 
  ggplot(mapping = aes(x = x, y = norm_pdf)) +
  # scale the population density for viewing purposes
  geom_vline(xintercept = c(mu0 - sd0, mu0 + sd0),
             color = "black", linetype = "dashed", alpha = 0.5,
             size = 1.45) +
  geom_vline(xintercept = c(mu0 - 2*sd0, mu0 + 2*sd0),
             color = "black", linetype = "dotted", alpha = 0.5,
             size = 1.45) +
  geom_vline(xintercept = mu0,
             color = "black", alpha = 0.5, size = 1.45) +
  geom_line(mapping = aes(group = group_id,
                          color = as.factor(group_id)),
            size = 1.2) +
  # scale_color_discrete("ID", l = 45) +
  scale_color_brewer("ID", palette = "Set1") +
  theme_bw()

### allocate the GROUPS to each of the observations
n_obs <- 100

set.seed(12313)
obs_in_groups <- sample(1:n_groups, size = n_obs, replace = TRUE)

sort(unique(obs_in_groups))

data.frame(group_id = obs_in_groups) %>% 
  count(group_id)

### generate the random observations associated within each group
set.seed(9113)
xobs <- rnorm(n = n_obs, mean = muj[obs_in_groups], sd = sdj[obs_in_groups])

### grouping and counting

df <- tibble::tibble(
  x = xobs,
  group_id = obs_in_groups
)

df %>% 
  group_by(group_id) %>% 
  summarise(N_j = n(),
            mu_j = mean(x),
            sd_j = sd(x))

### GRAND-AVERAGE may not necessarily represent the 
### naive average of the observations
mean(df$x) ### naive estimate -- sample average of all observations

### precision weighted grand average accounting for the
### within group dispersion
df %>% 
  group_by(group_id) %>% 
  summarise(N_j = n(),
            mu_j = mean(x),
            sd_j = sd(x),
            prec_j = 1 / var(x)) %>% 
  ungroup() %>% 
  mutate(prec_mu_j = prec_j * mu_j) %>% 
  summarise(sum(prec_mu_j) / sum(prec_j))

### visualize the distributions in each group
df %>% 
  ggplot(mapping = aes(x = x)) +
  geom_vline(xintercept = mu0,
             color = "black", linetype = "dotted", size = 1.25) +
  geom_vline(data.frame(mu_j = muj) %>% tibble::rowid_to_column("group_id"),
             mapping = aes(xintercept = mu_j,
                           color = as.factor(group_id)),
             size = 1.35, alpha = 0.5) +
  geom_freqpoly(mapping = aes(color = as.factor(group_id),
                              group = group_id,
                              y = stat(density)),
                bins = 55, size = 1.12) +
  # scale_color_discrete("ID", l = 45) +
  scale_color_brewer("ID", palette = "Set1") +
  theme_bw()


