### repeat our multilevel (hierarchical) model example (the "8 schools"
### formulation) but with rstanarm

library(dplyr)
library(ggplot2)

### generate the data as we did before
tau_true <- 5
mu_true <- 55

### number of the groups
num_groups <- 8

### randomly generate the group means (the thetas)
set.seed(6123)
theta_true <- rnorm(n = num_groups, mean = mu_true, sd = tau_true)

theta_true

### the KNONWN likelihood (or observation level) noise, controls
### the spread of the observations around each GROUP mean
lik_noise <- 0.8 ### (SIGMA)

### number of TOTAL observations
num_obs <- 70

### randomly assign the observations to each GROUP
### it's a simple way to quickly yield non-uniform sample
### size per group
set.seed(81231)
obs_in_group <- sample(1:num_groups, size = num_obs, replace = TRUE)

### generate the random observations
set.seed(1231)
y <- rnorm(n = num_obs, mean = theta_true[obs_in_group], sd = lik_noise)

### package everything together
df <- tibble::tibble(
  y = y,
  J = obs_in_group
)

df %>% count(J)

### take a lot at the random observations, and include boxplots
### for simple summary stats
df %>% 
  ggplot(mapping = aes(x = as.factor(J), y = y)) +
  geom_boxplot() +
  geom_point(size = 3, alpha = 0.5) +
  theme_bw()

### calculate summary stats based on the samples
group_df <- df %>% 
  group_by(J) %>% 
  summarise(nj = n(),
            ybarj = mean(y),
            sdy = sd(y)) %>% 
  ungroup() %>% 
  mutate(sdj = lik_noise / sqrt(nj))

group_df

### we will NOT assume that the likelihood noise is known
### we will need to work with the individual observations
### INSTEAD of the grouped sufficient statistics

### make a few adjustments to the observation level

df_b <- df %>% 
  tibble::rowid_to_column("obs_id") %>% 
  mutate(j_id = as.factor(J))

df_b %>% count(j_id)

levels(df_b$j_id)

### use rstanarm to fit the model (sample the posterior)

library(rstanarm)

### the "8 schools" formulation is a VARIANCE COMPONENTS model
### or a varying intercept without predictors model OR
### partial pooling

mod_partial_pool <- stan_lmer(formula = y ~ 1 + (1 | j_id),
                              data = df_b,
                              seed = 724212)

mod_partial_pool

summary(mod_partial_pool)

plot(mod_partial_pool)

### we can plot the group specific terms only
mod_partial_pool$coefficients

names(mod_partial_pool$coefficients)

plot(mod_partial_pool, pars = names(mod_partial_pool$coefficients)[-1])

### the model is a stanreg object
class(mod_partial_pool)

### to access the posterior samples we have to convert from the
### stanreg object to a data.frame
as.data.frame(mod_partial_pool) %>% tbl_df() %>% glimpse()

### we can make custom plots

### for exmaple, create posterior histograms for the different parameters
as.data.frame(mod_partial_pool) %>% tbl_df() %>% 
  tibble::rowid_to_column("post_id") %>% 
  tidyr::gather(key = "key", value = "value", -post_id) %>% 
  ggplot(mapping = aes(x = value)) +
  geom_histogram(bins = 55) +
  facet_wrap(~key, scales = "free") +
  theme_bw() +
  theme(axis.text.y = element_blank())

### let's get the posterior in the formulation we used for the laplace
### approximation, so instead of having the group "relative effect" 
### from the grand average, let's get the individual group means
### or the theta parameters

### easiest way is through prediction
pred_group <- df_b %>% distinct(j_id) %>% arrange(j_id)

pred_group

post_pred <- posterior_predict(mod_partial_pool, pred_group)

post_pred %>% class()

post_pred %>% dim()

### visualize the posterior (Predictive) distributions on the
### group means
post_pred %>% as.data.frame() %>% tbl_df() %>% 
  tibble::rowid_to_column("post_id") %>% 
  tidyr::gather(key = "key", value = "value", -post_id) %>% 
  ggplot(mapping = aes(x = value)) +
  geom_histogram(bins = 55) +
  geom_vline(data = data.frame(key = as.character(1:8),
                               true_value = theta_true),
             mapping = aes(xintercept = true_value),
             color = "red", linetype = "dashed") +
  facet_wrap(~key, labeller = label_bquote(.(sprintf("theta: %s", key)))) +
  theme_bw() +
  theme(axis.text.y = element_blank())

### look at the posteriors as boxplots
post_pred %>% as.data.frame() %>% tbl_df() %>% 
  tibble::rowid_to_column("post_id") %>% 
  tidyr::gather(key = "key", value = "value", -post_id) %>% 
  ggplot(mapping = aes(x = key, y = value)) +
  geom_boxplot() +
  geom_point(data = data.frame(key = as.character(1:8),
                               true_value = theta_true),
             mapping = aes(x = key, y = true_value),
             color = "red", shape = 4, size = 4) +
  theme_bw()

### posterior correlation
post_pred %>% as.data.frame() %>% tbl_df() %>% 
  cor() %>% 
  corrplot::corrplot(type = "upper", method = "number")

### scatter plot to check
post_pred %>% as.data.frame() %>% tbl_df() %>% 
  ggplot(mapping = aes(x = `1`, y = `2`)) +
  geom_point(alpha = 0.2) +
  theme_bw()

### look at the posterior correlation for all parameeters including
### the sigmas
as.data.frame(mod_partial_pool) %>% tbl_df() %>% 
  cor() %>% 
  corrplot::corrplot(type = "upper", method = "square")

### the priors

prior_summary(mod_partial_pool)

### compare the posterior to the prior just for the group specific terms

posterior_vs_prior(mod_partial_pool,
                   pars = names(mod_partial_pool$coefficients)[-1]) +
  ggplot2::scale_color_discrete(guide = FALSE)

### or we can compare the posterior and prior directly per parameter
posterior_vs_prior(mod_partial_pool,
                   pars = names(mod_partial_pool$coefficients)[-1],
                   group_by_parameter = TRUE) +
  ggplot2::scale_color_discrete(guide = FALSE) +
  theme_bw()

### no-pooling - do NOT use a hierarchical structure...which means
### essentially a linear model

### the group variable will therefore just a single predictor
### with 1 level per group

### let's look at the design matrix

model.matrix( y ~ j_id, data = df_b) %>% head()

df_b %>% head()

### sample the regular linear model
mod_no_pool <- stan_glm(y ~ j_id, data = df_b,
                        prior = normal(location = 0, scale = 10),,
                        prior_aux = exponential(rate = 1),
                        seed = 23131)

mod_no_pool

summary(mod_no_pool)

prior_summary(mod_no_pool)

plot(mod_no_pool, pars = names(mod_no_pool$coefficients))

plot(mod_no_pool, pars = names(mod_no_pool$coefficients)[-1])

### posteriors on the unknown group means, use posterior predictions
post_pred_no_pool <- posterior_predict(mod_no_pool, pred_group)

post_pred_no_pool %>% as.data.frame() %>% tbl_df() %>% 
  tibble::rowid_to_column("post_id") %>% 
  tidyr::gather(key = "key", value = "value", -post_id) %>% 
  ggplot(mapping = aes(x = value)) +
  geom_histogram(bins = 55) +
  geom_vline(data = data.frame(key = as.character(1:8),
                               true_value = theta_true),
             mapping = aes(xintercept = true_value),
             color = "red", linetype = "dashed") +
  facet_wrap(~key, labeller = label_bquote(.(sprintf("theta: %s", key)))) +
  theme_bw() +
  theme(axis.text.y = element_blank())

### compare with the partial pooling case
post_pred %>% as.data.frame() %>% tbl_df() %>% 
  tibble::rowid_to_column("post_id") %>% 
  mutate(post_type = "partial pooling") %>% 
  bind_rows(post_pred_no_pool %>% as.data.frame() %>% tbl_df() %>% 
              tibble::rowid_to_column("post_id") %>% 
              mutate(post_type = "no pooling")) %>% 
  tidyr::gather(key = "key", value = "value", -post_id, -post_type) %>% 
  ggplot(mapping = aes(x = value)) +
  geom_freqpoly(mapping = aes(color = post_type),
                size = 1.1,
                bins = 55) +
  facet_wrap(~key, scales = "free") +
  ggthemes::scale_color_colorblind("") +
  theme_bw() +
  theme(axis.text.y = element_blank())

### compare using boxplots
post_pred %>% as.data.frame() %>% tbl_df() %>% 
  tibble::rowid_to_column("post_id") %>% 
  mutate(post_type = "partial pooling") %>% 
  bind_rows(post_pred_no_pool %>% as.data.frame() %>% tbl_df() %>% 
              tibble::rowid_to_column("post_id") %>% 
              mutate(post_type = "no pooling")) %>% 
  tidyr::gather(key = "key", value = "value", -post_id, -post_type) %>% 
  ggplot(mapping = aes(x = key, y = value)) +
  geom_boxplot(mapping = aes(fill = post_type,
                             color = post_type),
               alpha = 0.45) +
  geom_point(data = data.frame(key = as.character(1:8),
                               true_value = theta_true),
             mapping = aes(x = key, y = true_value),
             shape = 4, color = "red", size = 5) +
  geom_point(data = group_df %>% 
               mutate(key = factor(J)),
             mapping = aes(x = key,
                           y = ybarj),
             color = "black", shape = 0, size = 3) +
  ggthemes::scale_fill_colorblind("") +
  ggthemes::scale_color_colorblind("") +
  theme_bw()

