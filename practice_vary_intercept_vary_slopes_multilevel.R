### generate synthetic data for a multilevel model with group
### specific intercepts and slopes for continuous responses

library(dplyr)
library(ggplot2)

### specify the parameters of the group specific mean hyperprior
m0 <- 0
s0 <- 2.5

### specify the number of groups
J <- 11

### specify the total number of predictors associated with each
### observation
P <- 2

### the total number of columns in the design matrix is then
### the number of predictors plus 1 to account for the intercept

### the total number of columns is then (K)
num_p <- P + 1

### generate the group specific prior means
set.seed(9000123)
mu_b <- rnorm(n = num_p, m0, s0)

### set the group-specific prior covariance matrix, do this by
### setting the true group-specific prior standard deviations and
### the group-specific correlation matrix

### the group-specific prior standard deviation is the variation
### in the regression coefficients across the groups
prior_tau_b <- c(3.5, 1, 0.25)

### the group-specific correlation matrix represents how correlated
### the regression coefficients are across the groups
rho_01 <- 0.2
rho_02 <- -0.2
rho_12 <- -0.7

prior_corr_b <- matrix(c(1, rho_01, rho_02, rho_01, 1, rho_12, rho_02, rho_12, 1),
                       nrow = 3, byrow = TRUE)

prior_corr_b

prior_sigma_b <- diag(prior_tau_b) %*% prior_corr_b %*% diag(prior_tau_b)

prior_sigma_b

### generate the group-specific regression coefficients
set.seed(81231)
beta_groups <- MASS::mvrnorm(n = J, mu = mu_b, Sigma = prior_sigma_b)

beta_groups

### randomly generate the group assignments for each observation
### assuming each group is uniformly likely

### total number of observations
N <- 125

set.seed(389341)
jj <- sample(1:J, size = N, replace = TRUE)

### generate the inputs assuming independent standard normals
### these inputs are at the observation level
set.seed(912312)
x_inputs <- MASS::mvrnorm(n = N, mu = rep(0, P), Sigma = diag(P))

colnames(x_inputs)

### include the intercept column of 1s
Xmat <- cbind(rep(1, N), x_inputs)

Xmat %>% head()

### now calculate the linear predictor for all observations accounting
### for the group assignment for each observation
jj %>% head()

beta_groups

beta_groups[jj[1:3], ]

dim(beta_groups[jj, ])

### define a function which allows looping over all of the observations
### and calculating the linear predictor while accounting for the
### grouping structure
calc_linpred <- function(n, jid, X, B)
{
  as.numeric(X[n, ] %*% as.matrix(B[jid[n], ]))
}

### check
calc_linpred(1, jj, Xmat, beta_groups)

Xmat[1, ] %*% as.matrix(beta_groups[jj[1], ])

### loop over all observations
true_linpred <- purrr::map_dbl(seq_along(jj),
                               calc_linpred,
                               jid = jj,
                               X = Xmat,
                               B = beta_groups)

### specify the likelihood noise to be something relatively low for
### now 
true_liknoise <- 0.15

### the noisy continuous observations
set.seed(44314139)
y_noisy <- rnorm(n = N, mean = true_linpred, sd = true_liknoise)

### package together
df <- x_inputs %>% as.data.frame() %>% tbl_df() %>% 
  purrr::set_names(sprintf("x_%02d", 1:ncol(x_inputs))) %>% 
  mutate(jid = jj,
         f = true_linpred,
         y = y_noisy)

### visualize the true linear predictor with respect to each input
### broken up by the groups
df %>% 
  tibble::rowid_to_column("obs_id") %>% 
  tidyr::gather(key = "key", value = "value",
                -obs_id, -jid, -f, -y) %>% 
  ggplot(mapping = aes(x = value, y = f)) +
  geom_point() +
  facet_grid(key ~ jid) +
  theme_bw()

### define a fine grid in the inputs to help visualize the true
### linear predictor trends, repeat that 
input_grid <- expand.grid(x_01 = seq(-3, 3, length.out = 11),
                          x_02 = seq(-3, 3, length.out = 11),
                          jid = 1:J,
                          KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>% 
  as.data.frame() %>% tbl_df()

Xgrid <- input_grid %>% select(starts_with("x_")) %>% as.matrix()

colnames(Xgrid) <- NULL

head(Xgrid)

grid_linpred <- purrr::map_dbl(seq_along(input_grid$jid),
                               calc_linpred,
                               jid = input_grid$jid,
                               X = cbind(rep(1, nrow(Xgrid)), Xgrid),
                               B = beta_groups)

input_grid_b <- input_grid %>% 
  mutate(f = grid_linpred)

### visualize the true linear predictor trends with the randomly generated
### observations

### first consider the trends with repsect to x1
df %>% 
  ggplot(mapping = aes(x = x_01, y = f)) +
  geom_line(data = input_grid_b,
            mapping = aes(group = interaction(jid, x_02)),
            color = "blue", alpha = 0.5) +
  geom_point() +
  facet_wrap(~jid, labeller = "label_both") +
  theme_bw()

### next consider the trends with respect to x2
df %>% 
  ggplot(mapping = aes(x = x_02, y = f)) +
  geom_line(data = input_grid_b,
            mapping = aes(group = interaction(jid, x_01)),
            color = "blue", alpha = 0.5) +
  geom_point() +
  facet_wrap(~jid, labeller = "label_both") +
  theme_bw()

### look at the coefficient values across the groups
beta_groups %>% 
  as.data.frame() %>% tbl_df() %>% 
  purrr::set_names(sprintf("beta_%d", 1:ncol(beta_groups) - 1)) %>% 
  tibble::rowid_to_column("jid") %>% 
  tidyr::gather(key = "key", value = "value", -jid) %>% 
  tidyr::separate(key,
                  c("beta_word", "beta_index"),
                  sep = "_") %>% 
  ggplot(mapping = aes(x = as.factor(jid), y = value)) +
  geom_hline(yintercept = 0, color = "grey50", size = 2) +
  geom_point(size = 4,
             mapping = aes(color = beta_index,
                           shape = beta_index),
             position = position_dodge(0.1)) +
  ggthemes::scale_color_calc("beta index") +
  scale_shape_discrete("beta index", solid = FALSE) +
  theme_bw()

### scatter plot between the regression coefficients
beta_groups %>% 
  as.data.frame() %>% tbl_df() %>% 
  purrr::set_names(sprintf("beta_%d", 1:ncol(beta_groups) - 1)) %>% 
  tibble::rowid_to_column("jid") %>% 
  ggplot(mapping = aes(x = beta_1, y = beta_2)) +
  geom_point(mapping = aes(color = as.factor(jid)),
             size = 4.5) +
  scale_color_viridis_d("jid") +
  theme_bw()

### check the number of observations per group
df %>% 
  ggplot(mapping = aes(x = as.factor(jid))) +
  geom_bar() +
  theme_bw()

### look at the scatter plot of the noisy observations
### with respect to the two inputs
df %>% 
  tibble::rowid_to_column("obs_id") %>% 
  tidyr::gather(key = "key", value = "value", -obs_id, -jid, -f, -y) %>% 
  ggplot(mapping = aes(x = value, y = y)) +
  geom_point(alpha = 0.5) +
  facet_grid(. ~ key) +
  theme_bw()

### color by the groups
df %>% 
  tibble::rowid_to_column("obs_id") %>% 
  tidyr::gather(key = "key", value = "value", -obs_id, -jid, -f, -y) %>% 
  ggplot(mapping = aes(x = value, y = y)) +
  geom_point(alpha = 0.5, size = 2,
             mapping = aes(color = as.factor(jid))) +
  facet_grid(. ~ key) +
  scale_color_viridis_d("jid") +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(alpha = 1., size = 2)))

### use rstanarm to fit the multilevel model
df_b <- df %>% 
  mutate(j = as.factor(jid))

library(rstanarm)

fit_01 <- stan_lmer(y ~ 1 + x_01 + x_02 + (1 + x_01 + x_02 | j), df_b,
                    prior_intercept = normal(location = 0, scale = 2),
                    prior_covariance = decov(concentration = 0.5, shape = 2, scale = 2),
                    prior_aux = cauchy(),
                    adapt_delta = 0.995,
                    iter = 3000, warmup = 1000,
                    seed = 412412)

summary(fit_01)

coef(fit_01)

### posterior summary of the fixe effects
summary(fit_01, pars = c("(Intercept)", "x_01", "x_02", "sigma"))

### plot the posterior summaries of the fixed effects
plot(fit_01, pars = c("(Intercept)", "x_01", "x_02", "sigma"))

### plot the posterior summaries for the group specific terms
### the random effects
plot(fit_01, regex_pars = "^[b]")

### look at the fixed effects point estimates
fixef(fit_01)

### look at the random effects point estimates
ranef(fit_01)

### look at the posterior on the hierarchical prior covariance matrix
plot(fit_01, regex_pars = "Sigma")

### look at the posterior on the diagonal terms of the hierarchical
### prior covariance matrix
plot(fit_01, pars = c("Sigma[j:(Intercept),(Intercept)]", "Sigma[j:x_01,x_01]", 
                      "Sigma[j:x_02,x_02]"))

### compare to the true values
prior_sigma_b %>% diag()

### calculate the group specific coefficients by adding in the
### fixed effects to the random effects

### start with the varying intercepts
as.data.frame(fit_01) %>% tbl_df() %>% 
  select(names(fixef(fit_01)), matches("^[b]")) %>% 
  select(contains("Intercept")) %>% 
  tibble::rowid_to_column("post_id") %>% 
  tidyr::gather(key = "key", value = "value", -post_id, -`(Intercept)`) %>% 
  mutate(jid = as.integer(stringr::str_extract(key, "\\d+"))) %>% 
  mutate(beta_0 = `(Intercept)` + value) %>% 
  group_by(jid) %>% 
  summarise(num_post = n(),
            post_q50 = median(beta_0),
            post_avg = mean(beta_0)) %>% 
  ungroup()

### the posterior samples on the fixed effects
fe_lf <- as.data.frame(fit_01) %>% tbl_df() %>% 
  select(names(fixef(fit_01))) %>% 
  tibble::rowid_to_column("post_id") %>% 
  tidyr::gather(key = "fixed_effect_name", value = "fixed_effect_value", -post_id)

### the random effect posterior samples
re_lf <- as.data.frame(fit_01) %>% tbl_df() %>% 
  select(matches("^[b]")) %>% 
  tibble::rowid_to_column("post_id") %>% 
  tidyr::gather(key = "key", value = "random_effect_value", -post_id) %>% 
  tidyr::separate(key,
                  c("b_left", "b_right"),
                  sep = "\\s") %>% 
  tidyr::separate(b_left,
                  c("b_bracket", "fixed_effect_name"),
                  sep = "\\[") %>% 
  mutate(jid = as.integer(stringr::str_extract(b_right, "\\d+"))) %>% 
  select(post_id, fixed_effect_name, jid, random_effect_value)

### join the fixed effect values with the random effect values per group
### and calculae the group specific term then summarize over all post samples
post_summary_group_terms <- re_lf %>% 
  left_join(fe_lf, by = c("post_id", "fixed_effect_name")) %>% 
  mutate(value = fixed_effect_value + random_effect_value) %>% 
  group_by(fixed_effect_name, jid) %>% 
  summarise(num_post = n(),
            q05 = quantile(value, 0.05),
            q25 = quantile(value, 0.25),
            avg = mean(value),
            q50 = median(value),
            q75 = quantile(value, 0.75),
            q95 = quantile(value, 0.95)) %>% 
  ungroup()

post_summary_group_terms

### compare the group specific terms posterior summaries
post_summary_group_terms %>% 
  ggplot(mapping = aes(x = as.factor(jid))) +
  geom_linerange(mapping = aes(ymin = q05, ymax = q95,
                               group = interaction(jid, fixed_effect_name),
                               color = fixed_effect_name),
                 size = 1.2,
                 position = position_dodge(0.15)) +
  geom_linerange(mapping = aes(ymin = q25, ymax = q75,
                               group = interaction(jid, fixed_effect_name),
                               color = fixed_effect_name),
                 size = 2.2,
                 position = position_dodge(0.15)) +
  geom_point(mapping = aes(y = avg,
                           group = interaction(jid, fixed_effect_name),
                           color = fixed_effect_name),
             size = 3.5,
             position = position_dodge(0.15)) +
  ggthemes::scale_color_calc("parameter type") +
  labs(y = "value") +
  theme_bw() +
  theme(legend.position = "top")

### include the true group specific coefficient values that generated
### the synthetic data
post_summary_group_terms %>% 
  ggplot(mapping = aes(x = as.factor(jid))) +
  geom_linerange(mapping = aes(ymin = q05, ymax = q95,
                               group = interaction(jid, fixed_effect_name),
                               color = fixed_effect_name),
                 size = 1.2,
                 position = position_dodge(0.15)) +
  geom_linerange(mapping = aes(ymin = q25, ymax = q75,
                               group = interaction(jid, fixed_effect_name),
                               color = fixed_effect_name),
                 size = 2.2,
                 position = position_dodge(0.15)) +
  geom_point(mapping = aes(y = avg,
                           group = interaction(jid, fixed_effect_name),
                           color = fixed_effect_name),
             size = 3.5,
             position = position_dodge(0.15)) +
  geom_point(data = beta_groups %>% 
               as.data.frame() %>% tbl_df() %>% 
               purrr::set_names(c("(Intercept)", "x_01", "x_02")) %>% 
               tibble::rowid_to_column("jid") %>% 
               tidyr::gather(key = "key", value = "true_value", -jid),
             mapping = aes(x = as.factor(jid), y = true_value,
                           shape = key),
             size = 4, color = "black",
             position = position_dodge(0.15)) +
  ggthemes::scale_color_calc("parameter type") +
  scale_shape_discrete("parameter type", solid = FALSE) +
  labs(y = "value") +
  theme_bw() +
  theme(legend.position = "top")

### create a prediction grid to visualize the posterior predictive trends
pred_input_grid <- expand.grid(x_01 = -3:3,
                               x_02 = -3:3,
                               jid = 1:J,
                               KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>% 
  as.data.frame() %>% tbl_df()

pred_input_grid_b <- pred_input_grid %>% 
  mutate(j = as.factor(jid))

### make posterior predictions, and summarize over the posterior
### samples at prediction point
post_pred_01 <- posterior_predict(fit_01, pred_input_grid_b)

post_pred_summary_01 <- post_pred_01 %>% as.data.frame() %>% tbl_df() %>% 
  tibble::rowid_to_column("post_id") %>% 
  tidyr::gather(key = "pred_id", value = "pred_value", -post_id) %>% 
  mutate_at(c("pred_id"), as.integer) %>% 
  group_by(pred_id) %>% 
  summarise(num_post = n(),
            q05 = quantile(pred_value, 0.05),
            q25 = quantile(pred_value, 0.25),
            avg = mean(pred_value),
            q50 = median(pred_value),
            q75 = quantile(pred_value, 0.75),
            q95 = quantile(pred_value, 0.95)) %>% 
  ungroup() %>% 
  left_join(pred_input_grid_b %>% tibble::rowid_to_column("pred_id"),
            by = "pred_id")

### visualize the posterior predictive summaries, start with the
### trends with respect x1
post_pred_summary_01 %>% 
  ggplot(mapping = aes(x = x_01)) +
  geom_ribbon(mapping = aes(ymin = q05, ymax = q95,
                            group = interaction(x_02, j)),
              fill = "dodgerblue", alpha = 0.5) +
  geom_ribbon(mapping = aes(ymin = q25, ymax = q75,
                            group = interaction(x_02, j)),
              fill = "steelblue") +
  geom_line(mapping = aes(y = avg,
                          group = interaction(x_02, j)),
            color = "black") +
  facet_wrap(~j, labeller = "label_both") +
  labs(y = "value") +
  theme_bw()

### include the true linear predictor for comparison
Xpred_grid <- pred_input_grid_b %>% select(starts_with("x_")) %>% as.matrix()

colnames(Xpred_grid) <- NULL

head(Xpred_grid)

pred_grid_linpred <- purrr::map_dbl(seq_along(pred_input_grid_b$jid),
                                    calc_linpred,
                                    jid = pred_input_grid_b$jid,
                                    X = cbind(rep(1, nrow(Xpred_grid)), Xpred_grid),
                                    B = beta_groups)

pred_input_grid_c <- pred_input_grid_b %>% 
  mutate(f_true = pred_grid_linpred)

pred_input_grid_c

### visualize the posteior predictive summaries witht he true values
post_pred_summary_01 %>% 
  ggplot(mapping = aes(x = x_01)) +
  geom_ribbon(mapping = aes(ymin = q05, ymax = q95,
                            group = interaction(x_02, j)),
              fill = "dodgerblue", alpha = 0.5) +
  geom_ribbon(mapping = aes(ymin = q25, ymax = q75,
                            group = interaction(x_02, j)),
              fill = "steelblue") +
  geom_line(mapping = aes(y = avg,
                          group = interaction(x_02, j)),
            color = "black") +
  geom_line(data = pred_input_grid_c,
            mapping = aes(x = x_01, y = f_true,
                          group = interaction(x_02, j)),
            color = "red", linetype = "dashed") +
  facet_wrap(~j, labeller = "label_both") +
  labs(y = "value") +
  theme_bw()

### now visualize the trends with respect to x02
post_pred_summary_01 %>% 
  ggplot(mapping = aes(x = x_02)) +
  geom_ribbon(mapping = aes(ymin = q05, ymax = q95,
                            group = interaction(x_01, j)),
              fill = "dodgerblue", alpha = 0.5) +
  geom_ribbon(mapping = aes(ymin = q25, ymax = q75,
                            group = interaction(x_01, j)),
              fill = "steelblue") +
  geom_line(mapping = aes(y = avg,
                          group = interaction(x_01, j)),
            color = "black") +
  geom_line(data = pred_input_grid_c,
            mapping = aes(x = x_02, y = f_true,
                          group = interaction(x_01, j)),
            color = "red", linetype = "dashed") +
  facet_wrap(~j, labeller = "label_both") +
  labs(y = "value") +
  theme_bw()

### check the bayesian Rsquared for the model
rstanarm::bayes_R2(fit_01) %>% quantile()

### now fit a varying intercept model, but allow the slopes to NOT
### vary by group

fit_02 <- stan_lmer(y ~ 1 + x_01 + x_02 + (1 | j), df_b,
                    prior_intercept = normal(location = 0, scale = 2),
                    prior_covariance = decov(concentration = 0.5, shape = 2, scale = 2),
                    prior_aux = cauchy(),
                    adapt_delta = 0.995,
                    iter = 3000, warmup = 1000,
                    seed = 412412)

summary(fit_02)

### make posterior predictions with the varying intercept only model
post_pred_02 <- posterior_predict(fit_02, pred_input_grid_b)

post_pred_summary_02 <- post_pred_02 %>% as.data.frame() %>% tbl_df() %>% 
  tibble::rowid_to_column("post_id") %>% 
  tidyr::gather(key = "pred_id", value = "pred_value", -post_id) %>% 
  mutate_at(c("pred_id"), as.integer) %>% 
  group_by(pred_id) %>% 
  summarise(num_post = n(),
            q05 = quantile(pred_value, 0.05),
            q25 = quantile(pred_value, 0.25),
            avg = mean(pred_value),
            q50 = median(pred_value),
            q75 = quantile(pred_value, 0.75),
            q95 = quantile(pred_value, 0.95)) %>% 
  ungroup() %>% 
  left_join(pred_input_grid_b %>% tibble::rowid_to_column("pred_id"),
            by = "pred_id")

### visualize the trends of the varying intercept only model and compare
### to the true linear predictor trends
post_pred_summary_02 %>% 
  ggplot(mapping = aes(x = x_01)) +
  geom_ribbon(mapping = aes(ymin = q05, ymax = q95,
                            group = interaction(x_02, j)),
              fill = "dodgerblue", alpha = 0.5) +
  geom_ribbon(mapping = aes(ymin = q25, ymax = q75,
                            group = interaction(x_02, j)),
              fill = "steelblue") +
  geom_line(mapping = aes(y = avg,
                          group = interaction(x_02, j)),
            color = "black") +
  geom_line(data = pred_input_grid_c,
            mapping = aes(x = x_01, y = f_true,
                          group = interaction(x_02, j)),
            color = "red", linetype = "dashed") +
  facet_wrap(~j, labeller = "label_both") +
  labs(y = "value") +
  theme_bw()

post_pred_summary_02 %>% 
  ggplot(mapping = aes(x = x_02)) +
  geom_ribbon(mapping = aes(ymin = q05, ymax = q95,
                            group = interaction(x_01, j)),
              fill = "dodgerblue", alpha = 0.5) +
  geom_ribbon(mapping = aes(ymin = q25, ymax = q75,
                            group = interaction(x_01, j)),
              fill = "steelblue") +
  geom_line(mapping = aes(y = avg,
                          group = interaction(x_01, j)),
            color = "black") +
  geom_line(data = pred_input_grid_c,
            mapping = aes(x = x_02, y = f_true,
                          group = interaction(x_01, j)),
            color = "red", linetype = "dashed") +
  facet_wrap(~j, labeller = "label_both") +
  labs(y = "value") +
  theme_bw()

### check the Bayesian Rsquared for the model
rstanarm::bayes_R2(fit_02) %>% quantile()

### now use a non-hierarchical linear model with the groups
### handled by a categorical variable with dummy variables
### allow for interactions with both continuous variables and
### ALL categorical variables, so this is the CORRECT functional
### form but there is NO POOLING between the groups

### just use stan_lm and use the R2 prior

fit_03 <- stan_lm(y ~ j * (x_01 + x_02), df_b,
                  prior = R2(location = 0.5),
                  adapt_delta = 0.995,
                  iter = 3000, warmup = 1000,
                  seed = 412412)

summary(fit_03)

### make posterior predictions with the no pooling model with the
### correct interactions
post_pred_03 <- posterior_predict(fit_03, pred_input_grid_b)

post_pred_summary_03 <- post_pred_03 %>% as.data.frame() %>% tbl_df() %>% 
  tibble::rowid_to_column("post_id") %>% 
  tidyr::gather(key = "pred_id", value = "pred_value", -post_id) %>% 
  mutate_at(c("pred_id"), as.integer) %>% 
  group_by(pred_id) %>% 
  summarise(num_post = n(),
            q05 = quantile(pred_value, 0.05),
            q25 = quantile(pred_value, 0.25),
            avg = mean(pred_value),
            q50 = median(pred_value),
            q75 = quantile(pred_value, 0.75),
            q95 = quantile(pred_value, 0.95)) %>% 
  ungroup() %>% 
  left_join(pred_input_grid_b %>% tibble::rowid_to_column("pred_id"),
            by = "pred_id")

### visualize the trends of the no pooling model with the correct
### interaction terms
post_pred_summary_03 %>% 
  ggplot(mapping = aes(x = x_01)) +
  geom_ribbon(mapping = aes(ymin = q05, ymax = q95,
                            group = interaction(x_02, j)),
              fill = "dodgerblue", alpha = 0.5) +
  geom_ribbon(mapping = aes(ymin = q25, ymax = q75,
                            group = interaction(x_02, j)),
              fill = "steelblue") +
  geom_line(mapping = aes(y = avg,
                          group = interaction(x_02, j)),
            color = "black") +
  geom_line(data = pred_input_grid_c,
            mapping = aes(x = x_01, y = f_true,
                          group = interaction(x_02, j)),
            color = "red", linetype = "dashed") +
  facet_wrap(~j, labeller = "label_both") +
  labs(y = "value") +
  theme_bw()

post_pred_summary_03 %>% 
  ggplot(mapping = aes(x = x_02)) +
  geom_ribbon(mapping = aes(ymin = q05, ymax = q95,
                            group = interaction(x_01, j)),
              fill = "dodgerblue", alpha = 0.5) +
  geom_ribbon(mapping = aes(ymin = q25, ymax = q75,
                            group = interaction(x_01, j)),
              fill = "steelblue") +
  geom_line(mapping = aes(y = avg,
                          group = interaction(x_01, j)),
            color = "black") +
  geom_line(data = pred_input_grid_c,
            mapping = aes(x = x_02, y = f_true,
                          group = interaction(x_01, j)),
            color = "red", linetype = "dashed") +
  facet_wrap(~j, labeller = "label_both") +
  labs(y = "value") +
  theme_bw()

### check the Bayesian rsquared
rstanarm::bayes_R2(fit_03) %>% quantile()

### now fit a simple additive model, this should be the no pooling
### analog to the varying intercept only model

fit_04 <- stan_lm(y ~ j + x_01 + x_02, df_b,
                  prior = R2(location = 0.5),
                  adapt_delta = 0.995,
                  iter = 3000, warmup = 1000,
                  seed = 412412)

summary(fit_04)

### make posterior predictions with the simple additive model

post_pred_04 <- posterior_predict(fit_04, pred_input_grid_b)

post_pred_summary_04 <- post_pred_04 %>% as.data.frame() %>% tbl_df() %>% 
  tibble::rowid_to_column("post_id") %>% 
  tidyr::gather(key = "pred_id", value = "pred_value", -post_id) %>% 
  mutate_at(c("pred_id"), as.integer) %>% 
  group_by(pred_id) %>% 
  summarise(num_post = n(),
            q05 = quantile(pred_value, 0.05),
            q25 = quantile(pred_value, 0.25),
            avg = mean(pred_value),
            q50 = median(pred_value),
            q75 = quantile(pred_value, 0.75),
            q95 = quantile(pred_value, 0.95)) %>% 
  ungroup() %>% 
  left_join(pred_input_grid_b %>% tibble::rowid_to_column("pred_id"),
            by = "pred_id")

### visualize the trends of the simple additive model 

post_pred_summary_04 %>% 
  ggplot(mapping = aes(x = x_01)) +
  geom_ribbon(mapping = aes(ymin = q05, ymax = q95,
                            group = interaction(x_02, j)),
              fill = "dodgerblue", alpha = 0.5) +
  geom_ribbon(mapping = aes(ymin = q25, ymax = q75,
                            group = interaction(x_02, j)),
              fill = "steelblue") +
  geom_line(mapping = aes(y = avg,
                          group = interaction(x_02, j)),
            color = "black") +
  geom_line(data = pred_input_grid_c,
            mapping = aes(x = x_01, y = f_true,
                          group = interaction(x_02, j)),
            color = "red", linetype = "dashed") +
  facet_wrap(~j, labeller = "label_both") +
  labs(y = "value") +
  theme_bw()

post_pred_summary_04 %>% 
  ggplot(mapping = aes(x = x_02)) +
  geom_ribbon(mapping = aes(ymin = q05, ymax = q95,
                            group = interaction(x_01, j)),
              fill = "dodgerblue", alpha = 0.5) +
  geom_ribbon(mapping = aes(ymin = q25, ymax = q75,
                            group = interaction(x_01, j)),
              fill = "steelblue") +
  geom_line(mapping = aes(y = avg,
                          group = interaction(x_01, j)),
            color = "black") +
  geom_line(data = pred_input_grid_c,
            mapping = aes(x = x_02, y = f_true,
                          group = interaction(x_01, j)),
            color = "red", linetype = "dashed") +
  facet_wrap(~j, labeller = "label_both") +
  labs(y = "value") +
  theme_bw()

### now compare the 4 models using the LOO metric
# loo_01 <- loo(fit_01, k_threshold = 0.7) # this takes a long time!!!

loo_01 <- loo(fit_01)

plot(loo_01)

loo_02 <- loo(fit_02)

plot(loo_02)

loo_03 <- loo(fit_03)

plot(loo_03)

loo_04 <- loo(fit_04)

plot(loo_04)

loo_compare(loo_01, loo_02, loo_03, loo_04)
