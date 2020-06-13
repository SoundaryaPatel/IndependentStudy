### demonstrate hierarchical model with known observation level sigma
### "8-schools" structure.

library(dplyr)
library(ggplot2)

### GENERATE SYNTHETIC DATA
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

### define the log-posterior in terms of the reparameterized
### group means, eta (non-centered), the reparameterized
### grand mean, mu_tilde, and the transformed hyperprior
### standard deviation, varphi

my_logpost <- function(unknowns, my_info)
{
  # unpack the vector of unknown parameters to learn
  eta <- unknowns[1:my_info$J]
  mu_tilde <- unknowns[my_info$J+1]
  varphi <- unknowns[length(unknowns)]
  
  # calculate the hyperparameters
  mu <- my_info$m0 + my_info$s0 * mu_tilde
  tau <- exp(varphi)
  
  # calculate the unkonwn group means, thetas
  theta <- mu + tau * eta
  
  # the log-likelihood
  log_lik <- sum(dnorm(my_info$ybarj,
                       mean = theta,
                       sd = my_info$sdj,
                       log = TRUE))
  
  # the log-hierarchical prior on the reparameterized
  # group means, eta
  log_prior <- sum(dnorm(eta, log = TRUE))
  
  # the log-hyperprior on the reparameterized 
  # hyperprior (grand) mean
  log_hyper_mu <- dnorm(mu_tilde, log = TRUE)
  
  # the log-hyperprior on the transformed hyperprior
  # standard deviation
  log_hyper_tau <- dexp(tau, rate = my_info$lambda0, log = TRUE)
  
  # the log of the derivative adjustment
  log_deriv_adjust <- varphi
  
  # sum everything together
  log_lik + log_prior + log_hyper_mu + log_hyper_tau + log_deriv_adjust
}

### define the list of required information
group_info <- list(
  J = nrow(group_df),
  ybarj = group_df$ybarj,
  sdj = group_df$sdj,
  m0 = 50,
  s0 = 10,
  lambda0 = 1/3
)

### ttest out the log-posterior function works
my_logpost(rep(0, group_info$J + 2), group_info)

my_logpost(rep(1.2, group_info$J + 2), group_info)

### use the same function from INFSCI 2595 to perform
### laplace approximation
my_laplace <- function(start_guess, logpost_func, ...)
{
  # code adapted from the `LearnBayes`` function `laplace()`
  fit <- optim(start_guess,
               logpost_func,
               gr = NULL,
               ...,
               method = "BFGS",
               hessian = TRUE,
               control = list(fnscale = -1, maxit = 1001))
  
  mode <- fit$par
  h <- -solve(fit$hessian)
  p <- length(mode)
  int <- p/2 * log(2 * pi) + 0.5 * log(det(h)) + logpost_func(mode, ...)
  
  list(mode = mode,
       var_matrix = h,
       log_evidence = int,
       converge = ifelse(fit$convergence == 0,
                         "YES", 
                         "NO"),
       iter_counts = fit$counts[1])
}

### perform the laplace approximation for a random guess
set.seed(4123)
start_guess_1 <- rnorm(n = (group_info$J +2))

start_guess_1

laplace_1 <- my_laplace(start_guess_1, my_logpost, group_info)

laplace_1$mode

sqrt(diag(laplace_1$var_matrix))

### try a second random initial guess
set.seed(321131)
start_guess_2 <- rnorm(n = (group_info$J +2))

start_guess_2

laplace_2 <- my_laplace(start_guess_2, my_logpost, group_info)

laplace_2$mode

sqrt(diag(laplace_2$var_matrix))

### double check
max( abs(laplace_1$mode - laplace_2$mode) / laplace_1$mode )



max( abs(sqrt(diag(laplace_1$var_matrix)) - sqrt(diag(laplace_2$var_matrix))) )

### results are pretty close

### generate posterior samples of the reparameterized unknown parameters
generate_post_samples <- function(mvn_result, length_eta, num_samples)
{
  MASS::mvrnorm(n = num_samples,
                mu = mvn_result$mode,
                Sigma = mvn_result$var_matrix) %>% 
    as.data.frame() %>% tbl_df() %>% 
    purrr::set_names(c(sprintf("eta_%02d", 1:length_eta), "mu_tilde", "varphi"))
}

set.seed(991231)
post_samples <- generate_post_samples(laplace_1, group_info$J, 3e3)

post_samples %>% glimpse()

post_samples %>% 
  ggplot(mapping = aes(x = eta_01, y = eta_02)) +
  geom_point(alpha = 0.2) +
  theme_bw()

### posterior correlation between the reparameterized variables
post_samples %>% 
  cor() %>% 
  corrplot::corrplot(type = "upper", method = "square")

### back-transform from the reparameterized space to the 
### original parameters we care about
post_params <- post_samples %>% 
  mutate(tau = exp(varphi),
         mu = group_info$m0 + group_info$s0 * mu_tilde) %>% 
  tibble::rowid_to_column("post_id") %>% 
  tidyr::gather(key = "eta_name", value = "eta",
                -post_id, -mu_tilde, -varphi, -tau, -mu) %>% 
  mutate(theta = mu + tau * eta) %>% 
  mutate(J = as.numeric(stringr::str_extract(eta_name, "\\d+")),
         theta_name = sprintf("theta_%02d", J)) %>% 
  select(post_id, mu, tau, theta, theta_name) %>% 
  tidyr::spread(theta_name, theta)

post_params %>% glimpse()

### look at the posterior parameter histograms compared to the
### TRUE parameter values that generated the data
post_params %>% 
  tidyr::gather(key = "key", value = "value", -post_id) %>% 
  ggplot(mapping = aes(x = value)) +
  geom_histogram(bins = 55) +
  geom_vline(data = data.frame(key = c("mu", "tau", sprintf("theta_%02d", 1:num_groups)),
                               true_value = c(mu_true, tau_true, theta_true)),
             mapping = aes(xintercept = true_value),
             color = "red", size = 1.5, linetype = "dashed") +
  facet_wrap(~key, scales = "free") +
  theme_bw() +
  theme(axis.text.y = element_blank())

### focus on just a few of the theta parameters
post_params %>% 
  tidyr::gather(key = "key", value = "value", -post_id) %>% 
  filter(key %in% sprintf("theta_%02d", 1:4)) %>% 
  ggplot(mapping = aes(x = value)) +
  geom_histogram(bins = 55) +
  geom_vline(data = data.frame(key = c(sprintf("theta_%02d", 1:4)),
                               true_value = theta_true[1:4]),
             mapping = aes(xintercept = true_value),
             color = "red", size = 1.5, linetype = "dashed") +
  facet_wrap(~key, scales = "free") +
  theme_bw() +
  theme(axis.text.y = element_blank())

### look at the posterior summaries of the group means
### compared to the hyperprior mean, overlay the observations
### include the true values
post_params %>% 
  select(-tau) %>% 
  tidyr::gather(key = "key", value = "value", -post_id) %>% 
  ggplot(mapping = aes(x = key, y = value)) +
  geom_boxplot() +
  geom_point(data = df %>% mutate(key = sprintf("theta_%02d", J)),
             mapping = aes(x = key, y = y),
             color = "darkorange", alpha = 0.5) +
  geom_point(data = data.frame(key = c("mu", sprintf("theta_%02d", 1:num_groups)),
                               true_value = c(mu_true, theta_true)),
             mapping = aes(x = key, y = true_value),
             shape = 4, size = 4, color = "red") +
  theme_bw()

### probability that group 2 is different from group 1
post_params %>% 
  mutate(diff_1_and_2 = abs(theta_02 - theta_01)) %>% 
  pull(diff_1_and_2) %>% 
  summary()

post_params %>% 
  ggplot(mapping = aes(x = abs(theta_02 - theta_01))) +
  stat_ecdf() +
  theme_bw()

### double check the posterior correlation
post_params %>% 
  select(-post_id) %>% 
  cor() %>% 
  corrplot::corrplot(type = "upper", method = "square")
