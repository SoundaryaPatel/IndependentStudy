### practice generating random spatial locations

### rnorm, rbeta, rbinom, MASS::mvrnorm

library(dplyr)
library(ggplot2)

### generating standard normals -- for LOCATIONS/POSITIONS

set.seed(12345)
x1 <- rnorm(25)
x2 <- rnorm(25)

df <- tibble::tibble(x1 = x1, x2 = x2)

df %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_point() +
  coord_equal() +
  theme_bw()

### correlated positions -- define a covariance matrix
rho_cor <- 0.55
Kmat <- matrix(c(1, rho_cor, rho_cor, 1), nrow = 2)

Kmat

### use the MASS::mvrnom() function to generate correlated random variables
set.seed(765543)
df2 <- MASS::mvrnorm(n = 25, mu = c(0, 0), Sigma = Kmat)

df2 <- df2 %>% 
  as.data.frame() %>% 
  tbl_df() %>% 
  purrr::set_names(c("x1", "x2"))

df2

df2 %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_point() +
  coord_equal() +
  theme_bw()

### comprae the standard independent normals with the correlated variables
df %>% mutate(type = "independent") %>% 
  bind_rows(df2 %>% mutate(type = "correlated")) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_point(mapping = aes(color = type)) +
  ggthemes::scale_color_colorblind("") +
  coord_equal() +
  theme_bw()

### what's going on behind the scenes when generating correlated guassians?

### compute the cholesky decomposition
Umat <- chol(Kmat)

Umat

all.equal(Kmat, t(Umat) %*% Umat)

### generate random gaussians with non-zero means and non-unity variance
set.seed(7617)
rnorm(n = 7)

set.seed(7617)
rnorm(n = 7, mean = 15, sd = 3)

### to generate random draws from an arbitrary Gaussian, we generate
### random standard normals then rescale

set.seed(7617)
zr <- rnorm(n = 7)

zr

### multiply by the standard deviation
3 * zr

### add back in the means
3 * zr + 15

### consider the multivariabte normal with uncorrelated variables
### with marginal variances of 4
Kind <- matrix(c(4, 0, 0, 4), nrow = 2)

Kind

Uind <- chol(Kind)

Uind

### generate random draws from the correlated gaussian using the
### covariance matrix

set.seed(9878)
B1 <- MASS::mvrnorm(n = 1, mu = c(0, 0), Sigma = Kmat)

B1

set.seed(9878)
MASS::mvrnorm(n = 1, mu = c(0, 0), Sigma = Kmat)

### use the cholesky decomposition, by first generating the independent
### z-scores
diag(2)

set.seed(9878)
Z1 <- rnorm(n = 2)
# Z1 <- MASS::mvrnorm(n = 1, mu = c(0, 0), Sigma = diag(2))

Z1

### convert from the independent z-scores to the dependent variables
### (original variables that are related to each other)
X1 <- as.vector(t(Umat) %*% as.matrix(Z1))

X1

### another way to consider, instead of just correlation, we could think
### of distinct groups or clusters
d <- 1:4

did <- tibble::tibble(
  id = d,
  x1 = c(-2, -2, 2, 2),
  x2 = c(-2, 2, -2, 2)
)

did %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_point(size = 5) +
  coord_equal() +
  theme_bw()

### standard normals around each cluster
set.seed(71231)
x1_a <- rnorm(n = 100)
x2_a <- rnorm(n = 100)

### how do we assign groupings?
set.seed(546123)
### random cluster assignments
group_ids <- sample(1:4, size = 100, replace = TRUE)

group_ids

### cluster centers
centers_x1 <- did$x1
centers_x2 <- did$x2

x1_results <- x1_a + centers_x1[group_ids]

x2_results <- x2_a + centers_x2[group_ids]

### replicate the centers for each group
centers_x1

group_ids

centers_x1[group_ids]

### combine the x1 and x2 positions for the randomly generated locations by clusters
df_clusters <- tibble::tibble(
  x1 = x1_results,
  x2 = x2_results,
  cluster_id = group_ids
)

df_clusters %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_point(data = did, size = 6) +
  geom_text(data = did, mapping = aes(label = id), 
            color = "white") +
  geom_point(mapping = aes(color = as.factor(cluster_id))) +
  ggthemes::scale_color_calc("cluster") +
  coord_equal() +
  theme_bw()

### include the original standard normal draws as grey points
df_clusters %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_point(data = tibble::tibble(x1 = x1_a, x2 = x2_a),
             color = "grey", alpha = 0.5) +
  geom_point(data = did, size = 6) +
  geom_text(data = did, mapping = aes(label = id), 
            color = "white") +
  geom_point(mapping = aes(color = as.factor(cluster_id))) +
  ggthemes::scale_color_calc("cluster") +
  coord_equal() +
  theme_bw()

### separate by the random cluster assignment
df_clusters %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_point(data = tibble::tibble(x1 = x1_a, x2 = x2_a, cluster_id = group_ids),
             color = "grey", alpha = 0.5) +
  geom_point(data = did, size = 6) +
  geom_text(data = did, mapping = aes(label = id), 
            color = "white") +
  geom_point(mapping = aes(color = as.factor(cluster_id))) +
  facet_wrap(~cluster_id, labeller = "label_both") +
  ggthemes::scale_color_calc("cluster") +
  coord_equal() +
  theme_bw()
  