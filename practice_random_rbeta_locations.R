### practice using the random beta distributed locations around clusters

library(dplyr)
library(ggplot2)

### randomly generate the cluster centers from a defined beta distribution
a0 <- 0.5
b0 <- 0.5

### what's the mean and standard deviation of the cluster generating process?
a0 / (a0 + b0)

sqrt((a0 * b0) / ((a0 + b0)^2 * (a0 + b0 + 1)))

### how many clusters?
n_clusters <- 4

### use the same parameterization for both directions
set.seed(12121)
xc_1 <- rbeta(n = n_clusters, shape1 = a0, shape2 = b0)
xc_2 <- rbeta(n = n_clusters, shape1 = a0, shape2 = b0)

### plot the randomly generated cluster centers
df_clusters <- tibble::tibble(x1 = xc_1, x2 = xc_2) %>% 
  tibble::rowid_to_column("cluster_id")

df_clusters %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_point(size = 4) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_bw()

### set the variance in each direction for each cluster
### for locations around the cluster centers
### set the variance in each direciton based on the ratio
### of the variance and the "bernoulli variance"
cluster_r <- 0.02
cluster_var_1 <- cluster_r * xc_1 * (1 - xc_1)
cluster_var_2 <- cluster_r * xc_2 * (1 - xc_2)

### calcualte the parameters of the beta distributions
### asssociated with each cluster, based on the cluster
### centers
cluster_size_1 <- 1 / cluster_r - 1
cluster_size_2 <- 1 / cluster_r - 1

cluster_a_1 <- xc_1 * cluster_size_1
cluster_b_1 <- (1 - xc_1) * cluster_size_1

cluster_a_2 <- xc_2 * cluster_size_2
cluster_b_2 <- (1 - xc_2) * cluster_size_2

### randomly assign locations to clusters
n_locations <- 100

set.seed(9871)
locations_in_groups <- sample(1:n_clusters, size = n_locations, replace = TRUE)

locations_in_groups

### generate the random locations around the cluster centers
set.seed(45131)
x_1 <- rbeta(n = n_locations, shape1 = cluster_a_1[locations_in_groups], shape2 = cluster_b_1[locations_in_groups])
x_2 <- rbeta(n = n_locations, shape1 = cluster_a_2[locations_in_groups], shape2 = cluster_b_2[locations_in_groups])

### plot the random locations around the cluster centers
df <- tibble::tibble(
  x1 = x_1,
  x2 = x_2,
  cluster_id = locations_in_groups
)

df %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_point(data = df_clusters,
             size = 7) +
  geom_text(data = df_clusters,
            mapping = aes(label = cluster_id),
            color = "white") +
  geom_point(mapping = aes(color = as.factor(cluster_id))) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  ggthemes::scale_color_calc("ID") +
  theme_bw()
