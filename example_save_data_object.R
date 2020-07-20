### generate random data and save it

library(dplyr)
library(ggplot2)

set.seed(1312)
x <- rnorm(n = 101)
y <- rbeta(n = 101, shape1 = .3, shape2 = 2.4)

df <- tibble::tibble(x = x, y = y)


df %>% ggplot(mapping = aes(x = x, y = y)) +
  geom_point() +
  theme_bw()

### save the data set, FIRST make the data sub-folder
df %>% readr::write_rds("data/example_data_set.rds")

### read in the data set
df_read <- readr::read_rds("data/example_data_set.rds")

df_read %>% 
  ggplot(mapping = aes(x = x, y = y)) +
  geom_point() +
  theme_bw()
