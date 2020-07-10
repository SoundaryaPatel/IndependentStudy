### set up the data for people in a city including their age

library(dplyr)
library(ggplot2)

num_people <- 125

### assume the AGE of a person has been standardized and we can just
### use a standard normal
set.seed(121323)
xage <- rnorm(n = num_people)

### assign each person to a city
num_cities <- 10

set.seed(71231)
jid <- sample(1:num_cities, size = num_people, replace = TRUE)

df_inputs <- tibble::tibble(age = xage, city_id = jid)

df_inputs

### make the model (design) matrix

df_inputs_b <- df_inputs %>% mutate(city_factor = as.factor(city_id))

Xmat <- model.matrix( ~ age + city_factor, df_inputs_b)

dim(Xmat)

Xmat %>% head()

### but what if we wanted interactions? so the impact of the age
### depends on the city?

Xmat_int <- model.matrix( ~ age * city_factor, df_inputs_b)

dim(Xmat_int)

Xmat_int %>% head()
