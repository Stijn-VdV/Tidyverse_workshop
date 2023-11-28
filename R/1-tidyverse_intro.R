###############################################
######## Tidyverse: Data Wrangling 101 ########
######## **Introduction to Tidyverse** ########
########_______________________________########
rm(list = ls())
.rs.restartR()

# - Install and import tidyverse ----
# install + load tidyverse
# if (!require("tidyverse")) install.packages("tidyverse")
# install.packages("tidyverse")

# check for updates
tidyverse::tidyverse_update()

# masked functions -> namespaces {package_name}::{function_name}()
filter
stats::filter
dplyr::filter

# - Tibbles ----
# set seed for reproducibility
set.seed(1)

# create a dataframe
a_dataframe <- data.frame(x = 1:25,
                          y = rnorm(25, 1, 2))

# create a tibble from the dataframe
# library(tidyverse)
# as_tibble(a_dataframe)
a_tibble <- tibble::as_tibble(a_dataframe)

# compare
a_dataframe
a_tibble

sum(a_dataframe$y) == sum(a_tibble$y)  # equal sums
all.equal(a_dataframe, a_tibble)    # some differences (?)
class(a_dataframe); class(a_tibble) # only differnce is class description

# clean environment
rm(list = ls())

# - Pipes: basics ----
# basics
24 + 9
sum(24, 9)

# magrittr
# library(tidyverse)
# library(dplyr)
library(magrittr) # pipe from magrittr package: %>%
24 %>% sum(9)
24 %>% sum(9, .) # . as placeholder

"one" %>% paste("two")
"one" %>% paste("two", .)

# base R
24 |> sum(9)

# show data
dplyr::glimpse(mtcars)
str(mtcars)
head(mtcars)

# load dplyr
library(dplyr) # filter, mutate, select

  # mtcars #1
df_cars1 <- tibble::rownames_to_column(mtcars, "car") # function from tibble package
df_cars1 <- filter(df_cars1, stringr::str_detect(car, "Merc"))
df_cars1 <- mutate(df_cars1, kml = mpg*(1.60934/3.78541))
df_cars1 <- select(df_cars1, car, kml, cyl, hp)

  # mtcars #2
df_cars2 <- select(
  mutate(
    filter(tibble::rownames_to_column(mtcars, "car"), stringr::str_detect(car, "Merc")),
    kml = mpg*(1.60934/3.78541)),
  car, kml, cyl, hp)

  # mtcars #3
df_cars3 <- mtcars %>%
  tibble::rownames_to_column("car") %>%
  filter(stringr::str_detect(car, "Merc")) %>%
  # convert miles per gallon -> km per liter
  mutate(kml = mpg*(1.60934/3.78541)) %>%
  select(car, kml, cyl, hp)

  # check
all.equal(df_cars1, df_cars2)
all.equal(df_cars2, df_cars3)
all.equal(df_cars1, df_cars3)

# - Tidy data ----
" - each variable must have its own column
  - each observation must have its own row
  - each value must have its own cell
==> Every variable goes in a column, every observation in a row"

# - Exercises ----
# EXERCISE 1: Rewrite the following lines of code using %>%
round(pi, 2)
sum(1, 5, 20)
as_tibble(mtcars)


# EXERCISE 2: The following block of code is difficult to read because of multiple
#             layers of nesting. Improve the readability of the code using %>%.
storms_clean <- summarize(
  group_by(
    filter(storms, name == "Sandy"), day),
  average_wind = mean(wind),
  sd_wind = sd(wind))


# EXERCISE 3: Do the following datasets follow the rules of 'tidy data'?
#             Why (not)? Use
mtcars
relig_income
billboard
