###############################################
######## Tidyverse: Data Wrangling 101 ########
########            **EXTRA**          ########
########_______________________________########

# Load packages ----
library(dplyr)
library(tidyr)
library(ggplot2)

# Import data ----
readxl::excel_sheets("data/extra/T_trails Sweden 2020.xlsx")

# observation data
df_obs <- readxl::read_xlsx("data/extra/T_trails Sweden 2020.xlsx", sheet = 1)
str(df_obs)

# metadata
df_met <- readxl::read_xlsx("data/extra/T_trails Sweden 2020.xlsx", sheet = 2)
str(df_met)

df_met <- df_met |> 
  mutate(across(contains("cover"), ~as.numeric(.x)))
  
# Data manipulation ----
#' Here, we'll explore general veg cover in different Habitat types. To prevent 
#' overplotting, we'll restrict our selection to the 10 most common habitat types.
count(df_met, `Habitat types`) |> 
  arrange(-n) |> View()

# store most commonly found habitats in separate vector
habs <- df_met |> 
  filter(`Habitat types` != "NA") |> 
  count(`Habitat types`) |> 
  slice_max(order_by = n, n = 3) |> # retains 4 habitats!!
  pull(`Habitat types`)

nrow(df_met) # 114
df_met |> filter(`Habitat types` %in% habs) |> nrow() #57

# use remaining habitats to filter data and join
df_prep <- df_met |> 
  filter(`Habitat types` %in% habs) |> 
  select(Trail, Transect, Plot, "Hab" = `Habitat types`, contains("Cover")) |> 
  pivot_longer(
    cols = contains("Cover"),
    names_to = "Cover_class",
    values_to = "Cover_val"
  ) |> 
  filter(Cover_class != "Total vegetation cover")

# Dataviz ----
df_prep |> 
  ggplot() +
  geom_boxplot(aes(x = Cover_val, fill = Cover_class)) +
  facet_wrap(~Hab) +
  theme_classic() +
  ggsci::scale_fill_igv() +
  scale_x_continuous(limits = c(0, 100)) +
  labs(x = "Cover (%)")
  
  



