# Useful scripts

# Keep columns starting just by x20
# select(starts_with("Petal") 
library(tidyverse)

UNEMP_DATA_clean <- UNEMP_DATA %>% 
  select(
    starts_with("time"),
    starts_with("x20")
  )
UNEMP_DATA_clean
str(UNEMP_DATA_clean)
