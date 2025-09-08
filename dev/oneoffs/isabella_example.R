pacman::p_load(
  dplyr,
  stringr
)

# To get fresh data
remotes::install_github('Food-Systems-Research-Institute/SMdata', force = TRUE)
library(SMdata)
# Note that you must not have the package loaded yet
# i.e. restart RStudio and do this before loading anything else

# Make a toy dataset
df <- head(metrics, 25)
str(df)

# Change entire variable names that have 'annual' in it, case insensitive. Leave
# everything else as is.
df
df %>% 
  mutate(
    variable_name = case_when(
      str_detect(variable_name, regex('annual', ignore_case = TRUE)) ~ 'CHANGED',
      .default = variable_name
    )
  )
  
# Replace a certain string in variable name, leave rest. 
df
df %>% 
  mutate(
    variable_name = str_replace(variable_name, 'oty', 'XXXXX')
    # variable_name = str_replace(variable_name, 'N 2 o', 'N2O')
  )

