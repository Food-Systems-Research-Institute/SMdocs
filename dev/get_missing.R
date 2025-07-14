# Get Missing
# 2025-07-14


# Description -------------------------------------------------------------

# Check missing data for each variable individually from wide df. Necessary 
# because variables have different update frequencies, i.e. some are annual,
# some are every 5 years, and they don't cover same ranges of time necessarily


# Housekeeping ------------------------------------------------------------

pacman::p_load(
  dplyr,
  tidyr
)


# Function ----------------------------------------------------------------

get_missing <- function(df, var, out = FALSE) {
  dat <- df %>% 
    select(fips, year, var) %>% 
    na.omit() %>% 
    complete(fips, year)
  years <- paste0(sort(unique(dat$year)), collapse = ', ')
  n_miss <- sum(is.na(dat[[var]]))
  total <- length(dat[[var]])
  perc_miss <- round((n_miss/total) * 100, 3)
  cat('\n\nYears:', years)
  cat('\nMissing: ', n_miss, ' out of ', total, ' (', perc_miss, '%)', sep = '')
  if (out) return(dat)
}
