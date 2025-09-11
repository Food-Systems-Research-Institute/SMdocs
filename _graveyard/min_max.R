# Min Max
# 2025-09-09


# Description -------------------------------------------------------------

# Min max normalization of a variable from 0 to 100.


# Dependencies ------------------------------------------------------------

# None


# Function ----------------------------------------------------------------

min_max <- function(x) {
  normed <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  normed * 100
}
