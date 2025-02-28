# Get Aggregations
# 2025-02-27


# Description -------------------------------------------------------------


# Function to take normalized metrics and aggregate them into indicators, 
# indices, and dimensions, according to various different methods, including:
#   1. Arithmetic Means
#   2. Geometric Means
#   3. Eventually add PCA loadings?

# We should set this up so that we can do each step individually, because we 
# will likely want to do indicators based on means, but then use PCA loadings
# at the indicator level to get indices, and so on.

# Functions list:
#   get_geo_mean - manually calculate geo means
#   get_agg_function - chooses aggregation function based on agg_type
#   get_agg_indicators - arith and geo means of metrics to get indicators
#     input is normalized_metrics_df, one for each transformation
#     requires refined and filtered frame as well
#   get_agg_indices - arith and geo means of indicators to get indices
#     input is normalized_metrics_df, one for each transformation
#     requires refined and filtered frame as well
#   get_agg_dimensions - arith and geo means of indicators to get dimensions
#     input is normalized_metrics_df, one for each transformation
#     requires refined and filtered frame as well

#   Probably something about wrangling too... and adding US median and NewEng



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  conflicted
)

conflicted::conflicts_prefer(
  dplyr::select(),
  dplyr::filter(),
  .quiet = TRUE
)

# Load refined and filtered framework
# frame <- readRDS('data/filtered_frame.rds')



# Utility Functions -------------------------------------------------------


get_geo_mean <- function(x, na.rm = TRUE){
  exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
}


agg_function <- function(x, agg_type) {
   if (agg_type == 'geometric') {
    get_geo_mean(x)
  } else if (agg_type == 'arithmetic') {
    mean(x)
  }
}


# Also put state names back in as a column and with real names, not codes
# Takes a list of the indicator, index, and dimension scores
get_organized_scores <- function(scores_list,
                                 state_key,
                                 metrics_df) {
  # All combinations, also a name
  combos <- expand.grid(
    names(scores_list[[1]]),
    c('arithmetic', 'geometric')
  ) %>% 
    mutate(name = paste0(Var1, '_', Var2))
  
  # Map to pull them all out
  scores <- map2(combos[[1]], combos[[2]], \(norm_type, agg_type) {
    
    # Get list of each df (dimension, index, indicator) for combo
    dfs <- all_scores %>% 
      map(\(level) level[[norm_type]]) %>% 
      map(\(norm) norm[[agg_type]])
    
    map(dfs, ~ {
      .x %>% 
        # Note that we are binding fips back in - this is hinky, note to fix
        bind_cols(
          metrics_df %>% 
            rownames_to_column('fips') %>% 
            select(fips)
        ) %>% 
        left_join(
          select(sm_data$state_key, state, state_code),
          by = join_by(fips == state_code) 
        ) %>% 
        select(-fips)
    })
  }) %>% 
    setNames(c(combos$name))
  
  return(scores)
} 


get_groupings <- function(scores_list) {
  
  final_scores <- map(scores_list, \(method) {
    map(method, \(level) {
      
      # Mean of every US state and DC
      us_means <- level %>%
        dplyr::select(-state) %>% 
        colMeans() %>% 
        as.list()
      us_means$state <- 'US_mean'
      
      # Median of every US state and DC
      us_medians <- level %>% 
        dplyr::select(-state) %>% 
        map_dbl(median) %>% 
        as.list()
      us_medians$state <- 'US_median'
       
      # Mean of just New England states
      ne_means <- level %>% 
        dplyr::filter(state %in% c('VT', 'NH', 'ME', 'MA', 'CT', 'RI')) %>% 
        dplyr::select(-state) %>% 
        colMeans() %>% 
        as.list()
      ne_means$state <- 'NE_mean'
      
      # Median of just New England states
      ne_medians <- level %>% 
        dplyr::filter(state %in% c('VT', 'NH', 'ME', 'MA', 'CT', 'RI')) %>% 
        dplyr::select(-state) %>% 
        map_dbl(median) %>% 
        as.list()
      ne_medians$state <- 'NE_median'
      
      # Return the level + US + NE groupings
      level %>% 
        bind_rows(us_means) %>% 
        bind_rows(us_medians) %>% 
        bind_rows(ne_means) %>% 
        bind_rows(ne_medians)
    })
  })
  
  return(final_scores)
}



# Aggregation Functions ---------------------------------------------------


get_agg_indicators <- function(normed_data,
                               framework) {
  
  indicator_scores <- map(normed_data, \(df) {
    
    # For each df, calculate indicator means
    indicators_out <- map(unique(framework$indicator), \(ind) {
      
      # Split into groups by indicator, with one or more metrics each
      variables <- framework %>% 
        dplyr::filter(indicator == ind) %>% 
        pull(variable_name) %>% 
        unique()
      indicator_metrics <- df %>% 
        select(all_of(variables))
      
      # Get arithmetic and geo means for each indicator
      dfs <- list()
      dfs$arithmetic <- indicator_metrics %>%
        rowwise() %>%
        mutate(
          !!sym(ind) := mean(c_across(everything())),
        ) %>%
        select(!!sym(ind))
      dfs$geometric <- indicator_metrics %>% 
        rowwise() %>% 
        mutate(
          !!sym(ind) := get_geo_mean(c_across(everything())),
        ) %>%
        select(!!sym(ind))
      return(dfs) 
    })
    
    # Rearrange so we put each aggregation method (arith, geo) together
    norm_out <- list()
    norm_out$arithmetic <- map(indicators_out, ~ {
      .x[grep("arithmetic", names(.x))]
    }) %>% 
      bind_cols()
    norm_out$geometric <- map(indicators_out, ~ {
      .x[grep("geometric", names(.x))]
    }) %>% 
      bind_cols()
    return(norm_out) 
  })
  
  return(indicator_scores)
}


get_agg_indices <- function(indicator_scores,
                            framework) {
  # Get list of indices as strings 
  indices <- unique(framework$index)
  
  index_scores <- map(indicator_scores, \(norm_type) {
    imap(norm_type, \(agg_df, agg_type) {
      map(indices, \(index_) {
        # Get names of indicators for this index
        index_indicators <- filtered_frame %>% 
          dplyr::filter(index == index_) %>% 
          pull(indicator) %>% 
          unique()
        # Get DF of indicators for this index
        index_indicator_df <- agg_df %>% 
          dplyr::select(all_of(index_indicators))
        # Get arithmetic or geometric mean, based on agg_type
        index_indicator_df %>% 
          rowwise() %>% 
          mutate(!!sym(index_) := agg_function(c_across(everything()), agg_type)) %>% 
          dplyr::select(!!sym(index_))
      }) %>% 
        bind_cols()
    })
  }) 
  return(index_scores)
}


get_agg_dimensions <- function(index_scores,
                               framework) {
    
  # Same process for dimensions
  dimensions <- unique(framework$dimension)
  
  dimension_scores <- map(index_scores, \(norm_type) {
    imap(norm_type, \(agg_df, agg_type) {
      map(dimensions, \(dimension_) {
        # Get names of indices for this dimension
        dimension_indices <- framework %>% 
          filter(dimension == dimension_) %>% 
          pull(index) %>% 
          unique()
        # Get DF of indice for this dimension
        dimension_index_df <- agg_df %>% 
          select(all_of(dimension_indices))
        # Get arithmetic or geometric mean, based on agg_type
        dimension_index_df %>% 
          rowwise() %>% 
          mutate(!!sym(dimension_) := agg_function(
            c_across(everything()), 
            agg_type
          )) %>% 
          select(!!sym(dimension_))
      }) %>% 
        bind_cols()
    })
  })
    
  return(dimension_scores)
}



# Everything --------------------------------------------------------------


# Putting everything into a single function so we can run start to finish
get_all_aggregations <- function(normed_data,
                                 framework,
                                 state_key,
                                 metrics_df) {
  
  # Make empty list for results from each level
  scores <- list()
  
  # Start with normed data, get each level of scores
  scores$indicator_scores <- get_agg_indicators(normed_data, framework)
  scores$index_scores <- get_agg_indices(scores$indicator_scores, framework)
  scores$dimension_scores <- get_agg_dimensions(scores$index_scores, framework)
  
  # Now organize and add groupings
  organized_scores <- get_organized_scores(scores, state_key, metrics_df)
  groupings <- get_groupings(organized_scores)
  
  return(groupings)
}
