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
  purrr,
  conflicted,
  tibble
)

conflicted::conflicts_prefer(
  dplyr::select(),
  dplyr::filter(),
  dplyr::pull(),
  dplyr::arrange(),
  .quiet = TRUE
)



# Utility Functions -------------------------------------------------------


get_geo_mean <- function(x, na.rm = TRUE){
  if (all(x > 0)) {
    exp(mean(log(x), na.rm = na.rm))
  } else if (any(x <= 0)) {
    horizontal_shift <- abs(min(x)) + 1
    exp(mean(log(x + horizontal_shift), na.rm = na.rm)) - horizontal_shift
  } 
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
# scores_list is a list with scores at each level (indicator, index, dimension)
get_organized_scores <- function(scores_list,
                                 state_key,
                                 fips_vector,
                                 aggregation = c('both', 'arithmetic', 'geometric')) {
  
  # Aggregations to put into expand grid combos
  if (aggregation == 'both') {
    combos <- expand.grid(
      names(scores_list[[1]]),
      c('arithmetic', 'geometric')
    ) %>% 
      mutate(name = paste0(Var1, '_', Var2))
  } else if (aggregation == 'arithmetic') {
    combos <- expand.grid(
      names(scores_list[[1]]),
      'arithmetic'
    ) %>% 
      mutate(name = paste0(Var1, '_', Var2))
  } else if (aggregation == 'geometric') {
    combos <- expand.grid(
      names(scores_list[[1]]),
      'geometric'
    ) %>% 
      mutate(name = paste0(Var1, '_', Var2))
  }
  
  # Map to pull them all out
  scores <- map2(combos[[1]], combos[[2]], \(norm_type, agg_type) {
    
    # Get list of each df (dimension, index, indicator) for combo
    dfs <- scores_list %>% 
      map(\(level) level[[norm_type]]) %>% 
      map(\(norm) norm[[agg_type]])
    
    map(dfs, ~ {
      .x %>% 
        bind_cols(as.data.frame(fips_vector)) %>% 
        left_join(
          select(state_key, state, state_code),
          by = join_by(fips_vector == state_code) 
        ) %>% 
        select(-fips_vector)
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


# Use framework to get unique indicators
# n_metrics is how many metrics we want to sample
# result is the variable_names that we are keeping
sample_metrics <- function(framework,
                           n_metrics,
                           seed) {
  
}

# Aggregation Functions ---------------------------------------------------


get_agg_indicators <- function(normed_data,
                               framework,
                               aggregation = c('both', 'arithmetic', 'geometric')) {
  
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
      if (aggregation %in% c('both', 'arithmetic')) {
        dfs$arithmetic <- indicator_metrics %>%
          rowwise() %>%
          mutate(
            !!sym(ind) := mean(c_across(everything())),
          ) %>%
          select(!!sym(ind))
      }
      if (aggregation %in% c('both', 'geometric')) {
        dfs$geometric <- indicator_metrics %>% 
          rowwise() %>% 
          mutate(
            !!sym(ind) := get_geo_mean(c_across(everything())),
          ) %>%
          select(!!sym(ind))
      }
      return(dfs) 
    })
    
    # Rearrange so we put each aggregation method (arith, geo) together
    norm_out <- list()
    if (aggregation %in% c('both', 'arithmetic')) {
      norm_out$arithmetic <- map(indicators_out, ~ {
        .x[grep("arithmetic", names(.x))]
      }) %>% 
        bind_cols()
    }
    if (aggregation %in% c('both', 'geometric')) {
      norm_out$geometric <- map(indicators_out, ~ {
        .x[grep("geometric", names(.x))]
      }) %>% 
        bind_cols()
    }
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
        index_indicators <- framework %>% 
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
          dplyr::filter(dimension == dimension_) %>% 
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


#' Putting everything into a single function so we can run start to finish

#' Normed data input is at data/rescaled_valued_metrics.rds. It includes all 9*
#'  iterations of transformation and scaling
#' Framework should be filtered to proper metrics at 'data/filtered_frame.rds'
#' If sample_metrics is TRUE, n_metrics MUST be included
get_all_aggregations <- function(normed_data,
                                 framework,
                                 state_key,
                                 aggregation = c('both', 'arithmetic', 'geometric'),
                                 remove_indicators = NULL,
                                 sample_metrics = NULL,
                                 n_metrics = NULL) {
  
  # Pull out proper fips vector from normed_data for use in get_organized
  fips_vector <- rownames(normed_data[[1]])
  
  ## Reduce inputs if removing indicators or metrics
  # If sampling metrics, remove them from all inputs
  if (!is.null(sample_metrics)) {
    if (is.null(n_metrics)) stop('Must provide n_metrics if sampling metrics')
    
    # Get a vector of metrics to keep based on n_metrics input
    sampled_metrics <- map(unique(framework$indicator), \(indic) {
      
      # Get the child metrics from each indicator
      child_metrics <- framework %>% 
        dplyr::filter(indicator == indic) %>% 
        pull(variable_name)
      
      # Get count of child metrics - don't need to sample more than that
      n_child_metrics <- length(child_metrics)
      
      # If capping out on metrics, just take whole child_metrics set
      # Otherwise, sample from child_metrics
      if (n_metrics >= n_child_metrics) {
        sampled_metrics <- child_metrics
      } else if (n_metrics < n_child_metrics) {
        sampled_metrics <- sample(child_metrics, n_metrics, replace = FALSE)
      }
      
      return(sampled_metrics)
    }) %>% 
      unlist()
        
    # Reduce the framework and valued_scaled_data inputs based on those metrics
    normed_data <- map(normed_data, ~ {
      dplyr::select(.x, all_of(sampled_metrics))
    })
    framework <- dplyr::filter(framework, variable_name %in% sampled_metrics)
    cat('\nSampling metrics\n')
  }
  
  
  # If removing indicators, remove from all of the above
  if (!is.null(remove_indicators) && remove_indicators != 'none') {
    
    # Reduce framework by removing indicators (and with them, metrics)
    framework <- framework %>% 
      filter(!indicator %in% remove_indicators)
    
    # Reduce normed datasets
    normed_data <- map(normed_data, ~ {
      dplyr::select(.x, all_of(framework$variable_name))
    })
    
    cat('\nRemoving indicators\n')
  } 
  
  
  ## Start with normed data, get each level of scores
  scores <- list()
  
  # cat('\nStarting indicators\n')
  scores$indicator_scores <- get_agg_indicators(normed_data, framework, aggregation = aggregation)
  
  # cat('\nStarting indices\n')
  scores$index_scores <- get_agg_indices(scores$indicator_scores, framework)
  
  # cat('\nStarting dimensions\n')
  scores$dimension_scores <- get_agg_dimensions(scores$index_scores, framework)
  
  # Now organize and add groupings
  # cat('\nStarting organization\n')
  organized_scores <- get_organized_scores(scores, state_key, fips_vector, aggregation = aggregation)
  
  # cat('\nStarting groupings\n')
  groupings <- get_groupings(organized_scores)
  
  # Finally, if metrics were selected, add a record of that
  if (!is.null(sample_metrics)) {
    groupings$sampled_metrics <- sampled_metrics
  }
  
  ## Print message about removing metrics or indicators
  if (!is.null(sample_metrics)) {
    cat('\nMetrics sampled:', paste0(sampled_metrics, sep = ','), '\n')
  } else {
    cat('\nNo metric sampling - all included\n')
  }
  
  if (!is.null(remove_indicators)) {
    cat('\nIndicators removed:', paste0(remove_indicators, sep = ','), '\n')
  } else {
    cat('\nNo indicators removed\n')
  }
  
  return(groupings)
}
