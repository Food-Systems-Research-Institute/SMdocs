# Takes clean tables with frequencies and scores and combines them into one

clean_refine_surveys <- function(
    tables, # list
    scope = c('indicator', 'index'),
    cat_must,
    cat_probably,
    n_votes
) {

  # Add category to tables
  props <- tables %>% 
    imap(~ .x %>% mutate(cat = .y)) %>% 
    bind_rows() %>% 
    select(-score)

  # Get proportion of probably include OR must include
  prop_prob_or_must_include <- props %>% 
    filter(cat %in% c(cat_must, cat_probably)) %>% 
    # group_by(scope) %>% 
    group_by(.[[scope]]) %>%
    summarize(prop_include = sum(freq) / n_votes) %>% 
    arrange(desc(prop_include))
  
  # Get proportion of must include
  prop_must_include <- props %>% 
    filter(cat == cat_must) %>% 
    group_by(.[[scope]]) %>% 
    summarize(prop_must = sum(freq) / n_votes) %>% 
    arrange(desc(prop_must))
  
  # Add up weighted scores
  scores <- tables %>% 
    bind_rows() %>% 
    group_by(.[[scope]]) %>% 
    summarize(score = sum(score, na.rm = TRUE)) %>% 
    arrange(desc(score))
  
  # Join everything together
  scores_table <- scores %>%
    full_join(prop_must_include) %>%
    full_join(prop_prob_or_must_include) %>%
    arrange(desc(score)) %>%
    mutate(
      across(where(is.numeric), ~ ifelse(is.na(.x), 0, .x)), 
      across(c(3:4), ~ format(round(.x, 2), nsmall = 2))
    ) %>%
    setNames(
      c(
        str_to_sentence(scope),
        'Score',
        'Proportion Must Include',
        'Proportion Must OR Probably Include'
      )
    )
  
  return(scores_table)
}



