# Giant Tables
# 2025-09-14

# Make two giant latex tables from giant_table made in excel_processing.R


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  stringr,
  knitr,
  kableExtra,
  RefManageR,
  fuzzyjoin,
  tidyr
)

pacman::p_load_current_gh('ChrisDonovan307/projecter')
devtools::load_all()



# Body Table --------------------------------------------------------------


# TODO: Pull this out into a different script. The body table depends on graphs
# that we put together in dp_trends. We need to run it after that. Whereas
# this script needs to be run before any of the dp_* pages.

# Pulling from giant table, here we pull out relevant columns for the table that
# goes into the body of the paper and make a latex table. We are also putting in a placeholder column for our trend graphs.


giant_tab <- SMdocs::giant_table
get_str(giant_tab)

# # Formatting for merges. Note this apparently has to be manual once page
# # breaks are clear.
% Dimensions
cell{2}{1}={r=7}{l},
cell{9}{1}={r=5}{l},
cell{14}{1}={r=3}{l},
cell{17}{1}={r=3}{l},
% Indicators
cell{7}{2}={r=2}{l},
cell{11}{2}={r=3}{l},
cell{17}{2}={r=3}{l},

# Wrangle table
trend_files <- dir('outputs/trend_plots/')
trend_file_vars <- trend_files %>%
  str_remove_all('fig_trend_') %>%
  str_remove_all('\\.png')
get_str(giant_tab)
body_table <- giant_tab %>%
  select(
    dimension,
    indicator,
    metric,
    definition,
    units,
    mean,
    sd,
    scale = resolution,
    weighting,
    source,
    variable_name # use this to link to trend graph, then drop
  ) %>%
  mutate(
    # Paste together mean and sd into one col
    across(c(mean, sd), ~ round(.x, 2)),
    mean_sd = case_when(
      !is.na(mean) & !is.na(sd) ~ paste0(mean, ' (', sd, ')'),
      .default = NA
    ),

    # variable_name = ifelse(is.na(variable_name), '', variable_name),

    # Where we have a metric, include an image of trend
    trend = case_when(
      metric != 'NONE' & variable_name %in% trend_file_vars ~
        paste0(
          '\\includegraphics[width=\\hsize,valign=c]{fig_trend_',
          variable_name,
          '.png}'
        ),
      .default = NA_character_
    ),
      # & str_detect(paste(trend_files, collapse = "|"), variable_name)) ~

    # Make indicators italic
    indicator = paste0('\\textit{', indicator, '}'),

    # Manually escape (so that we don't excape in kbl())
    across(
      everything(),
      ~ .x %>%
        str_replace_all('%', ' percent') %>%
        str_replace_all('\\$', ' dollars')
    ),

    # Put a space between Tradition and Heritage
    indicator = case_when(
      str_detect(indicator, 'Tradition') ~ 'Tradition and Heritage',
      .default = indicator
    ),

    # Fix formatting of weighting
    weighting = weighting %>%
      str_to_title() %>%
      str_replace_all('none', 'None') %>%
      str_replace_all('Gdp', 'GDP') %>%
      str_replace_all('Of', 'of'),

    # Put scale (resolution) into title case
    scale = str_to_title(scale)
  ) %>%

  # Final order of columns
  select(
    dimension,
    indicator,
    metric,
    definition,
    mean_sd,
    units,
    scale,
    trend,
    weighting,
    source
  ) %>%

  # Format headers
  setNames(c(names(.) %>% snakecase::to_title_case())) %>%
  rename('Metric Definition' = Definition) %>%
  rename('$\\mu (\\sigma)$' = 'Mean Sd') %>%

  # Missing cells should all be \textemdash
  mutate(across(everything(), ~ case_when(
    .x %in% c('NONE', 'None', 'none') | is.na(.x) ~ '\\textemdash',
    .default = .x
  )))

get_str(body_table)
body_table$Trend

# Get baseline table
body_latex <- body_table %>%
  kbl(
    format = 'latex',
    caption = 'Metric Attributes and Trends',
    escape = FALSE
  ) %>%
  kable_styling(
    font_size = 10
  )
body_latex

# Remove existing table formatting
body <- body_latex %>%
  str_split_i("\\\\begin\\{tabular\\}\\[t\\]\\{[^}]+\\}\\n", 2) %>%
  str_replace("^\\\\begin\\{tabular\\}\\[t\\]\\{[^}]+\\}\\s*", "") %>%
  str_remove_all('\\\\hline\n') %>%
  str_remove('\\\\end\\{tabular\\}\n\\\\end\\{table\\}')
body %>%
  cat()

# Add our own header and footer
    # dimension,
    # indicator,
    # metric,
    # definition,
    # mean_sd,
    # units,
    # trend,
    # weighting,
    # source
header <- '\\begin{landscape}
\\scriptsize
\\begin{longtblr}[
  placement = htbp,
  caption = Metric Attributes and Data Sources,
  label = {tab:tab_metrics_body},
  remark{Note} = {
    Metrics were collected at the county level where possible,
    otherwise at the state level. These scales are represented accordingly by $\\mu$
    and $\\sigma$. Trend graphs show locally estimated regression lines in each metric
    from 2000-2024 in blue, with state or counties in grey.
    Metrics in units of USD were inflation adjusted to 2024 using the CPI.
    Metrics without a trend graph were available for only a
    single year. Weighting variables were used in regression analyses only.
    Smoothed weights were 5-year metrics from USDA NASS or US Census Bureau
    ACS-5 and interpolated linearly between data points.
  }
]{
  % colspec={Q[50] Q[200] Q[200] Q[200]}, % Column widths,
  % Dimension cell merges
  cell{2}{1}={r=8}{l},
  cell{10}{1}={r=7}{l}, % econ to env (but clean, next is env)
  cell{17}{1}={r=7}{l},
  cell{24}{1}={r=8}{l},
  cell{32}{1}={r=4}{l}, % env to health
  cell{36}{1}={r=2}{l},
  cell{38}{1}={r=6}{l},
  cell{44}{1}={r=6}{l},
  cell{50}{1}={r=3}{l}, % health to prod
  cell{53}{1}={r=3}{l},
  cell{56}{1}={r=6}{l},
  cell{62}{1}={r=5}{l}, % prod to social
  cell{67}{1}={r=4}{l},
  cell{71}{1}={r=8}{l},
  cell{79}{1}={r=8}{l},
  % Indicators
  cell{7}{2}={r=2}{l},
  cell{11}{2}={r=3}{l},
  cell{17}{2}={r=3}{l},
  cell{22}{2}={r=2}{l},
  cell{28}{2}={r=2}{l},
  cell{32}{2}={r=2}{l},
  cell{34}{2}={r=2}{l},
  cell{47}{2}={r=2}{l},
  cell{51}{2}={r=2}{l},
  cell{63}{2}={r=3}{l},
  %
  rowhead = 1,
  row{1} = {font=\\bfseries},
  colsep = 1pt, % Spaces between column lines and text/figure
  cells={halign=c,valign=m}, % Center horizontal and vertical
  column{1-2}={wd=1.75cm}, % dimension, indicator
  column{3}={wd=2cm}, % metric
  column{4}={wd=4cm}, % definition
  column{5}={wd=1.75cm}, % mean, sd
  column{6}={wd=1.5cm}, % units
  column{7}={wd=1.25cm}, % scale
  column{8}={wd=1.25cm}, % trend
  column{9}={wd=1.75cm}, % weighting
  column{10}={wd=3cm}, % source
  vlines,
  % hline{1,2,Z}={solid} % for only header and footer
  hlines, % all horizontal lines
}
'

footer <- '\\end{longtblr}
\\end{landscape}'

# Put them all together
body_out <- paste0(header, body, footer)
cat(body_out)

# Save this to latex file
writeLines(body_out, 'outputs/tab_metrics_body.tex')



# Appendix Table ----------------------------------------------------------
## Ref Manager -------------------------------------------------------------


# Pull from bibtex file to convert shorthand citations in appendix table
bib_file <- ReadBib('dev/sm_data_survey.bib')
bib <- imap(bib_file, ~ {
  if (length(.x$author) > 2) {
    label = paste0(.x$author$family[[1]], " et al. (", .x$year, ")")
  } else if (length(.x$author) == 2) {
    label = paste0(.x$author$family[[1]], ' \\& ', .x$author$family[[2]], "  (", .x$year, ")")
  } else if (length(.x$author) == 1) {
    label = paste0(.x$author$family[[1]], " (", .x$year, ")")
  } else {
    return(NULL)
  }
  data.frame(
    key = .y,
    label = label
  )
}) %>%
  # setNames(c(names(bib_file))) %>%
  purrr::discard(is.null) %>%
  dplyr::bind_rows()
get_str(bib)



## Start Table -------------------------------------------------------------


#' To include:
#'  indicator
#'  metric
#'  n_counties
#'  n_years
#'  year_range
#'  resolution
#'  updates
#'  supporting literature
get_str(giant_table)
app_table <- SMdocs::giant_table %>%
  filter(
    !is.na(metric),
    metric != 'NONE',
    !is.na(variable_name)
  ) %>%
  select(
    indicator,
    metric,
    updates,
    desirable,
    states = n_states,
    counties = n_counties,
    years = n_years,
    range = year_range,
    citations = shorthand_citations
  ) %>%
  mutate(
    desirable = case_when(
      str_detect(desirable, 'lower') ~ 'lower',
      str_detect(desirable, 'higher') ~ 'higher',
      str_detect(desirable, 'target') ~ 'target',
      .default = NA
    ),
    across(c(updates, desirable), ~ str_to_title(.x)),
    across(everything(), ~ str_replace_all(.x, '&', '\\\\&')),
  ) %>%
  setNames(c(str_to_title(names(.))))
get_str(app_table)
app_table
app_table$Desirable



## Fixes -------------------------------------------------------------------

# Consumer price index - turn NAs into dashes
app_table <- app_table %>%
  mutate(
    across(c(Desirable, States, Counties, Years, Range, Citations),
    ~ case_when(
      Metric == 'Consumer Price Index for food at home' ~ '\\textemdash',
      .default = as.character(.x)
    )),
    Citations = case_when(
      is.na(Citations) | Citations == 'NA' ~ '\\textemdash',
      .default = Citations
    )
  )
app_table %>%
  filter(Metric == 'Consumer Price Index for food at home')
get_str(app_table)



## Citation Matching -------------------------------------------------------


# Match Citations column with key from bib files
get_str(bib)
get_str(app_table)

# Split citations on commas
app_table_long <- app_table %>%
  mutate(
    Citations = str_split(Citations, ","),
    row_id = row_number() # Use this to group by later
  ) %>%
  tidyr::unnest(Citations) %>%
  mutate(Citations = str_trim(Citations))
get_str(app_table_long)

# Join long table to bib labels and keys
app_table_long <- app_table_long %>%
  stringdist_left_join(bib, by = c('Citations' = 'label'), max_dist = 2)
app_table_long %>%
  select(Citations, label, key)
# Check here

# Fixes
app_table_long$key[app_table_long$Citations == 'Jones et al. (2016)'] <-
  'jones2016SystematicReviewMeasurement'
app_table_long %>%
  select(Citations, label, key)

# Collapse back down, make list cols for keys
keys <- app_table_long %>%
  group_by(row_id) %>%
  summarize(Citations = paste0(key, collapse = ','))
get_str(keys)

# Format citations
keys <- keys %>%
  mutate(
    Citations = case_when(
      is.na(Citations) | Citations == 'NA' ~ '\\textemdash',
      !is.na(Citations) ~ paste0('\\cite{', Citations, '}'),
      .default = NA
    )
  )
keys$Citations %>% head

# Join keys back to app_table
get_str(app_table)
get_str(keys)

app_table$Citations <- keys$Citations
get_str(app_table)



## Latex -------------------------------------------------------------------


# Get baseline table
app_latex <- app_table %>%
  kbl(
    format = 'latex',
    caption = 'Supplemental Metric Data',
    escape = FALSE
  ) %>%
  kable_styling(
    font_size = 10
  )
app_latex

# Remove existing table formatting
app_body <- app_latex %>%
  str_split_i("\\\\begin\\{tabular\\}\\[t\\]\\{[^}]+\\}\\n", 2) %>%
  str_replace("^\\\\begin\\{tabular\\}\\[t\\]\\{[^}]+\\}\\s*", "") %>%
  str_remove_all('\\\\hline\n') %>%
  str_remove('\\\\end\\{tabular\\}\n\\\\end\\{table\\}')
cat(app_body)

# Add our own header and footer
# \\begin{longtblr}[
#   caption = Metric Attributes and Data Sources,
#   label = {tab:tab_metrics_body},
#   remark{Note} = {
#     Metrics were collected at the county level where possible,
#     otherwise at the state level. These scales are represented accordingly by $\\mu$
#     and $\\sigma$. Trend graphs show locally estimated regression lines in each metric
#     from 2000-2024 in blue, with state or counties in grey.
#     Metrics in units of USD were inflation adjusted to 2024 using the CPI.
#     Metrics without a trend graph were available for only a
#     single year. Weighting variables were used in regression analyses only.
#     Smoothed weights were 5-year metrics from USDA NASS or US Census Bureau
#     ACS-5 and interpolated linearly between data points.
#   }
# ]{
app_header <- '\\begin{landscape}
\\scriptsize
\\begin{longtblr}[
  caption = Supplementary Metric Information,
  label = {tab:tab_metrics_appendix},
  remark{Note} = {Resolution reflects the finest scale to which data were
  calculated for analyses. Metrics derived from spatial datasets are available
  at finer scales. The Desirable column shows the direction of change that the
  authors considered beneficial. Metrics were excluded from trend analyses if
  the desirable state was a target. The States and Counties columns show the
  number of each that the metric is available for, out of a total of 9 states
  and 209 counties in the Northeast, not including Connecticut. Years describes
  the total number of years represented by each metric, and Range describes the
  difference between the first and last year available. The Citations column
  includes literature supporting the value of the indicator.}
]{
  rowhead = 1,
  row{1} = {font=\\bfseries},
  colsep = 2pt, % Spaces between column lines and text/figure
  cells={halign=c,valign=m}, % Center horizontal and vertical
  column{1-3}={wd=2cm}, % indicator, metric, updates
  column{4}={wd=1.5cm}, % Desirable
  column{5-8}={wd=1.75cm}, % States, counties, years, range
  column{9}={wd=3.25cm}, % Citations
  vlines,
  % hline{1,2,Z}={solid} % for only header and footer
  hlines, % all horizontal lines
}
'

app_footer <- '\\end{longtblr}
\\end{landscape}'

# Put them all together
app_body_out <- paste0(app_header, app_body, app_footer)
cat(app_body_out)

# Save this to latex file
writeLines(app_body_out, 'outputs/tab_metrics_appendix.tex')
