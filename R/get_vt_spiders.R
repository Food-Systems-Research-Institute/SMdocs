#' Create Vermont spider/radar charts for dimension scores
#'
#' This function creates side-by-side spider (radar) charts comparing Vermont's
#' dimension scores to US national medians. It generates charts for both
#' arithmetic and geometric aggregations of a specified normalization type.
#'
#' @param df_list A named list containing score data, where each element
#'   corresponds to a method (e.g., "minmax_arithmetic", "minmax_geometric")
#'   and contains a `dimension_scores` data frame.
#' @param norm_type Character string specifying the normalization type prefix
#'   (e.g., "minmax", "zscore"). This will be combined with "_arithmetic" and
#'   "_geometric" to access the appropriate data.
#'
#' @return NULL (invisible). The function creates plots as a side effect using
#'   base R graphics.
#'
#' @details The function creates two radar charts side by side:
#'   \itemize{
#'     \item Left chart: Arithmetic aggregation scores
#'     \item Right chart: Geometric aggregation scores
#'   }
#'
#'   Each chart compares:
#'   \itemize{
#'     \item US national median (purple dashed line)
#'     \item Vermont scores (green solid line with fill)
#'   }
#'
#'   The charts use the first 5 columns as dimensions, expecting a 'state'
#'   column for filtering. The function looks for 'US_median' and 'VT' entries
#'   in the state column.
#'
#' @importFrom purrr walk2 map_dbl
#' @importFrom dplyr filter select
#' @importFrom stringr str_to_title
#' @importFrom snakecase to_title_case
#' @importFrom fmsb radarchart
#'
#' @examples
#' \dontrun{
#' # Assuming you have a scores list with minmax methods
#' scores_list <- list(
#'   minmax_arithmetic = list(
#'     dimension_scores = data.frame(
#'       state = c("US_median", "VT", "CA"),
#'       economic = c(50, 45, 55),
#'       social = c(50, 60, 40),
#'       environmental = c(50, 70, 35),
#'       governance = c(50, 55, 45),
#'       infrastructure = c(50, 40, 65)
#'     )
#'   ),
#'   minmax_geometric = list(dimension_scores = ...)
#' )
#'
#' # Create Vermont spider charts for minmax normalization
#' get_vt_spiders(scores_list, "minmax")
#' }
#'
#' @export
get_vt_spiders <- function(df_list,
                           norm_type) {
  # Put inputs together to get set name
  set_ari <- paste0(norm_type, '_arithmetic')
  set_geo <- paste0(norm_type, '_geometric')

  par(mfrow = c(1, 2))

  purrr::walk2(
    list(
      df_list[[set_ari]]$dimension_scores,
      df_list[[set_geo]]$dimension_scores
    ),
    list(
      paste(snakecase::to_title_case(set_ari)),
      paste(snakecase::to_title_case(set_geo))
    ),

    ~ {

      # Get min and max for each dimension
      dim_min <- purrr::map_dbl(.x[1:5], min)
      dim_max <- purrr::map_dbl(.x[1:5], max)

      # National average
      # nat_avg <- .x %>%
      #   filter(state == 'US_mean') %>%
      #   select(-state)

      # National median
      nat_median <- .x %>%
        dplyr::filter(state == 'US_median') %>%
        dplyr::select(-state)

      # Vermont scores
      vt_dims <- .x %>%
        dplyr::filter(state == 'VT') %>%
        dplyr::select(-state)

      rbind(
        dim_max,
        dim_min,
        # nat_avg,
        nat_median,
        vt_dims
      ) %>%
        fmsb::radarchart(
          axistype = 0,

          # Polygon
          pcol = c('#b16286', '#427b58'),
          # pcol = c('#b16286', '#d79921', '#427b58'),
          pfcol = c('#FFFFFF00', '#689d6a80'),
          plwd = c(2, 3),
          plty = c(2, 1),

          # grid
          cglcol = 'darkgrey',
          cglty = 1,
          axislabcol = 'darkgrey',

          # titles
          # title = paste0('Vermont Dimension Scores\\n', .y),
          title = stringr::str_to_title(.y), #

          # scaling
          calcex = 0.6,
          palcex = 0.9,
          vlcex =  1
        )

      legend(
        x = 1,
        y = 1.25,
        legend = c('US', 'VT'),
        bty = "n",
        pch = 20,
        col = c('#b16286', '#427b58'),
        # col = c('#b16286', '#d79921', '#427b58'),
        text.col = "black",
        cex = 1,
        pt.cex = 2
      )
    })
  par(mfrow = c(1, 1))
}


#' Create a single spider/radar chart for dimension scores
#'
#' This function creates a single radar chart comparing Vermont's dimension scores
#' to US national medians, with customizable title and automatic dimension name
#' formatting.
#'
#' @param df A data frame containing dimension scores with a 'state' column and
#'   5 dimension columns. Must contain 'US_median' and 'VT' entries in the
#'   state column.
#' @param title Character string specifying the title for the chart.
#'
#' @return NULL (invisible). The function creates a plot as a side effect using
#'   base R graphics.
#'
#' @details The function:
#'   \itemize{
#'     \item Converts the first 5 column names to title case for display
#'     \item Creates min/max boundaries for the radar chart
#'     \item Plots US median (purple dashed line) vs Vermont (green filled)
#'     \item Includes a legend distinguishing US vs VT data
#'   }
#'
#'   The chart uses consistent styling with the paired charts from
#'   \code{get_vt_spiders()}.
#'
#' @importFrom purrr map_dbl
#' @importFrom dplyr filter select
#' @importFrom stringr str_to_title
#' @importFrom fmsb radarchart
#' @importFrom graphics legend
#'
#' @examples
#' \dontrun{
#' # Single dimension scores data frame
#' dim_scores <- data.frame(
#'   economic = c(50, 45, 55),
#'   social = c(50, 60, 40),
#'   environmental = c(50, 70, 35),
#'   governance = c(50, 55, 45),
#'   infrastructure = c(50, 40, 65),
#'   state = c("US_median", "VT", "CA")
#' )
#'
#' # Create single spider chart
#' get_single_spider(dim_scores, "Vermont Dimension Scores")
#' }
#'
#' @export
get_single_spider <- function(df, title) {

  names(df)[1:5] <- stringr::str_to_title(names(df)[1:5])

  # Get min and max for each dimension
  dim_min <- purrr::map_dbl(df[1:5], min)
  dim_max <- purrr::map_dbl(df[1:5], max)

  # National median
  nat_median <- df %>%
    dplyr::filter(state == 'US_median') %>%
    dplyr::select(-state)

  # Vermont scores
  vt_dims <- df %>%
    dplyr::filter(state == 'VT') %>%
    dplyr::select(-state)

  rbind(
    dim_max,
    dim_min,
    nat_median,
    vt_dims
  ) %>%
    fmsb::radarchart(
      axistype = 0,

      # Polygon
      pcol = c('#b16286', '#427b58'),
      pfcol = c('#FFFFFF00', '#689d6a80'),
      plwd = c(2, 3),
      plty = c(2, 1),

      # grid
      cglcol = 'darkgrey',
      cglty = 1,
      axislabcol = 'darkgrey',

      # titles
      title = title,

      # scaling
      calcex = .6,
      palcex = .9,
      vlcex =  1
    )

  graphics::legend(
    x = 1,
    y = 1.25,
    legend = c('US', 'VT'),
    bty = "n",
    pch = 20,
    col = c('#b16286', '#427b58'),
    text.col = "black",
    cex = 1,
    pt.cex = 2
  )
}
