pacman::p_load(
  fmsb
)

get_vt_spiders <- function(df_list,
                           norm_type) {
  
  # Put inputs together to get set name
  set_ari <- paste0(norm_type, '_arithmetic')
  set_geo <- paste0(norm_type, '_geometric')
  
  par(mfrow = c(1, 2))
  
  walk2(
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
      dim_min <- map_dbl(.x[1:5], min)
      dim_max <- map_dbl(.x[1:5], max)
      
      # National average
      nat_avg <- .x %>% 
        filter(state == 'US') %>% 
        select(-state)
      
      # Vermont scores
      vt_dims <- .x %>% 
        filter(state == 'VT') %>% 
        select(-state)
      
      rbind(
        dim_max,
        dim_min,
        nat_avg,
        vt_dims
      ) %>% 
        radarchart(
          axistype = 1,
          
          # Polygon
          pcol = c('#b16286', '#427b58'),
          pfcol = c('#FFFFFF00', '#689d6a80'),
          plwd = c(2, 3),
          plty = c(2, 1),
          
          # grid
          cglcol = 'darkgrey',
          cglty = 1,
          axislabcol = 'darkgrey',
          # caxislabels = rep('', 5),
          
          # titles
          title = paste0('Vermont Dimension Scores\n', .y),
          
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
        text.col = "black",
        cex = 1,
        pt.cex = 2
      )
    })
  par(mfrow = c(1, 1))
}