#'
#' This is a function which allows to create a map of the french activities by department.
#'
#' @param activity_to_display label of the activity
#' @importFrom tidyverse
#'
display_activity <- function(activity_to_display){
  # activity_to_display <- "1"
  activity_label <- AppMapActivity::lookup %>% filter(id == as.numeric(activity_to_display)) %>% select(apen700_label)

  # for that activity, the df of the count:
  dpt_select_activity <- AppMapActivity::dpt_count_act %>%
    filter(apen700_label == activity_label$apen700_label) %>%
    mutate(code_dept = dpt)

  dpt_p_det_act <- AppMapActivity::dpt_p_det %>%
    left_join(dpt_select_activity, by = "code_dept")

  ## special feature for the Parisian region and Corsica:
  idf_lim_dpt <- c("75", "92", "93", "94")

  dpt_p_det_act_no_idf <- dpt_p_det_act %>%
    filter(!code_dept %in% idf_lim_dpt)

  # multiplicative factor to increase the Parisian region size:
  factor_growth <- 4

  dpt_p_det_act_idf <- dpt_p_det_act %>%
    filter(code_dept %in% idf_lim_dpt) %>%
    mutate(long = factor_growth * (long - 6) + (2.348858 - 6) * (1 - factor_growth)
      , lat = factor_growth * (lat - 6) + (48.853226 - 6) * (1 - factor_growth))

  # center of Paris:(manual)
  # center_paris <- c(48.853226, 2.348858)

  dpt_p_det_act_sorted <- rbind(dpt_p_det_act_no_idf, NA, dpt_p_det_act_idf)
  dpt_p_det_act_sorted <- dpt_p_det_act_sorted %>% select(long, lat, group, count)

  # bit of cleaning
  rm(dpt_p_det_act_no_idf)
  rm(dpt_p_det_act_idf)
  rm(dpt_p_det_act)
  rm(dpt_select_activity)

  # fix encoding issue
  title <- paste0(activity_label$apen700_label)
  Encoding(title) <- "latin1"

  ggplot2::ggplot() +
    ggplot2::geom_polygon(data = dpt_p_det_act_sorted
                          , ggplot2::aes(x = long, y = lat, group = group, fill = count)
                          , size=.2, color = 'grey40') +
    ggplot2::scale_fill_gradientn(colours = colorRamps::matlab.like(10)) +
    ggplot2::ggtitle(title) +
    ggplot2::theme(axis.text = ggplot2::element_blank()
      , axis.title = ggplot2::element_blank()
      , panel.background = ggplot2::element_blank()
      , panel.grid.major = ggplot2::element_blank()
      , panel.grid.minor = ggplot2::element_blank()
      , axis.ticks.length = grid::unit(0, "cm")
      , panel.spacing = grid::unit(0, "lines")
      , plot.margin = grid::unit(c(0, 0, 0, 0), "lines")
      , plot.title = ggplot2::element_text(hjust = 0.5)
      , complete = TRUE) +
    ggplot2::coord_map()
}
