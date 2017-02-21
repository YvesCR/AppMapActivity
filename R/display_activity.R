#'
#' This is a function which allows to create a map of the french activities by department.
#'
#' @param activity_to_display label of the activity
#' @importFrom tidyverse, ggplot2
#'
display_activity <- function(activity_to_display){
  # activity_to_display <- "Défense"

  # for that activity, the df of the count:
  dpt_select_activity <- AppMapActivity::dpt_count_act %>%
    filter(apen700_label == activity_to_display) %>%
    mutate(code_dept = dpt) %>%
    mutate(code_dept = ifelse(code_dept == "20", "2A", code_dept)
           , count = ifelse(code_dept == "20", count / 2, count))

  # we deal with the Corsica issue by dividing the department numbers in 2:
  dpt_select_activity_2a <- dpt_select_activity %>%
    filter(code_dept == "2A") %>%
    mutate(code_dept = "2B")

  dpt_select_activity <- rbind(dpt_select_activity, dpt_select_activity_2a)

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

  ggplot() +
    geom_polygon(data = dpt_p_det_act_sorted
                          , aes(x = long, y = lat, group = group, fill = count)
                          , size=.2, color = 'grey40') +
    scale_fill_gradientn(colours = colorRamps::matlab.like(10)) +
    ggtitle(paste0(activity_to_display)) +
    theme(axis.text = element_blank()
                   , axis.title = element_blank()
                   , panel.background = element_blank()
                   , panel.grid.major = element_blank()
                   , panel.grid.minor = element_blank()
                   , axis.ticks.length = grid::unit(0, "cm")
                   , panel.spacing = grid::unit(0, "lines")
                   , plot.margin = grid::unit(c(0, 0, 0, 0), "lines")
                   , plot.title = element_text(hjust = 0.5)
                   , complete = TRUE) +
    coord_map()
}
