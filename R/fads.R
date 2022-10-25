#' FADS Read TSV File
#'
#' @param fadsData Path to FADS CSV file.
#'
#' @return Tibble containing data about droplets identified by the sorter.
#' @export
#'
fads_read_tsv <- function(fadsData) {
  fads <- readr::read_tsv(fadsData, skip = 7, col_names = c("time", "blue", "green", "red", "width", "spacing"), show_col_types = FALSE, locale = readr::locale(decimal_mark = ","))
  return(fads)
}





#' FADS Plot Red vs. Green Max PMT
#'
#' TODO plot elapsed time
#'
#' @param fads Tibble as returned from \code{fads_read_tsv()}.
#' @param bins Argument passed on to \code{ggplot2::geom_bin2d()}.
#'
#' @return A ggplot object.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_hline geom_vline geom_bin2d scale_fill_continuous labs
#' @importFrom rlang .data
fads_plot_red_green <- function(fads, bins = 100) {
  # extract data for the heading
  # - elapsed time
  # - number of droplets
  t <- max(fads[["time"]]) - min(fads[["time"]])
  t <- t / 1000/60 # convert ms into s
  nDroplets <- nrow(fads)

  ggplot(fads, aes(x = .data$green, y = .data$red)) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    geom_bin2d(bins = bins) +
    scale_fill_continuous(type = "viridis") +
    theme_pretty() +
    labs(x = "Green max PMT signal [V]",
         y = "Red max PMT signal [V]",
         fill = "Number of Droplets",
         title = paste("Red vs. green signal"),
         subtitle = paste0("Total number of droplets: ", nDroplets, ", elapsed time: ", round(t, 2), " min"))
}



#' FADS Plot Red vs. Green Max PMT
#'
#' TODO plot elapsed time
#'
#' This is a copy of \code{fads_plot_red_green()}, just with hexagonal tiles.
#'
#' @param fads Tibble as returned from \code{fads_read_tsv()}.
#' @param bins Argument passed on to \code{ggplot2::geom_bin2d()}.
#'
#' @return A ggplot object.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_hline geom_vline geom_hex scale_fill_continuous labs
#' @importFrom rlang .data
fads_plot_red_green_hex <- function(fads, bins = 100) {
  # extract data for the heading
  # - elapsed time
  # - number of droplets
  t <- max(fads[["time"]]) - min(fads[["time"]])
  t <- t / 1000/60 # convert ms into s
  nDroplets <- nrow(fads)

  ggplot(fads, aes(x = .data$green, y = .data$red)) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    geom_hex(bins = bins) +
    scale_fill_continuous(type = "viridis") +
    theme_pretty() +
    labs(x = "Green max PMT signal [V]",
         y = "Red max PMT signal [V]",
         fill = "Number of Droplets",
         title = paste("Red vs. green signal"),
         subtitle = paste0("Total number of droplets: ", nDroplets, ", elapsed time: ", round(t, 2), " min"))
}



#' FADS Plot With against Red/Green Signal
#'
#' TODO plot elapsed time
#'
#' @param fads Tibble as returned from \code{fads_read_tsv()}.
#' @param y Name of variable to plot on the y-axis.
#' @param bins Argument passed on to \code{ggplot2::geom_bin2d()}.
#'
#' @return A ggplot object.
#' @export
#'
#' @importFrom ggplot2 ggplot aes_string geom_hline geom_vline geom_bin2d scale_fill_continuous labs
fads_plot_width <- function(fads, y = "red", bins = 100) {
  # extract data for the heading
  # - elapsed time
  # - number of droplets
  t <- max(fads[["time"]]) - min(fads[["time"]])
  t <- t / 1000/60 # convert ms into s
  nDroplets <- nrow(fads)

  ggplot(fads, aes_string(x = "width", y = y)) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    geom_bin2d(bins = bins) +
    scale_fill_continuous(type = "viridis") +
    theme_pretty() +
    labs(x = "Droplet signal width [ms]",
         y = paste0("Max PMT signal (", y, ") [V]"),
         fill = "Number of Droplets",
         title = paste(y, "vs. signal width"),
         subtitle = paste0("Total number of droplets: ", nDroplets, ", elapsed time: ", round(t, 2), " min"))
}


#' FADS Plot Width vs. Time
#'
#' @param fads Tibble as returned from \code{fads_read_tsv()}.
#'
#' @return A ggplot object.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point expand_limits
#' @importFrom rlang .data
fads_plot_width_time <- function(fads) {

  t <- max(fads[["time"]]) - min(fads[["time"]])
  t <- t / 1000/60 # convert ms into s
  nDroplets <- nrow(fads)

  fads %>%
    fads_format_time_minutes() %>%
    ggplot(aes(x = .data$time, y = .data$width)) +
    geom_point(shape = 1, alpha = 0.1) +
    theme_pretty() +
    labs(y = "Droplet signal width [ms]",
         x = "Elapsed time [min]",
         title = "Droplet signal width",
         subtitle = paste0("Total number of droplets: ", nDroplets, ", elapsed time: ", round(t, 2), " min")) +
    expand_limits(y = 0)
}



#' FADS Plot Width vs. Time
#'
#' @param fads Tibble as returned from \code{fads_read_tsv()}.
#'
#' @return A ggplot object.
#' @export
#'
#' @importFrom ggplot2 ggplot aes scale_color_viridis_d geom_point labs expand_limits
#' @importFrom rlang .data
fads_plot_signal_time <- function(fads) {
  t <- max(fads[["time"]]) - min(fads[["time"]])
  t <- t / 1000/60 # convert ms into s
  nDroplets <- nrow(fads)
  fads %>%
    fads_format_time_minutes() %>%
    tidyr::pivot_longer(c(.data$red, .data$green), names_to = "channel", values_to = "signal") %>%
    ggplot(aes(x = .data$time, y = .data$signal, color = .data$channel)) +
    scale_color_manual(values = c("red" = "#A2142F", "green" = "#77AC30")) +
    geom_point(shape = 1, alpha = 0.1) +
    theme_pretty() +
    labs(x = "Time [min]",
         y = "Max PMT signal [V]",
         color = "Channel",
         title = "Max PMT signal vs. time",
         subtitle = paste0("Total number of droplets: ", nDroplets, ", elapsed time: ", round(t, 2), " min")) +
    expand_limits(y = 0)

}



#' FADS Plot Droplet Frequency
#'
#' @param fads Tibble as returned from \code{fads_read_tsv()}.
#'
#' @return A ggplot object
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point labs
#' @importFrom rlang .data
fads_plot_frequency <- function(fads) {
  # bin fads data by seconds and calculate number of droplets per bin
  fads %>%
    fads_format_time_seconds() %>%
    dplyr::mutate("time" = round(.data$time)) %>%
    dplyr::group_by(.data$time) %>%
    dplyr::summarize("frequency" = dplyr::n()) %>%
    ggplot(aes(x = .data$time, y = .data$frequency)) +
    geom_point(alpha = 0.1) +
    labs(y = "Calculated frequency [Hz]",
         x = "Elapsed time [s]") +
    theme_pretty()
}



#' FADS Format Time Into Minutes
#'
#' @param fads TODO
#'
#' @return TODO
#' @importFrom rlang .data
fads_format_time_minutes <- function(fads) {
  fads %>%
    # start at t = 0
    dplyr::mutate("time" = .data$time - min(.data$time)) %>%
    # convert into minutes
    dplyr::mutate("time" = .data$time / 1000 / 60) %>%
    return()
}


#' FADS Format Time Into Seconds
#'
#' @param fads TODO
#'
#' @return TODO
#' @importFrom rlang .data
fads_format_time_seconds <- function(fads) {
  fads %>%
    # start at t = 0
    dplyr::mutate("time" = .data$time - min(.data$time)) %>%
    # convert into minutes
    dplyr::mutate("time" = .data$time / 1000) %>%
    return()
}



#' FADS Remove Outliers
#'
#' Removes rows that contain green or red outliers.
#'
#' @param fads TODO.
#'
#' @return TODO
#' @export
fads_remove_outliers <- function(fads) {
  greenOutliers <- fads_get_outliers(fads[["green"]])
  redOutliers <- fads_get_outliers(fads[["red"]])
  widthOutliers <- fads_get_outliers(fads[["width"]])
  # remove datapoint if either green or red signal is outlier
  outliers <- greenOutliers | redOutliers | widthOutliers

  return(fads[!outliers, ])
}


#' FADS Get Outliers
#'
#' Returns TRUE for those elements that are less than Q1 - scale * IQR or more
#' than Q3 + scale * IQR, and FALSE otherwise.
#'
#' @param numbers TODO
#' @param scale Scale factor that is used to calculate how much a datapoint can
#'   deviate from the bulk before it is considered an outlier.
#'
#' @return TODO
fads_get_outliers <- function(numbers, scale = 5) {
  # get Q1 and Q3
  q <- stats::quantile(numbers, probs = c(0.25, 0.75), na.rm = FALSE)
  # IQR = Q3-Q1
  iqr <- q[2] - q[1]
  # outlier defined as: Q3 + 1.5*IQR or Q1 - 1.5 * IQR
  upper <- q[2] + scale * iqr
  lower <- q[1] - scale * iqr
  outliers <- numbers > upper | numbers < lower

  return(outliers)
}



#' FADS Get Marker Opacity
#'
#' The purpose of this function is to determine a suitable alpha value between 0
#' and 1 so that scatter plots with a large number of points become meaningful.
#'
#'
#' @param fads TODO
#'
#' @return An opacity value between 0 and 1.
fads_marker_opacity <- function(fads) {


}
