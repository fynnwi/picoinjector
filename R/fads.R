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
         subtitle = paste0("Total number of droplets: ", nDroplets))
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
         subtitle = paste0("Total number of droplets: ", nDroplets))
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
         subtitle = paste0("Total number of droplets: ", nDroplets))
}


#' FADS Plot Width vs. Time
#'
#' @param fads Tibble as returned from \code{fads_read_tsv()}.
#'
#' @return A ggplot object.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point
#' @importFrom rlang .data
fads_plot_width_time <- function(fads) {
  ggplot(fads, aes(x = .data$time, y = .data$width)) +
    geom_point(shape = 1, alpha = 0.1) +
    theme_pretty()
}



#' FADS Plot Width vs. Time
#'
#' @param fads Tibble as returned from \code{fads_read_tsv()}.
#'
#' @return A ggplot object.
#' @export
#'
#' @importFrom ggplot2 ggplot aes scale_color_viridis_d geom_point labs
#' @importFrom rlang .data
fads_plot_signal_time <- function(fads) {
  fads %>%
    tidyr::pivot_longer(c(red, green), names_to = "channel", values_to = "signal") %>%
    ggplot(aes(x = .data$time, y = .data$signal, color = .data$channel)) +
    scale_color_viridis_d() +
    geom_point(shape = 1, alpha = 0.1) +
    theme_pretty() +
    labs(x = "Time [ms]",
         y = "Max PMT signal [V]",
         color = "Channel")
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
    dplyr::mutate("seconds" = round(.data$time / 1000)) %>%
    dplyr::mutate("seconds" = .data$seconds - min(.data$seconds)) %>%
    dplyr::group_by(.data$seconds) %>%
    dplyr::summarize("frequency" = dplyr::n()) %>%
    ggplot(aes(x = .data$seconds, y = .data$frequency)) +
    geom_point(alpha = 0.1) +
    labs(y = "Calculated frequency [Hz]",
         x = "Elapsed time [s]") +
    theme_pretty()
}
