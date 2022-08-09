
#' Plot Height Profile
#'
#' @param heights Dataframe containing columnes 'position' and 'height'
#'
#' @return A ggplot object
#' @export
#'
#' @importFrom ggplot2 ggplot aes_string geom_line labs
plot_heightprofile <- function(heights) {
  p <- ggplot(heights, aes_string(x = "position", y = "height")) +
    geom_line() +
    labs(x = "Lateral position [um]",
         y = "Height [nm]") +
    theme_pretty()
  return(p)
}




#' Plot Height Profiles By Wafer
#'
#' @param heights TODO
#'
#' @return A ggplot object.
#' @export
#'
plot_heightprofile_bywafer <- function(heights) {
  p <- plot_heightprofile(heights) +
    ggplot2::facet_grid(wafer ~ direction)
  return(p)
}
