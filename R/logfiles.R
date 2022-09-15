

#' Read Elveflow OB1 Pressure Controller Log File
#'
#' @param filename Path to the log file.
#'
#' @return Channel pressures in long format.
#' @export
#'
#' @importFrom rlang .data
logfile_read_pressure <- function(filename) {

  pressure <- readr::read_tsv(filename,
                              col_names = c("elapsed_time", "p1_target", "p1_read", "p2_target", "p2_read","p3_target", "p3_read", "p4_target", "p4_read", "drop"),
                              skip = 1, show_col_types = FALSE) %>%
    dplyr::select(-.data$drop) %>%
    tidyr::pivot_longer(-.data$elapsed_time, values_to = "mbar") %>%
    tidyr::separate(.data$name, c("channel", "value")) %>%
    # round elapsed time to one decimal digit
    dplyr::mutate("elapsed_time" = round(.data$elapsed_time, 1))



  return(pressure)

}



#' Read Cetoni Syringe Pump Log File
#'
#' @param filename Path to the log file.
#'
#' @return Syringe flowrates in long format.
#' @export
#'
#' @importFrom rlang .data
logfile_read_syringepump <- function(filename) {
  syringe <- readr::read_delim(filename, delim = ";",
                               col_names = c("elapsed_time", "timestamp", "q1", "q2", "q3"),
                               skip = 1,
                               show_col_types = FALSE) %>%
    dplyr::mutate("elapsed_time" = as.numeric(gsub(",", "\\.", .data$elapsed_time))) %>%
    tidyr::pivot_longer(c(.data$q1, .data$q2, .data$q3), names_to = "channel", values_to = "flowrate") %>%
    # round elapsed time to one decimal digit
    dplyr::mutate("elapsed_time" = round(.data$elapsed_time, 1))

  # change format of fractional seconds to YYYY-MM-DD HH:MM:SS.fff
  syringe <- syringe %>%
    dplyr::mutate("timestamp" = sub(":([^:]*)$", ".\\1", .data$timestamp)) %>%
    # convert into datetime
    dplyr::mutate("timestamp" = lubridate::ymd_hms(.data$timestamp))


  return(syringe)
}



#' Plot Syringe Pump Log
#'
#' @param syringepumpLog Syringe pump log.
#'
#' @return A ggplot object.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_line labs
#' @importFrom rlang .data
logfile_plot_syringepump <- function(syringepumpLog) {
  ggplot(syringepumpLog, aes(x = .data$elapsed_time/60, y = .data$flowrate, color = .data$channel)) +
    geom_line() +
    theme_pretty() +
    labs(x = "Elapsed time [min]",
         y = "Flowrate [ul/h]",
         color = "Channel")
}


#' Plot Pressure Log
#'
#' @param pressureLog Pressure log.
#'
#' @return A ggplot object.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_line labs
#' @importFrom rlang .data
logfile_plot_pressure <- function(pressureLog) {
  ggplot(pressureLog, aes(x = .data$elapsed_time/60, y = .data$mbar, color = .data$channel, linetype = .data$value)) +
    geom_line() +
    theme_pretty() +
    labs(x = "Elapsed time [min]",
         y = "Pressure [mbar]",
         color = "Channel",
         linetype = "Value")
}









#' Align Pressure to Syringe Log
#'
#' @param pressureLog TODO
#' @param syringeLog TODO
#' @param tPressure In seconds.
#' @param tSyringe In seconds.
#'
#' @return TODO
#' @export
#'
#' @importFrom rlang .data
logfile_align_pressure <- function(pressureLog, syringeLog, tPressure, tSyringe) {
  # to do:
  # - I don't have to touch the syringe log, only pressure
  # 1. subtract time difference in elapsed time
  # 2. copy absolute time stamps from the syringe log

  deltaT <- tPressure - tSyringe
  df <- pressureLog %>%
    dplyr::mutate("elapsed_time" = .data$elapsed_time - deltaT)

  # find the zero time:
  tZero <- unique(syringeLog[["timestamp"]][syringeLog[["elapsed_time"]] == 0])
  if (length(tZero) > 1) {
    message("There are more than one possible timestamps for elapsed_time == 0:")
    message(paste(tZero, collapse = ", "))
    message("The first of these entries is used")
    tZero <- tZero[1]
  }

  df <- df %>%
    dplyr::mutate("timestamp" = tZero + lubridate::seconds(.data$elapsed_time)) %>%
    # reorder columns
    dplyr::select(.data$elapsed_time, .data$timestamp, .data$channel, .data$value, .data$mbar)

  # return the corrected pressure log with absolute time stamps
  return(df)
}
