
#' Plot a given trajectory under all available filters
#'
#' This function loads a CSV containing tracking data. Given a particular body-part,
#' axis, and level of smoothing, the function plots the trajectory of the body-part
#' along that axis.
#'
#' @param csv_or_path Either a csv loaded as an R object, or the full path to that CSV.
#' @param p_cutoff The confidence value at which all tracked points below this
#' threshold are replaced by linear interpolation between two higher-confidence points.
#' @param reference_distance The 'real-world-length' between your reference objects (can be in mm, cm, etc).
#' If you indicate a `manual_scale_factor`, this setting will be overrided by that factor.
#' @param manual_scale_factor The 'real-world-length' to pixel conversion factor (i.e. millimeters/pixel).
#' If you are using two reference points, you can ignore this parameter.
#' @param fps The frames per second of your CSV.
#' @param savgol_window_length The rolling window length of Savitzky-Golay filter smoothing to apply
#' to your tracking trajectory.
#' @param median_window_length The rolling window length of median filter smoothing to apply
#' to your tracking trajectory.
#' @param average_window_length The rolling window length of average filter smoothing to apply
#' to your tracking trajectory.
#' @param body_part The body_part you wish to plot.
#' @param axis The axis you wish to plot.
#' @return A figure with panels (each indicating the plotted track under a given filter
#' and level and smoothing).
#' @import ggplot2
#' @import pracma
#' @import data.table
#' @import ggpubr
#' @export
plot_filter_graphs <- function(csv_or_path, p_cutoff, reference_distance = NA,
                               manual_scale_factor = NA, fps = 2000,
                               savgol_window_length = 25, median_window_length = 25,
                               average_window_length = 25, body_part = "center", axis = "y") {

  if (is.character(csv_or_path)) {
    csv <- read.csv(csv_or_path, header = FALSE)
  } else {
    csv <- csv_or_path
  }

  body_parts_in_raw_csv <- as.vector(unlist(csv[2,]))
  axes_in_raw_csv <- as.vector(unlist(csv[3,]))

  names(csv)[1] <- "scorer"

  for(bp in unique(body_parts_in_raw_csv)) {
    if (bp != "bodyparts") {
      start <- match(tolower(bp), tolower(body_parts_in_raw_csv))
      names(csv)[start] <- tolower(paste0(bp, "_x"))
      names(csv)[start+1] <- tolower(paste0(bp, "_y"))
      names(csv)[start+2] <- tolower(paste0(bp, "_p"))
      }
  }

  csv <- tail(csv, -3)



  if (body_part == "toe" & axis == "y") {
    tracking <- csv$toe_y
    conf <- csv$toe_p
  } else if (body_part == "toe" & axis == "x") {
    tracking <- csv$toe_x
    conf <- csv$toe_p
  } else if (body_part == "center" & axis == "y") {
    tracking <- csv$center_y
    conf <- csv$center_p
  } else if (body_part == "center" & axis == "x") {
    tracking <- csv$center_x
    conf <- csv$center_p
  } else if (body_part == "heel" & axis == "y") {
    tracking <- csv$heel_y
    conf <- csv$heel_p
  } else if (body_part == "heel" & axis == "x") {
    tracking <- csv$heel_x
    conf <- csv$heel_p
  } else {
    stop("invalid body_part or axis! (use toe, center, or heel for body part, and use x or y for axis)")
  }

  tracking <- as.numeric(tracking)
  conf <- as.numeric(conf)

  tracking[as.numeric(conf) < p_cutoff] <- NA
  tracking <- imputeTS::na_interpolation(tracking, option = "linear")

  if (is.na(manual_scale_factor) | is.null(manual_scale_factor)) {
    scale_factor <- reference_distance/sqrt(abs((mean(as.numeric(csv$objecta_x))-mean(as.numeric(csv$objectb_x)))^2)+abs((mean(as.numeric(csv$objecta_y))-mean(as.numeric(csv$objectb_y)))^2))
  } else {
    scale_factor <- manual_scale_factor
  }

  col <- as.numeric(tracking)

  if (axis == "x") {
    col <- scale_factor*col
  } else if (axis == "y") {
    col <- scale_factor*(-1*(col-max(col)))
  }


  # smoothing filters (savitzky-golay, running median, running average)

  col_savgol <- pracma::savgol(as.numeric(col), fl = savgol_window_length, forder = 3, dorder = 0)
  col_median <- runmed(as.numeric(col), k = median_window_length, endrule = c("keep"))
  col_average <- data.table::frollmean(as.numeric(col), n = average_window_length, algo = "exact", align = "center") # experimental, beware of NAs

  sample_tracking <- data.frame(csv$scorer, col, col_savgol, col_median, col_average, conf)

  a <- ggplot2::ggplot(data = sample_tracking) +
    geom_point(mapping = aes(x = as.numeric(csv.scorer)/fps, y = col, alpha = as.numeric(conf)), color = "blue") +
    labs(x = "time (s)",
         y = "distance (mm)",
         title = "Raw tracking",
         alpha = "Confidence") +
    theme_classic()

  b <- ggplot2::ggplot(data = sample_tracking) +
    geom_point(mapping = aes(x = as.numeric(csv.scorer)/fps, y = col_savgol,  alpha = as.numeric(conf)), color = "orange") +
    labs(x = "time (s)",
         y = "distance (mm)",
         title = paste0("Savitzky-Golay filter (size ", as.character(savgol_window_length), ")"),
         alpha = "Confidence") +
    theme_classic()

  c <- ggplot2::ggplot(data = sample_tracking) +
    geom_point(mapping = aes(x = as.numeric(csv.scorer)/fps, y = col_median, alpha = as.numeric(conf)), color = "darkred") +
    labs(x = "time (s)",
         y = "distance (mm)",
         title = paste0("Rolling Median filter (size ", as.character(median_window_length), ")"),
         alpha = "Confidence") +
    theme_classic()

  d <- ggplot2::ggplot(data = sample_tracking) +
    geom_point(mapping = aes(x = as.numeric(csv.scorer)/fps, y = col_average, alpha = as.numeric(conf)), color = "purple") +
    labs(x = "time (s)",
         y = "distance (mm)",
         title = paste0("Rolling Average filter (size ", as.character(average_window_length), ")"),
         alpha = "Confidence") +
    theme_classic()


  figure <- ggarrange(a, b, c, d,
            ncol = 2, nrow = 2,
            labels = c("A", "B", "C", "D"),
            legend = "bottom",
            common.legend = TRUE)

  return(figure)
}

#' Plot a univariate projection under a given filter
#'
#' This function loads a CSV containing tracking data. Given a particular body-part,
#' smoothing filter, and level of smoothing, the function plots the univariate
#' trajectory of the body-part along that axis.
#'
#' @param csv_or_path Either a csv loaded as an R object, or the full path to that CSV.
#' @param p_cutoff The confidence value at which all tracked points below this
#' threshold are replaced by linear interpolation between two higher-confidence points.
#' @param filter The filter chosen to smooth tracked trajectories.
#' Options are "none", "savitzky-golay", "median", or "average" (average recommended)
#' @param body_part The body_part you wish to plot.
#' @param reference_distance The 'real-world-length' between your reference objects (can be in mm, cm, etc).
#' If you indicate a `manual_scale_factor`, this setting will be overrided by that factor.
#' @param fps The frames per second of your CSV.
#' @param manual_scale_factor The 'real-world-length' to pixel conversion factor (i.e. millimeters/pixel).
#' If you are using two reference points, you can ignore this parameter.
#' @param savgol_window_length The rolling window length of Savitzky-Golay filter smoothing to apply
#' to your tracking trajectory.
#' @param median_window_length The rolling window length of median filter smoothing to apply
#' to your tracking trajectory.
#' @param average_window_length The rolling window length of average filter smoothing to apply
#' to your tracking trajectory.
#' @return A figure with diagnostic panels (indicating unannotated and annotated univariate projections,
#' and estimated horizontal and vertical positions and velocities).
#' @import ggplot2
#' @import pracma
#' @import data.table
#' @import ggpubr
#' @export
plot_univariate_projection <- function(csv_or_path, manual_scale_factor = NA, p_cutoff = 0, filter = "none", body_part = "center",
                                       reference_distance, fps = 2000, savgol_window_length = 11,
                                       median_window_length = 11, average_window_length = 11,
                                       shake_threshold = 0.35, window_threshold = 0.5) {

  params <- set_parameters(fps = fps,
                 shake.threshold = shake_threshold,
                 window.threshold = window_threshold)

  if (is.character(csv_or_path)) {
    csv <- read.csv(csv_or_path, header = FALSE)
  } else {
    csv <- csv_or_path
  }

  body_parts_in_raw_csv <- as.vector(unlist(csv[2,]))
  axes_in_raw_csv <- as.vector(unlist(csv[3,]))

  names(csv)[1] <- "scorer"

  for(bp in unique(body_parts_in_raw_csv)) {
    if (bp != "bodyparts") {
      start <- match(tolower(bp), tolower(body_parts_in_raw_csv))
      names(csv)[start] <- tolower(paste0(bp, "_x"))
      names(csv)[start+1] <- tolower(paste0(bp, "_y"))
      names(csv)[start+2] <- tolower(paste0(bp, "_p"))
    }
  }

  csv <- tail(csv, -3)


  if (body_part == "toe") {
    tracking_y <- csv$toe_y
    tracking_x <- csv$toe_x
    conf <- as.numeric(csv$toe_p)
  } else if (body_part == "center") {
    tracking_y <- csv$center_y
    tracking_x <- csv$center_x
    conf <- as.numeric(csv$center_p)
  } else if (body_part == "heel") {
    tracking_y <- csv$heel_y
    tracking_x <- csv$heel_x
    conf <- as.numeric(csv$heel_p)
  } else {
    stop("invalid body_part! (use toe, center, or heel for body part)")
  }

  tracking_x <- as.numeric(tracking_x)
  tracking_y <- as.numeric(tracking_y)

  tracking_x[conf < p_cutoff] <- NA
  tracking_x <- imputeTS::na_interpolation(tracking_x, option = "linear")
  tracking_y[conf < p_cutoff] <- NA
  tracking_y <- imputeTS::na_interpolation(tracking_y, option = "linear")

  if (is.na(manual_scale_factor)) {
    scale_factor <- reference_distance/sqrt(abs((mean(as.numeric(csv$objecta_x))-mean(as.numeric(csv$objectb_x)))^2)+abs((mean(as.numeric(csv$objecta_y))-mean(as.numeric(csv$objectb_y)))^2))
  } else {
    scale_factor <- manual_scale_factor
  }

  col_x <- scale_factor*(as.numeric(tracking_x))
  col_y <- scale_factor*(as.numeric(tracking_y))
  col_y <- max(col_y) - col_y

  # smoothing filters (savitzky-golay, running median, running average)

  col_savgol_x <- pracma::savgol(as.numeric(col_x), fl = savgol_window_length, forder = 3, dorder = 0)
  col_savgol_y <- pracma::savgol(as.numeric(col_y), fl = savgol_window_length, forder = 3, dorder = 0)

  col_median_x <- as.numeric(runmed(as.numeric(col_x), k = median_window_length, endrule = c("keep")))
  col_median_y <- as.numeric(runmed(as.numeric(col_y), k = median_window_length, endrule = c("keep")))

  col_average_x <- data.table::frollmean(as.numeric(col_x), n = average_window_length, algo = "exact", align = "center") # experimental, beware of NAs
  col_average_y <- data.table::frollmean(as.numeric(col_y), n = average_window_length, algo = "exact", align = "center") # experimental, beware of NAs

  sample_tracking <- data.frame(csv$scorer, col_x, col_y,
                                col_savgol_x, col_savgol_y,
                                col_median_x, col_median_y,
                                col_average_x, col_average_y,
                                conf)

  raw_features <- extract_features(x = sample_tracking$col_x, y = sample_tracking$col_y,
                                   diagnostics = TRUE, parameters = params)
  savgol_features <- extract_features(x = sample_tracking$col_savgol_x, y = sample_tracking$col_savgol_y,
                                      diagnostics = TRUE, parameters = params)
  median_features <- extract_features(x = sample_tracking$col_median_x, y = sample_tracking$col_median_y,
                                      diagnostics = TRUE, parameters = params)
  average_features <- extract_features(x = as.numeric(na.omit(sample_tracking$col_average_x)), y = as.numeric(na.omit(sample_tracking$col_average_y)),
                                      diagnostics = TRUE, parameters = params)

  plotlist <- list(raw_features,
                   savgol_features,
                   median_features,
                   average_features)

  if(tolower(filter) == "none") {
    plot(plotlist[[1]])
  } else if(tolower(filter) == "savitzky-golay") {
    plot(plotlist[[2]])
  } else if(tolower(filter) == "median") {
      plot(plotlist[[3]])
  } else if (tolower(filter) == "average") {
    plot(plotlist[[4]])
  } else {
    stop("Invalid filter name! (Use 'none', 'savitzky-golay', 'median', or 'average'")
  }

}


#' Run PAWS analysis for a single CSV (rather than in batch)
#'
#' This function loads a single CSV containing tracking data. The function then scores the tracked
#' data and outputs PAWS scores.
#'
#' @param csv_or_path Either a csv loaded as an R object, or the full path to that CSV.
#' @param p_cutoff The confidence value at which all tracked points below this
#' threshold are replaced by linear interpolation between two higher-confidence points.
#' @param filter The filter chosen to smooth tracked trajectories.
#' Options are "none", "savitzky-golay", "median", or "average" (average recommended)
#' @param body_part The body_parts you wish to plot.
#' @param reference_distance The 'real-world-length' between your reference objects (can be in mm, cm, etc).
#' If you indicate a `manual_scale_factor`, this setting will be overrided by that factor.
#' @param fps The frames per second of your CSV.
#' @param manual_scale_factor The 'real-world-length' to pixel conversion factor (i.e. millimeters/pixel).
#' If you are using two reference points, you can ignore this parameter.
#' @param savgol_window_length The rolling window length of Savitzky-Golay filter smoothing to apply
#' to your tracking trajectory.
#' @param median_window_length The rolling window length of median filter smoothing to apply
#' to your tracking trajectory.
#' @param average_window_length The rolling window length of average filter smoothing to apply
#' to your tracking trajectory.
#' @param window_threshold The threshold to examine a given window of tracking activity
#' for a withdrawal behavior. Higher values apply a more conservative window threshold.
#' @param shake_threshold The threshold to examine a given window of tracking activity
#' for shaking behaviors. Higher values apply a more conservative shaking threshold (Tip:
#' if shake segmentation does not match what you see in a video, you can fine-tune the threshold
#' to match your observations).
#' @return A data frame in RStudio containing PAWS scores for a single file.
#' @import ggplot2
#' @import pracma
#' @import data.table
#' @import ggpubr
#' @export
mini_paws <- function(csv_or_path, manual_scale_factor = NA, p_cutoff = 0, filter = "none", body_part = "center", reference_distance,
                      fps = 2000, savgol_window_length = 11, median_window_length = 11,
                      average_window_length = 11, shake_threshold = 0.35, window_threshold = 0.5) {


  params <- set_parameters(fps = fps,
                           shake.threshold = shake_threshold,
                           window.threshold = window_threshold)

  if (is.character(csv_or_path)) {
    csv <- read.csv(csv_or_path, header = FALSE)
  } else {
    csv <- csv_or_path
  }

  body_parts_in_raw_csv <- as.vector(unlist(csv[2,]))
  axes_in_raw_csv <- as.vector(unlist(csv[3,]))

  names(csv)[1] <- "scorer"

  for(bp in unique(body_parts_in_raw_csv)) {
    if (bp != "bodyparts") {
      start <- match(tolower(bp), tolower(body_parts_in_raw_csv))
      names(csv)[start] <- tolower(paste0(bp, "_x"))
      names(csv)[start+1] <- tolower(paste0(bp, "_y"))
      names(csv)[start+2] <- tolower(paste0(bp, "_p"))
    }
  }

  csv <- tail(csv, -3)

  if (body_part == "toe") {
    tracking_y <- csv$toe_y
    tracking_x <- csv$toe_x
    conf <- as.numeric(csv$toe_p)
  } else if (body_part == "center") {
    tracking_y <- csv$center_y
    tracking_x <- csv$center_x
    conf <- as.numeric(csv$center_p)
  } else if (body_part == "heel") {
    tracking_y <- csv$heel_y
    tracking_x <- csv$heel_x
    conf <- as.numeric(csv$heel_p)
  } else {
    stop("invalid body_part! (use toe, center, or heel for body part)")
  }

  tracking_x <- as.numeric(tracking_x)
  tracking_y <- as.numeric(tracking_y)

  tracking_x[conf < p_cutoff] <- NA
  tracking_x <- imputeTS::na_interpolation(tracking_x, option = "linear")
  tracking_y[conf < p_cutoff] <- NA
  tracking_y <- imputeTS::na_interpolation(tracking_y, option = "linear")

  if (is.na(manual_scale_factor)) {
    scale_factor <- reference_distance/sqrt(abs((mean(as.numeric(csv$objecta_x))-mean(as.numeric(csv$objectb_x)))^2)+abs((mean(as.numeric(csv$objecta_y))-mean(as.numeric(csv$objectb_y)))^2))
  } else {
    scale_factor <- manual_scale_factor
  }

  col_x <- scale_factor*(as.numeric(tracking_x))
  col_y <- scale_factor*(as.numeric(tracking_y))
  col_y <- max(col_y) - col_y

  # smoothing filters (savitzky-golay, running median, running average)

  if (tolower(filter) == "none") {
    sample_tracking <- data.frame(csv$scorer, col_x, col_y, conf)
    features <- extract_features(x = sample_tracking$col_x, y = sample_tracking$col_y,
                                     diagnostics = TRUE, parameters = params)
    ps_pre_peak <- pain_score(features, strains = "C57B6-", feature.set = "pre.peak")
    ps_post_peak <- pain_score(features, strains = "C57B6-", feature.set = "post.peak")
  } else if (tolower(filter) == "savitzky-golay") {
    col_x <- pracma::savgol(as.numeric(col_x), fl = savgol_window_length, forder = 3, dorder = 0)
    col_y <- pracma::savgol(as.numeric(col_y), fl = savgol_window_length, forder = 3, dorder = 0)
    sample_tracking <- data.frame(csv$scorer, col_x, col_y, conf)
    features <- extract_features(x = sample_tracking$col_x, y = sample_tracking$col_y,
                                        diagnostics = TRUE, parameters = params)
    ps_pre_peak <- pain_score(features, strains = "C57B6-", feature.set = "pre.peak")
    ps_post_peak <- pain_score(features, strains = "C57B6-", feature.set = "post.peak")
  } else if (tolower(filter) == "median") {
    col_x <- as.numeric(runmed(as.numeric(col_x), k = median_window_length, endrule = c("keep")))
    col_y <- as.numeric(runmed(as.numeric(col_y), k = median_window_length, endrule = c("keep")))
    sample_tracking <- data.frame(csv$scorer, col_x, col_y, conf)
    features <- extract_features(x = sample_tracking$col_x, y = sample_tracking$col_y,
                                        diagnostics = TRUE, parameters = params)
    ps_pre_peak <- pain_score(features, strains = "C57B6-", feature.set = "pre.peak")
    ps_post_peak <- pain_score(features, strains = "C57B6-", feature.set = "post.peak")
  } else if (tolower(filter) == "average") {
    col_x <- data.table::frollmean(as.numeric(col_x), n = average_window_length, algo = "exact", align = "center") # experimental, beware of NAs
    col_y <- data.table::frollmean(as.numeric(col_y), n = average_window_length, algo = "exact", align = "center") # experimental, beware of NAs
    sample_tracking <- data.frame(csv$scorer, col_x, col_y, conf)
    features <- extract_features(x = as.numeric(na.omit(sample_tracking$col_x)), y = as.numeric(na.omit(sample_tracking$col_y)),
                                         diagnostics = TRUE, parameters = params)
    ps_pre_peak <- pain_score(features, strains = "C57B6-", feature.set = "pre.peak")
    ps_post_peak <- pain_score(features, strains = "C57B6-", feature.set = "post.peak")
  }


  headers <- c("pre-pain_score", "post-pain_score", "pre-max_height", "pre-max_x_velocity", "pre-max_y_velocity", "pre-distance_traveled",
               "post-max_height", "post-max_x_velocity", "post-max_y_velocity", "post-distance_traveled",
               "post-number_of_shakes", "post-shaking_duration", "post-guarding_duration")

  df <- data.frame(matrix(ncol=13,nrow=0, dimnames=list(NULL, headers)))

  df[1,] <- c(ps_pre_peak, ps_post_peak, features$pre.peak$max.height, features$pre.peak$max.x.velocity,
                  features$pre.peak$max.y.velocity, features$pre.peak$distance.traveled,
                  features$post.peak$max.height, features$post.peak$max.x.velocity,
                  features$post.peak$max.y.velocity, features$post.peak$distance.traveled,
                  features$post.peak$number.of.shakes, features$post.peak$shaking.duration,
                  features$post.peak$guarding.duration)

  return(df)

}

