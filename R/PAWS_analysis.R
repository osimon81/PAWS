
#' Run PAWS analysis on a full set of CSVs in batch
#'
#' This function is used to run PAWS analysis either in a custom script or in the
#' dashboard. `paws_analysis` will use custom parameters to group all tracked data
#' into a single CSV containing pre- and post-peak metrics.
#'
#' @param csv_directory Path to the folder containing individual tracked CSVs.
#' @param save_directory Path to the folder where PAWS output file should be saved.
#' @param p_cutoff The confidence value at which all tracked points below this
#' threshold are replaced by linear interpolation between two higher-confidence points.
#' @param manual_scale_factor The 'real-world-length' to pixel conversion factor (i.e. millimeters/pixel).
#' If you are using two reference points, you can ignore this parameter.
#' @param filter_chosen The filter chosen to smooth tracked trajectories.
#' Options are "none", "savitzky-golay", "median", or "average" (average recommended)
#' @param reference_distance The 'real-world-length' between your reference objects (can be in mm, cm, etc).
#' If you indicate a `manual_scale_factor`, this setting will be overrided by that factor.
#' @param stims The word/acronym you use to indicate stimulation type in your tracked CSVs.
#' If you have a set of tracked CSVs with dynamic brush and light pinprick
#' stimulations (named DB_1.csv and LP_2.csv), set `stims` to `c("DB", "LP")`. This
#' parameter enables PAWS to automatically and adaptively group scored data by stimulus
#' in the final output file, so you don't have to.
#' @param body_parts The names of the body-part(s) you wish to score
#' using PAWS. If you label the paw with body-parts "toe," "center," and "heel,"
#' you should set `body_parts` to `c("toe", "center", "heel")`.
#' @param reference_points The names of the two reference points you wish to use
#' to adaptively scale videos to their appropriate real-world dimensions. If you
#' do not choose to have reference points (and instead wish to apply a manual scale
#' factor to every video), you can ignore this setting.
#' @param groups The names of the groups you wish to separate out (based on the filenames
#' of the CSVs you provide). This parameter enables PAWS to automatically and adaptively
#' group scored data by experimental group in the final output file, so you don't have to.
#' @param fps The frames per second of your set of CSVs
#' @param window_threshold The threshold to examine a given window of tracking activity
#' for a withdrawal behavior. Higher values apply a more conservative window threshold.
#' @param shake_threshold The threshold to examine a given window of tracking activity
#' for shaking behaviors. Higher values apply a more conservative shaking threshold (Tip:
#' if shake segmentation does not match what you see in a video, you can fine-tune the threshold
#' to match your observations).
#' @param fixed_baseline The height (in units of choice) above the lowest y-axis position of the paw, used
#' to determine the baseline for activity. the `y_threshold` parameter is used to set a baseline for this
#' level of activity.
#' @param y_threshold The threshold (in units of choice) above the fixed baseline at which the start and end
#' time-points of activity are determined.
#' @param stimulus_velocity_factor The 'b' term in a*sin(b(x - c)) + d used to fit the stimulus sine function.
#' @param stimulus_displacement_factor The 'c' term in a*sin(b(x - c)) + d -- number of seconds
#' after video starts until the stimulus contacts the paw.
#' @param expanded_analysis Whether or not to export additional PAWS metrics (t-star, withdrawal latency)
#' alongside the usual pain score metrics.
#' @return A single CSV grouped by both stimulus and experimental group, containing PAWS metrics for each body-part.
#' @import ggplot2
#' @importFrom pracma savgol
#' @importFrom data.table frollmean
#' @import ggpubr
#' @import stringr
#' @import dplyr
#' @import pawscore
#' @export
paws_analysis <- function(csv_directory, save_directory, p_cutoff = 0.30,
                          manual_scale_factor = NA, filter_chosen = "average", filter_length = 11,
                          reference_distance = 40, stims = c("cb", "db", "lp", "hp"),
                          body_parts = c("toe", "center", "heel"),
                          reference_points = c("objecta", "objectb"),
                          groups, fps = 2000, window_threshold = 0.5,
                          fixed_baseline = 3,
                          y_threshold = 1.5,
                          shake_threshold = 0.35,
                          savgol_filter_smoothing_multiplier = 3,
                          expanded_analysis = FALSE,
                          stimulus_velocity_factor = 0.0798,
                          stimulus_displacement_factor = 0.17) {

  start_time <- Sys.time()

  headers <- c("pre-max_height", "pre-max_x_velocity", "pre-max_y_velocity", "pre-distance_traveled",
               "post-max_height", "post-max_x_velocity", "post-max_y_velocity", "post-distance_traveled",
               "post-number_of_shakes", "post-shaking_duration", "post-guarding_duration", "body_part", "file",
               "stimulus", "group", "pre-pain_score", "post-pain_score")

  combined_dataframe <- data.frame(matrix(ncol=17,nrow=0, dimnames=list(NULL, headers)))

  if (expanded_analysis == TRUE) {
    # accommodate extra rows for t* and withdrawal latency measurements

    headers <- c("pre-max_height", "pre-max_x_velocity", "pre-max_y_velocity", "pre-distance_traveled",
                 "post-max_height", "post-max_x_velocity", "post-max_y_velocity", "post-distance_traveled",
                 "post-number_of_shakes", "post-shaking_duration", "post-guarding_duration", "body_part", "file",
                 "stimulus", "group", "pre-pain_score", "post-pain_score", "withdrawal_latency")

    combined_dataframe <- data.frame(matrix(ncol=18,nrow=0, dimnames=list(NULL, headers)))
  }

  params <- set_parameters(fps = 2000, # FIX THIS WHEN FPS DOWNSAMPLING IS PATCHED
                           shake.threshold = shake_threshold,
                           window.threshold = window_threshold,
                           fixed.baseline = list(y = fixed_baseline,
                                                 threshold = y_threshold))

  # Sets the initial index value to 1, corresponding to the first CSV in the
  # folder, and sets the directory.

  index = 1

  file_names <- list.files(csv_directory, pattern="*.csv", recursive = TRUE,
                      full.names = FALSE)
  file_paths <- list.files(csv_directory, pattern="*.csv", recursive = TRUE,
                      full.names = TRUE)

  for (i in 1:length(file_paths)) {

    skip_to_next <- FALSE
    message("Loading file: ", file_names[i])

    # Assign stimulation types, assign groups, and assign NAs if no ID detected

    stim <- stims[which(str_detect(tolower(substr(file_names[index],1,nchar(file_names[index])-3)), tolower(stims)))]

    group_val <- NA

    for (group_index in 1:length(groups)) {
      int <- grep(paste0("\\", groups[group_index]), file_names[index])
      if (length(int) == 1) {
        group_val <- group_index
        break
      }
    }

    group <- groups[group_val]

    if (length(stim) == 0) {
      stim = "no_stim_ID"
    }

    if (length(group) == 0) {
      group = "no_group_ID"
    }

    # Set dataframe columns to correspond with DLC data columns

    raw_csv <- read.csv(file_paths[i], comment.char = '#', header = FALSE)

    # remove trimmings at the top of CSVs, tidy up and rename data frame

    body_parts_in_raw_csv <- as.vector(unlist(raw_csv[2,]))
    axes_in_raw_csv <- as.vector(unlist(raw_csv[3,]))

    names(raw_csv)[1] <- "index"

    for (body_part_index in 1:length(body_parts)) {
      start <- match(tolower(body_parts[body_part_index]), tolower(body_parts_in_raw_csv))
      names(raw_csv)[start] <- paste0(body_parts[body_part_index], "_x")
      names(raw_csv)[start+1] <- paste0(body_parts[body_part_index], "_y")
      names(raw_csv)[start+2] <- paste0(body_parts[body_part_index], "_likelihood")
    }


    if (is.na(manual_scale_factor)) {
      for (reference_point_index in 1:length(reference_points)) {
        start <- match(reference_points[reference_point_index], body_parts_in_raw_csv)
        names(raw_csv)[start] <- paste0(reference_points[reference_point_index], "_x")
        names(raw_csv)[start+1] <- paste0(reference_points[reference_point_index], "_y")
        names(raw_csv)[start+2] <- paste0(reference_points[reference_point_index], "_likelihood")
      }
    }

    raw_csv <- raw_csv[-c(1:3),]
    raw_csv <- as.data.frame(sapply(raw_csv, as.numeric))

    # create tracks object which stores trajectories and likelihoods for entire video

    tracks <- list()
    reference <- list()

    for (body_part in 1:length(body_parts)) {
      tracks <- append(tracks, list(list(x = list(NA), y = list(NA), p = list(NA))))
    }

    if (is.na(manual_scale_factor)) {
      for (reference_point in 1:length(reference_points)) {
        reference <- append(reference, list(list(x = list(NA), y = list(NA), p = list(NA))))
      }
      names(reference) <- reference_points
    }


    names(tracks) <- body_parts


    for (body_part in body_parts) {
      tracks[[body_part]][[1]] <- raw_csv[[paste0(body_part, "_x")]]
      tracks[[body_part]][[2]] <- raw_csv[[paste0(body_part, "_y")]]
      tracks[[body_part]][[3]] <- raw_csv[[paste0(body_part, "_likelihood")]]
    }

    if (is.na(manual_scale_factor)) {
      for (reference_point in reference_points) {
        reference[[reference_point]][[1]] <- raw_csv[[paste0(reference_point, "_x")]]
        reference[[reference_point]][[2]] <- raw_csv[[paste0(reference_point, "_y")]]
        reference[[reference_point]][[3]] <- raw_csv[[paste0(reference_point, "_likelihood")]]
      }
    }

    # Inverting the y-axis for regions so the lowest position is 0

    for (body_part in body_parts) {
      tracks[[body_part]][['y']] <- max(na.omit(tracks[[body_part]][['y']])) - tracks[[body_part]][['y']]
    }


    # Omitting tracking data below the p-cutoff threshold

    for (body_part in body_parts) {
      x_track <- tracks[[body_part]][['x']]
      x_track[which(tracks[[body_part]][['p']] < p_cutoff)] <- NA
      tryCatch(tracks[[body_part]][['x']] <- imputeTS::na_interpolation(x_track, option = "linear"),
               error = function(e) { skip_to_next <- TRUE})
      if(skip_to_next) { next }
      y_track <- tracks[[body_part]][['y']]
      y_track[which(tracks[[body_part]][['p']] < p_cutoff)] <- NA
      tryCatch(tracks[[body_part]][['y']] <- imputeTS::na_interpolation(y_track, option = "linear"),
               error = function(e) { skip_to_next <- TRUE})
      if(skip_to_next) { next }

    }

    # calculating confident scale factor for video (using distance formula):

    if (is.na(manual_scale_factor)) {
      scale_factor <- reference_distance / sqrt((mean(reference[[1]][['x']], na.rm = TRUE) - mean(reference[[2]][['x']], na.rm = TRUE))^2 +
                                                  (mean(reference[[1]][['y']], na.rm = TRUE) - mean(reference[[2]][['y']], na.rm = TRUE))^2)
    } else {
      scale_factor <- manual_scale_factor
    }

    ###### scale axis based on fps ###### (EXPERIMENTAL)
    message("Scaling ", fps, " fps...")

    resize <- function(input, len, current_fps, target_fps) {
      # Calculate the scaling factor based on the ratio of current and target FPS
      scaling_factor <- target_fps / current_fps

      # Calculate the new length of the scaled data
      new_len <- len * scaling_factor

      # Use `approx` to interpolate the input data to the new length
      approximated_data <- approx(seq_along(input), input, n = new_len)

      # Return the scaled data
      return(approximated_data$y)
    }


    for (body_part in body_parts) {
      tryCatch(tracks[[body_part]][['x']] <- resize(input = tracks[[body_part]][['x']], len = length(tracks[[body_part]][['x']]),
                                                    current_fps = fps, target_fps = 2000),
               error = function(e) { skip_to_next <- TRUE})
      if(skip_to_next) { next }

      tryCatch(tracks[[body_part]][['y']] <- resize(input = tracks[[body_part]][['y']], len = length(tracks[[body_part]][['y']]),
                                                    current_fps = fps, target_fps = 2000),
               error = function(e) { skip_to_next <- TRUE})
      if(skip_to_next) { next }

      tryCatch(tracks[[body_part]][['p']] <- resize(input = tracks[[body_part]][['p']], len = length(tracks[[body_part]][['p']]),
                                                    current_fps = fps, target_fps = 2000),
               error = function(e) { skip_to_next <- TRUE})
      if(skip_to_next) { next }

    }

    frames <- as.numeric(length(tracks[[1]][['y']]))

    ##########################################


    # apply filter if one is chosen

    message("Applying ", filter_chosen, " filter to time series for body parts...")

    for (body_part in body_parts) {
      if (tolower(filter_chosen) == "savitzky-golay") {
        tracks[[body_part]][['x']] <- savgol(tracks[[body_part]][['x']], fl = filter_length, forder = 3, dorder = 0)
        tracks[[body_part]][['x']][1:(filter_length*savgol_filter_smoothing_multiplier)] <- tracks[[body_part]][['x']][(filter_length*savgol_filter_smoothing_multiplier)]
        tracks[[body_part]][['x']][(length(tracks[[body_part]][['x']]) - filter_length*savgol_filter_smoothing_multiplier):length(tracks[[body_part]][['x']])] <- tracks[[body_part]][['x']][length(tracks[[body_part]][['x']]) - filter_length*savgol_filter_smoothing_multiplier]

        tracks[[body_part]][['y']] <- savgol(tracks[[body_part]][['y']], fl = filter_length, forder = 3, dorder = 0)
        tracks[[body_part]][['y']][1:(filter_length*savgol_filter_smoothing_multiplier)] <- tracks[[body_part]][['y']][(filter_length*savgol_filter_smoothing_multiplier)]
        tracks[[body_part]][['y']][(length(tracks[[body_part]][['y']]) - filter_length*savgol_filter_smoothing_multiplier):length(tracks[[body_part]][['y']])] <- tracks[[body_part]][['y']][length(tracks[[body_part]][['y']]) - filter_length*savgol_filter_smoothing_multiplier]

      } else if (tolower(filter_chosen) == "median") {
        tracks[[body_part]][['x']] <- runmed(tracks[[body_part]][['x']], k = filter_length, endrule = "keep")
        tracks[[body_part]][['y']] <- runmed(tracks[[body_part]][['y']], k = filter_length, endrule = "keep")
      } else if (tolower(filter_chosen) == "average") {
        tracks[[body_part]][['x']] <- frollmean(tracks[[body_part]][['x']], n = filter_length, algo = "exact", align = "center")
        tracks[[body_part]][['y']] <- frollmean(tracks[[body_part]][['y']], n = filter_length, algo = "exact", align = "center")
      } else if (tolower(filter_chosen) == "none") {
        message("No filter applied.")
      }
    }


    # calculate stimulus trajectory (use only if indicated)

    if (expanded_analysis) {
      x = 1:length(tracks[[1]][[2]])
      stim_y = y_threshold*sin(stimulus_velocity_factor*(x-(fps*stimulus_displacement_factor*fps))) + fixed_baseline
      stim_y[stim_y<fixed_baseline] = fixed_baseline
      stim_y[1:round(stimulus_displacement_factor*fps)] = fixed_baseline
      post_stim_vec = diff(stim_y)

      post_stim_vec = which(diff(stim_y) != 0)
      stim_end = which(diff(post_stim_vec) > 1)[1]
      stim_end_frame = post_stim_vec[stim_end]

      tryCatch(stim_y[stim_end_frame:length(stim_y)] <- fixed_baseline,
               error = function(e) {message("Warning: Error detecting end of stimulus.")})
    }

    # Extract features (x and y coordinates) for toe, center, and heel

    message("Extracting features and applying mm/px scale factor ", round(scale_factor, 6), "...")

    features <- list()

    for (body_part in 1:length(body_parts)) {
      features <- append(features, list(NA))
    }

    names(features) <- body_parts

    # Extract pre-peak and post-peak features

    for (body_part in body_parts) {

      tryCatch(features[[body_part]] <- extract_features(x = (as.numeric(na.omit(tracks[[body_part]][['x']])) * scale_factor),
                                                         y = (as.numeric(na.omit(tracks[[body_part]][['y']])) * scale_factor),
                                                         parameters = set_parameters(
                                                           fixed.baseline = list(y = fixed_baseline,
                                                                                 threshold = y_threshold),
                                                           based.on = params)),
               error = function(e) { skip_to_next <- TRUE})

      if(skip_to_next) { next }
    }

    if(skip_to_next) { next }


    # Extract pre-peak and post-peak pain scores

    message("Assigning pre- and post-peak pain scores to body parts...")

    pain_scores <- list()

    for (body_part in 1:length(body_parts)) {
      pain_scores <- append(pain_scores, list(list(pre.peak = list(NA), post.peak = list(NA))))
    }

    names(pain_scores) <- body_parts

    for (body_part in body_parts) {

      tryCatch(pain_scores[[body_part]][['pre.peak']] <- as.numeric(pain_score(paw.features = features[[body_part]],
                                                                               strains = 'C57B6-',
                                                                               feature.set = c("pre.peak"),
                                                                               pain.model = NULL)),
               error = function(e) { skip_to_next <- TRUE})

      if(skip_to_next) { next }


      tryCatch(pain_scores[[body_part]][['post.peak']] <- as.numeric(pain_score(paw.features = features[[body_part]],
                                                                                strains = 'C57B6-',
                                                                                feature.set = c("post.peak"),
                                                                                pain.model = NULL)),
               error = function(e) { skip_to_next <- TRUE})

      if(skip_to_next) { next }

    }

    if(skip_to_next) { next }


    for (body_part in body_parts) {

      if (expanded_analysis) {
        tryCatch(combined_dataframe[nrow(combined_dataframe)+1,] <- c(features[[body_part]][['pre.peak']]$max.height,
                                                                      features[[body_part]][['pre.peak']]$max.x.velocity,
                                                                      features[[body_part]][['pre.peak']]$max.y.velocity,
                                                                      features[[body_part]][['pre.peak']]$distance.traveled,
                                                                      features[[body_part]][['post.peak']]$max.height, #5
                                                                      features[[body_part]][['post.peak']]$max.x.velocity,
                                                                      features[[body_part]][['post.peak']]$max.y.velocity,
                                                                      features[[body_part]][['post.peak']]$distance.traveled,
                                                                      features[[body_part]][['post.peak']]$number.of.shakes,
                                                                      features[[body_part]][['post.peak']]$shaking.duration, #10
                                                                      features[[body_part]][['post.peak']]$guarding.duration,
                                                                      body_part, file_names[i], stim, group[1], #15
                                                                      pain_scores[[body_part]][['pre.peak']],
                                                                      pain_scores[[body_part]][['post.peak']],
                                                                      which(tracks[[body_part]][['y']] > stim_y)[1] / 2000), # withdrawal latency



                 error = function(e) {skip_to_next <- TRUE})
      } else {


        tryCatch(combined_dataframe[nrow(combined_dataframe)+1,] <- c(features[[body_part]][['pre.peak']]$max.height,
                                                                      features[[body_part]][['pre.peak']]$max.x.velocity,
                                                                      features[[body_part]][['pre.peak']]$max.y.velocity,
                                                                      features[[body_part]][['pre.peak']]$distance.traveled,
                                                                      features[[body_part]][['post.peak']]$max.height,
                                                                      features[[body_part]][['post.peak']]$max.x.velocity,
                                                                      features[[body_part]][['post.peak']]$max.y.velocity,
                                                                      features[[body_part]][['post.peak']]$distance.traveled,
                                                                      features[[body_part]][['post.peak']]$number.of.shakes,
                                                                      features[[body_part]][['post.peak']]$shaking.duration,
                                                                      features[[body_part]][['post.peak']]$guarding.duration,
                                                                      body_part, file_names[i], stim, group,
                                                                      pain_scores[[body_part]][['pre.peak']],
                                                                      pain_scores[[body_part]][['post.peak']]),
                 error = function(e) {skip_to_next <- TRUE})

      }



      if(skip_to_next) { next }


    }

    index <- index + 1

    }

  message("Done! Processed ", as.character(index-1), " file(s) in ", round((Sys.time()-start_time), 3), " seconds.")

  if (expanded_analysis == FALSE) {
    combined_dataframe <- combined_dataframe[, c(15, 12, 14, 16, 17, 1:11, 13)]
    combined_dataframe <- combined_dataframe[
      order(combined_dataframe[,1], combined_dataframe[,2], combined_dataframe[,3]),
    ]
  } else if (expanded_analysis == TRUE) {
    combined_dataframe <- combined_dataframe[, c(15, 12, 14, 16, 17, 1:11, 18, 13)]
    combined_dataframe <- combined_dataframe[
      order(combined_dataframe[,1], combined_dataframe[,2], combined_dataframe[,3]),
    ]
  }


  write.csv(combined_dataframe, paste0(save_directory, "/", "PAWS_Results.csv"), row.names = FALSE)
}







