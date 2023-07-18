
#' Batch convert SLEAP H5 files into DeepLabCut-like CSVs
#'
#' This function loads a directory containing H5 files generated from SLEAP.
#' It processes the first track in the CSV (assuming single-animal) and rearranges
#' the tracked points to match the style of that in DeepLabCut. Generated CSVs
#' are all saved to a separate directory, but can be saved in the H5 directory if
#' no `save_folder` parameter is assigned.
#'
#' @param h5_folder Path to the folder containing SLEAP H5s.
#' @param save_folder Path to the folder where DeepLabCut-like CSVs will be saved.
#' @return Converted CSVs stored in the `save_folder` directory.
#' @import rhdf5
#' @export
h5_to_csv <- function(h5_folder, save_folder = NA) {
  if (is.na(save_folder)) {
    save_folder <- h5_folder
  }

  files <- list.files(path = h5_folder, pattern = "*.h5", full.names = TRUE)
  file_names <- list.files(path = h5_folder, pattern = "*.h5", full.names = FALSE)
  j = 1
  for (file in files) {
    h5f = H5Fopen(file)

    body_parts <- h5f$node_names

    tracks <- list()

    for (body_part in 1:length(body_parts)) {
      tracks <- append(tracks, list(list(x = list(NA), y = list(NA), likelihood = list(NA))))
    }

    names(tracks) <- body_parts

    i = 1

    for (body_part in body_parts) {
      tracks[[body_part]][['x']] <- h5f$tracks[,i,1,1]
      tracks[[body_part]][['y']] <- h5f$tracks[,i,2,1]
      tracks[[body_part]][['likelihood']] <- h5f$point_scores[,i,1]
      i = i + 1
    }

    for (body_part in body_parts) {
      tracks[[body_part]][['likelihood']][which(is.nan(tracks[[body_part]][['likelihood']]))] <- 1
      tracks[[body_part]][['x']][which(is.nan(tracks[[body_part]][['x']]))] <- NA
      tracks[[body_part]][['y']][which(is.nan(tracks[[body_part]][['y']]))] <- NA
    }


    df <- data.frame(scorer = c("bodyparts", "coords", 1:length(tracks[[1]][['x']])))

    for (body_part in body_parts) {
      x_data <- c(body_part, "x", tracks[[body_part]][['x']])
      y_data <- c(body_part, "y", tracks[[body_part]][['y']])
      p_data <- c(body_part, "likelihood", tracks[[body_part]][['likelihood']])

      df <- cbind(df, x_data, y_data, p_data)

    }

    header <- c("index")

    for (body_part in body_parts) {
      header <- c(header, paste0(body_part, "_x"), paste0(body_part, "_y"), paste0(body_part, "_p"))
    }

    names(df) <- header

    write.csv(df, row.names = FALSE, file = paste0(save_folder, "/", substr(file_names[j], 1, nchar(file_names[j])-3), ".csv"))
    j = j + 1
  }
}

