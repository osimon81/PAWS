

#' Plot pain score distributions
#'
#' This function plots the distribution of pain scores, at either pre-peak or post-peak.
#'
#' @param csv_path The path to the aggregated CSV exported from the `paws_analysis` function
#' @param peak Whether to plot pre-peak (`pre`) or post-peak (`post`) pain scores.
#' @return A density plot of pain score distributions.
#' @import ggplot2
#' @import pracma
#' @import ggpubr
#' @importFrom dplyr select, filter
#' @import magrittr
#' @export
plot_pain_scores <- function(csv_path, peak = "pre") {
  if (peak != "pre" & peak != "post") {
    stop("Invalid peak specification! (Hint: use 'pre' for pre-peak, and 'post' for post-peak)")
  }


  comb <- read.csv(csv_path)

  figure <- list()

  if (tolower(peak) == "pre") {
    for (chosen_group in 1:length(unique(comb$group))) {
      for (chosen_bp in 1:length(unique(comb$body_part))) {
        figure[[length(figure)+1]] <- comb %>%
          filter(group == unique(comb$group)[chosen_group] & body_part == unique(comb$body_part)[chosen_bp]) %>%
          ggplot() +
          geom_density(mapping = aes(x = pre.pain_score,
                                     color = stimulus,
                                     fill = stimulus),
                       linewidth = 1,
                       alpha = 0.2) +
          theme_classic() +
          labs(
            title = paste0("Pain score distributions for ", unique(comb$body_part)[chosen_bp], ": ", unique(comb$group)[chosen_group]),
            x = "Pre-Peak Pain Score",
            y = "density",
            color = "Stimulus",
            fill = "Stimulus") +
          geom_vline(xintercept=0, linetype="dashed")
      }
    }

    scores_figure <- ggarrange(plotlist = figure,
                               labels = LETTERS[1:(length(unique(comb$body_part))*length(unique(comb$group)))],
                               common.legend = TRUE,
                               legend = "right")

    return(scores_figure)
  }

  if (tolower(peak) == "post") {
    for (chosen_group in 1:length(unique(comb$group))) {
      for (chosen_bp in 1:length(unique(comb$body_part))) {
        figure[[length(figure)+1]] <- comb %>%
          filter(group == unique(comb$group)[chosen_group] & body_part == unique(comb$body_part)[chosen_bp]) %>%
          ggplot() +
          geom_density(mapping = aes(x = post.pain_score,
                                     color = stimulus,
                                     fill = stimulus),
                       linewidth = 1,
                       alpha = 0.2) +
          theme_classic() +
          labs(
            title = paste0("Pain score distributions for ", unique(comb$body_part)[chosen_bp], ": ", unique(comb$group)[chosen_group]),
            x = "Post-Peak Pain Score",
            y = "density",
            color = "Stimulus",
            fill = "Stimulus") +
          geom_vline(xintercept=0, linetype="dashed")
      }
    }

    scores_figure <- ggarrange(plotlist = figure,
                               labels = LETTERS[1:(length(unique(comb$body_part))*length(unique(comb$group)))],
                               common.legend = TRUE,
                               legend = "right")

    return(scores_figure)
  }


  #scores_figure
}

#' Plot PAWS features
#'
#' This function plots a given PAWS feature across stimuli, groups, and body-parts.
#'
#' @param csv_path The path to the aggregated CSV exported from the `paws_analysis` function
#' @param plot_features The PAWS feature to plot. Options are `pre-peak max height`,
#' `pre-peak max x-velocity`, `pre-peak max y-velocity`, `pre-peak distance traveled`,
#' `post-peak max height`, `post-peak max x-velocity`, `post-peak max y-velocity`,
#' `post-peak distance traveled`, `post-peak number of shakes`, `post-peak shaking duration`,
#' `post-peak guarding duration`
#' @return A bar plot of PAWS features over a distribution.
#' @import ggplot2
#' @import pracma
#' @import data.table
#' @import ggpubr
#' @export
plot_paws_features <- function(csv_path, plot_features) {
  df <- read.csv(csv_path)

  if (tolower(plot_features) == "pre-peak max height") {
    to_plot <- df %>%
      select(group, body_part, stimulus, pre.max_height)
    title = "Pre-Peak Maximum Height (converted units)"
    names(to_plot)[4] <- "factor"
  } else if (tolower(plot_features) == "pre-peak max x-velocity") {
    to_plot <- df %>%
      select(group, body_part, stimulus, pre.max_x_velocity)
    title = "Pre-Peak Maximum X-Velocity (converted units/s)"
    names(to_plot)[4] <- "factor"
  } else if (tolower(plot_features) == "pre-peak max y-velocity") {
    to_plot <- df %>%
      select(group, body_part, stimulus, pre.max_y_velocity)
    title = "Pre-Peak Maximum Y-Velocity (converted units/s)"
    names(to_plot)[4] <- "factor"
  } else if (tolower(plot_features) == "pre-peak distance traveled") {
    to_plot <- df %>%
      select(group, body_part, stimulus, pre.distance_traveled)
    title = "Pre-Peak Maximum Distance Traveled (converted units)"
    names(to_plot)[4] <- "factor"
  } else if (tolower(plot_features) == "post-peak max height") {
    to_plot <- df %>%
      select(group, body_part, stimulus, post.max_height)
    title = "Post-Peak Maximum Height (converted units)"
    names(to_plot)[4] <- "factor"
  } else if (tolower(plot_features) == "post-peak max x-velocity") {
    to_plot <- df %>%
      select(group, body_part, stimulus, post.max_x_velocity)
    title = "Post-Peak Maximum X-Velocity (converted units/s)"
    names(to_plot)[4] <- "factor"
  } else if (tolower(plot_features) == "post-peak max y-velocity") {
    to_plot <- df %>%
      select(group, body_part, stimulus, post.max_y_velocity)
    title = "Post-Peak Maximum Y-Velocity (converted units/s)"
    names(to_plot)[4] <- "factor"
  } else if (tolower(plot_features) == "post-peak distance traveled") {
    to_plot <- df %>%
      select(group, body_part, stimulus, post.distance_traveled)
    title = "Post-Peak Distance Traveled (converted units)"
    names(to_plot)[4] <- "factor"
  } else if (tolower(plot_features) == "post-peak number of shakes") {
    to_plot <- df %>%
      select(group, body_part, stimulus, post.number_of_shakes)
    title = "Post-Peak Number of Shakes"
    names(to_plot)[4] <- "factor"
  } else if (tolower(plot_features) == "post-peak shaking duration") {
    to_plot <- df %>%
      select(group, body_part, stimulus, post.shaking_duration)
    title = "Post-Peak Shaking Duration (s)"
    names(to_plot)[4] <- "factor"
  } else if (tolower(plot_features) == "post-peak guarding duration") {
    to_plot <- df %>%
      select(group, body_part, stimulus, post.guarding_duration)
    title = "Post-Peak Guarding Duration (s)"
    names(to_plot)[4] <- "factor"
  }

  summary_stats <- to_plot %>%
    group_by(group, stimulus, body_part) %>%
    summarise(mean = mean(factor),
              sdev = sd(factor),
              n = n(),
              stderror = sdev/sqrt(n))

  feature_plot <- ggplot(data = summary_stats) +
    geom_col(mapping = aes(x = group, y = mean, fill = group), color = "black",
             position = position_dodge()) +
    geom_errorbar(aes(x = group, ymin=mean-stderror, ymax=mean+stderror), width=.2,
                                position=position_dodge()) +
    facet_grid(body_part ~ stimulus) +
    geom_point(data = to_plot, mapping = aes(x = group, y = factor),
               position = position_jitter(w = 0.2, h = 0)) +
    labs(y = "Mean",
         x = "Group",
         title = title,
         fill = "Group")

  return(feature_plot)

  }



