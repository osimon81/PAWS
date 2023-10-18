
#' Launch the PAWS Dashboard
#'
#' This function launches a Shiny dashboard for running PAWS interactively without
#' any code. Settings will be able to be specified in the Dashboard and visualizations
#' that are the output of many PAWS functions will appear in their corresponding tabs here, too.
#'
#' @return The PAWS dashboard.
#' @import shiny
#' @import shinydashboard
#' @export
paws_dashboard <- function() {
  ui <- shinydashboard::dashboardPage(
    dashboardHeader(title = "PAWS Dashboard"),
    dashboardSidebar(

      dashboardSidebar(
        sidebarMenu(
          menuItem("Convert SLEAP H5s to CSVs", tabName = "h5_to_csv", icon = icon("file-csv")),
          menuItem("Optimize smoothing filters", tabName = "preprocess", icon = icon("dashboard")),
          menuItem("Run PAWS diagnostics", tabName = "paws_diagnostics", icon = icon("stethoscope")),
          menuItem("Run PAWS analysis", tabName = "sleap_paws", icon = icon("satellite-dish")),
          menuItem("Visualize pain scores", tabName = "paws_scores", icon = icon("circle-nodes")),
          menuItem("Visualize PAWS kinematics", tabName = "paws_features", icon = icon("person-walking-arrow-right"))
        )
      )

    ),
    dashboardBody(
      tabItems(

        # First tab content

        tabItem(tabName = "h5_to_csv",
                h2("Batch convert SLEAP H5s to DeepLabCut-like CSVs:"),
                fluidRow(
                  box(title = "Enter path to where SLEAP H5s are stored:",
                      textInput("h5Path_SLEAP", label = "",
                                placeholder = "Enter path here...")
                  ),
                  box(title = "Enter path to where SLEAP CSVs will be saved (if left blank, CSVs will be saved in the H5 folder):",
                      textInput("h5_to_csvPath_SLEAP", label = "",
                                placeholder = "Enter path here...")
                  )
                ),
                fluidRow(
                  actionButton("h5_convert", label = "Convert H5s to CSVs!"),
                  align = "center"
                )
        ),

        # Second tab content

        tabItem(tabName = "preprocess",
                h2("Optimize smoothing filters by assessing tracking trajectories in a CSV:"),
                fluidRow(
                  box(title = "Upload a CSV and enter video parameters.",
                      fileInput("pre_process_upload", "Upload a tracked CSV (DeepLabCut or SLEAP) here:",
                                accept = c(
                                  "text/csv",
                                  "text/comma-separated-values,",
                                  ".csv")),
                      numericInput("pre_process_fps", "Enter the video frames per second (fps) here:",
                                   value = 2000),
                      numericInput("pre_process_ref_distance", "Enter the measured distance (mm) between your reference points here:",
                                   value = 40),
                      numericInput("pre_process_scale_factor", "If you aren't tracking reference points, enter a manual mm/px scale factor:",
                                   value = 0.15)
                  ),
                  box(title = "Select body parts, axes, and thresholds to track trajectories.",
                      radioButtons("pre_process_body_part", "Choose a bodypart to graph:",
                                   choiceNames = list("toe", "center", "heel"),
                                   choiceValues = list("toe", "center", "heel")),
                      radioButtons("pre_process_axis", "Choose an axis to graph:",
                                   choiceNames = list("x", "y"),
                                   choiceValues = list("x", "y")),
                      sliderInput("pre_process_fixed_baseline", "Set a fixed baseline for paw height:", 0, 10, 1,
                                   step = 0.5),
                      sliderInput("pre_process_y_threshold", "Set a y-threshold for determining activity window (above baseline):", 0, 5, 0.5,
                                   step = 0.1)
                  )
                ),
                fluidRow(
                  box(title = "Visualize tracked trajectories.",
                      plotOutput("filter_matrix", height = 325)
                  ),
                  box(title = "Adjust filters to find an optimal filter and level of smoothing.",
                      sliderInput("pre_process_savgol_slider", "Savitzky-Golay filter length:", 3, 101, 25,
                                  step = 2),
                      sliderInput("pre_process_median_slider", "Rolling Median filter length:", 1, 101, 25,
                                  step = 2),
                      sliderInput("pre_process_average_slider", "Rolling Average filter length:", 1, 101, 25,
                                  step = 2),
                      sliderInput("pre_process_p_cutoff", "P-cutoff:", 0, 1, 0.6,
                                  step = 0.1)
                  )
                )
        ),

        # Third tab content

        tabItem(tabName = "sleap_paws",
                h2("Run PAWS analysis using CSV files from SLEAP or DeepLabCut:"),
                fluidRow(
                  box(title = "Enter video parameters and the full paths for the following folders:",
                      textInput("csvPath_SLEAP", label = "Enter a path for the folder containing your tracked CSVs:",
                                placeholder = "Enter path here..."),
                      textInput("savePath_SLEAP", label = "Enter a path for the PAWS output file to be saved to:",
                                placeholder = "Enter path here..."),
                      numericInput("fps_SLEAP", label = "Enter the frames per second (fps) for your videos:",
                                   value = 2000),
                      numericInput("ref_distance_SLEAP", label = "Enter the measured distance (mm) between your reference points:",
                                   value = 40),
                      numericInput("manual_scale_factor_SLEAP", label = "Enter a backup mm/px scale factor to scale all your videos (leave blank if you intend to use reference points!):",
                                   value = NA),
                      sliderInput("fixed_baseline_SLEAP", "Set a fixed baseline for paw height:", 0, 10, 1,
                                  step = 0.5),
                      sliderInput("y_threshold_SLEAP", "Set a y-threshold for determining activity window (above baseline):", 0, 5, 0.5,
                                  step = 0.1)
                  ),
                  box(title = "Enter parameters for PAWS analysis:",
                      sliderInput("p_cutoff_SLEAP", label = "Set a p-cutoff threshold for outlier interpolation:", min = 0, max = 1,
                                  value = 0.3),
                      sliderInput("window_threshold_SLEAP", "Enter the threshold for paw withdrawal:", min = 0.05, max = 1,
                                  value = 0.5),
                      sliderInput("shake_threshold_SLEAP", "Enter the threshold for shaking behaviors:", min = 0.05, max = 1,
                                  value = 0.35),
                      selectInput("filter_choice_SLEAP", label = "Choose a smoothing filter:",
                                  choice = c("None", "Average", "Median", "Savitzky-Golay"),
                                  selected = "None"),
                      sliderInput("filter_size_SLEAP", label = "Select a filter length / window size for this filter", min = 3, max = 101,
                                  value = 25, step = 2),
                      sliderInput("withdrawal_latency_threshold_SLEAP", label = "Enter the threshold for determining withdrawal latency:", min = 0.001, max = 0.01,
                                  value = 0.001, step = 0.001),
                      checkboxInput("expanded_analysis_SLEAP", label = "Run expanded analysis? (exports t-star and withdrawal latency)", value = FALSE)

                  )
                ),
                fluidRow(
                  box(title = "Enter the body parts and 2 reference points you wish to track:",

                      textInput('body_parts_SLEAP','Separate each body part you wish to track (in the order they appear in the CSVs) by a comma:', placeholder = "body_part1, body_part2...",
                                value = "toe, center, heel"),
                      textInput('reference_points_SLEAP','List the two reference points you wish to track (in the order they appear in the CSVs), separated by commas:', placeholder = "object_a, object_b",
                                value = "objecta, objectb")),
                  box(title = "Enter the names for the stimuli and groups to be identified from CSV file names:",
                      textInput('group_text_SLEAP','Separate each group by a comma:', placeholder = "group1, group2, group3..."),
                      textInput('stim_text_SLEAP','Separate each stimulus by a comma:', placeholder = "stim1, stim2, stim3...",
                                value = "cb, db, lp, mp"))
                ),
                fluidRow(
                  actionButton("paws_SLEAP", label = "Run PAWS analysis!"),
                  align = "center"
                )
        ),

        # Fourth tab content

        tabItem(tabName = "paws_diagnostics",
                h2("Run PAWS diagnostics using individual CSV files before analyzing your full dataset:"),
                box(title = "Upload a CSV and enter video parameters.",
                    fileInput("diagnostic_upload", "Upload a tracked CSV (DeepLabCut or SLEAP) here:",
                              accept = c(
                                "text/csv",
                                "text/comma-separated-values,",
                                ".csv")),
                    numericInput("diagnostic_fps", "Enter the video frames per second (fps) here:",
                                 value = 2000),
                    numericInput("diagnostic_ref_distance", "Enter the measured distance (mm) between your reference points here:",
                                 value = 40),
                    numericInput('diagnostic_scale_factor', "Enter a backup mm/px scale factor to scale all your videos (leave blank if you intend to use reference points!)",
                                 value = NA),
                    sliderInput("diagnostic_fixed_baseline", "Set a fixed baseline for paw height:", 0, 10, 1,
                                step = 0.5),
                    sliderInput("diagnostic_y_threshold", "Set a y-threshold for determining activity window (above baseline):", 0, 5, 0.5,
                                step = 0.1)
                ),
                box(title = "Enter parameters for PAWS analysis:",
                    sliderInput("p_cutoff_diagnostic", label = "Set a p-cutoff for your CSVs:", min = 0, max = 1,
                                value = 0.45),
                    sliderInput("diagnostic_shake_threshold", "Enter the threshold for shaking behaviors:", min = 0.05, max = 1,
                                value = 0.35),
                    sliderInput("diagnostic_window_threshold", "Enter the threshold for paw withdrawal:", min = 0.05, max = 1,
                                value = 0.5),
                    selectInput("filter_choice_diagnostic", label = "Choose a smoothing filter:",
                                choice = c("None", "Average", "Median", "Savitzky-Golay"),
                                selected = "None"),
                    sliderInput("filter_size_diagnostic", label = "Select a filter length / window size for this filter", min = 3, max = 101,
                                value = 25, step = 2),

                ),

                fluidRow(
                  box(
                    box(
                      title = "Raw diagnostics: Toe",
                      plotOutput("toe_diagnostics_raw"),
                      width = 6
                    ),
                    box(
                      title = "Filtered diagnostics: Toe",
                      plotOutput("toe_diagnostics_filter"),
                      width = 6
                    ),

                    width = 12
                  ),
                  box(
                    title = "PAWS metrics: Toe",
                    DT::DTOutput("toe_table_diagnostics"),
                    width = 12
                  ),

                ),

                fluidRow(
                  box(
                    box(
                      title = "Raw diagnostics: Center",
                      plotOutput("center_diagnostics_raw"),
                      width = 6
                    ),
                    box(
                      title = "Filtered diagnostics: Center",
                      plotOutput("center_diagnostics_filter"),
                      width = 6
                    ),
                    width = 12
                  ),
                  box(
                    title = "PAWS metrics: Center",
                    DT::DTOutput("center_table_diagnostics"),
                    width = 12
                  ),
                ),

                fluidRow(
                  box(
                    box(
                      title = "Raw diagnostics: Heel",
                      plotOutput("heel_diagnostics_raw"),
                      width = 6
                    ),
                    box(
                      title = "Filtered diagnostics: Heel",
                      plotOutput("heel_diagnostics_filter"),
                      width = 6
                    ),
                    width = 12
                  ),
                  box(
                    title = "PAWS metrics: Heel",
                    DT::DTOutput("heel_table_diagnostics"),
                    width = 12
                  ),
                )

        ),

        # Fifth tab content

        tabItem(tabName = "paws_scores",
                h2("Visualize PAWS score distributions across groups:"),
                fluidRow(
                  box(title = "Enter path to where PAWS Results are stored:",
                      fileInput("paws_results_path", "",
                                accept = c(
                                  "text/csv",
                                  "text/comma-separated-values,",
                                  ".csv")),
                      width = 12
                  )
                ),
                fluidRow(
                  box(title = "Pre-peak Pain Score:",
                      plotOutput("pre_pain_scores"),
                      width = 12
                  ),
                  box(title = "Post-peak Pain Score:",
                      plotOutput("post_pain_scores"),
                      width = 12
                  )
                )
        ),

        tabItem(tabName = "paws_features",
                h2("Plot PAWS features:"),
                box(title = "Enter path to where PAWS Results are stored:",
                    fileInput("paws_features_results_path", "",
                              accept = c(
                                "text/csv",
                                "text/comma-separated-values,",
                                ".csv")),
                    selectInput("paws_features_to_plot", "", choice = c("Pre-peak Max Height",
                                                                        "Pre-peak Max X-velocity",
                                                                        "Pre-peak Max Y-velocity",
                                                                        "Pre-peak Distance traveled",
                                                                        "Post-peak Max height",
                                                                        "Post-peak Max X-velocity",
                                                                        "Post-peak Max Y-velocity",
                                                                        "Post-peak Distance traveled",
                                                                        "Post-peak Number of shakes",
                                                                        "Post-peak Shaking duration",
                                                                        "Post-peak Guarding duration"), selected = NULL),
                    width = 12
                ),
                fluidRow(
                  box(title = "PAWS Features Plot:",
                      plotOutput("paws_features_plot"),
                      width = 12
                  )
                )
        )
      )
    )
  )

  server <- function(input, output) {

    output$filter_matrix <- renderPlot(
      plot_filter_graphs(csv_or_path = input$pre_process_upload$datapath,
                         p_cutoff = input$pre_process_p_cutoff,
                         reference_distance = input$pre_process_ref_distance,
                         manual_scale_factor = input$pre_process_scale_factor,
                         fixed_baseline = input$pre_process_fixed_baseline,
                         y_threshold = input$pre_process_y_threshold,
                         fps = input$pre_process_fps,
                         savgol_window_length = input$pre_process_savgol_slider,
                         median_window_length = input$pre_process_median_slider,
                         average_window_length = input$pre_process_average_slider,
                         body_part = input$pre_process_body_part,
                         axis = input$pre_process_axis)
    )

    observeEvent(input$paws_SLEAP, {
      paws_analysis(csv_directory = input$csvPath_SLEAP,
                    save_directory = input$savePath_SLEAP,
                    p_cutoff = input$p_cutoff_SLEAP,
                    manual_scale_factor = input$manual_scale_factor_SLEAP,
                    filter_length = input$filter_size_SLEAP,
                    filter_chosen = input$filter_choice_SLEAP,
                    reference_distance = input$ref_distance_SLEAP,
                    body_parts = unlist(strsplit(input$body_parts_SLEAP, split = ", ")),
                    reference_points = unlist(strsplit(input$reference_points_SLEAP, split = ", ")),
                    stims = unlist(strsplit(input$stim_text_SLEAP, split = ", ")),
                    groups = unlist(strsplit(input$group_text_SLEAP, split = ", ")),
                    fps = input$fps_SLEAP,
                    shake_threshold = input$shake_threshold_SLEAP,
                    fixed_baseline = input$fixed_baseline_SLEAP,
                    y_threshold = input$y_threshold_SLEAP,
                    window_threshold = input$window_threshold_SLEAP,
                    withdrawal_latency_threshold = input$withdrawal_latency_threshold_SLEAP,
                    expanded_analysis = input$expanded_analysis_SLEAP)
    })

    observeEvent(input$h5_convert, {
      h5_to_csv(h5_folder = input$h5Path_SLEAP,
                save_folder = input$h5_to_csvPath_SLEAP)
    })

    output$pre_pain_scores <- renderPlot(
      plot_pain_scores(csv_path = input$paws_results_path$datapath,
                       peak = "pre")
    )

    output$post_pain_scores <- renderPlot(
      plot_pain_scores(csv_path = input$paws_results_path$datapath,
                       peak = "post")
    )

    output$toe_diagnostics_raw <- renderPlot(
      plot_univariate_projection(csv_or_path = input$diagnostic_upload$datapath,
                                 filter = "None",
                                 p_cutoff = input$p_cutoff_diagnostic,
                                 manual_scale_factor = input$diagnostic_scale_factor,
                                 body_part = "toe",
                                 fixed_baseline = input$diagnostic_fixed_baseline,
                                 y_threshold = input$diagnostic_y_threshold,
                                 shake_threshold = input$diagnostic_shake_threshold,
                                 window_threshold = input$diagnostic_window_threshold,
                                 reference_distance = input$diagnostic_ref_distance,
                                 savgol_window_length = input$filter_size_diagnostic,
                                 median_window_length = input$filter_size_diagnostic,
                                 average_window_length = input$filter_size_diagnostic)
    )

    output$toe_diagnostics_filter <- renderPlot(
      plot_univariate_projection(csv_or_path = input$diagnostic_upload$datapath,
                                 filter = input$filter_choice_diagnostic,
                                 p_cutoff = input$p_cutoff_diagnostic,
                                 manual_scale_factor = input$diagnostic_scale_factor,
                                 body_part = "toe",
                                 fixed_baseline = input$diagnostic_fixed_baseline,
                                 y_threshold = input$diagnostic_y_threshold,
                                 shake_threshold = input$diagnostic_shake_threshold,
                                 window_threshold = input$diagnostic_window_threshold,
                                 reference_distance = input$diagnostic_ref_distance,
                                 savgol_window_length = input$filter_size_diagnostic,
                                 median_window_length = input$filter_size_diagnostic,
                                 average_window_length = input$filter_size_diagnostic)
    )

    output$center_diagnostics_raw <- renderPlot(
      plot_univariate_projection(csv_or_path = input$diagnostic_upload$datapath,
                                 filter = "None",
                                 p_cutoff = input$p_cutoff_diagnostic,
                                 manual_scale_factor = input$diagnostic_scale_factor,
                                 body_part = "center",
                                 fixed_baseline = input$diagnostic_fixed_baseline,
                                 y_threshold = input$diagnostic_y_threshold,
                                 shake_threshold = input$diagnostic_shake_threshold,
                                 window_threshold = input$diagnostic_window_threshold,
                                 reference_distance = input$diagnostic_ref_distance,
                                 savgol_window_length = input$filter_size_diagnostic,
                                 median_window_length = input$filter_size_diagnostic,
                                 average_window_length = input$filter_size_diagnostic)
    )

    output$center_diagnostics_filter <- renderPlot(
      plot_univariate_projection(csv_or_path = input$diagnostic_upload$datapath,
                                 filter = input$filter_choice_diagnostic,
                                 p_cutoff = input$p_cutoff_diagnostic,
                                 manual_scale_factor = input$diagnostic_scale_factor,
                                 body_part = "center",
                                 fixed_baseline = input$diagnostic_fixed_baseline,
                                 y_threshold = input$diagnostic_y_threshold,
                                 shake_threshold = input$diagnostic_shake_threshold,
                                 window_threshold = input$diagnostic_window_threshold,
                                 reference_distance = input$diagnostic_ref_distance,
                                 savgol_window_length = input$filter_size_diagnostic,
                                 median_window_length = input$filter_size_diagnostic,
                                 average_window_length = input$filter_size_diagnostic)
    )

    output$heel_diagnostics_raw <- renderPlot(
      plot_univariate_projection(csv_or_path = input$diagnostic_upload$datapath,
                                 filter = "None",
                                 p_cutoff = input$p_cutoff_diagnostic,
                                 manual_scale_factor = input$diagnostic_scale_factor,
                                 body_part = "heel",
                                 fixed_baseline = input$diagnostic_fixed_baseline,
                                 y_threshold = input$diagnostic_y_threshold,
                                 shake_threshold = input$diagnostic_shake_threshold,
                                 window_threshold = input$diagnostic_window_threshold,
                                 reference_distance = input$diagnostic_ref_distance,
                                 savgol_window_length = input$filter_size_diagnostic,
                                 median_window_length = input$filter_size_diagnostic,
                                 average_window_length = input$filter_size_diagnostic)
    )

    output$heel_diagnostics_filter <- renderPlot(
      plot_univariate_projection(csv_or_path = input$diagnostic_upload$datapath,
                                 filter = input$filter_choice_diagnostic,
                                 p_cutoff = input$p_cutoff_diagnostic,
                                 manual_scale_factor = input$diagnostic_scale_factor,
                                 body_part = "heel",
                                 fixed_baseline = input$diagnostic_fixed_baseline,
                                 y_threshold = input$diagnostic_y_threshold,
                                 shake_threshold = input$diagnostic_shake_threshold,
                                 window_threshold = input$diagnostic_window_threshold,
                                 reference_distance = input$diagnostic_ref_distance,
                                 savgol_window_length = input$filter_size_diagnostic,
                                 median_window_length = input$filter_size_diagnostic,
                                 average_window_length = input$filter_size_diagnostic)
    )

    output$toe_table_diagnostics <- DT::renderDataTable(
      mini_paws(csv_or_path = input$diagnostic_upload$datapath,
                filter = input$filter_choice_diagnostic,
                p_cutoff = input$p_cutoff_diagnostic,
                manual_scale_factor = input$diagnostic_scale_factor,
                body_part = "toe",
                fixed_baseline = input$diagnostic_fixed_baseline,
                y_threshold = input$diagnostic_y_threshold,
                shake_threshold = input$diagnostic_shake_threshold,
                window_threshold = input$diagnostic_window_threshold,
                reference_distance = input$diagnostic_ref_distance,
                savgol_window_length = input$filter_size_diagnostic,
                median_window_length = input$filter_size_diagnostic,
                average_window_length = input$filter_size_diagnostic),
      options = list(scrollX = TRUE)
    )

    output$center_table_diagnostics <- DT::renderDataTable(
      mini_paws(csv_or_path = input$diagnostic_upload$datapath,
                filter = input$filter_choice_diagnostic,
                p_cutoff = input$p_cutoff_diagnostic,
                manual_scale_factor = input$diagnostic_scale_factor,
                body_part = "center",
                shake_threshold = input$diagnostic_shake_threshold,
                window_threshold = input$diagnostic_window_threshold,
                reference_distance = input$diagnostic_ref_distance,
                savgol_window_length = input$filter_size_diagnostic,
                median_window_length = input$filter_size_diagnostic,
                average_window_length = input$filter_size_diagnostic),
      options = list(scrollX = TRUE)
    )

    output$heel_table_diagnostics <- DT::renderDataTable(
      mini_paws(csv_or_path = input$diagnostic_upload$datapath,
                filter = input$filter_choice_diagnostic,
                p_cutoff = input$p_cutoff_diagnostic,
                manual_scale_factor = input$diagnostic_scale_factor,
                body_part = "heel",
                shake_threshold = input$diagnostic_shake_threshold,
                window_threshold = input$diagnostic_window_threshold,
                reference_distance = input$diagnostic_ref_distance,
                savgol_window_length = input$filter_size_diagnostic,
                median_window_length = input$filter_size_diagnostic,
                average_window_length = input$filter_size_diagnostic),
      options = list(scrollX = TRUE)
    )

    output$paws_features_plot <- renderPlot(
      plot_paws_features(csv_path = input$paws_features_results_path$datapath,
                         plot_features = input$paws_features_to_plot)
    )
  }

  shinyApp(ui, server)

}
