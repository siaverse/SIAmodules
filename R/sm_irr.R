
# Module documentation ----------------------------------------------------

# This is the user-facing documentation.

#' Interactive Module for Range-restricted Reliability
#'
#' Interactive illustration of range-restricted reliability issue and the difficulties
#' with maximum likelihood estimation, described in more detail in the context
#' of inter-rater reliability in grant proposal review.
#'
#' @author
#' Jan Netik\cr
#' Institute of Computer Science of the Czech Academy of Sciences\cr
#' <netik@cs.cas.cz>
#'
#' Patricia Martinkova\cr
#' Institute of Computer Science of the Czech Academy of Sciences\cr
#' <martinkova@cs.cas.cz>
#'
#' Elena A. Erosheva
#' University of Washington
#'
#' Carole J. Lee
#' University of Washington
#'
#' @references Erosheva, E., Martinkova, P., & Lee, C. (2021). When zero may not
#'   be zero: A cautionary note on the use of inter-rater reliability in
#'   evaluating grant peer review. *Journal of the Royal Statistical Society --
#'   Series A, 184*(3), 904--919. \doi{10.1111/rssa.12681}
#'
#' @name sm_irr
#' @family SIAmodules
NULL


# Module definition -------------------------------------------------------

## Server part ------------------------------------------------------------

#' `sm_irr` module (internal documentation)
#'
#' This is the internal documentation of your module that is not included in the
#' help index of the package. You may include your notes here. For [user-facing
#' help page][sm_irr], please edit the entry in the YAML.
#'
#' Even being internal, a user can still discover this help page. To prevent
#' that, include the `@noRd` roxygen2 tag below (in the source `.R` file).
#'
#' If your module uses any external packages, such as ggplot2,
#' **you have to declare the imports** with the `@importFrom` tag and include
#' the package in the DESCRIPTION.
#' See <https://r-pkgs.org/dependencies-in-practice.html> for more details.
#'
#' @param id *character*, the ID assigned by ShinyItemAnalysis.
#' @param imports *list*, objects exported for the module by ShinyItemAnalysis.
#' @param ... Additional parameters (not used at the moment).
#'
#' @keywords internal
#' @rdname sm_irr_internal
#'
#' @import shiny
#' @import ggplot2
#'
#' @importFrom forcats fct_inorder
#' @importFrom dplyr select mutate filter pull bind_rows if_else case_when
#' @importFrom tibble tibble
#' @importFrom purrr set_names map_dfc
#' @importFrom tidyr pivot_longer
#' @importFrom stats qnorm
#' @importFrom plotly ggplotly renderPlotly
#' @importFrom glue glue
#' @importFrom stringr str_detect
#' @importFrom lme4 lmer
#' @importFrom ShinyItemAnalysis theme_app ICCrestricted
#' @importFrom utils write.csv
#'
sm_irr_server <- function(id, imports = NULL, ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # load AIBS data from SIA, there is no need to store them here
    AIBS <- ShinyItemAnalysis::AIBS

    k_max <- AIBS$ScoreRankAdj %>% max(na.rm = TRUE)


    # transform percentage to Ks (check against Ks, not percents)
    n_sel <- reactive({
      round(input$reliability_restricted_proportion * .01 * k_max)
    })

    # ** Update slider input ######
    observe({
      updateSliderInput(
        session,
        inputId = "reliability_restricted_proportion",
        min = ceiling(200 / k_max)
      )
    })


    # this chunk ensures slider is reset to 100% whenever direction or BS samples are changed
    # however, it is rather lenient and causes some issues, e.g. double estimation on BS num change
    # in addition, user request should be fulfilled without any unsolicited actions, such as slider change

    # observeEvent(c(
    #   input$data_toydata,
    #   input$reliability_restricted_clear,
    #   input$reliability_restricted_direction,
    #   25
    # ), {
    #   updateSliderInput(
    #     session,
    #     inputId = "reliability_restricted_proportion",
    #     value = 100
    #   )
    # })

    # ** Caterpillar plot input ######
    reliability_restricted_caterpillarplot_input <- reactive({
      AIBS %>%
        mutate(hl = case_when(
          input$reliability_restricted_direction == "top" & ScoreRankAdj <= n_sel() ~ "sol",
          input$reliability_restricted_direction == "bottom" & ScoreRankAdj > (k_max - n_sel()) ~ "sol",
          TRUE ~ "alp"
        ) %>% factor(levels = c("alp", "sol"))) %>%
        ggplot(aes(x = .data$ScoreRankAdj, y = .data$Score, group = .data$ID, alpha = .data$hl)) +
        geom_line(col = "gray") +
        suppressWarnings(geom_point(aes(text = paste0(
          "Proposal ID: ", .data$ID, "\n", # TODO generalize
          "Rank: ", .data$ScoreRankAdj, "\n", # TODO generalize
          "Score: ", .data$Score, "\n" # add avg score? not in every dataset, computed only in ICCrestricted
        )), shape = 1, size = 1.5)) +
        stat_summary(
          fun = mean, fun.args = list(na.rm = TRUE), geom = "point", col = "red",
          shape = 5, size = 2.5, stroke = .35
        ) +
        scale_alpha_manual(values = c(alp = .3, sol = 1), drop = FALSE) +
        coord_cartesian(ylim = c(1, 5)) +
        labs(x = "Proposal rank", y = "Overall scientific merit score") +
        theme_app()
    })

    # ** Plotly output for caterpillar plot ######
    output$reliability_restricted_caterpillarplot <- renderPlotly({
      reliability_restricted_caterpillarplot_input() %>%
        ggplotly(tooltip = c("text")) %>%
        plotly::config(displayModeBar = FALSE)
    })

    # ** DB for caterpillar plot ######
    output$DB_reliability_restricted_caterpillarplot <- downloadHandler(
      filename = function() {
        "fig_reliability_caterpillar.png"
      },
      content = function(file) {
        ggsave(file,
          plot = reliability_restricted_caterpillarplot_input(), width = unit(7, "in"), height = unit(5, "in"), scale = 1.2
        )
      }
    )


    reliability_restricted_res <- reactiveValues(vals = NULL) # init ICCs bank

    # ** Clearing ICC computed entries ######
    observeEvent(input$reliability_restricted_clear, {
      cat("Clearing ICC bank...\n")
      reliability_restricted_res$vals <- NULL
    })

    observeEvent(
      c(
        input$reliability_restricted_clear,
        input$reliability_restricted_proportion,
        input$reliability_restricted_direction
        # 25 bootstrap input
      ),
      {
        isolate({
          entries <- reliability_restricted_res$vals %>%
            names()

          # propose a new entry
          new_entry <- paste0(
            "dir-", input$reliability_restricted_direction,
            "_bs-", 25,
            "_sel-", n_sel()
          )

          # check if proposed not already available, else compute
          if (new_entry %in% entries) {
            message("rICC computation: using already computed value...\n")
          } else {
            reliability_restricted_res[["vals"]][[new_entry]] <- ICCrestricted(
              AIBS,
              case = "ID",
              var = "Score",
              rank = "ScoreRankAdj",
              dir = input$reliability_restricted_direction,
              sel = n_sel(),
              nsim = 25
            )
          }
        })
      }
    )

    # ** ICC plot - current choice ######
    reliability_restricted_iccplot_curr <- reactive({
      req(reliability_restricted_res$vals)
      plt_data <- reliability_restricted_res$vals %>%
        bind_rows(.id = "name") %>%
        filter(str_detect(
          .data$name,
          paste0(
            "dir-", input$reliability_restricted_direction,
            "_bs-", 25
          )
        ))

      # translate empty tibble to NULL for further use in req()
      if (nrow(plt_data) == 0) {
        NULL
      } else {
        plt_data
      }
    })

    # ** Reliability plot input ######
    reliability_restricted_iccplot_input <- reactive({
      # req(AIBS) # propagate validation msg to the plot
      req(reliability_restricted_iccplot_curr())

      curr_plt_name <- paste0(
        "dir-", input$reliability_restricted_direction,
        "_bs-", 25,
        "_sel-", n_sel()
      )

      reliability_restricted_iccplot_curr() %>%
        mutate(hl = if_else(.data$name == curr_plt_name, "sol", "alp") %>%
          factor(levels = c("alp", "sol"))) %>%
        ggplot(aes(.data$prop_sel, .data$ICC1, ymin = .data$ICC1_LCI, ymax = .data$ICC1_UCI, alpha = .data$hl)) + # TODO general
        geom_linerange() + # separate as plotly messes up otherwise
        suppressWarnings(geom_point(aes(text = paste0(
          ifelse(.data$prop_sel == 1, "Complete range",
            paste0(
              "Proportion of ", dir, " proposals: ", scales::percent(.data$prop_sel, .01)
            )
          ), "\n",
          "ICC1: ", round(.data$ICC1, 2), "\n",
          "LCI: ", round(.data$ICC1_LCI, 2), "\n",
          "UCI: ", round(.data$ICC1_UCI, 2)
        )))) +
        scale_x_continuous(labels = scales::percent) +
        scale_alpha_manual(values = c(alp = .25, sol = 1), drop = FALSE) +
        labs(
          x = paste0("Proportion of ", input$reliability_restricted_direction, " proposals"),
          y = "Inter-rater reliability"
        ) +
        coord_cartesian(ylim = c(0, 1), xlim = c(0, 1)) + # TODO general
        theme_app()
    })

    # ** Reliability plot render ######
    output$reliability_restricted_iccplot <- renderPlotly({
      reliability_restricted_iccplot_input() %>%
        ggplotly(tooltip = "text") %>%
        plotly::config(displayModeBar = FALSE)
    })

    # ** DB for ICC reliability plot ######
    output$DB_reliability_restricted_iccplot <- downloadHandler(
      filename = function() {
        "fig_reliability_resctricted.png"
      },
      content = function(file) {
        ggsave(file,
          plot = reliability_restricted_iccplot_input(),
          width = unit(7, "in"), height = unit(5, "in"), scale = 1.2
        )
      }
    )

    # ** DB for ICC reliability table ######
    output$DB_reliability_restricted_iccdata <- downloadHandler(
      filename = function() {
        "range_restricted_reliability_data.csv"
      },
      content = function(file) {
        data <- reliability_restricted_iccplot_curr() %>% select(-.data$name)
        write.csv(data, file, row.names = FALSE)
      }
    )

    output$icc_text <- renderText({
      # isolate({
      full <- reliability_restricted_res[["vals"]][[paste0(
        "dir-", input$reliability_restricted_direction,
        "_bs-", 25,
        "_sel-", k_max
      )]]
      # })

      curr <- reliability_restricted_res[["vals"]][[paste0(
        "dir-", input$reliability_restricted_direction,
        "_bs-", 25,
        "_sel-", n_sel()
      )]]


      full_part <- if (is.null(full)) {
        "Please, set the slider to 100% in order to estimate and display inter-rater reliability the complete dataset."
      } else {
        paste0(
          "For the complete dataset, the estimated inter-rater reliability is ",
          round(full$ICC1, 2), ", with 95% CI of [", round(full$ICC1_LCI, 2), ", ", round(full$ICC1_UCI, 2), "]."
        )
      }
      curr_part <- if (identical(curr, full) | is.null(curr)) {
        "Set the slider to different value to see how the estimate changes for other subset of the data."
      } else {
        paste0(
          "For the ", round(curr$prop_sel * 100), "%",
          " (that is ", curr$n_sel, ") of ", curr$dir, " proposals,",
          " the estimated inter-rater reliability is ",
          round(curr$ICC1, 2), ", with 95% CI of [", round(curr$ICC1_LCI, 2), ", ", round(curr$ICC1_UCI, 2), "]."
        )
      }

      paste(full_part, curr_part)
    })
  })
}


## UI part ----------------------------------------------------------------

#' @rdname sm_irr_internal
#'
#' @importFrom plotly plotlyOutput
#'
sm_irr_ui <- function(id, imports = NULL, ...) {
  ns <- NS(id)

  tagList(
    h3("Range-restricted Reliability"),

    # description -------------------------------------------------------------

    p(
      "This module illustrates the issue of range-restricted reliability and
      the difficulties with maximum likelihood estimation, described in more
      detail in the context of inter-rater reliability in grant proposal review
      in",
      a(
        "Erosheva, Martinkova, & Lee (2021)",
        href = "https://doi.org/10.1111/rssa.12681",
        target = "_blank", .noWS = "after"
      ), ". We use their AIBS grant proposal peer-review dataset for presentation."
    ),
    p(
      "Below, you may select the ratio and type of range restriction given by the",
      strong("proportion of rated proposals", .noWS = "after"), ".",
      "In contexts other than grant review, the subject/object of interest may be different:
      a student in educational assessment, a job application
      in hiring, a patient in a medical study, etc. Further, you may select the",
      strong("direction"), "of restriction (top or bottom). The left plot illustrates
      the variability in ratings for the whole dataset outshading the data which
      would be lost due to range-restriction. The right plot provides the estimates
      of the calculated inter-rater reliability estimates, defined by intraclass
      corelation in the simplest model including the ratee effect only. The estimates
      are accompanied by a bootstrapped 95% confidence interval based on 25 bootstrap samples.",
      class = "mb-5"
    ),


    # UI ----------------------------------------------------------------------

    fluidRow(
      column(
        4, sliderInput(
          inputId = ns("reliability_restricted_proportion"),
          label = "Proportion",
          min = 0,
          max = 100,
          step = 1,
          value = 100,
          animate = animationOptions(2000),
          post = "%", width = "100%"
        )
      ),
      column(
        2, selectInput(
          inputId = ns("reliability_restricted_direction"),
          label = "Direction",
          choices = c("top", "bottom"),
          selected = "top"
        )
      ),
      column(
        2, actionButton(
          inputId = ns("reliability_restricted_clear"),
          label = "Clear everything",
          icon = icon("eraser")
        ),
        class = "align-self-center pb-4"
      )
    ),
    fluidRow(
      column(6, plotlyOutput(ns("reliability_restricted_caterpillarplot"))),
      column(6, plotlyOutput(ns("reliability_restricted_iccplot"))),
      style = "padding-bottom: 20px;"
    ),


    # plots -------------------------------------------------------------------

    fluidRow(
      class = "mb-4", # margin after the row
      column(
        6,
        downloadButton(ns("DB_reliability_restricted_caterpillarplot"),
          label = "Download figure"
        )
      ),
      column(
        6,
        downloadButton(ns("DB_reliability_restricted_iccplot"),
          label = "Download figure"
        ),
        downloadButton(ns("DB_reliability_restricted_iccdata"),
          label = "Download data"
        )
      )
    ),


    # interpretation ----------------------------------------------------------

    h4("Interpretation"),
    fluidRow(column(
      12,
      textOutput(ns("icc_text")),
    ), class = "mb-4"),


    # sample R code -----------------------------------------------------------

    h4("Selected R code"),
    tags$pre(includeText(system.file("sc/restr_range.R", package = "SIAmodules")),
      class = "language-r mb-4"
    ),


    # references --------------------------------------------------------
    h4("References"),
    HTML('<ul class = "biblio">
              <li>Erosheva, E., Martinkova, P., & Lee, C. (2021).
               When zero may not be zero: A cautionary note on the use of inter-rater
               reliability in evaluating grant peer review. Journal of the Royal Statistical Society - Series A,
               184(3), 904-919.
              <a href = "https://doi.org/10.1111/rssa.12681", target = "_blank">doi:10.1111/rssa.12681</a>
              </li>

              <li>Martinkova, P., & Drabinova, A. (2018).
              ShinyItemAnalysis for teaching psychometrics and to enforce routine analysis of educational tests.
              The R Journal, 10(2), 503-515.
              <a href = "https://doi.org/10.32614/RJ-2018-074", target = "_blank">doi:10.32614/RJ-2018-074</a>
              </li>
            </ul>'),

    # acknowledgements -------------------------------------------------------------
    h4("Acknowledgements"),
    p(
      "ShinyItemAnalysis Modules are developed by the ",
      a(
        "Computational Psychometrics Group",
        href = "https://www.cs.cas.cz/comps/",
        target = "_blank", .noWS = "after"
      ), " supported by the Czech Science Foundation under Grant Number ",
      a(
        "21-03658S",
        href = "https://www.cs.cas.cz/comps/projectTheorFoundComPs.html",
        target = "_blank", .noWS = "after"
      ), " awarded to Patricia Martinkova. Elena Erosheva and Carole Lee's
    contributions to the development of this module and
    the data collection were further supported by the National Science
    Foundation under Grant Number 1759825.", br(), "Disclaimer:
    Any opinions, findings, and conclusions or recommendations expressed in this
    material are those of the author(s) and do not necessarily reflect the views
    of the National Science Foundation. "
    )
  )
}
