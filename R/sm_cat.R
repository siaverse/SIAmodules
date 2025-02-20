# Module documentation ----------------------------------------------------

# This is the user-facing documentation.

#' Interactive Module for Computerized Adaptive Tests
#'
#' Interactive illustration of computerized adaptive test (CAT) with the `mirtCAT` package.
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
#' @name sm_cat
#' @family SIAmodules
NULL


# Module definition -------------------------------------------------------

## Server part ------------------------------------------------------------

#' `sm_cat` module (internal documentation)
#'
#' This is the internal documentation of your module that is not included in the
#' help index of the package. You may include your notes here. For [user-facing
#' help page][sm_cat], please edit the entry in the YAML.
#'
#' Even being internal, a user can still discover this help page. To prevent
#' that, include the `@noRd` roxygen2 tag below (in the source `.R` file).
#'
#' If your module uses any external packages, such as ggplot2,
#' **you have to declare the imports** with the `@importFrom` tag and include
#' the package in the DESCRIPTION. See
#' <https://r-pkgs.org/dependencies-in-practice.html> for more details.
#'
#' @param id *character*, the ID assigned by ShinyItemAnalysis.
#' @param imports *list*, objects exported for the module by ShinyItemAnalysis.
#' @param ... Additional parameters (not used at the moment).
#'
#' @keywords internal
#' @rdname sm_cat_internal
#'
#' @import shiny
#' @import ggplot2
#'
#' @importFrom mirtCAT mirtCAT generate_pattern
#' @importFrom forcats fct_inorder
#' @importFrom dplyr mutate filter pull
#' @importFrom tibble tibble
#' @importFrom purrr set_names map_dfc map_int
#' @importFrom tidyr pivot_longer
#' @importFrom mirt extract.item extract.mirt iteminfo
#' @importFrom stats qnorm
#' @importFrom plotly ggplotly renderPlotly
#' @importFrom glue glue
#' @importFrom utils packageVersion
#'
sm_cat_server <- function(id, imports = NULL, ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # on module initialization, update choices in IRT model selectInput
    observe({
      # check if the module is run inside SIA app,
      # SIA >= 1.5.0 provides more detailed info that we cannot check on SIA 1.5.0
      if (packageVersion("ShinyItemAnalysis") > "1.5.0") {
        runs_from_sia <- !is.null(imports) && !is.null(attr(imports, "imported_by_sia")) && attr(imports, "imported_by_sia")
      } else {
        runs_from_sia <- !is.null(imports)
      }


      if (runs_from_sia) {
        # list these specified IRT models that comes from the app
        irt_models <- c(
          "Module's example 2PL" = "example",
          "SIA-fitted IRT for binary data" = "sia_binary",
          "SIA-fitted NRM" = "sia_nrm"
        )

        # and pass it as the choices to be displayed in the UI
        updateSelectInput(inputId = "irt_model", choices = irt_models)
      }
    })

    # IRT model object
    mod <- reactive({
      # return the appropriate IRT model based on the `input$irt_model`
      switch(input$irt_model,
        example = example_2pl_mod,
        sia_binary = imports$IRT_binary_model(),
        # object for NRM option consist of fitted model and some additional information
        # that we have no use for in this module
        sia_nrm = {
          fit <- imports$IRT_bock_fit_and_orig_levels()[["fit"]]
          attr(fit, "orig_levels") <- imports$IRT_bock_fit_and_orig_levels()[["orig_levels"]]
          fit
        },
      )
    })


    item_infos <- reactive({
      req(mod())

      items <- seq_len(extract.mirt(mod(), "nitems")) %>% set_names()

      items %>%
        map_dfc(~ extract.item(mod(), .x) %>% iteminfo(info_thetas)) %>%
        mutate(theta = info_thetas) %>%
        pivot_longer(-.data$theta, values_to = "info", names_to = "item")

      # note that `info_thetas` is defined outside the server function at the very bottom of this file
      # if you have many such objects, feel free to create a separate file in `/R` for them
      # (all .R/.rda files there are sourced and the results available to use in your package)
    })


    # create a reactive component that "listens" for any change of UI inputs or anything reactive
    sim_res_raw <- reactive({
      # based on given theta, generate plausible response pattern
      pat <- generate_pattern(mod(), input$true_theta)

      mirtCAT(
        mo = mod(),
        local_pattern = pat,
        start_item = "MI",
        method = "MAP",
        criteria = "MI",
        design = list(min_SEM = input$min_se) # pass the input value
      ) %>%
        summary()
    })

    administered_items <- reactive({
      sim_res_raw()[["items_answered"]]
    })

    sim_res <- reactive({
      sim_res_raw <- sim_res_raw()

      tibble(
        item = c(0L, sim_res_raw$items_answered),
        theta = sim_res_raw$thetas_history[, 1],
        se = sim_res_raw$thetas_SE_history[, 1],
      ) %>% mutate(
        item_pos = seq_along(.data$item) - 1L,
        item = .data$item %>% as.factor() %>% fct_inorder(),
        ci = .data$se * qnorm(.975), # for 95% CI
        lci = .data$theta - .data$ci,
        uci = .data$theta + .data$ci,
      )
    })

    # render the UI with a fresh sliderInput for current item position, with
    # specified range using the length of administered items which are unknown beforehand
    # edit animation interval to be slower
    output$item_pos_ui <- renderUI({
      n_administered_items <- length(administered_items())

      sliderInput(
        ns("item_pos"),
        "CAT step",
        value = 0, min = 0, max = n_administered_items, step = 1,
        animate = animationOptions(
          interval = 2000
        )
      )
    })


    # make two reactives, one for each plot - there are situations that effectively changes only one of them
    # think of toggling the `input$only_ans` button - neither the plot with estimates
    # nor the underlying data are subject of change, but the filtered data are
    # in the ideal world, the data would be static and we would only send requests to plotly on
    # which IIC and item position to highlight, but that would require a lot of JavaScript
    # to achieve and would make the codebase for this module at least twice larger
    estimates_plt <- reactive({
      bkg_data <- sim_res()

      filtered_data <- bkg_data %>%
        filter(.data$item_pos <= input$item_pos)

      filtered_data %>%
        ggplot(aes(.data$item,
          group = 1,
          text = glue(
            "<b>Item: {.data$item}</b>
            Theta: {round(.data$theta, 2L)}
            95% CI [{round(.data$lci, 2L)}, {round(.data$uci, 2L)}]"
          )
        )) +
        geom_segment(aes(y = cur_theta(), yend = cur_theta(), x = 0.85, xend = input$item_pos + 1L), linetype = "dashed", col = "gray50") +
        geom_hline(aes(yintercept = input$true_theta), linetype = "solid", col = "gray80", alpha = .5, size = 1) +
        geom_ribbon(aes(y = .data$theta, ymin = .data$lci, ymax = .data$uci), alpha = .15, fill = "gray75", data = bkg_data) +
        geom_ribbon(aes(y = .data$theta, ymin = .data$lci, ymax = .data$uci), alpha = .25, fill = "gray60") +
        geom_line(aes(y = .data$theta), data = bkg_data, col = "gray70", alpha = .4) +
        geom_line(aes(y = .data$theta), col = blue, size = .85, alpha = .4) +
        geom_point(aes(y = .data$theta), data = bkg_data, col = "gray80") +
        geom_point(aes(y = .data$theta), col = blue, size = 2) +
        scale_x_discrete(expand = expansion(mult = c(.01, .01))) +
        scale_y_continuous(limits = range(info_thetas), expand = expansion(mult = c(.01, .01))) +
        labs(x = "Item", y = "Ability estimate") +
        theme_minimal()
    })


    # get the ability estimate based on which current item was chosen (IIC of the item with max. info at that theta)
    cur_theta <- reactive({
      sim_res()[["theta"]][input$item_pos + 1L]
    })

    next_item <- reactive({
      c(administered_items(), "end of the test")[input$item_pos + 1L]
    })

    cur_item <- reactive({
      c(0L, administered_items())[input$item_pos + 1L]
    })


    infos_plt <- reactive({
      next_item <- next_item()

      # require that we do not demand 10th item if there was only 5 items answered, for example...
      # otherwise, an error would appear for 0.5 second or so
      # if we had used the normal sliderInput and only updated the max. value with updateSliderIntput,
      # this likely wouldn't have been an issue
      req(!is.na(next_item))

      already_admin <- sim_res() %>%
        filter(.data$item_pos <= input$item_pos) %>%
        pull(.data$item)

      bkg_data <- item_infos()

      # filter only administered items
      if (input$only_ans) {
        bkg_data <- bkg_data %>% filter(.data$item %in% administered_items())
      }

      filtered_data <- bkg_data %>%
        filter(.data$item == next_item)

      filtered_data %>%
        ggplot(aes(.data$theta, .data$info,
          group = .data$item,
          text = glue(
            "<b>Item: {.data$item}</b>
            Information: {round(.data$info, 2L)}
            Theta: {round(.data$theta, 2L)}"
          )
        )) +
        geom_vline(aes(xintercept = input$true_theta), linetype = "solid", col = "gray80", alpha = .5, size = 1) +
        geom_line(aes(col = .data$item %in% already_admin), show.legend = FALSE, alpha = .7, data = bkg_data) +
        {
          if (next_item != "end of the test") {
            geom_line(show.legend = FALSE, col = blue, size = 1)
          }
        } +
        geom_vline(aes(xintercept = cur_theta()), linetype = "dashed") +
        scale_x_continuous(limits = range(info_thetas), expand = expansion(mult = c(.01, .01))) +
        scale_color_manual(values = c(`TRUE` = "#2C7BB6", `FALSE` = "gray80")) +
        scale_y_reverse() +
        coord_flip() +
        labs(x = "Ability", y = "Information") +
        theme_minimal()
    })


    scored_resps <- reactive({
      sr <- sim_res_raw()$scored_responses
      ans_items <- sim_res_raw()$items_answered

      if (input$irt_model == "sia_nrm") {
        # score items according to the key SIA NRM
        orig_levels <- attr(mod(), "orig_levels")
        corr_ans <- orig_levels %>% map_int(~ which(attr(.x, "key")))

        # reorder according to the answered items
        corr_ans <- corr_ans[ans_items]
      } else {
        corr_ans <- rep(1L, length(sr))
      }

      sr <- ifelse(sr == corr_ans, "correct", "incorrect")
      c(sr, "end of the test")
    })

    next_item_resp <- reactive({
      scored_resps()[input$item_pos + 1L]
    })

    infos_plt_plotly <- reactive({
      text <- glue(
        "Current ability est.: {round(cur_theta(), 2)}
        Next item: {next_item()}
        Next item response: {next_item_resp()}"
      )

      infos_plt() %>%
        ggplotly(tooltip = "text") %>%
        plotly::add_annotations(
          text = text, showarrow = F, xref = "paper", yref = "paper",
          x = .02, y = 1, align = "left", xanchor = "left", yanchor = "top"
        )
    })


    estimates_plt_plotly <- reactive({
      estimates_plt() %>% ggplotly(tooltip = "text")
    })

    merged_plots <- reactive({
      info <- infos_plt_plotly()

      estimates <- estimates_plt_plotly() %>% plotly::layout(yaxis = list(side = "right"))

      # merge into one output plotly plot
      plotly::subplot(info, estimates,
        titleX = TRUE, titleY = TRUE,
        margin = 0,
        widths = c(.3, .7)
      ) %>%
        plotly::config(displayModeBar = FALSE)
    }) %>% # cache the value of the reactive - compute only if any of these objects changes
      # note that it is not recommended to include large objects as cache key,
      # because the whole key has to be hashed and objects serialized
      # see https://shiny.rstudio.com/articles/caching.html#faq-large-cache-key
      bindCache(mod(), input$true_theta, input$min_se, input$item_pos, input$only_ans)

    output$merged <- renderPlotly({
      # wait for the input to be available, as we construct the UI for the slider
      # dynamically, you can place this `req` call in `merged_plots` reactive or "earlier",
      # but placing it right here prevents somewhat futile call to `merged_plots`
      req(input$item_pos)

      merged_plots()
    })
  })
}


# create a non-reactive, static numeric vector consisting of ability levels
# we'll need these to construct Item Information Function (IIF) plots
# as this variable is constant across all sessions (for all users), it lives
#  outside the server function so it is not run for each user unnecessarily
# see https://shiny.rstudio.com/articles/scoping.html
info_thetas <- seq(-4, 4, length.out = 300L)

blue <- "#2C7BB6"
grey <- "#dadada"


## UI part ----------------------------------------------------------------

#' @rdname sm_cat_internal
#'
#' @importFrom plotly plotlyOutput
#'
sm_cat_ui <- function(id, imports = NULL, ...) {
  ns <- NS(id)


  tagList(
    h3("Computerized Adaptive Tests"),
    p(
      "This module provides illustration of computerized adaptive test (CAT) with the ",
      tags$code("{mirtCAT}"), " package.",
      "In the first step of the CAT, the respondent's estimated ability is preset at 0.
      In each step of the CAT (right slider),
      the item with the highest information for the estimated ability (left part of the figure)
      is presented to the respondent, and, based upon their answer (correct/incorrect),
      a new ability estimate is computed (right part of the figure).
      This is repeated in an iterative cycle, until a stopping criterion
      (defined by the standard error of the ability estimate, middle slider) is met."
    ),
    fluidRow(
      column(
        3,
        selectInput(ns("irt_model"),
          "IRT model to use",
          choices = c(
            "Module's example 2PL" = "example"
          )
        )
      )
    ),
    fluidRow(
      column(
        3,
        sliderInput(
          ns("true_theta"),
          # math mode is delimited with \( and \)
          # in R you have to escape \ with another \, hence the \\
          # in order to render the math, you have to use some math typesetting library,
          # e.g., mathjax or, preferably, katex
          # see preview_mod() definition for more detail
          "Respondent's true ability (\\(\\theta\\))",
          value = 1, min = -4, max = 4, step = .1
        )
      ),
      column(
        3,
        sliderInput(
          ns("min_se"),
          "Min. SE",
          value = .35, min = 0, max = 1.5, step = .01
        )
      ),
      column(
        3,
        # use less efficient way to obtain nice ticks in the slider
        # normal way would be to use `sliderInput()` here and
        # `updateSliderInput()` in the server part
        uiOutput(ns("item_pos_ui"))
      ),
      column(
        3,
        checkboxInput(ns("only_ans"),
          "Show IICs for only administered items",
          value = FALSE
        )
      )
    ),
    plotlyOutput(ns("merged"), height = "550px"),

    # sample R code -----------------------------------------------------------

    h4("Selected R code"),
    pre(includeText(system.file("sc/cat.R", package = "SIAmodules")), class = "mb-4 language-r"),

    # references --------------------------------------------------------

    h4("References"),
    p(
      "\u0160t\u011bp\u00e1nek, L., Martinkov\u00e1, P. (2020). Feasibility of computerized adaptive testing evaluated by Monte-Carlo and post-hoc simulations. In ",
      tags$i("Proceedings of the 2020 Federated Conference on Computer Science and Information Systems (FedCSIS),"),
      " pp. 359\\u2013367, ",
      tags$a(
        "doi:10.15439/2020F197",
        href = "https://doi.org/10.15439/2020F197",
        target = "_blank"
      )
    ),

    # acknowledgements --------------------------------------------------------

    h4("Acknowledgements"),
    p(
      "ShinyItemAnalysis Modules are developed by the",
      a(
        "Computational Psychometrics Group",
        href = "https://www.cs.cas.cz/comps/",
        target = "_blank"
      ),
      "supported by the Czech Science Foundation under Grant Number",
      a(
        "21-03658S",
        href = "https://www.cs.cas.cz/comps/projectTheorFoundComPs.html",
        target = "_blank",
        .noWS = "after"
      ),
      "."
    )
  )
}
