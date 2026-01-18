# Module documentation ----------------------------------------------------

# This is the user-facing documentation.

#' Interactive Module for Differential Item Functioning in Change (DIF-C)
#'
#' Interactive illustration of Differential Item Functioning in Change (DIF-C).
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
#' Adela Hladka \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{hladka@@cs.cas.cz} \cr
#'
#' @name sm_dif_c
#' @family SIAmodules
NULL


# Module definition -------------------------------------------------------

## Server part ------------------------------------------------------------

#' `sm_dif_c` module (internal documentation)
#'
#' This is the internal documentation of your module that is not included in the
#' help index of the package. You may include your notes here. For [user-facing
#' help page][sm_dif_c], please edit the entry in the YAML.
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
#' @rdname sm_dif_c_internal
#'
#' @import shiny
#' @import ggplot2
#'
#' @importFrom difNLR difNLR
#' @importFrom plotly hide_legend config
#' @importFrom dplyr summarise group_by n across where
#' @importFrom DT renderDT
#' @importFrom stats median qchisq sd symnum t.test
#' @importFrom shinyjs enable disable
#' @importFrom stats coef

sm_dif_c_server <- function(id, imports = NULL, ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # in case there are no imports, set a default values as in SIA
    if (is.null(imports)) {
      imports <- list(
        setting_figures = list(
          text_size = 12,
          height = 4,
          width = 8,
          dpi = 600
        )
      )
    }

    # LearningtoLearn data and static variables are precomputed in data-raw/create_internal_data.R
    # and loaded as the package attaches as `ltl` list


    # summary -----------------------------------------------------------------

    # total scores module (see the very end of this file for definitions)
    total_scores_summary_tab_server("total_scores_summary_tab_ts", ltl$total_score, ltl$group)
    total_scores_summary_tab_server("total_scores_summary_tab_dm", ltl$dif_matching, ltl$group)

    total_scores_t_test_server("total_scores_t_test_ts", ltl$total_score, ltl$group)
    total_scores_t_test_server("total_scores_t_test_dm", ltl$dif_matching, ltl$group)

    total_scores_hist_server("total_scores_hist_dm", ltl$dif_matching, ltl$group, "Total score in Grade 6")
    total_scores_hist_server("total_scores_hist_ts", ltl$total_score, ltl$group, "Total score in Grade 9")

    # DIF-C -------------------------------------------------------------------

    match_NLR <- c("DIF_NLR_summary_matching", "DIF_NLR_items_matching")
    puri_NLR <- c("DIF_NLR_purification_print", "DIF_NLR_purification_plot")

    # ** UPDATING INPUTS ####
    DIF_nlr <- reactiveValues(
      model = NULL,
      type = NULL,
      correction = NULL,
      purification = NULL
    )

    # ** Updating model ####
    observeEvent(input$DIF_NLR_model_print, {
      DIF_nlr$model <- input$DIF_NLR_model_print
    })
    observeEvent(input$DIF_NLR_model_plot, {
      DIF_nlr$model <- input$DIF_NLR_model_plot
    })
    observeEvent(DIF_nlr$model, {
      if (DIF_nlr$model != input$DIF_NLR_model_print) {
        updateSelectInput(
          session = session,
          inputId = "DIF_NLR_model_print",
          selected = DIF_nlr$model
        )
      }
      if (DIF_nlr$model != input$DIF_NLR_model_plot) {
        updateSelectInput(
          session = session,
          inputId = "DIF_NLR_model_plot",
          selected = DIF_nlr$model
        )
      }
    })

    # ** Updating type ####
    observeEvent(input$DIF_NLR_type_print,
      {
        DIF_nlr$type <- input$DIF_NLR_type_print
      },
      ignoreNULL = FALSE
    )
    observeEvent(input$DIF_NLR_type_plot,
      {
        DIF_nlr$type <- input$DIF_NLR_type_plot
      },
      ignoreNULL = FALSE
    )

    observeEvent(DIF_nlr$type,
      {
        dif_type <- DIF_nlr$type

        # to uncheck all, we have to change the input to empty char.
        if (is.null(dif_type)) dif_type <- character(0)

        if (!setequal(dif_type, input$DIF_NLR_type_print)) {
          updateCheckboxGroupInput(
            session = session,
            inputId = "DIF_NLR_type_print",
            selected = dif_type
          )
        }

        if (!setequal(dif_type, input$DIF_NLR_type_plot)) {
          updateCheckboxGroupInput(
            session = session,
            inputId = "DIF_NLR_type_plot",
            selected = dif_type
          )
        }
      },
      ignoreNULL = FALSE
    )

    # ** Updating correction ####
    observeEvent(input$DIF_NLR_correction_method_print, {
      DIF_nlr$correction <- input$DIF_NLR_correction_method_print
    })
    observeEvent(input$DIF_NLR_correction_method_plot, {
      DIF_nlr$correction <- input$DIF_NLR_correction_method_plot
    })
    observeEvent(DIF_nlr$correction, {
      if (DIF_nlr$correction != input$DIF_NLR_correction_method_print) {
        updateSelectInput(
          session = session,
          inputId = "DIF_NLR_correction_method_print",
          selected = DIF_nlr$correction
        )
      }
      if (DIF_nlr$correction != input$DIF_NLR_correction_method_plot) {
        updateSelectInput(
          session = session,
          inputId = "DIF_NLR_correction_method_plot",
          selected = DIF_nlr$correction
        )
      }
    })

    # ** Updating purification ####
    observeEvent(input$DIF_NLR_purification_print, {
      DIF_nlr$purification <- input$DIF_NLR_purification_print
    })
    observeEvent(input$DIF_NLR_purification_plot, {
      DIF_nlr$purification <- input$DIF_NLR_purification_plot
    })
    observeEvent(DIF_nlr$purification, {
      if (DIF_nlr$purification != input$DIF_NLR_purification_print) {
        updateCheckboxInput(
          session = session,
          inputId = "DIF_NLR_purification_print",
          value = DIF_nlr$purification
        )
      }
      if (DIF_nlr$purification != input$DIF_NLR_purification_plot) {
        updateCheckboxInput(
          session = session,
          inputId = "DIF_NLR_purification_plot",
          value = DIF_nlr$purification
        )
      }
    })

    # ** Updating DMV ####
    observeEvent(input$DIF_NLR_summary_matching, {
      DIF_nlr$matching <- input$DIF_NLR_summary_matching
    })
    observeEvent(input$DIF_NLR_items_matching, {
      DIF_nlr$matching <- input$DIF_NLR_items_matching
    })
    observeEvent(DIF_nlr$matching, {
      if (DIF_nlr$matching != input$DIF_NLR_summary_matching) {
        updateCheckboxInput(
          session = session,
          inputId = "DIF_NLR_summary_matching",
          value = DIF_nlr$matching
        )
      }
      if (DIF_nlr$matching != input$DIF_NLR_items_matching) {
        updateCheckboxInput(
          session = session,
          inputId = "DIF_NLR_items_matching",
          value = DIF_nlr$matching
        )
      }
    })

    mapply(
      function(match, puri) {
        observeEvent(input[[match]], {
          if (input[[match]] == "zuploaded") {
            updateCheckboxInput(session, puri, value = FALSE)
            shinyjs::disable(puri)
          } else {
            shinyjs::enable(puri)
          }
        })
      },
      match = match_NLR, puri = puri_NLR
    )

    # ** Updating item slider ####
    observe({
      item_count <- ncol(ltl$items)
      updateSliderInput(
        session = session,
        inputId = "DIF_NLR_item_plot",
        max = item_count
      )
    })

    # ** MODEL ####
    model_DIF_NLR <- reactive({
      validate(
        # type is checked with shiny::isTruthy, so no condition needed
        need(DIF_nlr$type, "At least one parameter is required for DIF testing.")
      )

      data <- ltl$items
      group <- ltl$group

      model <- DIF_nlr$model
      type <- paste0(DIF_nlr$type, collapse = "")
      adj.method <- DIF_nlr$correction
      purify <- DIF_nlr$purification
      match <- DIF_nlr$matching


      # if the observed variable is not zscore, use grade 6 zscore
      if (match == "zuploaded") {
        match <- scale(ltl$dif_matching)
      }

      fit <- tryCatch(
        difNLR(
          Data = data, group = group, focal.name = 1, match = match,
          model = model, type = type,
          p.adjust.method = adj.method, purify = purify,
          test = "LR", method = "nls"
        ),
        error = function(e) e
      )

      validate(
        need(
          inherits(fit, "difNLR"),
          paste0("Error returned: ", fit$message)
        ),
        errorClass = "validation-error"
      )

      fit
    })

    # ** Enabling/disabling options for type of DIF in print ####
    observeEvent(input$DIF_NLR_model_print, {
      # what parameters can be selected with choice of model
      enaSelection <- switch(input$DIF_NLR_model_print,
        "Rasch" = c("b"),
        "1PL" = c("b"),
        "2PL" = c("a", "b"),
        "3PLcg" = c("a", "b"),
        "3PLdg" = c("a", "b"),
        "3PLc" = c("a", "b", "c"),
        "3PLd" = c("a", "b", "d"),
        "4PLcgdg" = c("a", "b"),
        "4PLcg" = c("a", "b", "d"),
        "4PLdg" = c("a", "b", "c"),
        "4PL" = c("a", "b", "c", "d")
      )
      # what parameters cannot be selected with choice of model
      disSelection <- setdiff(letters[1:4], enaSelection)

      # converting letters to numbers
      myLetters <- letters[1:26]
      disNum <- match(disSelection, myLetters)
      enaNum <- match(enaSelection, myLetters)

      # updating selected choices for type of DIF
      updateCheckboxGroupInput(
        session = session,
        inputId = "DIF_NLR_type_print",
        selected = enaSelection
      )

      # create object that identifies enabled and disabled options
      disElement <- paste0("#", ns("DIF_NLR_type_print"), " :nth-child(", disNum, ") label")
      enaElement <- paste0("#", ns("DIF_NLR_type_print"), " :nth-child(", enaNum, ") label")

      # disable checkbox options of group
      shinyjs::enable(selector = enaElement)
      shinyjs::disable(selector = disElement)
    })

    # ** Equation ####
    output$DIF_NLR_equation_print <- renderUI({
      model <- input$DIF_NLR_model_print

      if (model == "Rasch") {
        txta <- ""
      } else {
        if (model == "1PL") {
          txta <- "a_i"
        } else {
          txta <- "a_{iG_p}"
        }
      }

      txtb <- "b_{iG_p}"

      txt2 <- paste0(txta, "\\left(Z_p - ", txtb, "\\right)")
      txt2 <- paste0("e^{", txt2, "}")
      txt2 <- paste0("\\frac{", txt2, "}{1 + ", txt2, "}")

      if (model %in% c("3PLcg", "4PLcgdg", "4PLcg")) {
        txtc <- "c_i"
      } else {
        if (model %in% c("3PLc", "4PLdg", "4PL")) {
          txtc <- "c_{iG_p}"
        } else {
          txtc <- ""
        }
      }

      if (model %in% c("3PLdg", "4PLcgdg", "4PLdg")) {
        txtd <- "d_i"
      } else {
        if (model %in% c("3PLd", "4PLcg", "4PL")) {
          txtd <- "d_{iG_p}"
        } else {
          txtd <- ""
        }
      }

      if (txtc == "" & txtd == "") {
        txt3 <- ""
      } else {
        if (txtd == "") {
          txt3 <- paste0(txtc, " + \\left(1 - ", txtc, "\\right) \\cdot ")
        } else {
          if (txtc == "") {
            txt3 <- txtd
          } else {
            txt3 <- paste0(txtc, " + \\left(", txtd, " - ", txtc, "\\right) \\cdot ")
          }
        }
      }

      txt1 <- paste0(
        "\\mathrm{P}\\left(Y_{pi} = 1 | Z_p, G_p\\right) = "
      )

      txt <- paste0("$$", txt1, txt3, txt2, "$$")
      txt
    })

    # ** Enabling/disabling options for type of DIF in plot ####
    observeEvent(input$DIF_NLR_model_plot, {
      # what parameters can be selected with choice of model
      enaSelection <- switch(input$DIF_NLR_model_plot,
        "Rasch" = c("b"),
        "1PL" = c("b"),
        "2PL" = c("a", "b"),
        "3PLcg" = c("a", "b"),
        "3PLdg" = c("a", "b"),
        "3PLc" = c("a", "b", "c"),
        "3PLd" = c("a", "b", "d"),
        "4PLcgdg" = c("a", "b"),
        "4PLcg" = c("a", "b", "d"),
        "4PLdg" = c("a", "b", "c"),
        "4PL" = c("a", "b", "c", "d")
      )
      # what parameters cannot be selected with choice of model
      disSelection <- setdiff(letters[1:4], enaSelection)

      # converting letters to numbers
      myLetters <- letters[1:26]
      disNum <- match(disSelection, myLetters)
      enaNum <- match(enaSelection, myLetters)

      # updating selected choices for type of DIF
      updateCheckboxGroupInput(
        session = session,
        inputId = "DIF_NLR_type_plot",
        selected = enaSelection
      )

      # create object that identifies enabled and disabled options
      disElement <- paste0("#", ns("DIF_NLR_type_plot"), " :nth-child(", disNum, ") label")
      enaElement <- paste0("#", ns("DIF_NLR_type_plot"), " :nth-child(", enaNum, ") label")

      # disable checkbox options of group
      shinyjs::enable(selector = enaElement)
      shinyjs::disable(selector = disElement)
    })

    # ** SUMMARY ####

    # ** Summary table ####
    coef_nlr_dif <- reactive({
      model <- model_DIF_NLR()

      stat <- model$Sval

      # deal with only one pval base od model specs
      pval <- if (model$p.adjust.method == "none") {
        model$pval
      } else {
        model$adj.pval
      }

      pval_symb <- symnum(pval,
        c(0, 0.001, 0.01, 0.05, 0.1, 1),
        symbols = c("***", "**", "*", ".", "")
      )
      pval_symb[pval_symb == "?"] <- ""

      res <- coeffs_se_names()
      coeffs <- res$coeffs
      se <- res$se

      colnames(coeffs) <- paste0("\\(\\mathit{", gsub("Dif", "_{Dif}", colnames(coeffs)), "}\\)")
      colnames(se) <- paste0("SE(\\(\\mathit{", gsub("Dif", "_{Dif}", colnames(se)), "}\\))")

      # zigzag
      coeffs_se <- cbind(coeffs, se)[, order(c(seq(ncol(coeffs)), seq(ncol(se))))]

      tab <- data.frame(
        stat,
        paste(formatC(pval, digits = 3, format = "f"), pval_symb),
        coeffs_se
      )

      colnames(tab) <-
        c(
          "LR (\\(\\mathit{\\chi^2}\\))",
          ifelse(
            model$p.adjust.method == "none",
            "\\(\\mathit{p}\\)-value",
            "adj. \\(\\mathit{p}\\)-value"
          ),
          colnames(coeffs_se)
        )

      rownames(tab) <- ltl$item_names

      tab
    })

    output$coef_nlr_dif <- renderDT(
      {
        coef_nlr_dif() |> mutate(across(where(is.numeric), ~ round(.x, 3)))
      },
      options = list(
        dom = "tipr", pageLength = 10, scrollX = TRUE,
        pagingType = "full_numbers",
        fixedColumns = list(leftColumns = 1)
      ),
      selection = "none",
      # style = "bootstrap4",
      extensions = "FixedColumns"
    )

    coeffs_se_names <- reactive({
      model <- model_DIF_NLR()

      tmp <- coef(model, IRTpars = TRUE, SE = TRUE, CI = 0, simplify = TRUE)

      res <- NULL
      res$se <- as.data.frame(tmp[grepl("SE", rownames(tmp)), , drop = FALSE])
      rownames(res$se) <- gsub(" SE", "", rownames(res$se))
      res$coeffs <- as.data.frame(tmp[grepl("estimate", rownames(tmp)), , drop = FALSE])
      rownames(res$coeffs) <- gsub(" estimate", "", rownames(res$coeffs))
      res
    })

    # ** Items detected text ####
    output$nlr_dif_items <- renderPrint({
      DIFitems <- model_DIF_NLR()$DIFitems
      if (DIFitems[1] == "No DIF item detected") {
        txt <- "No item was detected as DIF."
      } else {
        txt <- paste0("Items detected as DIF items: ", paste(ltl$item_names[DIFitems], collapse = ", "))
      }
      HTML(txt)
    })

    # ** Purification table ####
    dif_nlr_puri_table <- reactive({
      tab <- model_DIF_NLR()$difPur

      if (!is.null(tab)) {
        colnames(tab) <- ltl$item_names
        rownames(tab) <- paste0("Step ", seq(0, nrow(tab) - 1))
        tab
      }
    })
    output$dif_nlr_puri_table <- renderDT(
      dif_nlr_puri_table()
    )

    # ** Purification info - number of iter ####
    output$dif_nlr_puri_info <- renderPrint({
      model <- model_DIF_NLR()
      if (input$DIF_NLR_purification_print & !is.null(model_DIF_NLR()$difPur)) {
        cat("The table below describes purification process. The rows correspond to the purification iteration and the columns
        correspond to items. Value of '1' in the i-th row means that an item was detected as DIF in (i-1)-th step,
        while the value of '0' means that the item was not detected as DIF. The first row corresponds to the initial
        classification of the items when all items were used for the calculation of the DIF matching criterion. ")
        nrIter <- model$nrPur
        cat(
          "In this case, the convergence was", ifelse(model$conv.puri, "reached", "NOT reached even"), "after", nrIter,
          ifelse(nrIter == 1, "iteration.", "iterations.")
        )
      } else if (input$DIF_NLR_purification_print & is.null(model_DIF_NLR()$difPur)) {
        cat("No DIF item was detected whatsoever, nothing to show.")
      } else {
        cat("Item purification was not requested, nothing to show.")
      }
    })

    # ** Note setup ####
    note_nlr <- reactive({
      res <- NULL

      model <- model_DIF_NLR()

      df <- model$df
      if (length(dim(df)) == 2L) {
        df <- df[, 1L]
      }

      thr <- if (length(unique(df)) == 1) {
        unique(qchisq(1 - model$alpha, df))
      } else {
        NULL
      }

      res$mod <- paste("Model:", switch(unique(model$model),
        "Rasch" = "Rasch model",
        "1PL" = "1PL model",
        "2PL" = "2PL model",
        "3PL" = "3PL model",
        "3PLcg" = "3PL model with fixed guessing for groups",
        "3PLdg" = "3PL model with fixed inattention parameter for groups",
        "3PLc" = "3PL model",
        "3PLd" = "3PL model with inattention parameter",
        "4PLcgdg" = "4PL model with fixed guessing and inattention parameter for groups",
        "4PLcgd" = "4PL model with fixed guessing for groups",
        "4PLd" = "4PL model with fixed guessing for groups",
        "4PLcdg" = "4PL model with fixed inattention parameter for groups",
        "4PLc" = "4PL model with fixed inattention parameter for groups",
        "4PL" = "4PL model"
      ))

      res$dmv <- paste("Observed score:", switch(as.character(model$match[1]), # ensures number is recognize as unnamed element
        "score" = "total score",
        "zscore" = "standardized total score",
        "uploaded"
      ))

      res$type <-
        paste0(
          "DIF type tested: difference in parameters ",
          paste0(input$DIF_NLR_type_print, collapse = ", ")
        )

      res$p_adj <- paste("P-value correction method:", switch(model$p.adjust.method,
        holm = "Holm",
        hochberg = "Hochberg",
        hommel = "Hommel",
        bonferroni = "Bonferroni",
        BH = "Benjamini-Hochberg",
        BY = "Benjamini-Yekutieli",
        fdr = "FDR",
        none = "none"
      ))

      res$puri <- paste("Item purification:", ifelse(model$purification == T, "used", "unutilized"))
      res$thr_rounded <- paste("Detection threshold:", round(thr, 3))

      res
    })

    output$note_nlr <- renderUI({
      HTML(
        paste(
          "Notes:",
          note_nlr()$dmv,
          note_nlr()$mod,
          note_nlr()$type,
          note_nlr()$p_adj,
          note_nlr()$puri,
          note_nlr()$thr_rounded,
          "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
          sep = "</br>"
        )
      )
    })

    # ** Download tables ####
    output$download_nlr_dif <- downloadHandler(
      filename = function() {
        paste("DIF_NLR_statistics", ".csv", sep = "")
      },
      content = function(file) {
        data <- coef_nlr_dif()

        coef_names <- colnames(coeffs_se_names()$coeffs)
        se_names <- paste0("SE(", colnames(coeffs_se_names()$se), ")")

        par_names <- c(coef_names, se_names)[order(c(seq(coef_names), seq(se_names)))]

        colnames(data) <-
          c(
            "LR (X^2)",
            ifelse(
              "\\(mathit{p}\\)-value" %in% colnames(data),
              "p-value",
              "adj. p-value"
            ),
            "",
            "",
            par_names
          )

        rownames(data) <- ltl$item_names

        write.csv(data[, -4], file) # w/o blank col
        write(paste(
          "Note:",
          note_nlr()$dmv,
          note_nlr()$mod,
          gsub(",", "", note_nlr()$type), # get rid of the comma - it separates col in CSV
          note_nlr()$p_adj,
          note_nlr()$puri,
          note_nlr()$thr_rounded,
          "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
          sep = "\n"
        ), file, append = T)
      }
    )
    output$download_nlr_dif_puri <- downloadHandler(
      filename = function() {
        paste0("DIF_NLR_purification", ".csv")
      },
      content = function(file) {
        data <- dif_nlr_puri_table()
        write.csv(data, file)
      }
    )


    # ** ITEMS ####

    # ** Plot ####
    plot_DIF_NLRInput <- reactive({
      fit <- model_DIF_NLR()
      item <- input$DIF_NLR_item_plot

      g <- plot(fit, item = item)[[1]] +
        xlab(paste("Standardized total score in", get_grade_name(input$DIF_NLR_summary_matching))) +
        theme_app() +
        theme(
          legend.box.just = "top",
          legend.position = c(0.01, 0.98),
          legend.justification = c(0, 1),
          legend.key.width = unit(1, "cm"),
          legend.box = "horizontal"
        ) +
        ggtitle(ltl$item_names[item])
      g
    })

    # ** Output plot ####
    output$plot_DIF_NLR <- renderPlotly({
      g <- plot_DIF_NLRInput()
      p <- ggplotly(g)

      p$x$data[[1]]$text <- paste0(
        "Group: Basic school track", "<br />",
        "Standardized total score in ", get_grade_name(input$DIF_NLR_summary_matching), ": ", round(p$x$data[[1]]$x, 3), "<br />",
        "Probability: ", round(p$x$data[[1]]$y, 3)
      )
      p$x$data[[2]]$text <- paste0(
        "Group: Selective academic track", "<br />",
        "Standardized total score in ", get_grade_name(input$DIF_NLR_summary_matching), ": ", round(p$x$data[[2]]$x, 3), "<br />",
        "Probability: ", round(p$x$data[[2]]$y, 3)
      )

      p$x$data[[3]]$text <- paste0(
        gsub(
          "size", "Group: Basic school track<br />Count",
          unlist(lapply(strsplit(p$x$data[[3]]$text, "<br />", fixed = TRUE), function(x) x[3]))
        ),
        "<br />Standardized total score in ", get_grade_name(input$DIF_NLR_summary_matching), ": ", round(p$x$data[[3]]$x, 3),
        "<br />Empirical probability: ", round(p$x$data[[3]]$y, 3)
      )

      p$x$data[[4]]$text <- paste0(
        gsub(
          "size", "Group: Selective academic school track<br />Count",
          unlist(lapply(strsplit(p$x$data[[4]]$text, "<br />", fixed = TRUE), function(x) x[3]))
        ),
        "<br />Standardized total score in ", get_grade_name(input$DIF_NLR_summary_matching), ": ", round(p$x$data[[4]]$x, 3),
        "<br />Empirical probability: ", round(p$x$data[[4]]$y, 3)
      )

      p$elementId <- NULL
      hide_legend(p) |> plotly::config(displayModeBar = FALSE)
    })

    # ** DB for plot ####
    output$DP_plot_DIF_NLR <- downloadHandler(
      filename = function() {
        paste0("fig_DIFNonlinear_", ltl$item_names[input$DIF_NLR_item_plot], ".png")
      },
      content = function(file) {
        ggsave(file,
          plot = plot_DIF_NLRInput() +
            theme(text = element_text(size = imports$setting_figures$text_size)),
          device = "png",
          height = imports$setting_figures$height, width = imports$setting_figures$width,
          dpi = imports$setting_figures$dpi
        )
      }
    )

    # ** Equation ####
    output$DIF_NLR_equation_plot <- renderUI({
      model <- input$DIF_NLR_model_plot

      if (model == "Rasch") {
        txta <- ""
      } else {
        if (model == "1PL") {
          txta <- "a_i"
        } else {
          txta <- "a_{iG_p}"
        }
      }

      txtb <- "b_{iG_p}"

      txt2 <- paste0(txta, "\\left(Z_p - ", txtb, "\\right)")
      txt2 <- paste0("e^{", txt2, "}")
      txt2 <- paste0("\\frac{", txt2, "}{1 + ", txt2, "}")

      if (model %in% c("3PLcg", "4PLcgdg", "4PLcg")) {
        txtc <- "c_i"
      } else {
        if (model %in% c("3PLc", "4PLdg", "4PL")) {
          txtc <- "c_{iG_p}"
        } else {
          txtc <- ""
        }
      }

      if (model %in% c("3PLdg", "4PLcgdg", "4PLdg")) {
        txtd <- "d_i"
      } else {
        if (model %in% c("3PLd", "4PLcg", "4PL")) {
          txtd <- "d_{iG_p}"
        } else {
          txtd <- ""
        }
      }

      if (txtc == "" & txtd == "") {
        txt3 <- ""
      } else {
        if (txtd == "") {
          txt3 <- paste0(txtc, " + \\left(1 - ", txtc, "\\right) \\cdot ")
        } else {
          if (txtc == "") {
            txt3 <- txtd
          } else {
            txt3 <- paste0(txtc, " + \\left(", txtd, " - ", txtc, "\\right) \\cdot ")
          }
        }
      }

      txt1 <- paste0(
        "\\mathrm{P}\\left(Y_{pi} = 1 | Z_p, G_p\\right) = "
      )

      txt <- paste0("$$", txt1, txt3, txt2, "$$")
      txt
    })

    # ** Table of coefficients ####
    output$tab_coef_DIF_NLR <- renderTable(
      {
        item <- input$DIF_NLR_item_plot
        fit <- model_DIF_NLR()

        tab <- t(coef(fit, IRTpars = TRUE, SE = TRUE, CI = 0, item = item)[[1]])

        rownames(tab) <- paste0("\\(\\mathit{", gsub("Dif", "_{Dif}", rownames(tab)), "}\\)")
        colnames(tab) <- c("Estimate", "SE")

        tab
      },
      include.rownames = T
    )
  })
}



## UI part ----------------------------------------------------------------

#' @rdname sm_dif_c_internal
#'
#' @importFrom plotly plotlyOutput
#' @importFrom DT DTOutput
#'
sm_dif_c_ui <- function(id, imports = NULL, ...) {
  ns <- NS(id)


  tagList(
    h3("Differential Item Functioning in Change (DIF-C)"),
    p(
      "This ", code("ShinyItemAnalysis"), " module provides an interactive
        illustration of Differential Item Functioning in Change (DIF-C) using
        binary ", code("LearningToLearn"), " data, as described in detail by ",
      a(
        "Martinkov\u00e1, Hladk\u00e1, and Potu\u017en\u00edkov\u00e1 (2020)",
        href = "https://doi.org/10.1016/j.learninstruc.2019.101286",
        target = "_blank",
        .noWS = "after"
      ),
      ". Their study demonstrates that this more detailed item-level analysis
        can reveal between-group differences in pre-post gains even when no
        differences are observable in total score gains. The DIF-C analysis here
        is implemented with generalized logistic regression models from the ",
      code("difNLR"), " package ",
      a(
        "(Hladk\u00e1 & Martinkov\u00e1, 2020)",
        href = "https://doi.org/10.32614/RJ-2020-014",
        target = "_blank",
        .noWS = "after"
      ),
      "."
    ),


    # tabs --------------------------------------------------------------------

    tabsetPanel(
      tabPanel(
        "Total scores",
        h3("Summary of total scores"),
        p(
          "This section examines differences in observed scores between two
          groups of respondents: the basic school track and selective academic
          track. To explore differential item functioning in change (DIF-C),
          refer to the dedicated DIF sections. "
        ),

        # total scores module (see shiny modules definition in the end of the file)
        h4("Grade 6"),
        h5("Summary of total scores for groups"),
        total_scores_summary_tab_ui(ns("total_scores_summary_tab_dm")),
        h5("Comparison of total scores"),
        total_scores_t_test_ui(ns("total_scores_t_test_dm")),
        h5("Histograms of total scores for groups"),
        total_scores_hist_ui(ns("total_scores_hist_dm")),
        h4("Grade 9"),
        h5("Summary of total scores for groups"),
        total_scores_summary_tab_ui(ns("total_scores_summary_tab_ts")),
        h5("Comparison of total scores"),
        total_scores_t_test_ui(ns("total_scores_t_test_ts")),
        h5("Histograms of total scores for groups"),
        total_scores_hist_ui(ns("total_scores_hist_ts"))
      ),
      tabPanel(
        "DIF-C Summary",
        br(),
        h3("DIF-C analysis"),
        p(
          "Differential item functioning in change (DIF-C) is assessed by analyzing DIF on Grade 9 item responses
          while matching on Grade 6 total scores of the same respondents in a longitudinal framework.
          This analysis compares two respondent groups: the basic school track (reference) and the selective academic track. "
        ),
        h4("Method specification"),
        p(
          "Here, you can specify the assumed",
          strong("model", .noWS = "after"),
          ". By default, the 2PL model is selected, as in the original study.
          In the 3PL and 4PL models, the abbreviations \\(c_{g}\\) or \\(d_{g}\\)
          indicate that the parameters \\(c_i\\) or \\(d_i\\) are assumed
          to be equal across groups; otherwise they are allowed to differ.
          With the ", strong("type"), " option, you can specify which parameters
          to test for between-group differences in DIF analysis. Additionally,
          you can choose a ", strong("correction method"), "for multiple
          comparisons or enable ", strong("item purification", .noWS = "after"),
          ", which is available for DIF analysis only. "
        ),
        p(
          "Finally, you may change the", strong("Observed score", .noWS = "after"),
          ". Selecting the standardized total score from Grade 6 enables DIF-C
          analysis, while using scores from Grade 9 allows for standard DIF analysis. "
        ),
        fluidRow(
          column(
            2,
            selectInput(
              inputId = ns("DIF_NLR_model_print"),
              label = "Model",
              choices = c(
                "Rasch" = "Rasch",
                "1PL" = "1PL",
                "2PL" = "2PL",
                "3PLcg" = "3PLcg",
                "3PLdg" = "3PLdg",
                "3PLc" = "3PLc",
                "3PLd" = "3PLd",
                "4PLcgdg" = "4PLcgdg",
                "4PLcgd" = "4PLcgd",
                "4PLcdg" = "4PLcdg",
                "4PL" = "4PL"
              ),
              selected = "2PL"
            )
          ),
          column(
            1,
            checkboxGroupInput(
              inputId = ns("DIF_NLR_type_print"),
              label = "Type",
              choices = c(
                "\\(a\\)" = "a",
                "\\(b\\)" = "b",
                "\\(c\\)" = "c",
                "\\(d\\)" = "d"
              ),
              selected = c("a", "b")
            )
          ),
          column(
            2,
            selectInput(
              inputId = ns("DIF_NLR_correction_method_print"),
              label = "Correction method",
              choices = c(
                "Benjamini-Hochberg" = "BH",
                "Benjamini-Yekutieli" = "BY",
                "Bonferroni" = "bonferroni",
                "Holm" = "holm",
                "Hochberg" = "hochberg",
                "Hommel" = "hommel",
                "None" = "none"
              ),
              selected = "none"
            ),
            checkboxInput(
              inputId = ns("DIF_NLR_purification_print"),
              label = "Item purification",
              value = FALSE
            )
          ),
          column(
            2,
            selectInput(
              inputId = ns("DIF_NLR_summary_matching"),
              label = "Observed score",
              choices = grades_dict,
              selected = "zuploaded"
            )
          )
        ),
        h4("Equation"),
        p("The displayed equation is based on the model selected below"),
        fluidRow(
          column(12, uiOutput(ns("DIF_NLR_equation_print")), align = "center")
        ),
        h4("Summary table"),
        p(
          "This summary table contains information about DIF test statistic
          \\(LR(\\chi^2)\\), corresponding \\(p\\)-values considering selected
          adjustement, and significance codes. This table also provides
          estimated parameters for the best fitted model for each item. Note
          that \\(a_{iG_p}\\) (and also other parameters) from the equation
          above consists of a parameter for the reference group (basic school track) and a
          parameter for the difference between the two groups,
          i.e., \\(a_{iG_p} = a_{i} + a_{iDif}G_{p}\\), where \\(G_{p} = 0\\)
          for the basic school track and \\(G_{p} = 1\\) for the selective academic track. "
        ),
        uiOutput(ns("DIF_NLR_na_alert")),
        strong(textOutput(ns("nlr_dif_items"))),
        br(),
        tags$head(tags$style("#coef_nlr_dif  {white-space: nowrap;}")),
        fluidRow(column(12, align = "left", DTOutput(ns("coef_nlr_dif"), width = "100%"))),
        fluidRow(column(12, align = "left", uiOutput(ns("note_nlr")))),
        br(),
        fluidRow(column(
          2,
          downloadButton(ns("download_nlr_dif"), "Download table")
        )),
        br(),
        h4("Purification process"),
        textOutput(ns("dif_nlr_puri_info")),
        br(),
        tags$head(tags$style(
          "#dif_nlr_puri_table  {white-space: nowrap;}"
        )),
        fluidRow(column(
          12,
          align = "center", DTOutput(ns("dif_nlr_puri_table"))
        )),
        conditionalPanel(
          condition = "input.DIF_NLR_purification_print",
          downloadButton(ns("download_nlr_dif_puri"), "Download table"),
          class = "mb-5", ns = ns
        )
      ),
      tabPanel(
        "DIF-C Items",
        h3("DIF-C analysis"),
        p(
          "Differential item functioning in change (DIF-C) is assessed by analyzing DIF on Grade 9 item responses
          while matching on Grade 6 total scores of the same respondents in a longitudinal framework.
          This analysis compares two respondent groups: the basic school track (reference) and the selective academic track. "
        ),
        h4("Method specification"),
        p(
          "Here, you can specify the assumed",
          strong("model", .noWS = "after"),
          ". By default, the 2PL model is selected, as in the original study.
          In the 3PL and 4PL models, the abbreviations \\(c_{g}\\) or \\(d_{g}\\)
          indicate that the parameters \\(c_i\\) or \\(d_i\\) are assumed
          to be equal across groups; otherwise they are allowed to differ.
          With the ", strong("type"), " option, you can specify which parameters
          to test for between-group differences in DIF analysis. Additionally,
          you can choose a ", strong("correction method"), "for multiple
          comparisons or enable ", strong("item purification", .noWS = "after"),
          ", which is available for DIF analysis only. "
        ),
        p(
          "Finally, you may change the", strong("Observed score", .noWS = "after"),
          ". Selecting the standardized total score from Grade 6 enables DIF-C
          analysis, while using scores from Grade 9 allows for standard DIF analysis.
          For selected", strong("item"), "you can view a plot
          of its characteristic curves and a table of its estimated parameters
          with standard errors. "
        ),
        fluidRow(
          column(
            2,
            selectInput(
              inputId = ns("DIF_NLR_model_plot"),
              label = "Model",
              choices = c(
                "Rasch" = "Rasch",
                "1PL" = "1PL",
                "2PL" = "2PL",
                "3PLcg" = "3PLcg",
                "3PLdg" = "3PLdg",
                "3PLc" = "3PLc",
                "3PLd" = "3PLd",
                "4PLcgdg" = "4PLcgdg",
                "4PLcgd" = "4PLcgd",
                "4PLcdg" = "4PLcdg",
                "4PL" = "4PL"
              ),
              selected = "2PL"
            )
          ),
          column(
            1,
            checkboxGroupInput(
              inputId = ns("DIF_NLR_type_plot"),
              label = "Type",
              choices = c(
                "\\(a\\)" = "a",
                "\\(b\\)" = "b",
                "\\(c\\)" = "c",
                "\\(d\\)" = "d"
              ),
              selected = c("a", "b")
            )
          ),
          column(
            2,
            selectInput(
              inputId = ns("DIF_NLR_correction_method_plot"),
              label = "Correction method",
              choices = c(
                "Benjamini-Hochberg" = "BH",
                "Benjamini-Yekutieli" = "BY",
                "Bonferroni" = "bonferroni",
                "Holm" = "holm",
                "Hochberg" = "hochberg",
                "Hommel" = "hommel",
                "None" = "none"
              ),
              selected = "none"
            ),
            checkboxInput(
              inputId = ns("DIF_NLR_purification_plot"),
              label = "Item purification",
              value = FALSE
            )
          ),
          column(
            2,
            selectInput(
              inputId = ns("DIF_NLR_items_matching"),
              label = "Observed score",
              choices = grades_dict,
              selected = "zuploaded"
            )
          ),
          column(
            2,
            sliderInput(
              inputId = ns("DIF_NLR_item_plot"),
              label = "Item",
              min = 1,
              value = 1,
              max = 41,
              step = 1,
              animate = animationOptions(interval = 1600)
            )
          )
        ),
        h4("Plot with estimated DIF generalized logistic curve"),
        p(
          "Points in the plot represent the proportion of correct responses (empirical
          probabilities) with respect to standardized total score in Grade 6. Their size
          reflects the number of respondents who achieved a particular level of
          standardized total score in Grade 6 with respect to the group membership (either
          basic school track or selective academic track)."
        ),
        plotlyOutput(ns("plot_DIF_NLR")),
        downloadButton(ns("DP_plot_DIF_NLR"), "Download figure"),
        h4("Equation"),
        fluidRow(
          column(12, uiOutput(ns("DIF_NLR_equation_plot")), align = "center")
        ),
        h4("Table of parameters"),
        p(
          "This table summarizes estimated item parameters align with their
          standard errors. Note that \\(a_{iG_p}\\) (and also other
          parameters) from the equation above consists of a parameter for the
          reference group (basic school track) and a parameter for the difference
          between the two groups, i.e., \\(a_{iG_p} = a_{i} + a_{iDif}G_{p}\\),
          where \\(G_{p} = 0\\) for the basic school track and \\(G_{p} = 1\\)
          for the selective academic track. "
        ),
        fluidRow(
          column(12, tableOutput(ns("tab_coef_DIF_NLR")), align = "center")
        )
      )
    ),


    # sample R code -----------------------------------------------------------

    h4("Selected R code"),
    pre(includeText(system.file("sc/difcLtL.R", package = "SIAmodules")), class = "mb-4 language-r"),

    # references --------------------------------------------------------
    h4("References"),
    HTML('<ul class = "biblio">
              <li>Martinkov\u00e1, P., Drabinov\u00e1, A., & Potu\u017en\u00edkov\u00e1, E. (2020).
              Is academic tracking related to gains in learning competence?
              Using propensity score matching and differential item change functioning analysis for better understanding
              of tracking implications.
              <i>Learning and Instruction 66</i>(April).
              <a href = "https://doi.org/10.1016/j.learninstruc.2019.101286",
              target = "_blank">doi:10.1016/j.learninstruc.2019.101286</a>
              </li>

              <li>Hladk\u00e1 A., & Martinkov\u00e1, P. (2020).
              difNLR: Generalized logistic regression models for DIF and DDF detection.
              <i>The R Journal, 12</i>(1), 300-323.
              <a href = "https://doi.org/10.32614/RJ-2020-014", target = "_blank">doi:10.32614/RJ-2020-014.</a>
              </li>

            </ul>'),

    # acknowledgements --------------------------------------------------------

    h4("Acknowledgements"),
    p(
      "This ShinyItemAnalysis Module was developed with support of the Czech Science Foundation under Grant Number",
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



## globals

grades_dict <- c(
  "Grade 6" = "zuploaded",
  "Grade 9" = "zscore"
)

get_grade_name <- function(input) {
  names(grades_dict[input == grades_dict])
}

## {shiny} modules -------------------------------------------------------


### total score summary table module -------------------------------------

total_scores_summary_tab_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(column(
      12,
      tableOutput(ns("tab"))
    ))
  )
}

total_scores_summary_tab_server <- function(id, dif_matching, group) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      d <- reactive({
        d <- tibble(
          score = dif_matching,
          group = factor(group, labels = c("Basic school track", "Selective academic track"))
        )

        d |>
          group_by(group) |>
          summarise(
            n = n(),
            min = min(.data$score, na.rm = TRUE),
            max = max(.data$score, na.rm = TRUE),
            mean = mean(.data$score, na.rm = TRUE),
            median = median(.data$score, na.rm = TRUE),
            SD = sd(.data$score, na.rm = TRUE),
            skeewness = ShinyItemAnalysis:::skewness(.data$score),
            kurtosis = ShinyItemAnalysis:::kurtosis(.data$score)
          )
      })

      output$tab <- renderTable(d(), digits = 2, colnames = TRUE)
    }
  )
}


### total scores histogram module ----------------------------------------

total_scores_hist_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(column(
      12,
      plotlyOutput(ns("plot"))
    ))
  )
}

total_scores_hist_server <- function(id, dif_matching, group, xlab = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      plt <- reactive({
        d <- tibble(
          score = dif_matching,
          group = factor(group, labels = c("Basic school track", "Selective academic track"))
        )

        d |>
          ggplot(aes(x = .data$score, fill = .data$group, col = .data$group)) +
          geom_histogram(binwidth = 1, position = "dodge2", alpha = 0.75) +
          xlab(xlab) +
          ylab("Number of respondents") +
          scale_fill_manual(
            values = c("dodgerblue2", "goldenrod2"),
            labels = c("Reference", "Focal")
          ) +
          scale_colour_manual(
            values = c("dodgerblue2", "goldenrod2"),
            labels = c("Reference", "Focal")
          ) +
          theme_app()
      })

      output$plot <- renderPlotly({
        g <- plt() |>
          ggplotly() |>
          config(displayModeBar = FALSE)

        g$x$data[[1]]$text <- paste0(
          "Count: ", g$x$data[[1]]$y, "<br />",
          "Score: ", round(g$x$data[[1]]$x), "<br />",
          "Group: ", "Basic school track"
        )
        g$x$data[[2]]$text <- paste0(
          "Count: ", g$x$data[[2]]$y, "<br />",
          "Score: ", round(g$x$data[[2]]$x), "<br />",
          "Group: ", "Selective academic track"
        )
        g
      })
    }
  )
}


### total scores t-test module -----------------------------------------

total_scores_t_test_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(column(
      12,
      tableOutput(ns("tab"))
    ))
  )
}

total_scores_t_test_server <- function(id, dif_matching, group) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      d <- reactive({
        d <- tibble(
          score = dif_matching,
          group = factor(group, labels = c("Basic school track", "Selective academic track"))
        )

        res <- t.test(score ~ group, d)

        data.frame(
          `Diff.` = res$estimate[1] - res$estimate[2],
          LCI = res$conf.int[1],
          UCI = res$conf.int[2],
          `\\(t\\)` = res$statistic,
          df = res$parameter,
          `\\(p\\)` = res$p.value,
          check.names = FALSE
        )
      })

      output$tab <- renderTable(d(), digits = 3, colnames = TRUE, rownames = FALSE)
    }
  )
}
