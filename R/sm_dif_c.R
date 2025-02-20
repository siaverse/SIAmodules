
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

sm_dif_c_server <- function(id, imports = NULL, ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # in case ther are no imports, set a default values as in SIA
    if (is.null(imports)) {
      imports <- list(
        setting_figures = list(
          text_size = 12,
          height = 4,
          width =8,
          dpi = 600
          )
        )
    }

    # static data -------------------------------------------------------------

    # TODO: possible performance benefit loading LtL outside of the module function
    # but that would penalize other module users (thats again why its better not
    # to make module compilations)
    LearningToLearn <- ShinyItemAnalysis::LearningToLearn

    items <- LearningToLearn[60:100]
    group <- LearningToLearn[, "track_01"]
    dif_matching <- reactiveVal(LearningToLearn[, "score_6"])

    item_names <- reactiveVal(names(items))
    total_score <- reactiveVal(rowSums(items))
    z_score <- reactiveVal(scale(rowSums(items)))
    dif_present <- reactiveVal(TRUE)
    binary <- reactiveVal(items)
    group <- reactiveVal(group)


    # summary -----------------------------------------------------------------

    # total scores module (see the very end of this file for definitions)
    total_scores_summary_tab_server("total_scores_summary_tab_ts", total_score, group)
    total_scores_summary_tab_server("total_scores_summary_tab_dm", dif_matching, group)

    total_scores_t_test_server("total_scores_t_test_ts", total_score, group)
    total_scores_t_test_server("total_scores_t_test_dm", dif_matching, group)

    total_scores_hist_server("total_scores_hist_ts", total_score, group, "Total score")
    total_scores_hist_server("total_scores_hist_dm", dif_matching, group, "Observed score")


    # DIF-C -------------------------------------------------------------------

    # ** Warning, if total_score() or z_score() have NAs
    na_score <- reactive({
      if (any(is.na(total_score())) | any(is.na(z_score()))) {
        txt <- "<font color = 'orange'>
				For this analysis, observations with missing values have been omitted.
				</font>"
      } else {
        txt <- ""
      }
      txt
    })

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
        updateCheckboxGroupInput(
          session = session,
          inputId = ns("DIF_NLR_model_print"),
          selected = DIF_nlr$model
        )
      }
      if (DIF_nlr$model != input$DIF_NLR_model_plot) {
        updateCheckboxGroupInput(
          session = session,
          inputId = ns("DIF_NLR_model_plot"),
          selected = DIF_nlr$model
        )
      }
    })

    # ** Updating type ####
    observeEvent(input$DIF_NLR_type_print, {
      DIF_nlr$type <- input$DIF_NLR_type_print
    })
    observeEvent(input$DIF_NLR_type_plot, {
      DIF_nlr$type <- input$DIF_NLR_type_plot
    })
    observeEvent(DIF_nlr$type, {
      if (length(DIF_nlr$type) != length(input$DIF_NLR_type_print)) {
        updateCheckboxGroupInput(
          session = session,
          inputId = ns("DIF_NLR_type_print"),
          selected = DIF_nlr$type
        )
      } else {
        if (any(DIF_nlr$type != input$DIF_NLR_type_print)) {
          updateCheckboxGroupInput(
            session = session,
            inputId = ns("DIF_NLR_type_print"),
            selected = DIF_nlr$type
          )
        }
      }
      if (length(DIF_nlr$type) != length(input$DIF_NLR_type_plot)) {
        updateCheckboxGroupInput(
          session = session,
          inputId = ns("DIF_NLR_type_plot"),
          selected = DIF_nlr$type
        )
      } else {
        if (any(DIF_nlr$type != input$DIF_NLR_type_plot)) {
          updateCheckboxGroupInput(
            session = session,
            inputId = ns("DIF_NLR_type_plot"),
            selected = DIF_nlr$type
          )
        }
      }
    })

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
          inputId = ns("DIF_NLR_correction_method_print"),
          selected = DIF_nlr$correction
        )
      }
      if (DIF_nlr$correction != input$DIF_NLR_correction_method_plot) {
        updateSelectInput(
          session = session,
          inputId = ns("DIF_NLR_correction_method_plot"),
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
          inputId = ns("DIF_NLR_purification_print"),
          value = DIF_nlr$purification
        )
      }
      if (DIF_nlr$purification != input$DIF_NLR_purification_plot) {
        updateCheckboxInput(
          session = session,
          inputId = ns("DIF_NLR_purification_plot"),
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
          inputId = ns("DIF_NLR_summary_matching"),
          value = DIF_nlr$matching
        )
      }
      if (DIF_nlr$matching != input$DIF_NLR_items_matching) {
        updateCheckboxInput(
          session = session,
          inputId = ns("DIF_NLR_items_matching"),
          value = DIF_nlr$matching
        )
      }
    })

    # update selectInput & disable purification if DMV present
    observe({
      if (dif_present() == TRUE) {
        lapply(match_NLR, function(i) {
          updateSelectInput(
            session,
            paste0(i),
            choices = c(
              "Grade 6" = "zuploaded",
              "Grade 9" = "zscore"
            ),
            selected = "zuploaded"
          )
        })
      } else {
        lapply(match_NLR, function(i) {
          updateSelectInput(
            session,
            paste0(i),
            choices = c(
              "Standardized total score" = "zscore"
            ),
            selected = "zscore"
          )
        })
      }
    })

    mapply(
      function(match, puri) {
        observeEvent(input[[paste0(match)]], {
          if (input[[paste0(match)]] %in% c("uploaded", "zuploaded")) {
            updateCheckboxInput(session, paste0(puri), value = FALSE)
            shinyjs::disable(paste0(puri))
          } else {
            shinyjs::enable(paste0(puri))
          }
        })
      },
      match = match_NLR, puri = puri_NLR
    )

    # ** Updating item slider ####
    observe({
      item_count <- ncol(binary())
      updateSliderInput(
        session = session,
        inputId = ns("DIF_NLR_item_plot"),
        max = item_count
      )
    })

    # ** MODEL ####
    model_DIF_NLR <- reactive({
      data <- data.frame(binary())
      group <- unlist(group())

      model <- input$DIF_NLR_model_print
      type <- paste0(input$DIF_NLR_type_print, collapse = "")
      adj.method <- input$DIF_NLR_correction_method_print
      purify <- input$DIF_NLR_purification_print

      if (input$DIF_NLR_summary_matching == "zscore") {
        match <- z_score()
      } else if (input$DIF_NLR_summary_matching == "zuploaded") {
        match <- scale(apply(as.data.frame(unlist(dif_matching())), 1, sum))
      }

      fit <- tryCatch(
        difNLR(
          Data = data, group = group, focal.name = 1, match = match,
          model = model, type = type,
          p.adjust.method = adj.method, purify = purify,
          test = "LR"
        ),
        error = function(e) e
      )

      validate(
        need(
          class(fit) == "difNLR",
          paste0("This method cannot be used on this data. Error returned: ", fit$message)
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
        inputId = ns("DIF_NLR_type_print"),
        selected = enaSelection
      )

      # create object that identifies enabled and disabled options
      disElement <- paste0("#DIF_NLR_type_print :nth-child(", disNum, ") label")
      enaElement <- paste0("#DIF_NLR_type_print :nth-child(", enaNum, ") label")

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
        inputId = ns("DIF_NLR_type_plot"),
        selected = enaSelection
      )

      # create object that identifies enabled and disabled options
      disElement <- paste0("#DIF_NLR_type_plot :nth-child(", disNum, ") label")
      enaElement <- paste0("#DIF_NLR_type_plot :nth-child(", enaNum, ") label")

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

      coeffs <- coeffs_se_names()$coeffs
      se <- coeffs_se_names()$se

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

      rownames(tab) <- item_names()

      tab
    })

    output$coef_nlr_dif <- renderDT(
      {
        coef_nlr_dif() %>% mutate(across(where(is.numeric), ~ round(.x, 3)))
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
      res <- NULL

      # res$coeffs <- do.call(rbind, lapply(model$nlrPAR, function(x) {na.omit(x[c("a", "b", "c", "d", "aDif", "bDif", "cDif", "dDif")])}))
      # res$se <- do.call(rbind, lapply(model$nlrSE, function(x) {na.omit(x[c("a", "b", "c", "d", "aDif", "bDif", "cDif", "dDif")])}))
      # colnames(se) <- paste0("SE(", colnames(se), ")")

      res$se <- do.call(rbind, model$nlrSE)
      res$coeffs <- do.call(rbind, model$nlrPAR)
      res
    })

    # ** Items detected text ####
    output$nlr_dif_items <- renderPrint({
      DIFitems <- model_DIF_NLR()$DIFitems
      if (DIFitems[1] == "No DIF item detected") {
        txt <- "No item was detected as DIF."
      } else {
        txt <- paste0("Items detected as DIF items: ", paste(item_names()[DIFitems], collapse = ", "))
      }
      HTML(txt)
    })

    # ** Purification table ####
    dif_nlr_puri_table <- reactive({
      tab <- model_DIF_NLR()$difPur

      if (!is.null(tab)) {
        colnames(tab) <- item_names()
        rownames(tab) <- paste0("Step ", seq(0, nrow(tab) - 1))
        tab
      }
    })
    output$dif_nlr_puri_table <- renderDT(
      {
        dif_nlr_puri_table()
      },
      rownames = T,
      colnames = T
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
      thr <- if (length(unique(model$df)) == 1) {
        unique(qchisq(1 - model$alpha, model$df))
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

        rownames(data) <- item_names()

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

    # ** Warning for missing values ####
    output$DIF_NLR_na_alert <- renderUI({
      txt <- na_score()
      HTML(txt)
    })

    # ** ITEMS ####

    # ** Plot ####
    plot_DIF_NLRInput <- reactive({
      fit <- model_DIF_NLR()
      item <- input$DIF_NLR_item_plot

      g <- plot(fit, item = item)[[1]] +
        theme_app() +
        theme(
          legend.box.just = "top",
          legend.position = c(0.01, 0.98),
          legend.justification = c(0, 1),
          legend.key.width = unit(1, "cm"),
          legend.box = "horizontal"
        ) +
        ggtitle(item_names()[item])
      g
    })

    # ** Output plot ####
    output$plot_DIF_NLR <- renderPlotly({
      g <- plot_DIF_NLRInput()
      p <- ggplotly(g)

      p$x$data[[1]]$text <- paste0(
        "Group: Reference", "<br />",
        "Match: ", p$x$data[[1]]$x, "<br />",
        "Probability: ", p$x$data[[1]]$y
      )
      p$x$data[[2]]$text <- paste0(
        "Group: Focal", "<br />",
        "Match: ", p$x$data[[2]]$x, "<br />",
        "Probability: ", p$x$data[[2]]$y
      )

      p$x$data[[3]]$text <- gsub("size", "Group: Reference<br />Count", p$x$data[[3]]$text)
      p$x$data[[3]]$text <- gsub("match", "Z-score", p$x$data[[3]]$text)
      p$x$data[[3]]$text <- gsub("prob", "Empirical probability", p$x$data[[3]]$text)
      p$x$data[[4]]$text <- gsub("size", "Group: Focal<br />Count", p$x$data[[4]]$text)
      p$x$data[[4]]$text <- gsub("match", "Z-score", p$x$data[[4]]$text)
      p$x$data[[4]]$text <- gsub("prob", "Empirical probability", p$x$data[[4]]$text)

      p$elementId <- NULL
      hide_legend(p) %>% plotly::config(displayModeBar = FALSE)
    })

    # ** DB for plot ####
    output$DP_plot_DIF_NLR <- downloadHandler(
      filename = function() {
        paste0("fig_DIFNonlinear_", item_names()[input$DIF_NLR_item_plot], ".png")
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

        tab_coef <- fit$nlrPAR[[item]]
        tab_sd <- fit$nlrSE[[item]]

        tab <- t(rbind(tab_coef, tab_sd))

        rownames(tab) <- paste0("\\(\\mathit{", gsub("Dif", "_{Dif}", rownames(tab)), "}\\)")
        colnames(tab) <- c("Estimate", "SE")

        tab
      },
      include.rownames = T
    )

    # ** Warning for missing values ####
    output$DIF_NLR_item_na_alert <- renderUI({
      txt <- na_score()
      HTML(txt)
    })
  }
  )
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
      "This ", code("ShinyItemAnalysis"), " module provides interactive display of
        Differential Item Functioning in Change (DIF-C) with binary
        Learning to Learn data, as more closesly described in ",
      a(
        "Martinkov\u00e1, Hladk\u00e1, and Potu\u017en\u00edkov\u00e1 (2020)",
        href = "https://doi.org/10.1016/j.learninstruc.2019.101286",
        target = "_blank",
        .noWS = "after"
      ),
      ". In their paper, Martinkov\u00e1 et al. demonstrate that this more detailed
        item-level analysis is able to detect between-group differences in
        pre-post gains even in case when no difference is observable in gains in
        total scores. DIF analysis is implemented with generalized logistic
        regression models in the ", code("difNLR"), " package ",
      a(
        "(Hladk\u00e1 & Martinkov\u00e1, 2020)",
        href = "https://doi.org/10.32614/RJ-2020-014",
        target = "_blank",
        .noWS = "after"
      ),
      ". This module is a part of the ", code("ShinyItemAnalysis"), " package ",
      a(
        "(Martinkov\u00e1 & Drabinov\u00e1, 2018).",
        href = "https://doi.org/10.32614/RJ-2018-074",
        target = "_blank",
        .noWS = "after"
      ),
    ),


    # tabs --------------------------------------------------------------------

    tabsetPanel(
      tabPanel(
        "Total scores",
        h4("Summary of total scores"),
        p(
          "DIF analysis may come to a different conclusion than a test of group differences in total scores.
                        Two groups may have the same distribution of total scores, yet, some items may function differently
                        for the two groups. Also, one of the groups may have a significantly lower total score, yet, it may
                        happen that there is no DIF item ",
          a("(Martinkov\u00e1 et al., 2017)",
            href = "https://doi.org/10.1187/cbe.16-10-0307",
            target = "_blank", .noWS = "after"
          ),
          ". This section examines the differences in observed scores only. Explore further DIF sections to analyze
                        differential item functioning."
        ),

        # total scores module (see shiny modules definition in the end of the file)
        h5("Grade 6"),

        h6("Summary of total scores for groups"),
        total_scores_summary_tab_ui(ns("total_scores_summary_tab_dm")),

        h6("Comparison of total scores"),
        total_scores_t_test_ui(ns("total_scores_t_test_dm")),

        h6("Histograms of total scores for groups"),
        total_scores_hist_ui(ns("total_scores_hist_dm")),


        h5("Grade 9"),

        h6("Summary of total scores for groups"),
        total_scores_summary_tab_ui(ns("total_scores_summary_tab_ts")),

        h6("Comparison of total scores"),
        total_scores_t_test_ui(ns("total_scores_t_test_ts")),

        h6("Histograms of total scores for groups"),
        total_scores_hist_ui(ns("total_scores_hist_ts"))
      ),
      tabPanel(
        "DIF-C Summary",
        br(),
        p(
          "In DIF analysis, the groups are compared in functioning of items with respect to respondent ability.
                        In many methods, observed ability such as the standardized total score is used as the matching criterion.
                        DIF can also be explored with respect to other observed score or criterion.
                        For example, to analyze instructional sensitivity, ",
          a("Martinkov\u00e1 et al. (2020)",
            href = "https://doi.org/10.1016/j.learninstruc.2019.101286",
            target = "_blank"
          ),
          " analyzed differential item functioning in change (DIF-C) by analyzing DIF on Grade 9 item answers
                          while matching on Grade 6 total scores of the same respondents in a longitudinal setting
                          (see toy data ", code("Learning to Learn 9"), " in the Data section)."
        ),
        h4("Method specification"),
        p(
          "Here you can specify the assumed",
          strong("model", .noWS = "after"),
          ". In 3PL and 4PL models, the abbreviations \\(c_{g}\\) or
          \\(d_{g}\\) mean that parameters \\(c_i\\) or \\(d_i\\) are assumed
          to be the same for both groups, otherwise they are allowed to
          differ. With", strong("type"), "you can specify the type of DIF to
          be tested by choosing the parameters in which a difference between
          groups should be tested. You can also select", strong("correction
          method"), "for multiple comparison or",
          strong("item purification", .noWS = "after"), "."
        ),
        p(
          "Finally, you may change the",
          strong("Observed score", .noWS = "after"),
          ". While matching on the standardized total score is typical, the
          upload of other Observed scores is possible in the ",
          strong("Data"), "section. Using a pre-test (standardized) total
          score allows for testing differential item functioning in change
          (DIF-C) to provide proofs of instructional sensitivity",
          a(
            "(Martinkov\u00e1 et al., 2020)",
            href = "https://doi.org/10.1016/j.learninstruc.2019.101286",
            target = "_blank",
            .noWS = "after"
          ),
          ", also see", code("Learning To Learn 9"), " toy dataset."
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
              selected = "3PLcg"
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
              selected = c("\\(a\\)", "b")
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
              choices = c("Standardized total score" = "zscore"),
              selected = "zscore"
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
          above consists of a parameter for the reference group and a
          parameter for the difference between focal and reference groups,
          i.e., \\(a_{iG_p} = a_{i} + a_{iDif}G_{p}\\), where \\(G_{p} = 0\\)
          for the reference group and \\(G_{p} = 1\\) for the focal group, as
          stated in the table below. "
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
        h4("Method specification"),
        p(
          "Here you can specify the assumed", strong("model", .noWS = "after"),
          ". In 3PL and 4PL models, the abbreviations \\(c_{g}\\) or
          \\(d_{g}\\) mean that parameters \\(c\\) or \\(d\\) are assumed to
          be the same for both groups, otherwise they are allowed to differ.
          With", strong("type"), "you can specify the type of DIF to be tested
          by choosing the parameters in which a difference between groups
          should be tested. You can also select", strong("correction method"),
          "for multiple comparison or",
          strong("item purification", .noWS = "after"), "."
        ),
        p(
          "Finally, you may change the",
          strong("Observed score", .noWS = "after"),
          ". While matching on the standardized total score is typical, the
          upload of other observed scores is possible in the", strong("Data"),
          "section. Using a pre-test (standardized) total score allows for
          testing differential item functioning in change (DIF-C) to provide
          proofs of instructional sensitivity",
          a(
            "(Martinkov\u00e1 et al., 2020)",
            href = "https://doi.org/10.1016/j.learninstruc.2019.101286",
            target = "_blank",
            .noWS = "after"
          ),
          ", also see",
          code("Learning To Learn 9"),
          "toy dataset. For selected", strong("item"), "you can display plot
          of its characteristic curves and table of its estimated parameters
          with standard errors."
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
              selected = "3PLcg"
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
              choices = c("Standardized total score" = "zscore"),
              selected = "zscore"
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
          "Points represent a proportion of the correct answer (empirical
          probabilities) with respect to the observed score. Their size is
          determined by the count of respondents who achieved a given level of
          observed score with respect to the group membership."
        ),
        plotlyOutput(ns("plot_DIF_NLR")),
        downloadButton(ns("DP_plot_DIF_NLR"), "Download figure"),
        h4("Equation"),
        fluidRow(
          column(12, uiOutput(ns("DIF_NLR_equation_plot")), align = "center")
        ),
        h4("Table of parameters"),
        p(
          "This table summarizes estimated item parameters together with their
          standard errors. Note that \\(a_{iG_p}\\) (and also other
          parameters) from the equation above consists of a parameter for the
          reference group and a parameter for the difference between focal and
          reference groups, i.e., \\(a_{iG_p} = a_{i} + a_{iDif}G_{p}\\),
          where \\(G_{p} = 0\\) for the reference group and \\(G_{p} = 1\\)
          for the focal group, as stated in the table below. "
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

              <li>Martinkov\u00e1, P., & Drabinov\u00e1, A. (2018).
              ShinyItemAnalysis for teaching psychometrics and to enforce routine analysis of educational tests.
              <i>The R Journal, 10</i>(2), 503-515.
              <a href = "https://doi.org/10.32614/RJ-2018-074", target = "_blank">doi:10.32614/RJ-2018-074</a>
              </li>

              <li>Martinkov\u00e1, P., Drabinov\u00e1, A., Liaw, Y. L., Sanders, E. A., McFarland, J. L., & Price, R. M. (2017).
              Checking equity: Why differential item functioning analysis should be a routine part
              of developing conceptual Assessments.
              <i>CBE-Life Sciences Education, 16</i>(2), rm2.
              <a href = "https://doi.org/10.1187/cbe.16-10-0307",
              target = "_blank">doi:10.1187/cbe.16-10-0307</a>
              </li>

              <li>Kolek, L., \u0160isler, V., Martinkov\u00e1, P., & Brom, C. (2021).
              Can video games change attitudes towards history? Results from a laboratory experiment measuring short- and long-term effects.
              <i>Journal of Computer Assisted Learning, 37</i>(5), 1348-1369.
              <a href = "https://doi.org/10.1111/jcal.12575", target = "_blank">doi:10.1111/jcal.12575</a>
              </li>

            </ul>'),

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
          score = dif_matching(),
          group = group() %>% factor(labels = c("reference (0)", "focal (1)"))
        )

        d %>%
          group_by(group) %>%
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
          score = dif_matching(),
          group = group() %>% factor(labels = c("reference (0)", "focal (1)"))
        )

        d %>%
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
        plt() %>%
          ggplotly() %>%
          config(displayModeBar = FALSE)
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
          score = dif_matching(),
          group = group() %>% factor(labels = c("reference (0)", "focal (1)"))
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
