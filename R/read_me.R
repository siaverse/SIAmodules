#' How to use this package
#'
#' Prints information on how to use the modules included in the package. You may
#' have already seen the same information when loading the package.
#'
#' The printed message also displays hyperlinks to each module's help page,
#' where you can find more detailed information, including the citation entry.
#'
#' Note that if your console does not support hyperlinks, modules titles will be
#' accompanied by a regular `R` code you may paste into your console to arrive
#' at the same help page.
#'
#' @return An object of class `siamod_readme`. Called for side effects.
#' @export
#'
#' @importFrom yaml read_yaml
#' @importFrom purrr map_chr
#' @importFrom cli format_inline
#'
#' @examples
#' read_me()
#'
read_me <- function() {
  # cannot use siatools::get_modules, because it operates on current project directory
  mods_yaml <- system.file("sia/modules.yml", package = "SIAmodules", mustWork = TRUE)

  mods <- yaml::read_yaml(mods_yaml)

  mods_titles <- mods |>
    purrr::map_chr("title") |>
    sort()

  mods_bullets <- paste0(
    "{.help [", mods_titles, "](SIAmodules::", names(mods_titles), ")}"
  )

  # to ANSI
  mods_bullets <- mods_bullets |> purrr::map_chr(cli::format_inline)

  # make each entry a CLI bullet
  names(mods_bullets) <- rep("*", length(mods_bullets))

  list_heading <- paste0(
    "These are the available modules",
    if (cli::ansi_has_hyperlink_support()) {
      " (click for more details)"
    },
    ":"
  )

  msg <- c(
    # cli::rule("SIAmodules"), "\n",
    cli::format_inline(
      c(
        "{.pkg SIAmodules} is a curated collection of extension modules ",
        "for the {.pkg ShinyItemAnalysis} package."
      )
    ),
    ">" = cli::format_inline(
      c(
        "To use them, run {.run ShinyItemAnalysis::run_app()} ",
        "(they offer no functionality on their own)."
      )
    ),
    "\n",
    list_heading,
    mods_bullets
  )

  structure(msg, class = "siamod_readme")
}


#' @export
print.siamod_readme <- function(x, ...) {
  rlang::inform(x, use_cli_format = TRUE)
}
