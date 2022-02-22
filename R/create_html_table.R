#' create the html to display a data.frame
#'
#' @name create_html_table
#' @param df1 data.frame for which an html table representation will be mad
#' @param header list of character vectors to use as th table headers. Each vector generate one header row.
#' @param class Character string with the class name to assign to the table statement
#' @return A list() with a shiny.tag class that can be converted into an HTML string via as.character() and saved to a file with save_html().
#' @export
#' @examples
#' \dontrun{
#' df1 <- data.frame(f1=c(1,2),f2=c("A","B"))
#' st <- create_html_table(df1)
#' }
#'


create_html_table <- function(df1, header = list(names(df1)),class=NULL) {

  `%>%` <- magrittr::`%>%`

  td <- function(x) {
  htmltools::tags$td(htmltools::HTML(x), .noWS = "outside")
  }

  th <- function(x) {
    htmltools::tags$th(htmltools::HTML(x), .noWS = "outside")
  }

  tr <- function(x, type = "td") {
    if (type == "th") {
      htmltools::tags$tr(purrr::map(x, th))
    } else {
      htmltools::tags$tr(purrr::map(x, td))
    }
  }

  d <- df1 %>%          # table rows
    dplyr::mutate(dplyr::across(where(is.numeric), as.character)) %>%
    dplyr::rowwise() %>%
    dplyr::transmute(line = list(c(dplyr::c_across()))) %>%
    dplyr::pull(line)          # convert to list of rows

  if (any(0 < purrr::map_dbl(header, length))) {
   htmltools::div(
      htmltools::tags$table(class = class, border = 0, cellspacing = 0,
        cellpadding = 0, style = 'border-collapse:collapse;border:none;',
        purrr::map(header,  ~ tr(., type = "th")),
        purrr::map(d, tr)
      )
   )
  } else {
   htmltools::div(
      htmltools::tags$table(class = class, border = 0, cellspacing = 0,
        cellpadding = 0, style = 'border-collapse:collapse;border:none;',
        purrr::map(d, tr)
      )
   )
  }
}
