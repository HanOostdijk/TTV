#' create the html to display a data.frame
#'
#' @name create_html_table
#' @param df1 data.frame for which an html table representation will be made
#' @param header list of character vectors to use as th table headers. Each vector generates one header row.
#' @param class Character string with the class name to assign to the table tag
#' @param caption Character string with the caption for the table
#' @param tr_class Character string with the class name to assign to the tr tags
#' @param html_include Character string with html code that is included before the table statement
#' @param tstyle Character string with the style information to assign to the table tag
#' @return A list() with a shiny.tag class that can be converted into an HTML string via as.character() and saved to a file with save_html().
#' @export
#' @examples
#' \dontrun{
#'  fit_table_def <- "
#'   <style>
#'   .fit-table {
#'     width: auto;
#'     table-layout: auto;
#'   }
#'
#'   .fit-table th, td {
#'     padding: 10px;
#'     border: none;
#'     white-space: nowrap;
#'   }
#'
#'   .fit-table  tr {
#'     line-height: 80%;
#'     text-align: left;
#'   }
#'   </style>
#'   "
#'
#' df1 <- data.frame(f1=c(1,2),f2=c("A","B"))
#' st <- create_html_table(df1,tstyle='border-collapse:collapse;border:none;')
#' st <- create_html_table(df1,class="fit-table",html_include=fit_table_def)
#' st <- create_html_table(df1,caption="this is my caption")
#' }
#'


create_html_table <- function(df1, header = list(names(df1)),
                              class=NULL,tr_class=NULL, caption=NULL,
                              html_include = NULL,
                              tstyle=NULL) {

  td <- function(x) {
  htmltools::tags$td(htmltools::HTML(x), .noWS = "outside")
  }

  th <- function(x) {
    htmltools::tags$th(htmltools::HTML(x), .noWS = "outside")
  }

  tr <- function(x, type = "td",tr_classi=tr_class) {
    if (type == "th") {
      htmltools::tags$tr(purrr::map(x, th),class=tr_classi)
    } else {
      htmltools::tags$tr(purrr::map(x, td),class=tr_classi)
    }
  }

  d <- df1 %>%          # table rows
    dplyr::mutate(dplyr::across(where(is.numeric), as.character)) |>
    dplyr::rowwise() |>
    dplyr::transmute(line = list(c(dplyr::c_across(tidyselect::everything())))) |>
    dplyr::pull(line)          # convert to list of rows

  html1 <- htmltools::div(
      htmltools::tags$table(class = class, style = tstyle,
        if (!is.null(caption)) htmltools::tags$caption(caption)  ,
      if (any(0 < purrr::map_dbl(header, length))) purrr::map(header,  ~ tr(., type = "th")),
      purrr::map(d, tr)
      )
   )

  if (!is.null(html_include)) {
    htmltools::tags$html(htmltools::HTML(html_include) , html1)
  } else {
    html1
  }
}
