#' create the html to display a data.frame
#'
#' The function `create_html_table2` creates the html to display the rows of a data.frame in HTML. \cr
#' The header can be missing or contain one or more rows with the default being the names of the data.frame. \cr
#' A caption can be specified and CSS information can be given in a character vector to be included with the `style_info` argument as a `<style>` tag.\cr

#' @name create_html_table2
#' @param df1 data.frame for which an html table representation will be made
#' @param headers List of character vectors to use as th table headers.
#' Each vector generates one header row
#' @param colspan List of integer vectors of same length as `headers`
#' Each vector indicates for the corresponding header the column where a `colspan="2"` will be inserted in the `th<>` definition
#' @param caption Character string to use as caption for the table
#' @param class Character string with the class name to assign to the table tag
#' @param tdHTML Boolean indicating that (non-header) fields should be handled as HTML
#' @param style_info Character string with info that is included with the `<style>` tag before the table statement.
#' Lines starting with the R comment symbol # will be removed
#' @param html_file Character string which, if specified, indicates the filename in which the generated HTML is saved
#' @param format_thf Function with arguments `headers` and `colspan` that formats the th elements in a tr element
#' @param format_thd Function with arguments cell contents, `tdHTML` and optional column index that formats one td element
#' @return A list() with a shiny.tag class that can be converted into an HTML string via `as.character()`, viewed with `htmltools::browsable()` and saved to a file with `htmltools::save_html()` .
#' @details
#' ## headers
#'  - the default headers are the column names of the data.frame: `names(df1)`
#'  - one or more header rows can be specified by the entries in a list: `list(c(...),c(...))`
#'  - no headers will be produced by specifying an empty list: `list()`
#'
#' @export
#' @examples
#' create_html_table2(df1,html_file="h.html")
#'
#'
create_html_table2 <-
  function(df1,
           headers = NULL,
           colspan = NULL,
           caption = NULL,
           tdHTML = F,
           style_info = NULL,
           class = NULL,
           html_file = NULL,
           format_thf = NULL,
           format_tdf = NULL
           ) {

    format_th <- function(headers, colspan) {
      purrr::map2(headers, colspan, \(h1, c1) {
        purrr::imap(unname(h1), \(h2, i2) {
          if (i2 %in% c1) {
            htmltools::tags$th(htmltools::HTML(h2), colspan = "2")
          } else if (i2 %in% (c1 + 1)) {
            NULL
          } else
            htmltools::tags$th(htmltools::HTML(h2))
        })
      })
    }

    format_td <- function(x, td_keep, col_index) {
      if (td_keep) {
        x1 <- htmltools::HTML(x)
      } else {
        x1 <- x
      }
      htmltools::tags$td(x1)
    }

    format_style <- function(st) {
      # remove lines starting with the R comment symbol
      st <- stringr::str_squish(stringr::str_split(st,"\n")[[1]])
      paste0(st[stringr::str_detect(st,"^#",negate = T)],collapse="\n")

    }

    if (is.null(format_thf)) format_thf <- format_th
    if (is.null(format_tdf)) format_tdf <- format_td

    if (is.null(headers))
      headers = list(names(df1))
    if (is.null(colspan))
      colspan = rep(list(c()), length(headers))

    html_table <- htmltools::tags$div(
      if (!is.null(style_info))
        htmltools::tags$style(format_style(style_info)),
      htmltools::tags$table(
        class = class,
        if (!is.null(caption))
          htmltools::tags$caption(caption),
        htmltools::tags$thead(purrr::map(
          format_thf(headers, colspan), htmltools::tags$tr
        )),
        htmltools::tags$tbody(purrr::pmap(as.list(df1), function(...) {
          htmltools::tags$tr(purrr::imap(unname(list(...)), function(cel_waarde, index) {
            format_tdf(cel_waarde, tdHTML, col_index = index)
          }))
        }))

      )
    )
    if (!is.null(html_file)) {
      htmltools::save_html(html_table, html_file)
    }
    html_table
  }


