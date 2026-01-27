#' create the html to display a data.frame
#'
#' This function is totally depended on the package `htmltools`
#'
#' @name create_html_table
#' @param df1 data.frame for which an html table representation will be made
#' @param header list of character vectors to use as th table headers. Each vector generates one header row.
#' @param tableclass Character string with the class name to assign to the table tag
#' @param tablestyle Character string with the style information to assign to the table tag
#' @param caption Character string with the table caption
#' @param captionclass Character string with the class for the table caption
#' @param captionstyle Character string with the style for the table caption
#' @param thclass Character string with the class name to assign to the th tags
#' @param thstyle Character string with the style information to assign to the th tags
#' @param tdclass Character string with the class name to assign to the td tags
#' @param tdstyle Character string with the style information to assign to the td tags
#' @param html_include Character string with html code that is included before the table statement
#' @return A list() with a shiny.tag class that can be converted into an HTML string via as.character() and saved to a file with htmltools::save_html().
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
#' st <- create_html_table(df1,tablestyle='border-collapse:collapse;border:none;')
#' st <- create_html_table(df1,tableclass="fit-table",html_include=fit_table_def)
#' st <- create_html_table(df1,caption="this is my caption",
#'            captionstyle="white-space: nowrap;caption-side: bottom;")
#' cat( as.character(st) )
#' htmltools::save_html(st,'test.html')
#' }
#'


create_html_table <- function(df1, header = list(names(df1)),
                              tableclass=NULL, tablestyle=NULL,
                              thclass=NULL, thstyle=NULL,
                              tdclass=NULL, tdstyle=NULL,
                              caption=NULL, captionclass= NULL, captionstyle=NULL,
                              html_include = NULL)
  {

  td <- function(x,td_class,td_style) {
    htmltools::tags$td(htmltools::HTML(x), class=td_class, style=td_style, .noWS = "outside")
  }

  th <- function(x,th_class,th_style) {
    htmltools::tags$th(htmltools::HTML(x), class=th_class, style=th_style, .noWS = "outside")
  }

  # tr <- function(x, type = "td",tr_classi=tr_class) {
  #   if (type == "th") {
  #     htmltools::tags$tr(purrr::map(x, th),class=tr_classi)
  #   } else {
  #     htmltools::tags$tr(purrr::map(x, td),class=tr_classi)
  #   }
  # }

  d <- df1 %>%          # table rows
    dplyr::mutate(dplyr::across(where(is.numeric), as.character)) |>
    dplyr::rowwise() |>
    dplyr::transmute(line = list(c(dplyr::c_across(tidyselect::everything())))) |>
    dplyr::pull(line)          # convert to list of rows

  html1 <- htmltools::div(
      htmltools::tags$table(class = tableclass, style = tablestyle,
        if (!is.null(caption)) htmltools::tags$caption(caption,class=captionclass,style=captionstyle)  ,
     # if (any(0 < purrr::map_dbl(header, length))) purrr::map(header,  ~ tr(., type = "th")),
      if (any(0 < purrr::map_dbl(header, length)))
        purrr::map(header, ~ htmltools::tags$tr(purrr::map(., ~th(.,th_class=thclass,th_style=thstyle)))),
      purrr::map(d,      ~ htmltools::tags$tr(purrr::map(., ~td(.,td_class=tdclass,td_style=tdstyle))))
      )
   )

  if (!is.null(html_include)) {
    htmltools::tags$html(htmltools::HTML(html_include) , html1)
  } else {
    html1
  }
}
