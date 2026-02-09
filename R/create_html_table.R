#' create the html to display a data.frame
#'
#' The function `create_html_table` creates the html to display the rows of a data.frame in the table format. \cr
#' The header can be missing or contain one or more rows with the default being the names of the data.frame. \cr
#' A classname for the table and a caption with its class and style can be specified. \cr
#' Also for the `td` and `th` elements a class and style can be specified. However see the `create_style_table` function.\cr
#' It is possible to include a character string containing html information via the `html_include` argument.
#' This information can contain a `style` definition.\cr
#' \cr
#' The function `create_style_table` creates style information that can also be included by using the `html_include` argument of the `create_html_table` function.
#' This is an alternative for specifying the class and style for the `th` and `td` elements.
#' The function uses a style template that is filled in with the arguments of function. See `template_create_style_table` in the examples.
#' If the template does not satisfy your needs, you can specify your own template such as is done in the example `fit_table_def`.
#'
#' @name create_html_table
#' @param df1 data.frame for which an html table representation will be made
#' @param header Character vector or list of character vectors or matrix of character string to use as th table headers. Each vector or matrix row generates one header row.
#' @param tableclass Character string with the class name to assign to the table tag
#' @param caption Character string with the table caption
#' @param captionclass Character string with the class for the table caption
#' @param captionstyle Character string with the style for the table caption
#' @param thclass Character string with the class name to assign to the th tags
#' @param thstyle Character string with the style information to assign to the th tags
#' @param tdclass Character string with the class name to assign to the td tags
#' @param tdstyle Character string with the style information to assign to the td tags
#' @param html_include Character string with html code that is included before the table statement
#' @return For `create_html_table` a list() with a shiny.tag class that can be converted into an HTML string via `as.character()`, viewed with `htmltools::browsable()` and saved to a file with `htmltools::save_html()` .
#' @export
#' @examples
#' \dontrun{
#'
#' # the style used in create_style_table is given below. The arguments of the function are glue-wise
#' # inserted between (^,$) pairs with exception of classname, width and white_space that get some preprocessing:
#' #  class1 <- ifelse(is.null(classname),"",paste0(".",classname))
#' #  width1 <- ifelse((is.null(width)||(nchar(width)==0)),"",paste0("width: ",width,";"))
#' #  ws1    <- ifelse((is.null(white_space)||(nchar(white_space)==0)),"",paste0("white-space: ",white_space,";"))
#' template_create_style_table <- "
#' <style>
#'   table^class1$ {
#'     border-collapse: collapse;
#'     border: ^tabborder$;
#'     table-layout: auto;
#'     ^width1$
#'   }
#'
#'   ^class1$ :is(th, td) {
#'   border: ^border$;
#'   padding-top: ^padding_top$;
#'   padding-bottom: ^padding_bottom$;
#'   padding-left: ^padding_left$;
#'   padding-right: ^padding_right$;
#'   text-align: ^text_align$;
#'   background-color : ^background_color$ ;
#'   color :  ^color$ ;
#'   ^ws1$
#' }
#' </style>
#'   "
#'
#'  fit_table_def <- "
#'  <style>
#'    table.fit-table {
#'     border: none;
#'     width: auto;
#'     table-layout: auto;
#'   }
#'
#'    table.fit-table th, table.fit-table td {
#'     border: none;
#'     padding-top: 8px;
#'     padding-bottom: 8px;
#'     text-align: var(--tabel-align, left);
#'     white-space: nowrap;
#'   }
#'
#'    .is-right {
#'      --tabel-align: right;
#'   }
#'
#'    .is-center {
#'      --tabel-align: center;
#'   }
#'  </style>
#'   "
#'
#' df1 <- data.frame(f1=c(1,2),f2=c("Antwerpen","Bern"))
#' tb1 <- create_html_table(df1)
#' tb1 <- create_html_table(df1,tableclass="fit-table is-center",html_include=fit_table_def,
#'     caption="this is my caption", captionstyle="white-space: nowrap;caption-side: bottom;")
#' htmltools::browsable(tb1)
#' cat( as.character(tb1) )
#' htmltools::save_html(tb1,'test.html')
#' }
#'

create_html_table <- function(df1, header = list(names(df1)),
                              tableclass=NULL,
                              thclass=NULL, thstyle=NULL,
                              tdclass=NULL, tdstyle=NULL,
                              caption=NULL, captionclass= NULL, captionstyle=NULL,
                              html_include = NULL)
  {

  td <- function(x,td_class,td_style) {
    htmltools::tags$td(htmltools::HTML(x), class=td_class, style=td_style, .noWS = "outside")
  }

  ch <- create_header(header, thclass, thstyle)

  d <- df1 |>         # table rows
    dplyr::mutate(dplyr::across(where(is.numeric), as.character)) |>
    dplyr::rowwise() |>
    dplyr::transmute(line = list(c(dplyr::c_across(tidyselect::everything())))) |>
    dplyr::pull(line)          # convert to list of rows

  html1 <- htmltools::tags$table(class = tableclass,
        if (!is.null(caption)) htmltools::tags$caption(caption,class=captionclass,style=captionstyle)  ,
        if (!is.null(ch)) ch  ,
        purrr::map(d,      ~ htmltools::tags$tr(purrr::map(., ~td(.,td_class=tdclass,td_style=tdstyle))))
      )

  if (!is.null(html_include)) {
    htmltools::tags$html(htmltools::HTML(html_include) , html1)
  } else {
    html1
  }
}

cpm <- function(m=NULL) {
  # now done via lists to keep possible html attributes
  if (is.null(m)) return( NULL)
  if (is.character(m)) {
    m <-  list(m)
  }
  n <- purrr::map_int(m,length)
  if (!all(n[1] == n  ) ) stop("list elements in `cpm` differ in length")
  nr <- length(n) ; nc <- n[1]
  if (0 %in% c(nr,nc)) return( NULL)
  list(nr,nc,m)
}

create_header <- function(header_data, thclass=NULL, thstyle=NULL) {
  m <- cpm(header_data)
  if (is.null(m)) return( list())
  rows <-  m[[1]]
  cols <-  m[[2]]
  m    <-  m[[3]]
  visited <- matrix(FALSE, nrow = rows, ncol = cols)
  html_cells <- list()
  p    <- purrr::pluck

  isleeg <- function(x) {
    nchar(x) == 0
  }

  for (i in 1:rows) {
    row_cells <- list()
    for (j in 1:cols) {
      if (visited[i, j]) next

      # Bepaal colspan (hoeveel lege cellen rechts?)
      curr_colspan <- 1
      while ((j + curr_colspan) <= cols && (isleeg(p(m,i, j + curr_colspan)))) {
        curr_colspan <- curr_colspan + 1
      }

      # Bepaal rowspan (hoeveel lege cellen onder?)
      curr_rowspan <- 1
      while ((i + curr_rowspan) <= rows && (isleeg(p(m,i + curr_rowspan, j)))) {
        curr_rowspan <- curr_rowspan + 1
      }

      # Markeer de 'geabsorbeerde' cellen in het masker
      visited[i:(i + curr_rowspan - 1), j:(j + curr_colspan - 1)] <- TRUE

      # Maak de TH of TD tag
      row_cells[[length(row_cells) + 1]] <- htmltools::tags$th(
        rowspan = if(curr_rowspan > 1) curr_rowspan else NULL,
        colspan = if(curr_colspan > 1) curr_colspan else NULL,
        p(m,i, j), class=thclass, style=thstyle
      )
    }
    html_cells[[i]] <- htmltools::tags$tr(row_cells)
  }
  return(html_cells)
}

#' create the style info to display a data.frame
#'
#' @name create_style_table
#' @param classname Character string with the class name to use in this style
#' @param tabborder Character string with `border` definition for `table`
#' @param border Character string with `border` definition for `th` and `td`
#' @param width Character string with `width` definition for `table`. E.g. `"auto"` or `"50%"`
#' @param padding_left Character string with `padding-left` definition for `th` and `td`
#' @param padding_top Character string with `padding-top` definition for `th` and `td`
#' @param padding_right Character string with `padding-right` definition for `th` and `td`
#' @param padding_bottom Character string with `padding-bottom` definition for `th` and `td`
#' @param text_align Character string with `text-align` definition for `th` and `td`
#' @param background_color Character string with `background-color` definition for `th` and `td`
#' @param color Character string with `color` definition for `th` and `td`#'
#' @param white_space Character string with `white-space` definition for `th` and `td`
#' @return For `create_style_table` a character string with the generated style block information.
#' @export
#' @rdname create_html_table
#' @examples
#' \dontrun{
#' st <- create_style_table(classname="mc",tabborder="2px solid black", background_color = "#00ffFF",
#'    color="red",padding_top = "12px",padding_bottom = "2px")
#' tb <- create_html_table(df1,tableclass="mc",html_include=st)
#' }
#'

create_style_table <- function (classname=NULL,
       tabborder="2px solid black",
       border="1px solid #ddd",
       width = "auto",
       padding_left = "4px",
       padding_top  = padding_left,
       padding_right  = padding_left,
       padding_bottom = padding_top,
       text_align = "center",
       background_color= "white",
       color = "black",
       white_space =  NULL)
{
  class1 <- ifelse(is.null(classname),"",paste0(".",classname))
  width1 <- ifelse((is.null(width)||(nchar(width)==0)),"",paste0("width: ",width,";"))
  ws1    <- ifelse((is.null(white_space)||(nchar(white_space)==0)),"",paste0("white-space: ",white_space,";"))

  my_template <- "
  <style>
  table^class1$ {
    border-collapse: collapse;
    border: ^tabborder$;
    table-layout: auto;
    ^width1$
  }

  ^class1$ :is(th, td) {
    border: ^border$;
    padding-top: ^padding_top$;
    padding-bottom: ^padding_bottom$;
    padding-left: ^padding_left$;
    padding-right: ^padding_right$;
    text-align: ^text_align$;
    background-color : ^background_color$ ;
    color :  ^color$ ;
    ^ws1$
  }
  </style>
"

  glue::glue(my_template,.open="^",.close="$")
}
