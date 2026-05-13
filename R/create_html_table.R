#' create the html to display a data.frame
#'
#' The function `create_html_table` creates the html to display the rows of a data.frame in the table format. \cr
#' The row can be displayed as one block or as blocks grouped by a common field that is indicated by the `group1` parameter.\cr
#' The header can be missing or contain one or more rows with the default being the names of the data.frame. \cr
#' A caption can be specified and for all html elements a class and style can be specified. \cr E.g for the caption one can indicate `c_style="white-space: nowrap;caption-side: bottom;"`.\cr
#' The specifications for html classes can be given in a character vector to be included with the `i_style` argument as a `<style>` tag.\cr
#' \cr
#' The function `create_style_table` creates style information that can also be included by using the `i_style` argument of the `create_html_table` function.
#' This is an alternative for specifying the class and style for the `th` and `td` elements.
#' The function uses a style template that is filled in with the arguments of the function. See `template_create_style_table` in the examples.
#' If the template does not satisfy your needs, you can specify your own template such as is done in the example `fit_table_def`.
#'
#' @name create_html_table
#' @param df1 data.frame for which an html table representation will be made
#' @param vars1 Character vector with the names of the fields to include in the detail lines in the table
#' @param group1 Character string with the name of the field that will be used as group header. When NULL no groups are created
#' @param header1 Character vector or list of character vectors to use as th table headers. Each vector generates one header row for each of the groups
#' @param caption1 Character string to use as caption for the table
#' @param gr_tr Function that transforms group1 into the group heading
#' @param t_class Character string with the class name to assign to the table tag
#' @param t_style Character string with the style information to assign to the table tag
#' @param c_class Character string with the class name to assign to the caption tag
#' @param c_style Character string with the style information to assign to the caption tag
#' @param g_class Character string with the class name to assign to the td tags of the group lines
#' @param g_style Character string with the style information to assign to td tags of the group lines
#' @param hr_class Character string with the class name to assign to the tr tags of the header lines
#' @param hr_style Character string with the style information to assign to tr tags of the header lines
#' @param hh_class Character string with the class name to assign to the th tags of the header lines
#' @param hh_style Character string with the style information to assign to th tags of the header lines
#' @param d_class Character string with the class name to assign to the td tags of the detail lines
#' @param d_style Character string with the style information to assign to td tags of the detail lines
#' @param i_style Character string with info that is included with the `<style>` tag before the table statement
#' @return For `create_html_table` a list() with a shiny.tag class that can be converted into an HTML string via `as.character()`, viewed with `htmltools::browsable()` and saved to a file with `htmltools::save_html()` .
#' @export
#' @examples
#'
#' # the style used in create_style_table is given below. The arguments of the function are glue-wise
#' # inserted between (^,$) pairs with exception of classname, width and white_space that get some preprocessing:
#' #  class1 <- ifelse(is.null(classname),"",paste0(".",classname))
#' #  width1 <- ifelse((is.null(width)||(nchar(width)==0)),"",paste0("width: ",width,";"))
#' #  ws1    <- ifelse((is.null(white_space)||(nchar(white_space)==0)),"",paste0("white-space: ",white_space,";"))
#' template_create_style_table <- "
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
#'   "
#'
#'  fit_table_def <- "
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
#'   "
#'
#' df1 <- data.frame(f1=c(1,2),f2=c("Antwerpen","Bern"))
#' tb1 <- create_html_table(df1)
#' tb2 <- create_html_table(df1,t_class="fit-table is-center",i_style=fit_table_def,
#'     caption="this is my caption", c_style="white-space: nowrap;caption-side: bottom;")
#' cat( as.character(tb2) )
#' #htmltools::browsable(tb2)
#' #htmltools::save_html(tb2,'test.html')
#' df2 <- data.frame(f1=c(1,2,3),
#'                   f2=c("Antwerpen","Bern","Luik"),
#'                   f3=c("België","Zwitserland","België"))
#' tb3 <- create_html_table(df2,vars1=c("f1","f2"),group1="f3")
#' tb4 <- create_html_table(df2,vars1=c("f1","f2"),group1="f3",
#'   gr_style= "text-align:center;",
#'   gd_style= "padding:8px;font-weight:800;color:red;",
#'   dd_style= "border: 1px solid red;padding:16px;",
#'   dr_style= "text-align:center;font-weight:800;",
#'   t_style = "border: 1px solid black;border-collapse:nocollapse;")
#'
#'
create_html_table <- function(df1,vars1=names(df1),group1=NULL,header1=vars1, caption1=NULL, gr_tr=NULL,
                t_class= NULL, t_style=NULL, # table class and style
                c_class= NULL, c_style=NULL, # caption class and style
                gr_class= NULL, gr_style=NULL, # group tr class and style
                gd_class= NULL, gd_style=NULL, # group td class and style
                hr_class= NULL, hr_style=NULL, # header tr class and style
                hh_class= NULL, hh_style=NULL, # header th class and style
                dr_class= NULL, dr_style=NULL, # detail tr class and style
                dd_class= NULL, dd_style=NULL, # detail td class and style
                i_style=NULL        # information to include as <style> element
) {
  # select key (to gather headings) and vars (to display in underlying detail lines)
  # if (!is.null(header1)) header1 <- tags$tr(purrr::map(header1, tags$th))
  header1 <-  create_header(header1,
                             trclass=hr_class, trstyle=hr_style,
                             thclass=hh_class, thstyle=hh_style)
  df2 <- df1 |>
    dplyr::select(all_of(c(group1, vars1)))
  if ( (!is.null(gr_tr)) && (!is.null(group1)) ) {
    # if requested build the header lines
    df2 <- df2 |>
      dplyr::mutate (!!group1 := purrr::map(!!rlang::sym(group1), gr_tr))
  }
  df3 <- df2 |>
    dplyr::rowwise() |>
    # make data for detail line character tr tag with underlying td tags
    dplyr::mutate(dplyr::across(where(is.numeric), as.character)) |>
    dplyr::mutate(line1 = list(c(dplyr::c_across(!(!!group1)),use.names=F)),
                  tags1 = list (htmltools::tags$tr(purrr::map(line1,~htmltools::tags$td(htmltools::HTML(.),class=dd_class,style=dd_style)),class=dr_class,style=dr_style )) ) |>
    dplyr::ungroup()
  if (!is.null(group1)) {
    et4 <- df3 |>
      dplyr::nest_by(!!rlang::sym(group1)) |>
      # make data for header lines and intertwine headers with detail lines
      dplyr::mutate(
        hdr1 = htmltools::tagList(htmltools::tags$tr(htmltools::tags$td(!!rlang::sym(group1),`colspan`=length(vars1),class=gd_class,style=gd_style),class=gr_class,style=gr_style) ),
        tags2 = list(do.call(htmltools::tagList,data$tags1)),
        tags3 = list(c_tls(hdr1,c_tls(header1,tags2)) ) ) |>
      dplyr::ungroup() |>
      dplyr::pull(tags3)
  }
  else {
    et4 <- htmltools::tagList(header1,dplyr::pull(df3,"tags1"))
  }
  et5 <- et4 |>
    # retrieve the intertwined tr tags
    unlist(recursive=F) |>
    # create one list with all tr tags
    do.call(htmltools::tagList,args=_)
  # create table tag with attributes
  et6 <- htmltools::tags$table(
    if (!is.null(caption1)) htmltools::tags$caption(caption1,class=c_class,style=c_style),
    et5)
  et6 <- htmltools::tagAppendAttributes(et6,class=t_class,style=t_style)
  # if requested include (style) information
  if (!is.null(i_style)) {
    c_tls(htmltools::tags$style(htmltools::HTML(i_style)) , et6)
  } else {
    et6
  }
}

c_tls <- function( tl1,tl2) {
  # combine taglists: NOT tagList(tl1,tl2)
  if ("shiny.tag" %in% class(tl1)) tl1 <- list(tl1)
  if ("shiny.tag" %in% class(tl2)) tl2 <- list(tl2)
  do.call(htmltools::tagList, c(tl1,tl2))
}

create_header <- function(header_data,
              trclass=NULL, trstyle=NULL,thclass=NULL, thstyle=NULL) {
  m <- TTV:::cpm(header_data)
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

      # Maak de TH tag
      row_cells[[length(row_cells) + 1]] <- htmltools::tags$th(
        rowspan = if(curr_rowspan > 1) curr_rowspan else NULL,
        colspan = if(curr_colspan > 1) curr_colspan else NULL,
        p(m,i, j), class=thclass, style=thstyle
      )
    }
    html_cells[[i]] <- htmltools::tags$tr(row_cells, class=trclass, style=trstyle)
  }
  return(html_cells)
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
#' @return For `create_style_table` a character string with the generated content for the `<style>` block (excluding the tags themselves)
#' @export
#' @rdname create_html_table
#' @examples
#'
#' st5 <- create_style_table(classname="mc",tabborder="2px solid black", background_color = "#00ffFF",
#'    color="red",padding_top = "12px",padding_bottom = "2px")
#' tb5 <- create_html_table(df1,t_class="mc",i_style=st5)
#' cat( as.character(tb5) )
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
"

  glue::glue(my_template,.open="^",.close="$")
}
