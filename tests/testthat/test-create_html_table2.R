
# voor testen buiten de test interface
setup_test <- function(){
  library(TTV)
  library(testthat)
  library(dplyr)
  library(htmltools)
  hb <- htmltools::browsable
  assign('hb',hb,envir = .GlobalEnv)
}
#setup_test()

# input test data.frame
df1 <- tibble::tribble(
  ~code,~`mijn<br>naam`,~punten, ~perc,
  "A1" ,"<b>Emre</b>", 18.1234, 1,
  "B1" ,"Ans", 19, 3,
) |>
  dplyr::mutate(punten= format(punten,digits=5))

# aux funs
fx <- function (html1) {
  h <- stringr::str_replace_all(as.character(html1),"\n"," ")
  stringr::str_squish(stringr::str_replace_all(h,"\\s+"," "))
}

fy <- function (tags1) {
  x <- htmltools::renderTags(tags1)
  fx(x$html)
}

test_that("create_html_table2 new functions okay?", {

  test_html_dsn <- tempfile(pattern="testhtml",fileext = ".html")
  # base case
  var_a <- create_html_table2(df1)
  exp_a  <-"<div> <table> <thead> <tr> <th>code</th> <th>mijn<br>naam</th> <th>punten</th> <th>perc</th> </tr> </thead> <tbody> <tr> <td>A1</td> <td>&lt;b&gt;Emre&lt;/b&gt;</td> <td>18.123</td> <td>1</td> </tr> <tr> <td>B1</td> <td>Ans</td> <td>19.000</td> <td>3</td> </tr> </tbody> </table> </div>"
  expect_equal(fx(var_a),exp_a )
  expect_false(unname(fs::file_exists(test_html_dsn)) )
  # base case with html_file=
  var_b <- create_html_table2(df1,html_file=test_html_dsn)

  expect_equal(fx(var_b),fx(var_a) )              # result is same (with or without html_file)
  expect_true(unname(fs::file_exists(test_html_dsn)) ) # but now html_file does exist

  var_c <- readLines(test_html_dsn)               # read contents of html_file
  var_c <- paste0(var_c,collapse="\n")            # convert to character string
  var_c <- stringr::str_split(var_c,"</*body>") [[1]][2] # select everything within <body> ... </body>
  expect_equal(fx(var_c),fx(var_a) )              # contents equal to base case
  unlink(test_html_dsn)

  # base case with class= and style_info=
  var_d <- create_html_table2(df1,class= "ttvax",style_info =".ttvax {width: auto; table-layout:auto ;")
  exp_d  <-"<div> <style>.ttvax {width: auto; table-layout:auto ;</style> <table class=\"ttvax\"> <thead> <tr> <th>code</th> <th>mijn<br>naam</th> <th>punten</th> <th>perc</th> </tr> </thead> <tbody> <tr> <td>A1</td> <td>&lt;b&gt;Emre&lt;/b&gt;</td> <td>18.123</td> <td>1</td> </tr> <tr> <td>B1</td> <td>Ans</td> <td>19.000</td> <td>3</td> </tr> </tbody> </table> </div>"
  expect_equal(fx(var_d),exp_d )

  # one alternative header
  var_e <- create_html_table2(df1,headers=paste0("h",1:4))
  exp_e  <-"<div> <table> <thead> <tr> <th>h1</th> </tr> <tr> <th>h2</th> </tr> <tr> <th>h3</th> </tr> <tr> <th>h4</th> </tr> </thead> <tbody> <tr> <td>A1</td> <td>&lt;b&gt;Emre&lt;/b&gt;</td> <td>18.123</td> <td>1</td> </tr> <tr> <td>B1</td> <td>Ans</td> <td>19.000</td> <td>3</td> </tr> </tbody> </table> </div>"
  expect_equal(fx(var_e),exp_e )

  # two alternative headers
  var_f <- create_html_table2(df1,headers=list(paste0("h",1:4),paste0("k",1:4)))
  exp_f  <-"<div> <table> <thead> <tr> <th>h1</th> <th>h2</th> <th>h3</th> <th>h4</th> </tr> <tr> <th>k1</th> <th>k2</th> <th>k3</th> <th>k4</th> </tr> </thead> <tbody> <tr> <td>A1</td> <td>&lt;b&gt;Emre&lt;/b&gt;</td> <td>18.123</td> <td>1</td> </tr> <tr> <td>B1</td> <td>Ans</td> <td>19.000</td> <td>3</td> </tr> </tbody> </table> </div>"
  expect_equal(fx(var_f),exp_f )

  # two alternative headers met colspan
  var_g <- create_html_table2(df1,headers=list(paste0("h",1:4),paste0("k",1:4)),colspan=list(2,3))
  exp_g  <-"<div> <table> <thead> <tr> <th>h1</th> <th colspan=\"2\">h2</th> <th>h4</th> </tr> <tr> <th>k1</th> <th>k2</th> <th colspan=\"2\">k3</th> </tr> </thead> <tbody> <tr> <td>A1</td> <td>&lt;b&gt;Emre&lt;/b&gt;</td> <td>18.123</td> <td>1</td> </tr> <tr> <td>B1</td> <td>Ans</td> <td>19.000</td> <td>3</td> </tr> </tbody> </table> </div>"
  expect_equal(fx(var_g),exp_g )

  # base case met tdHTML = T
  var_h <- create_html_table2(df1, tdHTML =T)
  exp_h  <-"<div> <table> <thead> <tr> <th>code</th> <th>mijn<br>naam</th> <th>punten</th> <th>perc</th> </tr> </thead> <tbody> <tr> <td>A1</td> <td><b>Emre</b></td> <td>18.123</td> <td>1</td> </tr> <tr> <td>B1</td> <td>Ans</td> <td>19.000</td> <td>3</td> </tr> </tbody> </table> </div>"
  expect_equal(fx(var_h),exp_h )

  #two alternative headers met colspan en alternatieve th format functie (ignores colspan)
  my_th_fun <- function(headers, ...) {
    purrr::map(headers, \(h1) {
      purrr::map(unname(h1), \(h2) {
          htmltools::tags$th(htmltools::HTML(h2))
      })
    })
  }

  var_i <- create_html_table2(df1, headers=list(paste0("h",1:4),paste0("k",1:4)),colspan=list(2,3),
                              format_thf = my_th_fun)
  expect_equal(fx(var_i),exp_f ) # results zelfde als two alternative headers zonder colspan

  #base case met alternatieve td format functie (voegt col_index toe)
  my_td_fun <- function(x, td_keep, col_index) {
    x <- paste0(x,col_index)
    if (td_keep) {
      x1 <- htmltools::HTML(x)
    } else {
      x1 <- x
    }
    htmltools::tags$td(x1)
  }

  var_j <- create_html_table2(df1, format_tdf = my_td_fun)
  exp_j  <-"<div> <table> <thead> <tr> <th>code</th> <th>mijn<br>naam</th> <th>punten</th> <th>perc</th> </tr> </thead> <tbody> <tr> <td>A11</td> <td>&lt;b&gt;Emre&lt;/b&gt;2</td> <td>18.1233</td> <td>14</td> </tr> <tr> <td>B11</td> <td>Ans2</td> <td>19.0003</td> <td>34</td> </tr> </tbody> </table> </div>"
  expect_equal(fx(var_j),exp_j ) # elke kolom heeft toegevoegd gekregen het kolom-nummer

  var_k <- create_html_table2(df1, caption = "mijn fraaie tabel")
  exp_k  <-"<div> <table> <caption>mijn fraaie tabel</caption> <thead> <tr> <th>code</th> <th>mijn<br>naam</th> <th>punten</th> <th>perc</th> </tr> </thead> <tbody> <tr> <td>A1</td> <td>&lt;b&gt;Emre&lt;/b&gt;</td> <td>18.123</td> <td>1</td> </tr> <tr> <td>B1</td> <td>Ans</td> <td>19.000</td> <td>3</td> </tr> </tbody> </table> </div>"
  expect_equal(fx(var_k),exp_k )


  var_l <- create_html_table2(df1, headers=list()) # no headers
  exp_l  <-"<div> <table> <thead></thead> <tbody> <tr> <td>A1</td> <td>&lt;b&gt;Emre&lt;/b&gt;</td> <td>18.123</td> <td>1</td> </tr> <tr> <td>B1</td> <td>Ans</td> <td>19.000</td> <td>3</td> </tr> </tbody> </table> </div>"
  expect_equal(fx(var_l),exp_l )

})

