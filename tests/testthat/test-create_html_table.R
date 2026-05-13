
# voor testen buiten de test interface
  # library(TTV)
  # library(testthat)
  # library(dplyr)
  # library(htmltools)
  # hb <- htmltools::browsable
#

# input test data.frame
df1 <- data.frame(f1=1:2,f2=3:4,f3=5:6,f4=c("A","B"))
# aux funs
fx <- function (html1) {
  h <- stringr::str_replace_all(as.character(html1),"\n"," ")
  stringr::str_replace_all(h,"\\s+"," ")
}

fy <- function (tags1) {
  x <- htmltools::renderTags(tags1)
  fx(x$html)
}

et1 <- tibble::tribble(
  ~code,~naam,~punten, ~perc,
  "A1" ,"<b>Emre</b>", 18.1234, 1,
  "A1" ,"Ma", 12, 2,
  "B1" ,"Ans", 19, 3,
  "B1" ,"Bart", 11899.12, 4,
  "B1" ,"Cornelis", 20, 5
) |>
  dplyr::mutate(punten= format(punten,digits=5))
# |>   print()

urls <- c(A1="http://xyz.nl#a1",B1="http://xyz.nl#b1")

c_h1<- function(key) {
  htmltools::tagAppendAttributes(htmltools::a(key),href= urls[key])
}

style_info1 <- '
  .c_class {
    font-size: 200% ;
    font-weight: 800;
    white-space: nowrap ;
    background-color : blue ;
  }
 '

# tests
test_that("cpm okay?", {
  act1 <-  TTV:::cpm()
  expect_null(act1)
  act2 <-  TTV:::cpm(list())
  expect_null(act2)
  act3 <-  TTV:::cpm(list('abc'))
  exp3 <- list(1L,1,list('abc'))
  expect_equal(act3,exp3)
  act4 <-  TTV:::cpm(list('a','b'))
  exp4 <- list(2L,1L,list('a','b'))
  expect_equal(act4,exp4)
  act5 <-  TTV:::cpm(list(c('a','b')))
  exp5 <- list(1L,2L,list(c('a','b')))
  expect_equal(act5,exp5)
  act6 <-  TTV:::cpm(list(c('a','b','c'),c('d','e','f')))
  exp6 <- list(2L,3L,list(c('a','b','c'),c('d','e','f')))
  expect_equal(act6,exp6)
  act7 <-  TTV:::cpm( c('a','b') )
  exp7 <- list(1L,2L,list(c('a','b')))
  expect_equal(act7,exp7)
  h1   <-  htmltools::HTML('<a href="x">tt </a>' )
  act8 <-  TTV:::cpm(h1)
  exp8 <- list(1L,1L,list(h1))
  expect_identical(act8,exp8)
  h1 <- c('<a href="#x">x1</a>','<a href="#y">y1 </a>')
  h2 <- purrr::map(h1,htmltools::HTML)
  act10 <- TTV:::cpm(list(h2) )
  exp10 <- list(1L,2L,list(h2))
  expect_identical(act10,exp10)
  act9 <-  TTV:::cpm("abc"  )
  exp9 <- list(1L,1L,list("abc"))
  expect_equal(act9,exp9)
})

test_that("create_style_table okay?", {
  act1 <-  create_style_table()
  exp1 <- "table { border-collapse: collapse; border: 2px solid black; table-layout: auto; width: auto; } :is(th, td) { border: 1px solid #ddd; padding-top: 4px; padding-bottom: 4px; padding-left: 4px; padding-right: 4px; text-align: center; background-color : white ; color : black ; }"
  expect_equal(fx(act1),exp1)
  act2 <- create_style_table(width="50%")
  exp2 <- "table { border-collapse: collapse; border: 2px solid black; table-layout: auto; width: 50%; } :is(th, td) { border: 1px solid #ddd; padding-top: 4px; padding-bottom: 4px; padding-left: 4px; padding-right: 4px; text-align: center; background-color : white ; color : black ; }"
  expect_equal(fx(act2),exp2)
  act3 <- create_style_table(white_space="nowrap")
  exp3 <- "table { border-collapse: collapse; border: 2px solid black; table-layout: auto; width: auto; } :is(th, td) { border: 1px solid #ddd; padding-top: 4px; padding-bottom: 4px; padding-left: 4px; padding-right: 4px; text-align: center; background-color : white ; color : black ; white-space: nowrap; }"
  expect_equal(fx(act3),exp3)
  act4 <- create_style_table(classname="mc",tabborder="2px solid red")
  exp4 <- "table.mc { border-collapse: collapse; border: 2px solid red; table-layout: auto; width: auto; } .mc :is(th, td) { border: 1px solid #ddd; padding-top: 4px; padding-bottom: 4px; padding-left: 4px; padding-right: 4px; text-align: center; background-color : white ; color : black ; }"
  expect_equal(fx(act4),exp4)
  act5 <- create_html_table(df1,t_class="mc",i_style = act4)
  exp5 <-  "<style>table.mc { border-collapse: collapse; border: 2px solid red; table-layout: auto; width: auto; } .mc :is(th, td) { border: 1px solid #ddd; padding-top: 4px; padding-bottom: 4px; padding-left: 4px; padding-right: 4px; text-align: center; background-color : white ; color : black ; }</style> <table class=\"mc\"> <tr> <th>f1</th> <th>f2</th> <th>f3</th> <th>f4</th> </tr> <tr> <td>1</td> <td>3</td> <td>5</td> <td>A</td> </tr> <tr> <td>2</td> <td>4</td> <td>6</td> <td>B</td> </tr> </table>"
  expect_equal(fx(act5),exp5)
})

test_that("create_header okay?", {
  act1 <- TTV:::create_header(NULL)
  expect_equal(act1,list())
  act1a <- TTV:::create_header(list())
  expect_equal(act1a,list())
  act2 <- fy(TTV:::create_header(names(df1)))
  exp2 <- "<tr> <th>f1</th> <th>f2</th> <th>f3</th> <th>f4</th> </tr>"
  expect_equal(act2,exp2)
  act3 <- fy(TTV:::create_header(list(c("a 1","b  1"),c("a 2","b  2"))))
  exp3 <- "<tr> <th>a 1</th> <th>b 1</th> </tr> <tr> <th>a 2</th> <th>b 2</th> </tr>"
  expect_equal(act3,exp3)
  act4 <- fy(TTV:::create_header(list(c("a","","b",""),c("","","b1","b2"))))
  exp4 <- "<tr> <th rowspan=\"2\" colspan=\"2\">a</th> <th colspan=\"2\">b</th> </tr> <tr> <th>b1</th> <th>b2</th> </tr>"
  expect_equal(act4,exp4)
  h1 <- c('<a href="#x">x1</a>','<a href="#y">y1 </a>')
  act4 <- fy(TTV:::create_header(list(purrr::map(h1,htmltools::HTML)) ))
  exp4 <- "<tr> <th><a href=\"#x\">x1</a></th> <th><a href=\"#y\">y1 </a></th> </tr>"
  expect_equal(act4,exp4)
})

test_that("create_html_table new functions okay?", {

  table1a <- create_html_table(et1,vars1= c("naam","punten"),group1="code",header1=c("a","b"), caption1="mycaption", gr_tr=c_h1,
                  t_class= "t-class", t_style="border-collapse:collapse;", # table class and style
                  c_class= "c_class", c_style="font-style: italic;",# caption class and style
                  gr_class= "gr-class", gr_style="font-size:120%;", # group tr class and style
                  gd_class= "gd-class", gd_style="font-size:140%;", # group td class and style
                  hr_class= "hr-class", hr_style="font-size:110%;", # header tr class and style
                  hh_class= "hh-class", hh_style="font-size:90%;",  # header th class and style
                  dr_class= "dr-class", dr_style="font-size:80%;",  # detail tr class and style
                  dd_class= "dd-class", dd_style="font-size:70%;",  # detail td class and style
                  i_style=style_info1               # information to include as <style> element
  )
  exp1a  <- "<style> .c_class { font-size: 200% ; font-weight: 800; white-space: nowrap ; background-color : blue ; } </style> <table class=\"t-class\" style=\"border-collapse:collapse;\"> <caption class=\"c_class\" style=\"font-style: italic;\">mycaption</caption> <tr class=\"gr-class\" style=\"font-size:120%;\"> <td colspan=\"2\" class=\"gd-class\" style=\"font-size:140%;\"> <a href=\"http://xyz.nl#a1\">A1</a> </td> </tr> <tr class=\"hr-class\" style=\"font-size:110%;\"> <th class=\"hh-class\" style=\"font-size:90%;\">a</th> <th class=\"hh-class\" style=\"font-size:90%;\">b</th> </tr> <tr class=\"dr-class\" style=\"font-size:80%;\"> <td class=\"dd-class\" style=\"font-size:70%;\"><b>Emre</b></td> <td class=\"dd-class\" style=\"font-size:70%;\"> 18.123</td> </tr> <tr class=\"dr-class\" style=\"font-size:80%;\"> <td class=\"dd-class\" style=\"font-size:70%;\">Ma</td> <td class=\"dd-class\" style=\"font-size:70%;\"> 12.000</td> </tr> <tr class=\"gr-class\" style=\"font-size:120%;\"> <td colspan=\"2\" class=\"gd-class\" style=\"font-size:140%;\"> <a href=\"http://xyz.nl#b1\">B1</a> </td> </tr> <tr class=\"hr-class\" style=\"font-size:110%;\"> <th class=\"hh-class\" style=\"font-size:90%;\">a</th> <th class=\"hh-class\" style=\"font-size:90%;\">b</th> </tr> <tr class=\"dr-class\" style=\"font-size:80%;\"> <td class=\"dd-class\" style=\"font-size:70%;\">Ans</td> <td class=\"dd-class\" style=\"font-size:70%;\"> 19.000</td> </tr> <tr class=\"dr-class\" style=\"font-size:80%;\"> <td class=\"dd-class\" style=\"font-size:70%;\">Bart</td> <td class=\"dd-class\" style=\"font-size:70%;\">11899.120</td> </tr> <tr class=\"dr-class\" style=\"font-size:80%;\"> <td class=\"dd-class\" style=\"font-size:70%;\">Cornelis</td> <td class=\"dd-class\" style=\"font-size:70%;\"> 20.000</td> </tr> </table>"
  expect_equal(fx(table1a),exp1a )


  table1b <- create_html_table(et1,vars1= c("naam","punten"),group1="code",header1=c("a","b"))
  exp1b  <- "<table> <tr> <td colspan=\"2\">A1</td> </tr> <tr> <th>a</th> <th>b</th> </tr> <tr> <td><b>Emre</b></td> <td> 18.123</td> </tr> <tr> <td>Ma</td> <td> 12.000</td> </tr> <tr> <td colspan=\"2\">B1</td> </tr> <tr> <th>a</th> <th>b</th> </tr> <tr> <td>Ans</td> <td> 19.000</td> </tr> <tr> <td>Bart</td> <td>11899.120</td> </tr> <tr> <td>Cornelis</td> <td> 20.000</td> </tr> </table>"

  expect_equal(fx(table1b),exp1b)

  table1c <- create_html_table(et1,vars1= c("naam","punten"),group1=NULL,header1=c("a","b"))
  #hb(table1c)
  exp1c  <- "<table> <tr> <th>a</th> <th>b</th> </tr> <tr> <td><b>Emre</b></td> <td> 18.123</td> </tr> <tr> <td>Ma</td> <td> 12.000</td> </tr> <tr> <td>Ans</td> <td> 19.000</td> </tr> <tr> <td>Bart</td> <td>11899.120</td> </tr> <tr> <td>Cornelis</td> <td> 20.000</td> </tr> </table>"

  expect_equal(fx(table1c),exp1c)
})


test_that("create_html_table okay?", {
  act1 <- fx(create_html_table(df1))
  exp1 <- "<table> <tr> <th>f1</th> <th>f2</th> <th>f3</th> <th>f4</th> </tr> <tr> <td>1</td> <td>3</td> <td>5</td> <td>A</td> </tr> <tr> <td>2</td> <td>4</td> <td>6</td> <td>B</td> </tr> </table>"
  expect_equal(act1,fx(exp1))
  act1a <- fx(create_html_table(df1 |> dplyr::filter(F)))
  exp1a <- "<table> <tr> <th>f1</th> <th>f2</th> <th>f3</th> <th>f4</th> </tr> </table>"
  expect_equal(act1a,fx(exp1a))
  act2 <- fx(create_html_table(df1,t_class="aap-noot"))
  exp2 <-  "<table class=\"aap-noot\"> <tr> <th>f1</th> <th>f2</th> <th>f3</th> <th>f4</th> </tr> <tr> <td>1</td> <td>3</td> <td>5</td> <td>A</td> </tr> <tr> <td>2</td> <td>4</td> <td>6</td> <td>B</td> </tr> </table>"
  expect_equal(act2,fx(exp2))
  act3 <- fx(create_html_table(df1,t_class="aap-noot",header = list(c("X","Y"))))
  exp3 <- "<table class=\"aap-noot\"> <tr> <th>X</th> <th>Y</th> </tr> <tr> <td>1</td> <td>3</td> <td>5</td> <td>A</td> </tr> <tr> <td>2</td> <td>4</td> <td>6</td> <td>B</td> </tr> </table>"
  expect_equal(act3,fx(exp3))
  act6 <- fx(create_html_table(df1, header=NULL))
  exp6 <-  "<table> <tr> <td>1</td> <td>3</td> <td>5</td> <td>A</td> </tr> <tr> <td>2</td> <td>4</td> <td>6</td> <td>B</td> </tr> </table>"
  expect_equal(act6,fx(exp6))
  act8 <- fx(create_html_table(df1,caption="my caption",c_class="mycaptionclass",c_style="white-space: nowrap;caption-side: bottom;"))
  exp8 <- "<table> <caption class=\"mycaptionclass\" style=\"white-space: nowrap;caption-side: bottom;\">my caption</caption> <tr> <th>f1</th> <th>f2</th> <th>f3</th> <th>f4</th> </tr> <tr> <td>1</td> <td>3</td> <td>5</td> <td>A</td> </tr> <tr> <td>2</td> <td>4</td> <td>6</td> <td>B</td> </tr> </table>"
  expect_equal(act8,fx(exp8))
})
