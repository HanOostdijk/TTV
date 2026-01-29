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
# tests
test_that("create_style_table okay?", {
  st1 <-  create_style_table()
  res1 <- "<style> table { border-collapse: collapse; border: 2px solid black; table-layout: auto; width: auto; } :is(th, td) { border: 1px solid #ddd; padding-top: 4px; padding-bottom: 4px; padding-left: 4px; padding-right: 4px; text-align: center; background-color : white ; color : black ; } </style>"
  expect_equal(fx(st1),res1)
  st2 <- create_style_table(width="50%")
  res2 <- "<style> table { border-collapse: collapse; border: 2px solid black; table-layout: auto; width: 50%; } :is(th, td) { border: 1px solid #ddd; padding-top: 4px; padding-bottom: 4px; padding-left: 4px; padding-right: 4px; text-align: center; background-color : white ; color : black ; } </style>"
  expect_equal(fx(st2),res2)
  st3 <- create_style_table(ws="nowrap")
  res3 <- "<style> table { border-collapse: collapse; border: 2px solid black; table-layout: auto; width: auto; } :is(th, td) { border: 1px solid #ddd; padding-top: 4px; padding-bottom: 4px; padding-left: 4px; padding-right: 4px; text-align: center; background-color : white ; color : black ; white-space: nowrap; } </style>"
  expect_equal(fx(st3),res3)
  st4 <- create_style_table(classname="mc")
  res4 <- "<style> table.mc { border-collapse: collapse; border: 2px solid black; table-layout: auto; width: auto; } .mc :is(th, td) { border: 1px solid #ddd; padding-top: 4px; padding-bottom: 4px; padding-left: 4px; padding-right: 4px; text-align: center; background-color : white ; color : black ; } </style>"
  expect_equal(fx(st4),res4)
  st5 <- create_html_table(df1,tableclass="mc",html_include = st4)
  res5 <- "<html> <style> table.mc { border-collapse: collapse; border: 2px solid black; table-layout: auto; width: auto; } .mc :is(th, td) { border: 1px solid #ddd; padding-top: 4px; padding-bottom: 4px; padding-left: 4px; padding-right: 4px; text-align: center; background-color : white ; color : black ; } </style> <div> <table class=\"mc\"> <tr> <th>f1</th> <th>f2</th> <th>f3</th> <th>f4</th> </tr> <tr><td>1</td><td>3</td><td>5</td><td>A</td></tr> <tr><td>2</td><td>4</td><td>6</td><td>B</td></tr> </table> </div> </html>"
  expect_equal(fx(st5),res5)
})

test_that("create_header okay?", {
  st1 <- create_header(NULL)
  res1 <- NULL
  expect_equal(st1,list())
  st2 <- fy(create_header(names(df1)))
  res2 <- "<tr> <th>f1</th> <th>f2</th> <th>f3</th> <th>f4</th> </tr>"
  expect_equal(st2,res2)
  st3 <- fy(create_header(list(c("a 1","b  1"),c("a 2","b  2"))))
  res3 <- "<tr> <th>a 1</th> <th>b 1</th> </tr> <tr> <th>a 2</th> <th>b 2</th> </tr>"
  expect_equal(st3,res3)
  st4 <- fy(create_header(list(c("a","","b",""),c("","","b1","b2"))))
  res4 <- "<tr> <th rowspan=\"2\" colspan=\"2\">a</th> <th colspan=\"2\">b</th> </tr> <tr> <th>b1</th> <th>b2</th> </tr>"
  expect_equal(st4,res4)
})

test_that("create_html_table okay?", {
  st1 <- fx(create_html_table(df1))
  res1 <- "<div> <table> <tr> <th>f1</th> <th>f2</th> <th>f3</th> <th>f4</th> </tr> <tr><td>1</td><td>3</td><td>5</td><td>A</td></tr> <tr><td>2</td><td>4</td><td>6</td><td>B</td></tr> </table> </div>"
  expect_equal(st1,fx(res1))
  st2 <- fx(create_html_table(df1,tableclass="aap-noot"))
  res2 <-  "<div> <table class=\"aap-noot\"> <tr> <th>f1</th> <th>f2</th> <th>f3</th> <th>f4</th> </tr> <tr><td>1</td><td>3</td><td>5</td><td>A</td></tr> <tr><td>2</td><td>4</td><td>6</td><td>B</td></tr> </table> </div>"
  expect_equal(st2,fx(res2))
  st3 <- fx(create_html_table(df1,tableclass="aap-noot",header = list(c("X","Y"))))
  res3 <- "<div> <table class=\"aap-noot\"> <tr> <th>X</th> <th>Y</th> </tr> <tr><td>1</td><td>3</td><td>5</td><td>A</td></tr> <tr><td>2</td><td>4</td><td>6</td><td>B</td></tr> </table> </div>"
  expect_equal(st3,fx(res3))
  st6 <- fx(create_html_table(df1, header=NULL))
  res6 <-  "<div> <table> <tr><td>1</td><td>3</td><td>5</td><td>A</td></tr> <tr><td>2</td><td>4</td><td>6</td><td>B</td></tr> </table> </div>"
  expect_equal(st6,fx(res6))
  st8 <- fx(create_html_table(df1,caption="my caption",captionclass="mycaptionclass",,captionstyle="white-space: nowrap;caption-side: bottom;"))
  res8 <- "<div> <table> <caption class=\"mycaptionclass\" style=\"white-space: nowrap;caption-side: bottom;\">my caption</caption> <tr> <th>f1</th> <th>f2</th> <th>f3</th> <th>f4</th> </tr> <tr><td>1</td><td>3</td><td>5</td><td>A</td></tr> <tr><td>2</td><td>4</td><td>6</td><td>B</td></tr> </table> </div>"
  expect_equal(st8,fx(res8))
  st9 <- fx(create_html_table(df1,thclass='TH',tdclass="TD",tdstyle="white-space: nowrap;"))
  res9 <- "<div> <table> <tr> <th class=\"TH\">f1</th> <th class=\"TH\">f2</th> <th class=\"TH\">f3</th> <th class=\"TH\">f4</th> </tr> <tr><td class=\"TD\" style=\"white-space: nowrap;\">1</td><td class=\"TD\" style=\"white-space: nowrap;\">3</td><td class=\"TD\" style=\"white-space: nowrap;\">5</td><td class=\"TD\" style=\"white-space: nowrap;\">A</td></tr> <tr><td class=\"TD\" style=\"white-space: nowrap;\">2</td><td class=\"TD\" style=\"white-space: nowrap;\">4</td><td class=\"TD\" style=\"white-space: nowrap;\">6</td><td class=\"TD\" style=\"white-space: nowrap;\">B</td></tr> </table> </div>"
  expect_equal(st9,fx(res9))
})
