df1 <- data.frame(f1=c(1,2),f2=c("A","B"))
fx <- function (html1) stringr::str_replace_all(as.character(html1),"  "," ")

fit_table_def <- "
  <style>
  .fit-tablex {
    width: auto;
    table-layout: auto;
  }

  .fit-tablex th, td {
    padding: 10px;
    border: none;
    white-space: nowrap;
  }

  .fit-tablex  tr {
    line-height: 80%;
    text-align: left;
  }
  </style>
  "
fit_table_res <- "<html>
 \n <style>
 .fit-tablex {
  width: auto;
  table-layout: auto;
 }

 .fit-tablex th, td {
  padding: 10px;
  border: none;
  white-space: nowrap;
 }

 .fit-tablex tr {
  line-height: 80%;
  text-align: left;
 }
 </style>
 \n <div>
  <table class=\"fit-tablex\">
   <tr><th>f1</th><th>f2</th></tr>
   <tr><td>1</td><td>A</td></tr>
   <tr><td>2</td><td>B</td></tr>
  </table>
 </div>
</html>"

test_that("create_html_table okay", {
  st1 <- fx(create_html_table(df1))
  res1 <- "<div>\n <table>\n  <tr><th>f1</th><th>f2</th></tr>\n  <tr><td>1</td><td>A</td></tr>\n  <tr><td>2</td><td>B</td></tr>\n </table>\n</div>"
  expect_equal(st1,res1)
  st2 <- fx(create_html_table(df1,tableclass="aap-noot"))
  res2 <- "<div>\n <table class=\"aap-noot\">\n  <tr><th>f1</th><th>f2</th></tr>\n  <tr><td>1</td><td>A</td></tr>\n  <tr><td>2</td><td>B</td></tr>\n </table>\n</div>"
  expect_equal(st2,res2)
  st3 <- fx(create_html_table(df1,tableclass="aap-noot",header = list(c("X","Y"))))
  res3 <- "<div>\n <table class=\"aap-noot\">\n  <tr><th>X</th><th>Y</th></tr>\n  <tr><td>1</td><td>A</td></tr>\n  <tr><td>2</td><td>B</td></tr>\n </table>\n</div>"
  expect_equal(st3,res3)
  st4 <- fx(create_html_table(df1,tableclass="aap-noot",header = list(c("X1","Y1"),c("X2","Y2"))))
  res4 <- "<div>\n <table class=\"aap-noot\">\n  <tr><th>X1</th><th>Y1</th></tr>\n  <tr><th>X2</th><th>Y2</th></tr>\n  <tr><td>1</td><td>A</td></tr>\n  <tr><td>2</td><td>B</td></tr>\n </table>\n</div>"
  expect_equal(st4,res4)
  st5 <- fx(create_html_table(df1,tableclass="aap-noot", tablestyle="border-collapse:collapse;border:none;"))
  res5 <- "<div>\n <table class=\"aap-noot\" style=\"border-collapse:collapse;border:none;\">\n  <tr><th>f1</th><th>f2</th></tr>\n  <tr><td>1</td><td>A</td></tr>\n  <tr><td>2</td><td>B</td></tr>\n </table>\n</div>"
  expect_equal(st5,res5)
  st6 <- fx(create_html_table(df1, header=NULL))
  res6 <- "<div>\n <tabl>\n  <tr><td>1</td><td>A</td></tr>\n  <tr><td>2</td><td>B</td></tr>\n </table>\n</div>"
  expect_equal(st5,res5)
  st7 <- fx( create_html_table(df1,tableclass="fit-tablex",html_include=fit_table_def))
  expect_equal(st7,fit_table_res)
  st8 <- fx(create_html_table(df1,caption="my caption"))
  res8 <- "<div>\n <table>\n  <caption>my caption</caption>\n  <tr><th>f1</th><th>f2</th></tr>\n  <tr><td>1</td><td>A</td></tr>\n  <tr><td>2</td><td>B</td></tr>\n </table>\n</div>"
  expect_equal(st8,res8)
  st9 <- fx(create_html_table(df1,caption="my caption",captionstyle="white-space: nowrap;caption-side: bottom;"))
  res9 <- "<div>\n <table>\n  <caption style=\"white-space: nowrap;caption-side: bottom;\">my caption</caption>\n  <tr><th>f1</th><th>f2</th></tr>\n  <tr><td>1</td><td>A</td></tr>\n  <tr><td>2</td><td>B</td></tr>\n </table>\n</div>"
  expect_equal(st9,res9)
  st10 <- fx(create_html_table(df1,caption="my caption",captionclass="mycaptionclass"))
  res10 <- "<div>\n <table>\n  <caption class=\"mycaptionclass\">my caption</caption>\n  <tr><th>f1</th><th>f2</th></tr>\n  <tr><td>1</td><td>A</td></tr>\n  <tr><td>2</td><td>B</td></tr>\n </table>\n</div>"
  expect_equal(st10,res10)
  st11 <- fx(create_html_table(df1,thclass='TH',tdclass="TD"))
  res11 <- "<div>\n <table>\n  <tr><th class=\"TH\">f1</th><th class=\"TH\">f2</th></tr>\n  <tr><td class=\"TD\">1</td><td class=\"TD\">A</td></tr>\n  <tr><td class=\"TD\">2</td><td class=\"TD\">B</td></tr>\n </table>\n</div>"
  expect_equal(st11,res11)
  st12 <- fx(create_html_table(df1,tdstyle="white-space: nowrap;"))
  res12 <- "<div>\n <table>\n  <tr><th>f1</th><th>f2</th></tr>\n  <tr><td style=\"white-space: nowrap;\">1</td><td style=\"white-space: nowrap;\">A</td></tr>\n  <tr><td style=\"white-space: nowrap;\">2</td><td style=\"white-space: nowrap;\">B</td></tr>\n </table>\n</div>"
  expect_equal(st12,res12)


})
