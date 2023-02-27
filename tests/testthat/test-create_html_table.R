df1 <- data.frame(f1=c(1,2),f2=c("A","B"))
fx <- function (html1) stringr::str_replace_all(as.character(html1),"  "," ")

test_that("create_html_table okay", {
  st1 <- fx(create_html_table(df1))
  res1 <- "<div>\n <table border=\"0\" cellspacing=\"0\" cellpadding=\"0\" style=\"border-collapse:collapse;border:none;\">\n  <tr><th>f1</th><th>f2</th></tr>\n  <tr><td>1</td><td>A</td></tr>\n  <tr><td>2</td><td>B</td></tr>\n </table>\n</div>"
  expect_equal(st1,res1)
  st2 <- fx(create_html_table(df1,class="aap-noot"))
  res2 <- "<div>\n <table class=\"aap-noot\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\" style=\"border-collapse:collapse;border:none;\">\n  <tr><th>f1</th><th>f2</th></tr>\n  <tr><td>1</td><td>A</td></tr>\n  <tr><td>2</td><td>B</td></tr>\n </table>\n</div>"
  expect_equal(st2,res2)
  st3 <- fx(create_html_table(df1,class="aap-noot",header = list(c("X","Y"))))
  res3 <- "<div>\n <table class=\"aap-noot\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\" style=\"border-collapse:collapse;border:none;\">\n  <tr><th>X</th><th>Y</th></tr>\n  <tr><td>1</td><td>A</td></tr>\n  <tr><td>2</td><td>B</td></tr>\n </table>\n</div>"
  expect_equal(st3,res3)
  st4 <- fx(create_html_table(df1,class="aap-noot",header = list(c("X1","Y1"),c("X2","Y2"))))
  res4 <- "<div>\n <table class=\"aap-noot\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\" style=\"border-collapse:collapse;border:none;\">\n  <tr><th>X1</th><th>Y1</th></tr>\n  <tr><th>X2</th><th>Y2</th></tr>\n  <tr><td>1</td><td>A</td></tr>\n  <tr><td>2</td><td>B</td></tr>\n </table>\n</div>"
  expect_equal(st4,res4)
})
