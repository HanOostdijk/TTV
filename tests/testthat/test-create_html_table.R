df1 <- data.frame(f1=c(1,2),f2=c("A","B"))

test_that("create_html_table okay", {
  st1 <- create_html_table(df1)
  res1 <- "<table border=\"0\" cellspacing=\"0\" cellpadding=\"0\" style=\"border-collapse:collapse;border:none;\">\n  <tr><th>f1</th><th>f2</th></tr>\n  <tr><td>1</td><td>A</td></tr>\n  <tr><td>2</td><td>B</td></tr>\n</table>"
  expect_equal(as.character(st1),res1)
  st2 <- create_html_table(df1,class="aap-noot")
  res2 <- "<table class=\"aap-noot\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\" style=\"border-collapse:collapse;border:none;\">\n  <tr><th>f1</th><th>f2</th></tr>\n  <tr><td>1</td><td>A</td></tr>\n  <tr><td>2</td><td>B</td></tr>\n</table>"
  expect_equal(as.character(st2),res2)
  st3 <- create_html_table(df1,class="aap-noot",header = list(c("X","Y")))
  res3 <- "<table class=\"aap-noot\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\" style=\"border-collapse:collapse;border:none;\">\n  <tr><th>X</th><th>Y</th></tr>\n  <tr><td>1</td><td>A</td></tr>\n  <tr><td>2</td><td>B</td></tr>\n</table>"
  expect_equal(as.character(st3),res3)
  st4 <- create_html_table(df1,class="aap-noot",header = list(c("X1","Y1"),c("X2","Y2")))
  res4 <- "<table class=\"aap-noot\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\" style=\"border-collapse:collapse;border:none;\">\n  <tr><th>X1</th><th>Y1</th></tr>\n  <tr><th>X2</th><th>Y2</th></tr>\n  <tr><td>1</td><td>A</td></tr>\n  <tr><td>2</td><td>B</td></tr>\n</table>"
  expect_equal(as.character(st4),res4)
})
