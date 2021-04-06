# ciks <- c(1588340, 1634222, 928633, 1368163, 1535602, 1291274, 1599469, 1599576,
# 1535631, 1793755, 1544599, 1068833, 1727588, 824468, 1321482, 903954, 861177,
# 1132716, 1610520, 1114446, 1615423, 1535660, 1535784, 1491719, 1528147, 1615423,
# 1615424, 1615305, 1649647, 1689918, 1658354, 1667654, 1649591,
# 1650162, 1649592, 1536550, 1641992)

# test for single portfolio
cik_LGT <- "1641992"
one_bank <- get_13f(cik = cik_LGT, year = 2020)


test_that("`get_13f()` returns data frame with > 0 rows for single portfolio `", {

  expect_s3_class(one_bank, "data.frame")
  expect_gt(nrow(one_bank), 0)

})

# test for multiple portfolios
multiple_banks <- c("1727588", "824468", "1321482")
test_13f_multiple_banks <- get_13f(cik = multiple_banks, year = 2020)

test_that("`get_13f()` returns data frame with > 0 rows for multiple portfolio `", {

  expect_s3_class(test_13f_multiple_banks, "data.frame")
  expect_gt(nrow(test_13f_multiple_banks), 0)

})

# test for multiple portfolios from 2021
multiple_banks <- c("1727588", "824468", "1321482")
test_13f_multiple_banks_2021 <- get_13f(cik = multiple_banks, year = 2021)

test_that("`get_13f()` returns data frame with > 0 rows for multiple portfolio `", {

  expect_s3_class(test_13f_multiple_banks_2021, "data.frame")
  expect_gt(nrow(test_13f_multiple_banks_2021), 0)

})
