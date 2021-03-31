problematic_ciks <- c(1321482, 903954, 861177)

get_filings_try_catch <- function(cik_arg, year_arg) {
  out <- tryCatch(
    {
      # Just to highlight: if you want to use more than one
      # R expression in the "try" part then you'll have to
      # use curly brackets.
      # 'tryCatch()' will return the last evaluated expression
      # in case the "try" part was completed successfully
      tidysec::get_13f(cik = cik_arg, year = year_arg, amendments = TRUE)
      # The return value of `readLines()` is the actual value
      # that will be returned in case there is no condition
      # (e.g. warning or error).
      # You don't need to state the return value via `return()` as code
      # in the "try" part is not wrapped inside a function (unlike that
      # for the condition handlers for warnings and error below)
    },
    error = function(cond) {
      message(paste("No filings available"))
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      return(filing_proto)
    },
    warning = function(cond) {
      message(paste("Warning in filingss function"))
      message("Here's the original warning message:")
      message(cond)
      # Choose a return value in case of warning
      return(NULL)
    },
    finally = {
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
      # If you want more than one expression to be executed, then you
      # need to wrap them in curly brackets ({...}); otherwise you could
      # just have written 'finally=<expression>'
      # message(paste("Processed URL:", url))
      message("Some other message at the end")
    }
  )
  return(out)
}

ciks <- c(
  1588340, 1634222, 928633, 1368163, 1535602, 1291274, 1599469,
  1599576, 1535631, 1793755, 1544599, 1068833, 1727588, 824468,
  1321482, 903954, 861177, 1132716, 1610520, 1114446, 1615423,
  1535660, 1535784, 1491719, 1528147, 1615423, 1615424, 1615305,
  1649647, 1689918, 1658354, 1667654, 1649591, 1650162, 1649592,
  1536550, 1641992
)

y <- lapply(ciks, get_filings_try_catch, year = 2021)
