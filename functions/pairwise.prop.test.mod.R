# modified pairwise prop test

# funcion is modified to return not just the p value matrix, but also other outputs

pairwise.prop.test.mod<-function (x, n, p.adjust.method = p.adjust.methods, ...) 
{
  p.adjust.method <- match.arg(p.adjust.method)
  METHOD <- "Pairwise comparison of proportions"
  DNAME <- deparse1(substitute(x))
  if (is.matrix(x)) {
    if (ncol(x) != 2) 
      stop("'x' must have 2 columns")
    n <- rowSums(x)
    x <- x[, 1]
  }
  else {
    DNAME <- paste(DNAME, "out of", deparse1(substitute(n)))
    if (length(x) != length(n)) 
      stop("'x' and 'n' must have the same length")
  }
  OK <- complete.cases(x, n)
  x <- x[OK]
  n <- n[OK]
  if (length(x) < 2L) 
    stop("too few groups")
  compare.levels <- function(i, j) {
    prop.test(x[c(i, j)], n[c(i, j)], ...)$statistic
  }
  level.names <- names(x) %||% seq_along(x)
  PVAL <- pairwise.table(compare.levels, level.names, p.adjust.method)
  ans <- list(method = METHOD, data.name = DNAME, p.value = PVAL, 
              p.adjust.method = p.adjust.method)
  class(ans) <- "pairwise.htest"
  ans
}


