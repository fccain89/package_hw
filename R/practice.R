# 1.c.
col_means <- function(df) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }

  means <- numeric(ncol(df))

  for (i in seq_along(df)) {
    if (is.numeric(df[[i]])) {
      means[i] <- sum(df[[i]], na.rm = TRUE) / sum(!is.na(df[[i]]))
    } else {
      means[i] <- NA
    }
  }

  names(means) <- names(df)
  return(means)
}
