#' Write SAM assesseent to a TAF json file
#'
#' Write a SAM assessment to a json file int the correct
#' format to be uploaded to the TAF assessment results
#' database
#'
#' @param fit a fitted object from a SAM model fit.
#' @param dir an optional directory name.
#'
#' @importFrom icesTAF xtab2taf
#'
#' @export
write_sam_upload <- function(fit, dir = NULL) {
  stock <-
    list(
      list(
        values = faytable(fit),
        valueType = "harvest",
        unit = "F"
      ),
      list(
        values = ntable(fit),
        valueType = "stock.n",
        unit = "N"
      ),
      list(
        values = fit$data$stockMeanWeight,
        valueType = "stock.wt",
        unit = "kg"
      ),
      list(
        values = getFleet(fit, 1),
        valueType = "catch.n",
        unit = "N"
      ),
      list(
        values = fit$data$catchMeanWeight,
        valueType = "catch.wt",
        unit = "kg"
      ),
      list(
        values = getFleet(fit, 1) * fit$data$landFrac,
        valueType = "landings.n",
        unit = "N"
      ),
      list(
        values = fit$data$landMeanWeight,
        valueType = "landings.wt",
        unit = "kg"
      ),
      list(
        values = getFleet(fit, 1) * (1 - fit$data$landFrac),
        valueType = "discards.n",
        unit = "N"
      ),
      list(
        values = fit$data$disMeanWeight,
        valueType = "discards.wt",
        unit = "kg"
      ),
      list(
        values = fit$data$propF,
        valueType = "harvest.spwn",
        unit = "proportion"
      ),
      list(
        values = fit$data$propM,
        valueType = "m.spwn",
        unit = "proportion"
      ),
      list(
        values = fit$data$propMat,
        valueType = "mat",
        unit = "proportion"
      ),
      list(
        values = fit$data$natMor,
        valueType = "m",
        unit = "M"
      )
    )
  names(stock) <- sapply(stock, "[[", "valueType")

  stock <-
    lapply(stock, function(x) {
      x$values <- icesTAF::xtab2taf(x$values)
      x
    })

  stock
}


# internal functions from stockassessment package
faytable <- function(fit) {
  idx <- fit$conf$keyLogFsta[1, ] + 2
  ret <- cbind(NA, exp(t(fit$pl$logF)))[, idx]
  ret[, idx == 0] <- 0
  colnames(ret) <- fit$conf$minAge:fit$conf$maxAge
  rownames(ret) <- fit$data$years
  return(ret)
}

ntable <- function(fit) {
  ret <- exp(t(fit$pl$logN))
  colnames(ret) <- fit$conf$minAge:fit$conf$maxAge
  rownames(ret) <- fit$data$years
  return(ret)
}

getFleet <- function(fit, fleet) {
  fidx <- fit$data$aux[, "fleet"] == fleet
  aux <- fit$data$aux[fidx, ]
  logobs <- fit$data$logobs[fidx]
  .goget <- function(y, a) {
    ret <- exp(logobs[aux[, "year"] == y & aux[, "age"] ==
      a])
    ifelse(length(ret) == 0, 0, ret)
  }
  yr <- min(aux[, "year"]):max(aux[, "year"])
  ar <- min(aux[, "age"]):max(aux[, "age"])
  tmp <- outer(yr, ar, Vectorize(.goget))
  dimnames(tmp)[[1]] <- yr
  dimnames(tmp)[[2]] <- ar
  return(tmp)
}
