#' Return indices of local maxima
#'
#' Not robust. Defines local maxima as points that are
#' greater than both their immediate neighbors. As such,
#' it is quite sensitive to noise.
#'
#' @param x The numeric vector over which maxima are calculated
#' @param endpoints Directs if the first and/or last points should
#'   be included as viable candidates. Options are
#'   \code{c("neither", "start", "end", "both")}. Default is "neither"
#'
#' @export
localMaxima <- function(x, endpoints = c("neither", "start", "end", "both")) {
    if (length(x) < 3) {
        stop("'x' must be of at least length 3!")
    }
    endpoints <- endpoints[1]
    maxa <- c()
    for (idx in 2:(length(x)-1)) {
        if (x[idx] > x[idx - 1]) {
            if (x[idx] > x[idx + 1]) {
                maxa <- c(maxa, idx)
            }
        }
    }
    start <- FALSE
    end <- FALSE
    if (endpoints == "both") {
        start <- TRUE
        end <- TRUE
    } else if (endpoints == "start") {
        start <- TRUE
    } else if (endpoints == "end") {
        end <- TRUE
    } else if (endpoints == "neither") {
        # Do nothing
    } else {
        warning("'endpoints' must be 'both', 'start', 'end', or 'neither'! Defaulting to 'both'")
        start <- TRUE
        end <- TRUE
    }
    if (start) {
        if (x[1] > x[2]) {
            maxa <- c(1, maxa)
        }
    }
    if (end) {
        if (x[length(x)] > x[length(x)-1]) {
            maxa <- c(maxa, length(x))
        }
    }
    return(maxa)
}

#' Return indices of local minima
#'
#' Not robust. Defines local minima as points that are
#' less than both their immediate neighbors. As such,
#' it is quite sensitive to noise.
#'
#' @param x The numeric vector over which minima are calculated
#' @param endpoints Directs if the first and/or last points should
#'   be included as viable candidates. Options are
#'   \code{c("neither", "start", "end", "both")}. Default is "neither"
#'
#' @export
localMinima <- function(x, endpoints = c("neither", "start", "end", "both")) {
    if (length(x) < 3) {
        stop("'x' must be of at least length 3!")
    }
    endpoints <- endpoints[1]
    mina <- c()
    for (idx in 2:(length(x)-1)) {
        if (x[idx] < x[idx - 1]) {
            if (x[idx] < x[idx + 1]) {
                mina <- c(mina, idx)
            }
        }
    }
    start <- FALSE
    end <- FALSE
    if (endpoints == "both") {
        start <- TRUE
        end <- TRUE
    } else if (endpoints == "start") {
        start <- TRUE
    } else if (endpoints == "end") {
        end <- TRUE
    } else if (endpoints == "neither") {
        # Do nothing
    } else {
        warning("'endpoints' must be 'both', 'start', 'end', or 'neither'! Defaulting to 'both'")
        start <- TRUE
        end <- TRUE
    }
    if (start) {
        if (x[1] < x[2]) {
            mina <- c(1, mina)
        }
    }
    if (end) {
        if (x[length(x)] < x[length(x)-1]) {
            mina <- c(mina, length(x))
        }
    }
    return(mina)
}