#' Set the number of cores.
#'
#' Sets the number of cores option that will be used
#' by functions that check it. i.e. \code{mc.cores}
#'
#' @param number_of_cores The number of cores to set.
#' @return The number of cores successfully set.
#' @export
set_n_cores <- function(number_of_cores) {
    if (!(number_of_cores > 0)) {
        stop("number_of_cores must be in [1, MAX_CORES_AVAILABLE]")
    }
    options(mc.cores = number_of_cores)
    return(number_of_cores)
}

#' Change the default plot size.
#'
#' Changes the default plot size. Useful for in-line display
#' as in Jupyter Lab. Has no relation to the size of saved
#' plot image files.
#'
#' @param width The width in inches.
#' @param height The height in inches. If height is NULL, a square
#'   aspect ratio is set with \code{width} side length.
#' @return Nothing.
#' @export
change_plot_size <- function(width = 7, height = NULL) {
    if (!(width > 0)) {
        stop("width must be > 0")
    }
    if (!(is.null(height)) && !(height > 0)) {
        stop("height must be > 0")
    }
    if (is.null(height)) {
        options(repr.plot.width = width,
                repr.plot.height = width)
    } else {
        options(repr.plot.width = width,
                repr.plot.height = height)
    }
}

#' Convert a vector to a pretty-printable string.
#'
#' Converts a vector to a string that can be printed. The vector
#' will be printed  in columns where the column width is dynamically
#' chosen based on the longest element in the vector.
#'
#' @param vec The vector to be printed.
#' @param number_of_columns The number of columns to print the vector
#'   with.
#' @param indent A string to be printed before every row.
#' @return The pretty-printable string. Best passed to \code{cat}.
#' @export
pretty_print_vector <- function(vec, number_of_columns = 1,
                                indent = "    ") {
    if (!(number_of_columns > 0)) {
        stop("number_of_columns must be > 0")
    } else if (number_of_columns > length(vec)) {
        stop("number_of_columns must be < length(vec)")
    }
    
    # Save computation in special case empty vector
    if (length(vec) == 0) {
        final_str <- "\n" 
        return(final_str)
    }
    
    final_str <- ""
    vec <- as.character(vec)
    element_width <- max(sapply(vec, nchar))
    n_complete_rows <- floor(length(vec) / number_of_columns)
    n_remainder_elements <- length(vec) %% number_of_columns
    
    for (i in 1:n_complete_rows) {
        final_str <- paste(final_str,
                           indent,
                           sep = "")
        for (j in (number_of_columns - 1):0) {
            final_str <- paste(final_str,
                               sprintf("%*s",
                                       element_width,
                                       vec[i * number_of_columns - j]),
                               sep = "")
            if (j != 0) {
                final_str <- paste(final_str,
                                   indent,
                                   sep = "")
            }
        }
        final_str <- paste(final_str,
                           "\n",
                           sep = "")
    }
    if (n_remainder_elements != 0) {
        final_str <- paste(final_str,
                           indent,
                           sep = "")
        for (j in (remainder_elements - 1):0) {
            final_str <- paste(final_str,
                               sprintf("%*s",
                                       element_width,
                                       vec[length(vec) - j]),
                               sep = "")
            if (j != 0) {
                final_str <- paste(final_str,
                                   indent,
                                   sep = "")
            }
        }
        final_str <- paste(final_str,
                           "\n",
                           sep = "")
    }
    return(final_str)
}

#' Convert n seconds to DD:HH:MM:SS string.
#'
#' Converts a numeric number of seconds to a string containing that time
#' in DD:HH:MM:SS format (D = days, H = hours, M = minutes, S = seconds).
#'
#' @param seconds The number of seconds to convert.
#' @param digits The number of digits after the decimal point to keep.
#' @param strip_zeros If TRUE, then leading 00: fields will be removed.
#' @return The string representing the converted time.
#' @export
convert_sec_to_dhms <- function(seconds, digits = 0, strip_zeros = FALSE) {
    stopifnot(is.numeric(seconds))
    
    # d, h, m, s refers to DAYS, HOURS, MINUTES, SECONDS
    d <- seconds / ((60 ^ 2) * 24)
    h <- (d - (d <- floor(d))) * 24
    m <- (h - (h <- floor(h))) * 60
    s <- (m - (m <- floor(m))) * 60
    
    format <- "%02d:%02d:%02d:%0"
    format_digits <- if (digits <- abs(digits)) {
        paste0(digits + 3, ".", digits, "f")
    } else {
        "2d"
    }
    out <- sprintf(paste(format, format_digits, sep = ""),
                   d, h, m, if (!digits) round(s) else s)
    out[grep("NA", out, ignore.case = TRUE)] <- NA
    if (strip_zeros) {
        out <- sub("^(00:){1,}", "", out)
    }
    return(out)
}
     
#' Display a color palette.
#'
#' Plots a vector of colors as a stack of bars in a ggplot plot.
#' How good this looks is heavily dependent on the plot size and
#' number of colors. Intended for quick visualization, not for
#' reliably producing palette representations that look good.
#'
#' @param palette A vector of colors, typically as hex strings.
#' @return A ggplot object showing the colors in the palette.
#' @export
display_palette <- function(palette) {
    n_bars <- length(palette)
    df <- data.frame("x" = factor(as.character(1:n_bars), levels = rev(1:n_bars)),
                     "y" = rep(0.2, n_bars),
                     "color" = palette,
                     "text_y" = rep(0.3, n_bars))

    plt <- (
        ggplot2::ggplot(data = df)
        + ggplot2::geom_col(ggplot2::aes(x = x, y = y, fill = x),
                            show.legend = FALSE)
        + ggplot2::geom_text(ggplot2::aes(x = x, y = text_y, label = color),
                             family = "mono", show.legend = FALSE)
        + ggplot2::scale_y_continuous(limits = c(0, 0.5), expand = ggplot2::expand_scale(mult = c(0, 0)))
        + ggplot2::coord_flip()
        + ggplot2::scale_fill_manual(values = rev(palette))
        + ggplot2::theme_classic()
        + ggplot2::theme(axis.title = ggplot2::element_blank(),
                         axis.text = ggplot2::element_blank(),
                         axis.ticks = ggplot2::element_blank(),
                         axis.line = ggplot2::element_blank())
    )
    plt
}
                   
cat_split <- function(..., file = "", sep = " ", fill = FALSE,
                      labels = NULL, append = FALSE) {
    cat(..., file = "", sep = sep, fill = fill,
        labels = labels, append = append)
    if (file != "") {
        cat(..., file = file, sep = sep, fill = fill,
            labels = labels, append = append)
    }
}
