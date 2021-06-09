source("/home/nfox/projects/libraries/r_library/singlecellexperiment/sce_utility.R")
#' Generate sorted expression matrix for stacked violin plot
#' 
#' Given a SingleCellExperiment and a list of markers, it sorts
#' the markers by "cluster with maximum expression" and returns
#' an expression matrix with only those markers in the sorted order.
#' This means that the top gene will be maximally expressed in the first
#' cluster and so on.
#' 
#' @param sce_obj SingleCellExperiment object holding the
#'   expression values to be plotted in the \code{logcounts}
#'   assay. The colData dataframe must have the cluster assignment
#'   in the \code{metaclust} column.
#' @param marker_df Dataframe with 2 columns: \code{gene} and
#'   \code{cluster}. All genes in this dataframe will be included
#'   in the stacked violin plot.
#' @return An expression matrix containing only the marker genes, sorted
#'   so that the first genes are markers for the first cluster.
sort_genes <- function(sce_obj, marker_df) {
    working_matrix <- SingleCellExperiment::logcounts(sce_obj)
    working_matrix <- working_matrix[marker_df$gene, ]
    working_matrix <- as.matrix(working_matrix)
    rownames(working_matrix) <- marker_df$gene
    cell_groups <- as.numeric(SingleCellExperiment::colData(sce_obj)$metaclust)
    groups <- sort(as.numeric(unique(cell_groups)))
    group_means <- t(apply(working_matrix, 1,
                           function(x) sapply(split(x, cell_groups), mean)
                     )
                   )    
    max_clusters <- apply(group_means, 1, which.max)
    working_matrix <- working_matrix[order(max_clusters), ]
    working_matrix
}

#' Generate dataframe for feeding to ggplot
#' 
#' Take a SingleCellExperiment object and a dataframe
#' listing markers to display and construct a dataframe
#' that can be fed to ggplot for building a stacked violin
#' plot.
#' 
#' @param sce_obj SingleCellExperiment object holding the
#'   expression values to be plotted in the \code{logcounts}
#'   assay. The colData dataframe must have the cluster assignment
#'   in the \code{metaclust} column.
#' @param marker_df Dataframe with 2 columns: \code{gene} and
#'   \code{cluster}. All genes in this dataframe will be included
#'   in the stacked violin plot.
#' @param sort If true, then genes will be sorted according to cluster
#'   with maximum mean expression.
#' @param show_cluster If NULL, then the gene names will be shown. If
#'   !NULL, then the value of show_cluster will be printed after the
#'   gene name, then the value of column 2 of marker_df will be appended
#'   and used as row labels. Example: If show_cluster = ": Cluster ",
#'   and column 2 of marker_df are the cluster names c(1:10), then
#'   each row of the violins will be labeled "GENE: Cluster CLUSTERNAME".
#' @param clusters ordered vector of clusters to ensure consistent
#'   coloring across multiple plots
#' @param show_missing_clusters If true, includes a single 0 point for
#'   clusters that are not represnted in the data. Method argument
#'   clusters must not be NULL if show_missing_clusters is TRUE.
#' @return A new dataframe ready to be plugged into ggplot.
generate_dataframe <- function(sce_obj, marker_df,
                               sort = TRUE, show_cluster = NULL,
                               clusters = NULL, show_missing_clusters = FALSE) {
    rownames(marker_df) <- marker_df$gene
    if (sort) {
        working_matrix <- sort_genes(sce_obj, marker_df)
    } else {
        working_matrix <- SingleCellExperiment::logcounts(sce_obj)
        working_matrix <- working_matrix[marker_df$gene, ]
        working_matrix <- as.matrix(working_matrix)
        rownames(working_matrix) <- marker_df$gene
    }
    if (!is.null(show_cluster)) {
        marker_df <- marker_df[rownames(working_matrix), ]
        genes <- apply(marker_df, 1, function(x) paste(x[1],
                                                       show_cluster,
                                                       x[2],
                                                       sep = ""))
    } else {
        genes <- rownames(working_matrix)
    }
    n_genes <- length(genes)
    n_cells <- dim(working_matrix)[2]

    cell_data <- SingleCellExperiment::colData(sce_obj)

    cell <- rep(cell_data$Barcode,
                n_genes)
    if (is.null(clusters)) {
        type <- factor(rep(cell_data$metaclust,
                           n_genes),
                       levels = sort(as.numeric(unique(cell_data$metaclust))),
                       ordered = TRUE)
    } else {
        type <- factor(rep(cell_data$metaclust,
                           n_genes),
                       levels = clusters, 
                       ordered = TRUE)
    }
    gene <- factor(c(sapply(genes, function(x) rep(x, n_cells))),
                   levels = genes,
                   ordered = TRUE)
    expr <- c(t(working_matrix))
    new_df <- data.frame("cell" = cell,
                         "type" = type,
                         "gene" = gene,
                         "expr" = expr,
                         stringsAsFactors = FALSE)
    if (show_missing_clusters) {
        missing_levels <- levels(type)[!(levels(type) %in% type)]
        if (length(missing_levels) != 0) {
            filler_cell <- rep("FILLER", n_genes * length(missing_levels))
            filler_type <- factor(rep(missing_levels, n_genes),
                                  levels = clusters,
                                  ordered = TRUE)
            filler_gene <- factor(c(sapply(genes, function(x) rep(x, length(missing_levels)))),
                                  levels = genes,
                                  ordered = TRUE)
            filler_expr <- rep(0, n_genes * length(missing_levels))
            filler_df <- data.frame("cell" = filler_cell,
                                    "type" = filler_type,
                                    "gene" = filler_gene,
                                    "expr" = filler_expr,
                                    stringsAsFactors = FALSE)
            new_df <- rbind(new_df, filler_df)
        }
    }
    new_df
}

#' Build color palette for hierarchical cell types
#'  
#' For building a stacked violin plot where the cell types fall into
#' broad categories. For example, the first 3 cell types are all of
#' broad category 1, the next 2 are of broad category 2, and the last
#' is in broad category 3. In this case, the first three colors would
#' be shades of red, the second two would be shades of purple, and the
#' last would be a shade of orange. Thus the different broad categories
#' are visualizable, but the subtypes are also distinguishable. The
#' \code{mapping} argument in this case would be \code{c(1, 1, 1, 2, 2, 3)}.
#' 
#' @param mapping numeric vector of broad category assignments for the
#'   cell types.
#' @return color palette for a sorted \code{mapping}.
build_hierarchical_color_palette <- function(mapping) {
    mapping <- sort(mapping)
    mapping <- factor(mapping,
                      levels = unique(mapping),
                      ordered = TRUE)
    if (length(levels(mapping)) > 6) {
        stop("Too many categories! Max is 6.")
    }
    if (max(as.numeric(names(summary(mapping)))) > 7) {
        stop("Too many members of a category! Max is 7.")
    }
    color_pals <- c("Reds",
                    "Purples",
                    "Oranges",
                    "Greys",
                    "Greens",
                    "Blues")
    palette <- c()
    for (i in 1:length(levels(mapping))) {
        n <- summary(mapping)[i]
        if (n == 1) {
            palette <- c(palette, 
                         RColorBrewer::brewer.pal(9, name = color_pals[i])[6])
        } else if (n == 2) {
            palette <- c(palette,
                         RColorBrewer::brewer.pal(9, name = color_pals[i])[5:6])
        } else if (n %in% c(3, 4, 5, 6)) {
            palette <- c(palette,
                         RColorBrewer::brewer.pal(9, name = color_pals[i])[4:(n+3)])
        } else {
            palette <- c(palette,
                         RColorBrewer::brewer.pal(9, name = color_pals[i])[3:9])
        }
    }
    palette
}

#' Build color palette for categorical cell types
#'  
#' Creates a collection of colors to plot categorical data,
#' where there shouldn't be any indication of order or scale.
#' Can theoretically provide an infinitely large color palette,
#' however there are losses. This method can provide up to
#' 12 colors from a single palette. 13-17 colors come from
#' two palettes appended together. All 17 colors are technically
#' unique, but the design is not ideal and there are several
#' pairs that are difficult to distinguish. With 18 or more
#' colors, the 12-color palette simply repeats as necessary,
#' creating duplicate colors because in this intended use case,
#' (for a stacked violin plot), the violins will occur all in
#' a row and should be visually distinguishable, even if not
#' uniquely colored.
#'
#' @param n Number of colors needed
#' @return color palette for a sorted \code{mapping}.
build_categorical_color_palette <- function(n) {
    base_palette <- (c(RColorBrewer::brewer.pal(9, name = "Set1"),
                       RColorBrewer::brewer.pal(8, name = "Set2"))[1:n])
    base_palette[6] <- "#E0E010"
    if (n <= 17) {
        palette <- base_palette[1:n]
    } else {
        warning("Repeating colors...")
        palette <- rep(base_palette,
                       as.integer(n / 17) + 1)
        palette <- palette[1:n]
    }
    palette
}

#' Build a stacked violin ggplot
#' 
#' Take a SingleCellExperiment object with expression values
#' in the \code{logcounts} assay, and a 2-column dataframe
#' of markers and generate a stacked violin plot, showing
#' the expression distributions per cluster per gene.
#' 
#' @param sce_obj SingleCellExperiment object holding the
#'   expression values to be plotted in the \code{logcounts}
#'   assay. The colData dataframe must have the cluster assignment
#'   in the \code{metaclust} column.
#' @param marker_df Dataframe with 2 columns: \code{gene} and
#'   \code{cluster}. All genes in this dataframe will be included
#'   in the stacked violin plot.
#' @param mapping numeric vector of broad category assignments for the
#'   cell types. If the cell types also have parent categories, this
#'   vector should hold them. For example, for 5 cell types in 3
#'   broad categories, \code{mapping} might be \code{c(1, 1, 2, 3, 3)}.
#' @param sort If true, then genes will be sorted according to cluster
#'   with maximum mean expression.
#' @param show_cluster If NULL, then the gene names will be shown. If
#'   !NULL, then the value of show_cluster will be printed after the
#'   gene name, then the value of column 2 of marker_df will be appended
#'   and used as row labels. Example: If show_cluster = ": Cluster ",
#'   and column 2 of marker_df are the cluster names c(1:10), then
#'   each row of the violins will be labeled "GENE: Cluster CLUSTERNAME".
#' @param clusters ordered vector of clusters to ensure consistent
#'   coloring across multiple plots
#' @param show_missing_clusters If true, includes a single 0 point for
#'   clusters that are not represnted in the data. Method argument
#'   clusters must not be NULL if show_missing_clusters is TRUE.
#' @return A ggplot object holding the stacked violin plot.
build_stacked_violin_plot <- function(sce_obj, marker_df, mapping = NULL,
                                      sort = TRUE, show_cluster = NULL,
                                      clusters = NULL, show_missing_clusters = FALSE) {
    if (show_missing_clusters && is.null(clusters)) {
        stop("If show_missing_clusters is TRUE, clusters cannot be NULL")
    }
    data_df <- generate_dataframe(sce_obj, marker_df, sort, show_cluster,
                                  clusters, show_missing_clusters)
    if (is.null(mapping)) {
        if (is.null(clusters)) {
            n_cell_groups <- SingleCellExperiment::colData(sce_obj)$metaclust
            n_cell_groups <- length(sort(as.numeric(unique(n_cell_groups))))
            palette <- build_categorical_color_palette(n_cell_groups)
        } else {
            palette <- build_categorical_color_palette(length(clusters))
        }
    } else {
        palette <- build_hierarchical_color_palette(mapping)
    }
    p <- (
        ggplot2::ggplot(data = data_df)
        + ggplot2::geom_violin(mapping = ggplot2::aes(x = type,
                                                      y = expr, 
                                                      fill = type),
                                                      # size = 0.5),
                               show.legend = FALSE,
                               scale = "width")
        + ggplot2::stat_summary(mapping = ggplot2::aes(x = type,
                                                       y = expr),
                                fun.y = "mean",
                                size = 0.5,
                                geom = "point")
        + ggplot2::scale_x_discrete(position = "top")
        + ggplot2::scale_y_continuous(limits = c(0, NA),
                                      expand = ggplot2::expand_scale(mult = c(0, 0.05)),
                                      position = "right",
                                      breaks = function(x) floor(x[2]))
        + ggplot2::facet_grid(rows = ggplot2::vars(gene),
                              scales = "free_y",
                              switch = "y")
        + ggplot2::theme_classic()
        + ggplot2::xlab("Cell Type\n")
        + ggplot2::ylab(NULL)
        + ggplot2::theme(panel.grid = ggplot2::element_blank(),
                         panel.background = ggplot2::element_rect(fill = "#f0f0f0"),
                         strip.background = ggplot2::element_rect(color = "white"),
                         strip.text.y = ggplot2::element_text(angle = 180, hjust = 1),
                         axis.line.y.right = ggplot2::element_blank(),
                         axis.ticks.y = ggplot2::element_blank(),
                         axis.text.y = ggplot2::element_text(size = 7)
                        )
    )
    if (is.null(clusters)) {
        p <- (
              p
              + ggplot2::scale_fill_manual(values = palette)
             )
    } else {
        p <- (
              p
              + ggplot2::scale_fill_manual(values = palette,
                                           limits = levels(data_df$type))
             )
    }
    p
}

#' Build a colored scatterplot ggplot object
#' 
#' Create a 2-D scatterplot from a SingleCellExperiment object.
#' Intended for plotting low-dimensional embeddings of scRNA-seq
#' data, colored by batch or cluster or gene expression.
#'         
#' @param sce_obj SingleCellExperiment object to be plotted.
#' @param x x coordinates for plotting. Can be a character string in
#'   \code{colnames(colData(sce_obj))} or a numeric vector with the
#'   same length as \code{dim(sce_obj)[2]}.
#' @param y y coordinates for plotting. Can be a character string in
#'   \code{colnames(colData(sce_obj))} or a numeric vector with the
#'   same length as \code{dim(sce_obj)[2]}.
#' @param color If null, all points will be black. If character string,
#'   must be a column in \code{colData(sce_obj)} or a gene in
#'   \code{rownames(sce_obj)}. If color is a gene, then expression will
#'   be taken from the logcounts assay.
#' @param palette If NULL, will automatically pick colors with
#'   \code{build_categorical_color_palette()}. Otherwise will use
#'   palette (string vector of colors) to color points.
#' @param alpha_highlight A list of 2 character vectors. The first
#'   character vector must be of length 1 and hold a column name from
#'   \code{colData(sce_obj)}. The second character vector must be
#'   a vector of factor levels in the column indicated by the first
#'   vector. These levels will be plotted at full intensity. All
#'   other levels will be plotted at \code{alpha_min} transparency.
#' @param alpha_min The transparency alpha value that will be used
#'   for all levels of \code{alpha_highlight[[1]][1]} not indicated
#'   by \code{alpha_highlight[[2]]}.
#' @param plot_title Character string for plot title. "Cells" by default.
#' @param legend_title Character string for legend title.
#'   Default is \code{color}.
#' @param show_legend Logical indicating whether or not to include a legend.
#' @return A ggplot object holding the scatter plot.
scatter_plot_sce <- function(sce_obj, x, y, color = NULL, palette = NULL,
                             alpha_highlight = NULL, alpha_min = 0.3,
                             plot_title = "Cells", legend_title = NULL,
                             show_legend = TRUE) {
    data_df <- data.frame(SingleCellExperiment::colData(sce_obj))
    if (is.character(x)) {
        if (!(x %in% colnames(data_df))) {
            stop(paste(x, "is not a valid column name in colData(sce_obj)", sep = " "))
        }
    } else {
        if (!(is.vector(x) || !(is.numeric(x)))) {
            stop("x must be a numeric vector")
        } else if (length(x) != dim(data_df)[1]) {
            stop("x must have same length as number of cells in sce_obj")
        }
    }
    if (is.character(y)) {
        if (!(y %in% colnames(data_df))) {
            stop(paste(y, "is not a valid column name in colData(sce_obj)", sep = " "))
        }
    } else {
        if (!(is.vector(y) || !(is.numeric(y)))) {
            stop("y must be a numeric vector")
        } else if (length(y) != dim(data_df)[1]) {
            stop("y must have same length as number of cells in sce_obj")
        }
    }
    if (!is.null(palette)) {
        if (is.null(color)) {
            stop("palette cannot be set if color is NULL!")
        } 
        if (length(palette) != length(unique(data_df[, color]))) {
            stop("palette must have same number of colors as levels in color factor.")
        }
    }
    if (!(is.null(alpha_highlight))) {
        if (length(alpha_highlight) != 2) {
            stop("alpha_highlight must be a list of 2 character vectors")
        }
        if (length(alpha_highlight[[1]]) != 1) {
            stop("alpha_highlight[[1]][1] must be a single character string")
        }
        if (!(alpha_highlight[[1]][1] %in% colnames(data_df)) | !is.factor(data_df[, alpha_highlight[[1]][1]])) {
            stop("alpha_highlight[[1]][1] must be a factor in colData(sce_obj)")
        }
        if (!(any(alpha_highlight[[2]] %in% levels(data_df[, alpha_highlight[[1]][1]])))) {
            stop("alpha_highlight[[2]] must be a character vector of levels in colData(sce_obj)[, alpha_highlight[[1]][1]]")
        }
        alpha <- rep(alpha_min, dim(data_df)[1])
        filter <- data_df[, alpha_highlight[[1]][1]] %in% alpha_highlight[[2]]
        alpha[filter] <- 1
    } else {
        alpha <- rep(1, dim(data_df)[1])
    }
    if (!(is.null(color))) {
        color_var <- color
        if (!(color %in% colnames(data_df))) {
            if (color %in% rownames(sce_obj)) {
                color <- as.matrix(SingleCellExperiment::logcounts(sce_obj[color, ]))[color, ]
            } else {
                stop("color must be column name of colData(sce_obj) or row name of sce_obj")
            }
        }
        p <- (
            ggplot2::ggplot(data = data_df)
            + ggplot2::geom_point(ggplot2::aes_string(x = x, y = y, color = color),
                                  size = 0.5, show.legend = show_legend, alpha = alpha)
            + ggplot2::theme_classic()
            + ggplot2::labs(title = plot_title)
            + ggplot2::theme(axis.text = ggplot2::element_blank(),
                             axis.ticks = ggplot2::element_blank(),
                             axis.title = ggplot2::element_blank(),
                             axis.line = ggplot2::element_blank()
                            )
        )
        if (is.factor(data_df[, color])) {
            if (is.null(palette)) {
                pal <- build_categorical_color_palette(length(levels(data_df[, color])))
                if (length(pal) > 17) {
                    stop(paste("More color levels than available colors from ", 
                               "build_categorical_color_palette. Please pass ",
                               "palette manually via palette argument."))
                }
            } else {
                pal <- palette
            }
            p <- p + ggplot2::scale_colour_manual(values = pal)
            if (is.null(legend_title)) {
                p <- p + ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 2)))
            } else {
                p <- p + ggplot2::guides(colour = ggplot2::guide_legend(title = legend_title,
                                                                        override.aes = list(size = 2)))
            }        
        } else {
            p <- p + ggplot2::scale_colour_distiller(palette = "Blues",
                                                     direction = 1)
            if (is.null(legend_title)) {
                p <- p + ggplot2::guides(colour = ggplot2::guide_colourbar(title = color_var),
                                                                           draw.llim = FALSE)
            } else {
                p <- p + ggplot2::guides(colour = ggplot2::guide_colourbar(title = legend_title),
                                                                           draw.llim = FALSE)
            }
        }
    } else {
        p <- (
            ggplot2::ggplot(data = data_df)
            + ggplot2::geom_point(ggplot2::aes_string(x = x, y = y),
                         size = 0.5, color = "black")
            + ggplot2::theme_classic()
            + ggplot2::labs(title = plot_title)
            + ggplot2::theme(axis.text = ggplot2::element_blank(),
                    axis.ticks = ggplot2::element_blank(),
                    axis.title = ggplot2::element_blank(),
                    axis.line = ggplot2::element_blank()
                   )
        )
        
    }
    p
}

#' Build two panel plots of marker expression and clusters
#'         
#' Creates and saves to disk a collection of plots, each a
#' 2-panel plot of a 2-D embedding of a SingleCellExperiment
#' object. The left panel is colored by the expression of a
#' single gene, the right plot is colored by cluster for
#' easy comparison. All plots are saved as PNG files to
#' a given folder.
#'         
#' @param sce_obj The SingleCellExperiment object to be plotted
#' @param markers A character vector of gene names. One 2-panel
#'   plot will be generated per marker.
#' @param x Either a column name in \code{colData(sce_obj)} or
#'   a numeric vector for the x-dimension of the 2-D embedding.
#' @param y Same as x, but the y-dimension.
#' @param color If null, all points will be black. If character string,
#'   must be a column in \code{colData(sce_obj)} or a gene in
#'   \code{rownames(sce_obj)}. If color is a gene, then expression will
#'   be taken from the logcounts assay.
#' @param palette If NULL, will automatically pick colors with
#'   \code{build_categorical_color_palette()}. Otherwise will use
#'   palette (string vector of colors) to color points.
#' @param plot_title A string to be appended to the end of every
#'   plot title. Can be a single static string, or a vector
#'   of \code{length(markers)}. If \code{NULL}, the title will just be the
#'   name of the marker gene.
#' @param save If \code{TRUE}, will save all plots to disk
#'   in the location indicated by \code{path}.
#' @param path A string indicating the folder in which the
#'   PNG files will be saved. Must exist at time of function call.
#' @param w Width of plots in inches. Default is the size of a
#'   widescreen PowerPoint slide.
#' @param h Height of plots in inches. Default is the size of a
#'   widescreen PowerPoint slide.
#' @param return_plots If \code{TRUE}, will return the collection
#'   of plots as a list. Otherwise returns \code{NULL}
#' @param order_files If \code{TRUE}, will add a 4 digit counter
#'   to the front of each file name so that order is preserved.
#' @param create_pdf If \code{TRUE}, will also generate a PDF concatenating
#'   all PNG files generated.
#' @param keep_images If \code{FALSE}, PNG files will be deleted after
#'   concatenation into PDF file.
#' @return A list of cowplot lists if return_plots is TRUE, NULL otherwise.        
build_marker_expression_slides <- function(sce_obj, markers,
                                           x, y, color = NULL, palette = NULL,
                                           plot_title = NULL,
                                           save = TRUE, path = NULL,
                                           w = 13.333, h = 7.5,
                                           return_plots = FALSE,
                                           order_files = FALSE,
                                           create_pdf = FALSE,
                                           keep_images = TRUE,
                                           pdf_title = "marker_expr.pdf") {
    if (save) {
        if (is.null(path)) {
            path <- getwd()
        } else {
            if (!dir.exists(path)) {
                stop("path must be a valid directory!")
            }
        }
    } else {
        if (!(is.null(path))) {
            warning("path is not NULL, but plots are not being saved to disk.")
        }
        if (!return_plots) {
            stop("save and return_plots cannot both be FALSE.")
        }
        if (create_pdf || keep_images) {
            stop("save must be TRUE to set create_pdf or keep_images!")
        }
    }
    if (any(!(markers %in% rownames(sce_obj)))) {
        stop("markers must be in row names of sce_obj!")
    }
    if (!is.null(plot_title)) {
        if (length(plot_title) == 1) {
            plot_title = rep(plot_title, length(markers))
        } else if (length(plot_title) != length(markers)) {
            stop("if not NULL, plot_title must be length 1 or length(markers)")
        }
    }        
    if (save && (!create_pdf && !keep_images)) {
        stop(paste("create_pdf and keep_images cannot both be false,",
                   "then no files would be generated!"))
    }
    all_plots <- list()
    cat("Generating images...\n")
    filenames <- character(length(markers))
    for (i in 1:(length(markers))) {
        marker <- markers[i]
        if (is.null(plot_title)) {
            p_title <- marker
        } else {
            p_title <- paste(marker, plot_title[i], sep = "")
        }
        p1 <- scatter_plot_sce(sce_obj,
                               x = x,
                               y = y,
                               color = marker,
                               plot_title = p_title,
                               legend_title = "")
        p2 <- scatter_plot_sce(sce_obj,
                               x = x,
                               y = y,
                               color = color,
                               palette = palette,
                               plot_title = "",
                               legend_title = "Cluster")
        plots <- list(numeric(2))
        plots[[1]] <- p1
        plots[[2]] <- p2
        p_list <- cowplot::plot_grid(plotlist = plots, align = "hv",
                                     nrow = 1, ncol = 2, axis = "lb")
        if (save) {
            if (order_files) {
                filename <- paste(sprintf("%04d_", i),
                                  gsub(" ", "_", p_title),
                                  "_expression.png",
                                  sep = "")
            } else {
                filename <- paste(gsub(" ", "_", p_title),
                                  "_expression.png",
                                  sep = "")
            }
            ggplot2::ggsave(filename,
                            plot = p_list, path = path,
                            device = "png", width = w, height = h, units = "in")
            filenames[i] <- filename
        }
        if (return_plots) {
            all_plots <- append(all_plots, p_list)
        }
    }
    if (save) {
        if (create_pdf) {
            cat("Converting images to pdf...\n")
            backup_path <- getwd()
            setwd(path)
            system2("convert", args = c(paste("*.png ", pdf_title, sep = "")))
            if (!keep_images) {
                cat("Removing images...\n")
                system2("rm", filenames)
            }
            setwd(backup_path)
        }
    }
    if (return_plots) {
        return(all_plots)
    }
}
        
# Histogram for metadata
        
metadata_histogram <- function(sce_obj, x,
                               row_metadata = FALSE,
                               title = NULL, x_label = NULL,
                               bins = NULL, binwidth = NULL,
                               mean = TRUE, median = TRUE) {
    cell_data <- get_cell_data(sce_obj)
    gene_data <- get_gene_data(sce_obj)
    if (x %in% colnames(cell_data)
     && x %in% colnames(gene_data)) {
        if (row_metadata) {
            warning("x is in both rowData and colData. Using rowData. Unset row_metadata to change.")
            data <- gene_data
            datatype <- "Genes"
        } else {
            warning("x is in both rowData and colData. Using colData. Set row_metadata to change.")
            data <- cell_data
            datatype <- "Cells"
        }
    } else if (x %in% colnames(cell_data)) {
        data <- cell_data
        datatype <- "Cells"
    } else if (x %in% colnames(gene_data)) {
        data <- gene_data
        datatype <- "Genes"
    } else {
        stop("x must be a column in either colData(sce_obj) or rowData(sce_obj)")
    }
    if (!is.null(bins) && !is.null(binwidth)) {
        stop("bins and binwidth cannot both be non-NULL")
    }
    if (is.null(title)) {
        title <- sprintf("%s by %s", datatype, x)
    }
    if (is.null(x_label)) {
        x_label <- x
    }
    plot_obj <- (
        ggplot2::ggplot(data = data)
        + ggplot2::geom_histogram(ggplot2::aes_string(x = x), bins = bins, binwidth = binwidth)
        + ggplot2::labs(x = x_label, y = "Counts",
                        title = title)
    ) 
    if (mean && median) {
        plot_obj <- (
            plot_obj
            + ggplot2::geom_vline(ggplot2::aes(xintercept = mean(data[, x]),
                                               colour = "mean"),
                                  size = 0.25)
            + ggplot2::geom_vline(ggplot2::aes(xintercept = median(data[, x]),
                                               colour = "median"),
                                  size = 0.25)
            + ggplot2::scale_colour_manual(name = "", labels = c(sprintf("mean    = %.0f", mean(data[, x])),
                                                                 sprintf("median = %.0f", median(data[, x]))),
                                           values = c(mean = "red", median = "blue"))
        )
    } else if (mean) {
        plot_obj <- (
            plot_obj
            + ggplot2::geom_vline(ggplot2::aes(xintercept = mean(data[, x]),
                                               colour = "mean"),
                                  size = 0.25)
            + ggplot2::scale_colour_manual(name = "", labels = c(sprintf("mean = %.0f", mean(data[, x]))),
                                           values = c(mean = "red"))
        )
    } else if (median) {
        plot_obj <- (
            plot_obj
            + ggplot2::geom_vline(ggplot2::aes(xintercept = median(data[, x]),
                                               colour = "median"),
                                  size = 0.25)
            + ggplot2::scale_colour_manual(name = "", labels = c(sprintf("median = %.0f", median(data[, x]))),
                                           values = c(median = "blue"))
        )
    }
    return(plot_obj)
}
        
# Display colors for palette. Palette is a character vector of colors.
     
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