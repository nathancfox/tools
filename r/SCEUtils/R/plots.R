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
#' @export
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
#' @param n Number of colors needed. If omitted, the max number
#'   is returned.
#' @param colorgorical If TRUE, uses a longer palette from Colorgorical.
#'   else, uses a modified combination of RColorBrewer Set1 and Set2.
#' @return categorical color palette.
#' @export
build_categorical_color_palette <- function(n = NULL, colorgorical = FALSE) {
    if (is.null(n)) {
        n <- 17
    }
    if (colorgorical) {
        base_palette <- c("#35618f", "#68c3ef", "#3330b7", "#f3c5fa",
                          "#ab2378", "#2eece6", "#0b5313", "#97d864",
                          "#39970e", "#0df38f", "#738a69", "#cdd9b8",
                          "#7c440e", "#fa756b", "#b42716", "#f79302",
                          "#cd8f72", "#f4d403", "#8184fb", "#6e417b",
                          "#fa7ee3", "#6108e8", "#f62ef3", "#34f50e")
    } else {
        base_palette <- (c(RColorBrewer::brewer.pal(9, name = "Set1"),
                           RColorBrewer::brewer.pal(8, name = "Set2"))[1:n])
        base_palette[6] <- "#E0E010"
    }
    if (n <= length(base_palette)) {
        palette <- base_palette[1:n]
    } else {
        warning("Repeating colors...")
        palette <- rep(base_palette,
                       as.integer(n / length(base_palette)) + 1)
        palette <- palette[1:n]
    }
    palette
}

#' Build a categorical color palette for large clusters.
#'
#' Builds a categorical color palette for clusters larger
#' than a certain threshold. Clusters smaller than the
#' threshold get colored black by default.
#'
#' @param clusters A factor holding the cluster assignments
#'   for each cell. The palette order corresponds to the factor
#'   level order. This is ideally the \code{clusters} member of
#'   a clustering results object.
#' @param threshold The minimum number of cells a cluster must
#'   have to be assigned a color. Default is 5.
#' @param omitted_color. A hex string indicating the color to use
#'   for clusters that don't meet the threshold. Default is black.
#' @return A vector of hex colors the same length as the number
#'   of levels in \code{clusters}. The order of colors matches
#'   the order of the levels in \code{clusters}.
#' @export
build_thresholded_cluster_palette <- function(clusters, threshold = 5,
                                              omitted_color = "#000000") {
    color_opts <- build_categorical_color_palette()
    n_colors <- length(color_opts)
    cluster_names <- levels(clusters)
    palette <- character(length(cluster_names))
    counter <- 1
    for (i in seq_along(palette)) {
        if (sum(clusters == cluster_names[i]) >= threshold) {
            if (counter > length(color_opts)) {
                stop("More valid clusters than colors available!")
            }
            palette[i] <- color_opts[counter]
            counter <- counter + 1
        } else {
            palette[i] <- omitted_color
        }
    }
    return(palette)
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
#'   clusters that are not represented in the data. Method argument
#'   clusters must not be NULL if show_missing_clusters is TRUE.
#' @return A ggplot object holding the stacked violin plot.
#' @export
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
#' @param na_trans If color is a discrete scale, this is passed to na.trans
#'   in \code{ggplot2::scale_colour_manual()}.
#' @param na_val If color is a discrete scale, this is passed to na.value
#'   in \code{ggplot2::scale_colour_manual()}.
#' @param size Will be passed to ggplot2::geom_point(ggplot2::aes_string)
#' @param cont_color_trans trans argument for the continuous color distiller
#'   scale used by ggplot. Example: pass "log10" to do a log10 transformation
#'   on the continuous color variable.
#' @param palette If NULL, will automatically pick colors with
#'   \code{build_categorical_color_palette()}. Otherwise will use
#'   palette (string vector of colors) to color points.
#' @param group_labels If \code{TRUE}, the right plot will have labels
#'   placed at the mean position of each color group.
#' @param group_min_size The minimum size group in the right plot that
#'   will be labeled or colored. Groups smaller than this will be
#'   colored gray and unlabeled.
#' @param surface_density If \code{TRUE}, a surface density plot will be
#'   plotted instead of a marker expression color. Only compatible when
#'   color is a marker gene.
#' @param cont_palette A continuous color palette compatible with
#'   ggplot2. Default is the Blues palette using the distiller function.
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
#' @param shuffle If \code{TRUE}, the cells will be plotted in random order.
#' @return A ggplot object holding the scatter plot.
#' @export
scatter_plot_sce <- function(sce_obj, x, y, color = NULL, na_trans = TRUE, na_val = "#000000",
                             size = 0.5, cont_color_trans = "identity", palette = NULL,
                             cont_palette = ggplot2::scale_colour_distiller(palette = "Blues",
                                                     direction = 1,
                                                     trans = cont_color_trans),

                             alpha_highlight = NULL, alpha_min = 0.3,
                             plot_title = "Cells", legend_title = NULL,
                             show_legend = TRUE, group_labels = FALSE,
                             group_min_size = 1, surface_density = FALSE, shuffle = FALSE) {
    if (shuffle) {
        sce_obj <- sce_obj[, sample(1:(dim(sce_obj)[2]))]
    }
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
        if (is.factor(data_df[, color])) {
            if (length(palette) != length(levels(data_df[, color]))) {
                temp <- unique(data_df[, color])
                temp <- temp[!is.na(temp)]
                if (length(palette) != length(temp)) {
                    stop("palette must have same number of colors as levels in color factor.")
                }
            }
        }
        else {
            if (length(palette) != length(unique(data_df[, color]))) {
                stop("palette must have same number of colors as levels in color factor.")
            }
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
        # alpha <- rep(1, dim(data_df)[1])
        alpha <- 1
    }
    if (!(is.null(color))) {
        # Required to convert single element factors. Factors throw a weird
        # bug when indexing the rownames of the logcounts matrices.
        gene_flag <- FALSE
        color <- as.character(color)
        color_var <- color
        if (!(color %in% colnames(data_df))) {
            if (color %in% rownames(sce_obj)) {
                gene_flag <- TRUE
                if (surface_density) {
                    # color <- "color_col"
                    # data_df$color_col <- "FILL"
                    # data_df$color_col <- as.factor(data_df$color)
                } else {
                    color <- as.matrix(SingleCellExperiment::logcounts(sce_obj[color, ]))[color, ]
                }
            } else {
                stop("color must be column name of colData(sce_obj) or row name of sce_obj")
            }
        }
        if (surface_density) {
            p <- (
                ggplot2::ggplot(data = data_df)
                + ggplot2::geom_point(ggplot2::aes_string(x = x, y = y), color = "black",
                                      size = size, show.legend = show_legend, alpha = alpha)
                + ggplot2::theme_classic()
                + ggplot2::labs(title = plot_title)
                + ggplot2::theme(axis.text = ggplot2::element_blank(),
                                 axis.ticks = ggplot2::element_blank(),
                                 axis.title = ggplot2::element_blank(),
                                 axis.line = ggplot2::element_blank()
                                )
            )
        } else {
            p <- (
                ggplot2::ggplot(data = data_df)
                + ggplot2::geom_point(ggplot2::aes_string(x = x, y = y, color = color),
                                      size = size, show.legend = show_legend, alpha = alpha)
                + ggplot2::theme_classic()
                + ggplot2::labs(title = plot_title)
                + ggplot2::theme(axis.text = ggplot2::element_blank(),
                                 axis.ticks = ggplot2::element_blank(),
                                 axis.title = ggplot2::element_blank(),
                                 axis.line = ggplot2::element_blank()
                                )
            )
        }
        if (!gene_flag && is.factor(data_df[, color])) {
            if (is.null(palette)) {
                pal <- build_categorical_color_palette(length(levels(data_df[, color])))
                if (length(pal) > 17) {
                    pal <- viridis::viridis(length(levels(data_df[, color])))
                    set.seed(42)
                    pal <- sample(pal)
                    # stop(paste("More color levels than available colors from ", 
                    #            "build_categorical_color_palette. Please pass ",
                    #            "palette manually via palette argument."))
                }
                group_counts <- as.vector(table(data_df[, color]))
                pal[group_counts < group_min_size] <- "#8F8F8F"
            } else {
                pal <- palette
            }
            p <- p + ggplot2::scale_colour_manual(values = pal,
                                                  na.translate = na_trans, na.value = na_val)
            if (is.null(legend_title)) {
                p <- p + ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 2)))
            } else {
                p <- p + ggplot2::guides(colour = ggplot2::guide_legend(title = legend_title,
                                                                        override.aes = list(size = 2)))
            }        
            if (group_labels) {
                group_means <- apply(data_df[, c(x, y)], 2,
                                     function(z) {
                                         sapply(split(z, data_df[, color]), mean)
                                     })
                group_means <- data.frame(x = group_means[, 1],
                                          y = group_means[, 2])
                group_counts <- as.vector(table(data_df[, color]))
                group_means <- group_means[group_counts >= group_min_size, ]
                p <- p + ggplot2::geom_label(data = group_means,
                                             ggplot2::aes(x = x, y = y,
                                                          label = rownames(group_means)))
            }
        } else {
            if (surface_density) {
                if (!(color_var %in% rownames(sce_obj))) {
                    warning("surface_density is ignored if color is not a rowname of the SCE object!")
                    p <- p + cont_palette
                    if (is.null(legend_title)) {
                        p <- p + ggplot2::guides(colour = ggplot2::guide_colourbar(title = color_var),
                                                                                   draw.llim = FALSE)
                    } else {
                        p <- p + ggplot2::guides(colour = ggplot2::guide_colourbar(title = legend_title),
                                                                                   draw.llim = FALSE)
                    }
                } else {
                    # p <- p + ggplot2::scale_colour_discrete(c("#000000"))
                    sd_df <- data_df[SingleCellExperiment::logcounts(sce_obj)[color_var, ] > 0, ]
                    p <- p + ggplot2::geom_density_2d_filled(ggplot2::aes(x = x, y = y),
                                                             data = sd_df, alpha = 0.5,
                                                             show.legend = FALSE)
                }
            } else {
                p <- p + cont_palette
                if (is.null(legend_title)) {
                    p <- p + ggplot2::guides(colour = ggplot2::guide_colourbar(title = color_var),
                                                                               draw.llim = FALSE)
                } else {
                    p <- p + ggplot2::guides(colour = ggplot2::guide_colourbar(title = legend_title),
                                                                               draw.llim = FALSE)
                }
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
#' @param group_labels If TRUE, the right plot will have labels placed
#'   at the mean position of each color group.
#' @param group_min_size The minimum size group in the right plot that
#'   will be labeled or colored. Groups smaller than this will be
#'   colored gray and unlabeled.
#' @param surface_density If \code{TRUE}, a surface density plot will
#'   drawn instead of a marker expression plot on the left panel.
#' @param plot_title A string to be appended to the end of every
#'   plot title. Can be a single static string, or a vector
#'   of \code{length(markers)}. If \code{NULL}, the title will just be the
#'   name of the marker gene.
#' @param plot_title_append If \code{TRUE}, then \code{plot_title} will be
#'   appended to the end of the marker name. Otherwise, will replace the
#'   marker name as the title. Can also be a single static string, or a vector
#'   of \code{length(markers)}.
#' @param legend_title If \code{!NULL}, will be used as the legend title
#'   for the right plot.
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
#' @export
build_marker_expression_slides <- function(sce_obj, markers,
                                           x, y, color = NULL, palette = NULL,
                                           group_labels = FALSE, group_min_size = 1,
                                           surface_density = FALSE,
                                           plot_title = NULL, plot_title_append = TRUE,
                                           legend_title = NULL,
                                           save = TRUE, path = NULL,
                                           w = 13.333, h = 7.5,
                                           return_plots = FALSE,
                                           order_files = FALSE,
                                           file_names = NULL,
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
    if (!is.null(file_names)) {
        file_names <- as.character(file_names)
        if (length(file_names) != length(markers)) {
            stop("Number of filenames must equal number of markers!")
        }
    }
    all_plots <- list(numeric(length(markers)))
    cat("Generating images...\n")
    filenames <- character(length(markers))
    for (i in 1:(length(markers))) {
        marker <- markers[i]
        if (is.null(plot_title)) {
            p_title <- marker
        } else if (plot_title_append) {
            p_title <- paste(marker, plot_title[i], sep = "")
        } else {
            p_title <- plot_title[i]
        }
        p1 <- scatter_plot_sce(sce_obj,
                               x = x,
                               y = y,
                               color = marker,
                               surface_density = surface_density,
                               plot_title = p_title,
                               legend_title = "")
        p2 <- scatter_plot_sce(sce_obj,
                               x = x,
                               y = y,
                               color = color,
                               palette = palette,
                               plot_title = "\n\n",
                               legend_title = legend_title,
                               group_labels = group_labels,
                               group_min_size = group_min_size)
        plots <- list(numeric(2))
        plots[[1]] <- p1
        plots[[2]] <- p2
        p_list <- cowplot::plot_grid(plotlist = plots, align = "hv",
                                     nrow = 1, ncol = 2, axis = "lb")
        if (save) {
            if (is.null(file_names)) {
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
            } else {
                if (order_files) {
                    filename <- paste(sprintf("%04d_", i),
                                      gsub(" ", "_", file_names[i]),
                                      ".png",
                                      sep = "")
                } else {
                    filename <- paste(gsub(" ", "_", file_names[i]),
                                      ".png",
                                      sep = "")
                }
            }
            ggplot2::ggsave(filename,
                            plot = p_list, path = path,
                            device = "png", width = w, height = h, units = "in")
            filenames[i] <- filename
        }
        if (return_plots) {
            all_plots[[i]] <- p_list
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
        
#' Plot a histogram of a variable in rowData and colData.
#'
#' Plots a ggplot histogram of a metadata variable. Can also
#' annotate with the mean and/or median. Note that the mean and
#' median legend values will only be accurate to 0 decimal places.
#'
#' @param sce_obj The SingleCellExperiment object.
#' @param x The name of the variable to plot.
#' @param row_metadata TRUE if x is in rowData, FALSE if in colData.
#' @param title The title of the plot.
#' @param x_label The x-axis label of the plot.
#' @param bins The number of bins to use. Cannot be set if binwidth is set.
#' @param binwidth The size of each bin. Cannot be set if bins is set.
#' @param mean If TRUE, a line will be plotted to indicate the mean of the
#'   distribution.
#' @param median If TRUE, a line will be plotted to indicate the median of
#'   the distribution.
#' @export
metadata_histogram <- function(sce_obj, x,
                               row_metadata = FALSE,
                               title = NULL, x_label = NULL,
                               bins = NULL, binwidth = NULL,
                               mean = TRUE, median = TRUE) {
    cell_data <- data.frame(SingleCellExperiment::colData(sce_obj))
    gene_data <- data.frame(SingleCellExperiment::rowData(sce_obj))
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

#' Build dataframe to ggplot cluster overlap heatmap.
#'
#' Build a dataframe suitable for ggplot. The dataframe
#' allows plotting a heatmap showing the overlap of two
#' clustering partitions of the same cells. Each cell of
#' the heatmap is the percentage of the x-axis cluster
#' in the y-axis cluster. If 2 SCE objects are passed,
#' only the cells in common will be used.
#'
#' @param sce1 The first SingleCellExperiment containing
#'   cell IDs and cluster labels for those cells in the
#'   colData.
#' @param sce2 The second SingleCellExperiment with the
#'   same characteristics as sce1. If NULL, then all cells
#'   in sce1 are used with two different clustering partitions
#'   stored in the sce1 colData. If not NULL, then only the
#'   cells with identical cell IDs are used.
#' @param cluster A 1 or 2-member vector containing the
#'   colData column names for the cluster labels. If cluster
#'   has length 1, the same column is used from both SCE
#'   objects. If cluster has length 2, then \code{cluster[1]} 
#'   is used for sce1 and likewise for 2.
#' @param cell_id A 1 or 2-member vector containing the
#'   colData column names for the cell IDs. If cell_id
#'   has length 1, the same column is used from both SCE
#'   objects. If cell_id has length 2, then \code{cell_id[1]} 
#'   is used for sce1 and likewise for 2.
#' @param verbose If TRUE, then the number of cells in each
#'   SCE object and the number of cells in common is printed
#'   to stdout. 
#' @return A data frame with 4 columns and c x d rows where
#'   c and d are the number of unique clusters in the two
#'   clustering partitions. The first 2 columns represent
#'   possible pairs of clusters, named "cluster_1" and "cluster_2".
#'   They refer to clusters in sce1 and sce2 or \code{cluster[1]}
#'   and \code{cluster[2]}. The 3rd and 4th columns contain
#'   the percentage of cluster_1 in cluster_2 or vice versa and
#'   are named "percent_1_in_2" and "percent_2_in_1".
#' @export
build_cluster_overlaps_df <- function(sce1, sce2 = NULL,
                                      cluster = c("metacluster"),
                                      cell_id = c("Barcode"), verbose = TRUE) {
    if (length(cluster) == 0 || length(cluster) > 2) {
        stop("cluster must be a vector of length 1 or 2!")
    } else if (length(cluster) == 1) {
        cluster <- rep(cluster, 2)
    }
    if (length(cell_id) == 0 || length(cell_id) > 2) {
        stop("cell_id must be a vector of length 1 or 2!")
    } else if (length(cell_id) == 1) {
        cell_id <- rep(cell_id, 2)
    }
    if (is.null(sce2)) {
        if (length(cluster) != 2) {
            stop("cluster must have 2 values if sce2 is NULL!")
        } else if (cluster[1] == cluster[2]) {
            stop("cluster must have 2 different values if sce2 is NULL!")
        }
        if (length(cell_id) != 1) {
            stop("cell_id cannot have 2 values if sce2 is NULL!")
        }
    }
    cluster_1 <- SummarizedExperiment::colData(sce1)[, cluster[1]]
    cell_id_1 <- SummarizedExperiment::colData(sce1)[, cell_id[1]]
    if (is.null(sce2)) {
        cluster_2 <- SummarizedExperiment::colData(sce1)[, cluster[2]]
        cell_id_2 <- SummarizedExperiment::colData(sce1)[, cell_id[2]]
        cluster_ids_1 <- cluster_1
        cluster_ids_2 <- cluster_2
        if (verbose) {
            cat("All Cells Used : ", length(cell_id_1), "\n", sep = "")
        }
    } else {
        cluster_2 <- SummarizedExperiment::colData(sce2)[, cluster[2]]
        cell_id_2 <- SummarizedExperiment::colData(sce2)[, cell_id[2]]
        cells_in_common <- intersect(cell_id_1, cell_id_2)
        f1 <- match(cells_in_common, cell_id_1)
        f2 <- match(cells_in_common, cell_id_2)
        cluster_ids_1 <- cluster_1[f1]
        cluster_ids_2 <- cluster_2[f2]
        if (verbose) {
            cat("sce1 Cells      : ", length(cell_id_1), "\n", sep = "")
            cat("sce2 Cells      : ", length(cell_id_2), "\n", sep = "")
            cat("Cells in Common : ", length(cells_in_common), "\n", sep = "")
        }
    }
    
    cluster_levels_1 <- sort(unique(as.numeric(as.character(cluster_ids_1))))
    cluster_levels_2 <- sort(unique(as.numeric(as.character(cluster_ids_2))))
    overlap_df <- data.frame(cluster_1 = factor(rep(cluster_levels_1,
                                                    length(cluster_levels_2)),
                                                levels = cluster_levels_1),
                             cluster_2 = factor(rep(cluster_levels_2,
                                                    each = length(cluster_levels_1)),
                                                levels = cluster_levels_2),
                             stringsAsFactors = FALSE)
    cluster_mat_1 <- t(sapply(cluster_levels_1, function(x) {cluster_ids_1 == x}))
    cluster_mat_2 <- t(sapply(cluster_levels_2, function(x) {cluster_ids_2 == x}))
    sums_overlap <- cluster_mat_1 %*% t(cluster_mat_2)
    overlap_df$percent_1_in_2 <- as.vector(sums_overlap / rowSums(sums_overlap))
    overlap_df$percent_2_in_1 <- as.vector(t(t(sums_overlap) / colSums(sums_overlap)))
    return(overlap_df)
}

#' Build an expression plot for markers in metaclusters.
#'
#' @export
build_marker_dotplot <- function(sce_obj, markers, marker_names = NULL, cluster = "metacluster",
                                 plot_title = "Mean Expression and % Expressed",
                                 stat = c("norm_mean", "z_score")) {
    # Preprocessing
    sce_obj <- sce_obj[, !is.na(SingleCellExperiment::colData(sce_obj)[, cluster])]
    data <- as.data.frame(SingleCellExperiment::colData(sce_obj))
    expr <-  SingleCellExperiment::logcounts(sce_obj)[markers, ]
    if (is.null(marker_names)) {
        marker_names <- markers
    }

    # Construct dataframe
    if (stat == "norm_mean") {
        means <- apply(expr, 1, function(x) {
            sapply(split(x, as.factor(data[[cluster]])), function(y) {
                mean(y) / max(x)
            })
        })
    } else if (stat == "z_score") {
        means <- apply(expr, 1, function(x) {
            sapply(split(x, as.factor(data[[cluster]])), function(y) {
                mean((y - mean(x)) / sd(x))
            })
        })
    } else {
        stop("invalid stat argument")
    }

    incidence <- apply(expr, 1, function(x) {
        sapply(levels(data[[cluster]]), function (y) {
            sum(data[[cluster]] == y & x > 0) / sum(data[[cluster]] == y)
        })
    })

    df <- data.frame(marker = rep(marker_names, each = length(levels(data[[cluster]]))),
                     cluster = factor(rep(levels(data[[cluster]]), times = length(marker_names)),
                                          levels = levels(data[[cluster]])),
                     incidence = as.vector(incidence)
    )
    df[, stat] = as.vector(means)

    # Plot
    plot <- (
        ggplot2::ggplot(df)
            + ggplot2::geom_point(ggplot2::aes_string(x = "cluster", y = "marker",
                                                      size = "incidence", color = stat))
            + ggplot2::labs(title = plot_title)
            + ggplot2::xlab(cluster)
            + ggplot2::ylab("Marker")
            + ggplot2::scale_colour_gradient(low = "white",
                                             high = "dark red")
            + ggplot2::scale_y_discrete(limits = rev(marker_names))
            + ggplot2::theme_classic()
            + ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 0.5,
                                                                 angle = 0))
    )
    return(plot)
}

#' Build a colData boxplot for a SingleCellExperiment
#'
#' Build a boxplot for a continuous y variable in
#' the SingleCellExperiment object, stratified by a discrete
#' variable in the SingleCellExperiment colData.
#'
#' @export
build_boxplot <- function(sce_obj, x, y, xlab = NULL, ylab = NULL,
                          title = NULL, y_transform = "identity",
                          assay = "logcounts") {
    data_df <- data.frame(SingleCellExperiment::colData(sce_obj))
    marker <- FALSE
    if (!(x %in% colnames(SingleCellExperiment::colData(sce_obj)))) {
        if (length(x) == dim(sce_obj)[2]) {
            data_df[[x]] <- x
        } else {
            stop(paste0(x, " is not a colData column or a cell split vector of correct length!"))
        }
    }
    if (!(y %in% colnames(SingleCellExperiment::colData(sce_obj)))) {
        if (y %in% rownames(sce_obj)) {        
            if (!(assay %in% SummarizedExperiment::assayNames(sce_obj))) {
                stop(paste0(assay, " is not an assay in sce_obj!"))
            }
            gene_expr <- SummarizedExperiment::assay(sce_obj, assay)[y, ]
            data_df[[y]] <- gene_expr
            marker <- TRUE
        } else {
            stop(paste0(y, " is not a colData column or a gene name!"))
        }
    }
    if (is.null(xlab)) {
        if (length(x) == dim(sce_obj)[2]) {
            xlab <- "Cell Partition"
        } else {
            xlab <- x
        }
    }
    if (is.null(ylab)) {
        if (marker) {
            ylab <- paste0(y, " Expression")
        } else {
            ylab <- y
        }
    }
    if (is.null(title)) {
        title <- sprintf("%s Stratified by %s", ylab, xlab)        
    }
    p <- (
          ggplot2::ggplot(data_df)
          + ggplot2::geom_boxplot(ggplot2::aes_string(x = x, y = y))
          + ggplot2::theme_classic()
          + ggplot2::labs(title = title)
          + ggplot2::xlab(xlab)
          + ggplot2::ylab(ylab)
          + ggplot2::scale_y_continuous(trans = y_transform)
    )
    return(p)
}

#' Build a stratified bar plot showing cluster and batch abundance.
#'
#' Build a stratified bar plot, showing the number of cells in
#' each cluster, with each bar's batch breakdown shown by
#' stratified fill.
#'
#' @export
cluster_batch_bars <- function(sce_obj, cluster = "metacluster",
                               batch = "study_id", title = NULL) {
    if (is.null(title)) {
        title <- tools::toTitleCase(paste(stringr::str_replace(cluster, "_", " "),
                                          "stratified by",
                                          stringr::str_replace(batch, "_", " "),
                                          sep = " "))
    }
    p <- (
        ggplot2::ggplot(data = data.frame(SingleCellExperiment::colData(sce_obj)))
        + ggplot2::geom_bar(ggplot2::aes_string(x = cluster, fill = batch),
                            position = ggplot2::position_stack(reverse = TRUE))
        + ggplot2::scale_y_continuous(limits = c(0, NA),
                                      expand = ggplot2::expand_scale(mult = c(0, 0.05)))
        + ggplot2::labs(title = title)
        + ggplot2::xlab("Cluster")
        + ggplot2::ylab("Cell Count")
        + ggplot2::theme_classic()
        + ggplot2::theme(axis.title.x = ggplot2::element_text(margin = ggplot2::margin(c(15, 0, 0, 0))),
                         axis.title.y = ggplot2::element_text(margin = ggplot2::margin(c(0, 15, 0, 0))))
    )
    return(p)
}

#' Plot a kernel density estimate for summed gene expression over cells
#'
#' @export
plot_expression_density_estimate <- function(sce_obj, genes = NULL, cells = NULL,
                                             plot_title = "", x_title = "Summed Gene Expression", y_title = "",
                                             assay = "logcounts") {
    if (is.null(genes)) {
        genes <- c(1:(dim(sce_obj)[1]))
    } else if (is.logical(genes)) {
        if (length(genes) != dim(sce_obj)[1]) {
            stop("If 'genes' is a logical vector, it must be the same length as the number of genes")
        } else {
            genes <- genes
        }
    } else if (is.integer(genes)) {
        if (!all(genes %in% c(1:(dim(sce_obj)[1])))) {
            stop("If 'genes' is an integer index vector, all values must be between 1 and the number of genes")
        } else if (length(genes) != length(unique(genes))) {
            warning("Repeated values in 'genes', dropping duplicates")
            genes <- unique(genes)
        } else {
            genes <- genes
        }
    } else if (is.character(genes)) {
        if (!all(genes %in% rownames(sce_obj))) {
            stop("If 'genes' is a character vector, all values must be in rownames of 'sce_obj'")
        } else if (length(genes) != length(unique(genes))) {
            warning("Repeated values in 'genes', dropping duplicates")
            genes <- unique(genes)
        } else {
            genes <- genes
        }
    }
    if (is.null(cells)) {
        cells <- c(1:(dim(sce_obj)[2]))
    } else if (is.logical(cells)) {
        if (length(cells) != dim(sce_obj)[2]) {
            stop("If 'cells' is a logical vector, it must be the same length as the number of cells")
        } else {
            cells <- cells
        }
    } else if (is.integer(cells)) {
        if (!all(cells %in% c(1:(dim(sce_obj)[2])))) {
            stop("If 'cells' is an integer index vector, all values must be between 1 and the number of cells")
        } else if (length(cells) != length(unique(cells))) {
            warning("Repeated values in 'cells', dropping duplicates")
            cells <- unique(cells)
        } else {
            cells <- cells
        }
    } else if (is.character(cells)) {
        if (!all(cells %in% colnames(sce_obj))) {
            stop("If 'cells' is a character vector, all values must be in colnames of 'sce_obj'")
        } else if (length(cells) != length(unique(cells))) {
            warning("Repeated values in 'cells', dropping duplicates")
            cells <- unique(cells)
        } else {
            cells <- cells
        }
    }
    if (!(assay %in% SummarizedExperiment::assayNames(sce_obj))) {
        stop("'assay' is not an assay in 'sce_obj'")
    }
    summed_expression <- Matrix::colSums(SummarizedExperiment::assay(sce_obj, assay)[genes, cells, drop = FALSE])
    data <- data.frame(expression = summed_expression)
    plot_obj <- (
        ggplot2::ggplot(data = data)
        + ggplot2::geom_density(ggplot2::aes(x = expression))
        + ggplot2::labs(title = plot_title)
        + ggplot2::xlab(x_title)
        + ggplot2::ylab(y_title)
        + ggplot2::theme_classic()
    )
    return(plot_obj)
}

#' Plot a volcano plot or an MA plot of DE results
#'
#' Designed to work with the output of functions in \code{diff_exp.R}
#'
#' @param de_res The DE results. A data.frame from the CSV file written by \code{write_de()}.
#' @param plot Indicates if the plot should be a volcano or MA plot.
#' @param ct_markers An optional vector of genes that are known markers
#'   and should be indicated as such.
#' @param correct Indicates the method for multiple hypothesis test correction.
#' @param plot_title The string used as the title for the plot.
#' @param alpha The significance cut off value. p-values below this
#'   will be labeled significant. Only applicable in auroc_fc or MA plots.
#' @param mean_expression A vector of mean gene expression values for
#'   the genes in \code{de_res}, to be plotted in the MA plot. Default
#'   is the mean expression across the cells in the cluster being
#'   tested, as reported in \code{de_res}.
#' @export
plot_de_results <- function(de_res, plot = c("volcano", "ma", "auroc_fc"),
                            ct_markers = NULL, correct = c("BH", "bonferroni", "none"), plot_title = "DE Results",
                            alpha = NULL, mean_expression = NULL) {
    valid_args <- list(plot = c("volcano", "ma", "auroc_fc"),
                       correct = c("BH", "bonferroni", "none"))
    if (length(plot > 1)) {
        plot <- plot[1]
    }
    if (length(correct > 1)) {
        correct <- correct[1]
    }
    if (!(plot %in% valid_args[["plot"]])) {
        stop(sprintf("plot must be in [%s]\n", paste(valid_args[["plot"]], collapse = ", ")))
    }
    if (!(correct %in% valid_args[["correct"]])) {
        stop(sprintf("correct must be in [%s]\n", paste(valid_args[["correct"]], collapse = ", ")))
    }
    if (is.null(alpha)) {
        if (plot == "volcano") {
            alpha <- 0.05
        } else if (plot == "ma") {
            alpha <- 0.05
        } else if (plot == "auroc_fc") {
            alpha <- 0.8
        } else {
            stop(sprintf("Invalid 'plot' argument \"%s\" got past input validation.", plot))
        }
    }
    gene_f <- !((is.na(de_res$de_pval)) | (is.na(de_res$log2fc)))
    data <- data.frame(pval = de_res$de_pval, logfc = de_res$log2fc,
                       mean_exp = de_res$mean_exp)
    if (plot == "auroc_fc") {
        if (!("de_roc" %in% colnames(de_res))) {
            stop("If 'plot' is \"auroc_fc\", 'de_res' must have a \"de_roc\" column.")
        }
        data$de_roc <- de_res$de_roc
    }
    num_genes <- dim(data)[1]
    rownames(data) <- rownames(de_res)
    if (!is.null(ct_markers)) {
        if (!(all(ct_markers %in% rownames(data)))) {
            stop("ct_markers must be a character vector of gene names in the de_res")
        } else if (any(!(ct_markers %in% rownames(data)[gene_f]))) {
            warning("some of ct_markers filtered because of missing DE results")
            ct_markers <- ct_markers[ct_markers %in% rownames(data)[gene_f]]
        }
    }
    if (!is.null(mean_expression)) {
        if (!is.numeric(mean_expression)) {
            stop("mean_expression must be a numeric vector")
        }
        if (length(mean_expression) != length(gene_f)) {
            stop("mean_expression must be the same length as the number of gene")
        }
        if (any(is.na(mean_expression))) {
            stop("NA values are not allowed in mean_expression")
        }
        data$mean_exp <- mean_expression
    } else {
        # Nothing. Use the mean expression given.
    }
    data <- data[gene_f, ]
    if (!is.null(ct_markers)) {
        data$known <- "novel"
        data[ct_markers, "known"] <- "known_marker"
        data$known <- factor(data$known, levels = c("known_marker", "novel"))
        data$known_stroke <- 0
        data[ct_markers, "known_stroke"] <- 1
    }
    data$pval[data$pval == 0] <- min(data$pval[data$pval != 0])
    if (correct == "BH") {
        data$pval <- p.adjust(data$pval, method = "BH")
        alpha <- alpha  # Just for readability
    } else if (correct == "bonferroni") {
        data$pval <- data$pval  # Just for readability
        alpha <- 0.05 / num_genes
    } else if (correct == "none") {
        # Do nothing
    } else {
        stop(sprintf("Invalid correct value \"%s\" made it past input validation.", correct))
    }
    if (plot == "volcano") {
        data$pval[data$pval != 0] <- log10(data$pval[data$pval != 0])
        if (alpha != 0) {
            alpha <- log10(alpha)
        }
        if (correct == "BH") {
            y_label_correct <- " - Benjamini/Hochberg Corrected"
        } else if (correct == "bonferroni") {
            y_label_correct <- " (Bonferroni Corrected Threshold)"
        } else {
            y_label_correct <- ""
        }
        p <- (
            ggplot2::ggplot(data = data)
            + ggplot2::geom_hline(yintercept = alpha, size = 0.25)
            + ggplot2::scale_y_reverse()
            + ggplot2::ggtitle(plot_title)
            + ggplot2::xlab("log2 Fold Change")
            + ggplot2::ylab(paste0("log10 p-value ", y_label_correct))
            + ggplot2::theme(legend.title = ggplot2::element_blank())
         )
        if (!(is.null(ct_markers))) {
            p <- (
                p
                + ggplot2::geom_point(ggplot2::aes(x = logfc, y = pval,
                                                   color = known, size = known, stroke = known_stroke))
                + ggplot2::scale_colour_manual(name = "Marker",
                                               label = c("Known", "Novel"),
                                               values = c("#E00202", "#000000"))  # black and red
                + ggplot2::scale_size_manual(values = c(3, 0.5), guide = "none")
            )
        } else {
            p <- (
                p
                + ggplot2::geom_point(ggplot2::aes(x = logfc, y = pval), size = 0.5)
            )
        }
    } else if (plot == "auroc_fc") {
        if (is.null(mean_expression)) {
            # If not passed, mean_exp from write_de is not log transformed
            # if passed, user is responsible for passing desired transformation
            data$mean_exp <- log2(data$mean_exp + 1)
        }
        p <- (
            ggplot2::ggplot(data = data)
            + ggplot2::geom_hline(yintercept = alpha, size = 0.25)
            + ggplot2::ggtitle(plot_title)
            + ggplot2::scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0))
            + ggplot2::xlab("log2 Fold Change")
            + ggplot2::ylab("AUROC")
            + ggplot2::scale_colour_gradient(name = "Mean Expr.")
         )
        if (!(is.null(ct_markers))) {
            p <- (
                p
                + ggplot2::geom_point(ggplot2::aes(x = logfc, y = de_roc, color = mean_exp,
                                                   shape = known, size = known, stroke = known_stroke))
                + ggplot2::scale_shape_manual(name = "Marker",
                                              label = c("Known", "Novel"),
                                              values = c("diamond", "circle"))
                + ggplot2::scale_size_manual(values = c(3, 1), guide = "none")
            )
        } else {
            p <- (
                p
                + ggplot2::geom_point(ggplot2::aes(x = logfc, y = de_roc,
                                                   color = mean_exp), size = 1)
            )
        }
    } else if (plot == "ma") {
        data$significant <- "Not Significant"
        data$significant[data$pval < alpha] <- "Significant"
        data$significant <- factor(data$significant, levels = c("Significant", "Not Significant"))
        if (correct == "BH") {
            pvalue_label <- " (BH Correct)"
        }
        else if (correct == "bonferroni") {
            pvalue_label <- " (Bonf. Corrected)"
        } else {
            pvalue_label <- ""
        }
        p <- (
            ggplot2::ggplot(data = data)
            + ggplot2::scale_colour_manual(name = paste0("p-value", pvalue_label),
                                           values = c("#E00202", "#000000"))  # black and red
            + ggplot2::scale_size_manual(values = c(4, 0.5), guide = "none")
            + ggplot2::geom_hline(yintercept = 0)
            + ggplot2::ggtitle(plot_title)
            + ggplot2::xlab("Mean Normalized Expression")
            + ggplot2::ylab("log2 Fold Change")
        )
        if (!(is.null(ct_markers))) {
            p <- (
                p
                + ggplot2::geom_point(ggplot2::aes(x = mean_exp, y = logfc, color = significant,
                                      size = known, shape = known))
                + ggplot2::scale_shape_manual(name = "Marker",
                                              label = c("Known", "Novel"),
                                              values = c("triangle", "circle"))
            )
        } else {
            p <- (
                p
                + ggplot2::geom_point(ggplot2::aes(x = mean_exp, y = logfc,
                                      color = significant, size = significant))
            )
        } 
    } else {
        stop(sprintf("Invalid 'plot' argument \"%s\" made it past input validation.", plot))
        stop(sprintf("Invalid 'correct' argument \"%s\" made it past input validation.", correct))
    }
    return(p)
}
