#' RUn Cluster Plots
#'
#' @param data data.frame for cluster plot
#' @param .id_col id column
#' @param .attributes selector for attributes
#' @param palette_col color palette for clusters
#' @param body_bg_col background color
#' @param min_num_clusts minimum number of clusters
#' @param max_num_clusts maximum number of clusters
#'
#' @return list of cluster plots
#' @export

make_cluster_phylo <- function(data,
                               .id_col,
                               .attributes = tidyselect::everything(),
                               palette_col = 'Dark2',
                               body_bg_col = "white",
                               min_num_clusts = 4,
                               max_num_clusts = 6

) {


  clust_data <- data %>%
    dplyr::rename(product = {{ .id_col }}) %>%
    dplyr::group_by(.data$product) %>%
    dplyr::summarise(dplyr::across({{ .attributes }}, mean, na.rm = TRUE)) %>%
    tibble::column_to_rownames("product")

  if(any(is.na(clust_data))) clust_data <- missMDA::imputePCA(clust_data)$completeObs

  max_num_clusts <- min(max_num_clusts, nrow(clust_data) - 1)

  df_dist <- clust_data %>%
    scale() %>%
    t() %>%
    stats::dist()

  res_hc <- stats::hclust(d = df_dist, method = "ward.D2")

  num_clust_res <- clust_data %>%
    scale() %>%
    factoextra::fviz_nbclust(factoextra::hcut, method = "gap_stat", k.max = max_num_clusts)

  num_clusts <- num_clust_res$data %>%
    tibble::rownames_to_column("Number") %>%
    dplyr::mutate(Number = as.numeric(.data$Number)) %>%
    dplyr::filter(.data$Number >= min_num_clusts) %>%
    dplyr::filter(.data$Number <= max_num_clusts) %>%
    dplyr::filter(.data$gap == max(.data$gap)) %>%
    dplyr::pull(.data$Number) %>%
    as.numeric()

  plot_list <- res_hc %>%
    factoextra::fviz_dend	(type = "phylogenic",
                           repel = TRUE,
                           cex = 1,
                           k = num_clusts,
                           palette = grDevices::palette(palette_col)
    )  +
    ggplot2::theme_void() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(fill = body_bg_col, color = body_bg_col),
      legend.key = ggplot2::element_rect(fill = body_bg_col, color = body_bg_col),
      legend.box.background = ggplot2::element_rect(fill = body_bg_col, color = body_bg_col),
      legend.position = "bottom",
      axis.title = ggplot2::element_blank()
    )
  return(list("Cluster Analysis" = c(
    "Cluster Plot" = list(plot_list)
  )))
}
