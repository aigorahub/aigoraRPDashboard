#' Run PCA
#'
#' @param data data.frame to be PCAed
#' @param .id_col id column
#' @param .attributes selector for attributes to be PCAed
#' @param num_vars_to_show top n attributes to show in biplots
#' @param main_color main color
#' @param minor_color minor color
#' @param body_bg_col background color
#' @param custom_theme custom theme
#'
#' @return list of PCA plots:
#' -scree plot
#' -list of plots of contribution to components
#' -list of biplots
#' @export
#'
make_pca_plot_list <- function(data,
                               .id_col,
                               .attributes = tidyselect::everything(),
                               num_vars_to_show = NULL,
                               main_color = "red",
                               minor_color = "dark grey",
                               body_bg_col = "white",
                               custom_theme = ggplot2::theme_minimal()
) {
  # browser()

  # Prepare data ------------------------------------------------------------
  pca_data <- data %>%
    dplyr::rename(rowname = {{ .id_col }}) %>%
    pivot_wider(id_cols = c(rowname, Study, Panelist_Name), names_from = Attribute_Name, values_from = Attribute_Value) %>%
    dplyr::group_by(rowname) %>%
    dplyr::summarise(dplyr::across({{ .attributes }}, mean, na.rm = TRUE)) %>%
    tibble::column_to_rownames()

  if(any(is.na(pca_data))) pca_data <- missMDA::imputePCA(pca_data)$completeObs

  eigenvalue_thresh <- max( 100 / (dim(pca_data) - 1) )

  pre_pca_res <- FactoMineR::PCA(pca_data, ncp = Inf, graph = FALSE)
  num_comps <- sum(pre_pca_res$eig[, 2] > eigenvalue_thresh)

  pca_res <- FactoMineR::PCA(pca_data, ncp = num_comps, graph = FALSE)
  num_rel_comps <- sum(pca_res$eig[, 2] > eigenvalue_thresh)

  if(num_rel_comps < 2) stop("At least two meaningful dimensions are needed to create biplots.  Try increasing the number of attributes.")


  # Scree plot --------------------------------------------------------------
  scree_plot <- pca_res %>%
    factoextra::fviz_screeplot(
      addlabels = TRUE,
      barfill = main_color,
      barcolor = main_color,
      linecolor = minor_color,
      main = "Scree Plot"
    ) +
    ggplot2::geom_hline(yintercept = eigenvalue_thresh,
                        color = minor_color,
                        linetype = 2) +
    custom_theme


  # Importance plots --------------------------------------------------------
  theo_contrib <- 100 / ncol(pca_data)

  attrib_imp_plot_list <- c(1:num_rel_comps,
                            list(1:num_rel_comps)) %>%
    purrr::set_names(
      sprintf("Attribute Importance: %s",
              purrr::map_chr(.,
                             ~ifelse(
                               length(.x) == 1,
                               sprintf("Dimension %d", .x),
                               "Overall"
                             )))) %>%
    purrr::map(~{
      pca_res %>%
        factoextra::fviz_contrib(
          choice = "var",
          axes = .x,
          fill = main_color,
          color = main_color,
        ) +
        ggplot2::ggtitle(label =  "") +
        ggplot2::geom_hline(yintercept = theo_contrib,
                            color = minor_color,
                            linetype = 2) +
        custom_theme +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    })


  # Biplots -----------------------------------------------------------------
  if(is.null(num_vars_to_show)) {
    select.var <- NULL
  } else {
    select.var <- list(contrib = num_vars_to_show)
  }

  pca_biplot_list <- 1:num_rel_comps %>%
    utils::combn(2) %>%
    as.data.frame() %>%
    purrr::set_names(purrr::map_chr(., ~{
      sprintf("Dimension %d vs Dimension %d", .x[[1]], .x[[2]])
    })) %>%
    purrr::map(~{
      output <- pca_res %>%
        factoextra::fviz_pca_biplot(
          map = "symmetric",
          ggtheme = ggplot2::theme_minimal(),
          axes = .x,
          title = "",
          font.family = custom_theme$text$family,
          col.ind = main_color,
          col.var = minor_color,
          select.var = select.var,
          repel = TRUE
        ) +
        custom_theme +
        ggplot2::theme(
          panel.background = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank()
        )

      return(output)
    })



  # Organize output ---------------------------------------------------------
  pca_plot_list <- list(
    "Principle Component Analysis - Diagnostics" = c(
      "Scree Plot" = list(scree_plot),
      attrib_imp_plot_list
    ),
    "Principle Components Analysis - Biplots with Names" = pca_biplot_list
  )

  return(pca_plot_list)
}
