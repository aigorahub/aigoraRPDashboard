#' @export
prep_pca_data <- function(data,
                          .id_cols,
                          .attributes_name_colname,
                          .attributes_value_colname,
                          .studies_colname,
                          .panelists_name_colname,
                          .attributes = tidyselect::everything()) {

  # browser()
  pca_data <- data %>%
    tidyr::pivot_wider(id_cols = c({{.id_cols}}, {{.panelists_name_colname}}),
                       names_from = {{.attributes_name_colname}},
                       values_from = {{.attributes_value_colname}}) %>%
    dplyr::group_by(across({{ .id_cols }})) %>%
    dplyr::summarise(dplyr::across({{ .attributes }}, mean, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-{{.panelists_name_colname}}) %>%
    tidyr::unite(rowname, {{.id_cols}}) %>%
    tibble::column_to_rownames()

  if(any(is.na(pca_data))) pca_data <- missMDA::imputePCA(pca_data)$completeObs

  return(
    structure(
      pca_data,
      class = c("aigora_pca", "tbl_df", "tbl", "data.frame")
    )
  )
}

#' @export
get_num_comps <- function(pca_res, eigenvalue_thresh){
  sum(pca_res$eig[, 2] > eigenvalue_thresh)
}

#' @export
get_eigenvalue_thresh <- function(pca_data){
  max( 100 / (dim(pca_data) - 1) )
}

#' @export
run_pca <- function(pca_data){
  eigenvalue_thresh <- get_eigenvalue_thresh(pca_data)

  pre_pca_res <- FactoMineR::PCA(pca_data, ncp = Inf, graph = FALSE)
  num_comps <- get_num_comps(pre_pca_res, eigenvalue_thresh)

  pca_res <- FactoMineR::PCA(pca_data, ncp = num_comps, graph = FALSE)
  num_rel_comps <- get_num_comps(pca_res, eigenvalue_thresh)

  if(num_rel_comps < 2) stop("At least two meaningful dimensions are needed to create biplots.  Try increasing the number of attributes.")

  return(pca_res)
}

#' @export
make_scree_plot <- function(pca_res,
                            eigenvalue_thresh,
                            main_color = "red",
                            minor_color = "dark grey",
                            custom_theme = ggplot2::theme_minimal()){
  pca_res %>%
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

}

#' @export
get_theo_contrib <- function(pca_data){
  100 / ncol(pca_data)
}

#' @export
make_importance_plot <- function(pca_res,
                                 theo_contrib,
                                 num_rel_comps,
                                 main_color = "red",
                                 minor_color = "dark grey",
                                 custom_theme = ggplot2::theme_minimal()){

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


}

#' @export
make_biblots <- function(pca_res,
                         num_rel_comps,
                         num_vars_to_show = NULL,
                         main_color = "red",
                         minor_color = "dark grey",
                         custom_theme = ggplot2::theme_minimal()){
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
}

#' @export
plot.aigora_pca <- function(pca_data,
                            num_vars_to_show = NULL,
                            main_color = "red",
                            minor_color = "dark grey",
                            body_bg_col = "white",
                            custom_theme = ggplot2::theme_minimal()){

  # browser()
  eigenvalue_thresh <- get_eigenvalue_thresh(pca_data)
  pca_res <- run_pca(pca_data)
  num_rel_comps <- get_num_comps(pca_res, eigenvalue_thresh)
  theo_contrib <- get_theo_contrib(pca_data)

  scree_plot <- make_scree_plot(pca_res, eigenvalue_thresh)
  importance_plot <- make_importance_plot(pca_res, theo_contrib, num_rel_comps)
  biplots <- make_biblots(pca_res, num_rel_comps, num_vars_to_show)

  pca_plot_list <- list(
    "Principle Component Analysis - Diagnostics" = c(
      "Scree Plot" = list(scree_plot),
      importance_plot
    ),
    "Principle Components Analysis - Biplots with Names" = biplots
  )
  return(pca_plot_list)
}
