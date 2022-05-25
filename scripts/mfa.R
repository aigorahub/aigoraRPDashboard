plot_mfa <- function(df_scaled, df_cata, palette) {

  df_scaled_mean <- df_scaled %>%
    pivot_wider(names_from = Attribute, values_from = Response) %>%
    group_by(Product) %>%
    summarise_if(is.numeric, mean) %>%
    ungroup() %>%
    column_to_rownames("Product") %>%
    # scale(scale = FALSE) %>%
    as.data.frame() %>%
    rownames_to_column("Product")

  cata_contingency <- df_cata %>%
    pivot_wider(names_from = Attribute, values_from = Response) %>%
    select(-c(Replication:Group)) %>%
    select_if(~ sum(.) > 0) %>%
    bind_cols(df_cata %>%
                pivot_wider(names_from = Attribute, values_from = Response) %>%
                select(Product:Group)) %>%
    relocate(c(Panelist:Group), .before = 1) %>%
    arrange(Panelist, Product) %>%
    group_by(Product) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE) %>%
    ungroup()

  cata_n <- df_cata %>% count(Product) %>% pull(n) %>% unique()
  trsf_perc <- function(x) (x/cata_n)

  cata_perc <- cata_contingency %>%
    #mutate_if(is.numeric, trsf_perc) %>%
    column_to_rownames("Product") %>%
    #scale(scale = FALSE) %>%
    as.data.frame() %>%
    rownames_to_column("Product")

  mfa_data <- df_scaled_mean %>%
    #left_join(cata_contingency) %>%
    left_join(cata_perc) %>%
    column_to_rownames("Product")

  scaled_attr_n <- df_scaled_mean %>% select_if(is.numeric) %>% ncol()
  cata_attr_n <- cata_contingency %>% select_if(is.numeric) %>% ncol()

  num_prods <- mfa_data %>% nrow()

  res_mfa <- mfa_data %>%
    MFA(
      group = c(scaled_attr_n, cata_attr_n),
      type = c("s", "s"),
      name.group = c("scaled", "cata"),
      graph = FALSE,
      ncp = num_prods - 1
    )

  mfa_plot <- res_mfa %>%
    fviz_mfa_var(#choice = "group",
      palette = palette_b,
      repel = TRUE,
      geom = c("point", "text"),
      title = "MFA between Scaled and CATA Variables")


  hcpc_mfa_cluster <- res_mfa %>%
    HCPC(nb.clust = -1, consol=FALSE, method = "ward", cluster.CA="rows",graph=FALSE)

  res_mfa_dend <- fviz_dend(hcpc_mfa_cluster,
                            palette = palette_b,
                            rect = TRUE,
                            rect_fill = FALSE
  )


  mfa_plot_list <- list("MFA" = mfa_plot, "MFA Dendrogram"= res_mfa_dend) #hamza: changed added

  return(mfa_plot_list)
}

dark_blue_b <- "#0e2b63"
mid_blue_b <- "#004f9f"
cyan_b <- "#00b1eb"
sky_b <- "#0c61a7"
orange_b <- "#ef7d00"
green_b <- "#50af47"
purple_b <- "#5a328a"
yellow_b <- "#ffbb00"
light_green_b <- "#afca0b"
magenta_b <- "#e72582"
grey_b <- "#575756"
red_b <- "#eb4a00"
brown_b <- "#632900"
yellow_gray_b <- "#cfcc2a"
rose_b <- "#af474f"
dirt_b <- "#8a5c32"
blue_green_b <- "#328a83"
blue_purple_b <- "#4756af"
light_brown_b <- "#af7747"
purple_red_b <- "#a647af"


white_b <- "#FFFFFF"

palette_b <- c(dark_blue_b,
               orange_b,
               mid_blue_b,
               yellow_b,
               cyan_b,
               green_b,
               purple_b,
               light_green_b,
               magenta_b,
               grey_b,
               #white_b,
               sky_b,
               red_b,
               brown_b,
               yellow_gray_b,
               rose_b,
               dirt_b,
               blue_green_b,
               blue_purple_b,
               light_brown_b,
               purple_red_b
)
getColors<-function(themeColors, n){
  colors_list<-c("#245668","#0F7279","#0D8F81","#39AB7E","#6EC574","#A9DC67","#EDEF5D","#4b2991","#872ca2","#c0369d","#ea4f88","#fa7876","#f6a97a","#edd9a3","#798234","#a3ad62","#d0d3a2","#fdfbe4","#f0c6c3","#df91a3","#d46780","#A16928","#bd925a","#d6bd8d","#edeac2","#b5c8b8","#79a7ac","#2887a1","#88CCEE","#CC6677","#DDCC77","#117733","#332288","#AA4499","#44AA99","#999933","#882255","#661100","#6699CC","#888888","#c4e6c3","#96d2a4","#6dbc90","#4da284","#36877a","#266b6e","#1d4f60","#f7feae","#b7e6a5","#7ccba2","#46aea0","#089099","#00718b","#045275","#ede5cf","#e0c2a2","#d39c83","#c1766f","#a65461","#813753","#541f3f","#ffc6c4","#f4a3a8","#e38191","#cc607d","#ad466c","#8b3058","#672044","#fbe6c5","#f5ba98","#ee8a82","#dc7176","#c8586c","#9c3f5d","#70284a","#d2fbd4","#a5dbc2","#7bbcb0","#559c9e","#3a7c89","#235d72","#123f5a","#d3f2a3","#97e196","#6cc08b","#4c9b82","#217a79","#105965","#074050","#f3cbd3","#eaa9bd","#dd88ac","#ca699d","#b14d8e","#91357d","#6c2167","#e4f1e1","#b4d9cc","#89c0b6","#63a6a0","#448c8a","#287274","#0d585f","#ecda9a","#efc47e","#f3ad6a","#f7945d","#f97b57","#f66356","#ee4d5a","#fde0c5","#facba6","#f8b58b","#f59e72","#f2855d","#ef6a4c","#eb4a40","#fef6b5","#ffdd9a","#ffc285","#ffa679","#fa8a76","#f16d7a","#e15383","#f3e0f7","#e4c7f1","#d1afe8","#b998dd","#9f82ce","#826dba","#63589f","#f9ddda","#f2b9c4","#e597b9","#ce78b3","#ad5fad","#834ba0","#573b88","#f6d2a9","#f5b78e","#f19c7c","#ea8171","#dd686c","#ca5268","#b13f64","#f3e79b","#fac484","#f8a07e","#eb7f86","#ce6693","#a059a0","#5c53a5","#fcde9c","#faa476","#f0746e","#e34f6f","#dc3977","#b9257a","#7c1d6f","#d1eeea","#a8dbd9","#85c4c9","#68abb8","#4f90a6","#3b738f","#2a5674")
  colors_list<-sample(colors_list)
  if(n<=20){
    colors<-themeColors[1:n]
  }else{
    colors<-c(themeColors[1:20],colors_list[1:(n-20)])
  }

  return(colors)
}

palette_b = getColors(palette_b, 100)

library(tidyverse)
library(FactoMineR)
library(factoextra)
data <- openxlsx::read.xlsx("tests/testdata/Study1.xlsx")%>%
  tidyr::pivot_longer(cols = dplyr::starts_with("Attribute"),
                      names_to = "Attribute_Name",
                      values_to = "Attribute_Value") %>%
  dplyr::mutate(Study = "Study1", Group = "a") %>%
  select(Replication = Study, Product = Product_Name, Panelist = Panelist_Name, Group, Attribute = Attribute_Name, Response = Attribute_Value)

plot_mfa(df_scaled = data %>% filter(grepl("Scaled", Attribute)),
         df_cata = data %>% filter(grepl("CATA", Attribute)),
         palette_b)
