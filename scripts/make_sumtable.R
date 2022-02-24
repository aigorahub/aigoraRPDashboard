make_sumtable <- function(data,
                          selected_products,
                          selectedAttributes){
  res <- data %>%
    filter(Product_Name %in% unlist(unique(selected_products)),
           Attribute_Name %in% unlist(unique(selectedAttributes))) %>%
    group_by(Study, Product_Name, Attribute_Name) %>%
    summarize(Mean_scores = round(mean(Attribute_Value), 2)) %>%
    ungroup()
}
