make_sumtable <- function(data){
  res <- data %>%
    filter(Product_Name %in% unlist(unique(sumtableVls$selectedSamples)),
           Attribute_Name %in% unlist(unique(sumtableVls$selectedAttributes))) %>%
    group_by(Study, Product_Name, Attribute_Name) %>%
    summarize(Mean_scores = round(mean(Attribute_Value), 2)) %>%
    ungroup()
}
