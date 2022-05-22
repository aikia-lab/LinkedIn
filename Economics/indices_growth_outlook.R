


library(magrittr)
source(stringr::str_c(getwd(),"/functions/theme_linkedin.R"))

index_grth <- read.csv("C:/Users/Admin/Desktop/index_growth.csv") %>% dplyr::as_tibble()

index_plot <- 
  index_grth %>% 
    janitor::clean_names() %>% 
    dplyr::mutate(date = factor(date, levels = c(
                              "2006","2007","2008","2009","2010","2011",
                              "2012","2013","2014","2015","2016","2017",
                              "2018","2019","2020","2021","currrent Q122",
                              "2022 est","2023 est"))) %>%
  tidyr::pivot_longer(-date, names_to = "indices", values_to = "values") %>%
  ggplot2::ggplot(ggplot2::aes(x = date, y = values, fill = factor(indices))) +
  ggplot2::geom_bar(stat = "identity", position ="dodge", size = 1) +
  ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%")) +
  ggplot2::geom_hline(yintercept= 8, linetype='dotted', col = 'red', size = 1)+
  ggplot2::annotate("text", x = "2022 est", y = 8, label = "8% Level", vjust = -0.5)+
  ggplot2::scale_fill_manual(
      values = c(palette_main[c(4,1)]),
      labels = c("MSCI China Index","MSCI World Index")) +
  ggplot2::labs(title = "Index Earnings Growth Outlook",
                subtitle = "current outlook for company profitability",
                x = "Date",
                y = "Earning Growth Y/Y",
                fill = "Earnings Outlook",
                caption = "data: Bloomberg") +
  theme_linkedin_light() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 75, vjust = 0.5, hjust=0.5))
                 
  


save_linkedin(index_plot, "index_plot")

  
  