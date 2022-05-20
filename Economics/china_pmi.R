


library(magrittr)
source(stringr::str_c(getwd(),"/functions/theme_linkedin.R"))
setwd("C:/Users/Admin/Documents/Git Projects/LinkedIn")

china_pmi <- read.csv("C:/Users/Admin/Desktop/china_pmi.csv") %>% dplyr::as_tibble()

china_pmi_plot <- 
  china_pmi %>% 
  dplyr::mutate(Date = lubridate::mdy(Date),
                serv_pmi = round(Caixin.China.Services.PMI.SA-50, digits = 1),
                manu_pmi = round(Caixin.China.Manufacturing.PMI.SA-50, digits = 1)) %>% 
  dplyr::select(-Caixin.China.Services.PMI.SA,
                -Caixin.China.Manufacturing.PMI.SA) %>%
  tidyr::pivot_longer(-Date, names_to = "pmi", values_to = "values") %>%
  dplyr::mutate(text = ifelse(Date == Date[1:4], values + 50, "")) %>%
  ggplot2::ggplot(ggplot2::aes(x = Date, y = values, fill = factor(pmi))) +
  ggplot2::geom_bar(stat = "identity", position ="dodge", size = 1) +
  ggplot2::geom_text(ggplot2::aes(label = text), colour = "red", hjust = 1, vjust = 1, size = 4)+
  ggplot2::scale_y_continuous(labels = function(y) y + 50) +
  ggplot2::scale_fill_manual(
    values = c(main_color, main_color_light),
    labels = c("Manufacturing","Service")) +
  ggplot2::labs(title = "China's Purchasing Manager Index",
                subtitle = "weakaning Growth Outlook",
                x = "Date",
                y = "Contraction < 50% < Expansion M/M",
                fill = "PMI",
                caption = "data: Bloomberg") +
  theme_linkedin_light() 

  save_linkedin(china_pmi_plot, "pmi_plot")


