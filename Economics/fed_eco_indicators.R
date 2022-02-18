
library(magrittr)
library(ggtext)
source(paste0(getwd(),"/functions/theme_linkedin.R"))

eco_data <- tidyquant::tq_get(
  x = c("DFEDTARL", "DFEDTAR","CPIAUCSL","T10Y2Y",'T10Y3M'), # DFEDTARL deprecated in 2008. DFEDTAR new 1
  get = "economic.data",
  from = "1970-01-01") %>% 
  dplyr::mutate(symbol = ifelse(symbol == "DFEDTAR","DFEDTARL",symbol))


indi <- eco_data %>% 
  dplyr::mutate(price = ifelse(symbol == "CPIAUCSL", (price / dplyr::lag(price,12) -1)*100, price)) %>% 
  tidyr::drop_na(price) %>%
  dplyr::filter(date > "1982-01-01") %>% 
  ggplot2::ggplot(ggplot2::aes(x = date, y = price, group = symbol, color = symbol)) +
  ggplot2::geom_line()+
  ggplot2::geom_hline(yintercept = c(-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11), color = main_color_light, alpha = 0.5, linetype = "dashed")+
  ggplot2::scale_color_manual(values = palette_main,
                             guide = NULL) +
  theme_linkedin_light() +
  ggplot2::labs(title = "Historical US Economic & Funds Rate Phases",
                subtitle = "Economic Indicators in Relation to FED Funds Rate Levels",
                x = "Date",
                y = "Levels",
                color = "Series",
                caption = "Source: FRED Series") +
  
# Inflation
  ggplot2::annotate(x = lubridate::as_date("2014-01-01"), 
                    y = 8, 
                    geom = "label", 
                    label = 
                      "Inflation Indicator YoY", 
                    color = main_color,
                    hjust = 0,
                    vjust = -0.5) +
  ggplot2::geom_segment(ggplot2::aes(x = lubridate::as_date("2020-01-01"), xend = lubridate::as_date("2021-10-01"),
                                     y = 8.3, yend = 6.5),
                                    color = main_color_light) +

# FED FUNDS
  ggplot2::annotate(x = lubridate::as_date("1985-01-01"), 
                    y = 10.3, 
                    geom = "label", 
                    label = 
                      "FED Funds Rate", 
                    color = main_color,
                    hjust = 0,
                    vjust = -0.5) +
  ggplot2::geom_segment(ggplot2::aes(x = lubridate::as_date("1985-01-01"), xend = lubridate::as_date("1986-01-01"),
                                     y = 10, yend = 10.5),
                        color = main_color_light) +
  
# Recession   
  ggplot2::annotate(x = lubridate::as_date("1998-01-01"), 
                    y = 7, 
                    geom = "label", 
                    label = 
                      "Recession Indicator (10Y-3M)", 
                    color = main_color,
                    hjust = 0,
                    vjust = -0.5) +
  ggplot2::geom_segment(ggplot2::aes(x = lubridate::as_date("2004-01-01"), xend = lubridate::as_date("2004-01-01"),
                                     y = 3.8, yend = 7.2),
                        color = main_color_light) +
  
# Economic   
  ggplot2::annotate(x = lubridate::as_date("1990-01-01"), 
                    y = -2, 
                    geom = "label", 
                    label = 
                      "Growth Indicator (10Y-2Y)", 
                    color = main_color,
                    hjust = 0,
                    vjust = -0.5) +
  ggplot2::geom_segment(ggplot2::aes(x = lubridate::as_date("1992-01-01"), xend = lubridate::as_date("1992-01-01"),
                                     y = -1, yend = 2),
                        color = main_color_light) +
# Final Indicator
  ggplot2::geom_point(x = lubridate::as_date("2022-02-16"),
                      y = 0.4,
                      color = "red") +
  ggplot2::annotate(x = lubridate::as_date("2011-01-01"), 
                    y = -3, 
                    geom = "label", 
                    label = "lowest starting Point for rate hikes",
                    fontface = "bold",
                    color = palette_main[3],#main_color,
                    hjust = 0,
                    vjust = -0.5) +
  ggplot2::geom_segment(ggplot2::aes(x = lubridate::as_date("2021-01-01"), xend = lubridate::as_date("2022-02-16"),
                                     y = -1.9, yend = 0.35),
                        color = palette_main[3])



save_linkedin(indi, "fed_eco_indicator", format = "png")









