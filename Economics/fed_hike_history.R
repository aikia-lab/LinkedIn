
library(magrittr)
source(stringr::str_c(getwd(),"/R/theme_linkedin.R"))

# Retrieve Data -----------------------------------------------------------

fed_rate_lower_bound <- tidyquant::tq_get(
  x = c("DFEDTARL", "DFEDTAR"), # Two Series needed, old one deprecated in 2008
  get = "economic.data",
  from = "1990-01-01")


fed_rate_histo <- fed_rate_lower_bound %>% 
  dplyr::arrange(date) %>% 
  dplyr::select(-symbol) %>% 
  dplyr::mutate(rate_chg = price - dplyr::lag(price),
                action = dplyr::case_when(
                  rate_chg > 0 ~ "Hike",
                  rate_chg < 0 ~ "Cut",
                  rate_chg == 0 ~ "Stale"
                ),
                action_num = dplyr::case_when(
                  rate_chg > 0 ~ 1,
                  rate_chg < 0 ~ -1,
                  rate_chg == 0 ~ 0,
                ),
                i = NA) %>% 
  tidyr::replace_na(replace = list(action_num = 0))

#Function for Counting continuous hike phases ------------------------

  count_hikes <- function(row_number){
    
    if(row_number == 1){
      counter <- 0
      i <<- 1
    } else {
      
      today <- fed_rate_histo[row_number,]
      yesterday <- fed_rate_histo[row_number - 1,]
      
      if(today$action == "Stale"){
        if(yesterday$action == "Cut" | is.na(yesterday$action)){
          counter <- 0
        } else {
          counter <- yesterday$action_num
        }
      } else if (today$action == "Cut"){
        if(yesterday$action_num != 0){
          counter <- yesterday$action_num
        } else {
          counter <- 0
          i <<- i + 1
        }
      } else {
        counter <- yesterday$action_num + 1
      }
      
    }
    
    fed_rate_histo[row_number, "action_num"] <<- counter
    fed_rate_histo[row_number, "phase"] <<- i
    
  }
  
  purrr::map(c(1:nrow(fed_rate_histo)), count_hikes)


# Plot Preparation --------------------------------------------------------

  
  hikes <- fed_rate_histo %>% 
    dplyr::filter(action_num != 0) %>% 
    dplyr::mutate(phase = factor(paste0("phase_", phase))) %>% 
    dplyr::group_by(phase) %>% 
    dplyr::filter(phase != "phase_21") %>% 
    dplyr::mutate(day_count = dplyr::row_number(),
                  month = ceiling(day_count /30),
                  date_min = min(date),
                  date_max = max(date),
                  phase_name = stringr::str_c(lubridate::month(date_min, abbr = T, label = T),
                                              ". ",
                                              lubridate::year(date_min),
                                              " - ",
                                              lubridate::month(date_max, abbr = T, label = T),
                                              ". ",
                                              lubridate::year(date_max))) 
  
  annotation <- hikes %>% 
    dplyr::transmute(phase_name = phase_name,
                     year_start = lubridate::year(date_min),
                     year_end = lubridate::year(date_max),
                     price_min = min(price),
                     price_max = max(price),
                     month_start = 1,
                     month_end = max(month),
                     no_hikes = max(action_num)) %>% 
    dplyr::distinct()
  
  
 hike_plot <-  hikes %>%  
    ggplot2::ggplot()+
    ggplot2::geom_line(ggplot2::aes(month, price, color = phase_name), size = 1)+
    ggplot2::geom_text(data = annotation,
                       ggplot2::aes(month_end, price_max, label = year_end,  color = phase_name),
                       nudge_y = 0.2,
                       nudge_x = -1.10,
                       size = 6) +
    ggplot2::geom_text(data = annotation,
                       ggplot2::aes(month_start, price_min, label = year_start,  color = phase_name),
                       nudge_y = 0.2,
                       size = 6,
                       nudge_x = -0.3) +  
    ggplot2::geom_text(data = annotation,
                       ggplot2::aes(month_end, price_max, 
                                    label = paste0("Number of Hikes: ",no_hikes), color = phase_name),
                       nudge_x = -2.55,
                       nudge_y = -0.1,
                       size = 3)+
    ggplot2::labs(title = "Duration of FED target rate hike phases",
                  subtitle = "Timespan from first hike to first cut",
                  x = "Duration in Months",
                  y = "Fed Target Rate",
                  color = "Phase",
                  caption = "Source: FRED Series DFEDTARL and DFEDTAR\nFederal Reserve Bank of St. Louis")+
    ggplot2::scale_y_continuous(labels = ~paste0(.x, "%"))+
    scale_color_linkedin_four()+
    theme_linkedin_light() +
    ggplot2::theme(legend.position = "none")
  
save_linkedin(hike_plot, "hike_plot", format = "jpg")  
  
    