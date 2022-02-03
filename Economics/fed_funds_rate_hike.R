
library(magrittr)

source(stringr::str_c(getwd(),"/Source Scripts/theme_linkedin.R"))

# Future Prices public prices ---------------------------------------------

# FED Meetings
url <- "https://www.federalreserve.gov/monetarypolicy/fomccalendars.htm"

fed <- rvest::read_html(url)

fed_df <- fed %>% 
  rvest::html_node(".panel.panel-default") %>%
  rvest::html_nodes(".row.fomc-meeting") %>% 
  rvest::html_text() %>% 
  stringr::str_remove_all("\\r|\\n") %>%
  stringr::str_remove_all("\\*") %>%
  stringr::str_squish() %>%
  dplyr::as_tibble() %>% 
  tidyr::separate(col = value, sep = " ", into = c("month", "date")) %>% 
  dplyr::mutate(month_no = match(month, month.name),
                date = as.integer(stringr::str_extract(date, "(?<=-).*"))) %>% 
  dplyr::mutate(meeting_date = paste0(lubridate::year(lubridate::today()),"-",month_no,"-",date))

# Get Ticker 30 Day Federal Funds Futures -------------------------
relevant_ticker <- function(month, year){
  
  base_ticker = "FF"
  month_lookup = tibble::tibble(month = 1:12,
                                letter = c("F","G","H","J","K","M","N","Q","U","V","X","Z"))
  month = month_lookup[month_lookup$month==month,"letter"]
  year_single <- substr(as.character(year),3,4)
  
  relevant_tick <- paste0(base_ticker, month, year_single)
  
  return(relevant_tick)
}

# Future Prices ---------------------------------

get_future_prices <- function(ticker){
  
  
  # Future prices are on marketwatch.com
  mw_url <- paste0("https://www.marketwatch.com/investing/future/", ticker)
  
  #Check robot.txt, get approval and establish a session
  session <- polite::bow(mw_url, force = FALSE,verbose = TRUE, delay = 5)
  
  #Get price and time information
  price <- polite::scrape(session) %>% 
    rvest::html_nodes(".intraday__price") %>% 
    rvest::html_nodes(".value") %>% 
    rvest::html_text() %>% 
    as.double()
  
  timestamp <- polite::scrape(session) %>% 
    rvest::html_nodes(".intraday__timestamp") %>% 
    rvest::html_nodes(".timestamp__time") %>%
    rvest::html_text() %>% 
    stringr::str_extract("(?<=:\\s).*$") %>% 
    lubridate::mdy_hm()
  
  rm(session)
  
  return(tibble::tibble(date = timestamp,
                        future = ticker,
                        price = price))
  
}



# Rate Hike Function ----------------------------------------
get_hike_prob <- function(meeting=NULL, previous_month_price=NULL){
  

  # Link to CME Methodology: 
  # https://www.cmegroup.com/education/demos-and-tutorials/fed-funds-futures-probability-tree-calculator.html
  
  cat(cli::col_magenta("Calculate Probabilities for ", meeting, "\n"))
  
  if(is.null(meeting)){
    stop("No Meeting Date set")
  }
  
  month <- lubridate::month(meeting)
  year <- lubridate::year(meeting)
  
  
  # Find the correct Fed Fund Future Ticker
  actual_ticker <- relevant_ticker(month, year)
  
  if(month == 1){
    previous_ticker <- relevant_ticker(12, year-1)
  } else {
  previous_ticker <- relevant_ticker((month-1), year)
  }
  if(month < 12){
    nxt_ticker <- relevant_ticker(month+1, year)
  } else{
    nxt_ticker <- relevant_ticker(1, year+1)
  } 
  
  
  # Get the prices
  act_month <- get_future_prices(actual_ticker)%>% 
    dplyr::pull(price)
  
  if(month > lubridate::month(lubridate::today())){
  pre_month <- get_future_prices(previous_ticker)%>% 
    dplyr::pull(price)
  
  } else {
    pre_month <- previous_month_price
  }
  
  nxt_month <- get_future_prices(nxt_ticker) %>% 
    dplyr::pull(price)
  

  N <- lubridate::days_in_month(meeting)[[1]] #Days in Meeting Month
  M <- lubridate::day(meeting)-1 #Days in month till meeting
  
  
  if((month+1) %in% fed_df$month_no){
    # Type 2 Meeting
    FFER_start <- 100 - pre_month
    implied_rate <- 100 - act_month
    
    FFER_end <- (N/(N-M)) * (implied_rate - (M/N) * FFER_start)
  
  } else {
    # Type 1 Meeting
    implied_rate <- 100 - act_month
    FFER_end <- 100 - nxt_month
    FFER_start <-  (N/M) * (implied_rate - (((N-M)/N) * FFER_end))
    
  }
  
  #Probability of Rate change in Meeting
  p_change <- (FFER_end - FFER_start) / 0.25
  
  
  # Check if Hike (p>0), or cut (p<0) 
  if(sign(p_change)==1|sign(p_change)==0){
    p_unchange <- 1 - p_change
    type = "hike"
  }else{
    p_unchange <- 1 + p_change
    type <- "cut"
  }
  
  probs <- tibble::tibble("meeting" = lubridate::as_date(meeting), 
                          "p_change" = p_change,
                          "p_nochange" = p_unchange,
                          "impl.rate" = FFER_end,
                          "type" = type)
  return(probs)
  
}

all_meetings <- fed_df %>% 
  dplyr::pull(meeting_date)

# * To get the previous month future price (not trading anymore) check out "https://quotes.ino.com/charting/?s=CBOT_ZQ.Z21"
#    You can find the quote there but it isn't scrapeable
#    The previous month price has to be set in the purrr::map_df below to get the first months probs
# * Scraping interval is 5 Seconds. Faster interval is not permitted


meeting_probs <- purrr::map_df(all_meetings, get_hike_prob, previous_month_price = 99.92) %>%
  dplyr::mutate(no.steps = cumsum(p_change)) 


# LinkedIn Plot -----------------------------------------------------------

m <- meeting_probs %>% 
  dplyr::mutate(meeting = factor(meeting)) %>% 
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(meeting, no.steps, group = type), color = palette_main[4])+
  ggplot2::geom_hline(yintercept = c(1,2,3), color = main_color_light, alpha = 0.5, linetype = "dashed")+
  ggplot2::geom_point(ggplot2::aes(meeting, no.steps, size = p_change), colour = palette_main[4]) +
  ggplot2::geom_text(ggplot2::aes(meeting, no.steps, label = scales::percent(p_change, accuracy = 0.01)), 
                     colour = palette_main[1], 
                     nudge_y = 0.19)+
  ggplot2::geom_segment(ggplot2::aes(x = 2.15, xend = 2.3,
                                     y = 0.9, yend = 0.7),
                        color = main_color_light)+
  ggplot2::geom_segment(ggplot2::aes(x = 3.8, xend = 3.6,
                                     y = 2.08, yend = 2.18),
                        color = main_color_light)+
  ggplot2::scale_size_continuous(range = c(1,10),
                                 guide = NULL)+
  ggplot2::annotate(x = 2, 
                    y = 1, 
                    geom = "label", 
                    label = "For the meeting in march,\na 98.8% hike is priced in\nwith a probability to occur\non this meeting date of 95.1%", 
                    color = main_color,
                    hjust = 0,
                    vjust = 1.5)+
  ggplot2::annotate(x = 1.8, 
                    y = 1.6, 
                    geom = "label", 
                    label = 
                    "For the FED's june meeting \n100% of a first hike,\n100% of a second hike and\n8% of a third hike\nare priced in the future. \nThe probability that the second hike\noccurs in this meeting is 71.25%", 
                    color = main_color,
                    hjust = 0,
                    vjust = -0.5)+
  ggplot2::labs(x = "FED Meeting date",
                y = "Rate Hikes priced into\nthe respective month's future",
                size = "Probability of rate hike\nat a given meeting",
                title = "FED Fund Rate Market Expectations 2022",
                subtitle = "The rate hikes priced in at a given FED meeting (1 hike = 25bps)",
                caption = "Futures data by Bloomberg\nMethodology: Own + CME")+
  theme_linkedin_light()


save_linkedin(m, "fed_rate_hike", format = "png")




