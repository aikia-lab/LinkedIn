

library(magrittr)

symbol = "AAPL"

url <- glue::glue("https://query2.finance.yahoo.com/v10/finance/quoteSummary/{symbol}?modules=earningsTrend&ssl=true")
res <- jsonlite::fromJSON(url)$quoteSummary$result

flat <- lapply(res, function(x) {
  x[[1]][[1]] %>% dplyr::select(-maxAge)
})


df <- suppressMessages(Reduce(dplyr::full_join, 
                                       flat)) %>% jsonlite::flatten() %>% dplyr::mutate(endDate = lubridate::as_datetime(.data$endDate)) %>% 
                 dplyr::select(growth_period = period, endDate, dplyr::contains(".raw")) %>% 
                 dplyr::relocate("growth.raw", .after = "growth_period") %>% 
                 janitor::clean_names()

colnames(df) <- gsub("_raw", "", colnames(df))

df <- df %>% dplyr::mutate(ticker_yh = symbol) %>% dplyr::relocate(ticker_yh, 
                                                                     .before = "end_date")


