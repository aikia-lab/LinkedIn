



update_ici_flows <- function(){
  
  library(magrittr)
  
  ici_url <- "https://www.ici.org/research/stats/weekly-mfflows"
  ici_url_etf <- "https://www.ici.org/research/stats/weekly-etf"
  
  l <- charToRaw(paste0(readLines(ici_url), collapse = "\n"))
  l_etf <- charToRaw(paste0(readLines(ici_url_etf), collapse = "\n"))
  
  pg <- rvest::read_html(l)
  pg_etf <- rvest::read_html(l_etf)
  
  
  release_date <- pg %>% 
    rvest::html_nodes(".field-top-info") %>% 
    rvest::html_text() %>% 
    .[2] %>% 
    stringr::str_trim() %>% 
    lubridate::mdy()
  
  
  
  # Net Asset ---------------------------------------------------------------
  
  # Mutual Funds 
  base_mutual_aum_url <- "https://www.ici.org/research/stats/trends"
  l_base_mutual_aum_url <- charToRaw(paste0(readLines(base_mutual_aum_url), collapse = "\n"))
  
  mutual_aum_url <- l_base_mutual_aum_url %>% 
    rvest::read_html() %>% 
    rvest::html_nodes(".heading-node") %>% 
    rvest::html_nodes("a") %>% 
    rvest::html_attr("href") %>% 
    stringr::str_c("https://www.ici.org",.)
  
  l_aum <- charToRaw(paste0(readLines(mutual_aum_url), collapse = "\n"))
  
  pg_aum <- rvest::read_html(l_aum)
  
  tables <- pg_aum %>% 
    rvest::html_table(fill = T)
  
  mutual_aum <- tables[[1]] %>% 
    #dplyr::select(-X6) %>% 
    dplyr::mutate(X1 = ifelse(X1 == "", "Classification", X1))
  
  names(mutual_aum) <- mutual_aum[1,]
  
  
  # Find the correct Ultimo date to match -----------------------------------
  
  
  ultimo <- names(mutual_aum)[2] %>% 
    stringr::str_c("01",.) %>% 
    lubridate::dmy() %>% 
    + months(1) %>% 
    - lubridate::days(1)

  total_mutual_aum <- mutual_aum %>% 
    dplyr::filter(Classification != "Classification") %>% 
    dplyr::select(1,"AUM" = 2) %>% 
    dplyr::mutate(AUM = as.numeric(stringr::str_remove(AUM, ",")) * 1000000000,
                  date = ultimo,
                  level_2 =stringr::str_to_lower(
                    stringr::str_trim(
                      stringr::str_remove_all(Classification, "equity|bond")
                    )
                  ),
                  level_3 = dplyr::case_when(
                    stringr::str_detect(level_2, "total") ~ level_2,
                    level_2 == "hybrid" ~ level_2,
                    level_2 == "municipal" ~ level_2,
                    TRUE ~ stringr::str_c("total ", level_2)
                  )
    ) %>% 
    dplyr::select(date, level_3, AUM)
  
  
  
  # Mutual Funds ------------------------------------------------------------
  
  link_to_ici_file <- pg %>% 
    rvest::html_nodes(".heading-node") %>% 
    rvest::html_nodes("a") %>% 
    rvest::html_attr("href") %>% 
    .[stringr::str_detect(.,"flows_data")] %>% 
    stringr::str_c("https://www.ici.org",.)
  
  
  tmp <- tempfile(fileext = ".xls")  
  
  download.file(link_to_ici_file,
                destfile = tmp,
                mode = "wb")
  
  header <- readxl::read_xls(tmp,
                             range = "A4:AN7") %>% 
    janitor::remove_empty(which = c("rows","cols"))
  
  header_full <- header %>% 
    tidyr::fill(dplyr::everything()) %>% 
    tibble::rownames_to_column() %>% 
    dplyr::mutate(rowname = stringr::str_c("level_",rowname)) %>% 
    tidyr::pivot_longer(cols = c(-"rowname"),
                        names_to = "row",
                        values_to = "value") %>% 
    dplyr::group_by(rowname) %>% 
    tidyr::fill(dplyr::everything()) %>% 
    tidyr::pivot_wider(names_from = rowname,
                       values_from = value) %>% 
    dplyr::select(-row) %>% 
    dplyr::filter(level_1 != "Date") %>% 
    dplyr::mutate(dplyr::across(dplyr::everything(),
                                ~stringr::str_trim(
                                  stringr::str_replace_all(
                                    tolower(.x),"\\s{2,}"," ")
                                )
    )
    )
  
  
  body <- readxl::read_xls(tmp,
                           col_names = FALSE,
                           skip = 9) %>% 
    janitor::remove_empty(which = c("rows","cols")) %>% 
    dplyr::mutate(date = lubridate::mdy(...1),
                  group_flag = dplyr::case_when(
                    !is.na(date) ~ 0,
                    TRUE ~ 1
                  ),
                  groups = cumsum(group_flag),
                  ...1 = date) %>% 
    dplyr::filter(groups < 2,
                  !is.na(date)) %>% 
    dplyr::group_by(groups) %>% 
    dplyr::group_split() %>% 
    setNames(c("body_ultimo","body_weekly_forecast")) %>% 
    purrr::map(.,~dplyr::select(., -c(date, groups, group_flag)))
  
  
  
  body_ultimo <- body$body_ultimo %>% 
    tidyr::pivot_longer(cols = -c("...1"),
                        names_to = "index",
                        values_to = "value") %>% 
    tidyr::pivot_wider(names_from = ...1,
                       values_from = value) %>% 
    dplyr::select(-index)
  
  
  body_weekly_forecast <- body$body_weekly_forecast %>% 
    tidyr::pivot_longer(cols = -c("...1"),
                        names_to = "index",
                        values_to = "value") %>% 
    tidyr::pivot_wider(names_from = ...1,
                       values_from = value) %>% 
    dplyr::select(-index)
  
  
  # Combine Mutual Funds with AUM -------------------------------------------
  
  table_total_ultimo <- cbind(header_full, body_ultimo) %>% 
    tidyr::pivot_longer(cols = -c(1:3),
                        names_to = "date",
                        values_to = "net_flow") %>% 
    dplyr::mutate(date = lubridate::as_date(date),
                  net_flow = net_flow * 1000000,
                  last_update = release_date) %>% 
    dplyr::ungroup() %>% 
    janitor::clean_names()
  
  
  table_weekly_forecast <- cbind(header_full, body_weekly_forecast) %>% 
    tidyr::pivot_longer(cols = -c(1:3),
                        names_to = "date",
                        values_to = "net_flow") %>% 
    dplyr::mutate(date = lubridate::as_date(date),
                  net_flow = net_flow * 1000000,
                  last_update = release_date) %>% 
    dplyr::ungroup() %>% 
    janitor::clean_names()  
  

  unlink(tmp)
  closeAllConnections()
  
  
  
  
  # ETF ---------------------------------------------------------------------
  
  
  link_to_ici_file_etf <- pg_etf %>% 
    rvest::html_nodes(".heading-node") %>% 
    rvest::html_nodes("a") %>% 
    rvest::html_attr("href") %>% 
    .[stringr::str_detect(.,"flows_data")] %>% 
    stringr::str_c("https://www.ici.org",.)
  
  
  tmp_etf <- tempfile(fileext = ".xls")  
  
  download.file(link_to_ici_file_etf,
                destfile = tmp_etf,
                mode = "wb")
  
  
  
  # Net Assets --------------------------------------------------------------
  
  base_ici_etf_aum_url <- "https://ici.org/research/stats/etf"
  
  l_base_etf_aum_url <- charToRaw(paste0(readLines(base_ici_etf_aum_url), collapse = "\n"))
  
  etf_aum_url <- l_base_etf_aum_url %>% 
    rvest::read_html() %>% 
    rvest::html_nodes(".heading-node") %>% 
    rvest::html_nodes("a") %>% 
    rvest::html_attr("href") %>% 
    stringr::str_c("https://www.ici.org",.)
  
  l_etf_aum <- charToRaw(paste0(readLines(etf_aum_url), collapse = "\n"))
  
  pg_etf_aum <- rvest::read_html(l_etf_aum)
  
  tables_etf <- pg_etf_aum %>% 
    rvest::html_table(fill = T)
  
  etf_aum <- tables_etf[[1]] %>% 
    #dplyr::select(-X6) %>% 
    dplyr::mutate(X1 = ifelse(X1 == "", "Classification", X1))
  
  names(etf_aum) <- etf_aum[1,]
  
  etf_ultimo <- names(etf_aum)[3] %>% 
    stringr::str_c("01 ",.) %>% 
    lubridate::dmy() %>% 
    + months(1) %>% 
    - lubridate::days(1)
  
  
  # AUM is the net flow in addition to the price changes in the marketa
  # whereas the weekly estimated flows are dollar dominated liqui flows
  total_etf_aum <- etf_aum %>% 
    dplyr::select(-2) %>% 
    dplyr::filter(Classification != "Classification") %>% 
    dplyr::select(1,"AUM" = 3) %>% 
    dplyr::mutate(AUM = as.numeric(stringr::str_remove(AUM, ",")) * 1000000000,
                  date = etf_ultimo,
                  level_2 =stringr::str_to_lower(
                    stringr::str_trim(
                      stringr::str_remove_all(Classification, "equity|bond")
                    )
                  ),
                  level_3 = dplyr::case_when(
                    level_2 == "all" ~ "total etfs",
                    level_2 == "global/international equity" ~ "total world",
                    level_2 == "commodities" ~ "total commodity",
                    level_2 == "total domestic equity" ~ "total domestic",
                    stringr::str_detect(level_2, "total") ~ level_2,
                    TRUE ~ stringr::str_c("total ", level_2)
                  )
    ) %>% 
    dplyr::select(date, level_3, AUM) %>% 
    tidyr::pivot_wider(names_from = level_3,
                       values_from = AUM) %>% 
    dplyr::mutate(`total equity` = `total domestic` + `total world`) %>% 
    tidyr::pivot_longer(cols = c(dplyr::starts_with("total")),
                        names_to = "level_3",
                        values_to = "AUM") %>% 
    dplyr::mutate(level_3 = ifelse(level_3 == "total hybrid", "hybrid", level_3))
  
  
  
  # combine ETF and AUM -----------------------------------------------------
  
  
  header_etf <- readxl::read_xls(tmp_etf,
                                 range = "A4:AN7") %>% 
    janitor::remove_empty(which = c("rows","cols"))
  
  header_full_etf <- header_etf %>% 
    tidyr::fill(dplyr::everything()) %>% 
    tibble::rownames_to_column() %>% 
    dplyr::mutate(rowname = stringr::str_c("level_",rowname)) %>% 
    tidyr::pivot_longer(cols = c(-"rowname"),
                        names_to = "row",
                        values_to = "value") %>% 
    dplyr::group_by(rowname) %>% 
    tidyr::fill(dplyr::everything()) %>% 
    tidyr::pivot_wider(names_from = rowname,
                       values_from = value) %>% 
    dplyr::select(-row) %>% 
    dplyr::filter(level_1 != "Date") %>% 
    dplyr::mutate(dplyr::across(dplyr::everything(),
                                ~stringr::str_trim(
                                  stringr::str_replace_all(
                                    tolower(.x),"\\s{2,}"," ")
                                )
    )
    ) %>% 
    dplyr::mutate(dplyr::across(
      .cols = level_2,
      .fns = ~ifelse(stringr::str_detect(.x, "etf"), .x,
                     ifelse(stringr::str_detect(.x, "total"),
                            paste(.x, level_1),
                            .x))),
      dplyr::across(
        .cols = level_2,
        .fns = ~ifelse(stringr::str_detect(.x, "total|hybrid|municipal"), .x,
                       paste("total", .x)),
        .names = "level_3"))
  
  
  body_etf <- readxl::read_xls(tmp_etf,
                               col_names = FALSE,
                               skip = 8) %>% 
    janitor::remove_empty(which = c("rows","cols")) %>% 
    dplyr::mutate(date = lubridate::mdy(...1),
                  group_flag = dplyr::case_when(
                    !is.na(date) ~ 0,
                    TRUE ~ 1
                  ),
                  groups = cumsum(group_flag),
                  ...1 = date) %>% 
    dplyr::filter(groups < 2,
                  !is.na(date)) %>% 
    dplyr::group_by(groups) %>% 
    dplyr::group_split() %>% 
    setNames(c("body_ultimo","body_weekly_forecast")) %>% 
    purrr::map(.,~dplyr::select(., -c(date, groups, group_flag)))
  
  
  
  body_ultimo_etf <- body_etf$body_ultimo %>% 
    tidyr::pivot_longer(cols = -c("...1"),
                        names_to = "index",
                        values_to = "value") %>% 
    tidyr::pivot_wider(names_from = ...1,
                       values_from = value) %>% 
    dplyr::select(-index)
  
  
  body_weekly_forecast_etf <- body_etf$body_weekly_forecast %>% 
    tidyr::pivot_longer(cols = -c("...1"),
                        names_to = "index",
                        values_to = "value") %>% 
    tidyr::pivot_wider(names_from = ...1,
                       values_from = value) %>% 
    dplyr::select(-index)
  
  
  
  table_total_ultimo_etf <- cbind(header_full_etf, body_ultimo_etf) %>% 
    tidyr::pivot_longer(cols = -c(1:3),
                        names_to = "date",
                        values_to = "net_flow") %>% 
    dplyr::mutate(date = lubridate::as_date(date),
                  net_flow = net_flow * 1000000,
                  last_update = release_date) %>% 
    dplyr::ungroup() %>% 
    janitor::clean_names()
  
  
  
  table_weekly_forecast_etf <- cbind(header_full_etf, body_weekly_forecast_etf) %>% 
    tidyr::pivot_longer(cols = -c(1:3),
                        names_to = "date",
                        values_to = "net_flow") %>% 
    dplyr::mutate(date = lubridate::as_date(date),
                  net_flow = net_flow * 1000000,
                  last_update = release_date) %>% 
  dplyr::ungroup() %>% 
    janitor::clean_names() 
  
  
  
  unlink(tmp)
  closeAllConnections()
  
  
  return(
    list(
      mutual = list(
        ultimo_flows = table_total_ultimo,
        weekly_forecast = table_weekly_forecast,
        monthly_AUM = total_mutual_aum),
      etf = list(
        ultimo_flows = table_total_ultimo_etf,
        weekly_forecast = table_weekly_forecast_etf,
        monthly_AUM = total_etf_aum)
    )
  )
}






