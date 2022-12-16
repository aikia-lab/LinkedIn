

library(magrittr)

# Input Variables ---------------------------------------------------------

decay <- 0.97
weights <- c(0.33,0.33,0.33)

portf <- c("ADBE","BA","LCID")

returnlist <- list()


sample_ts_raw <- portf %>% 
    tidyquant::tq_get(.,
                      from = "2020-01-01") %>% 
    dplyr::as_tibble()
  

sample_ts <- sample_ts_raw %>% 
  tidyr::pivot_wider(date,names_from = symbol, values_from = adjusted) %>% 
  tidyr::drop_na() %>% 
  janitor::clean_names()



# Create History Function -------------------------------------------------


VaR_history <- function(i){
  
  
  price_table <- sample_ts %>% 
    dplyr::arrange(desc(date)) %>% 
    .[i:(i+250),] %>% 
    dplyr::mutate(dplyr::across(2:4,
                                .fns = ~.x/dplyr::lead(.x)-1,
                                .names = "{.col}_return")) %>% 
    tidyr::drop_na() %>% 
    dplyr::mutate(decay_factor = (1-decay) * decay^(dplyr::row_number()-1)) # decay factor (weights sum up to 1))
  
  Vola_1 <- sqrt(t(as.matrix(price_table[,5]))%*% (as.matrix(price_table[,5])*price_table$decay_factor))
  Vola_2 <- sqrt(t(as.matrix(price_table[,6]))%*% (as.matrix(price_table[,6])*price_table$decay_factor))
  Vola_3 <- sqrt(t(as.matrix(price_table[,7]))%*% (as.matrix(price_table[,7])*price_table$decay_factor))
  
  FVaR_1 <- Vola_1 * weights[1] * 2.33#stats::quantile(dplyr::pull(price_table[,5]),c(0.01)) * -100 # *-1 to get negative sign
  FVaR_2 <- Vola_2 * weights[2] * 2.33#stats::quantile(dplyr::pull(price_table[,6]),c(0.01)) * -100
  FVaR_3 <- Vola_3 * weights[3] * 2.33#stats::quantile(dplyr::pull(price_table[,7]),c(0.01)) * -100  
  
  
  # Combine for Export
  FVaR <- rbind(FVaR_1 %>% as.data.frame() %>% `colnames<-`("FVaR") %>% `rownames<-`(portf[1]),
                FVaR_2 %>% as.data.frame() %>% `colnames<-`("FVaR") %>% `rownames<-`(portf[2]),
                FVaR_3 %>% as.data.frame() %>% `colnames<-`("FVaR") %>% `rownames<-`(portf[3])
                )
  FVaR$date = price_table %>% dplyr::filter(date == max(date)) %>% dplyr::pull(date)
  
  
  # CoVarianz Matrix
  covariance <- t(as.matrix(price_table[,5:7])) %*% (as.matrix(price_table[,5:7])*price_table$decay_factor)
  
  
  # Correlation Matrix
  correlation <- covariance/(as.matrix(c(Vola_1,Vola_2,Vola_3)) %*% t(as.matrix(c(Vola_1,Vola_2,Vola_3))))
  
  
  # adjust names for export
  correlation <- correlation %>% `rownames<-`(portf) %>% `colnames<-`(portf)
  
  
  # Value at Risk
  VaR_df <- tibble::tibble(VaR = as.numeric(
    sqrt((t(as.matrix(c(FVaR_1,FVaR_2,FVaR_3))) %*% correlation) %*% as.matrix(c(FVaR_1,FVaR_2,FVaR_3)))),
                          date = price_table %>% dplyr::filter(date == max(date)) %>% dplyr::pull(date))
  
  returnlist$VaR <- VaR_df
  returnlist$correlation <- correlation
  returnlist$FVaRs <- FVaR
  returnlist$decay <- scales::percent(1-decay, suffix = " %")
  
  return(returnlist)
}




var_histo <- purrr:::map(1:300, VaR_history)



# VaR timeline
var_plot <- purrr::map(var_histo,1) %>% 
  tibble::enframe() %>% 
  tidyr::unnest(value) %>% 
  plotly::plot_ly(x = ~date, y = ~VaR,
                  type = 'scatter',
                  mode = 'lines',
                  name = "Portfolio VaR",
                  color = I(aikia::aikia_main()),
                  hovertemplate = ~paste0('Date: %{x}',
                                          '<br><b>Portfolio VaR:</b> %{y:.2%}<extra></extra>')) %>% 
  plotly::layout(title = paste0("Portfolio VaR with decay factor: ",var_histo[[1]]$decay),
                 yaxis = list(tickformat = "0.1%"))


# FVaR time line
fvar_plot <- purrr::map(var_histo,3) %>% 
  Map(cbind, ., ticker = lapply(.,rownames)) %>% 
  tibble::enframe() %>% 
  tidyr::unnest(value) %>% 
  plotly::plot_ly(x = ~date, y = ~FVaR,
                  type = 'scatter',
                  mode = 'lines',
                  color = ~ticker,
                  colors = aikia::aikia_palette_main()[2:4],
                  hovertemplate = ~paste0('Date: %{x}<br>',
                          ticker,
                          '<br><b>Factor VaR:</b> %{y:.2%}<extra></extra>')) %>% 
  plotly::layout(title = "Factor Value-at-Risk of each Stock",
                 yaxis = list(tickformat = "0.1%"),
                 annotations = list(x = 0.2 , y = 1.05, text = "Factor Value-at-Risk of each Stock", showarrow = F, 
                                    xref='paper', yref='paper'))
  

# rm(cor_df)
# Correlation time line
cor_df <- tibble::tibble()
for(i in 1:length(var_histo)){
  df_tmp <- purrr::map(var_histo,2)[[i]]
  cor_new <- as.data.frame(as.table(df_tmp))
  cor_new$date <- var_histo[[i]]$VaR$date # save date of current CORRELATION matrix
  cor_df <- rbind(cor_df,cor_new) 
}


cor_plot <- cor_df %>% 
  dplyr::as_tibble() %>% 
  dplyr::filter(Var1 == portf[1],
                Var2 %in% c(portf[2:3])) %>% 
  plotly::plot_ly(x = ~date, y = ~Freq,
                  type = 'scatter',
                  mode = 'lines',
                  showlegend = F,
                  color = ~Var2,
                  colors = aikia::aikia_palette_main()[2:4],
                  hovertemplate = ~paste0('Date: %{x}<br>',
                                          Var1,'-',Var2,
                                          '<br><b>Correlation:</b> %{y:.2f}<extra></extra>')) %>% 
  plotly::layout(annotations = list(x = 0.2 , y = 1.05, text = paste0("Correlation with ",portf[1]), showarrow = F, 
                                    xref='paper', yref='paper'),
                 title = paste0("Correlation with ",portf[1]))
  

# PnL time line
ts_period <- purrr::map(var_histo,1) %>% 
  tibble::enframe() %>% 
  tidyr::unnest(value) %>% 
  dplyr::summarise(min_date = min(date),
                   max_date = max(date))


pnl_plot <- sample_ts %>% 
  dplyr::filter(date >= ts_period$min_date & date <= ts_period$max_date) %>% 
  dplyr::mutate(dplyr::across(2:4,
                              .fns = ~.x/dplyr::lead(.x)-1,
                              .names = "{.col} return")) %>% 
  dplyr::select(date, dplyr::contains('return')) %>% 
  tidyr::pivot_longer(-1, names_to = 'ticker',values_to = 'pnl') %>% 
  plotly::plot_ly(x = ~date, y = ~pnl,
                  type = 'bar',
                  color = ~ticker,
                  showlegend=F,
                  colors = aikia::aikia_palette_main()[2:4],
                  hovertemplate = ~paste0('Date: %{x}<br>',
                                          ticker,
                                          '<br><b>PnL:</b> %{y:.2%}<extra></extra>')) %>% 
  plotly::layout(title = "PnL timeline of each stock",
                 yaxis = list(tickformat = "0.1%"),
                 annotations = list(x = 0.2 , y = 1.05, text = "PnL time line of each Stock", showarrow = F, 
                                    xref='paper', yref='paper'))


# Combine all Plots
plotly::subplot(var_plot,fvar_plot,cor_plot,pnl_plot, nrows = 4, shareX = T) %>% 
  plotly::layout(title = paste0('<b>Portfolio Sensitivities with a decay factor of ',var_histo[[1]]$decay,
                                '</b> <br> <i>Portfolio VaR, Faktor VaR, Correlation, PnL History</i>'),
                 legend=list(title=list(text='<b> Stocks </b>')),
                 hovermode = "x unified")






















