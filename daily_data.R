#setwd("~/Investment/sagd_stocks")

library(tidyverse)
library(tidyquant)
library(jsonlite)

sagd_tickers <- c("ATH.TO", "CJ.TO", "SCR.TO")
market_ticker <- c("XEG.TO")
all_tickers <- c(sagd_tickers, market_ticker)

price_data <- tq_get(all_tickers, 
                     get  = "stock.prices", 
                     from = "2024-01-01")

price_data <- price_data |>
        mutate(symbol = str_remove(symbol, ".TO")) |>
        select(date, symbol, volume, adjusted) |>
        filter(date < Sys.Date())

wide_prices <- price_data |>
        select(date, symbol, adjusted) |> 
        pivot_wider(names_from = symbol, 
                    values_from = adjusted)

complete_check <- sum(is.na(wide_prices)) == 0
date_check <- max(price_data$date)

price_data <- price_data |>
        group_by(symbol) |>
        arrange(date) |>
        mutate(daily_return = (adjusted / lag(adjusted)) - 1,
               ma_20  = SMA(adjusted, n = 20),
               ma_50  = SMA(adjusted, n = 50),
               ma_200 = SMA(adjusted, n = 200),
               vol_sma5   = SMA(volume, n = 5),
               log_vol    = log(vol_sma5),
               vol_zscore = (log_vol - SMA(log_vol, n = 50)) / runSD(log_vol, n = 50),
               vwap_20        = VWAP(adjusted, volume, n = 20),
               vwap_dev       = (adjusted / vwap_20) - 1,
               amihud         = (abs(daily_return) * 100) / ((adjusted * volume) / 1e6),
               illiquidity_20 = SMA(amihud, n = 20)) |>
        ungroup() |>
        select(-vol_sma5, -log_vol, -vwap_20, -amihud)

price_data <- price_data |>
        mutate(daily_return   = round(daily_return, 4) * 100,
               vol_zscore     = round(vol_zscore, 4),
               vwap_dev       = round(vwap_dev * 100, 2),
               illiquidity_20 = round(illiquidity_20, 2),
               across(c(adjusted, ma_20:ma_200), \(x) round(x, digits = 2)))

wide_returns <- price_data |>
        select(date, symbol, daily_return) |>
        pivot_wider(names_from = symbol, values_from = daily_return) |>
        drop_na()

pca_returns <- wide_returns |>
        filter(date >= (max(date) - 90))

pca_fit <- prcomp(pca_returns |> select(-date), center = TRUE, scale. = TRUE)

pca_scores <- pca_returns |>
        select(date) |>
        bind_cols(as_tibble(pca_fit$x)) |>
        mutate(across(where(is.numeric), \(x) round(x, 4)))

pca_loadings <- as_tibble(pca_fit$rotation, rownames = "symbol") |>
        mutate(across(where(is.numeric), \(x) round(x, 4)))

if (sum(pca_loadings$PC1) < 0) {
        
        pca_scores$PC1   <- pca_scores$PC1 * -1
        pca_loadings$PC1 <- pca_loadings$PC1 * -1
        
}

pca_variance <- as.list(summary(pca_fit)$importance[2, ])

pca_list <- list(scores = pca_scores,
                 loadings = pca_loadings,
                 variance_explained = pca_variance)

wide_vol <- price_data |>
        filter(symbol != "XEG") |> 
        select(date, symbol, vol_zscore) |>
        pivot_wider(names_from = symbol, values_from = vol_zscore) |>
        drop_na()

pca_vol_data <- wide_vol |>
        filter(date >= (max(date) - 90))

pca_vol_fit <- prcomp(pca_vol_data |> select(-date), center = TRUE, scale. = TRUE)

pca_vol_scores <- pca_vol_data |>
        select(date) |>
        bind_cols(as_tibble(pca_vol_fit$x)) |>
        mutate(across(where(is.numeric), \(x) round(x, 4)))

pca_vol_loadings <- as_tibble(pca_vol_fit$rotation, rownames = "symbol") |>
        mutate(across(where(is.numeric), \(x) round(x, 4)))

if (sum(pca_vol_loadings$PC1) < 0) {
        
        pca_vol_scores$PC1   <- pca_vol_scores$PC1 * -1
        pca_vol_loadings$PC1 <- pca_vol_loadings$PC1 * -1
        
}

pca_vol_variance <- as.list(summary(pca_vol_fit)$importance[2, ])

pca_vol_list <- list(scores = pca_vol_scores,
                     loadings = pca_vol_loadings,
                     variance_explained = pca_vol_variance)

app_data <- list(is_complete = complete_check,
                 last_date = date_check,
                 prices = price_data,
                 pca = pca_list,
                 pca_volume = pca_vol_list) 

write_json(app_data, "app_data.json", pretty = TRUE, auto_unbox = TRUE)
