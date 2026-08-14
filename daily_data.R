#setwd("~/Investment/sagd_stocks")

library(tidyverse)
library(tidyquant)
library(jsonlite)
library(mclust)

# Custom function to compute rolling GMM for Volatility Regimes

calculate_rolling_regime <- function(returns, window_size = 126) {
        n <- length(returns)
        
        # Initialize empty vectors to hold our outputs
        regime_out <- rep(NA_character_, n)
        prob_out   <- rep(NA_real_, n)
        
        if (n < window_size) {
                return(data.frame(regime = regime_out, high_vol_prob = prob_out))
        }
        
        for (i in window_size:n) {
                # Extract the trailing window of returns and scale by 100
                window_data <- returns[(i - window_size + 1):i] * 100
                
                # Catch flat data arrays to prevent crashes
                if (sd(window_data, na.rm = TRUE) == 0) next
                
                # Fit a 2-state Gaussian Mixture Model
                # modelNames = "V" allows the two states to have Variable (different) variances
                mod_fit <- tryCatch({
                        Mclust(window_data, G = 2, modelNames = "V", verbose = FALSE)
                }, error = function(e) NULL)
                
                # If the model fails to converge, skip to the next day
                if (is.null(mod_fit) || is.null(mod_fit$parameters$variance$sigmasq)) next
                
                # Extract the variance (sigma squared) of the identified states
                vars <- mod_fit$parameters$variance$sigmasq
                
                # Extract the probability that the LAST day (today) belongs to each state
                # mod_fit$z is a matrix where columns are states and rows are days
                today_probs <- mod_fit$z[nrow(mod_fit$z), ]

                # Find which state has the larger variance
                if (length(vars) == 2) {
                        if (vars[2] > vars[1]) {
                                # State 2 is the volatile one
                                prob_high <- today_probs[2]
                        } else {
                                # State 1 is the volatile one
                                prob_high <- today_probs[1]
                        }
                } else {
                        # Fallback if the model forces a 1-state solution (extremely rare)
                        prob_high <- 0 
                }
                
                # Assign outputs
                prob_out[i] <- round(prob_high, 4)
                regime_out[i] <- ifelse(prob_high > 0.5, "High Volatility", "Low Volatility")
        }
        
        return(data.frame(regime = regime_out, high_vol_prob = prob_out))
}

# Data extraction and processing

sagd_tickers <- c("ATH.TO", "CJ.TO", "SCR.TO")
market_ticker <- c("XEG.TO")
oil_tickers <- c("CL=F", "CLM27.NYM")
all_tickers <- c(sagd_tickers, market_ticker)

# NETWORK RETRY LOOP (Max 3 Attempts, 1-Min Pause)

max_retries <- 3
attempt <- 0
sum_na <- 0

while(attempt <= max_retries) {
        price_data <- tq_get(all_tickers, 
                             get  = "stock.prices", 
                             from = "2024-01-01") |>
                mutate(symbol = str_remove(symbol, ".TO")) |>
                select(date, symbol, volume, adjusted) |>
                filter(date < Sys.Date())
        
        wide_prices <- price_data |>
                select(date, symbol, adjusted) |> 
                pivot_wider(names_from = symbol, values_from = adjusted)
        
        sum_na <- sum(is.na(wide_prices))
        
        if (sum_na == 0) {
                break
        } else {
                if (attempt < max_retries) {
                        message(paste("NAs found. Retrying in 60 seconds... (Attempt", attempt + 1, "of", max_retries, ")"))
                        Sys.sleep(60)
                }
                attempt <- attempt + 1
        }
}

# Capture the boolean before applying imputation algorithms

complete_check <- (sum_na == 0)
date_check <- max(price_data$date)

# IMPUTATION CASCADE (Average Peer Return -> Midpoint)

if (sum_na > 0) {

        wide_for_impute <- price_data |>
                select(date, symbol, adjusted) |>
                pivot_wider(names_from = symbol, values_from = adjusted) |>
                arrange(date)
        
        symbols_to_fix <- unique(price_data$symbol)
        
        # 2. Rule A: Sequential Average Peer Return Imputation
        for (i in 2:nrow(wide_for_impute)) {
                # Calculate daily returns for available symbols on day i
                day_returns <- numeric()
                for (sym in symbols_to_fix) {
                        if (!is.na(wide_for_impute[[sym]][i]) && !is.na(wide_for_impute[[sym]][i-1])) {
                                day_returns <- c(day_returns, (wide_for_impute[[sym]][i] / wide_for_impute[[sym]][i-1]) - 1)
                        }
                }
                
                avg_ret <- mean(day_returns, na.rm = TRUE)
                
                # Apply average return to missing symbols
                for (sym in symbols_to_fix) {
                        if (is.na(wide_for_impute[[sym]][i]) && !is.na(wide_for_impute[[sym]][i-1]) && !is.nan(avg_ret)) {
                                wide_for_impute[[sym]][i] <- wide_for_impute[[sym]][i-1] * (1 + avg_ret)
                        }
                }
        }
        
        # 3. Rule B: Fallback to Linear Midpoint Interpolation (if peer average was NaN)
        wide_for_impute <- wide_for_impute |>
                mutate(across(-date, ~ zoo::na.approx(.x, na.rm = FALSE)))
        
        # 4. Rebuild cleanly filled price_data and interpolate missing volumes
        price_data <- wide_for_impute |>
                pivot_longer(-date, names_to = "symbol", values_to = "adjusted") |>
                left_join(price_data |> select(date, symbol, volume), by = c("date", "symbol")) |>
                group_by(symbol) |>
                mutate(volume = zoo::na.approx(volume, na.rm = FALSE)) |>
                ungroup()
}


oil_data <- tq_get(oil_tickers, 
                   get  = "stock.prices", 
                   from = "2024-01-01") |>
        select(date, symbol, adjusted) |>
        pivot_wider(names_from = symbol, values_from = adjusted) |>
        rename(wti_front = `CL=F`, wti_12m = `CLM27.NYM`) |>
        arrange(date)

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
        mutate(calculate_rolling_regime(daily_return, window_size = 126)) |>
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
                 sum_na = sum_na,
                 last_date = date_check,
                 prices = price_data,
                 pca = pca_list,
                 pca_volume = pca_vol_list,
                 oil = oil_data) 

write_json(app_data, "app_data.json", pretty = TRUE, auto_unbox = TRUE)
