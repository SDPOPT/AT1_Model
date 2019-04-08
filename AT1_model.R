library(tidyquant)
library(xgboost)
library(gbm)
library(RSQLite)
library(readxl)
library(xlsx)
library(dygraphs)
library(DT)
library(formattable)
library(RQuantLib)
library(Rblpapi) 
library(corrplot)
library(jtools)
blpConnect()


ID <- tibble( ID = c("GB00BQY78G05 Index", "GB00BQY78F97 Index", 
                     "EU0009658426 Index", "VIXY US Equity",
                     "DXY Curncy", "SPX Index", "GT10 Govt",
                     "GTDEM10Y Govt", "IEF US Equity",
                     "UUP US Equity", "SX7EEX GR Equity")) %>%
  mutate(ticker = bdp(ID, "TICKER")$TICKER)

Price <- bdh(ID$ID, "PX_LAST", Sys.Date() - 3000, Sys.Date() - 1) 

Price <- mapply(function(x, y) {y <- y %>% mutate(ID = x) }, 
       names(Price), Price, 
       USE.NAMES = FALSE, SIMPLIFY = FALSE)
Price <- as_tibble(do.call("bind_rows", Price)) %>%
  left_join(ID) %>%
  select(date, ticker, price = PX_LAST)

lag1 <- data_lag(Price, 1)
lag2 <- data_lag(Price, 2)
lag3 <- data_lag(Price, 3)

Price1 <- rbind(Price, lag1, lag2, lag3) %>% 
  filter(is.na(price) == FALSE) %>%
  group_by(date) %>%
  mutate(count = NROW(ticker)) %>%
  ungroup() %>%
  filter(count == max(count)) %>%
  select(-count)
  

Price1 %>% filter(grepl("lag", ticker) == FALSE) %>%
  group_by(ticker) %>% 
  mutate(cum_return = price / price[1]) %>% 
  ggplot(aes(x = date, y = cum_return, color = ticker)) +
  geom_line()

data_lag <- function(Price, n = 1){
  
  lag_data <- Price %>%
    group_by(ticker) %>%
    arrange(-desc(date)) %>%
    mutate(price = lag(price, k = n)) %>%
    ungroup() %>%
    mutate(ticker = paste(ticker, "_lag", n, sep = ""))
             
}

linear_model <- function(Price, N) {  

train <- Price %>%
  arrange(-desc(date)) %>%
  group_by(ticker) %>%
  filter(row_number(date) <= N) %>%
  mutate(value = price / price[1]) %>%
  ungroup() %>%
  select(date, ticker, value) %>%
  spread(ticker, value)

model <- lm(IBXXC1P1 ~ DXY + SX7E + VIXY, data = train)

}

test <- Price1 %>%
  arrange(-desc(date)) %>%
  group_by(ticker) %>%
  filter(row_number(date) > 300) %>%
  mutate(value = price / price[1]) %>%
  ungroup() %>%
  select(date, ticker, value) %>%
  spread(ticker, value)

summary(at1_model)

result <- test %>%
  mutate(result = predict(at1_model, test))

return <- result %>%
  select(date, IBXXC1D1, IBXXC1P1, result) %>%
  mutate(index_return = percent(IBXXC1D1 - 1),
         model_return = percent(result - 1),
         price_return = percent(IBXXC1P1 - 1),
         strategy = percent(index_return - model_return))

ggplot(data = return) +
  geom_line(aes(x = date, y = index_return)) +
  geom_line(aes(x = date, y = model_return), color = "red") +
  geom_line(aes(x = date, y = price_return), color = "green") +
  geom_line(aes(x = date, y = strategy), color = "blue")

strategy <- return %>%
  select(date, strategy, index_return) %>%
  mutate(strategy = (strategy + 1) / (lag(strategy) + 1) - 1,
         index_return = (index_return + 1) / (lag(index_return) + 1) - 1) %>%
  filter(is.na(strategy) == FALSE,
         is.na(index_return) == FALSE)

strategy <- xts(strategy %>% select(-date), order.by = strategy$date)
Return.annualized(strategy)
StdDev.annualized(strategy)
SharpeRatio.annualized(strategy)
table.InformationRatio(strategy$strategy, strategy$index_return)
maxDrawdown(strategy)

corr <- Price1 %>% 
  select(-cat) %>% spread(key = ticker, value = value) %>% 
  select(-date) %>%
  cor() %>%
  corrplot(type = "upper")

train_data <- train %>%
  select(-date, -IBXXC1D1, -IBXXC1P1) %>%
  as.matrix()

train_label <- train %>%
  select(IBXXC1P1) %>%
  as.matrix()

dtrain <- xgb.DMatrix(data = train_data, label = train_label)
model <- xgboost(data = dtrain,
                 booster = "gblinear",
                 max.depth = 10,
                 eta = 1,
                 nthred = 3,
                 nrounds = 10)

