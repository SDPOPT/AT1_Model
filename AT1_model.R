library(tidyquant)
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
                     "DXY Curncy", "SPX Index", "USGG10YR  Index",
                     "FEZ US Equity", "SX7EEX GY Equity",
                     "IEF US Equity")) %>%
  mutate(ticker = bdp(ID, "TICKER")$TICKER)

Price <- bdh(ID$ID, "PX_LAST", Sys.Date() - 3000, Sys.Date() - 1) 

Price <- mapply(function(x, y) {y <- y %>% mutate(ID = x) }, 
       names(Price), Price, 
       USE.NAMES = FALSE, SIMPLIFY = FALSE)
Price <- as_tibble(do.call("bind_rows", Price)) %>%
  left_join(ID)

Price1 <- Price %>% 
  filter(is.na(PX_LAST) == FALSE) %>%
  group_by(date) %>%
  mutate(count = NROW(ticker)) %>%
  ungroup() %>%
  filter(count == max(count)) %>%
  select(date, ticker, price = PX_LAST)
  
train <- Price1 %>%
  arrange(-desc(date)) %>%
  group_by(ticker) %>%
  filter(row_number(date) <= 500) %>%
  mutate(value = price / price[1]) %>%
  ungroup() %>%
  select(date, ticker, value) %>%
  spread(ticker, value)
 
test <- Price1 %>%
  arrange(-desc(date)) %>%
  group_by(ticker) %>%
  filter(row_number(date) > 500) %>%
  mutate(value = price / price[1]) %>%
  ungroup() %>%
  select(date, ticker, value) %>%
  spread(ticker, value)

at1_model <- lm(IBXXC1P1 ~ DXY + SX7E + VIXY + IEF, data = train)
summary(at1_model)
anova(at1_model)

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

