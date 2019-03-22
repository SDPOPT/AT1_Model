library(tidyverse)
library(RSQLite)
library(readxl)
library(xlsx)
library(dygraphs)
library(DT)
library(xts)
library(formattable)
library(RQuantLib)
library(Rblpapi) 
library(corrplot)
blpConnect()


ID <- tibble( ID = c("GB00BQY78G05 Index", "GB00BQY78F97 Index", 
                     "EU0009658426 Index", "US06746L4225 US Equity",
                     "VGIT US Equity", "DXY Curncy", "SPX Index",
                     "IYG3X Index", "USGG10YR  Index")) %>%
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
  select(date, ticker, price = PX_LAST) %>%
  group_by(ticker) %>%
  arrange(-desc(date)) %>%
  mutate(return = price / lag(price) - 1,
         return = ifelse(is.na(return) == TRUE, 0, return)) %>%
  ungroup() %>%
  gather(price, return, key = "cat", value = "value")
  
key <- "return"

train <-Price1 %>%
  filter(cat == key) %>%
  arrange(-desc(date)) %>%
  group_by(ticker) %>%
  ungroup() %>%
  spread(ticker, value) %>%
  filter(row_number(date) <= 500)

test <- Price1 %>%
  filter(cat == key) %>%
  arrange(-desc(date)) %>%
  group_by(ticker) %>%
  ungroup() %>%
  spread(ticker, value) %>%
  filter(row_number(date) > 500)

at1_model <- lm(IBXXC1P1 ~ DXY + SX7E + VXX, data = train)
summary(at1_model)

anova(at1_model)

result <- test %>%
  mutate(result = predict(at1_model, test))

return <- result %>%
  select(date, IBXXC1D1, result) %>%
  mutate(index_return = percent(IBXXC1D1 / IBXXC1D1[1] - 1),
         model_return = percent(result / result[1] - 1),
         strategy = percent(index_return - model_return))

ggplot(data = return) +
  geom_line(aes(x = date, y = index_return)) +
  geom_line(aes(x = date, y = model_return), color = "red") +
  geom_line(aes(x = date, y = strategy), color = "blue")
