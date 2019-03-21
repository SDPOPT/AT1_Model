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
                     "USGG10YR  Index", "DXY Curncy")) %>%
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
  spread(ticker, price)
  

train <-Price1 %>%
  arrange(-desc(date)) %>%
  filter(row_number(date) <= 1000)

test <- Price1 %>%
  arrange(-desc(date)) %>%
  filter(row_number(date) > 1000)

at1_model <- lm(IBXXC1P1 ~ SX7E + VXX + USGG10YR, data = train)
summary(at1_model)

anova(at1_model)

test <- test %>%
  mutate(result = predict(at1_model, test))

ggplot(data = test) +
  # geom_line(aes(x = date, y = IBXXC1P1)) +
  # geom_line(aes(x = date, y = result), color = "blue") +
  geom_area(aes(x = date, y = result - IBXXC1P1))

