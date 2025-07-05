
# Packages ----------------------------------------------------------------

library(data.table)
library(dplyr)
library(readxl)

# Loading data ------------------------------------------------------------

MARKET <- read_excel("data/data_P2_ds_test.xlsx", 
                              col_types = c("date", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric"))
REFERENCE_MARKET <- read_excel("data/data_P2_ds_test.xlsx", 
                               sheet = "REFERENCE_MARKET", col_types = c("date", 
                                                                         "text", "numeric"))

# Exploring the data ------------------------------------------------------

MARKET |> glimpse()

REFERENCE_MARKET |> glimpse()

REFERENCE_MARKET$REF_MARKET |> unique()



# Converting tables to data.table -----------------------------------------

MARKET_dt <- as.data.table(MARKET)
REFERENCE_MARKET_dt <- as.data.table(REFERENCE_MARKET)


# Creating the latest available value  ------------------------------

latest_values <- MARKET_dt[,lapply(.SD, function(col) tail(na.omit(col), 1)), .SDcols = !'TERM']


# Pivoting MARKET to long format ------------------------------------------

MARKET_LONG <- melt(
  MARKET_dt,
  id.vars = "TERM",
  variable.name = 'MARKET',
  value.name = "MARKET_VALUE"
)


# Creating reference dt ---------------------------------------------------

REFERENCE_MAP <- data.table(
  MARKET = paste0('MARKET_', 1:7),
  REF_MARKET = c("REF_MARKET_2", "MARKET_1", "REF_MARKET_2",
                 "REF_MARKET_1", "REF_MARKET_3", "MARKET_5",
                 "REF_MARKET_2")
)


# Joining MARKET_LONG with REFERENCE_MAP ----------------------------------

MARKET_LONG_REF <- MARKET_LONG[REFERENCE_MAP, on = .(MARKET)] 


# Joining MERKET_LONG_REF with REFERENCE_MARKET_dt ------------------------

MARKET_COMPLETE <- MARKET_LONG_REF[REFERENCE_MARKET_dt, on = .(TERM, REF_MARKET)]


latest_values_LONG <- melt(
  latest_values,
  variable.name = 'MARKET',
  value.name = "LATEST_VALUE"
)

MARKET_COMPLETE <- MARKET_COMPLETE[latest_values_LONG, on = .(MARKET)]


# Step 1: Get base reference value for each MARKET
MARKET_COMPLETE[, BASE_VALUE := VALUE[TERM == max(TERM[!is.na(MARKET_VALUE)])], by = MARKET]

# Step 2: Fill missing MARKET_VALUE using the scaling formula
MARKET_COMPLETE[is.na(MARKET_VALUE), MARKET_VALUE := LATEST_VALUE * (VALUE / BASE_VALUE)]
