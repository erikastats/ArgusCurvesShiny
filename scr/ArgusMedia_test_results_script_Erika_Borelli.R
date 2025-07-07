
# Packages ----------------------------------------------------------------

library(data.table)
# library(dplyr)
library(readxl)

# Loading data ------------------------------------------------------------

MARKET <- read_excel("data/data_P2_ds_test.xlsx", 
                              col_types = c("date", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric")) |> 
                     as.data.table()
REFERENCE_MARKET <- read_excel("data/data_P2_ds_test.xlsx", 
                               sheet = "REFERENCE_MARKET",
                               col_types = c("date", "text", "numeric")) |>
                     as.data.table()

# Exploring the data ------------------------------------------------------

MARKET |> head()

REFERENCE_MARKET |> head()

REFERENCE_MARKET$REF_MARKET |> unique()

# Pivoting MARKET to long format ------------------------------------------

MARKET_LONG <- melt(
  MARKET,
  id.vars = "TERM",
  variable.name = 'MARKET',
  value.name = "MARKET_VALUE"
)


# Creating missing TERM values before 2025-03-01 --------------------------

full_terms <- data.table(TERM = as.POSIXct(seq(as.Date("2024-04-01"),
                                    by = "month", length.out = 11))
                         )

all_markets <- MARKET_LONG$MARKET |> unique()

new_grid <- CJ(TERM = full_terms$TERM,
                MARKET = all_markets, unique = TRUE)

new_grid[, `:=`(
  MARKET_VALUE = NA_real_)]

MARKET_LONG_COMPLETE <- rbind(new_grid, MARKET_LONG)

# Creating reference dt ---------------------------------------------------

REFERENCE_MAP <- data.table(
  MARKET = paste0('MARKET_', 1:7),
  REF_MARKET = c("REF_MARKET_2", "MARKET_1", "REF_MARKET_2",
                 "REF_MARKET_1", "REF_MARKET_3", "MARKET_5",
                 "REF_MARKET_2")
)


# Joining MARKET_LONG with REFERENCE_MAP ----------------------------------

MARKET_LONG_REF <- REFERENCE_MAP[MARKET_LONG_COMPLETE, on = .(MARKET)] 

# Getting the first and last known value  ------------------------------

# LATEST_VALUES_LONG <- melt(
#   MARKET[,lapply(.SD, function(col) tail(na.omit(col), 1)), .SDcols = !'TERM'],
#   variable.name =   "MARKET",
#   value.name = 'LATEST_VALUE'
#   )

# First values
first_values <- MARKET_LONG_REF[!is.na(MARKET_VALUE), .SD[1], by = MARKET]
setnames(first_values, old = c('MARKET_VALUE'), new = c('FIRST_VALUE'))
first_values <- first_values[, .(MARKET, FIRST_VALUE)]

# Last Values
last_values  <- MARKET_LONG_REF[!is.na(MARKET_VALUE), .SD[.N], by = MARKET]
setnames(last_values, old = c('MARKET_VALUE'), new = c('LAST_VALUE'))
last_values <- last_values[, .(MARKET, LAST_VALUE)]

# Merge into main table
MARKET_LONG_REF <- merge(MARKET_LONG_REF, first_values, by = "MARKET", all.x = TRUE) |>
  merge( last_values,  by = "MARKET", all.x = TRUE)

# Joining MARKET_LONG_REF with REFERENCE_MARKET ------------------------

MARKET_COMPLETE <- REFERENCE_MARKET[MARKET_LONG_REF, on = .(TERM, REF_MARKET)]

# Creating function to imput market curve ---------------------------------

impute_market_curve <- function(data, backward = FALSE){
  # Step 1: Get base reference value for each MARKET
  data[, BASE_VALUE := VALUE[TERM == max(TERM[!is.na(MARKET_VALUE)])], by = MARKET]
  
  # Step 2: Fill missing MARKET_VALUE using the scaling formula
  if (backward == FALSE){
    data[is.na(MARKET_VALUE), MARKET_VALUE := LAST_VALUE * (VALUE / BASE_VALUE)]
  } else {
    data[is.na(MARKET_VALUE), MARKET_VALUE := FIRST_VALUE * (VALUE / BASE_VALUE)]
  }
  
  data
}


# Applying the function to market different from 2 and 6 ------------------

data = MARKET_COMPLETE[!MARKET %in% c('MARKET_2', 'MARKET_6')]
MARKET_COMPLETE_first <- impute_market_curve(data)


# Applying function to market 2 and 6 -------------------------------------

data_ref <- MARKET_COMPLETE_first[MARKET %in%  c('MARKET_1', 'MARKET_5'), .(TERM, MARKET, MARKET_VALUE)]
setnames(data_ref, old = c('MARKET', 'MARKET_VALUE'), new = c('REF_MARKET', 'VALUE'))
data = MARKET_COMPLETE[MARKET %in% paste0('MARKET_', c(2,6)),  .SD, .SDcols = !'VALUE']
data = data_ref[data, on = .(TERM, REF_MARKET)]
MARKET_COMPLETE_2_6 <-  impute_market_curve(data)


# Joining everything ------------------------------------------------------

MARKET_COMPLETE_all <- rbind(MARKET_COMPLETE_first, MARKET_COMPLETE_2_6)


# Extend reference data back to 2024-04-01 with flat fill
min_date <- as.POSIXct("2024-04-01", tz = "UTC")

MARKET_COMPLETE_all_extended <- MARKET_COMPLETE_all[, .SD, by = REF_MARKET][
  , {
    min_term <- min(TERM)
    if (min_term > min_date) {
      missing_dates <- seq(from = min_date, to = min_term - 60*60*24, by = "month")
      filled <- data.table(TERM = missing_dates, REF_MARKET = REF_MARKET[1], VALUE = VALUE[1])
      rbind(filled, .SD, fill = TRUE)
    } else {
      .SD
    }
  }, by = REF_MARKET]





data = MARKET_COMPLETE_all[!MARKET %in% c('MARKET_2', 'MARKET_6')]
MARKET_COMPLETE_first <- impute_market_curve(data, backward = TRUE)


# Applying function to market 2 and 6 -------------------------------------

data_ref <- MARKET_COMPLETE_first[MARKET %in%  c('MARKET_1', 'MARKET_5'), .(TERM, MARKET, MARKET_VALUE)]
setnames(data_ref, old = c('MARKET', 'MARKET_VALUE'), new = c('REF_MARKET', 'VALUE'))
data = MARKET_COMPLETE[MARKET %in% paste0('MARKET_', c(2,6)),  .SD, .SDcols = !'VALUE']
data = data_ref[data, on = .(TERM, REF_MARKET)]
MARKET_COMPLETE_2_6 <-  impute_market_curve(data)