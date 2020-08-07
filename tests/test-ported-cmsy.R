


# setup -------------------------------------------------------------------
library(tidyverse)

library(R2jags)  # Interface with JAGS

library(coda)

library(parallel)

library(foreach)

library(doParallel)

library(gplots)

library(mvtnorm)

library(snpar)

library(knitr)

library(tinytex)

library(here)

library(portedcmsy)

# Rcpp::sourceCpp(here("src","schaefer.cpp"))

# functions <- list.files(here::here("R"))

# purrr::walk(functions, ~ source(here::here("R", .x)))

FullSchaefer <-
  F    # initialize variable; automatically set to TRUE if enough abundance data are available

n.chains     <- 4

ncores_for_computation <-
  8 # cores to be used for parallel processing of CMSY

cl  <- makeCluster(ncores_for_computation)

registerDoParallel(cl, cores = ncores_for_computation)

catch_file  <-
  "Stocks_Catch_EU_1.csv" #  name of file containing "Stock", "yr", "ct", and optional "bt"
id_file     <-
  "Stocks_ID_EU_8.csv" #  name of file containing stock-specific info and settings for the analysis
outfile     <-
  paste("Out_", format(Sys.Date(), format = "%B%d%Y_"), id_file, sep = "") # default name for output file

# stock_name <- "smn-dp"
stock_name <- "fle-2732" #
stock_name <- "Mull_bar_AD"

cdat <-
  read.csv(
    here("cmsy", catch_file),
    header = T,
    dec = ".",
    stringsAsFactors = FALSE
  ) %>%
  janitor::clean_names()

cinfo <-
  read.csv(
    here("cmsy", id_file),
    header = T,
    dec = ".",
    stringsAsFactors = FALSE
  ) %>%
  janitor::clean_names()

cdat <- cdat %>%
  filter(stock == stock_name)

cinfo <- cinfo %>%
  filter(stock == stock_name)

set.seed(42)
a <- Sys.time()
ported_cmsy <- portedcmsy::funct_cmsy(catches = cdat$ct / 1000,
                          catch_years = cdat$yr,
                          stock = cinfo$stock,
                          common_name = cinfo$name,
                          scientific_name = cinfo$scientific_name,
                          r.low = cinfo$r_low,
                          r.hi = cinfo$r_hi,
                          res = cinfo$resilience,
                          stb.low = cinfo$stb_low,
                          stb.hi = cinfo$stb_hi,
                          int.yr = cinfo$int_yr,
                          intb.low = cinfo$intb_low,
                          intb.hi = cinfo$intb_hi,
                          endb.low = cinfo$endb_low,
                          endb.hi = cinfo$endb_hi,
                          start.yr = min(cdat$yr),
                          end.yr = cinfo$end_year,
                          cores = 12)
Sys.time() - a

write_rds(ported_cmsy, "ported_cmsy.rds")
# rm(SchaeferParallelSearch)
a <- Sys.time()

source(here("cmsy","functional-cmsy-2019-9f.R"))

Sys.time() - a

sista_output <-  output %>%
  mutate(b_bmsy = bt / bmsy) %>%
  select(ct, b_bmsy, f_fmsy, year) %>%
  mutate(year = as.numeric(year) - 1 + 1950) %>%
  mutate(code = "SISTA16 CMSY") %>%
  filter(year %in% ported_cmsy$year)


ported_output <- ported_cmsy %>%
  mutate(b_bmsy = bt / bmsy) %>%
  select(ct, b_bmsy, f_fmsy, year) %>%
  mutate(code = "ported CMSY")

compare_outputs <- sista_output %>%
  bind_rows(ported_output) %>%
  group_by(code) %>%
  pivot_longer(c(-code,-year), names_to = "variable", values_to = "value")

compare_outputs %>%
  # filter(year >= 2000) %>%
  ggplot(aes(year, value, color = code, linetype = code)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  scale_y_continuous(limits = c(0,NA))



