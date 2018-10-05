#!/usr/bin/env Rscript

library(readr)
library(grDevices)
library(graphics)
library(stats)
library(utils)
library(MASS)

data <- read_csv("/home/user/r/Booky-R/data-ext-2.csv", col_types=
		cols(
			Title = col_character(),
			Pages = col_integer(),
			Rating = col_double()
		)
	)

pages <- data$Pages
rating <- data$Rating

# Robust regression line
robustfit <- rlm(rating ~ pages)

message("Summary: Pages")
print(summary(pages))

message("\nSummary: Rating")
print(summary(rating))

print(summary(robustfit), digits=8, correlation=TRUE)

print(confint.default(robustfit, level=.95), digits=8)
