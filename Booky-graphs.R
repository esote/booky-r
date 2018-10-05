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

pdf("/home/user/r/Booky-R/Extended2 - Book Pages vs Rating graphics.pdf", title="Extended2 - Book Pages vs Rating graphics")

pages <- data$Pages
rating <- data$Rating

# Outlier upper and lower bounds
ub_pages <- quantile(pages, .75) + 1.5 * IQR(pages)
ub_rating <- quantile(rating, .75) + 1.5 * IQR(rating)
lb_pages <- quantile(pages, .25) - 1.5 * IQR(pages)
lb_rating <- quantile(rating, .25) - 1.5 * IQR(rating)

if (lb_pages < 0) lb_pages <- 0
if (lb_rating < 0) lb_rating <- 0

pages_no_out <- c(lb_pages, ub_pages)
rating_no_out <- c(lb_rating, ub_rating)

# Scatter plot
plot(pages, rating, main="Pages vs. Rating", xlab="Pages", ylab="Rating")

# Scatter plot without outliers
plot(pages, rating, main="Pages vs. Rating\n[ no outliers ]", xlab="Pages", ylab="Rating",
	xlim=pages_no_out, ylim=rating_no_out)

# Smooth scatter plot
smoothScatter(pages, rating, main="Pages vs. Rating\n[ smooth density ]",
	xlab="Pages", ylab="Rating", nbin=1000, nrpoints=0)

# Smooth scatter plot without outliers
smoothScatter(pages, rating, main="Pages vs. Rating\n[ smooth density, no outliers ]",
	xlab="Pages", ylab="Rating", nbin=1000, nrpoints=0, 
	xlim=pages_no_out, ylim=rating_no_out)

# Robust linear regression
robustfit <- rlm(rating ~ pages)

# Scatter plot without outliers with robust linear regression line
plot(pages, rating, main="Pages vs. Rating\n[ no outliers, robust linear regression ]",
	xlab="Pages", ylab="Rating", xlim=pages_no_out, ylim=rating_no_out)
abline(robustfit, col="red", lwd=3)
text(-22, 4.73, expression(paste(italic("rating"), " = 3.79365179 + 0.00025298(",
	italic("pages"), ")")), pos=4)

# Smooth density plot without outliers with robust linear regression line
smoothScatter(pages, rating, main="Pages vs. Rating\n[ smooth density, no outliers, robust linear regression ]",
	xlab="Pages", ylab="Rating", nbin=1000, nrpoints=0,
	xlim=pages_no_out, ylim=rating_no_out)
abline(robustfit, col="red", lwd=3)
text(-22, 4.73, expression(paste(italic("rating"), " = 3.79365179 + 0.00025298(",
	italic("pages"), ")")), pos=4)

# Residual plot of robust linear regression
robustfit.resid = resid(robustfit)
plot(pages, robustfit.resid, ylab="Residuals", xlab="Pages", main="Residual Plot\n[ no outliers ]",
	xlim=pages_no_out)
abline(0,0)

par(mfrow=c(2,1))

# Histogram of pages with line
hist_pages <- hist(pages, main="Histogram: Pages", xlab="Pages", breaks=50)
xfit <- seq(min(pages), max(pages), length=500)
yfit <- dnorm(xfit, mean=mean(pages), sd=sd(pages))
yfit <- yfit*diff(hist_pages$mids[1:2])*length(pages)
lines(xfit, yfit)

# Histogram of rating with line
hist_rating <- hist(data$Rating, main="Histogram: Rating", xlab="Rating", breaks=50)
xfit <- seq(min(rating), max(rating), length=500)
yfit <- dnorm(xfit, mean=mean(rating), sd=sd(rating))
yfit <- yfit*diff(hist_rating$mids[1:2])*length(rating)
lines(xfit, yfit)

par(mfrow=c(4,1))

# Boxplot and Stripchart of pages
boxplot(pages, main="Boxplot: Pages", horizontal=TRUE)
stripchart(pages, method="jitter", main="Stripchart: Pages [ jitter ]", pch=16)
#boxplot(pages, main="Boxplot: Pages\n[ no outliers ]", horizontal=TRUE, outline=FALSE)

# Boxplot and Stripchart of rating
boxplot(rating, main="Boxplot: Rating", horizontal=TRUE)
stripchart(rating, method="jitter", main="Stripchart: Rating [ jitter ]", pch=16)
#boxplot(rating, main="Boxplot: Rating\n[ no outliers ]", horizontal=TRUE, outline=FALSE)
