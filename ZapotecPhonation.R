# -----------------------------------------------------------------------------
# 
# ZapotecPhonation.R
# 
# This script takes a CSV file and adds a column for phonation and tone. It
# then takes the results and runs a mixed effects linear regression statistical
# test to determine what acoustic measures are most significant in 
# differentiating the phonation types in Santiago Laxopa Zapotec. 
# 
# M. Brinkerhoff * UCSC * 2021-08-03 (Tu)
# 
# -----------------------------------------------------------------------------


# Install and load packages
install.packages("tidyverse")
install.packages("lme4")
library(tidyverse)
library(lme4)
# Load the CSV/txt file
# zapotec <- read.csv(file = "20210805_spectral_measures.txt")
zapotecVS <- read.table("20210816_VSOutput.txt", header = T, sep = "\t")


# Check the characteristics of the file
head(zapotecVS)
summary(zapotecVS)
str(zapotecVS)

# Add a column for tone and another column for phonation

# Add a column for the different phonation types using an ifelse statement that
# greps for the different phonation types
# An example of what it is looking like
# DF$KIND <- ifelse(grepl("gas", DF$GLDESC, ignore.case = T), "Materials", 
#                   ifelse(grepl("payroll", DF$GLDESC, ignore.case = T), "Payroll", "Other"))


zapotecVS$phonation <- factor(ifelse(grepl("modal", zapotecVS$Filename, ignore.case = T), "Modal",
                            ifelse(grepl("breathy", zapotecVS$Filename, ignore.case = T), "Breathy",
                                   ifelse(grepl("checked", zapotecVS$Filename, ignore.case = T), "Checked", "Laryngealized"
                                          )
                                   )
                            ), levels = c("Modal", "Breathy", "Checked", "Laryngealized"))

# Add a column for the different tones  using an ifelse statement that
# greps for the different tones
# An example of what it is looking like
# DF$KIND <- ifelse(grepl("gas", DF$GLDESC, ignore.case = T), "Materials", 
#                   ifelse(grepl("payroll", DF$GLDESC, ignore.case = T), "Payroll", "Other"))

zapotecVS$tone <- ifelse(grepl("_H$", zapotecVS$Filename, ignore.case = F), "H",
                       ifelse(grepl("_M$", zapotecVS$Filename, ignore.case = F), "M",
                              ifelse(grepl("_L$", zapotecVS$Filename, ignore.case = F), "L",
                                     ifelse(grepl("_HL$", zapotecVS$Filename, ignore.case = F), "HL", "MH"
                                            )
                                    )
                              )
                      )


# Some plotting
h1h2.box <- boxplot(H1H2c_mean ~ phonation, data = zapotecVS)
cpp.plot <- boxplot(CPP_mean ~ phonation, data = zapotecVS)

# Standardizing the scores
zapotec$H1H2u2 <- scale(zapotec$H1H2u, center = T, scale = T)
zapotec$CPP2 <- scale(zapotec$CPP, center = T, scale = T)
zapotec$H1H2c2 <- scale(zapotec$H1H2c, center = T, scale = T)

# Run lme4 focused on phonation by the different accoustic measurements
# Linear regression for H1u-H2u using uncorrected values
hist(zapotecVS$H1H2c_mean)
hist(zapotec$H1H2u2)

h1 <- lm(formula = H1H2c_mean ~ phonation,
                data = zapotecVS)
summary(h1)

h1.remake <- lm(formula = H1H2u2 ~ phonation,
         data = zapotec)
summary(h1.remake)

# Linear regression for CPP using uncorrected values
hist(zapotecVS$CPP_mean)
hist(zapotec$CPP2)

cpp <- lm(formula = CPP_mean ~ phonation,
          data = zapotecVS)
summary(cpp)

cpp.remake <- lm(formula = CPP2 ~ phonation,
                 data = zapotec)
summary(cpp.remake)

h1_cpp.lm <- lm(formula = H1H2u2 ~ CPP2,
                data = zapotec)
summary(h1_cpp.lm)


# (prelim_plot <- ggplot(zapotec, aes(x = phonation, y = CPP)) +
#     geom_point() +
#     geom_smooth(method = "lm"))
# 
# (prelim_plot2 <- ggplot(zapotec, aes(x = phonation, y = H1H2u)) +
#     geom_point() +
#     geom_smooth(method = "lm"))
# 
# (h1.cpp <- ggplot(zapotec, aes(x = CPP2, y = H1H2u2)) +
#     geom_point() +
#     geom_smooth(method = "lm"))

# linear mixed effects regressions using uncorrected values
h1h2c.lmer <- lmer(H1H2c_mean ~ phonation + (1 | tone), 
               data = zapotecVS )
summary(h1h2c.lmer)

cpp.lmer <- lmer(CPP_mean ~ phonation + (1 | tone), 
               data = zapotecVS )
summary(cpp.lmer)

# Linear regression for H1u-H2u using corrected values
hist(zapotec$H1H2c)
hist(zapotec$H1H2c2)

h1c <- lm(formula = H1H2u_mean ~ phonation,
         data = zapotecVS)
summary(h1c)

h1c.remake <- lm(formula = H1H2c2 ~ phonation,
                data = zapotec)
summary(h1.remake)

# linear mixed effects regressions using corrected values
h1c.lmer <- lmer(H1H2c ~ phonation + (1 | tone), 
                data = zapotec )
summary(h1c.lmer)

cpp.lmer <- lmer(CPP ~ phonation + (1 | tone), 
                 data = zapotec )
summary(cpp.lmer)
# Bayesian stat
install.packages("brms")
library(brms)
