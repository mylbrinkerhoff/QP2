# -----------------------------------------------------------------------------
# 
# ZapotecTone.R
# 
# This script takes a CSV file and adds a column for phonation and tone. It
# takes the labels for tone and plots the f0 values which have been 
# z-transformed. 
# 
# M. Brinkerhoff * UCSC * 2022-01-14 (Tu)
# 
# -----------------------------------------------------------------------------


# Install packages
# install.packages("tidyverse")
# install.packages("scales")
library(tidyverse)
library(scales)
library(metan)

# functions
std.error <- function(x, na.rm = T) {
  sqrt(var(x, na.rm = na.rm)/length(x[complete.cases(x)]))
}

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

# custom theme for plotting
my.theme <- theme(axis.text = element_text(colour="black", size=15),
                  text = element_text(size=16),
                  title = element_text(size=19),
                  axis.title.x=  element_text(vjust=-0.45),
                  axis.title.y = element_text(vjust=.2),
                  axis.ticks = element_line(colour="black"),
                  axis.line = element_line())

# Adding the csv/txt file
zapotecVS <- read.table("SLZTone.txt", header = T, sep = "\t")


# Extract the phonation and the tone labels into new
# Phonation Labels
# Add a column for the different tones  using an ifelse statement that
# greps for the different phonations
zapotecVS$phonation <- factor(ifelse(grepl("modal", zapotecVS$Filename, ignore.case = T), "Modal",
                                     ifelse(grepl("breathy", zapotecVS$Filename, ignore.case = T), "Breathy",
                                            ifelse(grepl("checked", zapotecVS$Filename, ignore.case = T), "Checked", "Laryngealized"
                                            )
                                     )
), levels = c("Modal", "Breathy", "Checked", "Laryngealized"))

# Tone Labels
# Add a column for the different tones  using an ifelse statement that
# greps for the different tones
# An example of what it is looking like
# DF$KIND <- ifelse(grepl("gas", DF$GLDESC, ignore.case = T), "Materials", 
#                   ifelse(grepl("payroll", DF$GLDESC, ignore.case = T), "Payroll", "Other"))

zapotecVS$tone <- ifelse(grepl("_H.mat", zapotecVS$Filename, ignore.case = F), "H",
                         ifelse(grepl("_M.mat", zapotecVS$Filename, ignore.case = F), "M",
                                ifelse(grepl("_L.mat", zapotecVS$Filename, ignore.case = F), "L",
                                       ifelse(grepl("_HL.mat", zapotecVS$Filename, ignore.case = F), "HL", 
                                              ifelse(grepl("_MH.mat", zapotecVS$Filename, ignore.case = F), "MH", "N"
                                       )
                                )
                         )
)
)

# Two way table for tone and phonation 
tone_phonation <- table(zapotecVS$tone, zapotecVS$phonation)
tone_phonation

# Normalize the scores for f0
# zapotecVS$normalized_tone <- scale(zapotecVS$strF0)

# # Normalize time 
# zapotecVS_time <- zapotecVS %>%
#   group_by(Filename) %>%
#   summarise(normalized_time = round((t_ms-seg_Start)/(seg_End-seg_Start),digits = 2)) %>%
#   ungroup()

# summarize the data in zapotecVS
zapotecVS_means <- zapotecVS %>% 
  mutate(normalized_time = round((t_ms-seg_Start)/(seg_End-seg_Start),digits = 2)) %>%
  left_join(zapotecVS_time) %>%
  group_by(tone,normalized_time) %>% 
  summarise(mean_F0 = mean(strF0), se = std.error(strF0)) 

# Plotting the tone
# ggplot of normalized tone (y-axis) over time (x axis)
ggplot(data = zapotecVS_means, aes(x = normalized_time, y=mean_F0, group=tone, colour=tone)) +
  # geom_point() + # use points and lines instead of geom_bar()
  # geom_line() +
  geom_smooth() +
  # note: we no longer need to specify position_dodge for the errorbars
  # geom_errorbar(width = .15, aes(ymin = mean_F0-se, ymax = mean_F0+se)) +
  my.theme

# Problems
Problems <- zapotecVS %>% filter(tone == "N")
Problems
