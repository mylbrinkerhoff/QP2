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
my.theme <- theme(axis.text = element_text(colour="black", size=25),
                  text = element_text(size=26),
                  title = element_text(size=29),
                  axis.title.x=  element_text(vjust=-0.45),
                  axis.title.y = element_text(vjust=.2),
                  axis.ticks = element_line(colour="black"),
                  axis.line = element_line(),
                  legend.position = c(.8,.9),
                  legend.background = element_rect(size=0.25,
                                                   linetype="solid",
                                                   color = "black"),
                  )

theme2 <- theme(axis.text = element_text(colour="black", size=25),
                text = element_text(size=26),
                title = element_text(size=29),
                axis.title.x=  element_text(vjust=-0.45),
                axis.title.y = element_text(vjust=.2),
                axis.ticks = element_line(colour="black"),
                axis.line = element_line(),
                legend.background = element_rect(size=0.25,
                                       linetype="solid",
                                       color = "black"),
      )

# Adding the csv/txt file.
# This is the file that is generated from VoiceSauce. 
zapotecVS <- read.table("SLZTone.txt", header = T, sep = "\t")


# Extract the phonation and the tone labels into new columns

# Phonation Labels

# Add a column for the different phonations using an ifelse statement that
# greps for the different phonations. 

# looking at file names that have the following format: 
# WordNumber_Word_instance_vowel quality_phonation_tone.mat

zapotecVS$phonation <- factor(ifelse(grepl("modal", zapotecVS$Filename, ignore.case = T), "Modal",
                                     ifelse(grepl("breathy", zapotecVS$Filename, ignore.case = T), "Breathy",
                                            ifelse(grepl("checked", zapotecVS$Filename, ignore.case = T), "Checked", "Laryngealized"
                                            )
                                     )
), levels = c("Modal", "Breathy", "Checked", "Laryngealized"))

# Tone Labels
# Add a column for the different tones  using an ifelse statement that
# greps for the different tones

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

# Checking for Problems with the greps
Problems <- zapotecVS %>% filter(tone == "N")
Problems

# Two way table for tone and phonation 
tone_phonation <- table(zapotecVS$tone, zapotecVS$phonation)
tone_phonation

# Normalize the scores for f0 (Will need this for later I think.)
# zapotecVS$normalized_tone <- scale(zapotecVS$strF0)

# summarize the data in zapotecVS
# zapotecVS_means <- zapotecVS %>% 
#   mutate(normalized_time = round((t_ms-seg_Start)/(seg_End-seg_Start),digits = 2)) %>%
#   left_join(zapotecVS_time) %>%
#   group_by(tone,normalized_time) %>% 
#   summarise(mean_F0 = mean(strF0), se = std.error(strF0)) 

# Normalize time
# takes in the data frame and adds a column for normalized time. 
zapotec_times <- zapotecVS %>% 
  mutate(normalized_time = round((t_ms-seg_Start)/(seg_End-seg_Start),digits = 2))

# Plotting the tone
# ggplot of normalized tone (y-axis) over time (x axis)
tone_plot <- ggplot(data = zapotec_times, 
       aes(x = normalized_time, 
          y=strF0, 
          group=tone, 
          colour=tone,
          fill=tone)) +
    geom_smooth(method = "loess") +
    labs(title = "FSR's average F0 contours across tonal patterns", 
       x = "Normalized time (% of vowel duration)",
       y = "F0 (Hz)",
       colour = "Tonal Pattern") +
    theme_bw() +
    guides(colour = guide_legend("Tonal Pattern", ncol = 5), 
           fill = guide_legend("Tonal Pattern", ncol = 5) ) +
    my.theme

print(tone_plot)
ggsave(filename = "TonePlot.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

# Phonation
# Evaluate the spectral tilt measurements according to different tones

# boxplot by phonation
h1h2_boxplot <- ggplot(data = zapotec_times, 
                    aes(x = phonation, 
                        y=HH2c,
                        group=interaction(tone, phonation),
                        colour=tone)
                    ) +
  geom_boxplot() +
  labs(title = "FSR's average H1-H2 values across tone and phonation", 
       x = "Phonation",
       y = "H1-H2") +
  theme_bw() +
  guides(colour = guide_legend("Tonal Pattern", ncol = 5), 
         fill = guide_legend("Tonal Pattern", ncol = 5) ) +
  my.theme

print(h1h2_plot)
ggsave(filename = "h1h2_box.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

# boxplot by tone
h1h2_boxplot2 <- ggplot(data = zapotec_times, 
                       aes(x = tone, 
                           y=HH2c,
                           group=interaction(tone, phonation),
                           colour=phonation)
) +
  geom_boxplot() +
  labs(title = "FSR's average H1-H2 values across tone and phonation", 
       x = "Phonation",
       y = "H1-H2") +
  theme_bw() +
  guides(colour = guide_legend("Phonation Pattern", ncol = 5)) +
  my.theme

print(h1h2_boxplot2)
ggsave(filename = "h1h2_box2.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)


# Line plot
h1h2_line <- ggplot(data = zapotec_times, 
                    aes(x = normalized_time, 
                        y=HH2c,
                        group=interaction(phonation, tone),
                        linetype=tone,
                        colour=phonation)
                    ) +
  geom_smooth(method = "loess") +
  labs(title = "FSR's average H1-H2 values across tone and phonation", 
       x = "Phonation",
       y = "H1-H2") +
  theme_bw() +
  guides(linetype = guide_legend("Tonal Pattern"), 
         colour = guide_legend("Phonation") ) +
  theme(axis.text = element_text(colour="black", size=25),
        text = element_text(size=26),
        title = element_text(size=29),
        axis.title.x=  element_text(vjust=-0.45),
        axis.title.y = element_text(vjust=.2),
        axis.ticks = element_line(colour="black"),
        axis.line = element_line(),
        legend.background = element_rect(size=0.25,
                                         linetype="solid",
                                         color = "black"),
  )

print(h1h2_line)
ggsave(filename = "h1h2_line.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## Line Plot at H
h1h2_line_H <- ggplot(data = zapotec_times[zapotec_times$tone=="H", ], 
                    aes(x = normalized_time, 
                        y=HH2c,
                        group=interaction(phonation, tone),
                        colour=phonation)
) +
  geom_smooth(method = "loess") +
  labs(title = "FSR's average H1-H2 at H", 
       x = "Normalized time (% of vowel duration)",
       y = "H1-H2 (dB)") +
  theme_bw() +
  guides(colour = guide_legend("Phonation")) +
  theme2

print(h1h2_line_H)
ggsave(filename = "h1h2_line_H.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## Line plot at L
h1h2_line_L <- ggplot(data = zapotec_times[zapotec_times$tone=="L", ], 
                      aes(x = normalized_time, 
                          y=HH2c,
                          group=interaction(phonation, tone),
                          colour=phonation)
) +
  geom_smooth(method = "loess") +
  labs(title = "FSR's average H1-H2 at L", 
       x = "Normalized time (% of vowel duration)",
       y = "H1-H2 (dB)") +
  theme_bw() +
  guides(colour = guide_legend("Phonation")) +
  theme2

print(h1h2_line_L)
ggsave(filename = "h1h2_line_L.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## Line Plot at M
h1h2_line_M <- ggplot(data = zapotec_times[zapotec_times$tone=="M", ], 
                      aes(x = normalized_time, 
                          y=HH2c,
                          group=interaction(phonation, tone),
                          colour=phonation)
) +
  geom_smooth(method = "loess") +
  labs(title = "FSR's average H1-H2 values at M", 
       x = "Normalized time (% of vowel duration)",
       y = "H1-H2 (dB)") +
  theme_bw() +
  guides(colour = guide_legend("Phonation")) +
  theme2

print(h1h2_line_M)
ggsave(filename = "h1h2_line_M.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## Line Plot at HL
h1h2_line_HL <- ggplot(data = zapotec_times[zapotec_times$tone=="HL", ], 
                      aes(x = normalized_time, 
                          y=HH2c,
                          group=interaction(phonation, tone),
                          colour=phonation)
) +
  geom_smooth(method = "loess") +
  labs(title = "FSR's average H1-H2 at HL", 
       x = "Normalized time (% of vowel duration)",
       y = "H1-H2 (dB)") +
  theme_bw() +
  guides(colour = guide_legend("Phonation")) +
  theme2

print(h1h2_line_HL)
ggsave(filename = "h1h2_line_HL.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## Line Plot at MH
h1h2_line_MH <- ggplot(data = zapotec_times[zapotec_times$tone=="MH", ], 
                      aes(x = normalized_time, 
                          y=HH2c,
                          group=interaction(phonation, tone),
                          colour=phonation)
) +
  geom_smooth(method = "loess") +
  labs(title = "FSR's average H1-H2 at MH", 
       x = "Normalized time (% of vowel duration)",
       y = "H1-H2 (dB)") +
  theme_bw() +
  guides(colour = guide_legend("Phonation")) +
  theme2

print(h1h2_line_MH)
ggsave(filename = "h1h2_line_MH.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)
