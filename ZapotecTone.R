# -----------------------------------------------------------------------------
# 
# ZapotecTone.R
# 
# This script takes a CSV file and adds a column for phonation and tone. It
# takes the labels for tone and plots the f0 values which have been 
# z-transformed. 
# 
# M. Brinkerhoff * UCSC * 2022-02-24 (Th)
# 
# -----------------------------------------------------------------------------

# Install packages
# install.packages("tidyverse")
# install.packages("scales")
install.packages("ggthemes")

library(tidyverse)
library(scales)
library(metan)
library(ggthemes)


# colorblind friendly colors
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

scale_fill_manual(values=cbbPalette) # To use for fills
scale_colour_manual(values=cbbPalette)  # To use for line and point colors

# scale_color_discrete <- scale_color_colorblind



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

zapotecRD <- read.table("RDZapotec.txt", header = T, sep = "\t")

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


# RD 
zapotecRD$phonation <- factor(ifelse(grepl("modal", zapotecRD$Filename, ignore.case = T), "Modal",
                                     ifelse(grepl("breathy", zapotecRD$Filename, ignore.case = T), "Breathy",
                                            ifelse(grepl("checked", zapotecRD$Filename, ignore.case = T), "Checked", "Laryngealized"
                                            )
                                     )
), levels = c("Modal", "Breathy", "Checked", "Laryngealized"))

zapotecRD$tone <- ifelse(grepl("_H.mat", zapotecRD$Filename, ignore.case = F), "H",
                         ifelse(grepl("_M.mat", zapotecRD$Filename, ignore.case = F), "M",
                                ifelse(grepl("_L.mat", zapotecRD$Filename, ignore.case = F), "L",
                                       ifelse(grepl("_HL.mat", zapotecRD$Filename, ignore.case = F), "HL", 
                                              ifelse(grepl("_MH.mat", zapotecRD$Filename, ignore.case = F), "MH", "N"
                                              )
                                       )
                                )
                         )
)

# Checking for Problems with the greps
# Problems <- zapotecVS %>% filter(tone == "N")
# Problems

# Joining the two tables into one giant table
# Add column for FSR and RD

zapotecVS$Speaker <- rep("FSR")

zapotecRD$Speaker <- rep("RD")

zapotec.join <- bind_rows(list(zapotecRD,zapotecVS), .id = "id")
zapotec.join

# Two way table for tone and phonation 
FSRtone_phonation <- table(zapotecVS$tone, zapotecVS$phonation)
FSRtone_phonation

RDtone_phonation <- table(zapotecRD$tone, zapotecRD$phonation)
RDtone_phonation

join.TP <- table(zapotec.join$tone, zapotec.join$phonation)
join.TP

# Normalize the scores for f0 in zapotec.join
zapotec.join$normalized_tone <- scale(zapotec.join$strF0)

# summarize the data in zapotecVS
# zapotecVS_means <- zapotecVS %>% 
#   mutate(normalized_time = round((t_ms-seg_Start)/(seg_End-seg_Start),digits = 2)) %>%
#   left_join(zapotecVS_time) %>%
#   group_by(tone,normalized_time) %>% 
#   summarise(mean_F0 = mean(strF0), se = std.error(strF0)) 

# Normalize time
# takes in the data frame and adds a column for normalized time. 
zapotec_timesFSR <- zapotecVS %>% 
  mutate(normalized_time = round((t_ms-seg_Start)/(seg_End-seg_Start),digits = 3))

zapotec_timesRD <- zapotecRD %>% 
    mutate(normalized_time = round((t_ms-seg_Start)/(seg_End-seg_Start),digits = 3))

zapotec_timesJoin <- zapotec.join %>%
    mutate(normalized_time = round((t_ms-seg_Start)/(seg_End-seg_Start),digits = 3))

# Plotting the tone
# ggplot of normalized tone (y-axis) over time (x axis)
FSRtone_plot <- ggplot(data = zapotec_timesFSR, 
       aes(x = normalized_time, 
          y=strF0, 
          group=tone, 
          colour=tone,
          fill=tone,
          )) +
    geom_smooth(method = "loess") +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    labs(title = "FSR's average F0 contours across tonal patterns", 
       x = "Normalized time (% of vowel duration)",
       y = "F0 (Hz)",
       colour = "Tonal Pattern") +
    theme_bw() +
    guides(colour = guide_legend("Tonal Pattern", ncol = 5), 
           fill = guide_legend("Tonal Pattern", ncol = 5) ) +
    my.theme

print(FSRtone_plot)
ggsave(filename = "FSRTonePlot.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

RDtone_plot <- ggplot(data = zapotec_timesRD, 
                      aes(x = normalized_time, 
                          y=strF0, 
                          group=tone, 
                          colour=tone,
                          fill=tone,
                      )) +
    geom_smooth(method = "loess") +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    labs(title = "RD's average F0 contours across tonal patterns", 
         x = "Normalized time (% of vowel duration)",
         y = "F0 (Hz)",
         colour = "Tonal Pattern") +
    theme_bw() +
    guides(colour = guide_legend("Tonal Pattern", ncol = 5), 
           fill = guide_legend("Tonal Pattern", ncol = 5) ) +
    my.theme

print(RDtone_plot)
ggsave(filename = "RDTonePlot.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

Join.tone_plot <- ggplot(data = times.zapotec.join, 
                         aes(x = normalized_time, 
                             y = normalized_tone, 
                             group=tone, 
                             colour=tone,
                             fill=tone,
                         )) +
    geom_smooth(method = "loess") +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    labs(title = "Normalized F0 contours across speakers", 
         x = "Normalized time (% of vowel duration)",
         y = "Normalized F0 (z-score)",
         colour = "Tonal Pattern") +
    theme_bw() +
    guides(colour = guide_legend("Tonal Pattern", ncol = 5), 
           fill = guide_legend("Tonal Pattern", ncol = 5) ) +
    my.theme


print(Join.tone_plot)
ggsave(filename = "JoinTonePlot.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

# Phonation for FSR
# Evaluate the spectral tilt measurements according to different tones

# boxplot by phonation
FSRh1h2_boxplot <- ggplot(data = zapotec_timesFSR, 
                    aes(x = phonation, 
                        y=H1H2c,
                        group=interaction(tone, phonation),
                        colour=tone)
                    ) +
  geom_boxplot() +
  labs(title = "FSR's average H1-H2 values across tone and phonation", 
       x = "Phonation",
       y = "H1-H2 (dB)") +
  theme_bw() +
  guides(colour = guide_legend("Tonal Pattern", ncol = 5), 
         fill = guide_legend("Tonal Pattern", ncol = 5) ) +
  my.theme

print(FSRh1h2_plot)
ggsave(filename = "FSRh1h2_box.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

# boxplot by tone
h1h2_boxplot2 <- ggplot(data = zapotec_timesFSR, 
                       aes(x = tone, 
                           y=H1H2c,
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
h1h2_line <- ggplot(data = zapotec_timesFSR, 
                    aes(x = normalized_time, 
                        y=H1H2c,
                        # group=interaction(phonation, tone),
                        linetype=tone,
                        colour=phonation)
                    ) +
  geom_smooth(method = "loess") +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
  labs(title = "FSR's average H1-H2 values across tone and phonation", 
       x = "Phonation",
       y = "H1-H2") +
  # theme_bw() +
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
h1h2_line_H <- ggplot(data = zapotec_times2[zapotec_times2$tone=="H", ], 
                    aes(x = normalized_time, 
                        y=H1H2c,
                        group=interaction(phonation, tone),
                        colour=phonation)
) +
  geom_smooth(method = "loess") +
  labs(title = "FSR's average H1-H2 at H", 
       x = "Normalized time (% of vowel duration)",
       y = "H1-H2 (dB)") +
  theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
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
h1h2_line_L <- ggplot(data = zapotec_times2[zapotec_times2$tone=="L", ], 
                      aes(x = normalized_time, 
                          y=H1H2c,
                          group=interaction(phonation, tone),
                          colour=phonation)
) +
  geom_smooth(method = "loess") +
  labs(title = "FSR's average H1-H2 at L", 
       x = "Normalized time (% of vowel duration)",
       y = "H1-H2 (dB)") +
  theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
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
                          y=H1H2c,
                          group=interaction(phonation, tone),
                          colour=phonation)
                    ) +
  geom_smooth(method = "loess") +
  labs(title = "FSR's average H1-H2 values at M", 
       x = "Normalized time (% of vowel duration)",
       y = "H1-H2 (dB)") +
  theme_bw() +
  guides(colour = guide_legend("Phonation")) +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
  theme2

print(h1h2_line_M)
ggsave(filename = "h1h2_line_M.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## Line Plot at HL
h1h2_line_HL <- ggplot(data = zapotec_times2[zapotec_times2$tone=="HL", ], 
                      aes(x = normalized_time, 
                          y=H1H2c,
                          group=interaction(phonation, tone),
                          colour=phonation)
) +
  geom_smooth(method = "loess") +
  labs(title = "FSR's average H1-H2 at HL", 
       x = "Normalized time (% of vowel duration)",
       y = "H1-H2 (dB)") +
  theme_bw() +
  guides(colour = guide_legend("Phonation")) +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
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
                          y=H1H2c,
                          group=interaction(phonation, tone),
                          colour=phonation)
) +
  geom_smooth(method = "loess") +
  labs(title = "FSR's average H1-H2 at MH", 
       x = "Normalized time (% of vowel duration)",
       y = "H1-H2 (dB)") +
  theme_bw() +
  guides(colour = guide_legend("Phonation")) +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
  theme2

print(h1h2_line_MH)
ggsave(filename = "h1h2_line_MH.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)




## Plotting by Phonation type
## Laryngealized

laryngeal <- zapotec_times %>%
                filter(phonation == "Laryngealized" & tone != 'M' & tone != 'MH') %>%
                select(H2Kc, 
                       HH2c, 
                       H2H4c, 
                       HA1c, 
                       HA2c, 
                       HA3c, 
                       H42Kc, 
                       H2KH5Kc,
                       CPP,
                       Energy,
                       HNR05,
                       HNR15,
                       HNR25,
                       HNR35,
                       normalized_time,
                       phonation,
                       tone
                       )

# Line graph of the Laryngealized vowels
line.laryngealized <- ggplot(data = laryngeal, 
                        aes(x = normalized_time, 
                            y=H1H2c,
                            colour=tone)
                        ) +
                    geom_smooth(method = "loess") +
                    labs(title = "H1-H2 values for Laryngealized vowels by tone", 
                        x = "Normalized time (% of vowel duration)",
                        y = "H1-H2 (dB)") +
                    theme_bw() +
                    guides(colour = guide_legend("Tone") ) +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
                    theme2


print(line.laryngealized)
ggsave(filename = "line_laryngealized.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## Checked
checked <- zapotec_times %>%
    filter(phonation == "Checked" & tone != 'M' & tone != 'MH') %>%
    select(H2Kc, 
           HH2c, 
           H2H4c, 
           HA1c, 
           HA2c, 
           HA3c, 
           H42Kc, 
           H2KH5Kc,
           CPP,
           Energy,
           HNR05,
           HNR15,
           HNR25,
           HNR35,
           normalized_time,
           phonation,
           tone
    )

checked.line <- ggplot(data = checked, 
                        aes(x = normalized_time, 
                             y=H1H2c,
                             colour=tone)
                        ) +
    geom_smooth(method = "loess") +
    labs(title = "H1-H2 values for checked vowels by tone", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-H2 (dB)") +
    theme_bw() +
    guides(colour = guide_legend("Tone") ) +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    theme2


print(checked.line)
ggsave(filename = "line_checked.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)


## Breathy
breathy <- zapotec_times %>%
    filter(phonation == "Breathy" & tone != 'M' & tone != 'MH') %>%
    select(H2Kc, 
           H1H2c, 
           H2H4c, 
           H1A1c, 
           H1A2c, 
           H1A3c, 
           H42Kc, 
           H2KH5Kc,
           CPP,
           Energy,
           HNR05,
           HNR15,
           HNR25,
           HNR35,
           normalized_time,
           phonation,
           tone
    )

breathy.line <- ggplot(data = breathy, 
                       aes(x = normalized_time, 
                           y=H1H2c,
                           colour=tone)
) +
    geom_smooth(method = "loess") +
    labs(title = "H1-H2 values for breathy vowels by tone", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-H2 (dB)") +
    theme_bw() +
    guides(colour = guide_legend("Tone") ) +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    theme2


print(breathy.line)
ggsave(filename = "line_breathy.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## Modal
modal <- zapotec_times %>%
    filter(phonation == "Modal" & tone != 'M' & tone != 'MH') %>%
    select(H2Kc, 
           H1H2c, 
           H2H4c, 
           H1A1c, 
           H1A2c, 
           H1A3c, 
           H42Kc, 
           H2KH5Kc,
           CPP,
           Energy,
           HNR05,
           HNR15,
           HNR25,
           HNR35,
           normalized_time,
           phonation,
           tone
    )

modal.line <- ggplot(data = modal, 
                       aes(x = normalized_time, 
                           y=H1H2c,
                           colour=tone)
) +
    geom_smooth(method = "loess") +
    labs(title = "H1-H2 values for modal vowels by tone", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-H2 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Tone") ) +
    theme2


print(modal.line)
ggsave(filename = "line_modal.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)


## plot of laryngealized and checked together

zapotec_CL <- bind_rows(list(laryngeal,checked), .id = "id")
h1h2_CheckedLaryngeal <- ggplot(data = zapotec_CL,  
                                aes(x = normalized_time, 
                                    y=HH2c,
                                    linetype=phonation,
                                    colour=tone)
) +
    geom_smooth(method = "loess") +
    labs(title = "FSR's Laryngeal and Checked H1-H2", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-H2 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Tone"),
           linetype = guide_legend("Phonation")
    ) +
    theme2

print(h1h2_CheckedLaryngeal)
ggsave(filename = "h1h2_CheckedLaryngeal.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## Laryngeal and modal
zapotec_MCL <- bind_rows(list(laryngeal,checked,modal), .id = "id")
h1h2_MCL <- ggplot(data = zapotec_MCL,  
                                aes(x = normalized_time, 
                                    y=H1H2c,
                                    linetype=phonation,
                                    colour=tone)
) +
    geom_smooth(method = "loess") +
    labs(title = "H1-H2 values for Modal, Laryngealized, and Checked", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-H2 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Tone"),
           linetype = guide_legend("Phonation")
    ) +
    theme2

print(h1h2_MCL)
ggsave(filename = "h1h2_MCL.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)


## Plotting h1-a1
h1a1_line <- ggplot(data = zapotec_times, 
                    aes(x = normalized_time, 
                        y=H1A1c,
                        group=interaction(phonation, tone),
                        linetype=tone,
                        colour=phonation)
) +
    geom_smooth(method = "loess") +
    labs(title = "FSR H1-A1 values", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-A1 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(linetype = guide_legend("Tone"), 
           colour = guide_legend("Phonation") ) +
    theme2

print(h1a1_line)
ggsave(filename = "h1a1_line.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## HA2
h1a2_line <- ggplot(data = zapotec_times, 
                    aes(x = normalized_time, 
                        y=H1A2c,
                        group=interaction(phonation, tone),
                        linetype=tone,
                        colour=phonation)
) +
    geom_smooth(method = "loess") +
    labs(title = "FSR H1-A2 values", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-A2 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(linetype = guide_legend("Tone"), 
           colour = guide_legend("Phonation") ) +
    theme2

print(h1a2_line)
ggsave(filename = "h1a2_line.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## H1A3c
h1a3_line <- ggplot(data = zapotec_times, 
                    aes(x = normalized_time, 
                        y=H1A3c,
                        group=interaction(phonation, tone),
                        linetype=tone,
                        colour=phonation)
) +
    geom_smooth(method = "loess") +
    labs(title = "FSR H1-A3 values", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-A3 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(linetype = guide_legend("Tone"), 
           colour = guide_legend("Phonation") ) +
    theme2

print(h1a3_line)
ggsave(filename = "h1a3_line.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## H1-H2K
H2k_line <- ggplot(data = zapotec_times, 
               aes(x = normalized_time, 
                       y=H2Kc,
                       group=interaction(phonation, tone),
                       linetype=tone,
                       colour=phonation)
) +
    geom_smooth(method = "loess") +
    labs(title = "FSR H1-H2k values", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-H2k (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(linetype = guide_legend("Tone"), 
           colour = guide_legend("Phonation") ) +
    theme2

print(H2k_line)
ggsave(filename = "h1h2k_line.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## CPP
CPP_line <- ggplot(data = zapotec_times2, 
                    aes(x = normalized_time, 
                        y=CPP,
                        group=interaction(phonation, tone),
                        linetype=tone,
                        colour=phonation)
) +
    geom_smooth(method = "loess") +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    labs(title = "FSR's CPP values", 
         x = "Normalized time (% of vowel duration)",
         y = "CPP") +
    theme_bw() +
    guides(linetype = guide_legend("Tone"), 
           colour = guide_legend("Phonation") ) +
    theme2

print(CPP_line)
ggsave(filename = "CPP_line.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)



zapotec_CL <- bind_rows(list(laryngeal,checked), .id = "id")
HA3_CheckedLaryngeal <- ggplot(data = zapotec_CL,  
                                aes(x = normalized_time, 
                                    y=H1A3c,
                                    linetype=phonation,
                                    colour=tone)
) +
    geom_smooth(method = "loess") +
    labs(title = "Laryngeal and Checked H1-A3", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-A3 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Tone"),
           linetype = guide_legend("Phonation")
    ) +
    theme2

print(HA3_CheckedLaryngeal)
ggsave(filename = "HA3_CheckedLaryngeal.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

# Phonation for RD
# Evaluate the spectral tilt measurements according to different tones
# Line plot
RDh1h2_line <- ggplot(data = zapotec_timesRD, 
                    aes(x = normalized_time, 
                        y=H1H2c,
                        # group=interaction(phonation, tone),
                        linetype=tone,
                        colour=phonation)
) +
    geom_smooth(method = "loess") +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    labs(title = "RD's average H1-H2 values across tone and phonation", 
         x = "Phonation",
         y = "H1-H2 (dB)") +
    # theme_bw() +
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

print(RDh1h2_line)
ggsave(filename = "RDh1h2_line.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## Line Plot at H
RDh1h2_line_H <- ggplot(data = zapotec_timesRD[zapotec_timesRD$tone=="H", ], 
                      aes(x = normalized_time, 
                          y=H1H2c,
                          group=interaction(phonation, tone),
                          colour=phonation)
) +
    geom_smooth(method = "loess") +
    labs(title = "RD's average H1-H2 at H", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-H2 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Phonation")) +
    theme2

print(RDh1h2_line_H)
ggsave(filename = "RDh1h2_line_H.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## Line plot at L
RDh1h2_line_L <- ggplot(data = zapotec_timesRD[zapotec_timesRD$tone=="L", ], 
                      aes(x = normalized_time, 
                          y=H1H2c,
                          group=interaction(phonation, tone),
                          colour=phonation)
) +
    geom_smooth(method = "loess") +
    labs(title = "RD's average H1-H2 at L", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-H2 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Phonation")) +
    theme2

print(RDh1h2_line_L)
ggsave(filename = "RDh1h2_line_L.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## Line Plot at M
RDh1h2_line_M <- ggplot(data = zapotec_timesRD[zapotec_timesRD$tone=="M", ], 
                      aes(x = normalized_time, 
                          y=H1H2c,
                          group=interaction(phonation, tone),
                          colour=phonation)
) +
    geom_smooth(method = "loess") +
    labs(title = "RD's average H1-H2 values at M", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-H2 (dB)") +
    theme_bw() +
    guides(colour = guide_legend("Phonation")) +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    theme2

print(RDh1h2_line_M)
ggsave(filename = "RDh1h2_line_M.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## Line Plot at HL
RDh1h2_line_HL <- ggplot(data = zapotec_timesRD[zapotec_timesRD$tone=="HL", ], 
                       aes(x = normalized_time, 
                           y=H1H2c,
                           group=interaction(phonation, tone),
                           colour=phonation)
) +
    geom_smooth(method = "loess") +
    labs(title = "RD's average H1-H2 at HL", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-H2 (dB)") +
    theme_bw() +
    guides(colour = guide_legend("Phonation")) +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    theme2

print(RDh1h2_line_HL)
ggsave(filename = "RDh1h2_line_HL.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## Line Plot at MH
RDh1h2_line_MH <- ggplot(data = zapotec_timesRD[zapotec_timesRD$tone=="MH", ], 
                       aes(x = normalized_time, 
                           y=H1H2c,
                           group=interaction(phonation, tone),
                           colour=phonation)
) +
    geom_smooth(method = "loess") +
    labs(title = "RD's average H1-H2 at MH", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-H2 (dB)") +
    theme_bw() +
    guides(colour = guide_legend("Phonation")) +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    theme2

print(RDh1h2_line_MH)
ggsave(filename = "RDh1h2_line_MH.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)




## Plotting by Phonation type
## Laryngealized

RDlaryngeal <- zapotec_timesRD %>%
    filter(phonation == "Laryngealized") %>%
    select(H2Kc, 
           H1H2c, 
           H2H4c, 
           H1A1c, 
           H1A2c, 
           H1A3c, 
           H42Kc, 
           H2KH5Kc,
           CPP,
           Energy,
           HNR05,
           HNR15,
           HNR25,
           HNR35,
           normalized_time,
           phonation,
           tone
    )

# Line graph of the Laryngealized vowels
RD.line.laryngealized <- ggplot(data = RDlaryngeal, 
                             aes(x = normalized_time, 
                                 y=H1H2c,
                                 colour=tone)
) +
    geom_smooth(method = "loess") +
    labs(title = "RD's H1-H2 values for Laryngealized vowels by tone", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-H2 (dB)") +
    theme_bw() +
    guides(colour = guide_legend("Tone") ) +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    theme2


print(RD.line.laryngealized)
ggsave(filename = "RDline_laryngealized.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## Checked
RDchecked <- zapotec_timesRD %>%
    filter(phonation == "Checked") %>%
    select(H2Kc, 
           H1H2c, 
           H2H4c, 
           H1A1c, 
           H1A2c, 
           H1A3c, 
           H42Kc, 
           H2KH5Kc,
           CPP,
           Energy,
           HNR05,
           HNR15,
           HNR25,
           HNR35,
           normalized_time,
           phonation,
           tone
    )

RD.checked.line <- ggplot(data = RDchecked, 
                       aes(x = normalized_time, 
                           y=H1H2c,
                           colour=tone)
) +
    geom_smooth(method = "loess") +
    labs(title = "RD's H1-H2 values for checked vowels by tone", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-H2 (dB)") +
    theme_bw() +
    guides(colour = guide_legend("Tone") ) +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    theme2


print(RD.checked.line)
ggsave(filename = "RDline_checked.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)


## Breathy
RDbreathy <- zapotec_timesRD %>%
    filter(phonation == "Breathy") %>%
    select(H2Kc, 
           H1H2c, 
           H2H4c, 
           H1A1c, 
           H1A2c, 
           H1A3c, 
           H42Kc, 
           H2KH5Kc,
           CPP,
           Energy,
           HNR05,
           HNR15,
           HNR25,
           HNR35,
           normalized_time,
           phonation,
           tone
    )

RDbreathy.line <- ggplot(data = RDbreathy, 
                       aes(x = normalized_time, 
                           y=H1H2c,
                           colour=tone)
) +
    geom_smooth(method = "loess") +
    labs(title = "RD's H1-H2 values for breathy vowels by tone", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-H2 (dB)") +
    theme_bw() +
    guides(colour = guide_legend("Tone") ) +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    theme2


print(RDbreathy.line)
ggsave(filename = "RDline_breathy.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## Modal
RDmodal <- zapotec_timesRD %>%
    filter(phonation == "Modal") %>%
    select(H2Kc, 
           H1H2c, 
           H2H4c, 
           H1A1c, 
           H1A2c, 
           H1A3c, 
           H42Kc, 
           H2KH5Kc,
           CPP,
           Energy,
           HNR05,
           HNR15,
           HNR25,
           HNR35,
           normalized_time,
           phonation,
           tone
    )

RDmodal.line <- ggplot(data = RDmodal, 
                     aes(x = normalized_time, 
                         y=H1H2c,
                         colour=tone)
) +
    geom_smooth(method = "loess") +
    labs(title = "RD's H1-H2 values for modal vowels by tone", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-H2 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Tone") ) +
    theme2


print(RDmodal.line)
ggsave(filename = "RDline_modal.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)


## plot of laryngealized and checked together

RDzapotec_CL <- bind_rows(list(RDlaryngeal,RDchecked), .id = "id")
RDh1h2_CL <- ggplot(data = RDzapotec_CL,  
                                aes(x = normalized_time, 
                                    y=H1H2c,
                                    linetype=phonation,
                                    colour=tone)
) +
    geom_smooth(method = "loess") +
    labs(title = "RD's Laryngeal and Checked H1-H2", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-H2 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Tone"),
           linetype = guide_legend("Phonation")
    ) +
    theme2

print(RDh1h2_CL)
ggsave(filename = "RDh1h2_CL.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## Laryngeal and modal
RDzapotec_MCL <- bind_rows(list(RDlaryngeal,RDchecked,RDmodal), .id = "id")
RDh1h2_MCL <- ggplot(data = RDzapotec_MCL,  
                   aes(x = normalized_time, 
                       y=H1H2c,
                       linetype=phonation,
                       colour=tone)
) +
    geom_smooth(method = "loess") +
    labs(title = "RD's H1-H2 values for Modal, Laryngealized, and Checked", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-H2 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Tone"),
           linetype = guide_legend("Phonation")
    ) +
    theme2

print(RDh1h2_MCL)
ggsave(filename = "RDh1h2_MCL.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## Plotting h1-a1
RDh1a1_line <- ggplot(data = zapotec_timesRD, 
                    aes(x = normalized_time, 
                        y=H1A1c,
                        group=interaction(phonation, tone),
                        linetype=tone,
                        colour=phonation)
) +
    geom_smooth(method = "loess") +
    labs(title = "RD's H1-A1 values", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-A1 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(linetype = guide_legend("Tone"), 
           colour = guide_legend("Phonation") ) +
    theme2

print(RDh1a1_line)
ggsave(filename = "RDh1a1_line.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## HA2
RDh1a2_line <- ggplot(data = zapotec_timesRD, 
                    aes(x = normalized_time, 
                        y=H1A2c,
                        group=interaction(phonation, tone),
                        linetype=tone,
                        colour=phonation)
) +
    geom_smooth(method = "loess") +
    labs(title = "RD's H1-A2 values", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-A2 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(linetype = guide_legend("Tone"), 
           colour = guide_legend("Phonation") ) +
    theme2

print(RDh1a2_line)
ggsave(filename = "RDh1a2_line.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## H1A3c
RDh1a3_line <- ggplot(data = zapotec_timesRD, 
                    aes(x = normalized_time, 
                        y=H1A3c,
                        group=interaction(phonation, tone),
                        linetype=tone,
                        colour=phonation)
) +
    geom_smooth(method = "loess") +
    labs(title = "RD's H1-A3 values", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-A3 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(linetype = guide_legend("Tone"), 
           colour = guide_legend("Phonation") ) +
    theme2

print(RDh1a3_line)
ggsave(filename = "RDh1a3_line.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## H1-H2K
H2k_line <- ggplot(data = zapotec_times, 
                   aes(x = normalized_time, 
                       y=H2Kc,
                       group=interaction(phonation, tone),
                       linetype=tone,
                       colour=phonation)
) +
    geom_smooth(method = "loess") +
    labs(title = "FSR H1-H2k values", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-H2k (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(linetype = guide_legend("Tone"), 
           colour = guide_legend("Phonation") ) +
    theme2

print(H2k_line)
ggsave(filename = "h1h2k_line.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## CPP
RDCPP_line <- ggplot(data = zapotec_timesRD, 
                   aes(x = normalized_time, 
                       y=CPP,
                       group=interaction(phonation, tone),
                       linetype=tone,
                       colour=phonation)
) +
    geom_smooth(method = "loess") +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    labs(title = "RD's CPP values", 
         x = "Normalized time (% of vowel duration)",
         y = "CPP") +
    theme_bw() +
    guides(linetype = guide_legend("Tone"), 
           colour = guide_legend("Phonation") ) +
    theme2

print(RDCPP_line)
ggsave(filename = "RDCPP_line.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

# RD's CPP at M
RDCPP_lineM <- ggplot(data = zapotec_timesRD[zapotec_timesRD$tone=="M", ], 
                     aes(x = normalized_time, 
                         y=CPP,
                         group=interaction(phonation, tone),
                         linetype=tone,
                         colour=phonation)
) +
    geom_smooth(method = "loess") +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    labs(title = "RD's CPP values at M", 
         x = "Normalized time (% of vowel duration)",
         y = "CPP") +
    theme_bw() +
    guides(linetype = guide_legend("Tone"), 
           colour = guide_legend("Phonation") ) +
    theme2

print(RDCPP_lineM)
ggsave(filename = "RDCPP_lineM.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

# RD's CPP at H
RDCPP_lineM <- ggplot(data = zapotec_timesRD[zapotec_timesRD$tone=="H", ], 
                      aes(x = normalized_time, 
                          y=CPP,
                          group=interaction(phonation, tone),
                          linetype=tone,
                          colour=phonation)
) +
    geom_smooth(method = "loess") +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    labs(title = "RD's CPP values at H", 
         x = "Normalized time (% of vowel duration)",
         y = "CPP") +
    theme_bw() +
    guides(linetype = guide_legend("Tone"), 
           colour = guide_legend("Phonation") ) +
    theme2

print(RDCPP_lineH)
ggsave(filename = "RDCPP_lineH.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

# RD's CPP at L
RDCPP_lineL <- ggplot(data = zapotec_timesRD[zapotec_timesRD$tone=="L", ], 
                      aes(x = normalized_time, 
                          y=CPP,
                          group=interaction(phonation, tone),
                          linetype=tone,
                          colour=phonation)
) +
    geom_smooth(method = "loess") +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    labs(title = "RD's CPP values at L", 
         x = "Normalized time (% of vowel duration)",
         y = "CPP") +
    theme_bw() +
    guides(linetype = guide_legend("Tone"), 
           colour = guide_legend("Phonation") ) +
    theme2

print(RDCPP_lineL)
ggsave(filename = "RDCPP_lineL.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

# RD's CPP at MH
RDCPP_lineM <- ggplot(data = zapotec_timesRD[zapotec_timesRD$tone=="MH", ], 
                      aes(x = normalized_time, 
                          y=CPP,
                          group=interaction(phonation, tone),
                          linetype=tone,
                          colour=phonation)
) +
    geom_smooth(method = "loess") +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    labs(title = "RD's CPP values at MH", 
         x = "Normalized time (% of vowel duration)",
         y = "CPP") +
    theme_bw() +
    guides(linetype = guide_legend("Tone"), 
           colour = guide_legend("Phonation") ) +
    theme2

print(RDCPP_lineMH)
ggsave(filename = "RDCPP_lineMH.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

# RD's CPP at HL
RDCPP_lineM <- ggplot(data = zapotec_timesRD[zapotec_timesRD$tone=="HL", ], 
                      aes(x = normalized_time, 
                          y=CPP,
                          group=interaction(phonation, tone),
                          linetype=tone,
                          colour=phonation)
) +
    geom_smooth(method = "loess") +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    labs(title = "RD's CPP values at HL", 
         x = "Normalized time (% of vowel duration)",
         y = "CPP") +
    theme_bw() +
    guides(linetype = guide_legend("Tone"), 
           colour = guide_legend("Phonation") ) +
    theme2

print(RDCPP_lineHL)
ggsave(filename = "RDCPP_lineHL.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

# RD H1A3 stuff
RDH1A3_H <- ggplot(data = zapotec_timesRD[zapotec_timesRD$tone == "H", ], 
                   aes(x = normalized_time, 
                       y=H1A3c,
                       group=interaction(phonation, tone),
                       linetype=tone,
                       colour=phonation)
) +
    geom_smooth(method = "loess") +
    labs(title = "RD's H1-A3 values at H", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-A3 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(linetype = guide_legend("Tone"), 
           colour = guide_legend("Phonation") ) +
    theme2

print(RDH1A3_H)
ggsave(filename = "RDH1A3_H.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

RDH1A3_HL <- ggplot(data = zapotec_timesRD[zapotec_timesRD$tone == "HL", ], 
                   aes(x = normalized_time, 
                       y=H1A3c,
                       group=interaction(phonation, tone),
                       linetype=tone,
                       colour=phonation)
) +
    geom_smooth(method = "loess") +
    labs(title = "RD's H1-A3 values at HL", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-A3 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(linetype = guide_legend("Tone"), 
           colour = guide_legend("Phonation") ) +
    theme2

print(RDH1A3_HL)
ggsave(filename = "RDH1A3_HL.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)
    
RDH1A3_MH <- ggplot(data = zapotec_timesRD[zapotec_timesRD$tone == "MH", ], 
                   aes(x = normalized_time, 
                       y=H1A3c,
                       group=interaction(phonation, tone),
                       linetype=tone,
                       colour=phonation)
) +
    geom_smooth(method = "loess") +
    labs(title = "RD's H1-A3 values at MH", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-A3 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(linetype = guide_legend("Tone"), 
           colour = guide_legend("Phonation") ) +
    theme2

print(RDH1A3_MH)
ggsave(filename = "RDH1A3_MH.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

RDH1A3_M <- ggplot(data = zapotec_timesRD[zapotec_timesRD$tone == "M", ], 
                   aes(x = normalized_time, 
                       y=H1A3c,
                       group=interaction(phonation, tone),
                       linetype=tone,
                       colour=phonation)
) +
    geom_smooth(method = "loess") +
    labs(title = "RD's H1-A3 values at M", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-A3 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(linetype = guide_legend("Tone"), 
           colour = guide_legend("Phonation") ) +
    theme2

print(RDH1A3_M)
ggsave(filename = "RDH1A3_M.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

RDH1A3_L <- ggplot(data = zapotec_timesRD[zapotec_timesRD$tone == "L", ], 
                   aes(x = normalized_time, 
                       y=H1A3c,
                       group=interaction(phonation, tone),
                       linetype=tone,
                       colour=phonation)
) +
    geom_smooth(method = "loess") +
    labs(title = "RD's H1-A3 values at L", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-A3 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(linetype = guide_legend("Tone"), 
           colour = guide_legend("Phonation") ) +
    theme2

print(RDH1A3_L)
ggsave(filename = "RDH1A3_L.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

RDH1A3_CheckedLaryngeal <- ggplot(data = RDzapotec_CL,  
                               aes(x = normalized_time, 
                                   y=H1A3c,
                                   linetype=phonation,
                                   colour=tone)
) +
    geom_smooth(method = "loess") +
    labs(title = "RD's Laryngeal and Checked H1-A3", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-A3 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Tone"),
           linetype = guide_legend("Phonation")
    ) +
    theme2

print(RDH1A3_CheckedLaryngeal)
ggsave(filename = "RDHA3_CheckedLaryngeal.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

# Phonation for joint
# Line plot
JOINh1h2_line <- ggplot(data = times.zapotec.join, 
                    aes(x = normalized_time, 
                        y=H1H2c,
                        # group=interaction(phonation, tone),
                        linetype=tone,
                        colour=phonation)
) +
    geom_smooth(method = "loess") +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    labs(title = "Joint H1-H2 values across tone and phonation", 
         x = "Phonation",
         y = "H1-H2 (dB)") +
    # theme_bw() +
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

print(JOINh1h2_line)
ggsave(filename = "JOINh1h2_line.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## Line Plot at H
JOINh1h2_line_H <- ggplot(data = times.zapotec.join[times.zapotec.join$tone=="H", ], 
                      aes(x = normalized_time, 
                          y=H1H2c,
                          group=interaction(phonation, tone),
                          colour=phonation)
) +
    geom_smooth(method = "loess") +
    labs(title = "Joint average H1-H2 at H", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-H2 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Phonation")) +
    theme2

print(JOINh1h2_line_H)
ggsave(filename = "JOINh1h2_line_H.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## Line plot at L
JOINh1h2_line_L <- ggplot(data = times.zapotec.join[times.zapotec.join$tone=="L", ], 
                      aes(x = normalized_time, 
                          y=H1H2c,
                          group=interaction(phonation, tone),
                          colour=phonation)
) +
    geom_smooth(method = "loess") +
    labs(title = "Joint average H1-H2 at L", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-H2 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Phonation")) +
    theme2

print(JOINh1h2_line_L)
ggsave(filename = "JOINh1h2_line_L.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## Line Plot at M
JOINh1h2_line_M <- ggplot(data = times.zapotec.join[times.zapotec.join$tone=="M", ], 
                      aes(x = normalized_time, 
                          y=H1H2c,
                          group=interaction(phonation, tone),
                          colour=phonation)
) +
    geom_smooth(method = "loess") +
    labs(title = "Joint average H1-H2 values at M", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-H2 (dB)") +
    theme_bw() +
    guides(colour = guide_legend("Phonation")) +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    theme2

print(JOINh1h2_line_M)
ggsave(filename = "JOINh1h2_line_M.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## Line Plot at HL
JOINh1h2_line_HL <- ggplot(data = times.zapotec.join[times.zapotec.join$tone=="HL", ], 
                       aes(x = normalized_time, 
                           y=H1H2c,
                           group=interaction(phonation, tone),
                           colour=phonation)
) +
    geom_smooth(method = "loess") +
    labs(title = "Joint average H1-H2 at HL", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-H2 (dB)") +
    theme_bw() +
    guides(colour = guide_legend("Phonation")) +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    theme2

print(JOINh1h2_line_HL)
ggsave(filename = "JOINh1h2_line_HL.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## Line Plot at MH
JOINh1h2_line_MH <- ggplot(data = times.zapotec.join[times.zapotec.join$tone=="MH", ], 
                       aes(x = normalized_time, 
                           y=H1H2c,
                           group=interaction(phonation, tone),
                           colour=phonation)
) +
    geom_smooth(method = "loess") +
    labs(title = "Joint average H1-H2 at MH", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-H2 (dB)") +
    theme_bw() +
    guides(colour = guide_legend("Phonation")) +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    theme2

print(JOINh1h2_line_MH)
ggsave(filename = "JOINh1h2_line_MH.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)




## Plotting by Phonation type
## Laryngealized

JOINlaryngeal <- times.zapotec.join %>%
    filter(phonation == "Laryngealized") %>%
    select(H2Kc, 
           H1H2c, 
           H2H4c, 
           H1A1c, 
           H1A2c, 
           H1A3c, 
           H42Kc, 
           H2KH5Kc,
           CPP,
           Energy,
           HNR05,
           HNR15,
           HNR25,
           HNR35,
           normalized_time,
           phonation,
           tone
    )

# Line graph of the Laryngealized vowels
JOINline.laryngealized <- ggplot(data = laryngeal, 
                             aes(x = normalized_time, 
                                 y=H1H2c,
                                 colour=tone)
) +
    geom_smooth(method = "loess") +
    labs(title = "Joint H1-H2 values for Laryngealized vowels by tone", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-H2 (dB)") +
    theme_bw() +
    guides(colour = guide_legend("Tone") ) +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    theme2


print(JOINline.laryngealized)
ggsave(filename = "JOINline_laryngealized.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## Checked
JOINchecked <- times.zapotec.join %>%
    filter(phonation == "Checked") %>%
    select(H2Kc, 
           H1H2c, 
           H2H4c, 
           H1A1c, 
           H1A2c, 
           H1A3c, 
           H42Kc, 
           H2KH5Kc,
           CPP,
           Energy,
           HNR05,
           HNR15,
           HNR25,
           HNR35,
           normalized_time,
           phonation,
           tone
    )

JOINchecked.line <- ggplot(data = JOINchecked, 
                       aes(x = normalized_time, 
                           y=H1H2c,
                           colour=tone)
) +
    geom_smooth(method = "loess") +
    labs(title = "Joint H1-H2 values for checked vowels by tone", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-H2 (dB)") +
    theme_bw() +
    guides(colour = guide_legend("Tone") ) +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    theme2


print(JOINchecked.line)
ggsave(filename = "JOINline_checked.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)


## Breathy
JOINbreathy <- times.zapotec.join %>%
    filter(phonation == "Breathy") %>%
    select(H2Kc, 
           H1H2c, 
           H2H4c, 
           H1A1c, 
           H1A2c, 
           H1A3c, 
           H42Kc, 
           H2KH5Kc,
           CPP,
           Energy,
           HNR05,
           HNR15,
           HNR25,
           HNR35,
           normalized_time,
           phonation,
           tone
    )

JOINbreathy.line <- ggplot(data = JOINbreathy, 
                       aes(x = normalized_time, 
                           y=H1H2c,
                           colour=tone)
) +
    geom_smooth(method = "loess") +
    labs(title = "Joint H1-H2 values for breathy vowels by tone", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-H2 (dB)") +
    theme_bw() +
    guides(colour = guide_legend("Tone") ) +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    theme2


print(JOINbreathy.line)
ggsave(filename = "JOINline_breathy.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## Modal
JOINmodal <- times.zapotec.join %>%
    filter(phonation == "Modal") %>%
    select(H2Kc, 
           H1H2c, 
           H2H4c, 
           H1A1c, 
           H1A2c, 
           H1A3c, 
           H42Kc, 
           H2KH5Kc,
           CPP,
           Energy,
           HNR05,
           HNR15,
           HNR25,
           HNR35,
           normalized_time,
           phonation,
           tone
    )

JOINmodal.line <- ggplot(data = JOINmodal, 
                     aes(x = normalized_time, 
                         y=H1H2c,
                         colour=tone)
) +
    geom_smooth(method = "loess") +
    labs(title = "Joint H1-H2 values for modal vowels by tone", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-H2 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Tone") ) +
    theme2


print(JOINmodal.line)
ggsave(filename = "JOINline_modal.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)


## plot of laryngealized and checked together

JOINzapotec_CL <- bind_rows(list(JOINlaryngeal,JOINchecked), .id = "id")
JOINh1h2_CheckedLaryngeal <- ggplot(data = JOINzapotec_CL,  
                                aes(x = normalized_time, 
                                    y=H1H2c,
                                    linetype=phonation,
                                    colour=tone)
) +
    geom_smooth(method = "loess") +
    labs(title = "Joint Laryngeal and Checked H1-H2", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-H2 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Tone"),
           linetype = guide_legend("Phonation")
    ) +
    theme2

print(JOINh1h2_CheckedLaryngeal)
ggsave(filename = "JOINh1h2_CheckedLaryngeal.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## Laryngeal and modal
JOINzapotec_MCL <- bind_rows(list(JOINlaryngeal,JOINchecked,JOINmodal), .id = "id")
JOINh1h2_MCL <- ggplot(data = JOINzapotec_MCL,  
                   aes(x = normalized_time, 
                       y=H1H2c,
                       linetype=phonation,
                       colour=tone)
) +
    geom_smooth(method = "loess") +
    labs(title = "Joint H1-H2 values for Modal, Laryngealized, and Checked", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-H2 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Tone"),
           linetype = guide_legend("Phonation")
    ) +
    theme2

print(JOINh1h2_MCL)
ggsave(filename = "JOINh1h2_MCL.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## Plotting h1-a1
JOINh1a1_line <- ggplot(data = times.zapotec.join, 
                    aes(x = normalized_time, 
                        y=H1A1c,
                        group=interaction(phonation, tone),
                        linetype=tone,
                        colour=phonation)
) +
    geom_smooth(method = "loess") +
    labs(title = "Joint H1-A1 values", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-A1 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(linetype = guide_legend("Tone"), 
           colour = guide_legend("Phonation") ) +
    theme2

print(JOINh1a1_line)
ggsave(filename = "JOINh1a1_line.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## HA2
JOINh1a2_line <- ggplot(data = times.zapotec.join, 
                    aes(x = normalized_time, 
                        y=H1A2c,
                        group=interaction(phonation, tone),
                        linetype=tone,
                        colour=phonation)
) +
    geom_smooth(method = "loess") +
    labs(title = "Joint H1-A2 values", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-A2 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(linetype = guide_legend("Tone"), 
           colour = guide_legend("Phonation") ) +
    theme2

print(JOINh1a2_line)
ggsave(filename = "JOINh1a2_line.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## H1A3c
JOINh1a3_line <- ggplot(data = times.zapotec.join, 
                    aes(x = normalized_time, 
                        y=H1A3c,
                        group=interaction(phonation, tone),
                        linetype=tone,
                        colour=phonation)
) +
    geom_smooth(method = "loess") +
    labs(title = "Joint H1-A3 values", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-A3 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(linetype = guide_legend("Tone"), 
           colour = guide_legend("Phonation") ) +
    theme2

print(JOINh1a3_line)
ggsave(filename = "JOINh1a3_line.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## H1-H2K
H2k_line <- ggplot(data = times.zapotec.join, 
                   aes(x = normalized_time, 
                       y=H2Kc,
                       group=interaction(phonation, tone),
                       linetype=tone,
                       colour=phonation)
) +
    geom_smooth(method = "loess") +
    labs(title = "FSR H1-H2k values", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-H2k (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(linetype = guide_legend("Tone"), 
           colour = guide_legend("Phonation") ) +
    theme2

print(H2k_line)
ggsave(filename = "h1h2k_line.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## CPP
JOINCPP_line <- ggplot(data = times.zapotec.join, 
                   aes(x = normalized_time, 
                       y=CPP,
                       group=interaction(phonation, tone),
                       linetype=tone,
                       colour=phonation)
) +
    geom_smooth(method = "loess") +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    labs(title = "Joint CPP values", 
         x = "Normalized time (% of vowel duration)",
         y = "CPP") +
    theme_bw() +
    guides(linetype = guide_legend("Tone"), 
           colour = guide_legend("Phonation") ) +
    theme2

print(JOINCPP_line)
ggsave(filename = "JOINCPP_line.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)



JOINHA3_CheckedLaryngeal <- ggplot(data = JOINzapotec_CL,  
                               aes(x = normalized_time, 
                                   y=H1A3c,
                                   linetype=phonation,
                                   colour=tone)
) +
    geom_smooth(method = "loess") +
    labs(title = "Joint Laryngeal and Checked H1-A3", 
         x = "Normalized time (% of vowel duration)",
         y = "H1-A3 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Tone"),
           linetype = guide_legend("Phonation")
    ) +
    theme2

print(JOINHA3_CheckedLaryngeal)
ggsave(filename = "JOINHA3_CheckedLaryngeal.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)


# Statistical analysis following Esposito 2010

# First take the plots containing the data for FSR and RD and split the data
# into fourths and then bar/boxplot each of the fourths to show the h1-h2 and 
# h1-a3 scores for each of the fourths. After this plotting, a linear 
# mixed-effects model is performed. 

## First
FSRFirst <- zapotec_timesFSR %>%
    filter(normalized_time <= 0.249) %>%
    select(H2Kc, 
           H1H2c, 
           H2H4c, 
           H1A1c, 
           H1A2c, 
           H1A3c, 
           H42Kc, 
           H2KH5Kc,
           CPP,
           Energy,
           HNR05,
           HNR15,
           HNR25,
           HNR35,
           normalized_time,
           phonation,
           tone
    )

RDFirst <- zapotec_timesRD %>%
    filter(normalized_time <= 0.249) %>%
    select(H2Kc, 
           H1H2c, 
           H2H4c, 
           H1A1c, 
           H1A2c, 
           H1A3c, 
           H42Kc, 
           H2KH5Kc,
           CPP,
           Energy,
           HNR05,
           HNR15,
           HNR25,
           HNR35,
           normalized_time,
           phonation,
           tone
    )

JointFirst <- zapotec_timesJoin %>%
    filter(normalized_time <= 0.249) %>%
    select(H2Kc, 
           H1H2c, 
           H2H4c, 
           H1A1c, 
           H1A2c, 
           H1A3c, 
           H42Kc, 
           H2KH5Kc,
           CPP,
           Energy,
           HNR05,
           HNR15,
           HNR25,
           HNR35,
           normalized_time,
           phonation,
           tone
    )

## Second
FSRSecond <- zapotec_timesFSR %>%
    filter(normalized_time >= 0.25 & normalized_time <= 0.499) %>%
    select(H2Kc, 
           H1H2c, 
           H2H4c, 
           H1A1c, 
           H1A2c, 
           H1A3c, 
           H42Kc, 
           H2KH5Kc,
           CPP,
           Energy,
           HNR05,
           HNR15,
           HNR25,
           HNR35,
           normalized_time,
           phonation,
           tone
    )

RDSecond <- zapotec_timesRD %>%
    filter(normalized_time >= 0.25 & normalized_time <= 0.499) %>%
    select(H2Kc, 
           H1H2c, 
           H2H4c, 
           H1A1c, 
           H1A2c, 
           H1A3c, 
           H42Kc, 
           H2KH5Kc,
           CPP,
           Energy,
           HNR05,
           HNR15,
           HNR25,
           HNR35,
           normalized_time,
           phonation,
           tone
    )

JointSecond <- zapotec_timesJoin %>%
    filter(normalized_time >= 0.25 & normalized_time <= 0.499) %>%
    select(H2Kc, 
           H1H2c, 
           H2H4c, 
           H1A1c, 
           H1A2c, 
           H1A3c, 
           H42Kc, 
           H2KH5Kc,
           CPP,
           Energy,
           HNR05,
           HNR15,
           HNR25,
           HNR35,
           normalized_time,
           phonation,
           tone
    )

## Third

FSRThird <- zapotec_timesFSR %>%
    filter(normalized_time >= 0.500 & normalized_time <= 0.749) %>%
    select(H2Kc, 
           H1H2c, 
           H2H4c, 
           H1A1c, 
           H1A2c, 
           H1A3c, 
           H42Kc, 
           H2KH5Kc,
           CPP,
           Energy,
           HNR05,
           HNR15,
           HNR25,
           HNR35,
           normalized_time,
           phonation,
           tone
    )

RDThird <- zapotec_timesRD %>%
    filter(normalized_time >= 0.500 & normalized_time <= 0.749) %>%
    select(H2Kc, 
           H1H2c, 
           H2H4c, 
           H1A1c, 
           H1A2c, 
           H1A3c, 
           H42Kc, 
           H2KH5Kc,
           CPP,
           Energy,
           HNR05,
           HNR15,
           HNR25,
           HNR35,
           normalized_time,
           phonation,
           tone
    )

JointThird <- zapotec_timesJoin %>%
    filter(normalized_time >= 0.500 & normalized_time <= 0.749) %>%
    select(H2Kc, 
           H1H2c, 
           H2H4c, 
           H1A1c, 
           H1A2c, 
           H1A3c, 
           H42Kc, 
           H2KH5Kc,
           CPP,
           Energy,
           HNR05,
           HNR15,
           HNR25,
           HNR35,
           normalized_time,
           phonation,
           tone
    )

## Fourth

FSRFourth <- zapotec_timesFSR %>%
    filter(normalized_time >= 0.750 & normalized_time <= 1.00) %>%
    select(H2Kc, 
           H1H2c, 
           H2H4c, 
           H1A1c, 
           H1A2c, 
           H1A3c, 
           H42Kc, 
           H2KH5Kc,
           CPP,
           Energy,
           HNR05,
           HNR15,
           HNR25,
           HNR35,
           normalized_time,
           phonation,
           tone
    )

RDFourth <- zapotec_timesRD %>%
    filter(normalized_time >= 0.750 & normalized_time <= 1.00) %>%
    select(H2Kc, 
           H1H2c, 
           H2H4c, 
           H1A1c, 
           H1A2c, 
           H1A3c, 
           H42Kc, 
           H2KH5Kc,
           CPP,
           Energy,
           HNR05,
           HNR15,
           HNR25,
           HNR35,
           normalized_time,
           phonation,
           tone
    )

JointFourth <- zapotec_timesJoin %>%
    filter(normalized_time >= 0.750 & normalized_time <= 1.00) %>%
    select(H2Kc, 
           H1H2c, 
           H2H4c, 
           H1A1c, 
           H1A2c, 
           H1A3c, 
           H42Kc, 
           H2KH5Kc,
           CPP,
           Energy,
           HNR05,
           HNR15,
           HNR25,
           HNR35,
           normalized_time,
           phonation,
           tone
    )

## Boxplots and Barplots
# FSR
FSRFirst_h1h2 <- ggplot(data = FSRFirst, 
                          aes(x = phonation, 
                              y=H1H2c,
                              #group=interaction(tone, phonation),
                              colour=phonation)
) +
    geom_boxplot() +
    labs(title = "FSR's average H1-H2 values in first fourth", 
         x = "Phonation",
         y = "H1-H2 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Phonation", ncol = 4), 
           fill = guide_legend("Phonation", ncol = 4) ) +
    my.theme
print(FSRFirst_h1h2)

FSRSecond_h1h2 <- ggplot(data = FSRSecond, 
                        aes(x = phonation, 
                            y=H1H2c,
                            #group=interaction(tone, phonation),
                            colour=phonation)
) +
    geom_boxplot() +
    labs(title = "FSR's average H1-H2 values in second fourth", 
         x = "Phonation",
         y = "H1-H2 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Phonation", ncol = 4), 
           fill = guide_legend("Phonation", ncol = 4) ) +
    my.theme
print(FSRSecond_h1h2)

FSRThird_h1h2 <- ggplot(data = FSRThird, 
                        aes(x = phonation, 
                            y=H1H2c,
                            #group=interaction(tone, phonation),
                            colour=phonation)
) +
    geom_boxplot() +
    labs(title = "FSR's average H1-H2 values in third fourth", 
         x = "Phonation",
         y = "H1-H2 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Phonation", ncol = 4), 
           fill = guide_legend("Phonation", ncol = 4) ) +
    my.theme
print(FSRThird_h1h2)

FSRFourth_h1h2 <- ggplot(data = FSRFourth, 
                        aes(x = phonation, 
                            y=H1H2c,
                            #group=interaction(tone, phonation),
                            colour=phonation)
) +
    geom_boxplot() +
    labs(title = "FSR's average H1-H2 values in last fourth", 
         x = "Phonation",
         y = "H1-H2 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Phonation", ncol = 4), 
           fill = guide_legend("Phonation", ncol = 4) ) +
    my.theme
print(FSRFourth_h1h2)

#H1-A3
FSRFirst_h1a3 <- ggplot(data = FSRFirst, 
                        aes(x = phonation, 
                            y=H1A3c,
                            #group=interaction(tone, phonation),
                            colour=phonation)
) +
    geom_boxplot() +
    labs(title = "FSR's average H1-A3 values in first fourth", 
         x = "Phonation",
         y = "H1-A3 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Phonation", ncol = 4), 
           fill = guide_legend("Phonation", ncol = 4) ) +
    my.theme
print(FSRFirst_h1a3)

FSRSecond_h1a3 <- ggplot(data = FSRSecond, 
                         aes(x = phonation, 
                             y=H1A3c,
                             #group=interaction(tone, phonation),
                             colour=phonation)
) +
    geom_boxplot() +
    labs(title = "FSR's average H1-A3 values in second fourth", 
         x = "Phonation",
         y = "H1-A3 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Phonation", ncol = 4), 
           fill = guide_legend("Phonation", ncol = 4) ) +
    my.theme
print(FSRSecond_h1a3)

FSRThird_h1a3 <- ggplot(data = FSRThird, 
                        aes(x = phonation, 
                            y=H1A3c,
                            #group=interaction(tone, phonation),
                            colour=phonation)
) +
    geom_boxplot() +
    labs(title = "FSR's average H1-A3 values in third fourth", 
         x = "Phonation",
         y = "H1-A3 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Phonation", ncol = 4), 
           fill = guide_legend("Phonation", ncol = 4) ) +
    my.theme
print(FSRThird_h1a3)

FSRFourth_h1a3 <- ggplot(data = FSRFourth, 
                         aes(x = phonation, 
                             y=H1A3c,
                             #group=interaction(tone, phonation),
                             colour=phonation)
) +
    geom_boxplot() +
    labs(title = "FSR's average H1-A3 values in last fourth", 
         x = "Phonation",
         y = "H1-A3 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Phonation", ncol = 4), 
           fill = guide_legend("Phonation", ncol = 4) ) +
    my.theme
print(FSRFourth_h1a3)


# RD
RDFirst_h1h2 <- ggplot(data = RDFirst, 
                        aes(x = phonation, 
                            y=H1H2c,
                            #group=interaction(tone, phonation),
                            colour=phonation)
) +
    geom_boxplot() +
    labs(title = "RD's average H1-H2 values in first fourth", 
         x = "Phonation",
         y = "H1-H2 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Phonation", ncol = 4), 
           fill = guide_legend("Phonation", ncol = 4) ) +
    my.theme
print(RDFirst_h1h2)

RDSecond_h1h2 <- ggplot(data = RDSecond, 
                         aes(x = phonation, 
                             y=H1H2c,
                             #group=interaction(tone, phonation),
                             colour=phonation)
) +
    geom_boxplot() +
    labs(title = "RD's average H1-H2 values in second fourth", 
         x = "Phonation",
         y = "H1-H2 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Phonation", ncol = 4), 
           fill = guide_legend("Phonation", ncol = 4) ) +
    my.theme
print(RDSecond_h1h2)

RDThird_h1h2 <- ggplot(data = RDThird, 
                        aes(x = phonation, 
                            y=H1H2c,
                            #group=interaction(tone, phonation),
                            colour=phonation)
) +
    geom_boxplot() +
    labs(title = "RD's average H1-H2 values in third fourth", 
         x = "Phonation",
         y = "H1-H2 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Phonation", ncol = 4), 
           fill = guide_legend("Phonation", ncol = 4) ) +
    my.theme
print(RDThird_h1h2)

RDFourth_h1h2 <- ggplot(data = RDFourth, 
                         aes(x = phonation, 
                             y=H1H2c,
                             #group=interaction(tone, phonation),
                             colour=phonation)
) +
    geom_boxplot() +
    labs(title = "RD's average H1-H2 values in last fourth", 
         x = "Phonation",
         y = "H1-H2 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Phonation", ncol = 4), 
           fill = guide_legend("Phonation", ncol = 4) ) +
    my.theme
print(RDFourth_h1h2)

#H1-A3
RDFirst_h1a3 <- ggplot(data = RDFirst, 
                        aes(x = phonation, 
                            y=H1A3c,
                            #group=interaction(tone, phonation),
                            colour=phonation)
) +
    geom_boxplot() +
    labs(title = "RD's average H1-A3 values in first fourth", 
         x = "Phonation",
         y = "H1-A3 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Phonation", ncol = 4), 
           fill = guide_legend("Phonation", ncol = 4) ) +
    my.theme
print(RDFirst_h1a3)

RDSecond_h1a3 <- ggplot(data = RDSecond, 
                         aes(x = phonation, 
                             y=H1A3c,
                             #group=interaction(tone, phonation),
                             colour=phonation)
) +
    geom_boxplot() +
    labs(title = "RD's average H1-A3 values in second fourth", 
         x = "Phonation",
         y = "H1-A3 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Phonation", ncol = 4), 
           fill = guide_legend("Phonation", ncol = 4) ) +
    my.theme
print(RDSecond_h1a3)

RDThird_h1a3 <- ggplot(data = RDThird, 
                        aes(x = phonation, 
                            y=H1A3c,
                            #group=interaction(tone, phonation),
                            colour=phonation)
) +
    geom_boxplot() +
    labs(title = "RD's average H1-A3 values in third fourth", 
         x = "Phonation",
         y = "H1-A3 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Phonation", ncol = 4), 
           fill = guide_legend("Phonation", ncol = 4) ) +
    my.theme
print(RDThird_h1a3)

RDFourth_h1a3 <- ggplot(data = RDFourth, 
                         aes(x = phonation, 
                             y=H1A3c,
                             #group=interaction(tone, phonation),
                             colour=phonation)
) +
    geom_boxplot() +
    labs(title = "RD's average H1-A3 values in last fourth", 
         x = "Phonation",
         y = "H1-A3 (dB)") +
    theme_bw() +
    scale_fill_manual(values=cbbPalette) + # To use for fills
    scale_colour_manual(values=cbbPalette) + # To use for line and point colors
    guides(colour = guide_legend("Phonation", ncol = 4), 
           fill = guide_legend("Phonation", ncol = 4) ) +
    my.theme
print(RDFourth_h1a3)



