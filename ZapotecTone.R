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
# Two way table for tone and phonation 
FSRtone_phonation <- table(zapotecVS$tone, zapotecVS$phonation)
FSRtone_phonation

RDtone_phonation <- table(zapotecRD$tone, zapotecRD$phonation)
RDtone_phonation


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

zapotec_times2 <- zapotecVS %>% 
    mutate(normalized_time = round((t_ms-seg_Start)/(seg_End-seg_Start),digits = 2)) %>%
    filter(tone != 'M' & tone != 'MH' )


# Plotting the tone
# ggplot of normalized tone (y-axis) over time (x axis)
tone_plot <- ggplot(data = zapotec_times, 
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
ggsave(filename = "h1h2_line_Mless.png", 
       device = "png", 
       units = "in", 
       width=16, 
       height=9, 
       dpi=600)

## Line Plot at H
h1h2_line_H <- ggplot(data = zapotec_times2[zapotec_times2$tone=="H", ], 
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
                          y=HH2c,
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
                            y=HH2c,
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
                             y=HH2c,
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

breathy.line <- ggplot(data = breathy, 
                       aes(x = normalized_time, 
                           y=HH2c,
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

modal.line <- ggplot(data = modal, 
                       aes(x = normalized_time, 
                           y=HH2c,
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
    labs(title = "Laryngeal and Checked H1-H2", 
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
                                    y=HH2c,
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
                        y=HA1c,
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
                        y=HA2c,
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

## HA3c
h1a3_line <- ggplot(data = zapotec_times, 
                    aes(x = normalized_time, 
                        y=HA3c,
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
                                    y=HA3c,
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

