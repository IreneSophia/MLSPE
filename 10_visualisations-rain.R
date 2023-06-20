# This script has been written by Irene Sophia Plank (c) 10planki@gmail.com
# It uses the summarised csv files produced by 7_merge.R and creates rain plots.

# Set working directory and load libraries --------------------------------

library(tidyverse)
library(ggplot2)
library(ggpubr)          # ggarrange
library(cowplot)         # save_plot
# load the R_rainclouds written by David Robinson
# (https://gist.github.com/dgrtwo/eb7750e74997891d7c20)
# which uses the package ggplot2 by Hadley Wickham
source("/home/emba/Documents/ML_speech/ressources/R_rainclouds.R")

setwd('/home/emba/Documents/ML_speech/Data/')

# Load data ---------------------------------------------------------------

df.dyad = read_csv('ML_dyad_230616.csv') %>%
  mutate(across(where(is.character),as.factor))

df.indi = read_csv('ML_indi_230616.csv') %>%
  mutate(across(where(is.character),as.factor)) %>%
  mutate(
    `diagnostic status` = recode(speaker, "ASD" = "autistic", "TD" = "non-autistic")
  ) 

bl = "#0072B2"
gr = "#009E73"

setwd('/home/emba/Documents/ML_speech/MLSPE_scripts/Pics')

# Individual differences --------------------------------------------------

sz = 9
    
pit_var = ggplot(df.indi, aes(x = task, y = pit_var, fill = `diagnostic status`)) +
  geom_flat_violin(aes(fill = `diagnostic status`),position = position_nudge(x = .2, y = 0), adjust = 1.5, trim = TRUE, alpha = .75, colour = NA)+
  geom_point(aes(x = as.numeric(task)-.25, y = pit_var, colour = `diagnostic status`),position = position_jitter(width=0.075), size = 2, shape = 20)+
  geom_boxplot(aes(x = task, y = pit_var, fill = `diagnostic status`),outlier.shape = NA, alpha = .75, width = .2, colour = "black")+
  scale_y_continuous(name = "pitch variance") +
  labs(x = '') +
  scale_colour_manual(values = c("autistic" = gr, "non-autistic" = bl)) +
  scale_fill_manual(values = c("autistic" = gr, "non-autistic" = bl)) + 
  ggtitle("Pitch variance")+
  theme_bw()+ 
  theme(text = element_text(size=sz), plot.title = element_text(hjust = 0.5),
        legend.position = "none")#, axis.title.y = element_blank())

int_var = ggplot(df.indi, aes(x = task, y = int_var, fill = `diagnostic status`)) +
  geom_flat_violin(aes(fill = `diagnostic status`),position = position_nudge(x = .2, y = 0), adjust = 1.5, trim = TRUE, alpha = .75, colour = NA)+
  geom_point(aes(x = as.numeric(task)-.25, y = int_var, colour = `diagnostic status`),position = position_jitter(width=0.075), size = 2, shape = 20)+
  geom_boxplot(aes(x = task, y = int_var, fill = `diagnostic status`),outlier.shape = NA, alpha = .75, width = .2, colour = "black")+
  scale_y_continuous(name = "intensity variance") +
  labs(x = '') +
  scale_colour_manual(values = c("autistic" = gr, "non-autistic" = bl)) +
  scale_fill_manual(values = c("autistic" = gr, "non-autistic" = bl)) + 
  ggtitle("Intensity variance")+
  theme_bw()+ 
  theme(text = element_text(size=sz), plot.title = element_text(hjust = 0.5),
        legend.position = "none")#, axis.title.y = element_blank())

art = ggplot(df.indi, aes(x = task, y = art, fill = `diagnostic status`)) +
  geom_flat_violin(aes(fill = `diagnostic status`),position = position_nudge(x = .2, y = 0), adjust = 1.5, trim = TRUE, alpha = .75, colour = NA)+
  geom_point(aes(x = as.numeric(task)-.25, y = art, colour = `diagnostic status`),position = position_jitter(width=0.075), size = 2, shape = 20)+
  geom_boxplot(aes(x = task, y = art, fill = `diagnostic status`),outlier.shape = NA, alpha = .75, width = .2, colour = "black")+
  scale_y_continuous(name = "syllables per phonation time") +
  labs(x = '') +
  scale_colour_manual(values = c("autistic" = gr, "non-autistic" = bl)) +
  scale_fill_manual(values = c("autistic" = gr, "non-autistic" = bl)) + 
  ggtitle("Articulation rate")+
  theme_bw()+ 
  theme(text = element_text(size=sz), plot.title = element_text(hjust = 0.5),
        legend.position = "none")#, axis.title.y = element_blank())

pit_sync = ggplot(df.indi, aes(x = task, y = pit_sync, fill = `diagnostic status`)) +
  geom_flat_violin(aes(fill = `diagnostic status`),position = position_nudge(x = .2, y = 0), adjust = 1.5, trim = TRUE, alpha = .75, colour = NA)+
  geom_point(aes(x = as.numeric(task)-.25, y = pit_sync, colour = `diagnostic status`),position = position_jitter(width=0.075), size = 2, shape = 20)+
  geom_boxplot(aes(x = task, y = pit_sync, fill = `diagnostic status`),outlier.shape = NA, alpha = .75, width = .2, colour = "black")+
  scale_y_continuous(name = "correlation coefficient") +
  labs(x = '') +
  scale_colour_manual(values = c("autistic" = gr, "non-autistic" = bl)) +
  scale_fill_manual(values = c("autistic" = gr, "non-autistic" = bl)) + 
  ggtitle("Turn-based adaptation of pitch")+
  theme_bw()+ 
  theme(text = element_text(size=sz), plot.title = element_text(hjust = 0.5),
        legend.position = "none")#, axis.title.y = element_blank())

int_sync = ggplot(df.indi, aes(x = task, y = int_sync, fill = `diagnostic status`)) +
  geom_flat_violin(aes(fill = `diagnostic status`),position = position_nudge(x = .2, y = 0), adjust = 1.5, trim = TRUE, alpha = .75, colour = NA)+
  geom_point(aes(x = as.numeric(task)-.25, y = int_sync, colour = `diagnostic status`),position = position_jitter(width=0.075), size = 2, shape = 20)+
  geom_boxplot(aes(x = task, y = int_sync, fill = `diagnostic status`),outlier.shape = NA, alpha = .75, width = .2, colour = "black")+
  scale_y_continuous(name = "correlation coefficient") +
  labs(x = '') +
  scale_colour_manual(values = c("autistic" = gr, "non-autistic" = bl)) +
  scale_fill_manual(values = c("autistic" = gr, "non-autistic" = bl)) + 
  ggtitle("Turn-based adaptation of intensity")+
  theme_bw()+ 
  theme(text = element_text(size=sz), plot.title = element_text(hjust = 0.5),
        legend.position = "none")#, axis.title.y = element_blank())

art_sync = ggplot(df.indi, aes(x = task, y = art_sync, fill = `diagnostic status`)) +
  geom_flat_violin(aes(fill = `diagnostic status`),position = position_nudge(x = .2, y = 0), adjust = 1.5, trim = TRUE, alpha = .75, colour = NA)+
  geom_point(aes(x = as.numeric(task)-.25, y = int_sync, colour = `diagnostic status`),position = position_jitter(width=0.075), size = 2, shape = 20)+
  geom_boxplot(aes(x = task, y = int_sync, fill = `diagnostic status`),outlier.shape = NA, alpha = .75, width = .2, colour = "black")+
  scale_y_continuous(name = "correlation coefficient") +
  labs(x = '') +
  scale_colour_manual(values = c("autistic" = gr, "non-autistic" = bl)) +
  scale_fill_manual(values = c("autistic" = gr, "non-autistic" = bl)) + 
  ggtitle("Turn-based adaptation of articulation rate")+
  theme_bw()+ 
  theme(text = element_text(size=sz), plot.title = element_text(hjust = 0.5),
        legend.position = "none")#, axis.title.y = element_blank())

p_leg = ggplot(df.indi, aes(x = task, y = int_sync, fill = `diagnostic status`)) +
  geom_flat_violin(aes(fill = `diagnostic status`),position = position_nudge(x = .2, y = 0), adjust = 1.5, trim = TRUE, alpha = .75, colour = NA)+
  scale_colour_manual(values = c("autistic" = gr, "non-autistic" = bl)) +
  scale_fill_manual(values = c("autistic" = gr, "non-autistic" = bl)) + 
  theme_bw()+ 
  theme(text = element_text(size=sz), plot.title = element_text(hjust = 0.5),
        legend.position = "right")#, axis.title.y = element_blank())
legend = as_ggplot(get_legend(p_leg))

p = ggarrange(pit_var, int_var, art, pit_sync, int_sync, art_sync, legend,
          #labels = c("A", "B", "C"),
          ncol = 3, nrow = 3)

ggsave("indi.png", width = 3500, height = 2500, units = "px")

# Dyadic differences ------------------------------------------------------

str = ggplot(df.dyad, aes(x = task, y = str, fill = `dyad type`)) +
  geom_flat_violin(aes(fill = `dyad type`),position = position_nudge(x = .2, y = 0), adjust = 1.5, trim = TRUE, alpha = .75, colour = NA)+
  geom_point(aes(x = as.numeric(task)-.25, y = str, colour = `dyad type`),position = position_jitter(width=0.075), size = 2, shape = 20)+
  geom_boxplot(aes(x = task, y = str, fill = `dyad type`),outlier.shape = NA, alpha = .75, width = .2, colour = "black")+
  scale_y_continuous(name = "silence-to-turn ratio") +
  labs(x = '') +
  scale_colour_manual(values = c("heterogeneous" = gr, "homogeneous" = bl)) +
  scale_fill_manual(values = c("heterogeneous" = gr, "homogeneous" = bl)) + 
  ggtitle("Silence-to-turn ratio")+
  theme_bw()+ 
  theme(text = element_text(size=sz), plot.title = element_text(hjust = 0.5),
        legend.position = "none")#, axis.title.y = element_blank())

ttg = ggplot(df.dyad, aes(x = task, y = ttg, fill = `dyad type`)) +
  geom_flat_violin(aes(fill = `dyad type`),position = position_nudge(x = .2, y = 0), adjust = 1.5, trim = TRUE, alpha = .75, colour = NA)+
  geom_point(aes(x = as.numeric(task)-.25, y = ttg, colour = `dyad type`),position = position_jitter(width=0.075), size = 2, shape = 20)+
  geom_boxplot(aes(x = task, y = ttg, fill = `dyad type`),outlier.shape = NA, alpha = .75, width = .2, colour = "black")+
  scale_y_continuous(name = "ms") +
  labs(x = '') +
  scale_colour_manual(values = c("heterogeneous" = gr, "homogeneous" = bl)) +
  scale_fill_manual(values = c("heterogeneous" = gr, "homogeneous" = bl)) + 
  ggtitle("Turn-taking gaps")+
  theme_bw()+ 
  theme(text = element_text(size=sz), plot.title = element_text(hjust = 0.5),
        legend.position = "none")#, axis.title.y = element_blank())

pit_sync_MEA = ggplot(df.dyad, aes(x = task, y = pit_sync_MEA, fill = `dyad type`)) +
  geom_flat_violin(aes(fill = `dyad type`),position = position_nudge(x = .2, y = 0), adjust = 1.5, trim = TRUE, alpha = .75, colour = NA)+
  geom_point(aes(x = as.numeric(task)-.25, y = pit_sync_MEA, colour = `dyad type`),position = position_jitter(width=0.075), size = 2, shape = 20)+
  geom_boxplot(aes(x = task, y = pit_sync_MEA, fill = `dyad type`),outlier.shape = NA, alpha = .75, width = .2, colour = "black")+
  scale_y_continuous(name = "correlation coefficient") +
  labs(x = '') +
  scale_colour_manual(values = c("heterogeneous" = gr, "homogeneous" = bl)) +
  scale_fill_manual(values = c("heterogeneous" = gr, "homogeneous" = bl)) + 
  ggtitle("Time-course synchrony of pitch")+
  theme_bw()+ 
  theme(text = element_text(size=sz), plot.title = element_text(hjust = 0.5),
        legend.position = "none")#, axis.title.y = element_blank())

int_sync_MEA = ggplot(df.dyad, aes(x = task, y = int_sync_MEA, fill = `dyad type`)) +
  geom_flat_violin(aes(fill = `dyad type`),position = position_nudge(x = .2, y = 0), adjust = 1.5, trim = TRUE, alpha = .75, colour = NA)+
  geom_point(aes(x = as.numeric(task)-.25, y = int_sync_MEA, colour = `dyad type`),position = position_jitter(width=0.075), size = 2, shape = 20)+
  geom_boxplot(aes(x = task, y = int_sync_MEA, fill = `dyad type`),outlier.shape = NA, alpha = .75, width = .2, colour = "black")+
  scale_y_continuous(name = "correlation coefficient") +
  labs(x = '') +
  scale_colour_manual(values = c("heterogeneous" = gr, "homogeneous" = bl)) +
  scale_fill_manual(values = c("heterogeneous" = gr, "homogeneous" = bl)) + 
  ggtitle("Time-course synchrony of intensity")+
  theme_bw()+ 
  theme(text = element_text(size=sz), plot.title = element_text(hjust = 0.5),
        legend.position = "none")#, axis.title.y = element_blank())

p_leg = ggplot(df.dyad, aes(x = task, y = int_sync_MEA, fill = `dyad type`)) +
  geom_flat_violin(aes(fill = `dyad type`),position = position_nudge(x = .2, y = 0), adjust = 1.5, trim = TRUE, alpha = .75, colour = NA)+
  scale_colour_manual(values = c("heterogeneous" = gr, "homogeneous" = bl)) +
  scale_fill_manual(values = c("heterogeneous" = gr, "homogeneous" = bl)) + 
  theme_bw()+ 
  theme(text = element_text(size=sz), plot.title = element_text(hjust = 0.5),
        legend.position = "right")#, axis.title.y = element_blank())
legend = as_ggplot(get_legend(p_leg))

p = ggarrange(str, ttg, legend, pit_sync_MEA, int_sync_MEA,
              #labels = c("A", "B", "C"),
              ncol = 3, nrow = 2)

ggsave("dyad.png", width = 3500, height = 2500, units = "px")
