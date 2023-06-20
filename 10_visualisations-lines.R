# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(ggpubr)          # ggarrange
library(cowplot)         # save_plot

# Load data ---------------------------------------------------------------

setwd('/home/iplank/Documents/ML_speech/Data/')

df.dyad = read_csv('ML_dyad.csv') %>%
  mutate(across(where(is.character),as.factor))

df.indi = read_csv('ML_indi.csv') %>%
  mutate(across(where(is.character),as.factor)) %>%
  mutate(
    `diagnostic status` = recode(speaker, "ASD" = "autistic", "TD" = "non-autistic")
  ) 

setwd('/home/iplank/Documents/ML_speech/Pics/')

bl = "#9C1006"
gr = "#007E36"


# Aggregate data ----------------------------------------------------------

df.dyad_agg = df.dyad %>% select(-c(dyad)) %>%
  group_by(context, task) %>%
  summarise(across(
    .cols = where(is.numeric),
    .fns = list(mean = mean, sd = sd, max = max), na.rm = T,
    .names = "{col}_{fn}"
  ))

df.indi_agg = df.indi %>% select(-c(dyad, context, subject)) %>%
  group_by(`diagnostic status`, task) %>%
  summarise(across(
    .cols = where(is.numeric),
    .fns = list(mean = mean, sd = sd, max = max), na.rm = T,
    .names = "{col}_{fn}"
  ))

# Box plots for presentations ---------------------------------------------

## Individual level: speech features

p1 = ggplot(data = df.indi, aes(x = `diagnostic status`, y = int_var, fill = `diagnostic status`)) +
  geom_boxplot() +
  labs (x = "diagnostic status", y = "variance") + 
  ggtitle("Variance of intensity") + 
  theme_classic() + 
  scale_colour_manual(values = c("autistic" = gr, "non-autistic" = bl)) +
  scale_fill_manual(values = c("autistic" = gr, "non-autistic" = bl)) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

p2 = ggplot(data = df.indi, aes(x = `diagnostic status`, y = pit_var, fill = `diagnostic status`)) +
  geom_boxplot() +
  labs (x = "diagnostic status", y = "variance") + 
  ggtitle("Variance of pitch") + 
  theme_classic() + 
  scale_colour_manual(values = c("autistic" = gr, "non-autistic" = bl)) +
  scale_fill_manual(values = c("autistic" = gr, "non-autistic" = bl)) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) 

p3 = ggplot(data = df.indi, aes(x = `diagnostic status`, y = artr, fill = `diagnostic status`)) +
  geom_boxplot() +
  labs (x = "diagnostic status", y = "syllables per second") + 
  ggtitle("Articulation rate") + 
  theme_classic() + 
  scale_colour_manual(values = c("autistic" = gr, "non-autistic" = bl)) +
  scale_fill_manual(values = c("autistic" = gr, "non-autistic" = bl)) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) 

p_leg = ggplot(data = df.indi, aes(x = `diagnostic status`, y = artr, fill = `diagnostic status`)) +
  geom_boxplot() +
  labs (x = "diagnostic status", y = "syllables per second") + 
  ggtitle("Articulation rate") + 
  theme_classic() + 
  scale_colour_manual(values = c("autistic" = gr, "non-autistic" = bl)) +
  scale_fill_manual(values = c("autistic" = gr, "non-autistic" = bl)) + 
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5)) 
legend = as_ggplot(get_legend(p_leg))

ggarrange(p1, p2, p3, legend,
          #labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)

ggsave("spindi.png", width = 2500, height = 1500, units = "px")

## Individual level: interpersonal synchronisation

p1 = ggplot(data = df.indi, aes(x = task, y = int_sync, fill = `diagnostic status`)) +
  geom_boxplot() +
  labs (x = "diagnostic status", y = "correlation coefficient") + 
  ggtitle("Turn-based synchronisation of intensity") + 
  theme_classic() + 
  scale_colour_manual(values = c("autistic" = gr, "non-autistic" = bl)) +
  scale_fill_manual(values = c("autistic" = gr, "non-autistic" = bl)) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

p2 = ggplot(data = df.indi, aes(x = task, y = pit_sync, fill = `diagnostic status`)) +
  geom_boxplot() +
  labs (x = "diagnostic status", y = "correlation coefficient") + 
  ggtitle("Turn-based synchronisation of pitch") + 
  theme_classic() + 
  scale_colour_manual(values = c("autistic" = gr, "non-autistic" = bl)) +
  scale_fill_manual(values = c("autistic" = gr, "non-autistic" = bl)) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) 

ggarrange(p1, p2, legend,
          #labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)

ggsave("syindi.png", width = 2500, height = 1500, units = "px")

## Dyad level: interpersonal synchrony

p1 = ggplot(data = df.dyad, aes(x = task, y = int_sync_MEA, fill = context)) +
  geom_boxplot() +
  labs (x = "dyad context", y = "correlation coefficient") + 
  ggtitle("Time-based synchrony of intensity") + 
  theme_classic() + 
  scale_colour_manual(values = c("heterogeneous" = gr, "homogeneous" = bl)) +
  scale_fill_manual(values = c("heterogeneous" = gr, "homogeneous" = bl)) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

p2 = ggplot(data = df.dyad, aes(x = task, y = pit_sync_MEA, fill = context)) +
  geom_boxplot() +
  labs (x = "dyad context", y = "correlation coefficient") + 
  ggtitle("Time-based synchrony of pitch") + 
  theme_classic() + 
  scale_colour_manual(values = c("heterogeneous" = gr, "homogeneous" = bl)) +
  scale_fill_manual(values = c("heterogeneous" = gr, "homogeneous" = bl)) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) 

p_leg = ggplot(data = df.dyad, aes(x = task, y = pit_sync_MEA, fill = context)) +
  geom_boxplot() +
  labs (x = "dyad context", y = "correlation coefficient") + 
  ggtitle("Time-based synchrony of pitch") + 
  theme_classic() + 
  scale_colour_manual(values = c("heterogeneous" = gr, "homogeneous" = bl)) +
  scale_fill_manual(values = c("heterogeneous" = gr, "homogeneous" = bl)) +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5)) 
legend = as_ggplot(get_legend(p_leg))

ggarrange(p1, p2, legend,
          #labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)

ggsave("sydyad.png", width = 2500, height = 1500, units = "px")

## Dyad level: communication features

p1 = ggplot(data = df.dyad, aes(x = task, y = str, fill = context)) +
  geom_boxplot() +
  labs (x = "dyad context", y = "ratio") + 
  ggtitle("Silence-to-turn ratio") + 
  theme_classic() + 
  scale_colour_manual(values = c("heterogeneous" = gr, "homogeneous" = bl)) +
  scale_fill_manual(values = c("heterogeneous" = gr, "homogeneous" = bl)) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

p2 = ggplot(data = df.dyad, aes(x = task, y = ttg, fill = context)) +
  geom_boxplot() +
  labs (x = "dyad context", y = "ms") + 
  ggtitle("Turn-taking gaps") + 
  theme_classic() + 
  scale_colour_manual(values = c("heterogeneous" = gr, "homogeneous" = bl)) +
  scale_fill_manual(values = c("heterogeneous" = gr, "homogeneous" = bl)) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) 

p_leg = ggplot(data = df.dyad, aes(x = task, y = pit_sync_MEA, fill = context)) +
  geom_boxplot() +
  labs (x = "dyad context", y = "correlation coefficient") + 
  ggtitle("Time-based synchrony of pitch") + 
  theme_classic() + 
  scale_colour_manual(values = c("heterogeneous" = gr, "homogeneous" = bl)) +
  scale_fill_manual(values = c("heterogeneous" = gr, "homogeneous" = bl)) +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5)) 
legend = as_ggplot(get_legend(p_leg))

ggarrange(p1, p2, legend,
          #labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)

ggsave("codyad.png", width = 2500, height = 1500, units = "px")


# Line graphs for presentations -------------------------------------------

w = 0.5   # width of errorbars
d = 0.4   # dodge
s = 5     # size of the point
sl = 1    # size of the lines

## Individual level: speech features

p1 = ggplot(data = df.indi_agg, aes(x = task, y = int_var_mean, 
                                    group = speaker, colour = speaker)) +
  geom_line(position = position_dodge(d), size = sl) +
  geom_errorbar(aes(ymin = int_var_mean - int_var_sd, 
                    ymax = int_var_mean + int_var_sd), size = sl, width = w, 
                position = position_dodge(d)) + 
  geom_point(position = position_dodge(d), size = s) + 
  labs (x = "diagnostic status", y = "variance") + 
  ggtitle("Variance of intensity") + 
  theme_classic() + 
  scale_colour_manual(values = c("autistic" = gr, "non-autistic" = bl)) +
  scale_fill_manual(values = c("autistic" = gr, "non-autistic" = bl)) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

p3 = ggplot(data = df.indi_agg, aes(x = task, y = sd_pitch_mean, 
                                    group = speaker_group, colour = speaker_group)) +
  geom_line(position = position_dodge(d), size = sl) +
  geom_errorbar(aes(ymin = sd_pitch_mean - sd_pitch_se, 
                    ymax = sd_pitch_mean + sd_pitch_se), width = w, size = sl, 
                position = position_dodge(d)) + 
  geom_point(size = s, position = position_dodge(d)) + 
  labs (x = "diagnosis", y = "sd") + 
  ggtitle("Standard deviation of pitch") + 
  theme_classic() + scale_colour_manual(values = MyColour) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) 

ggarrange(p1, p2, p3,
          #labels = c("A", "B", "C"),
          ncol = 3, nrow = 1)

# save_plot("/home/iplank/Documents/ML_speech/Pics/indi_speech.png",
#           plot = last_plot(),
#           base_height = 4,
#           base_asp = 1.7778) # some problem with Graphics API

## Individual level: interpersonal synchronisation

p1 = ggplot(data = df.indi_agg, aes(x = task, y = int_sync_mean, 
                                    group = speaker_group, colour = speaker_group)) +
  geom_line(position = position_dodge(d), size = sl) +
  geom_errorbar(aes(ymin = int_sync_mean - int_sync_se, 
                    ymax = int_sync_mean + int_sync_se), size = sl, width = w, 
                position = position_dodge(d)) + 
  geom_point(position = position_dodge(d), size = s) + 
  labs (x = "diagnosis", y = "sync") + 
  ggtitle("Turn-based intensity synchronisation") + 
  theme_classic() + scale_colour_manual(values = MyColour) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

p2 = ggplot(data = df.indi_agg, aes(x = task, y = pit_sync_mean, 
                                    group = speaker_group, colour = speaker_group)) +
  geom_line(position = position_dodge(d), size = sl) +
  geom_errorbar(aes(ymin = pit_sync_mean - pit_sync_se, 
                    ymax = pit_sync_mean + pit_sync_se), size = sl, width = w, 
                position = position_dodge(d)) + 
  geom_point(position = position_dodge(d), size = s) + 
  labs (x = "diagnosis", y = "sync") + 
  ggtitle("Turn-based pitch synchronisation") + 
  theme_classic() + scale_colour_manual(values = MyColour) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

ggarrange(p1, p2,
          #labels = c("A", "B", "C"),
          ncol = 2, nrow = 1)

## Dyad level: interactional features

p1d = ggplot(data = df.dyad_agg, aes(x = task, y = ttg_mean, 
                                    group = group, colour = group)) +
  geom_line(position = position_dodge(d), size = sl) +
  geom_errorbar(aes(ymin = ttg_mean - ttg_se, 
                    ymax = ttg_mean + ttg_se), size = sl, width = w, 
                position = position_dodge(d)) + 
  geom_point(position = position_dodge(d), size = s) + 
  labs (x = "dyad type", y = "ms") + 
  ggtitle("Turn-taking gap") + 
  theme_classic() + scale_colour_manual(values = MyColour) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

p2d = ggplot(data = df.dyad_agg, aes(x = task, y = ptr_mean, 
                                    group = group, colour = group)) +
  geom_line(position = position_dodge(d), size = sl) +
  geom_errorbar(aes(ymin = ptr_mean - ptr_se, 
                    ymax = ptr_mean + ptr_se), size = sl, width = w, 
                position = position_dodge(d)) + 
  geom_point(position = position_dodge(d), size = s) + 
  labs (x = "dyad type", y = "ratio") + 
  ggtitle("Pause-to-turn ratio") + 
  theme_classic() + scale_colour_manual(values = MyColour) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

p3d = ggplot(data = df.dyad_agg, aes(x = task, y = no_turns_mean, 
                                    group = group, colour = group)) +
  geom_line(position = position_dodge(d), size = sl) +
  geom_errorbar(aes(ymin = no_turns_mean - no_turns_se, 
                    ymax = no_turns_mean + no_turns_se), size = sl, width = w, 
                position = position_dodge(d)) + 
  geom_point(position = position_dodge(d), size = s) + 
  labs (x = "dyad type", y = "count") + 
  ggtitle("Number of turns") + 
  theme_classic() + scale_colour_manual(values = MyColour) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

p4d = ggplot(data = df.dyad_agg, aes(x = task, y = spr_mean, 
                                    group = group, colour = group)) +
  geom_line(position = position_dodge(d), size = sl) +
  geom_errorbar(aes(ymin = spr_mean - spr_se, 
                    ymax = spr_mean + spr_se), size = sl, width = w, 
                position = position_dodge(d)) + 
  geom_point(position = position_dodge(d), size = s) + 
  labs (x = "dyad type", y = "ratio") + 
  ggtitle("Speech rate") + 
  theme_classic() + scale_colour_manual(values = MyColour) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

ggarrange(p1d, p2d, p3d, p4d,
          #labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)

## Dyad level: interpersonal synchrony

p1d = ggplot(data = df.dyad_agg, aes(x = task, y = int_sync_mean, 
                                    group = group, colour = group)) +
  geom_line(position = position_dodge(d), size = sl) +
  geom_errorbar(aes(ymin = int_sync_mean - int_sync_se, 
                    ymax = int_sync_mean + int_sync_se), size = sl, width = w, 
                position = position_dodge(d)) + 
  geom_point(position = position_dodge(d), size = s) + 
  labs (x = "dyad type", y = "sync") + 
  ggtitle("Turn-based intensity synchrony") + 
  theme_classic() + scale_colour_manual(values = MyColour) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

p2d = ggplot(data = df.dyad_agg, aes(x = task, y = pit_sync_mean, 
                                    group = group, colour = group)) +
  geom_line(position = position_dodge(d), size = sl) +
  geom_errorbar(aes(ymin = pit_sync_mean - pit_sync_se, 
                    ymax = pit_sync_mean + pit_sync_se), size = sl, width = w, 
                position = position_dodge(d)) + 
  geom_point(position = position_dodge(d), size = s) + 
  labs (x = "dyad type", y = "sync") + 
  ggtitle("Turn-based intensity synchrony") + 
  theme_classic() + scale_colour_manual(values = MyColour) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

p3d = ggplot(data = df.dyad_agg, aes(x = task, y = int_sync_MEA_mean, 
                                    group = group, colour = group)) +
  geom_line(position = position_dodge(d), size = sl) +
  geom_errorbar(aes(ymin = int_sync_MEA_mean - int_sync_MEA_se, 
                    ymax = int_sync_MEA_mean + int_sync_MEA_se), size = sl, width = w, 
                position = position_dodge(d)) + 
  geom_point(position = position_dodge(d), size = s) + 
  labs (x = "dyad type", y = "sync") + 
  ggtitle("CCF-based intensity synchrony") + 
  theme_classic() + scale_colour_manual(values = MyColour) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

p4d = ggplot(data = df.dyad_agg, aes(x = task, y = pit_sync_MEA_mean, 
                                    group = group, colour = group)) +
  geom_line(position = position_dodge(d), size = sl) +
  geom_errorbar(aes(ymin = pit_sync_MEA_mean - pit_sync_MEA_se, 
                    ymax = pit_sync_MEA_mean + pit_sync_MEA_se), size = sl, width = w, 
                position = position_dodge(d)) + 
  geom_point(position = position_dodge(d), size = s) + 
  labs (x = "dyad type", y = "sync") + 
  ggtitle("CCF-based pitch synchrony") + 
  theme_classic() + scale_colour_manual(values = MyColour) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

ggarrange(p1d, p2d, p3d, p4d,
          #labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)

## turn-based synchrony and synchronisation

ggarrange(p1, p2, p1d, p2d,
          #labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)
