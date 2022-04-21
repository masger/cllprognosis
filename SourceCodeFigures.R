# Packages are loaded
library(tidyverse)
library(patchwork)


# Figure 2.a

pd2a <- read.xlsx(file = "./Data/55sourcedata.xlsx", sheetName = "Figure2a")

plymf <- pd2a %>% 
  filter( outcome == "lymf") %>% 
  ggplot() +
  geom_vline( xintercept = 0 , linetype = 2 ) +
  geom_line( aes(  timey, pred, col = group))+
  xlab("Time to diagnosis in years") +
  ylab("Lymphocyte count")+
  scale_color_manual( name = "", values = c("#1F77B4FF","#D62728FF")) +
  coord_cartesian( ylim = c(0,16)) +
  theme_bw()

phb <- pd2a %>% 
  filter( outcome == "hb") %>% 
  ggplot() +
  geom_vline( xintercept = 0 , linetype = 2 ) +
  geom_line( aes(  timey, pred, col = group))+
  xlab("Time to diagnosis in years") +
  ylab("Hemoglobin count")+
  scale_color_manual( name = "", values = c("#1F77B4FF","#D62728FF")) +
  coord_cartesian( ylim = c(8,10)) +
  theme_bw()

pcrp <- pd2a %>% 
  filter( outcome == "crp") %>% 
  ggplot() +
  geom_vline( xintercept = 0 , linetype = 2 ) +
  geom_line( aes(  timey, pred, col = group))+
  xlab("Time to diagnosis in years") +
  ylab("CRP")+
  scale_color_manual( name = "", values = c("#1F77B4FF","#D62728FF")) +
  coord_cartesian( ylim = c(4,5)) +
  theme_bw()

ptrc <- pd2a %>% 
  filter( outcome == "trc") %>% 
  ggplot() +
  geom_vline( xintercept = 0 , linetype = 2 ) +
  geom_line( aes(  timey, pred, col = group))+
  xlab("Time to diagnosis in years") +
  ylab("Platelet count")+
  scale_color_manual( name = "", values = c("#1F77B4FF","#D62728FF")) +
  coord_cartesian( ylim = c(200,280)) +
  theme_bw()


(plymf + phb) / (pcrp + ptrc) + 
  plot_layout( guides = "collect") +
  plot_annotation( title = "Predicted biomarker trajectories for a male age 70")


# Part of Figure 2.b - individual observations points are not included



