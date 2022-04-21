# Packages are loaded
library(tidyverse)
library(patchwork)
library(ggpubr)
library(openxlsx)


#Note that the source data was placed in a folder called Data 
#The working directory should be adjusted accordingly
#setwd("")


# Figure 2.a

pd2a <- read.xlsx("55sourcedata.xlsx", sheet = "Figure2a")

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


# Part of Figure 2.b - observations points are not included

pd2b <- read.xlsx("55sourcedata.xlsx", sheet = "Figure2b")

ggarrange(
  pd2b %>% 
    filter( slopegrp == "High slope") %>% 
    ggplot(data = ., aes (x = timey, y = lymf)) +
    #geom_point(obspointdata, alpha = 0.2)+
    geom_line(col = "#FF7F0EFF") +
    theme_bw() +
    xlab("") + ylab("Lymphocyte count") + ggtitle("High slope") +
    xlim(c(-5, 0)) +
    coord_cartesian(ylim = c(0.5,100))+
    scale_y_log10(breaks = c(1, 10, 100, 1000)),
  
  pd2b %>% 
    filter( slopegrp == "Medium slope") %>% 
    ggplot(data = ., aes (x = timey, y = lymf)) +
    #geom_point(obspointdata, alpha = 0.2)+
    geom_line(col = "#BCBD22FF") +
    theme_bw() +
    xlab("Time to diagnosis in years") + ylab("") + ggtitle("Medium slope") +
    xlim(c(-5, 0)) +
    coord_cartesian(ylim = c(0.5,100))+
    scale_y_log10(breaks = c(1, 10, 100, 1000)),
  
  pd2b %>% 
    filter( slopegrp == "Low slope") %>% 
    ggplot(data = ., aes (x = timey, y = lymf)) +
    #geom_point(obspointdata, alpha = 0.2)+
    geom_line(col = "#9467BDFF")+
    theme_bw() +
    xlab("") + ylab("") + ggtitle("Low slope") +
    xlim(c(-5, 0)) +
    coord_cartesian(ylim = c(0.5,100))+
    scale_y_log10(breaks = c(1, 10, 100, 1000)),
  
  ncol = 3, nrow = 1
)


# Figure 2.c

pd2c <- read.xlsx("55sourcedata.xlsx", sheet = "Figure2c")

phigh <- pd2c %>% 
  filter( slopegrp == "High slope") %>%
  ggplot( aes( time, pred, linetype = estatus, col = "dummy"))+
  geom_step() + 
  theme_bw() +
  scale_color_manual(values ="#FF7F0EFF")+
  scale_linetype_manual(
    breaks = c("1", "2"), 
    labels = c("Death", "Treatment"),
    values = c(1,2),
    name = "Event"
  ) +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    legend.position = "bottom"
  ) + 
  ylab("Probability of event")+
  guides( col = "none", linetype = "none")+
  coord_cartesian( ylim = c(0,1), xlim = c(0,5)) +
  ggtitle("High slope") + xlab("")

pmedium <- pd2c %>% 
  filter( slopegrp == "Medium slope") %>%
  ggplot( aes( time, pred, linetype = estatus, col = "dummy"))+
  geom_step() + 
  theme_bw() +
  scale_color_manual(values ="#BCBD22FF")+
  scale_linetype_manual(
    breaks = c("1", "2"), 
    labels = c("Death", "Treatment"),
    values = c(1,2),
    name = "Event"
  ) +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    legend.position = "bottom"
  ) +
  ylab("Probability of event")+
  guides( col = "none")+
  coord_cartesian( ylim = c(0,1), xlim = c(0,5)) +
  ggtitle("Medium slope") + ylab("") + xlab("Time in years")


plow <- pd2c %>% 
  filter( slopegrp == "Low slope") %>%
  ggplot( aes( time, pred,  linetype = estatus,col = "dummy"))+
  geom_step() + 
  theme_bw() +
  scale_color_manual(values ="#9467BDFF")+
  scale_linetype_manual(
    breaks = c("1", "2"), 
    labels = c("Death", "Treatment"),
    values = c(1,2),
    name = "Event"
  ) +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "bottom"
  ) +
  guides( col = "none", linetype = "none")+
  ylab("Probability of event")+
  coord_cartesian( ylim = c(0,1), xlim = c(0,5)) +
  ggtitle("Low slope") + ylab("") + xlab("")


(phigh + pmedium + plow)/(guide_area()) + plot_layout( guides = "collect", heights = c(4,1))


# Figure 3 - The source data is not available, since the curves jumps at event time points

#The original plot was made using the rawdata and the following code
#ajfit <- prodlim(Hist(etime, estatus) ~ strata(SlopeGroup), data = rawdata)
#par(mfrow = c(1, 2))
#plot(ajfit, cause = 2, ylab = "Cumulative incidence of treatment", xlim = c(0,5), legend.x = "topleft")
#plot(ajfit, cause = 1, ylab = "Cumulative incidence of death without treatment", xlim = c(0,5), legend = F)


# Figure 4a

pd4a <- read.xlsx("55sourcedata.xlsx", sheet = "Figure4a")

pd4a$groupf <- factor(pd4a$groupf, levels = c("LOG", "IND", "EXP"))

pd4a %>% 
  ggplot(aes (x = timey, y = predlymf)) +
  geom_vline(xintercept = 0, col = "black", linetype = 2) +
  #geom_point(alpha = 0.2)+
  #geom_smooth(se = F, size = 5)+
  geom_line( data = pd4a, aes( y = lymf1, col = fct_reorder(slopegrp,slope)))+
  geom_line( data = pd4a, aes( y = lymf2, col = groupf))+
  coord_cartesian(xlim = c(-5, 5), ylim =c(1,300)) +
  theme_bw() +  
  xlab("Time from diagnosis in years") + 
  ylab("Lymphocyte count") +
  scale_y_log10(breaks = c(1, 3,  10, 30, 100, 300)) + 
  scale_color_manual( 
    breaks = c("High slope","Medium slope", "Low slope", "LOG","IND", "EXP"),
    values = c("#FF7F0EFF","#BCBD22FF","#9467BDFF","#1F77B4FF","#E377C2FF","#D62728FF")
  )+
  guides( col = "none")+
  facet_grid( groupf ~fct_reorder(slopegrp,slope) )+
  theme( strip.background = element_rect( fill = "white"))
#Missing observations in either lymf1 or lymf2 are deleted on purpose


#Figure 4b

pd4b <- read.xlsx("55sourcedata.xlsx", sheet = "Figure4b")

pd4b %>% 
  filter( group %in% c("exp","ind","log")) %>% 
  mutate( group = toupper(group)) %>% 
  ggplot( aes(x = slopegrp, y = psub, fill = group)) + 
  geom_col() +
  scale_x_discrete( limits = c("Low slope","Medium slope","High slope")) +
  scale_fill_manual(name = "Growth pattern", values = c("#D62728FF","#E377C2FF","#1F77B4FF")) +
  theme_bw() + 
  xlab("Slope group") + 
  ylab("Percentage (%)") +
  coord_cartesian( ylim = c(0,1))


