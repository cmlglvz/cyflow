library(tidyverse)
library(viridis)
library(hrbrthemes)
library(ggpubr)
#library(ggTimeSeries)
#library(streamgraph)


#data <- read.csv("./Data/copper_cytometry.csv", header = TRUE, sep = ";", skip = 0, skipNul = FALSE)
#data$Muestreo <- factor(data$Muestreo, levels = unique(data$Muestreo))
#data$Tiempo <- factor(data$Tiempo, levels = unique(data$Tiempo))
#data$Replica <- factor(data$Replica, levels = unique(data$Replica))

luci <- read.csv("./Data/ostreococcus_lucimarinus.csv", header = TRUE, sep = ";", skip = 0, skipNul = FALSE)
luci <- luci[,-7]
luci$Muestreo <- factor(luci$Muestreo, levels = unique(luci$Muestreo))
luci$No_Muestreo <- factor(luci$No_Muestreo, levels = unique(luci$No_Muestreo))
luci$Tiempo <- factor(luci$Tiempo, levels = unique(luci$Tiempo))
luci$Replica <- factor(luci$Replica, levels = unique(luci$Replica))

long <- gather(luci[,c(2:5,7,9,11)], "Condition", "Counts", 5:7)
long$Condition <- factor(long$Condition, levels = unique(long$Condition))

pl1 <- ggplot(long, 
              aes(x = Muestreo, 
                  y = Counts, 
                  group = Condition, 
                  color = Condition)) +
  geom_line(linewidth = 1.5) + 
  geom_point(size = 2) +
  scale_color_manual(values = c("#F94144", "#16DB93", "#0054A3")) +
  geom_smooth(show.legend = TRUE, se = TRUE, linetype = "dashed", linewidth = 0.5) +
  ggtitle("Ostreococcus lucimarinus") +
  theme_ipsum_es() +
  ylab("Counts/mL")

ggsave(plot = pl1, 
       filename = "o_lucimarinus.tiff",
       path = "./", 
       width = 10,
       dpi = 600)

real <- gather(luci[,c(2:6,8,10)], "Condition", "Counts", 5:7)
real$Condition <- factor(real$Condition, levels = unique(real$Condition))

pl3 <- ggplot(real, 
              aes(x = Muestreo,
                  y = Counts,
                  fill = Condition)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("#F94144", "#16DB93", "#0054A3")) + 
  ggtitle("Ostreococcus lucimarinus boxplot version") + 
  theme_ipsum_ps() + 
  ylab("Counts/mL")

ggsave(plot = pl3, 
       filename = "o_lucimarinus_boxplot.tiff", 
       path = "./", 
       width = 10, 
       dpi = 600)

pl2 <- ggplot(real, 
              aes(x = Muestreo, 
                  y = Counts, 
                  group = Condition, 
                  color = Condition)) +
  geom_point() +
  scale_color_manual(values = c("#F94144", "#16DB93", "#0054A3")) +
  geom_smooth() +
  ggtitle("Ostreococcus lucimarinus jitter version") +
  theme_ipsum() +
  ylab("Counts/mL")

ggsave(plot = pl2, 
       filename = "o_lucimarinus_jitter.tiff",
       path = "./", 
       width = 10, 
       dpi = 600)

#filter <- dplyr::filter(longluci, Replica == 1 & Condition == "Cobre")
#ggTimeSeries::ggplot_waterfall(dtData = filter, cXColumnName = "No_Muestreo", cYColumnName = "Counts")

comm <- read.csv("./Data/micromonas_commoda.csv", header = TRUE, sep = ";", skip = 0, skipNul = FALSE)
comm$Muestreo <- factor(comm$Muestreo, levels = unique(comm$Muestreo))
comm$No_Muestreo <- factor(comm$No_Muestreo, levels = unique(comm$No_Muestreo))
comm$Tiempo <- factor(comm$Tiempo, levels = unique(comm$Tiempo))
comm$Replica <- factor(comm$Replica, levels = unique(comm$Replica))

longc <- gather(comm[,c(2:5,7,9,11)], "Condition", "Counts", 5:7)
longc$Condition <- factor(longc$Condition, levels = unique(longc$Condition))

pc1 <- ggplot(longc, 
              aes(x = Muestreo, 
                  y = Counts, 
                  group = Condition, 
                  color = Condition)) +
  geom_line(linewidth = 1.5) + 
  geom_point(shape = 15, size = 2) +
  scale_color_manual(values = c("#16DB93", "#F94144", "#0054A3")) +
  geom_smooth(show.legend = TRUE, se = TRUE, linetype = "dashed", linewidth = 0.5) +
  ggtitle("Micromonas commoda connected scatterplot") +
  theme_ipsum() +
  ylab("Counts/mL")

ggsave(plot = pc1, 
       filename = "m_commoda_scatter_connected.tiff",
       path = "./Figures/", 
       width = 10,
       dpi = 600)

realc <- gather(comm[,c(2:6,8,10)], "Condition", "Counts", 5:7)
realc$Condition <- factor(realc$Condition, levels = unique(realc$Condition))

pc2 <- ggplot(realc, 
              aes(x = Muestreo, 
                  y = Counts, 
                  group = Condition, 
                  color = Condition)) +
  geom_point(shape = 15) +
  scale_color_manual(values = c("#16DB93", "#F94144", "#0054A3")) +
  geom_smooth() +
  ggtitle("Micromonas commoda jitter version") +
  theme_ipsum() +
  ylab("Counts/mL")

ggsave(plot = pc2, 
       filename = "m_commoda_jitter.tiff",
       path = "./Figures/", 
       width = 10, 
       dpi = 600)

pc3 <- ggplot(realc, 
              aes(x = Muestreo,
                  y = Counts,
                  fill = Condition)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("#16DB93", "#F94144", "#0054A3")) + 
  ggtitle("Micromonas commoda boxplot version") + 
  theme_ipsum() + 
  ylab("Counts/mL")

ggsave(plot = pc3, 
       filename = "m_commoda_boxplot.tiff", 
       path = "./Figures/", 
       width = 10, 
       dpi = 600)

###############################################################################

pras <- read.csv("./Data/bathycoccus_prasinos.csv", header = TRUE, sep = ";", skip = 0, skipNul = FALSE)
pras$Muestreo <- factor(pras$Muestreo, levels = unique(pras$Muestreo))
pras$No_Muestreo <- factor(pras$No_Muestreo, levels = unique(pras$No_Muestreo))
pras$Tiempo <- factor(pras$Tiempo, levels = unique(pras$Tiempo))
pras$Replica <- factor(pras$Replica, levels = unique(pras$Replica))

longp <- gather(pras[,c(2:5,7,9,11)], "Condition", "Counts", 5:7)
longp$Condition <- factor(longp$Condition, levels = unique(longp$Condition))

pp1 <- ggplot(longp, 
              aes(x = Muestreo, 
                  y = Counts, 
                  group = Condition, 
                  color = Condition)) +
  geom_line(linewidth = 1) + 
  geom_point(shape = 17, size = 2) +
  scale_color_manual(values = c("#16DB93", "#0054A3", "#F94144")) +
  geom_smooth(show.legend = TRUE, se = TRUE, linetype = "dashed", linewidth = 0.5) +
  ggtitle("Bathycoccus prasinos connected scatterplot version") +
  theme_ipsum() +
  ylab("Counts/mL")

ggsave(plot = pp1, 
       filename = "b_prasinos_scatter_connected.tiff",
       path = "./Figures/", 
       width = 10,
       dpi = 600)

realp <- gather(pras[,c(2:6,8,10)], "Condition", "Counts", 5:7)
realp$Condition <- factor(realp$Condition, levels = unique(realp$Condition))

pp2 <- ggplot(realp, 
              aes(x = Muestreo, 
                  y = Counts, 
                  group = Condition, 
                  color = Condition)) +
  geom_point(shape = 17) +
  scale_color_manual(values = c("#16DB93", "#0054A3", "#F94144")) +
  geom_smooth() +
  ggtitle("Bathycoccus prasinos jitter version") +
  theme_ipsum() +
  ylab("Counts/mL")

ggsave(plot = pp2, 
       filename = "b_prasinos_jitter.tiff",
       path = "./Figures/", 
       width = 10, 
       dpi = 600)

pp3 <- ggplot(realp, 
              aes(x = Muestreo,
                  y = Counts,
                  fill = Condition)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("#16DB93", "#0054A3", "#F94144")) + 
  ggtitle("Bathycoccus prasinos boxplot version") + 
  theme_ipsum() + 
  ylab("Counts/mL")

ggsave(plot = pp3, 
       filename = "b_prasinos_boxplot.tiff", 
       path = "./Figures/", 
       width = 10, 
       dpi = 600)

filter <- filter(realp, Condition != "Control")
filterl <- filter(longp, Condition != "Control")

pp4 <- ggplot(filterl, 
              aes(x = Muestreo, 
                  y = Counts, 
                  group = Condition, 
                  color = Condition)) +
  geom_line(linewidth = 1) + 
  geom_point(shape = 17, size = 2) +
  scale_color_manual(values = c("#0054A3", "#F94144")) +
  geom_smooth(show.legend = TRUE, se = TRUE, linetype = "dashed", linewidth = 0.5) +
  ggtitle("Bathycoccus prasinos connected scatterplot version without Control group") +
  theme_ipsum() +
  ylab("Counts/mL")

ggsave(plot = pp4, 
       filename = "b_prasinos_scatter_connected_focused.tiff",
       path = "./Figures/", 
       width = 10,
       dpi = 600)

pp5 <- ggplot(filter, 
              aes(x = Muestreo, 
                  y = Counts, 
                  group = Condition, 
                  color = Condition)) +
  geom_point(shape = 17) +
  scale_color_manual(values = c("#0054A3", "#F94144")) +
  geom_smooth() +
  ggtitle("Bathycoccus prasinos jitter version without Control group") +
  theme_ipsum() +
  ylab("Counts/mL")

ggsave(plot = pp5, 
       filename = "b_prasinos_jitter_focused.tiff",
       path = "./Figures/", 
       width = 10, 
       dpi = 600)

pp6 <- ggplot(filter,
              aes(x = Muestreo,
                  y = Counts,
                  fill = Condition)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("#0054A3", "#F94144")) + 
  ggtitle("Bathycoccus prasinos boxplot version without Control group") + 
  theme_ipsum() + 
  ylab("Counts/mL")

ggsave(plot = pp6, 
       filename = "b_prasinos_boxplot_focused.tiff", 
       path = "./Figures/", 
       width = 10, 
       dpi = 600)

ggplot(filter, aes(x=year, y=n, group=name, color=name)) +
  geom_line() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Popularity of American names in the previous 30 years") +
  theme_ipsum() +
  ylab("Number of babies born")

long$Microorganism <- as.factor(long$Microorganism)
logbase <- vegan::decostand(long$Counts, "log") %>% as.list()
long <- add_column(long, Log.Counts = logbase, .name_repair = "minimal")
long$Log.Counts <- as.numeric(long$Log.Counts)

t0 <- ggplot(long, aes(x = Muestreo, 
                       y = Counts, 
                       fill = Microorganism)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("#16DB93", "#F94144")) + 
  hrbrthemes::theme_ipsum(base_size = 23, axis_col = "#000000", axis_title_size = 30, plot_title_size = 40, axis_text_size = 20) + 
  theme() + 
  facet_wrap(~Time)

fcm.data <- read.csv("/Users/Artemis/Documents/GitHub/Remember-R/Data/abundancia_eventos_citometria_pch23.csv", header = TRUE, sep = ";", dec = ",", skip = 0, row.names = 1)
fcm.data <- fcm.data[,-c(4,10,13,16,19,22,25,28)]
fcm <- as.data.frame(t(fcm.data))
fcm <- fcm[,-6]
fcm <- add_column(fcm, 
                  Time = c(rep("t0", 3), rep("t1", 9), rep("t3", 8)),
                  Condition = c(rep("In.Cond", 3), rep("Control", 3), rep("Cu", 2), rep("Fe", 2), rep("Cu:Fe", 2), rep("Control", 2), rep("Cu", 2), rep("Fe", 2), rep("Cu:Fe", 2)),
                  .before = "PPE", 
                  .name_repair = "minimal")
fcm$Time <- factor(fcm$Time, levels = unique(fcm$Time))
fcm$Condition <- factor(fcm$Condition, levels = unique(fcm$Condition))
long <- gather(fcm[,c(1:3,5)], "Event", "Count", 3:4)
long$Event <- factor(long$Event, levels = unique(long$Event))

t0 <- ggplot(long, aes(x = Condition, 
                       y = Count, 
                       fill = Event)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("#16DB93", "#F94144")) + 
  hrbrthemes::theme_ipsum(base_size = 23, axis_col = "#000000", axis_title_size = 30, plot_title_size = 40, axis_text_size = 20) + 
  theme() + 
  facet_wrap(~Time)
t0
ggsave(plot = t0, 
       filename = "t0.tiff",
       path = "Products/", 
       width = 21, 
       height = 13, 
       dpi = 600)

t1 <- long %>% 
  filter(Time == "t1") %>% 
  ggplot(aes(x = Condition, 
             y = Count, 
             fill = Event)) + 
  geom_violin() +
  geom_boxplot() + 
  scale_fill_viridis(discrete = TRUE, option = "plasma", direction = -1) + 
  hrbrthemes::theme_ipsum(base_size = 20, axis_col = "#000000", axis_title_size = 30, plot_title_size = 40, axis_text_size = 20) + 
  theme()

ggsave(plot = t1, 
       filename = "t1.tiff",
       path = "Products/", 
       width = 21, 
       height = 13, 
       dpi = 600)

t3 <- long %>% 
  filter(Time == "t3") %>% 
  ggplot(aes(x = Condition, 
             y = Count, 
             fill = Event)) + 
  geom_boxplot() + 
  scale_fill_viridis(discrete = TRUE, option = "plasma", direction = -1) + 
  hrbrthemes::theme_ipsum(base_size = 20, axis_col = "#000000", axis_title_size = 30, plot_title_size = 40, axis_text_size = 20) + 
  theme()

ggsave(plot = t3, 
       filename = "t3.tiff",
       path = "Products/", 
       width = 21, 
       height = 13, 
       dpi = 600)

chao1 %>% 
  rename("Chao1" = "S.chao1") %>% 
  ggplot(aes(x = Site, y = Chao1, fill = Site)) + 
  geom_violin(width = 1.2) + 
  geom_boxplot(width = 0.2, color = "grey", alpha = 0.2) + 
  scale_fill_viridis(discrete = TRUE) + 
  geom_jitter(color = "black", size = 0.5, alpha = 0.5) +
  theme_ipsum() + 
  theme(legend.position = "none") + 
  ggtitle("Chao1 richness")



long.fcm <- gather(rltv.fcm, "Event", "Count", c(2,3,4,5,6,8))
fcmplot <- ggplot(long.fcm, aes(x = Sample, 
                                y = Count, 
                                fill = Event)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(x = "Condition", 
       y = "Number of events", 
       title = "Number of events per bottles (conditions)") + 
  scale_fill_viridis(discrete = TRUE, begin = 0, end = 1, direction = 1, option = "inferno") + 
  hrbrthemes::theme_ipsum(base_size = 25, axis_col = "#000000", axis_title_size = 35, plot_title_size = 40) + 
  theme(legend.position = "bottom", legend.direction = "horizontal")
legend <- cowplot::get_legend(fcmplot)
grapho <- fcmplot + theme(legend.position = "none")
ggsave(plot = grapho, 
       filename = "number_events_conditions.tiff",
       path = "Products/", 
       width = 31, 
       height = 19, 
       dpi = 600)
ggsave(ggpubr::as_ggplot(legend), 
       filename = "legend.tiff", 
       path = "Products/", 
       width = 21,
       height = 10,
       dpi = 900)
