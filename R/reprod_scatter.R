# Reproduction of figure 4a from ID 96

library(ggplot2)
library(cowplot)
library(magick)

# import data from table in text, anonymised
df_text <- read.table(text = 'Period	206Pb/204Pb	207Pb/204Pb	208Pb/204Pb
Group_4	18.384	15.649	38.908
Group_4	18.332	15.660	38.770
Group_4	23.553	16.443	42.955
Group_4	17.577	15.489	38.172
Group_1	17.567	15.499	38.148
Group_1	17.723	15.539	38.193
Group_1	17.734	15.544	38.382
Group_1	17.549	15.483	38.134
Group_1	17.756	15.529	38.272
Group_1	17.750	15.532	38.297
Group_1	17.670	15.529	38.356
Group_1	17.682	15.522	38.342
Group_1	17.618	15.501	38.141
Group_1	17.629	15.513	38.227
Group_1	17.619	15.506	38.219
Group_1	18.007	15.577	38.502
Group_1	17.753	15.532	38.317
Group_1	17.697	15.523	38.326
Group_1	17.503	15.476	38.094
Group_1	17.564	15.475	38.031
Group_1	17.565	15.488	38.156
Group_1	18.291	15.620	38.910
Group_1	17.568	15.490	38.134
Group_2	17.682	15.526	38.323
Group_2	17.854	15.556	38.495
Group_2	17.730	15.538	38.371
Group_2	17.513	15.475	38.049
Group_2	17.717	15.539	38.363
Group_2	17.768	15.551	38.417
Group_2	17.632	15.508	38.230
Group_3	17.524	15.502	37.953
Group_3	17.682	15.526	38.324
Group_3	17.612	15.538	38.299
Group_3	17.628	15.509	38.213
Group_3	17.666	15.532	38.198',
                      header = TRUE)

# import data from appendix, filtered for site (as stated in caption), anonymised
df_app <- read.table(text = 'Period	206Pb/204Pb	207Pb/204Pb	208Pb/204Pb	207Pb/206Pb	208Pb/206Pb
Group_1	18.547	15.695	39.119	0.846	2.109
Group_1	17.567	15.499	38.148	0.882	2.172
Group_1	17.723	15.539	38.193	0.877	2.155
Group_1	17.734	15.544	38.382	0.876	2.164
Group_1	17.549	15.483	38.134	0.882	2.173
Group_1	17.756	15.529	38.272	0.875	2.155
Group_1	17.750	15.532	38.297	0.875	2.158
Group_1	17.670	15.529	38.356	0.879	2.171
Group_1	17.682	15.522	38.342	0.878	2.168
Group_1	17.618	15.501	38.141	0.880	2.165
Group_1	17.629	15.513	38.227	0.880	2.168
Group_1	17.619	15.506	38.219	0.880	2.169
Group_1	18.007	15.577	38.502	0.865	2.138
Group_1	17.753	15.532	38.317	0.875	2.158
Group_1	17.697	15.523	38.326	0.877	2.166
Group_1	17.503	15.476	38.094	0.884	2.176
Group_1	17.564	15.475	38.031	0.881	2.165
Group_1	17.565	15.488	38.156	0.882	2.172
Group_1	18.291	15.620	38.910	0.854	2.127
Group_1	17.568	15.490	38.134	0.882	2.171
Group_1	17.626	15.518	38.250	0.880	2.170
Group_1	19.334	15.796	40.171	0.817	2.078
Group_1	17.888	15.587	38.377	0.871	2.145
Group_1	17.984	15.569	38.451	0.866	2.138
Group_1	17.637	15.511	38.086	0.879	2.159
Group_1	17.652	15.523	38.280	0.879	2.169
Group_1	18.118	15.641	38.676	0.863	2.135
Group_1	17.685	15.533	38.345	0.878	2.168
Group_1	17.642	15.504	38.220	0.879	2.166
Group_1	17.707	15.539	38.331	0.878	2.165
Group_1	17.481	15.463	38.013	0.885	2.175
Group_1	17.682	15.543	38.278	0.879	2.165
Group_2	18.271	15.632	38.778	0.856	2.122
Group_2	18.222	15.663	38.718	0.860	2.125
Group_2	17.736	15.545	38.374	0.876	2.164
Group_2	17.810	15.553	38.500	0.873	2.162
Group_2	17.730	15.554	38.405	0.877	2.166
Group_2	17.916	15.576	38.633	0.869	2.156
Group_2	17.734	15.550	38.397	0.877	2.165
Group_2	17.973	15.582	38.618	0.867	2.149
Group_2	17.715	15.546	38.367	0.878	2.166
Group_2	17.723	15.552	38.409	0.878	2.167
Group_2	17.707	15.539	38.355	0.878	2.166
Group_2	17.800	15.549	38.466	0.874	2.161
Group_2	17.682	15.526	38.323	0.878	2.167
Group_2	17.854	15.556	38.495	0.871	2.156
Group_2	17.730	15.538	38.371	0.876	2.164
Group_2	17.513	15.475	38.049	0.884	2.173
Group_2	17.717	15.539	38.363	0.877	2.165
Group_2	17.768	15.551	38.417	0.875	2.162
Group_2	17.632	15.508	38.230	0.880	2.168
Group_3	17.729	15.522	38.310	0.876	2.161
Group_3	17.774	15.559	38.454	0.875	2.163
Group_3	17.930	15.582	38.564	0.869	2.151
Group_3	17.795	15.552	38.472	0.874	2.162
Group_3	17.788	15.560	38.470	0.875	2.163
Group_3	17.790	15.551	38.450	0.874	2.161
Group_3	17.771	15.546	38.364	0.875	2.159
Group_3	17.712	15.538	38.386	0.877	2.167
Group_3	17.877	15.578	38.508	0.871	2.154
Group_3	17.763	15.552	38.445	0.876	2.164
Group_3	17.716	15.539	38.390	0.877	2.167
Group_3	17.736	15.551	38.403	0.877	2.165
Group_3	17.760	15.545	38.408	0.875	2.163
Group_3	17.757	15.545	38.446	0.875	2.165
Group_3	17.663	15.524	38.259	0.879	2.166
Group_3	17.524	15.502	37.953	0.885	2.166
Group_3	17.682	15.526	38.324	0.878	2.167
Group_3	17.612	15.538	38.299	0.882	2.175
Group_3	17.628	15.509	38.213	0.880	2.168
Group_3	17.666	15.532	38.198	0.879	2.162',
                     header = TRUE)


df_text$Period <- gsub("\\_", " ", df_text$Period)
df_app$Period <- gsub("\\_", " ", df_app$Period)

# plot data from table in text
ggplot(df_text, aes(x = X206Pb.204Pb, y = X207Pb.204Pb, colour = Period)) +
  geom_point() +
  theme_bw() # results in outlier from Warring States period

# plot data from appendix
ggplot(df_app, aes(x = X206Pb.204Pb, y = X207Pb.204Pb, colour = Period)) +
  geom_point() +
  theme_bw()

# define common design elements
plot_design <- list(
  scale_color_discrete(palette = c("black", "red", "blue", "forestgreen")),
  scale_shape_manual(values = c("Group 1" = 12, "Group 2" = 10, "Group 3" = 17, "Group 4" = 15)),
  scale_x_continuous(name = expression(""^{206}*"Pb/"^{204}*Pb), limits = c(17.3, 18.6), expand = expansion(0)),
  scale_y_continuous(name = expression(""^{207}*"Pb/"^{204}*Pb), limits = c(15.4, 15.7001), expand = expansion(0)),
  theme_bw(base_size = 10),
  theme(
    panel.grid = element_blank(),
    legend.box.background = element_rect(colour = "black", linewidth = 0.45, fill = "red"),
    legend.position = "inside",
    legend.position.inside = c(0, 1),
    legend.justification = c(0, 1),
    legend.title = element_blank(),
    legend.margin = margin(0, 0, 0, 0, "mm"),
    legend.box.margin = margin(0, 0, 0, 0, "mm"),
    legend.text = element_text(margin = margin(0, 3, 0, 0, "mm")),
    legend.key.spacing.y = unit(-5, "pt"),
    plot.margin = margin(0, 5, 0, 0, "mm"),
    aspect.ratio = 746/1040
  )
)

# reproduce figure with data from table in text, resulting in removal of outlier
b <- ggplot(df_text, aes(x = X206Pb.204Pb, y = X207Pb.204Pb, colour = Period, shape = Period)) +
  geom_point() +
  plot_design

# reproduce figure with data from appendix + Group 4 from data in text
# (because there is no group 4 in appendix data), resulting in removal of outlier
c <- ggplot(df_app, aes(x = X206Pb.204Pb, y = X207Pb.204Pb, colour = Period, shape = Period)) +
  geom_point() +
  geom_point(data = subset(df_text, Period == "Group 4")) +
  plot_design

# import anaonymised original plot (requires package magick)
a <- ggdraw(clip = TRUE) +
  draw_image("analysis/supplementary-materials/ID96_original_anonymised.tif")

# combine plots for output
plot <- plot_grid(a, b, c, labels = c("a", "b", "c"), ncol = 3)

# export
ggsave("fig_repro_scatter.tif", plot = plot,
       height = 8.5, width = 10*3, dpi = 1200, units = "cm", compression = "lzw"
       )

