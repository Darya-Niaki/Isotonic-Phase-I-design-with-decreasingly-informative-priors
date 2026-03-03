library(ggplot2)
library(patchwork)

# First plot
dip1 <- ggplot(df_long, aes(
  x = as.factor(Index),
  y = Value,
  linetype = Method,
  shape = Method,
  color = Method
)) +
  geom_line(linewidth = 0.3, aes(group = Method)) +
  geom_point(size = 1.5) +
  labs(
    x = "Scenario",
    y = "Correct MTD selection (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  scale_shape_manual(
    name = "Maximum effective sample size",
    values = c(16, 2, 4, 1, 8)
  ) +
  scale_linetype_manual(
    name = "Maximum effective sample size",
    values = c("solid", "dashed", "dotted", "dotdash", "twodash")
  ) +
  scale_color_manual(
    values = rep("black", 5),
    name = "Maximum effective sample size"
  ) +
  scale_x_discrete(breaks = 1:10) +
  scale_y_continuous(limits = c(0, 100))


# Second plot
MTD <- ggplot(df_long_MTD, aes(
  x = as.factor(Index),
  y = Value,
  linetype = Method,
  shape = Method,
  color = Method
)) +
  geom_line(linewidth = 0.3, aes(group = Method)) +
  geom_point(size = 1.5) +
  labs(
    x = "Scenario",
    y = "Patients treated above MTD (#)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  scale_shape_manual(
    name = "Maximum effective sample size",
    values = c(16, 2, 4, 1, 8)
  ) +
  scale_linetype_manual(
    name = "Maximum effective sample size",
    values = c("solid", "dashed", "dotted", "dotdash", "twodash")
  ) +
  scale_color_manual(
    values = rep("black", 5),
    name = "Maximum effective sample size"
  ) +
  scale_x_discrete(breaks = 1:10) +
  scale_y_continuous(limits = c(0, 15))


# Combine with patchwork
plot_DIP <- (dip1 | MTD) +
  plot_annotation(title = "DIP_mean performance for TTL of 0.3") &
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  )

plot_DIP <- plot_DIP + plot_layout(guides = "collect")

plot_DIP






library(ggplot2)
library(patchwork)

## 1) Build your two plots with NO legends
dip1 <- ggplot(df_long, aes(
  x = as.factor(Index), y = Value,
  linetype = Method, shape = Method
)) +
  geom_line(linewidth = 0.3, aes(group = Method), color = "black") +
  geom_point(size = 1.5, color = "black") +
  labs(x = "Scenario", y = "Correct MTD selection (%)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  scale_shape_manual(values = c(16, 2, 4, 1, 8)) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "twodash")) +
  scale_x_discrete(breaks = 1:10) +
  scale_y_continuous(limits = c(0, 100))

MTD <- ggplot(df_long_MTD, aes(
  x = as.factor(Index), y = Value,
  linetype = Method, shape = Method
)) +
  geom_line(linewidth = 0.3, aes(group = Method), color = "black") +
  geom_point(size = 1.5, color = "black") +
  labs(x = "Scenario", y = "Patients treated above MTD (#)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  scale_shape_manual(values = c(16, 2, 4, 1, 8)) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "twodash")) +
  scale_x_discrete(breaks = 1:10) +
  scale_y_continuous(limits = c(0, 15))


## 2) Make a "legend only" plot
legend_plot <- ggplot(df_long, aes(
  x = as.factor(Index), y = Value,
  linetype = Method, shape = Method
)) +
  geom_line(aes(group = Method), color = "black") +
  geom_point(color = "black") +
  scale_shape_manual(name = "Maximum effective sample size",
                     values = c(16, 2, 4, 1, 8)) +
  scale_linetype_manual(name = "Maximum effective sample size",
                        values = c("solid", "dashed", "dotted", "dotdash", "twodash")) +
  theme_void() +
  theme(legend.position = "top")

# Extract legend as a patchwork object
legend_only <- get_legend <- function(my_plot) {
  ggplotGrob(my_plot)$grobs[which(sapply(ggplotGrob(my_plot)$grobs, function(x) x$name) == "guide-box")][[1]]
}
legend <- legend_only(legend_plot)

## 3) Combine with patchwork
final_plot <- (dip1 | MTD) / wrap_elements(legend) +
  plot_annotation(title = "DIP_mean performance for TTL of 0.3") &
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

final_plot





library(ggplot2)
library(patchwork)
library(grid)

# Helper: extract legend from a ggplot
get_legend <- function(p) {
  g <- ggplotGrob(p)
  g$grobs[[which(sapply(g$grobs, function(x) x$name) == "guide-box")]]
}

# 1) Plots without legends
dip1 <- dip1 + theme(legend.position = "none")
MTD  <- MTD + theme(legend.position = "none")

# 2) Legend-only plot (with minimal margins)
legend_plot <- ggplot(df_long, aes(
  x = as.factor(Index), y = Value,
  linetype = Method, shape = Method
)) +
  geom_line(aes(group = Method), color = "black") +
  geom_point(color = "black") +
  scale_shape_manual(name = "Maximum effective sample size",
                     values = c(16, 2, 4, 1, 8)) +
  scale_linetype_manual(name = "Maximum effective sample size",
                        values = c("solid", "dashed", "dotted", "dotdash", "twodash")) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.margin = margin(0, 0, 0, 0),  # Remove extra space
    legend.box.margin = margin(0, 0, 0, 0)
  )

legend <- get_legend(legend_plot)

# 3) Combine plots, compress legend row
final_plot <- (dip1 | MTD) /
  wrap_elements(legend) +
  plot_layout(heights = c(10, 1)) +  # reduce legend height
  plot_annotation(title = "DIP_mean performance for TTL of 0.3") &
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

final_plot





