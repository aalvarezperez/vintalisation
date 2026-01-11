devtools::install_local('.', build = TRUE, upgrade = 'never', force = TRUE)

# Visual test for theme_manychat()
library(ggplot2)
library(vintalisation)

df <- data.frame(
  category = rep(c("Alpha", "Beta", "Gamma", "Delta"), each = 3),
  group = rep(c("X", "Y", "Z"), 4),
  value = c(23, 45, 32, 56, 34, 67, 45, 23, 54, 78, 45, 89)
)

# 1. Basic Manychat theme with multi-line axis title
p1 <- ggplot(df, aes(x = category, y = value, fill = group)) +
  geom_col(position = "dodge") +
  scale_fill_manychat() +
  labs(
    title = "Testing Title Spacing\nWith a Second Line",
    subtitle = "Subtitle spacing should be balanced",
    x = "Category Label",
    y = "Value\n(units)",
    caption = "Source: Test data\nGenerated for visual inspection",
    tag = "A"
  ) +
  theme_manychat()

print(p1)

# 2. Faceted plot
p2 <- ggplot(df, aes(x = group, y = value, fill = group)) +
  geom_col() +
  facet_wrap(~category) +
  scale_fill_manychat() +
  labs(title = "Faceted Plot", subtitle = "Testing strip spacing") +
  theme_manychat()

print(p2)

# 3. Legend spacing with multiple items
df_legend <- data.frame(
  x = rep(1:5, 4),
  y = c(12, 15, 14, 18, 20, 8, 10, 12, 9, 11, 25, 28, 30, 27, 32, 5, 6, 8, 7, 9),
  group = rep(c("Group A", "Group B", "Group C", "Group D"), each = 5)
)

p3 <- ggplot(df_legend, aes(x = x, y = y, colour = group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manychat() +
  labs(title = "Legend Spacing Test", subtitle = "Horizontal legend at bottom") +
  theme_manychat()

print(p3)

# 4. Small base_size
p4 <- ggplot(df, aes(x = category, y = value, fill = group)) +
  geom_col(position = "dodge") +
  scale_fill_manychat() +
  labs(title = "Small base_size = 12", y = "Value\n(units)") +
  theme_manychat(base_size = 12)

print(p4)

# 5. Large base_size
p5 <- ggplot(df, aes(x = category, y = value, fill = group)) +
  geom_col(position = "dodge") +
  scale_fill_manychat() +
  labs(title = "Large base_size = 32", y = "Value\n(units)") +
  theme_manychat(base_size = 32)

print(p5)

# 6. Vertical y-axis
p6 <- ggplot(df, aes(x = category, y = value, fill = group)) +
  geom_col(position = "dodge") +
  scale_fill_manychat() +
  labs(title = "Vertical Y-Axis", y = "Longer label\nwith line break") +
  theme_manychat(y_axis_angle = 90)

print(p6)

# 7. Right-side legend (vertical direction)
p7 <- ggplot(df, aes(x = category, y = value, fill = group)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 50, lty = "dashed") + 
  scale_fill_manychat() +
  labs(title = "Right Legend", subtitle = "Should be vertical") +
  theme_manychat(legend_position = "right")

print(p7)




library(ggplot2)
library(ggrepel)
library(vintalisation)

# Sample data
set.seed(42)
df <- data.frame(
  x = c(2.1, 3.5, 4.2, 5.8, 6.1, 7.3, 8.0, 8.9, 3.2, 6.5),
  y = c(45, 52, 38, 71, 65, 82, 78, 91, 41, 69),
  label = c("Alpha", "Beta", "Gamma", "Delta", "Epsilon",
            "Zeta", "Eta", "Theta", "Iota", "Kappa"),
  group = rep(c("A", "B"), each = 5)
)

# 1. Basic scatter with labels
p1 <- ggplot(df, aes(x = x, y = y, colour = group)) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = label), size = 5, max.overlaps = 20) +
  scale_color_manychat() +
  labs(
    title = "Scatter with Repel Labels",
    subtitle = "Testing label spacing",
    x = "Metric A",
    y = "Metric B"
  ) +
  theme_manychat()

print(p1)

# 2. More crowded scatter (stress test)
set.seed(123)
df2 <- data.frame(
  x = rnorm(25, mean = 50, sd = 15),
  y = rnorm(25, mean = 100, sd = 25),
  label = paste0("Item ", 1:25),
  category = sample(c("High", "Medium", "Low"), 25, replace = TRUE)
)

p2 <- ggplot(df2, aes(x = x, y = y, colour = category)) +
  geom_point(size = 3.5) +
  geom_text_repel(
    aes(label = label),
    size = 4,
    box.padding = 0.5,
    max.overlaps = 15,
    segment.color = "grey50"
  ) +
  scale_color_manychat() +
  labs(
    title = "Crowded Scatter Plot",
    subtitle = "25 labeled points",
    x = "Score X",
    y = "Score Y"
  ) +
  theme_manychat()

print(p2)

# 3. Selective labeling (only highlight some points)
df3 <- data.frame(
  x = c(10, 25, 40, 55, 70, 85, 30, 60, 45, 75),
  y = c(120, 95, 150, 80, 200, 170, 110, 140, 130, 185),
  name = c("Product A", "Product B", "Product C", "Product D", "Product E",
           "Product F", "Product G", "Product H", "Product I", "Product J"),
  highlight = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)
)

p3 <- ggplot(df3, aes(x = x, y = y)) +
  geom_point(aes(colour = highlight), size = 4, show.legend = FALSE) +
  geom_text_repel(
    data = subset(df3, highlight),
    aes(label = name),
    size = 5,
    fontface = "bold",
    box.padding = 0.6,
    point.padding = 0.4,
    segment.curvature = -0.1,
    segment.color = "#1F1F1F"
  ) +
  scale_colour_manual(values = c("grey70", "#3B42C4")) +
  labs(
    title = "Selective Labeling",
    subtitle = "Highlighting key data points",
    x = "Investment ($K)",
    y = "Revenue ($K)"
  ) +
  theme_manychat()

print(p3)

