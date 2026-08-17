library(ggplot2)

t <- seq(0, 10, by = 0.01)
rates <- c(0.3, 0.5, 1, 2, 3)

df <- data.frame(
  t = rep(t, length(rates)),
  density = unlist(lapply(rates, function(r) dexp(t, rate = r))),
  lambda = factor(rep(rates, each = length(t)),
                  levels = rates,
                  labels = c("λ = 0.3/ч", "λ = 0.5/ч", "λ = 1/ч", "λ = 2/ч", "λ = 3/ч"))
)

p <- ggplot(df, aes(x = t, y = density, color = lambda)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Экспоненциальное распределение",
    subtitle = "λ - интенсивность событий (в среднем событий в час)",
    x = "Время t, часы",
    y = "Плотность вероятности f(t)",
    color = NULL
  ) +
  scale_color_manual(
    values = c("#1b9e77", "#66a61e", "#e6ab02", "#d95f02", "#7570b3")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  ) +
  annotate(
    "text", x = 6.2, y = 1.2,
    label = "Нет памяти:\nпрошедшее время не делает\nсобытие ближе",
    size = 4
  )

p

ggsave("exponential_distribution.png", plot = p,
       width = 8, height = 5, dpi = 600)
