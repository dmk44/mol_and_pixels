library(ggplot2)

simp <- data.frame(
  stratum = c(
    "< 2 см", "< 2 см", "< 2 см", "< 2 см",
    "≥ 2 см", "≥ 2 см", "≥ 2 см", "≥ 2 см",
    "Все пациенты", "Все пациенты", "Все пациенты", "Все пациенты"
  ),
  method = c(
    "Метод A", "Метод A", "Метод B", "Метод B",
    "Метод A", "Метод A", "Метод B", "Метод B",
    "Метод A", "Метод A", "Метод B", "Метод B"
  ),
  outcome = c(
    "Успех", "Неуспех", "Успех", "Неуспех",
    "Успех", "Неуспех", "Успех", "Неуспех",
    "Успех", "Неуспех", "Успех", "Неуспех"
  ),
  n = c(
    81, 6,
    234, 36,
    192, 71,
    55, 25,
    273, 77,
    289, 61
  ),
  stringsAsFactors = FALSE
)

simp$stratum <- factor(
  simp$stratum,
  levels = c("< 2 см", "≥ 2 см", "Все пациенты"),
  labels = c("Камни < 2 см", "Камни ≥ 2 см", "Все пациенты")
)

simp$method <- factor(simp$method, levels = c("Метод A", "Метод B"))
simp$outcome <- factor(simp$outcome, levels = c("Успех", "Неуспех"))

simp$fill_group <- paste(simp$method, simp$outcome, sep = "_")
totals <- aggregate(n ~ stratum + method, data = simp, sum)

p <- ggplot(simp, aes(x = method, y = n, fill = fill_group)) +
  geom_col(
    width = 0.65,
    color = "white",
    linewidth = 0.6
  ) +
  facet_wrap(~ stratum, nrow = 1) +
  geom_text(
    data = totals,
    aes(x = method, y = n + 10, label = n),
    inherit.aes = FALSE,
    size = 5,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Метод A_Успех"   = "#1f4e79",
      "Метод A_Неуспех" = "#a9c4df",
      "Метод B_Успех"   = "#a61c3c",
      "Метод B_Неуспех" = "#efb0bd"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 380),
    breaks = seq(0, 350, 50),
    expand = c(0, 0)
  ) +
  labs(
    title = "Парадокс Симпсона",
    subtitle = "Тёмные оттенки - успешные исходы",
    x = NULL,
    y = "Количество пациентов"
  ) +
  theme_classic(base_size = 15) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 13, face = "bold"),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 13),
    plot.subtitle = element_text(size = 12, color = "grey30", margin = margin(b = 10)),
    legend.position = "none",
    panel.spacing = grid::unit(1.2, "lines")
  )

p

ggsave(
  "simpsons_paradox_comparison.png",
  plot = p,
  width = 10,
  height = 5.8,
  dpi = 220,
  bg = "white"
)