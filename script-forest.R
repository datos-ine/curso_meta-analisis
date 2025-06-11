
# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  meta,
  scico,
  janitor,
  tidyverse
)


# Cargar datos ------------------------------------------------------------
datos <- dat.crisafulli2020



# Modelo meta-análisis ----------------------------------------------------
mod <- metaprop(event = cases,
                n = total,
                common = FALSE,
                studlab = study,
                data = datos)

summary(mod)


# Base para el forest plot ------------------------------------------------
datos_mod <- tibble(
  TE = c(mod$TE, mod$TE.random),
  lower = c(mod$lower, mod$lower.random),
  upper = c(mod$upper, mod$upper.random),
  studlab = c(mod$studlab, "Total") |> 
    fct_relevel("Total", after = Inf),
  total = if_else(studlab == "Total", "1", "0")
)


# Forest plot -------------------------------------------------------------
ggplot(data = datos_mod, aes(x = TE, y = fct_rev(studlab))) +
  geom_errorbarh(xmin = datos_mod$lower,
                 xmax = datos_mod$upper) +
  
  geom_point(aes(shape = total, color = total),
             size = c(rep(3, 26), 6),
             shape = c(rep(15, 26), 18)) +
  
  geom_vline(xintercept = mod$TE.random, 
             linetype = "dashed",
             color = "#1B0D33") +
  labs(x = "", y = "") +
  scale_x_continuous(transform = scales::exp_trans()) +
  scale_color_manual(values = c("#87B666","#1B0D33")) +
  theme_minimal() +
  theme(legend.position = "none")



