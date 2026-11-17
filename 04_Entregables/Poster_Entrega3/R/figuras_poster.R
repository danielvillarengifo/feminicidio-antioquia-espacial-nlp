# Figuras del póster: SVG vectoriales al tamaño físico en que se imprimen (texto ≥ 24 pt).

suppressPackageStartupMessages({ library(ggplot2); library(svglite) })

VERDE_OSC <- "#456A3E"; VERDE <- "#94B43B"; GRIS <- "#55595B"; GRIS_CLARO <- "#BFC3BC"
FUENTE <- "Ancizar Sans"
dir.create("figuras", showWarnings = FALSE)

tema <- function(base = 26) {
  theme_minimal(base_size = base, base_family = FUENTE) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "#E3E5E0", linewidth = 0.6),
          axis.text = element_text(colour = GRIS), axis.title = element_text(colour = GRIS, size = base - 2),
          legend.position = "top", legend.justification = "left", legend.title = element_blank(),
          legend.text = element_text(size = base - 2), plot.margin = margin(6, 14, 6, 6))
}
guardar <- function(g, nombre, ancho_cm, alto_cm) {
  ruta <- file.path("figuras", paste0(nombre, ".svg"))
  svglite(ruta, width = ancho_cm / 2.54, height = alto_cm / 2.54, system_fonts = list(sans = FUENTE))
  print(g); invisible(dev.off()); ruta
}
pt <- function(x) x / .pt  # tamaño de geom_text en puntos

# 1. Tasa de homicidio de mujeres: Antioquia y Colombia
fig_tasas <- function() {
  firme <- filter(tasas, !preliminar | anio == 2024)
  prelim <- filter(tasas, anio >= 2024)
  # rótulo arriba para la serie más alta de cada año y abajo para la más baja
  rotulos <- filter(tasas, anio %in% c(2018, 2024)) |> group_by(anio) |>
    mutate(vj = ifelse(tasa == max(tasa), -0.9, 1.9)) |> ungroup()
  ggplot(tasas, aes(anio, tasa, colour = serie)) +
    geom_line(data = firme, linewidth = 2.2) +
    geom_line(data = prelim, linewidth = 2.2, linetype = "22") +
    geom_point(size = 5) +
    geom_text(data = rotulos, aes(label = num(tasa, 1), vjust = vj),
              size = pt(24), show.legend = FALSE, family = FUENTE) +
    scale_colour_manual(values = c(Antioquia = VERDE_OSC, Colombia = "#8E938B")) +
    scale_x_continuous(breaks = 2018:2025, labels = c(2018:2024, "2025*")) +
    scale_y_continuous(limits = c(2, 6.2), breaks = c(2, 4, 6)) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL) + tema()
}

# QR al repositorio del proyecto
qr_repo <- function(url) {
  qrcode::generate_svg(qrcode::qr_code(url, ecl = "M"), filename = "figuras/qr_repositorio.svg",
                       size = 600, foreground = VERDE_OSC, background = "white", show = FALSE)
  "figuras/qr_repositorio.svg"
}

# Entregas 2 y 3
# 6. Viabilidad: cuántos casos tiene cada celda según el nivel territorial
fig_viabilidad <- function() {
  d <- bind_rows(
    muni_anio |> transmute(nivel = "Municipio–año", k),
    sub_anio  |> transmute(nivel = "Subregión–año", k)
  ) |> count(nivel, k) |> group_by(nivel) |> mutate(p = n / sum(n)) |> ungroup() |>
    mutate(nivel = factor(nivel, levels = c("Subregión–año", "Municipio–año")))
  ggplot(d, aes(p, nivel, fill = k)) +
    geom_col(width = 0.62, position = position_stack(reverse = TRUE)) +
    geom_text(data = filter(d, p >= 0.06), aes(label = pct(p), colour = ifelse(k %in% c("0 casos", "1–2"), "#222222", "white")),
              position = position_stack(vjust = 0.5, reverse = TRUE), size = pt(22), family = FUENTE, fontface = "bold") +
    scale_colour_identity() +
    scale_fill_manual(values = c(`0 casos` = GRIS_CLARO, `1–2` = "#C9D99A", `3–9` = VERDE, `10 o más` = VERDE_OSC), drop = FALSE,
                      labels = c(`0 casos` = "0 registros", `1–2` = "1–2", `3–9` = "3–9", `10 o más` = "10 o más")) +
    scale_x_continuous(expand = c(0, 0)) +
    labs(x = NULL, y = NULL) + tema(24) +
    theme(axis.text.x = element_blank(), panel.grid.major.y = element_blank(), legend.text = element_text(size = 22)) +
    guides(fill = guide_legend(nrow = 1))
}

# 7. Antioquia: dos registros oficiales (sin fuentes no oficiales)
fig_brecha_oficial <- function() {
  d <- tip |> filter(anio >= 2018, anio <= 2024) |>
    pivot_longer(c(ml, fis), names_to = "fuente", values_to = "n") |>
    mutate(fuente = factor(fuente, levels = c("ml", "fis"),
                           labels = c("Presuntos homicidios de mujeres (Medicina Legal)", "Víctimas registradas por presunto feminicidio (Fiscalía)")))
  ggplot(d, aes(factor(anio), n, fill = fuente)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.76) +
    geom_text(aes(label = n), position = position_dodge(width = 0.8), vjust = -0.35, size = pt(20), colour = GRIS, family = FUENTE) +
    scale_fill_manual(values = c(VERDE_OSC, GRIS_CLARO)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.22))) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL) + tema(22) +
    theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank(), legend.text = element_text(size = 21)) +
    guides(fill = guide_legend(nrow = 2))
}
