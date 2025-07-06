# HEATMAPS ROE, EPS Growth y Debt/Equity
# CARGAR LIBRERÍAS
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(stringr)
library(patchwork)

# === CONFIGURACIÓN DE SALIDA ===
fin_dir <- "output"
if (!dir.exists(fin_dir)) dir.create(fin_dir)

library(scales)   # Para oob = squish en EPS Growth

# === 1. CÁLCULO DE RATIOS ===
# EPS Growth
eps_df <- IncomeStatementPortfolio %>%
  filter(Concepto == "EPS (Diluted)") %>%
  pivot_longer(cols = -c(Concepto, Year), names_to = "symbol", values_to = "eps") %>%
  mutate(
    Year = ifelse(Year == "TTM", "2025", Year),
    Year = as.integer(Year)
  ) %>%
  arrange(symbol, Year) %>%
  group_by(symbol) %>%
  mutate(eps_growth = (eps - lag(eps)) / abs(lag(eps))) %>%
  ungroup() %>%
  select(symbol, year = Year, eps_growth) %>%
  filter(!is.na(eps_growth))

# ROE
oe_df <- BalanceSheetPortfolio %>%
  filter(Concepto == "Return on Equity") %>%
  pivot_longer(cols = -c(Concepto, Year), names_to = "symbol", values_to = "roe") %>%
  mutate(Year = as.integer(Year)) %>%
  select(symbol, year = Year, roe) %>%
  filter(!is.na(roe))

# Debt/Equity
debt_df <- BalanceSheetPortfolio %>%
  filter(Concepto %in% c("Total Liabilities", "Total Equity")) %>%
  pivot_longer(cols = -c(Concepto, Year), names_to = "symbol", values_to = "valor") %>%
  mutate(Year = as.integer(ifelse(Year == "TTM", 2025, Year))) %>%
  pivot_wider(names_from = Concepto, values_from = valor) %>%
  rename(debt = `Total Liabilities`, equity = `Total Equity`) %>%
  mutate(debt_equity = debt / equity) %>%
  select(symbol, year = Year, debt_equity) %>%
  filter(!is.na(debt_equity), is.finite(debt_equity))

# === 2. UNIFICAR RATIOS Y NORMALIZAR ===
ratios_tidy <- roe_df %>%
  full_join(eps_df, by = c("symbol", "year")) %>%
  full_join(debt_df, by = c("symbol", "year")) %>%
  pivot_longer(cols = c(roe, eps_growth, debt_equity), names_to = "ratio", values_to = "valor") %>%
  filter(year >= 2018 & year <= 2024, symbol %in% c("AAPL","KO","MELI","PFE","TM")) %>%
  mutate(
    symbol = recode(symbol,
                    "AAPL" = "Apple",
                    "KO"  = "Coca-Cola",
                    "MELI"= "Meli",
                    "PFE" = "Pfizer",
                    "TM"  = "Toyota"),
    symbol = factor(symbol, levels = c("Apple","Coca-Cola","Pfizer","Toyota","Meli")),
    ratio = recode(ratio,
                   "roe"         = "ROE por año",
                   "eps_growth"  = "EPS Growth por año",
                   "debt_equity" = "Debt/Equity por año"),
    year = factor(year, levels = 2018:2024)
  )

# Normalización y etiqueta de valor + valor normalizado
ratios_normalizados <- ratios_tidy %>%
  group_by(ratio, year) %>%
  mutate(
    valor_normalizado = (valor - min(valor, na.rm=TRUE)) /
      (max(valor, na.rm=TRUE) - min(valor, na.rm=TRUE)),
    label = paste0(round(valor,2), " (", round(valor_normalizado,2), ")")
  ) %>%
  ungroup()

# === 3. FUNCIÓN DE HEATMAP GENERAL ===
plot_ratio_normalizado <- function(data, ratio_label, palette) {
  df <- data %>% filter(ratio == ratio_label)
  ggplot(df, aes(x = year, y = fct_rev(symbol), fill = valor_normalizado)) +
    geom_tile(color = "white") +
    geom_text(aes(label = label), size = 3.2, color = "black", na.rm = TRUE) +
    scale_fill_gradient(low = palette[1], high = palette[3], limits = c(0,1), na.value = "grey90") +
    labs(title = ratio_label, x = NULL, y = NULL, fill = "Valor normalizado") +
    theme_minimal() +
    theme(
      plot.title    = element_text(face = "bold", size = 13, hjust = 0.5),
      axis.text.y   = element_text(size = 10),
      axis.text.x   = element_text(size = 9),
      panel.grid    = element_blank()
    )
}

# === 4. CREACIÓN DE LOS 3 HEATMAPS ===
gg_roe  <- plot_ratio_normalizado(ratios_normalizados, "ROE por año",        c("#d0f0c0","#a3d9a5","#006400"))
gg_eps  <- plot_ratio_normalizado(ratios_normalizados, "EPS Growth por año", c("#bdd7e7","#f7fbff","#08519c"))
gg_debt <- plot_ratio_normalizado(ratios_normalizados, "Debt/Equity por año",c("#fee0d2","#fc9272","#cb181d"))

# === 4\.5 Mostrar y guardar el heatmap combinado ===
# Asigna el heatmap combinado a un objeto para poder imprimirlo y guardarlo
combined_heatmap <- (gg_roe / gg_eps / gg_debt) +
  plot_annotation(
    title = "Evolución de Ratios Financieros por Activo (2018–2024) - Valores Normalizados",
    theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))
  )

# Imprime en consola / Visor de RStudio
print(combined_heatmap)

# Guarda el heatmap combinado en carpeta output/
ggsave(
  filename = file.path(fin_dir, "heatmaps_combinados.png"),
  plot     = combined_heatmap,
  width    = 10,
  height   = 8,
  dpi      = 300
)

# === 5. GRÁFICOS INDIVIDUALES ===
# Colores por empresa
company_colors <- c(
  "Apple"     = "#E41A1C",
  "Coca-Cola" = "#377EB8",
  "Pfizer"    = "#4DAF4A",
  "Toyota"    = "#984EA3",
  "Meli"      = "#FF7F00"
)

# Función para un gráfico individual
plot_individual <- function(df, ratio_label, symbol_name) {
  df %>%
    filter(ratio == ratio_label, symbol == symbol_name) %>%
    mutate(year = as.integer(as.character(year))) %>%
    ggplot(aes(x = year, y = valor)) +
    geom_line(color = company_colors[[symbol_name]], size = 1.2) +
    geom_point(color = company_colors[[symbol_name]], size = 3) +
    scale_x_continuous(breaks = 2018:2024) +
    labs(
      title = paste(symbol_name, "—", ratio_label),
      x     = "Año", y = ratio_label
    ) +
    theme_light(base_family = "sans") +
    theme(
      plot.title        = element_text(face = "bold", size = 12, hjust = 0.5),
      axis.text         = element_text(size = 8, color = "#333333"),
      axis.title        = element_text(size = 9, color = "#333333"),
      panel.grid.major  = element_line(color = "#CCCCCC", size = 0.5),
      panel.grid.minor  = element_blank(),
      panel.background  = element_rect(fill = "#FFFFFF"),
      plot.background   = element_rect(fill = "#FFFFFF"),
      legend.position   = "none"
    )
}

# Generar y guardar gráficos individuales
individual_plots <- list()
for (r in c("ROE por año", "EPS Growth por año", "Debt/Equity por año")) {
  for (s in levels(ratios_tidy$symbol)) {
    key <- paste0(gsub("[ /]","_", r), "__", s)
    p <- plot_individual(ratios_tidy, r, s)
    individual_plots[[key]] <- p
    ggsave(
      filename = file.path(fin_dir, paste0(key, ".png")),
      plot     = p,
      width    = 4,
      height   = 3,
      dpi      = 300
    )
  }
}

# === 6. GRÁFICOS GENERALES POR RATIO (5 activos juntos) ===
# ROE general
grafico_roe <- ratios_tidy %>%
  filter(ratio == "ROE por año") %>%
  mutate(year = as.integer(as.character(year))) %>%
  ggplot(aes(x = year, y = valor, color = symbol, group = symbol)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2018:2024) +
  labs(
    title = "Evolución del ROE por Empresa (2018–2024)",
    x = "Año", y = "ROE",
    color = "Empresa"
  ) +
  theme_light(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "bottom"
  )
# Guardar ROE general
ggsave(
  filename = file.path(fin_dir, "grafico_ROE_por_empresa.png"),
  plot     = grafico_roe,
  width    = 6,
  height   = 4,
  dpi      = 300
)

# EPS Growth general
grafico_eps <- ratios_tidy %>%
  filter(ratio == "EPS Growth por año") %>%
  mutate(year = as.integer(as.character(year))) %>%
  ggplot(aes(x = year, y = valor, color = symbol, group = symbol)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(-5, 5), oob = scales::squish) +
  scale_x_continuous(breaks = 2018:2024) +
  labs(
    title = "Evolución del EPS Growth por Empresa (2018–2024)",
    x = "Año", y = "EPS Growth",
    color = "Empresa"
  ) +
  theme_light(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "bottom"
  )
# Guardar EPS Growth general
ggsave(
  filename = file.path(fin_dir, "grafico_EPS_Growth_por_empresa.png"),
  plot     = grafico_eps,
  width    = 6,
  height   = 4,
  dpi      = 300
)

# Debt/Equity general
grafico_debt <- ratios_tidy %>%
  filter(ratio == "Debt/Equity por año") %>%
  mutate(year = as.integer(as.character(year))) %>%
  ggplot(aes(x = year, y = valor, color = symbol, group = symbol)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2018:2024) +
  labs(
    title = "Evolución del Debt/Equity por Empresa (2018–2024)",
    x = "Año", y = "Debt/Equity",
    color = "Empresa"
  ) +
  theme_light(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "bottom"
  )
# Guardar Debt/Equity general
ggsave(
  filename = file.path(fin_dir, "grafico_Debt_Equity_por_empresa.png"),
  plot     = grafico_debt,
  width    = 6,
  height   = 4,
  dpi      = 300
)

# Ahora tienes los 15 PNG individuales y 3 PNG generales en 'output/'
# En tu RMarkdown/PDF puedes enlazarlos directamente:
# ![ROE general](output/grafico_ROE_por_empresa.png)
# ![Apple — ROE](output/ROE_por_año__Apple.png)
# ...
# - Variables gg_roe, gg_eps, gg_debt con los 3 heatmaps
# - Carpeta 'output/' con 15 PNG individuales para hipervínculos

# === 7. GENERAR ÍNDICE DE HIPERVÍNCULOS PARA CANVA ===

# 7.1 Asegúrate de tener definida antes:
# fin_dir <- "output"
# if (!dir.exists(fin_dir)) dir.create(fin_dir)

# 7.2 Listar todos los PNG recién creados
all_files <- list.files(fin_dir, pattern = "\\\\.png$", full.names = FALSE)

# 7.3 Títulos en el mismo orden
titles <- c(
  "Heatmaps combinados",
  "General ROE", "General EPS Growth", "General Debt/Equity",
  paste0(c("Apple — ROE","Coca-Cola — ROE","Pfizer — ROE","Toyota — ROE","Meli — ROE")),
  paste0(c("Apple — EPS Growth","Coca-Cola — EPS Growth","Pfizer — EPS Growth","Toyota — EPS Growth","Meli — EPS Growth")),
  paste0(c("Apple — Debt/Equity","Coca-Cola — Debt/Equity","Pfizer — Debt/Equity","Toyota — Debt/Equity","Meli — Debt/Equity"))
)

# 7.4 Construir el Markdown
md_lines <- c("# Índice de Gráficos 🔗", "")
for(i in seq_along(all_files)){
  file <- all_files[i]
  title <- titles[i]
  md_lines <- c(md_lines,
                sprintf("- %s 🔗 [ver](%s/%s)", title, fin_dir, file)
  )
}

# 7.5 Guardar como MD para copiar a Canva
writeLines(md_lines, con = file.path(fin_dir, "indice_graficos.md"))
cat("📄 Se ha generado:", file.path(fin_dir, "indice_graficos.md"), "\n")

