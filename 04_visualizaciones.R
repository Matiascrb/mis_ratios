# 04 VISUALIZACIONES
# Integrante1:  Santiago Molina Torrillate
# Legajo: 909503
# Email: santiagomolina46@gmail.com

# Integrante2:  Matias Corbalan
# Legajo: 894445
# Email: matiascrb@gmail.com

# 1. Instalo y cargo paquetes necesarios ---------------------------------------
paquetes <- c(
  "tidyverse",   # manipulación y visualización (ggplot2, dplyr, tidyr, etc.)
  "scales",      # formatos numéricos y escalas
  "patchwork",   # combinar múltiples gráficos
  "ggrepel",     # etiquetas repelentes
  "waffle",      # gráficos de waffle
  "ggthemes"     # temas adicionales
)
for (pkg in paquetes) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

# 2. Defino directorios, fin_dir y carga de datos -----------------------------
fin_dir    <- "output"
if (!dir.exists(fin_dir)) dir.create(fin_dir)
input_dir  <- "input"
# Leer los data.frames preparados en la sección 02
precios_wide             <- read.csv(file.path(input_dir, "precios_activos_wide.csv"))
IncomeStatementPortfolio <- read.csv(file.path(input_dir, "IncomeStatementPortfolio.csv"))
BalanceSheetPortfolio     <- read.csv(file.path(input_dir, "BalanceSheetPortfolio.csv"))

# 3. Gráficos --------------------------------------------------------------

# --- Parámetros comunes ---
colores_empresas <- c(
  "AAPL" = "#264b96",   # azul fuerte
  "KO"   = "#ff7f03",   # naranja
  "MELI" = "#66a61e",   # verde
  "PFE"  = "#f5a1c5",   # rosa
  "TM"   = "#af83cf"    # violeta
)

# 3.1 Evolución de Ingresos Indexados (log) -----------------------------------
revenue_df <- IncomeStatementPortfolio %>%
  filter(str_detect(tolower(Concepto), "revenue")) %>%
  pivot_longer(cols = -c(Concepto, Year), names_to = "Empresa", values_to = "Revenue") %>%
  filter(Empresa %in% names(colores_empresas)) %>%
  mutate(Revenue = as.numeric(Revenue)) %>%
  filter(!is.na(Revenue), Year >= 2017, Year <= 2025) %>%
  group_by(Empresa) %>% arrange(Year) %>%
  mutate(Revenue_Index = 100 * Revenue / first(Revenue)) %>%
  ungroup()

graf_ingresos_log <- ggplot(revenue_df, aes(x = as.numeric(Year), y = Revenue_Index, color = Empresa)) +
  geom_line(size = 1.2) + geom_point(size = 2.5) +
  scale_y_log10(labels = comma_format()) +
  scale_color_manual(values = colores_empresas) +
  scale_x_continuous(breaks = 2017:2025, expand = expansion(mult = c(0.01, 0.15))) +
  labs(
    title    = "Evolución de Ingresos (escala log)",
    subtitle = "Índice base 100 desde 2017 (incluye TTM)",
    x        = "Año", y = "Índice (log)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title       = element_text(face = "bold", size = 15),
        plot.subtitle    = element_text(size = 11))

ggsave(
  filename = file.path(fin_dir, "graf_ingresos_log.png"),
  plot     = graf_ingresos_log,
  width    = 10, height = 6, dpi = 300,
  bg       = "transparent"
)

# 3.2 Waffle de Gross Margin Promedio ----------------------------------------
gross_margin_df <- IncomeStatementPortfolio %>%
  filter(Concepto == "Gross Profit") %>%
  pivot_longer(cols = -c(Concepto, Year), names_to = "Empresa", values_to = "GrossProfit") %>%
  mutate(GrossProfit = as.numeric(GrossProfit)) %>%
  filter(Empresa %in% names(colores_empresas)) %>%
  left_join(revenue_df %>% select(Year, Empresa, Revenue), by = c("Year","Empresa")) %>%
  mutate(GrossMargin = GrossProfit / Revenue) %>%
  filter(!is.na(GrossMargin), Year >= 2017, Year <= 2025)

gross_margin_promedios <- gross_margin_df %>%
  group_by(Empresa) %>% summarise(GrossMarginPromedio = mean(GrossMargin, na.rm=TRUE)) %>%
  mutate(GrossMarginPerc = round(GrossMarginPromedio * 100))

gross_margin_vector <- setNames(gross_margin_promedios$GrossMarginPerc,
                                gross_margin_promedios$Empresa)

graf_gross_waffle <- waffle(
  parts    = gross_margin_vector,
  rows     = 10,
  colors   = colores_empresas[names(gross_margin_vector)],
  title    = "Gross Margin Promedio (2017–2025)",
  legend_pos = "bottom"
)

ggsave(
  filename = file.path(fin_dir, "graf_gross_waffle.png"),
  plot     = graf_gross_waffle,
  width    = 8, height = 5, dpi = 300,
  bg       = "transparent"
)

# 3.3 Composición final Crecimiento y Eficiencia operativa ----------------------------------------------
crecimiento_y_eficiencia_operativa <- graf_ingresos_log + graf_gross_waffle +
  plot_layout(ncol=2, widths=c(2.6,2)) +
  plot_annotation(
    title = "Crecimiento y Eficiencia Operativa (2017–2025)",
    theme = theme(plot.title = element_text(size=17,face="bold",hjust=0.5))
  )
print(crecimiento_y_eficiencia_operativa)
ggsave(
  filename = file.path(fin_dir, "crecimiento_y_eficiencia_operativa.png"),
  plot     = slide2_final,
  width    = 14, height = 7.5, dpi = 320,
  bg       = "transparent"
)

# 3.4 Apalancamiento y Valoración Relativa ----------------------------------
# Traducción legible de símbolos
nombre_empresas <- c(
  "AAPL" = "Apple", "KO" = "Coca-Cola", "PFE" = "Pfizer",
  "TM"   = "Toyota", "MELI" = "Mercado Libre"
)

prep_var <- function(df, concepto, nuevo_nombre) {
  df %>%
    filter(Concepto == concepto) %>%
    mutate(Year = as.numeric(ifelse(Year=="TTM", NA, Year))) %>%
    filter(!is.na(Year), Year <= 2024) %>%
    pivot_longer(cols = all_of(names(nombre_empresas)), names_to="symbol", values_to=nuevo_nombre) %>%
    
    select(-Concepto)
}

ebitda     <- prep_var(IncomeStatementPortfolio, "EBITDA", "EBITDA")
mcap       <- prep_var(IncomeStatementPortfolio, "Market Capitalization", "MarketCap")
short_debt <- prep_var(BalanceSheetPortfolio, "Short Term Debt Incl. Current Port. of LT Debt", "ShortDebt")
long_debt  <- prep_var(BalanceSheetPortfolio, "Long Term Debt", "LongDebt")
cash       <- prep_var(BalanceSheetPortfolio, "Cash & Short Term Investments", "Cash")

ratios_df <- reduce(list(ebitda,mcap,short_debt,long_debt,cash), full_join, by=c("symbol","Year")) %>%
  mutate(
    TotalDebt     = ShortDebt + LongDebt,
    NetDebt       = TotalDebt - Cash,
    NetDebt_EBITDA= NetDebt/EBITDA,
    EV            = MarketCap + NetDebt,
    EV_EBITDA     = EV/EBITDA,
    nombre        = nombre_empresas[symbol]
  ) %>%
  filter(is.finite(NetDebt_EBITDA), is.finite(EV_EBITDA))

ratios_2024 <- ratios_df %>% filter(Year==2024)
facet_df <- ratios_2024 %>%
  select(nombre, NetDebt_EBITDA, EV_EBITDA) %>%
  pivot_longer(cols=-nombre, names_to="ratio", values_to="valor") %>%
  mutate(ratio = recode(ratio,
                        "NetDebt_EBITDA"="Net Debt / EBITDA",
                        "EV_EBITDA"     ="EV / EBITDA"),
         ratio = factor(ratio, levels=c("EV / EBITDA","Net Debt / EBITDA")))
# Ordeno
orden <- facet_df %>% filter(ratio=="EV / EBITDA") %>% arrange(desc(valor)) %>% pull(nombre)
facet_df$nombre <- factor(facet_df$nombre, levels=orden)
# Mediana
ev_median <- median(facet_df$valor[facet_df$ratio=="EV / EBITDA"], na.rm=TRUE)
# Paleta de colores
paleta_apalancamiento_y_valrelativa <- c(
  "Apple"         = "#264b96",
  "Coca-Cola"     = "#ff7f0e",
  "Mercado Libre" = "#66a61e",
  "Pfizer"        = "#f5a1c5",
  "Toyota"        = "#af83cf"
)

grafico_ratios <- ggplot(facet_df, aes(x=valor,y=nombre, fill=nombre)) +
  geom_col(width=0.6) + facet_wrap(~ratio, scales="free_x") +
  scale_fill_manual(values=paleta_apalancamiento_y_valrelativa) +
  geom_vline(data=data.frame(ratio="EV / EBITDA"), aes(xintercept=ev_median),
             linetype="dotted", color="gray40", size=0.8, inherit.aes=FALSE) +
  geom_vline(data=data.frame(ratio="Net Debt / EBITDA"), aes(xintercept=3),
             linetype="dashed", color="black", size=0.8, inherit.aes=FALSE) +
  labs(x=NULL,y=NULL) +
  theme_minimal(base_size=13) + theme(
    strip.text      = element_text(size=13, face="bold"),
    legend.position = "none",
    panel.grid      = element_line(color="gray85")
  )

titulo <- ggplot() + theme_void() +
  labs(title="Apalancamiento y Valoración Relativa",
       subtitle="Comparación Apple, Coca-Cola, Pfizer, Toyota y Mercado Libre (2024)") +
  theme(plot.title=element_text(size=18,face="bold"),
        plot.subtitle=element_text(size=13))

Apalancamiento_y_valoracion_relativa <- titulo / grafico_ratios + plot_layout(heights=c(0.12,1))
print(Apalancamiento_y_valoracion_relativa)

ggsave(
  filename = file.path(fin_dir, "Apalancamiento_y_valoracion_relativa.png"),
  plot     = Apalancamiento_y_valoracion_relativa,
  width    = 10, height = 6, dpi = 300,
  bg       = "transparent"
)

# 3.5 Heatmaps ROE, EPS Growth y Debt/Equity ------------------------------------
# Calculo el EPS Growth
eps_df <- IncomeStatementPortfolio %>%
  filter(Concepto == "EPS (Diluted)") %>%           
  pivot_longer(cols = -c(Concepto, Year), names_to = "symbol", values_to = "eps") %>%
  mutate(
    Year = ifelse(Year == "TTM", "2025", Year),     
    Year = as.integer(Year)
  ) %>%
  arrange(symbol, Year) %>%
  
# Ordenar por empresa y año
  group_by(symbol) %>%
  mutate(eps_growth = (eps - lag(eps)) / abs(lag(eps))) %>%
  
# Crecimiento interanual
  ungroup() %>%
  select(symbol, year = Year, eps_growth) %>%
  filter(!is.na(eps_growth))                       

# Calculo el ROE
roe_df <- BalanceSheetPortfolio %>%
  filter(Concepto == "Return on Equity") %>%        
# Extraigo el ROE
  pivot_longer(cols = -c(Concepto, Year), names_to = "symbol", values_to = "roe") %>%
  mutate(Year = as.integer(Year)) %>%               
# Convierto año a entero
  select(symbol, year = Year, roe) %>%
  filter(!is.na(roe))                               

# Calculo el Debt/Equity
debt_df <- BalanceSheetPortfolio %>%
  filter(Concepto %in% c("Total Liabilities", "Total Equity")) %>%                
# Deuda y patrimonio
  pivot_longer(cols = -c(Concepto, Year), names_to = "symbol", values_to = "valor") %>%
  mutate(Year = as.integer(ifelse(Year == "TTM", "2025", Year))) %>%             
  pivot_wider(names_from = Concepto, values_from = valor) %>%                        
  rename(debt = `Total Liabilities`, equity = `Total Equity`) %>%
  mutate(debt_equity = debt / equity) %>%                                            
  
# Calculo el ratio
  select(symbol, year = Year, debt_equity) %>%
  filter(!is.na(debt_equity), is.finite(debt_equity))                                 

# Filtro valores válidos

# Unifico ratios y normalizo valores para graficar
ratios_normalizados <- roe_df %>%
  full_join(eps_df, by = c("symbol","year")) %>%
  full_join(debt_df, by = c("symbol","year")) %>%
  pivot_longer(cols = c(roe, eps_growth, debt_equity), names_to = "ratio", values_to = "valor") %>%
  filter(year >= 2018, year <= 2024, symbol %in% names(colores_empresas)) %>%           
  mutate(
    ratio = recode(ratio,
                   roe = "ROE por año",
                   eps_growth = "EPS Growth por año",
                   debt_equity = "Debt/Equity por año"),
    symbol = recode(symbol,
                    AAPL = "Apple",
                    KO   = "Coca-Cola",
                    MELI = "Meli",
                    PFE  = "Pfizer",
                    TM   = "Toyota"),
    year = factor(year, levels = 2018:2024)
  ) %>%
  group_by(ratio, year) %>%
  mutate(
    valor_norm = (valor - min(valor, na.rm = TRUE)) / (max(valor, na.rm = TRUE) - min(valor, na.rm = TRUE)),
    label      = paste0(round(valor, 2), " (", round(valor_norm, 2), ")")
  ) %>%
  ungroup()

# Función para aplicar el heatmap
plot_ratio_normalizado <- function(data, ratio_label, palette) {
  df <- filter(data, ratio == ratio_label)
  ggplot(df, aes(x = year, y = fct_rev(symbol), fill = valor_norm)) +
    geom_tile(color = "white") +                                                  
    geom_text(aes(label = label), size = 3) +                                       
    scale_fill_gradient(low = palette[1], high = palette[3], limits = c(0, 1)) +
    labs(title = ratio_label, x = NULL, y = NULL, fill = "Normalizado") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title  = element_text(face = "bold", size = 13, hjust = 0.5),
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 10),
      panel.grid  = element_blank()                                                 
    )
}

# Genero los  heatmaps
gg_roe  <- plot_ratio_normalizado(ratios_normalizados, "ROE por año",        c("#d0f0c0", "#a3d9a5", "#006400"))
gg_eps  <- plot_ratio_normalizado(ratios_normalizados, "EPS Growth por año", c("#bdd7e7", "#f7fbff", "#08519c"))
gg_debt <- plot_ratio_normalizado(ratios_normalizados, "Debt/Equity por año",c("#fee0d2", "#fc9272", "#cb181d"))

# Combino y guardo los heatmaps
combined_heatmap <- (gg_roe / gg_eps / gg_debt) +
  plot_annotation(
    title = "Evolución de Ratios Financieros por Activo (2018–2024)",
    theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))
  )
print(combined_heatmap)
# Exporto a PNG con fondo transparente
ggsave(
  filename = file.path(fin_dir, "heatmaps_combinados.png"),
  plot     = combined_heatmap,
  width    = 10, height = 8,
  dpi      = 300,
  bg       = "transparent"
)

# 3.6 Gráficos Generales por Ratio  --------------------------------------
# Definición de colores por empresa
colores_empresas <- c(
  "Apple" = "#264b96",  
  "Coca-Cola"   = "#ff7f03",   
  "Meli" = "#66a61e",   
  "Pfizer"  = "#f5a1c5",   
  "Toyota"   = "#af83cf"    
)

# Función para graficar individual ----------------------------------
plot_individual <- function(df, ratio_label, symbol_name) {
  df %>%
    filter(ratio == ratio_label, symbol == symbol_name) %>%
    mutate(year = as.integer(as.character(year))) %>%
    ggplot(aes(x = year, y = valor)) +
    geom_line(color = colores_empresas[symbol_name], size = 1) +
    geom_point(color = colores_empresas[symbol_name], size = 2) +
    scale_x_continuous(breaks = 2018:2024) +
    labs(
      title = paste0(symbol_name, " — ", ratio_label),
      x = "Año", y = ratio_label
    ) +
    theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      axis.text  = element_text(size = 8),
      axis.title = element_text(size = 9)
    )
}

# Grafico ROE por año
grafico_roe <- ratios_tidy %>%
  filter(ratio == "ROE por año") %>%
  ggplot(aes(x = as.integer(as.character(year)), y = valor, color = symbol)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Evolución del ROE por Empresa (2018–2024)",
    x = "Año", y = "ROE",
    color = "Empresa"
  ) +
  scale_color_manual(name = "Empresa", values = colores_empresas) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title      = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "bottom"
  )
print(grafico_roe)

ggsave(
  filename = file.path(fin_dir, "evolucion_ROE.png"),
  plot     = grafico_roe,
  width    = 10, height = 8, dpi = 300,
  bg       = "transparent"
)

# Grafico EPS Growth por año
eps_plot_data <- ratios_tidy %>%
  filter(ratio == "EPS Growth por año")

grafico_eps <- ggplot(eps_plot_data, 
                      aes(x = as.integer(as.character(year)), 
                          y = valor, 
                          group = symbol, 
                          color = symbol)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(
    limits = c(-5, 5),
    oob = scales::squish
  ) +
  scale_x_continuous(breaks = 2018:2024) +
  labs(
    title = "Evolución del EPS Growth por Empresa (2018–2024)",
    x = "Año", y = "EPS Growth",
    color = "Empresa"
  ) +
  scale_color_manual(name = "Empresa", values = colores_empresas) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title   = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text    = element_text(size = 10),
    axis.title   = element_text(size = 11),
    legend.position = "bottom"
  )
print(grafico_eps)

ggsave(
  filename = file.path(fin_dir, "evolucion_EPS_Growth.png"),
  plot     = grafico_eps,
  width    = 10, height = 8, dpi = 300,
  bg       = "transparent"
)

# Grafico Debt/Equity por año
grafico_debt <- ratios_tidy %>%
  filter(ratio == "Debt/Equity por año") %>%
  ggplot(aes(x = as.integer(as.character(year)), y = valor, color = symbol)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Evolución del Debt/Equity por Empresa (2018–2024)",
    x = "Año", y = "Debt/Equity",
    color = "Empresa"
  ) +
  scale_color_manual(name = "Empresa", values = colores_empresas) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title      = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "bottom"
  )
print(grafico_debt)

ggsave(
  filename = file.path(fin_dir, "evolucion_Debt_Equity.png"),
  plot     = grafico_debt,
  width    = 10, height = 8, dpi = 300,
  bg       = "transparent"
)

# 3.7 Gráficos individuales --------------------------------------------

# Listas de ratios y empresas
ratios_list   <- c("ROE por año", "EPS Growth por año", "Debt/Equity por año")
empresas_list <- c("Apple", "Coca-Cola", "Pfizer", "Toyota", "Meli")

# Genero un bucle para los graficos
for (r in ratios_list) {
  plots_por_ratio <- lapply(empresas_list, function(s) {
    plot_individual(ratios_tidy, r, s)
  })
  print(
    wrap_plots(plots_por_ratio, nrow = 1) +
      plot_annotation(title = paste0("Gráficos individuales — ", r))
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
      width    = 10,
      height   = 6,
      dpi      = 300
    )
  }
}
