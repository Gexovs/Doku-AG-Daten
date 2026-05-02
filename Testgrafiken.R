# SE Sicherheitspolitik SoSe2026 #AG Daten

##########################################################
####### TESTGRAFIKEN ZUM DATENSATZCHECK! :-) #############
##########################################################

# ========================================================
# AKT 0
# Testgrafik: EUROPÄISCHE VERTEIDIGUNGSAUSGABEN
# Datensatz: SIPRI Military Expenditure Database 1949-2024
# ========================================================

library(readxl)
library(tidyverse)
library(janitor)
library(scales)

setwd("/Users/white_wolf/Documents/SE_Sicherheitspolitik/Test")

sipri_raw <- read_excel(
  "SIPRI-Milex-data-1949-2024_2.xlsx",
  sheet = "Share of GDP",
  skip = 5
)

sipri <- sipri_raw %>%
  clean_names()

sipri_long <- sipri %>%
  pivot_longer(
    cols = matches("^x?[0-9]{4}$"),
    names_to = "year",
    values_to = "share_gdp"
  ) %>%
  mutate(
    year = as.numeric(str_remove(year, "^x")),
    share_gdp = as.numeric(share_gdp)
  )

plot_data <- sipri_long %>%
  filter(
    country %in% c("Austria", "Germany", "France", "Poland"),
    year >= 2000,
    year <= 2024
  ) %>%
  mutate(
    country_de = factor(
      recode(
        country,
        "Austria" = "Österreich",
        "Germany" = "Deutschland",
        "France" = "Frankreich",
        "Poland" = "Polen"
      ),
      levels = c("Österreich", "Deutschland", "Frankreich", "Polen")
    ),
    line_width = if_else(country == "Austria", 1.5, 1.15),
    point_size = if_else(country == "Austria", 2.2, 1.8)
  )

print(
  plot_data %>%
    group_by(country_de) %>%
    summarise(
      first_year = min(year, na.rm = TRUE),
      last_year = max(year, na.rm = TRUE),
      max_share_gdp = max(share_gdp, na.rm = TRUE),
      .groups = "drop"
    )
)

farben <- c(
  "Österreich" = "#D71920",
  "Deutschland" = "#D6A300",
  "Frankreich" = "#1F5AA6",
  "Polen" = "#2E7D32"
)

p1 <- ggplot(plot_data, aes(x = year, y = share_gdp, color = country_de)) +
  geom_line(aes(linewidth = line_width), alpha = 0.95) +
  geom_point(aes(size = point_size), alpha = 0.95) +
  scale_linewidth_identity() +
  scale_size_identity() +
  geom_vline(
    xintercept = 2022,
    linetype = "dashed",
    linewidth = 0.75,
    color = "grey25"
  ) +
  annotate(
    "text",
    x = 2022.25,
    y = max(plot_data$share_gdp, na.rm = TRUE) * 0.92,
    label = "2022\nUkrainekrieg",
    hjust = 0,
    size = 3.5,
    fontface = "bold",
    color = "grey15"
  ) +
  scale_color_manual(values = farben, breaks = names(farben)) +
  scale_x_continuous(
    breaks = seq(2000, 2024, 4),
    expand = expansion(mult = c(0.01, 0.06))
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 0.1),
    expand = expansion(mult = c(0.06, 0.10))
  ) +
  labs(
    title = "Europäische Verteidigungsausgaben",
    subtitle = "Militärausgaben als Anteil am BIP, 2000–2024",
    x = NULL,
    y = "Anteil am BIP",
    color = NULL,
    caption = "Quelle: SIPRI Military Expenditure Database 1949–2024"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "#F7F7F3", color = NA),
    panel.background = element_rect(fill = "#F7F7F3", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "grey30", linewidth = 0.5),
    axis.line.y = element_line(color = "grey30", linewidth = 0.5),
    axis.text = element_text(color = "grey20"),
    axis.title.y = element_text(color = "grey15", margin = margin(r = 10)),
    plot.title = element_text(face = "bold", size = 18, color = "grey10"),
    plot.subtitle = element_text(size = 12.5, color = "grey20"),
    plot.caption = element_text(size = 9, color = "grey35", hjust = 1),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    plot.margin = margin(18, 24, 18, 24)
  )

p1



# =========================================================
# AKT 1 – DIE ZEITENWENDE
# Testgrafik: Verteidigungsausgaben 2020 vs. 2024
# Datensatz: EDA Defence Data 2024
# =========================================================

library(readxl)
library(tidyverse)
library(scales)

setwd("/Users/white_wolf/Documents/SE_Sicherheitspolitik/Test")

# 2020 aus Sheet "Billions"
eda_2020 <- read_excel("defence-data-2024.xlsx", sheet = "Billions") %>%
  filter(
    PMS %in% c("Austria", "Germany", "France", "Poland", "Finland"),
    Year == 2020
  ) %>%
  transmute(
    country = PMS,
    year = 2020,
    expenditure = as.numeric(`Total Defence Expenditure`)
  )

# 2024 aus Sheet "Member States 2024 "
eda_2024 <- read_excel("defence-data-2024.xlsx", sheet = "Member States 2024 ") %>%
  filter(
    `EU MS` %in% c("Austria", "Germany", "France", "Poland", "Finland")
  ) %>%
  transmute(
    country = `EU MS`,
    year = 2024,
    expenditure = as.numeric(`Total Defence Expenditure`)
  )

# Zusammenführen
plot_data <- bind_rows(eda_2020, eda_2024) %>%
  mutate(
    country_de = recode(
      country,
      "Austria" = "Österreich",
      "Germany" = "Deutschland",
      "France" = "Frankreich",
      "Poland" = "Polen",
      "Finland" = "Finnland"
    )
  )

# Kontrolle
print(plot_data)

# Reihenfolge nach 2024-Ausgaben
order_levels <- plot_data %>%
  filter(year == 2024) %>%
  arrange(expenditure) %>%
  pull(country_de)

plot_data <- plot_data %>%
  mutate(
    country_de = factor(country_de, levels = order_levels)
  )

# Grafik
p2 <- ggplot(
  plot_data,
  aes(
    x = expenditure,
    y = country_de,
    group = country_de
  )
) +
  geom_line(
    color = "grey65",
    linewidth = 2.2
  ) +
  geom_point(
    aes(fill = factor(year)),
    shape = 21,
    size = 5.5,
    color = "grey20",
    stroke = 0.4
  ) +
  scale_fill_manual(
    values = c(
      "2020" = "grey70",
      "2024" = "#D71920"
    ),
    labels = c("2020", "2024")
  ) +
  scale_x_continuous(
    labels = label_number(big.mark = ".", suffix = " Mio."),
    expand = expansion(mult = c(0.03, 0.08))
  ) +
  labs(
    title = "Aufrüstung seit der Zeitenwende",
    subtitle = "Verteidigungsausgaben ausgewählter Staaten, 2020 vs. 2024",
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = "Quelle: European Defence Agency, Defence Data 2024"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "#F7F7F3", color = NA),
    panel.background = element_rect(fill = "#F7F7F3", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(face = "bold", color = "grey15"),
    axis.text.x = element_text(color = "grey25"),
    plot.title = element_text(face = "bold", size = 19),
    plot.subtitle = element_text(size = 12.5),
    legend.position = "bottom",
    plot.caption = element_text(size = 9, color = "grey35"),
    plot.margin = margin(18, 24, 18, 24)
  )

p2

ggsave(
  "akt1_zeitenwende_slopechart_03.png",
  p2,
  width = 11,
  height = 7,
  dpi = 300,
  bg = "#F7F7F3"
)


# =========================================================
# VERSION 2:
# AKT 1 – DIE ZEITENWENDE
# Prozentuale Steigerung Verteidigungsausgaben 2020–2024
# Österreich hervorgehoben
# =========================================================

library(readxl)
library(tidyverse)
library(scales)

setwd("/Users/white_wolf/Documents/SE_Sicherheitspolitik/Test")

eda_2020 <- read_excel("defence-data-2024.xlsx", sheet = "Billions") %>%
  filter(
    PMS %in% c("Austria", "Germany", "France", "Poland", "Finland"),
    Year == 2020
  ) %>%
  transmute(
    country = PMS,
    exp_2020 = as.numeric(`Total Defence Expenditure`)
  )

eda_2024 <- read_excel("defence-data-2024.xlsx", sheet = "Member States 2024 ") %>%
  filter(
    `EU MS` %in% c("Austria", "Germany", "France", "Poland", "Finland")
  ) %>%
  transmute(
    country = `EU MS`,
    exp_2024 = as.numeric(`Total Defence Expenditure`)
  )

plot_data <- left_join(eda_2020, eda_2024, by = "country") %>%
  mutate(
    increase_pct = ((exp_2024 - exp_2020) / exp_2020) * 100,
    country_de = recode(
      country,
      "Austria" = "Österreich",
      "Germany" = "Deutschland",
      "France" = "Frankreich",
      "Poland" = "Polen",
      "Finland" = "Finnland"
    ),
    highlight = if_else(country == "Austria", "Österreich", "Andere")
  ) %>%
  arrange(increase_pct)

p3 <- ggplot(
  plot_data,
  aes(
    x = increase_pct,
    y = reorder(country_de, increase_pct),
    fill = highlight
  )
) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = paste0(round(increase_pct), "%")),
    hjust = -0.15,
    size = 4.8,
    fontface = "bold",
    color = "grey10"
  ) +
  scale_fill_manual(
    values = c(
      "Österreich" = "#D71920",
      "Andere" = "#17324D"
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.16)),
    labels = function(x) paste0(round(x), "%")
  ) +
  labs(
    title = "Aufrüstung seit der Zeitenwende",
    subtitle = "Steigerung der Verteidigungsausgaben 2020–2024 (nominal, nicht inflationsbereinigt)",
    x = NULL,
    y = NULL,
    caption = "Quelle: European Defence Agency, Defence Data 2024"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "#F7F7F3", color = NA),
    panel.background = element_rect(fill = "#F7F7F3", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(face = "bold", size = 13, color = "grey15"),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold", size = 20, color = "grey10"),
    plot.subtitle = element_text(size = 12.2, color = "grey25"),
    plot.caption = element_text(size = 9, color = "grey35"),
    plot.margin = margin(18, 30, 18, 18)
  )

p3

ggsave(
  "akt1_zeitenwende_pctincrease_02_at_highlight.png",
  p3,
  width = 11,
  height = 7,
  dpi = 300,
  bg = "#F7F7F3"
)




# =========================================================
# AKT 2 – GENERATION ZEITENWENDE
# EU-27 vs. Österreich: Prioritäten junger Menschen
# Datensatz: Flash Eurobarometer 574 / GESIS ZA8928
# =========================================================

library(haven)
library(tidyverse)
library(scales)

setwd("/Users/white_wolf/Documents/SE_Sicherheitspolitik/Test")

za8928 <- read_dta("ZA8928_v1-0-0.dta")

# Nur relevante Q7-Prioritäten
priority_vars <- c(
  "q7_1", "q7_2", "q7_3", "q7_4", "q7_5", "q7_6",
  "q7_7", "q7_8", "q7_9", "q7_10", "q7_11"
)

priority_labels <- c(
  q7_1  = "Irreguläre Migration",
  q7_2  = "Sicherheit & Verteidigung",
  q7_3  = "Wirtschaft & Finanzen",
  q7_4  = "Klima & Umwelt",
  q7_5  = "Bildung & Ausbildung",
  q7_6  = "Jobs & soziale Gleichheit",
  q7_7  = "Energie",
  q7_8  = "Forschung & Innovation",
  q7_9  = "Digitale Transformation",
  q7_10 = "Gesundheit",
  q7_11 = "Demokratie & Rechtsstaat"
)

# EU-27 gesamt und Österreich separat berechnen
plot_data <- za8928 %>%
  filter(d1 >= 16, d1 <= 30) %>%
  mutate(
    gruppe = if_else(isocntry == "AT", "Österreich", "EU-27 gesamt")
  ) %>%
  filter(gruppe %in% c("EU-27 gesamt", "Österreich")) %>%
  select(gruppe, all_of(priority_vars)) %>%
  pivot_longer(
    cols = all_of(priority_vars),
    names_to = "variable",
    values_to = "mentioned"
  ) %>%
  mutate(
    priority = recode(variable, !!!priority_labels),
    mentioned = as.numeric(mentioned)
  ) %>%
  group_by(gruppe, priority) %>%
  summarise(
    pct = mean(mentioned == 1, na.rm = TRUE) * 100,
    n = n(),
    .groups = "drop"
  )

# Reihenfolge nach EU-Gesamtwert
order_levels <- plot_data %>%
  filter(gruppe == "EU-27 gesamt") %>%
  arrange(pct) %>%
  pull(priority)

plot_data <- plot_data %>%
  mutate(
    priority = factor(priority, levels = order_levels),
    highlight = if_else(priority == "Sicherheit & Verteidigung", "Sicherheit", "Andere")
  )

# N für Caption
n_eu <- za8928 %>% filter(d1 >= 16, d1 <= 30) %>% nrow()
n_at <- za8928 %>% filter(d1 >= 16, d1 <= 30, isocntry == "AT") %>% nrow()

# Grafik
p5 <- ggplot(
  plot_data,
  aes(
    x = pct,
    y = priority,
    fill = gruppe
  )
) +
  geom_col(
    position = position_dodge(width = 0.72),
    width = 0.62
  ) +
  geom_text(
    aes(label = paste0(round(pct), "%")),
    position = position_dodge(width = 0.72),
    hjust = -0.15,
    size = 3.8,
    fontface = "bold",
    color = "grey10"
  ) +
  scale_fill_manual(
    values = c(
      "EU-27 gesamt" = "#17324D",
      "Österreich" = "#D71920"
    )
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.20)),
    labels = function(x) paste0(round(x), "%")
  ) +
  labs(
    title = "Generation Zeitenwende",
    subtitle = "Prioritäten junger Menschen für die EU: EU-27 vs. Österreich",
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = paste0(
      "Befragte: 16–30 Jahre; EU-27 n = ", n_eu,
      ", Österreich n = ", n_at,
      " | Quelle: Flash Eurobarometer 574 / GESIS ZA8928"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "#F7F7F3", color = NA),
    panel.background = element_rect(fill = "#F7F7F3", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(face = "bold", size = 11.5, color = "grey15"),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "top",
    legend.justification = "left",
    plot.title = element_text(face = "bold", size = 20, color = "grey10"),
    plot.subtitle = element_text(size = 12.5, color = "grey25"),
    plot.caption = element_text(size = 8.5, color = "grey35"),
    plot.margin = margin(18, 40, 18, 18)
  )

p5

ggsave(
  "akt2_generation_zeitenwende_eu_at_vergleich_01.png",
  p5,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "#F7F7F3"
)



# =========================================================
# AKT 3 – DIE FRAGE DER VERANTWORTUNG
# Testgrafik: Verteidigungsbereitschaft im Ernstfall
# Datensatz: AFP Austria Panel 2023–2025
# Quelle: doi:10.11587/UJJWTG
# =========================================================

library(haven)
library(tidyverse)
library(scales)

setwd("/Users/white_wolf/Documents/SE_Sicherheitspolitik/Test")

afp3 <- read_dta("10827_da_de_v3_2.dta")

# 1. Daten vorbereiten
raw_counts <- afp3 %>%
  mutate(
    gruppe = case_when(
      age >= 18 & age <= 29 ~ "18–29 Jahre",
      TRUE ~ "Gesamtbevölkerung"
    ),
    antwort = case_when(
      armed_resistance == "Nein, auf keinen Fall" ~ "Nein\nauf keinen Fall",
      armed_resistance == "Nein, eher nicht" ~ "Eher\nnicht",
      armed_resistance == "Weder ja noch nein" ~ "Unentschieden",
      armed_resistance == "Ja, wahrscheinlich schon" ~ "Wahrscheinlich\nja",
      armed_resistance == "Ja, auf jeden Fall" ~ "Ja\nauf jeden Fall",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(antwort)) %>%
  mutate(
    gruppe = factor(gruppe, levels = c("18–29 Jahre", "Gesamtbevölkerung")),
    antwort = factor(
      antwort,
      levels = c(
        "Nein\nauf keinen Fall",
        "Eher\nnicht",
        "Unentschieden",
        "Wahrscheinlich\nja",
        "Ja\nauf jeden Fall"
      )
    )
  ) %>%
  count(gruppe, antwort) %>%
  group_by(gruppe) %>%
  mutate(
    pct   = n / sum(n),
    label = paste0(round(pct * 100), "%"),
    xmax  = cumsum(pct),
    xmin  = xmax - pct,
    y     = c(2, 1)[match(gruppe, c("18–29 Jahre", "Gesamtbevölkerung"))]
  ) %>%
  ungroup()

# 2. Caption / Samplesizes
n_text <- raw_counts %>%
  group_by(gruppe) %>%
  summarise(n_valid = sum(n), .groups = "drop")

caption_text <- paste0(
  "Gültige Antworten: Gesamt n = ",
  n_text$n_valid[n_text$gruppe == "Gesamtbevölkerung"],
  " | 18–29 Jahre n = ",
  n_text$n_valid[n_text$gruppe == "18–29 Jahre"],
  " | Quelle: AFP Austria Panel 2023–2025 (doi:10.11587/UJJWTG)"
)

# 3. Farbpalette
farben <- c(
  "Nein\nauf keinen Fall" = "#B00020",
  "Eher\nnicht"           = "#F26A21",
  "Unentschieden"         = "#4F7EA8",
  "Wahrscheinlich\nja"    = "#7CB342",
  "Ja\nauf jeden Fall"    = "#137333"
)

# 4. Grafik erstellen
p7 <- ggplot(raw_counts) +
  geom_rect(
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = y - 0.28,
      ymax = y + 0.28,
      fill = antwort
    ),
    color = "#F7F7F3",
    linewidth = 1.4
  ) +
  geom_text(
    aes(
      x = xmin + pct / 2,
      y = y,
      label = label
    ),
    size = 5.3,
    fontface = "bold",
    color = "white"
  ) +
  scale_fill_manual(values = farben) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    labels = percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = c(1, 2),
    labels = c("Gesamtbevölkerung", "18–29 Jahre"),
    limits = c(0.45, 2.65),
    expand = c(0, 0)
  ) +
  annotate(
    "text",
    x = 0.16,
    y = 2.52,
    label = "Ablehnung",
    color = "#B00020",
    fontface = "bold",
    size = 5.2
  ) +
  annotate(
    "text",
    x = 0.50,
    y = 2.52,
    label = "Unentschieden",
    color = "#4F7EA8",
    fontface = "bold",
    size = 5.2
  ) +
  annotate(
    "text",
    x = 0.82,
    y = 2.52,
    label = "Zustimmung",
    color = "#137333",
    fontface = "bold",
    size = 5.2
  ) +
  labs(
    title = "Würden Österreicher ihr Land verteidigen?",
    subtitle = NULL,
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = caption_text
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.background    = element_rect(fill = "#F7F7F3", color = NA),
    panel.background   = element_rect(fill = "#F7F7F3", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.y        = element_text(face = "bold", size = 14, color = "grey15"),
    axis.text.x        = element_text(size = 11, color = "grey35"),
    axis.ticks         = element_blank(),
    legend.position    = "bottom",
    legend.text        = element_text(size = 11),
    plot.title         = element_text(
      face = "bold",
      size = 24,
      color = "grey10",
      margin = margin(b = 18)
    ),
    plot.caption       = element_text(size = 9, color = "grey35", hjust = 1),
    plot.margin        = margin(24, 34, 18, 24)
  )

# 5. Anzeigen
p7

# 6. Export
ggsave(
  "akt3_verantwortung_verteidigungsbereitschaft_final.png",
  p7,
  width = 13,
  height = 7.5,
  dpi = 300,
  bg = "#F7F7F3"
)



# =================================================================
# AKT 3 – DIE FRAGE DER VERANTWORTUNG
# Blütengrafik: Zukunftssorgen der 16- bis 30-Jährigen in der EU-27
# Datensatz: Flash Eurobarometer 556 / GESIS ZA8928
# Quelle: doi:10.4232/1.14507
#==================================================================
library(haven)
library(tidyverse)
library(scales)

setwd("/Users/white_wolf/Documents/SE_Sicherheitspolitik/Test")

eb556 <- read_dta("ZA8928_v1-0-0.dta")

q8_map <- tibble(
  item = c("q8_1", "q8_2", "q8_3", "q8_4", "q8_5",
           "q8_6", "q8_7", "q8_8", "q8_9", "q8_10"),
  sorge = c(
    "Jobs",
    "Klima",
    "Wirtschaft",
    "Lebenshaltung",
    "Wohnen",
    "Ungleichheit",
    "Mentale\nGesundheit",
    "EU-\nSicherheit",
    "Frieden",
    "Bildung"
  ),
  highlight = c(
    "Andere", "Andere", "Andere", "Andere", "Andere",
    "Andere", "Andere", "Sicherheit", "Sicherheit", "Andere"
  )
)

plot_data <- eb556 %>%
  filter(d1 >= 16, d1 <= 30) %>%
  mutate(across(all_of(q8_map$item), haven::zap_labels)) %>%
  select(all_of(q8_map$item), w1) %>%
  pivot_longer(
    cols = all_of(q8_map$item),
    names_to = "item",
    values_to = "mentioned"
  ) %>%
  left_join(q8_map, by = "item") %>%
  mutate(
    mentioned = as.numeric(mentioned),
    w1 = as.numeric(w1)
  ) %>%
  group_by(item, sorge, highlight) %>%
  summarise(
    pct = weighted.mean(mentioned == 1, w1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(pct)) %>%
  mutate(
    sorge = factor(sorge, levels = sorge),
    label = paste0(round(pct * 100), "%")
  )

n_valid <- eb556 %>%
  filter(d1 >= 16, d1 <= 30) %>%
  nrow()

farben <- c(
  "Andere" = "#17324D",
  "Sicherheit" = "#D71920"
)

p9 <- ggplot(plot_data, aes(x = sorge, y = pct, fill = highlight)) +
  geom_col(
    width = 0.92,
    color = "#F7F7F3",
    linewidth = 1.05
  ) +
  geom_text(
    aes(y = pct + 0.080, label = label),
    size = 5.2,
    fontface = "bold",
    color = "grey10"
  ) +
  coord_polar(start = -0.28, clip = "off") +
  scale_fill_manual(values = farben) +
  scale_y_continuous(
    limits = c(0, max(plot_data$pct) + 0.115),
    expand = c(0, 0)
  ) +
  labs(
    title = "Zukunftssorgen der 16- bis 30-Jährigen in der EU-27",
    caption = paste0(
      "EU-27 | n = ",
      n_valid,
      " | Flash Eurobarometer 556 / GESIS ZA8928 (doi:10.4232/1.14507)"
    )
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.background = element_rect(fill = "#F7F7F3", color = NA),
    panel.background = element_rect(fill = "#F7F7F3", color = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    
    axis.text.x = element_text(
      face = "bold",
      size = 12.1,
      color = "grey15",
      margin = margin(t = -190)
    ),
    
    axis.ticks = element_blank(),
    legend.position = "none",
    
    plot.title = element_text(
      face = "bold",
      size = 14.2,
      color = "grey10",
      hjust = 0.5,
      margin = margin(b = 0)
    ),
    
    plot.caption = element_text(
      size = 9,
      color = "grey35",
      hjust = 0.5
    ),
    
    plot.margin = margin(8, 24, 14, 24)
  )

p9

ggsave(
  "akt3_eurobarometer_zukunftssorgen_FINAL_MAXIMUM_DICHT.png",
  p9,
  width = 11,
  height = 11,
  dpi = 300,
  bg = "#F7F7F3"
)




# =========================================================
# AKT 1 - KRIEGSDIENSVERWEIGERUNG IN DEUTSCHLAND
# Zusatzgrafik:
# Anzahl der Anträge auf KDV in Deutschland steigt rasant!
# =========================================================

# Pakete ---------------------------------------------------

library(tidyverse)
library(scales)
library(stringr)
library(grid)


# Daten ----------------------------------------------------

kdv <- tribble(
  ~jahr, ~antraege,
  2020, 137,
  2021, 201,
  2022, 951,
  2023, 1609,
  2024, 2998,
  2025, 3867
)


# Aufbereitung ---------------------------------------------

kdv <- kdv %>%
  mutate(
    label = label_number(big.mark = ".", decimal.mark = ",")(antraege),
    fill_col = case_when(
      jahr == 2020 ~ "#C8C8C8",  # grau
      jahr == 2021 ~ "#B5B5B5",  # grau
      jahr == 2022 ~ "#D9A441",  # ocker
      jahr == 2023 ~ "#E97F25",  # orange
      jahr == 2024 ~ "#C83E4D",  # rot
      jahr == 2025 ~ "#8F0D22"   # dunkelrot
    )
  )


# Prozentanstieg 2021 -> 2025 ------------------------------

anstieg_2021_2025 <- round(
  (kdv$antraege[kdv$jahr == 2025] / kdv$antraege[kdv$jahr == 2021] - 1) * 100,
  0
)

anstieg_label <- label_number(
  big.mark = ".",
  decimal.mark = ","
)(anstieg_2021_2025)


# Quellenblock ---------------------------------------------
# Ziel:
# - Quellen kompakt halten
# - aber trotzdem sauber mit Bulletpoints
# - Folgezeilen eingerückt
# - stärker über die Breite ziehen

make_bullet <- function(text, width = 255) {
  wrapped <- str_wrap(text, width = width)
  lines <- strsplit(wrapped, "\n")[[1]]
  
  if (length(lines) == 1) {
    return(paste0("• ", lines))
  } else {
    return(
      paste0(
        "• ", lines[1],
        paste0("\n   ", lines[-1], collapse = "")
      )
    )
  }
}

zugriff <- "02.05.2026"

quelle_1 <- make_bullet(
  paste0(
    "Deutscher Bundestag, Drucksache 20/7858 (neu), „Kriegsdienstverweigerung in Deutschland“, ",
    "21.07.2023; korrigierte Fassung vom 06.02.2024. ",
    "URL: https://dserver.bundestag.de/btd/20/078/2007858.pdf ",
    "(Zugriff am ", zugriff, ")."
  )
)

quelle_2 <- make_bullet(
  paste0(
    "Deutscher Bundestag, Drucksache 21/898, „Kriegsdienstverweigerung in Deutschland in den Jahren 2024 und 2025“, ",
    "14.07.2025. URL: https://dserver.bundestag.de/btd/21/008/2100898.pdf ",
    "(Zugriff am ", zugriff, ")."
  )
)

quelle_3 <- make_bullet(
  paste0(
    "Der Tagesspiegel, „Musterung ist zurück: Zahl der Kriegsdienstverweigerer steigt“, ",
    "Stand: 27.04.2026, 05:39 Uhr. ",
    "URL: https://www.tagesspiegel.de/politik/musterung-ist-zuruck-zahl-der-kriegsdienstverweigerer-steigt-15525699.html ",
    "(Zugriff am ", zugriff, ")."
  )
)

hinweis <- str_wrap(
  paste(
    "Hinweis: Eigene Aufbereitung; kein amtlicher Rohdatensatz.",
    "Der Wert für 2025 wurde ergänzend aus dem genannten Medienbericht",
    "auf Grundlage einer BAFzA-Angabe übernommen.",
    "BAFzA- und Bundeswehr-Zahlen können wegen zeitverzögerter Weiterleitung voneinander abweichen."
  ),
  width = 255
)

quellen_text <- paste(
  "Datengrundlage:",
  quelle_1,
  quelle_2,
  quelle_3,
  hinweis,
  sep = "\n"
)


# Grafik ---------------------------------------------------

p <- ggplot(kdv, aes(x = jahr, y = antraege)) +
  
  geom_col(
    aes(fill = fill_col),
    width = 0.68,
    show.legend = FALSE
  ) +
  
  scale_fill_identity() +
  
  geom_text(
    aes(label = label),
    vjust = -0.30,
    size = 5.1,
    fontface = "bold",
    color = "black"
  ) +
  
  annotate(
    "label",
    x = 2024.25,
    y = 820,
    label = paste0("+", anstieg_label, "%\n2021–2025"),
    fontface = "bold",
    size = 4.2,
    label.size = 0.25,
    label.r = unit(0.12, "lines"),
    fill = "#F7F7F3",
    color = "#8F0D22"
  ) +
  
  scale_x_continuous(
    breaks = 2020:2025,
    expand = expansion(mult = c(0.04, 0.04))
  ) +
  
  scale_y_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ","),
    limits = c(0, 4100),
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  labs(
    title = "Kriegsdienstverweigerung in Deutschland nimmt deutlich zu",
    subtitle = "Anträge auf Kriegsdienstverweigerung, 2020–2025",
    x = NULL,
    y = "Anzahl der KDV-Anträge",
    caption = quellen_text
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "#F7F7F3", color = NA),
    panel.background = element_rect(fill = "#F7F7F3", color = NA),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey82", linewidth = 0.35),
    
    plot.title = element_text(
      face = "bold",
      size = 21,
      color = "grey10",
      margin = margin(t = 18, b = 8)
    ),
    
    plot.subtitle = element_text(
      size = 15.5,
      color = "grey25",
      margin = margin(b = 14)
    ),
    
    plot.caption = element_text(
      size = 6.5,
      color = "grey35",
      hjust = 0,
      lineheight = 0.94,
      margin = margin(t = 12)
    ),
    
    # wichtig: Caption an Panelbreite ausrichten,
    # damit sie bis fast ans Ende der Grafik läuft
    plot.caption.position = "panel",
    
    axis.title.y = element_text(
      size = 12.5,
      color = "grey25",
      margin = margin(r = 8)
    ),
    
    axis.text = element_text(color = "grey20"),
    axis.text.x = element_text(size = 12.5),
    axis.text.y = element_text(size = 11.5),
    
    # unten kompakter als bisher
    plot.margin = margin(t = 8, r = 16, b = 100, l = 18)
  )


# anzeigen --------------------------------------------------

p


# speichern -------------------------------------------------
# minimal breiter exportieren hilft zusätzlich beim Quellenblock

ggsave(
  filename = "kdv_deutschland_2020_2025_final_breite_caption.png",
  plot = p,
  width = 12.6,
  height = 7.9,
  dpi = 300,
  bg = "#F7F7F3"
)
















