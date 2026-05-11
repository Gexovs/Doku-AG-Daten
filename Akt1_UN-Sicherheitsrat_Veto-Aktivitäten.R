# =========================================================
# BLOCKIERTE WELTORDNUNG
# Negative Stimmen ständiger Mitglieder im UN-Sicherheitsrat
# 1946–2026
# =========================================================

# =========================================================
# PAKETE
# =========================================================
required_packages <- c(
  "ggplot2",
  "dplyr",
  "tidyr",
  "readxl",
  "janitor",
  "stringr",
  "patchwork",
  "av"
)

optional_packages <- c("ragg", "systemfonts")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

for (pkg in optional_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    try(install.packages(pkg), silent = TRUE)
  }
}

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(janitor)
  library(stringr)
  library(patchwork)
  library(av)
})


# =========================================================
# PROJEKTPFAD
# =========================================================

project_dir <- "/Users/white_wolf/Documents/SE_Sicherheitspolitik/Test"
setwd(project_dir)

input_file <- file.path(project_dir, "un_security_council_veto_dataset_clean.xlsx")
sheet_name <- "Veto_Votes_Expanded"

output_dir <- file.path(project_dir, "out_veto_animation_final")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_file)) {
  stop("Eingabedatei nicht gefunden: ", input_file)
}


# =========================================================
# SCHRIFT
# =========================================================

detect_font_family <- function() {
  candidates <- c("Arial", "Helvetica", "Liberation Sans", "DejaVu Sans")
  
  if (requireNamespace("systemfonts", quietly = TRUE)) {
    available_fonts <- unique(systemfonts::system_fonts()$family)
    for (cand in candidates) {
      if (cand %in% available_fonts) return(cand)
    }
  }
  
  "sans"
}

font_family <- detect_font_family()


# =========================================================
# HILFSFUNKTIONEN
# =========================================================

canonicalize_member <- function(x) {
  x <- str_squish(as.character(x))
  
  case_when(
    str_detect(x, regex("union of soviet socialist republics|soviet union|^ussr$|^u\\.?s\\.?s\\.?r\\.?$", ignore_case = TRUE)) ~ "UdSSR",
    str_detect(x, regex("^united states$|^usa$|^u\\.?s\\.?a\\.?$|^u\\.?s\\.?$|^us$", ignore_case = TRUE)) ~ "USA",
    str_detect(x, regex("^russian federation$|^russia$|russian federation", ignore_case = TRUE)) ~ "Russland",
    str_detect(x, regex("china", ignore_case = TRUE)) ~ "China",
    str_detect(x, regex("france", ignore_case = TRUE)) ~ "Frankreich",
    str_detect(x, regex("united kingdom|^uk$|britain|great britain|u\\.?k\\.?", ignore_case = TRUE)) ~ "UK",
    TRUE ~ NA_character_
  )
}

make_bullet <- function(text, width = 150) {
  wrapped <- str_wrap(text, width = width)
  lines <- strsplit(wrapped, "\n")[[1]]
  
  if (length(lines) == 1) {
    return(paste0("• ", lines))
  } else {
    return(
      paste0(
        "• ", lines[1],
        paste0("\n  ", lines[-1], collapse = "")
      )
    )
  }
}

open_file_after_render <- function(output_file) {
  output_file <- normalizePath(output_file, mustWork = FALSE)
  
  if (!file.exists(output_file)) {
    message("Datei konnte nicht automatisch geöffnet werden, weil sie nicht gefunden wurde: ", output_file)
    return(invisible(FALSE))
  }
  
  message("Öffne Video automatisch: ", output_file)
  
  if (Sys.info()[["sysname"]] == "Darwin") {
    system2("open", shQuote(output_file))
  } else if (.Platform$OS.type == "windows") {
    shell.exec(output_file)
  } else {
    system2("xdg-open", shQuote(output_file), wait = FALSE)
  }
  
  invisible(TRUE)
}


# =========================================================
# DATEN LADEN
# =========================================================

veto_expanded_raw <- read_excel(
  path = input_file,
  sheet = sheet_name
) %>%
  clean_names()

if (!all(c("year", "veto_member") %in% names(veto_expanded_raw))) {
  stop("Im Sheet '", sheet_name, "' fehlen nach clean_names() die Spalten 'year' und/oder 'veto_member'.")
}


# =========================================================
# DATEN AUFBEREITEN
# =========================================================

member_master <- tibble(
  member = c("UdSSR", "USA", "Russland", "China", "Frankreich", "UK"),
  short_name = c("UdSSR", "USA", "Russland", "China", "Frankreich", "UK"),
  order_id = 1:6
)

veto_prepped <- veto_expanded_raw %>%
  transmute(
    year = suppressWarnings(as.integer(year)),
    veto_member_raw = str_squish(as.character(veto_member)),
    member = canonicalize_member(veto_member_raw)
  )

unknown_members <- veto_prepped %>%
  filter(!is.na(veto_member_raw), is.na(member)) %>%
  distinct(veto_member_raw)

if (nrow(unknown_members) > 0) {
  print(unknown_members)
  stop("Nicht zuordenbare veto_member-Werte gefunden. Bitte Mapping prüfen.")
}

veto_member_year <- veto_prepped %>%
  filter(!is.na(year), year >= 1946, year <= 2026, !is.na(member)) %>%
  count(year, member, name = "vetos") %>%
  complete(
    year = 1946:2026,
    member = member_master$member,
    fill = list(vetos = 0L)
  ) %>%
  left_join(member_master, by = "member") %>%
  arrange(year, order_id)

veto_totals <- veto_member_year %>%
  group_by(year) %>%
  summarise(
    total = sum(vetos),
    .groups = "drop"
  )

veto_cumulative <- veto_member_year %>%
  group_by(member) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(cum_vetos = cumsum(vetos)) %>%
  ungroup()

diagnostic_cumulative <- veto_cumulative %>%
  group_by(member) %>%
  summarise(
    total_cum = max(cum_vetos),
    .groups = "drop"
  ) %>%
  arrange(desc(total_cum))

print(diagnostic_cumulative)


# =========================================================
# DESIGN
# =========================================================

COL <- list(
  bg          = "#000000",
  panel       = "#12233A",
  card        = "#1E3557",
  card_border = "#486791",
  bar         = "#8DB5F0",
  current     = "#F0A65D",
  grid        = "#3A5D8F",
  text        = "#F5F7FB",
  subtext     = "#D0D6E0",
  accent      = "#FFC69E",
  event_1991  = "#EDEDED",
  event_2014  = "#F2A23A",
  event_2022  = "#FF6E63"
)

event_tbl <- tibble::tribble(
  ~year, ~label, ~col, ~label_x, ~label_y,
  1991L, "1991 · ENDE KALTER KRIEG", COL$event_1991, 1987.9, 15.45,
  2014L, "2014 · KRIM",              COL$event_2014, 2012.7, 15.45,
  2022L, "2022 · UKRAINE",           COL$event_2022, 2021.0, 15.45
)

caption_text <- paste(
  "Datengrundlage:",
  make_bullet(
    "United Nations Dag Hammarskjöld Library, Security Council – Veto List. Daten 1946–2004 laut offiziellem Veto-Verzeichnis in A/58/47, Annex III; laufend fortgeführt durch die Dag Hammarskjöld Library. URL: https://www.un.org/depts/dhl/resguide/scact_veto_table_en.htm (Abgleich am 08.05.2026).",
    width = 152
  ),
  make_bullet(
    "United Nations Digital Library, United Nations Security Council voting data. Datensatzdatei 2026_02_06_sc_voting.csv, Version 6 vom 06.02.2026. URL: https://digitallibrary.un.org/record/4055387 (Abgleich am 08.05.2026).",
    width = 152
  ),
  make_bullet(
    "Eigene Aufbereitung. Gemeinsame Vetos mehrerer ständiger Mitglieder werden je Veto-Macht getrennt gezählt; 2026 ist im Datenstand nur teilweise erfasst.",
    width = 152
  ),
  sep = "\n"
)


# =========================================================
# PANEL 1: TITEL
# =========================================================

build_title_panel <- function(current_year) {
  ggplot() +
    annotate(
      "text",
      x = 0.5,
      y = 0.60,
      label = "BLOCKIERTE WELTORDNUNG",
      family = font_family,
      fontface = "bold",
      color = COL$text,
      size = 16.2,
      hjust = 0.5
    ) +
    annotate(
      "text",
      x = 0.5,
      y = 0.10,
      label = "Negative Stimmen ständiger Mitglieder im UN-Sicherheitsrat, 1946–2026",
      family = font_family,
      color = COL$subtext,
      size = 6.0,
      hjust = 0.5
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
    theme_void() +
    theme(
      plot.background = element_rect(fill = COL$bg, color = NA),
      plot.margin = margin(14, 20, 0, 20)
    )
}


# =========================================================
# PANEL 2: SCOREBOARD OBEN
# =========================================================

build_scoreboard_panel <- function(current_year) {
  cum_now <- veto_cumulative %>%
    filter(year == current_year) %>%
    arrange(order_id) %>%
    mutate(
      x = c(1, 2, 3, 4, 5, 6),
      xmin = x - 0.44,
      xmax = x + 0.44,
      ymin = 0.28,
      ymax = 0.90,
      box_mid_y = (ymin + ymax) / 2,
      
      # final feinjustiert:
      # Ländername mittig, Zahl leicht höher als zuvor
      name_y  = box_mid_y + 0.10,
      value_y = box_mid_y - 0.16
    )
  
  year_now <- veto_member_year %>%
    filter(year == current_year, vetos > 0) %>%
    arrange(desc(vetos), order_id)
  
  total_now <- veto_totals %>%
    filter(year == current_year) %>%
    pull(total)
  
  summary_text <- if (nrow(year_now) == 0) {
    paste0("JAHR ", current_year, "   |   Vetos im Jahr: ", total_now, "   |   Kein Veto")
  } else {
    paste0(
      "JAHR ", current_year,
      "   |   Vetos im Jahr: ", total_now,
      "   |   ",
      paste0(year_now$short_name, " ", year_now$vetos, collapse = "   |   ")
    )
  }
  
  ggplot() +
    geom_rect(
      aes(xmin = 0.20, xmax = 6.80, ymin = 0.02, ymax = 0.98),
      fill = COL$panel,
      color = COL$card_border,
      linewidth = 0.55
    ) +
    geom_rect(
      data = cum_now,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = COL$card,
      color = COL$card_border,
      linewidth = 0.35
    ) +
    geom_text(
      data = cum_now,
      aes(x = x, y = name_y, label = short_name),
      family = font_family,
      fontface = "bold",
      color = COL$text,
      size = 5.2,
      hjust = 0.5,
      vjust = 0.5
    ) +
    geom_text(
      data = cum_now,
      aes(x = x, y = value_y, label = cum_vetos),
      family = font_family,
      fontface = "bold",
      color = COL$accent,
      size = 6.4,
      hjust = 0.5,
      vjust = 0.5
    ) +
    annotate(
      "text",
      x = 3.5,
      y = 0.16,
      label = summary_text,
      family = font_family,
      fontface = "bold",
      color = COL$text,
      size = 4.95,
      hjust = 0.5,
      vjust = 0.5
    ) +
    coord_cartesian(xlim = c(0, 7), ylim = c(0, 1), clip = "off") +
    theme_void() +
    theme(
      plot.background = element_rect(fill = COL$bg, color = NA),
      plot.margin = margin(0, 22, 6, 22)
    )
}


# =========================================================
# PANEL 3: HAUPTGRAFIK
# =========================================================

build_main_panel <- function(current_year) {
  bars <- veto_totals %>%
    filter(year <= current_year) %>%
    mutate(is_current = year == current_year)
  
  events <- event_tbl %>%
    filter(year <= current_year)
  
  y_max <- 15.8
  
  p <- ggplot() +
    geom_col(
      data = bars %>% filter(!is_current),
      aes(x = year, y = total),
      width = 0.74,
      fill = COL$bar,
      color = NA
    ) +
    geom_col(
      data = bars %>% filter(is_current),
      aes(x = year, y = total),
      width = 0.74,
      fill = COL$current,
      color = "white",
      linewidth = 0.28
    ) +
    geom_text(
      data = bars %>% filter(is_current),
      aes(x = year, y = pmax(total + 0.64, 0.94), label = total),
      family = font_family,
      fontface = "bold",
      color = COL$text,
      size = 6.0
    ) +
    scale_x_continuous(
      breaks = seq(1950, 2020, by = 10),
      limits = c(1945, 2026.8),
      expand = expansion(mult = c(0.006, 0.006))
    ) +
    scale_y_continuous(
      breaks = seq(0, 15, by = 3),
      limits = c(0, y_max),
      expand = expansion(mult = c(0, 0.01))
    ) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_family = font_family, base_size = 18) +
    theme(
      plot.background = element_rect(fill = COL$bg, color = NA),
      panel.background = element_rect(fill = COL$bg, color = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = COL$grid, linewidth = 0.60),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(
        color = COL$text,
        size = 16.5,
        face = "bold",
        margin = margin(t = 10)
      ),
      axis.text.y = element_text(
        color = COL$subtext,
        size = 15.8
      ),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      plot.margin = margin(4, 24, 4, 24)
    )
  
  if (nrow(events) > 0) {
    p <- p +
      geom_segment(
        data = events,
        aes(x = year, xend = year, y = 0.20, yend = y_max, color = col),
        linewidth = 1.02,
        linetype = "dashed",
        alpha = 0.96,
        inherit.aes = FALSE,
        show.legend = FALSE
      ) +
      geom_label(
        data = events,
        aes(x = label_x, y = label_y, label = label, color = col),
        family = font_family,
        fontface = "bold",
        fill = COL$card,
        size = 3.85,
        label.size = 0.28,
        label.padding = grid::unit(0.24, "lines"),
        label.r = grid::unit(0.10, "lines"),
        lineheight = 0.92,
        inherit.aes = FALSE,
        show.legend = FALSE
      ) +
      scale_color_identity()
  }
  
  p
}


# =========================================================
# PANEL 4: QUELLENBLOCK
# =========================================================

build_caption_panel <- function() {
  ggplot() +
    annotate(
      "text",
      x = 0.01,
      y = 0.98,
      label = caption_text,
      family = font_family,
      color = COL$subtext,
      size = 3.38,
      lineheight = 1.10,
      hjust = 0,
      vjust = 1
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
    theme_void() +
    theme(
      plot.background = element_rect(fill = COL$bg, color = NA),
      plot.margin = margin(4, 28, 10, 28)
    )
}


# =========================================================
# KOMPLETTES FRAME
# =========================================================

build_full_frame <- function(current_year) {
  title_panel      <- build_title_panel(current_year)
  scoreboard_panel <- build_scoreboard_panel(current_year)
  main_panel       <- build_main_panel(current_year)
  caption_panel    <- build_caption_panel()
  
  title_panel /
    scoreboard_panel /
    main_panel /
    caption_panel +
    plot_layout(heights = c(0.14, 0.18, 0.45, 0.23)) &
    theme(plot.background = element_rect(fill = COL$bg, color = NA))
}


# =========================================================
# STATISCHE VORSCHAU IM PLOT-FENSTER
# =========================================================

final_preview_plot <- build_full_frame(2026)
final_preview_plot


# =========================================================
# VIDEO-RENDERFUNKTION MIT AUTO-ÖFFNEN
# =========================================================

render_video <- function(
    width,
    height,
    output_file,
    fps = 25,
    hold_frames = 10,
    end_pause = 45,
    delete_frames = FALSE,
    open_after_render = TRUE
) {
  
  frame_years <- c(
    rep(1946:2026, each = hold_frames),
    rep(2026, end_pause)
  )
  
  frames_dir <- file.path(
    output_dir,
    paste0(tools::file_path_sans_ext(basename(output_file)), "_frames")
  )
  
  dir.create(frames_dir, recursive = TRUE, showWarnings = FALSE)
  
  frame_files <- file.path(
    frames_dir,
    sprintf("frame_%04d.png", seq_along(frame_years))
  )
  
  use_ragg <- requireNamespace("ragg", quietly = TRUE)
  
  for (i in seq_along(frame_years)) {
    p <- build_full_frame(frame_years[i])
    
    if (use_ragg) {
      ragg::agg_png(
        filename = frame_files[i],
        width = width,
        height = height,
        units = "px",
        res = 144,
        background = COL$bg
      )
    } else {
      png(
        filename = frame_files[i],
        width = width,
        height = height,
        bg = COL$bg
      )
    }
    
    print(p)
    dev.off()
    
    if (i %% 50 == 0 || i == length(frame_years)) {
      message("Gerenderte Frames: ", i, " / ", length(frame_years))
    }
  }
  
  av::av_encode_video(
    input = frame_files,
    output = output_file,
    framerate = fps
  )
  
  message("Video gespeichert unter: ", output_file)
  
  if (open_after_render) {
    open_file_after_render(output_file)
  }
  
  if (delete_frames) {
    unlink(frames_dir, recursive = TRUE, force = TRUE)
    message("Temporäre Einzel-Frames gelöscht.")
  }
}


# =========================================================
# EXPORT-TOGGLES
# =========================================================

RUN_PREVIEW_1080P <- TRUE
RUN_EXPORT_1080P  <- FALSE
RUN_EXPORT_UHD    <- FALSE

preview_1080p_file <- file.path(output_dir, "veto_final_preview_1080p.mp4")
export_1080p_file  <- file.path(output_dir, "akt1_blockierte_weltordnung_final_1080p.mp4")
export_uhd_file    <- file.path(output_dir, "akt1_blockierte_weltordnung_final_uhd_3840x2160.mp4")


# =========================================================
# RENDERING
# =========================================================

if (RUN_PREVIEW_1080P) {
  render_video(
    width = 1920,
    height = 1080,
    output_file = preview_1080p_file,
    fps = 25,
    hold_frames = 10,
    end_pause = 45,
    delete_frames = FALSE,
    open_after_render = TRUE
  )
}

if (RUN_EXPORT_1080P) {
  render_video(
    width = 1920,
    height = 1080,
    output_file = export_1080p_file,
    fps = 25,
    hold_frames = 10,
    end_pause = 45,
    delete_frames = FALSE,
    open_after_render = TRUE
  )
}

if (RUN_EXPORT_UHD) {
  render_video(
    width = 3840,
    height = 2160,
    output_file = export_uhd_file,
    fps = 25,
    hold_frames = 10,
    end_pause = 45,
    delete_frames = FALSE,
    open_after_render = TRUE
  )
}
