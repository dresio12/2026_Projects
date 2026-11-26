library(shiny)
library(bslib)
library(DT)
library(dplyr)

# Load your data
r5_all <- readRDS("r5_all2026.rds") 
r5_bsplit <- readRDS("r5_bsplit2026.rds")
r5_psplit <- readRDS("r5_psplit2026.rds")
r5_btot <- readRDS("r5_btot2026.rds")
r5_ptot <- readRDS("r5_ptot2026.rds")

r5_advh <- readRDS("r5_advh2026.rds")
r5_advp <- readRDS("r5_advp2026.rds")

# Higher-is-better columns
advh_cols <- c("PS Score", "xwOBA", "wOBA", "xBA", "BA", "OBP", "xSLG", "SLG", 
               "BB%", "Max EV", "90th% EV", "Spd", "Z-Contact%", "Hard Hit%", 
               "PullAir%", "Z-Swing%", "Swing%")
advp_cols <- c("PS Score", "Max Velo", "K%", "Chase%", "Whiff%", "SwStr%",
               "Extension", "Contact Quality")

# Lower-is-better columns
advh_cols_bad <- c("K%", "Chase%", "Whiff%", "SwStr%")
advp_cols_bad <- c("xwOBA", "wOBA", "xBA", "BA", "OBP", "xSLG", "SLG", "BB%",
                   "Hard Hit%", "Z-Contact%")

# Rounding for the advanced tabs (display only)
round2_cols <- c("PS Score", "Spd", "Contact Quality")
round1_cols <- c("Chase%", "Swing%", "Z-Swing%", "Z-Contact%", "SwStr%",
                 "K%", "90th% EV", "PullAir%", "Whiff%", "BB%", "LA",
                 "Hard Hit%", "HardHit%")

# Rate stats displayed with fixed decimals on every tab (e.g. 0.3 -> 0.300)
three_dec_cols <- c("AVG", "BA", "OBP", "SLG", "OPS", "ISO", "BABIP",
                    "wOBA", "xBA", "xSLG", "xwOBA")
two_dec_cols   <- c("ERA", "FIP", "xFIP", "WHIP", "K/9", "BB/9", "K/BB", "HR/9")

# Dataset classification
dataset_types <- list(
  "R5 Roster" = "other",
  "Batting Split" = "batting",
  "Pitching Split" = "pitching",
  "Batting Totals" = "batting",
  "Pitching Totals" = "pitching",
  "Advanced Hitting" = "adv_batting",
  "Advanced Pitching" = "adv_pitching"
)

# Helper for creating tabs
dataset_tab <- function(name) {
  tabPanel(
    name,
    fluidPage(
      fluidRow(
        column(3, uiOutput(paste0("org_filter_", name))),
        column(3, uiOutput(paste0("position_filter_", name))),
        column(3, uiOutput(paste0("level_filter_", name))),
        column(3, uiOutput(paste0("pitches_filter_ui_", name)))
      ),
      br(),
      downloadButton(paste0("download_", name), "Download CSV"),
      br(), br(),
      DTOutput(paste0("table_", name))
    )
  )
}

# UI
ui <- fluidPage(
  title = "2026 R5 Draft-Eligible Database",
  theme = bs_theme(version = 5),
  tags$head(
    tags$style(HTML("
      /* Dark palette (keeps the navy look); light mode uses Bootstrap defaults */
      html[data-bs-theme='dark'] {
        --bs-body-bg: #0f2537;
        --bs-body-color: #ebebeb;
        --bs-tertiary-bg: #1b2f42;
        --bs-secondary-bg: #2b3e50;
      }
      th, td {
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }
      .dataTables_scrollBody {
        background-color: var(--bs-body-bg);
      }
      
      /* ---- DARK MODE: force readable table text ---- */
      html[data-bs-theme='dark'] .dataTables_wrapper,
      html[data-bs-theme='dark'] .dt-container,
      html[data-bs-theme='dark'] .dataTables_wrapper label,
      html[data-bs-theme='dark'] .dt-container label,
      html[data-bs-theme='dark'] .dataTables_info {
        color: #ffffff !important;
      }
      html[data-bs-theme='dark'] table.dataTable {
        --bs-table-color: #ffffff;
        --bs-table-bg: #0f2537;
        --bs-table-striped-color: #ffffff;
        --bs-table-striped-bg: rgba(255, 255, 255, 0.05);
        --bs-table-hover-color: #ffffff;
        --bs-table-hover-bg: rgba(255, 255, 255, 0.10);
        color: #ffffff;
      }
      html[data-bs-theme='dark'] table.dataTable th,
      html[data-bs-theme='dark'] table.dataTable td,
      html[data-bs-theme='dark'] table.dataTable tbody tr {
        color: #ffffff !important;
      }
      /* unshaded cells get the dark surface; shaded cells keep their inline color */
      html[data-bs-theme='dark'] table.dataTable tbody td {
        background-color: #0f2537;
      }
      /* brighter gridlines in dark mode (raise the alpha values to make them stronger) */
      html[data-bs-theme='dark'] table.dataTable {
        --bs-table-border-color: rgba(255, 255, 255, 0.30);
      }
      html[data-bs-theme='dark'] table.dataTable th,
      html[data-bs-theme='dark'] table.dataTable td {
        border-bottom: 1px solid rgba(255, 255, 255, 0.30) !important;
        border-right: 1px solid rgba(255, 255, 255, 0.15) !important;
      }
      
      /* ---- LIGHT MODE: force dark table text ---- */
      html[data-bs-theme='light'] table.dataTable th,
      html[data-bs-theme='light'] table.dataTable td,
      html[data-bs-theme='light'] table.dataTable tbody tr {
        color: #212529 !important;
      }
    "))
  ),
  
  # Title + dark/light toggle (dark by default)
  div(
    style = "display:flex; justify-content:space-between; align-items:center; margin: 15px 0;",
    h2("2026 R5 Draft-Eligible Database", style = "margin:0;"),
    input_dark_mode(id = "color_mode", mode = "dark")
  ),
  
  # --- Main Data Tabs ---
  tabsetPanel(
    dataset_tab("R5 Roster"),
    dataset_tab("Batting Split"),
    dataset_tab("Pitching Split"),
    dataset_tab("Batting Totals"),
    dataset_tab("Pitching Totals")
  ),
  
  br(), hr(), br(),
  
  # --- Advanced Data Tabs ---
  h3("Advanced Metrics", style = "text-align:center;"),
  tabsetPanel(
    dataset_tab("Advanced Hitting"),
    dataset_tab("Advanced Pitching")
  ),
  
  br(),
  div(
    style = "text-align:center; font-size: 12px; color: gray; margin-top: 20px;",
    "Statistical and eligibility data acquired from FanGraphs and RosterResource.
    Level in the R5 Roster tab is the FanGraphs projected level – for the player's
    actual level, refer to their MiLB profile. Advanced Stats courtesy of Prospect
    Savant."
  )
)

# Red-Yellow-Green color for a value x in [0, 1].
# Dark mode: deep, muted palette (pairs with light text).
# Light mode: Excel-style pastel palette (pairs with dark text).
gradient_color <- function(x, dark = TRUE) {
  if (dark) {
    if (x < 0.5) {
      ratio <- x * 2
      r <- round(120 + (200 - 120) * ratio)
      g <- round(30 + (150 - 30) * ratio)
      b <- round(30 + (60 - 30) * ratio)
    } else {
      ratio <- (x - 0.5) * 2
      r <- round(200 - (200 - 40) * ratio)
      g <- round(150 - (150 - 140) * ratio)
      b <- round(60 - (60 - 40) * ratio)
    }
  } else {
    if (x < 0.5) {
      ratio <- x * 2
      r <- round(248 + (255 - 248) * ratio)
      g <- round(105 + (235 - 105) * ratio)
      b <- round(107 + (132 - 107) * ratio)
    } else {
      ratio <- (x - 0.5) * 2
      r <- round(255 - (255 - 99) * ratio)
      g <- round(235 - (235 - 190) * ratio)
      b <- round(132 - (132 - 123) * ratio)
    }
  }
  sprintf("rgb(%d,%d,%d)", r, g, b)
}

# Shade one column with a binned red-yellow-green gradient.
# Uses styleInterval (range-based) instead of styleEqual (exact-match), so it
# is not thrown off by NAs, duplicate values, or floating-point mismatches.
shade_column <- function(dt, tmp, col, good = TRUE, dark = TRUE, n_bins = 30) {
  if (!col %in% names(tmp)) return(dt)
  
  vals <- tmp[[col]]
  if (!is.numeric(vals)) {
    message("Skipping shading for non-numeric column: ", col)
    return(dt)
  }
  
  rng <- range(vals, na.rm = TRUE)
  if (!all(is.finite(rng)) || rng[1] == rng[2]) return(dt)
  
  # n_bins - 1 inner cut points -> n_bins colors
  cuts <- seq(rng[1], rng[2], length.out = n_bins + 1)[-c(1, n_bins + 1)]
  mids <- (seq_len(n_bins) - 0.5) / n_bins
  if (!good) mids <- 1 - mids
  colors <- vapply(mids, gradient_color, character(1), dark = dark)
  
  dt |> formatStyle(col, backgroundColor = styleInterval(cuts, colors))
}

# Build a DT columnDef that forces a fixed number of decimals (display only)
fixed_dec_def <- function(tmp, cols, digits) {
  idx <- which(names(tmp) %in% cols) - 1   # DT uses 0-based indices
  if (!length(idx)) return(NULL)
  js <- sprintf("function(data, type, row) {
    if (type !== 'display' || typeof data !== 'number') return data;
    return data.toFixed(%d);
  }", digits)
  list(targets = idx, render = JS(js))
}

# Server
server <- function(input, output, session) {
  
  datasets <- list(
    "R5 Roster" = r5_all,
    "Batting Split" = r5_bsplit,
    "Pitching Split" = r5_psplit,
    "Batting Totals" = r5_btot,
    "Pitching Totals" = r5_ptot,
    "Advanced Hitting" = r5_advh,
    "Advanced Pitching" = r5_advp
  )
  
  # Current color mode (defaults to dark until the browser reports it)
  is_dark <- reactive({
    mode <- input$color_mode
    if (is.null(mode)) TRUE else identical(mode, "dark")
  })
  
  for (name in names(datasets)) {
    local({
      nm <- name
      df <- datasets[[nm]]
      type <- dataset_types[[nm]]
      
      # Filters
      output[[paste0("org_filter_", nm)]] <- renderUI({
        if ("Organization" %in% names(df))
          selectInput(paste0("organization_", nm), "Organization:",
                      choices = c("All", sort(unique(df$Organization))),
                      selected = "All")
      })
      
      output[[paste0("position_filter_", nm)]] <- renderUI({
        if ("Position" %in% names(df))
          selectInput(paste0("position_", nm), "Position:",
                      choices = c("All", sort(unique(df$Position))),
                      selected = "All")
      })
      
      output[[paste0("level_filter_", nm)]] <- renderUI({
        if ("Level" %in% names(df))
          selectInput(paste0("level_", nm), "Level:",
                      choices = c("All", sort(unique(df$Level))),
                      selected = "All")
      })
      
      output[[paste0("pitches_filter_ui_", nm)]] <- renderUI({
        if (type %in% c("batting", "adv_batting"))
          numericInput(paste0("pitches_filter_", nm), "Minimum PA:", value = 50, min = 0)
        else if (type %in% c("pitching", "adv_pitching"))
          numericInput(paste0("pitches_filter_", nm), "Minimum Pitches:", value = 100, min = 0)
      })
      
      # Filtered data reactive
      filtered_data <- reactive({
        tmp <- df
        
        org_input <- input[[paste0("organization_", nm)]]
        pos_input <- input[[paste0("position_", nm)]]
        lvl_input <- input[[paste0("level_", nm)]]
        pitch_input <- input[[paste0("pitches_filter_", nm)]]
        
        if (!is.null(org_input) && org_input != "All") tmp <- tmp |> filter(Organization == org_input)
        if (!is.null(pos_input) && pos_input != "All") tmp <- tmp |> filter(Position == pos_input)
        if (!is.null(lvl_input) && lvl_input != "All") tmp <- tmp |> filter(Level == lvl_input)
        
        if (!is.null(pitch_input)) {
          if (type %in% c("batting", "adv_batting") && "PA" %in% names(tmp)) tmp <- tmp |> filter(PA >= pitch_input)
          if (type %in% c("pitching", "adv_pitching") && "Pitches" %in% names(tmp)) tmp <- tmp |> filter(Pitches >= pitch_input)
        }
        
        tmp
      })
      
      # Render Table
      output[[paste0("table_", nm)]] <- renderDT({
        tmp <- filtered_data()
        dark <- is_dark()
        
        # Default sort
        order_opt <- NULL
        if (nm == "R5 Roster" && "Org Rank" %in% names(tmp))
          order_opt <- list(list(which(names(tmp) == "Org Rank") - 1, 'asc'))
        else if (type %in% c("batting", "adv_batting") && "wRC+" %in% names(tmp))
          order_opt <- list(list(which(names(tmp) == "wRC+") - 1, 'desc'))
        else if (type %in% c("pitching", "adv_pitching") && "ERA" %in% names(tmp))
          order_opt <- list(list(which(names(tmp) == "ERA") - 1, 'asc'))
        else if (grepl("Advanced", nm) && "PS Score" %in% names(tmp))
          order_opt <- list(list(which(names(tmp) == "PS Score") - 1, 'desc'))
        
        # Fixed-decimal display for rate stats (0.3 -> 0.300, 3.5 -> 3.50)
        col_defs <- Filter(Negate(is.null), list(
          fixed_dec_def(tmp, three_dec_cols, 3),
          fixed_dec_def(tmp, two_dec_cols, 2)
        ))
        
        dt <- datatable(
          tmp,
          options = list(
            scrollY = "400px",
            scrollX = TRUE,
            pageLength = 10,
            scrollCollapse = TRUE,
            paging = FALSE,
            order = order_opt,
            columnDefs = col_defs
          ),
          rownames = FALSE
        )
        
        # Gradient formatting for advanced datasets (Red-Yellow-Green)
        if (nm == "Advanced Hitting") {
          for (col in advh_cols)     dt <- shade_column(dt, tmp, col, good = TRUE,  dark = dark)
          for (col in advh_cols_bad) dt <- shade_column(dt, tmp, col, good = FALSE, dark = dark)
        }
        if (nm == "Advanced Pitching") {
          for (col in advp_cols)     dt <- shade_column(dt, tmp, col, good = TRUE,  dark = dark)
          for (col in advp_cols_bad) dt <- shade_column(dt, tmp, col, good = FALSE, dark = dark)
        }
        
        # Rounding for advanced datasets (display only; shading/sorting use full precision)
        if (grepl("Advanced", nm)) {
          r2 <- intersect(round2_cols, names(tmp))
          r1 <- intersect(round1_cols, names(tmp))
          if (length(r2)) dt <- dt |> formatRound(r2, digits = 2)
          if (length(r1)) dt <- dt |> formatRound(r1, digits = 1)
        }
        
        dt
      })
      
      # Download CSV
      output[[paste0("download_", nm)]] <- downloadHandler(
        filename = function() paste0(gsub(" ", "_", nm), "_filtered.csv"),
        content = function(file) write.csv(filtered_data(), file, row.names = FALSE)
      )
    })
  }
}

# Run App
shinyApp(ui, server)