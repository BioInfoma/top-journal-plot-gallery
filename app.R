# Top-Journal Plot Gallery
# Modern, Elegant & Spacious Scientific Visualization Reference
library(shiny)
library(bslib)
library(ggplot2)

if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)

cat_df <- read.csv("plot_catalog.csv", stringsAsFactors = FALSE)
tier_labels <- c("1" = "Tier 1: Simple", "2" = "Tier 2: Intermediate", "3" = "Tier 3: Advanced", "4" = "Tier 4: Mind-Blowing")
tier_badge_bg <- c("1" = "#15803D", "2" = "#1D4ED8", "3" = "#C2410C", "4" = "#7E22CE")

load_entry <- function(slug) {
  f <- file.path("plots", paste0(slug, ".R"))
  if (!file.exists(f)) return(NULL)
  e <- new.env()
  sys.source(f, envir = e)
  e
}

get_available_slugs <- function() {
  sub("\\.R$", "", list.files("plots", pattern = "\\.R$"))
}

app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#1E40AF",
  base_font = font_google("Inter", local = FALSE),
  heading_font = font_google("Inter", local = FALSE)
)

ui <- page_fluid(
  theme = app_theme,
  lang = "en",
  style = "max-width: 1440px; margin: 0 auto; padding: 18px 24px; background-color: #F8FAFC; min-height: 100vh;",
  
  # Custom CSS for clean aesthetics, proper spacing, and zero plot-collapse
  tags$head(
    tags$style(HTML("
      body { color: #1E293B; }
      .app-header { border-bottom: 1px solid #E2E8F0; padding-bottom: 14px; margin-bottom: 20px; }
      .plot-container {
        background: #FFFFFF;
        border: 1px solid #E2E8F0;
        border-radius: 12px;
        padding: 18px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.04);
        min-height: 560px;
        width: 100%;
        position: relative;
      }
      .sidebar-panel-clean {
        background: #FFFFFF;
        border: 1px solid #E2E8F0;
        border-radius: 12px;
        padding: 18px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.04);
      }
      .info-card {
        background: #FFFFFF;
        border: 1px solid #E2E8F0;
        border-radius: 12px;
        padding: 20px;
        margin-top: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.04);
      }
      .badge-tier { font-weight: 600; font-size: 0.78rem; padding: 4px 10px; border-radius: 6px; }
      .btn-nav { font-weight: 600; font-size: 0.85rem; padding: 6px 14px; }
      .nav-pills .nav-link.active { background-color: #1E40AF !important; }
      pre { background-color: #0F172A; color: #F8FAFC; padding: 16px; border-radius: 8px; font-size: 0.85rem; }
    "))
  ),
  
  # Clean Header Banner
  div(
    class = "app-header d-flex justify-content-between align-items-center flex-wrap gap-2",
    div(
      tags$h2(style = "margin: 0; font-weight: 800; color: #0F172A; letter-spacing: -0.5px;", 
              "Top-Journal Plot Gallery"),
      tags$p(style = "margin: 2px 0 0 0; color: #64748B; font-size: 0.95rem;", 
             "A visual encyclopedia of 100 publication-grade scientific visualizations (Nature, Science, Cell, Lancet)")
    ),
    div(
      class = "d-flex align-items-center gap-2",
      tags$span(class = "badge bg-primary text-white", style = "font-size: 0.85rem; padding: 6px 14px;", 
                "100 Visualizations")
    )
  ),
  
  # Two-Column Spacious Layout
  div(
    class = "row g-4",
    
    # Left Column: Streamlined Navigation & Filters
    div(
      class = "col-lg-4 col-md-5",
      div(
        class = "sidebar-panel-clean",
        
        tags$h5(style = "font-weight: 700; color: #0F172A; margin-bottom: 14px;", "Browse Catalog"),
        
        # 1. Search Bar
        div(
          class = "mb-3",
          tags$label(class = "form-label text-muted small fw-bold", "SEARCH"),
          textInput("q", NULL, placeholder = "Search by name, category, or keyword...")
        ),
        
        # 2. Filter Row: Tier & Category
        div(
          class = "row g-2 mb-3",
          div(
            class = "col-6",
            tags$label(class = "form-label text-muted small fw-bold", "TIER"),
            selectInput("tier_select", NULL, choices = c("All Tiers" = "all", "Tier 1: Simple" = "1", "Tier 2: Intermediate" = "2", "Tier 3: Advanced" = "3", "Tier 4: Mind-Blowing" = "4"))
          ),
          div(
            class = "col-6",
            tags$label(class = "form-label text-muted small fw-bold", "CATEGORY"),
            selectInput("cat_select", NULL, choices = c("All Categories" = "all", sort(unique(cat_df$category))))
          )
        ),
        
        # 3. Main Plot Selector
        div(
          class = "mb-3",
          tags$label(class = "form-label text-muted small fw-bold", "VISUALIZATION"),
          selectInput("plot_id", NULL, choices = NULL, width = "100%", selectize = TRUE)
        ),
        
        # 4. Quick Flip & Discovery Buttons
        div(
          class = "d-flex gap-2 mb-3",
          actionButton("prev_btn", "Previous", class = "btn btn-outline-secondary btn-nav flex-fill"),
          actionButton("next_btn", "Next", class = "btn btn-outline-secondary btn-nav flex-fill"),
          actionButton("surprise_btn", "Random", class = "btn btn-outline-primary btn-nav flex-fill")
        ),
        
        hr(style = "border-color: #E2E8F0; margin: 16px 0;"),
        
        # Quick summary stats
        div(
          class = "text-muted small",
          uiOutput("sidebar_info")
        )
      )
    ),
    
    # Right Column: High-Visibility Plot Stage & Detailed Guide
    div(
      class = "col-lg-8 col-md-7",
      
      # Top Meta Bar for the Selected Plot
      div(
        class = "d-flex justify-content-between align-items-center mb-3 flex-wrap gap-2",
        div(
          uiOutput("active_plot_title")
        ),
        div(
          class = "d-flex gap-2",
          downloadButton("dl_png", "PNG (300 DPI)", class = "btn btn-sm btn-outline-dark"),
          downloadButton("dl_pdf", "Vector PDF", class = "btn btn-sm btn-outline-dark"),
          downloadButton("dl_csv", "Data (CSV)", class = "btn btn-sm btn-outline-dark")
        )
      ),
      
      # 1. Dedicated, Guaranteed-Render Plot Canvas (Immediately Visible, No Collapse)
      div(
        class = "plot-container",
        plotOutput("plt", height = "540px", width = "100%")
      ),
      
      # 2. Information & Reproducibility Tabs
      div(
        class = "info-card",
        tabsetPanel(
          id = "detail_tabs",
          type = "pills",
          
          # Tab A: Deep Scientific Guide
          tabPanel(
            title = "Scientific Guide",
            div(class = "mt-3", uiOutput("plot_guide_content"))
          ),
          
          # Tab B: Reproducible R Code
          tabPanel(
            title = "R Source Code",
            div(
              class = "mt-3",
              div(
                class = "d-flex justify-content-between align-items-center mb-2",
                tags$span(class = "text-muted small fw-bold", "REPRODUCIBLE SOURCE SCRIPT"),
                tags$button(
                  class = "btn btn-sm btn-primary",
                  onclick = "navigator.clipboard.writeText(document.getElementById('code_block').innerText); this.innerText = 'Copied to Clipboard'; setTimeout(() => this.innerText = 'Copy Code', 2000);",
                  "Copy Code"
                )
              ),
              verbatimTextOutput("code_block")
            )
          ),
          
          # Tab C: Sample Dummy Data
          tabPanel(
            title = "Sample Data",
            div(
              class = "mt-3",
              div(
                class = "d-flex justify-content-between align-items-center mb-2",
                uiOutput("data_table_caption"),
                downloadButton("dl_csv_tab", "Download Full CSV", class = "btn btn-sm btn-outline-primary")
              ),
              div(style = "max-height: 420px; overflow-y: auto;", tableOutput("data_table_preview"))
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  available_slugs <- reactive({
    get_available_slugs()
  })
  
  # Filtered catalog based on sidebar controls
  filtered_plots <- reactive({
    d <- cat_df
    
    if (input$tier_select != "all") {
      d <- d[as.character(d$tier) == input$tier_select, ]
    }
    
    if (input$cat_select != "all") {
      d <- d[d$category == input$cat_select, ]
    }
    
    if (nzchar(input$q)) {
      query <- tolower(trimws(input$q))
      text_corpus <- tolower(paste(d$name, d$category, d$what_it_shows, d$typical_use, d$r_packages))
      d <- d[grepl(query, text_corpus, fixed = TRUE), ]
    }
    
    d
  })
  
  # Populate plot selector
  observe({
    d <- filtered_plots()
    if (nrow(d) == 0) {
      updateSelectInput(session, "plot_id", choices = c("No matching plots" = ""))
      return()
    }
    
    labels <- paste0(
      d$id, ". ", d$name,
      " (", d$category, ")"
    )
    choices <- setNames(d$id, labels)
    
    cur <- isolate(input$plot_id)
    if (length(cur) == 1 && cur %in% d$id) {
      updateSelectInput(session, "plot_id", choices = choices, selected = cur)
    } else {
      updateSelectInput(session, "plot_id", choices = choices, selected = d$id[1])
    }
  })
  
  # Previous Plot Button
  observeEvent(input$prev_btn, {
    d <- filtered_plots()
    req(nrow(d) > 0)
    cur_id <- as.integer(input$plot_id)
    idx <- which(d$id == cur_id)
    new_idx <- if (length(idx) == 0 || idx == 1) nrow(d) else idx - 1
    updateSelectInput(session, "plot_id", selected = d$id[new_idx])
  })
  
  # Next Plot Button
  observeEvent(input$next_btn, {
    d <- filtered_plots()
    req(nrow(d) > 0)
    cur_id <- as.integer(input$plot_id)
    idx <- which(d$id == cur_id)
    new_idx <- if (length(idx) == 0 || idx == nrow(d)) 1 else idx + 1
    updateSelectInput(session, "plot_id", selected = d$id[new_idx])
  })
  
  # Random Plot Button
  observeEvent(input$surprise_btn, {
    d <- filtered_plots()
    req(nrow(d) > 0)
    updateSelectInput(session, "plot_id", selected = sample(d$id, 1))
  })
  
  # Current active plot metadata
  active_row <- reactive({
    req(input$plot_id, nzchar(input$plot_id))
    cat_df[cat_df$id == as.integer(input$plot_id), ]
  })
  
  # Load active plot environment
  active_entry <- reactive({
    r <- active_row()
    req(r)
    load_entry(r$slug)
  })
  
  # Sidebar Info
  output$sidebar_info <- renderUI({
    d <- filtered_plots()
    r <- active_row()
    tagList(
      div(class = "d-flex justify-content-between mb-1", 
          span("Matching Plots:"), span(class = "fw-bold", nrow(d))),
      div(class = "d-flex justify-content-between mb-1", 
          span("Active Category:"), span(class = "fw-bold", r$category %||% "-")),
      div(class = "d-flex justify-content-between", 
          span("Active Tier:"), span(class = "fw-bold", tier_labels[as.character(r$tier)] %||% "-"))
    )
  })
  
  # Active Plot Header
  output$active_plot_title <- renderUI({
    r <- active_row()
    req(r)
    
    t_num <- as.character(r$tier)
    t_label <- tier_labels[t_num]
    t_color <- tier_badge_bg[t_num]
    
    tagList(
      div(
        class = "d-flex align-items-center gap-2 flex-wrap",
        tags$h3(style = "margin: 0; font-weight: 800; color: #0F172A; font-size: 1.45rem;", 
                paste0(r$id, ". ", r$name))
      ),
      div(
        class = "d-flex align-items-center gap-2 mt-1 flex-wrap",
        tags$span(class = "badge text-white", style = paste0("background-color: ", t_color, ";"), t_label),
        tags$span(class = "badge bg-light text-dark border", r$category),
        tags$span(class = "badge bg-light text-muted border", paste("Difficulty:", r$build_difficulty))
      )
    )
  })
  
  # RENDER PLOT (Always renders reliably, explicit height & width)
  output$plt <- renderPlot({
    r <- active_row()
    req(r)
    e <- active_entry()
    
    if (is.null(e)) {
      plot.new()
      text(0.5, 0.55, paste0(r$name, "\nPlot script not loaded"), 
           cex = 1.3, font = 2, col = "#64748B")
      text(0.5, 0.42, paste0("Associated Dataset: data/", r$dummy_dataset, ".csv\nRequired Packages: ", r$r_packages), 
           cex = 1.0, col = "#94A3B8")
      return()
    }
    
    tryCatch({
      p <- e$make_plot("data")
      if (inherits(p, "ggplot")) {
        print(p)
      } else if (inherits(p, "gtable") || inherits(p, "grob")) {
        grid::grid.newpage()
        grid::grid.draw(p)
      }
    }, error = function(err) {
      plot.new()
      text(0.5, 0.5, paste("Error rendering figure:\n", conditionMessage(err)), col = "#DC2626", cex = 1.1)
    })
  }, res = 110)
  
  # Scientific Guide Content
  output$plot_guide_content <- renderUI({
    r <- active_row()
    req(r)
    e <- active_entry()
    
    exp <- if (!is.null(e)) {
      e$explain()
    } else {
      list(
        how_to_read = r$what_it_shows,
        when_to_use = r$typical_use,
        pitfalls = "Maintain publication-grade styling, correct coordinate scaling, and documented confidence intervals.",
        journal_archetype = "Standard figure type in Nature, Science, Cell, and Lancet publications.",
        data_recipe = paste("Requires input from dataset:", r$dummy_dataset)
      )
    }
    
    pkg_badges <- lapply(trimws(unlist(strsplit(r$r_packages, "[,;]"))), function(p) {
      tags$span(class = "badge bg-light text-primary border me-1 font-monospace", p)
    })
    
    div(
      class = "row g-3",
      
      div(
        class = "col-md-6",
        div(
          style = "background: #F8FAFC; border-radius: 8px; padding: 18px; border: 1px solid #E2E8F0; border-left: 3px solid #1E40AF; height: 100%;",
          tags$h6(style = "font-weight: 700; color: #1E40AF; margin-bottom: 8px;", "How to Read This Figure"),
          tags$p(style = "color: #334155; line-height: 1.6; margin-bottom: 16px; font-size: 0.95rem;", exp$how_to_read),
          
          tags$h6(style = "font-weight: 700; color: #047857; margin-bottom: 8px;", "When to Use in Research"),
          tags$p(style = "color: #334155; line-height: 1.6; margin-bottom: 0; font-size: 0.95rem;", exp$when_to_use)
        )
      ),
      
      div(
        class = "col-md-6",
        div(
          style = "background: #F8FAFC; border-radius: 8px; padding: 18px; border: 1px solid #E2E8F0; border-left: 3px solid #DC2626; height: 100%;",
          tags$h6(style = "font-weight: 700; color: #B91C1C; margin-bottom: 8px;", "Common Pitfalls & Traps"),
          tags$p(style = "color: #334155; line-height: 1.6; margin-bottom: 16px; font-size: 0.95rem;", exp$pitfalls),
          
          tags$h6(style = "font-weight: 700; color: #0F172A; margin-bottom: 4px;", "Literature Archetype"),
          tags$p(style = "color: #475569; font-size: 0.92rem; margin-bottom: 14px;", exp$journal_archetype %||% "Nature & Science standard reporting"),
          
          tags$h6(style = "font-weight: 700; color: #0F172A; margin-bottom: 6px;", "Recommended Packages"),
          div(class = "d-flex flex-wrap gap-1", pkg_badges)
        )
      )
    )
  })
  
  # Reproducible Code Output
  output$code_block <- renderText({
    r <- active_row()
    req(r)
    f <- file.path("plots", paste0(r$slug, ".R"))
    if (file.exists(f)) {
      paste(readLines(f), collapse = "\n")
    } else {
      paste0("# Script for: ", r$name, " (slug: ", r$slug, ")\n",
             "# Dataset: data/", r$dummy_dataset, ".csv\n",
             "# Packages: ", r$r_packages, "\n")
    }
  })
  
  # Data Caption
  output$data_table_caption <- renderUI({
    r <- active_row()
    req(r)
    tags$span(class = "text-muted small fw-bold", paste0("SOURCE DATASET: data/", r$dummy_dataset, ".csv (First 25 Rows)"))
  })
  
  # Data Table Output
  output$data_table_preview <- renderTable({
    r <- active_row()
    req(r)
    f <- file.path("data", paste0(r$dummy_dataset, ".csv"))
    if (file.exists(f)) {
      head(read.csv(f), 25)
    } else {
      data.frame(Status = paste0("Simulated within plot script (data/", r$dummy_dataset, ".csv)"))
    }
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")
  
  # Downloads
  # 1. 300 DPI PNG
  output$dl_png <- downloadHandler(
    filename = function() paste0(active_row()$slug, "_topjournal_300dpi.png"),
    content = function(file) {
      e <- active_entry()
      req(e)
      p <- e$make_plot("data")
      if (inherits(p, "ggplot")) {
        ggsave(file, plot = p, device = "png", width = 8.5, height = 5.5, dpi = 300)
      } else if (inherits(p, "gtable") || inherits(p, "grob")) {
        png(file, width = 8.5, height = 5.5, units = "in", res = 300)
        grid::grid.newpage()
        grid::grid.draw(p)
        dev.off()
      } else {
        png(file, width = 8.5, height = 5.5, units = "in", res = 300)
        e$make_plot("data")
        dev.off()
      }
    }
  )
  
  # 2. Vector PDF
  output$dl_pdf <- downloadHandler(
    filename = function() paste0(active_row()$slug, "_topjournal.pdf"),
    content = function(file) {
      e <- active_entry()
      req(e)
      p <- e$make_plot("data")
      if (inherits(p, "ggplot")) {
        ggsave(file, plot = p, device = "pdf", width = 8.5, height = 5.5)
      } else if (inherits(p, "gtable") || inherits(p, "grob")) {
        pdf(file, width = 8.5, height = 5.5)
        grid::grid.newpage()
        grid::grid.draw(p)
        dev.off()
      } else {
        pdf(file, width = 8.5, height = 5.5)
        e$make_plot("data")
        dev.off()
      }
    }
  )
  
  # 3. CSV Dataset
  output$dl_csv <- downloadHandler(
    filename = function() paste0(active_row()$dummy_dataset, ".csv"),
    content = function(file) {
      src <- file.path("data", paste0(active_row()$dummy_dataset, ".csv"))
      if (file.exists(src)) file.copy(src, file)
      else write.csv(data.frame(Note = "Simulated inside script"), file, row.names = FALSE)
    }
  )
  
  output$dl_csv_tab <- downloadHandler(
    filename = function() paste0(active_row()$dummy_dataset, ".csv"),
    content = function(file) {
      src <- file.path("data", paste0(active_row()$dummy_dataset, ".csv"))
      if (file.exists(src)) file.copy(src, file)
      else write.csv(data.frame(Note = "Simulated inside script"), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
