library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(factoextra)
library(DT)

# ── Data prep ────────────────────────────────────────────────────────────────
hms <- read.csv("HMS_Small.csv") %>%
  mutate(
    age       = as.numeric(age),
    AI_3      = as.numeric(AI_3),
    flourish  = as.numeric(flourish),
    deprawsc  = as.numeric(deprawsc),
    anx_score = as.numeric(anx_score)
  )

ai_labels <- c(
  "1" = "Daily",
  "2" = "Several times/week",
  "3" = "Once/week",
  "4" = "Several times/month",
  "5" = "Once/month",
  "6" = "Never / Rarely"
)
hms$AI_label <- factor(ai_labels[as.character(hms$AI_3)], levels = ai_labels)

# ── Pre-compute clusters ──────────────────────────────────────────────────────
mk_cluster <- function(data, vars, k = 5, seed = 123) {
  df <- data %>% select(all_of(vars)) %>% drop_na() %>%
    mutate(across(everything(), as.numeric))
  sc <- scale(df)
  set.seed(seed)
  km <- kmeans(sc, centers = k, nstart = 50)
  list(df = df, scaled = sc, km = km,
       df_clust = df %>% mutate(cluster_group = factor(km$cluster)))
}

cl_dep <- mk_cluster(hms, c("age", "AI_3", "deprawsc"))
cl_fl  <- mk_cluster(hms, c("age", "AI_3", "flourish"))

# ── Palette config ────────────────────────────────────────────────────────────
# "FRENCH" is the Easter egg entry (second option)
palette_choices <- c(
  "Blues (default)" = "Blues",
  "Pastel"          = "FRENCH",      # 🇫🇷 surprise
  "Greens"          = "Greens",
  "Oranges"         = "Oranges",
  "Purples"         = "Purples",
  "Red-Yellow"      = "YlOrRd",
  "Teal-Green"      = "BuGn",
  "Set 1"           = "Set1",
  "Dark palette"    = "Dark2"
)
sequential_palettes <- c("Blues","Greens","Oranges","Purples","YlOrRd","BuGn")

# French flag gradient: deep blue → white → deep red, 6 steps
french_colors <- c("#002395", "#6688CC", "#C8D4EE", "#F0BBBB", "#CC3333", "#ED2939")

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$style(HTML("
    .row { display: flex; min-height: calc(100vh - 160px); }
    .col-sm-4 { display: flex; flex-direction: column; }
    .col-sm-4 > .well { flex: 1; margin-bottom: 0; }
  ")),

  titlePanel("Generative AI Use & Student Mental Health Explorer"),

  wellPanel(
    p(strong("About this app:"),
      "This interactive dashboard explores associations between intentional weekly
      generative AI use frequency and student well-being outcomes (depression,
      anxiety, flourishing, and age) using the Healthy Minds Study (HMS) 2024\u20132025.
      Adjust controls on the left \u2014 all outputs update instantly."),

    tags$details(
      style = "margin-top: 6px;",
      tags$summary(
        style = "cursor: pointer; color: #555; font-size: 0.9em;",
        "More \u25bc"
      ),
      tags$div(
        style = "margin-top: 10px; font-size: 0.9em; line-height: 1.7;",
        tags$table(
          style = "width: 100%; border-collapse: collapse;",
          tags$thead(
            tags$tr(
              tags$th(style = "text-align: left; padding: 6px 10px; border-bottom: 2px solid #ccc; width: 30%;", "Tab"),
              tags$th(style = "text-align: left; padding: 6px 10px; border-bottom: 2px solid #ccc;", "Description")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td(style = "padding: 6px 10px; border-bottom: 1px solid #eee; font-weight: bold;", "Visualization"),
              tags$td(style = "padding: 6px 10px; border-bottom: 1px solid #eee;",
                "Interactive chart showing the distribution of a selected outcome variable
                (depression, anxiety, flourishing, or age) across six AI use frequency groups.
                Choose between boxplot, violin plot, or jitter display, and customize the color palette.")
            ),
            tags$tr(
              tags$td(style = "padding: 6px 10px; border-bottom: 1px solid #eee; font-weight: bold;", "Summary Statistics"),
              tags$td(style = "padding: 6px 10px; border-bottom: 1px solid #eee;",
                "Descriptive statistics (N, mean, median, SD, min, max) for the selected outcome,
                broken down by each AI use frequency group.")
            ),
            tags$tr(
              tags$td(style = "padding: 6px 10px; border-bottom: 1px solid #eee; font-weight: bold;", "Statistical Tests"),
              tags$td(style = "padding: 6px 10px; border-bottom: 1px solid #eee;",
                "Five inferential tests on the selected outcome vs. AI use frequency:
                (1) Kruskal-Wallis test for group differences,
                (2) Spearman rank correlation,
                (3) simple linear regression (AI_3 as numeric predictor),
                (4) Dunn\u2019s post-hoc pairwise comparisons (Bonferroni-corrected),
                (5) Jonckheere-Terpstra ordered trend test.")
            ),
            tags$tr(
              tags$td(style = "padding: 6px 10px; border-bottom: 1px solid #eee; font-weight: bold;", "Correlation Heatmap"),
              tags$td(style = "padding: 6px 10px; border-bottom: 1px solid #eee;",
                "Spearman correlation matrix across all five variables (age, AI use frequency,
                depression, anxiety, and flourishing), visualized as a color-coded heatmap.
                Values range from \u22121 (negative) to +1 (positive).")
            ),
            tags$tr(
              tags$td(style = "padding: 6px 10px; font-weight: bold;", "Cluster Analysis"),
              tags$td(style = "padding: 6px 10px;",
                "K-means clustering (k\u2009=\u20095, standardized variables) of students based on
                age, AI use frequency, and either depression score (PHQ-9) or flourishing score.
                Results are shown as a PCA-projected scatter plot, a cluster profile heatmap
                (z-score colors with raw means), and a summary table of cluster means.")
            )
          )
        )
      )
    )
  ),

  sidebarLayout(
    sidebarPanel(

      conditionalPanel(
        condition = "input.tabs !== 'Cluster Analysis' && input.tabs !== 'Correlation Heatmap'",
        selectInput("outcome", "Outcome Variable",
          choices = c(
            "Depression Score (PHQ-9)" = "deprawsc",
            "Anxiety Score (GAD-7)"    = "anx_score",
            "Flourishing Score"        = "flourish",
            "Age"                      = "age"
          )
        )
      ),

      conditionalPanel(
        condition = "input.tabs === 'Cluster Analysis'",
        radioButtons("cluster_choice", "Cluster Analysis for:",
          choices  = c("Depression (PHQ-9)" = "dep", "Flourishing" = "fl"),
          selected = "dep"
        )
      ),

      conditionalPanel(
        condition = "input.tabs === 'Visualization'",
        hr(),
        radioButtons("plot_type", "Plot Type",
          choices  = c("Boxplot" = "box", "Violin Plot" = "violin", "Jitter" = "jitter"),
          selected = "box"
        ),
        conditionalPanel(
          condition = "input.plot_type === 'jitter'",
          checkboxInput("show_reg", "Show regression line", value = FALSE)
        ),
        hr(),
        selectInput("palette", "Color Palette",
                    choices = palette_choices, selected = "Blues")
      )
    ),

    mainPanel(
      tabsetPanel(id = "tabs",

        # ── Visualization ──
        tabPanel("Visualization",
          br(),
          plotOutput("main_plot", height = "450px")
        ),

        # ── Summary Statistics ──
        tabPanel("Summary Statistics",
          br(),
          DTOutput("summary_table")
        ),

        # ── Statistical Tests ──
        tabPanel("Statistical Tests",
          br(),
          h4("1. Kruskal-Wallis Test"),
          p("Non-parametric test for differences in outcome across all AI use groups."),
          verbatimTextOutput("kw_result"),

          hr(),
          h4("2. Spearman Rank Correlation"),
          p("Monotonic association between AI use frequency (numeric) and the outcome."),
          verbatimTextOutput("spearman_result"),

          hr(),
          h4("3. Simple Linear Regression"),
          p("Linear model: outcome ~ AI_3 (treating AI use as numeric predictor)."),
          verbatimTextOutput("lm_result"),

          hr(),
          h4("4. Dunn's Post-hoc Test (pairwise, Bonferroni)"),
          p("Pairwise comparisons after Kruskal-Wallis. Requires package 'dunn.test'."),
          verbatimTextOutput("dunn_result"),

          hr(),
          h4("5. Jonckheere-Terpstra Trend Test"),
          p("Tests for an ordered trend across AI use groups. Requires package 'clinfun'."),
          verbatimTextOutput("jt_result")
        ),

        # ── Correlation Heatmap ──
        tabPanel("Correlation Heatmap",
          br(),
          p("Spearman correlation matrix across all five variables (complete cases)."),
          plotOutput("cor_heatmap", height = "420px")
        ),

        # ── Cluster Analysis (merged) ──
        tabPanel("Cluster Analysis",
          br(),
          p(strong("Method:"), "k-means, k = 5, standardized variables — select Depression or Flourishing from the sidebar to switch clusters."),
          hr(),

          h4("Cluster Scatter (PCA projection)"),
          plotOutput("clust_scatter", height = "400px"),

          hr(),
          h4("Cluster Profile Heatmap"),
          plotOutput("clust_heatmap", height = "320px"),

          hr(),
          h4("Cluster Summary (raw means)"),
          DTOutput("clust_table")
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  df_clean <- reactive({
    hms %>% filter(!is.na(.data[[input$outcome]]))
  })

  outcome_label <- reactive({
    switch(input$outcome,
      deprawsc  = "Depression Score (PHQ-9, 0\u201327)",
      anx_score = "Anxiety Score (GAD-7, 0\u201321)",
      flourish  = "Flourishing Score",
      age       = "Age (years)"
    )
  })

  # Apply scale; French palette uses scale_fill_manual instead
  apply_fill_scale <- function(p) {
    pal <- input$palette
    if (pal == "FRENCH") {
      p +
        scale_fill_manual(values  = french_colors) +
        scale_color_manual(values = french_colors)
    } else {
      dir <- if (pal %in% sequential_palettes) -1L else 1L
      p +
        scale_fill_brewer(palette = pal, direction = dir) +
        scale_color_brewer(palette = pal, direction = dir)
    }
  }

  # ── Main plot ──────────────────────────────────────────────────────────────
  output$main_plot <- renderPlot({
    df  <- df_clean()
    out <- input$outcome

    base <- ggplot(df, aes(x = AI_label, y = .data[[out]], fill = AI_label))

    p <- switch(input$plot_type,
      box    = base + geom_boxplot(outlier.alpha = 0.4, width = 0.6),
      violin = base +
                 geom_violin(trim = FALSE, alpha = 0.85) +
                 geom_boxplot(width = 0.08, fill = "white", outlier.shape = NA),
      jitter = base +
                 geom_jitter(aes(color = AI_label), width = 0.25,
                             alpha = 0.45, size = 1.5, show.legend = FALSE) +
                 stat_summary(fun = median, geom = "crossbar",
                              width = 0.5, color = "black", linewidth = 0.6)
    )

    if (input$plot_type == "jitter" && input$show_reg) {
      p <- p + geom_smooth(
        aes(x = as.numeric(AI_label), y = .data[[out]]),
        method = "lm", se = TRUE, color = "firebrick",
        inherit.aes = FALSE, data = df
      )
    }

    p <- apply_fill_scale(p)

    p +
      labs(
        title   = paste("Distribution of", outcome_label(), "by AI Use Frequency"),
        x       = "AI Use Frequency",
        y       = outcome_label(),
        caption = "Data: Healthy Minds Study 2024\u20132025"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position  = "none",
        axis.text.x      = element_text(angle = 25, hjust = 1),
        plot.title       = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
  })

  # ── Summary table ─────────────────────────────────────────────────────────
  output$summary_table <- renderDT({
    df  <- df_clean(); out <- input$outcome
    tbl <- df %>%
      group_by(`AI Use Group` = AI_label) %>%
      summarise(
        N      = n(),
        Mean   = round(mean(.data[[out]], na.rm = TRUE), 2),
        Median = round(median(.data[[out]], na.rm = TRUE), 2),
        SD     = round(sd(.data[[out]], na.rm = TRUE), 2),
        Min    = min(.data[[out]], na.rm = TRUE),
        Max    = max(.data[[out]], na.rm = TRUE),
        .groups = "drop"
      )
    datatable(tbl, rownames = FALSE, width = "100%",
              options = list(dom = "t", pageLength = 10, ordering = FALSE))
  })

  # ── Statistical Tests ─────────────────────────────────────────────────────
  output$kw_result <- renderPrint({
    df <- df_clean(); out <- input$outcome
    kruskal.test(as.formula(paste(out, "~ AI_label")), data = df)
  })

  output$spearman_result <- renderPrint({
    df <- df_clean(); out <- input$outcome
    cor.test(df$AI_3, df[[out]], method = "spearman", exact = FALSE)
  })

  output$lm_result <- renderPrint({
    df <- df_clean(); out <- input$outcome
    summary(lm(as.formula(paste(out, "~ AI_3")), data = df))
  })

  output$dunn_result <- renderPrint({
    df <- df_clean(); out <- input$outcome
    if (!requireNamespace("dunn.test", quietly = TRUE)) {
      cat("Package 'dunn.test' not installed.\nRun: install.packages('dunn.test')\n"); return()
    }
    dunn.test::dunn.test(df[[out]], df$AI_label, method = "bonferroni", kw = FALSE, label = TRUE)
  })

  output$jt_result <- renderPrint({
    df <- df_clean(); out <- input$outcome
    if (!requireNamespace("clinfun", quietly = TRUE)) {
      cat("Package 'clinfun' not installed.\nRun: install.packages('clinfun')\n"); return()
    }
    clinfun::jonckheere.test(df[[out]], df$AI_3, alternative = "increasing")
  })

  # ── Correlation Heatmap ───────────────────────────────────────────────────
  output$cor_heatmap <- renderPlot({
    var_labels <- c(age = "Age", AI_3 = "AI Use (AI_3)",
                    flourish = "Flourish", deprawsc = "PHQ-9", anx_score = "GAD-7")

    cor_mat <- hms %>%
      select(age, AI_3, flourish, deprawsc, anx_score) %>%
      cor(use = "complete.obs", method = "spearman")

    as.data.frame(cor_mat) %>%
      rownames_to_column("Var1") %>%
      pivot_longer(-Var1, names_to = "Var2", values_to = "rho") %>%
      mutate(Var1 = recode(Var1, !!!var_labels),
             Var2 = recode(Var2, !!!var_labels)) %>%
      ggplot(aes(x = Var2, y = Var1, fill = rho)) +
        geom_tile(color = "white", linewidth = 0.6) +
        geom_text(aes(label = sprintf("%.2f", rho)), size = 4) +
        scale_fill_gradient2(limits = c(-1, 1), midpoint = 0,
                             low = "#2166AC", mid = "white", high = "#D6604D") +
        labs(title = "Spearman Correlation Heatmap", x = NULL, y = NULL, fill = "\u03c1") +
        theme_minimal(base_size = 13) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title  = element_text(face = "bold"))
  })

  # ── Cluster: driven by dedicated cluster_choice radio button ─────────────
  cl_use_dep <- reactive({
    req(input$cluster_choice)
    input$cluster_choice == "dep"
  })
  cl_active <- reactive({ if (cl_use_dep()) cl_dep else cl_fl })
  cl_var3   <- reactive({ if (cl_use_dep()) "deprawsc" else "flourish" })
  cl_var3_label <- reactive({
    if (cl_use_dep()) "PHQ-9 (deprawsc)" else "Flourish"
  })
  cl_title <- reactive({
    if (cl_use_dep()) "Cluster Analysis: Age, AI_3, and Depression (PHQ-9)"
    else              "Cluster Analysis: Age, AI_3, and Flourishing"
  })

  output$clust_scatter <- renderPlot({
    cl  <- cl_active()
    km_obj <- cl$km
    km_obj$cluster <- as.integer(cl$df_clust$cluster_group)
    fviz_cluster(km_obj, data = cl$scaled, geom = "point",
                 ellipse.type = "convex", ggtheme = theme_minimal(base_size = 13),
                 main = cl_title())
  })

  output$clust_heatmap <- renderPlot({
    cl        <- cl_active()
    var3      <- cl_var3()
    var3_lbl  <- cl_var3_label()

    cl$df_clust %>%
      group_by(cluster_group) %>%
      summarise(Avg_Age  = mean(age),
                Avg_AI3  = mean(AI_3),
                Avg_Var3 = mean(.data[[var3]]),
                Count    = n(), .groups = "drop") %>%
      arrange(Avg_Age) %>%
      mutate(cluster_label = paste0("Cluster ", cluster_group, " (n=", Count, ")"),
             cluster_label = factor(cluster_label, levels = cluster_label)) %>%
      pivot_longer(c(Avg_Age, Avg_AI3, Avg_Var3),
                   names_to = "feature", values_to = "value") %>%
      group_by(feature) %>%
      mutate(z = as.numeric(scale(value)), value_show = round(value, 2)) %>%
      ungroup() %>%
      mutate(feature = recode(feature,
               Avg_Age  = "Age",
               Avg_AI3  = "AI Use (AI_3)",
               Avg_Var3 = var3_lbl)) %>%
      ggplot(aes(x = feature, y = cluster_label, fill = z)) +
        geom_tile(color = "white", linewidth = 0.6) +
        geom_text(aes(label = value_show), size = 3.6) +
        scale_fill_gradient2(midpoint = 0,
                             low = "#2166AC", mid = "white", high = "#D6604D") +
        labs(x = NULL, y = NULL, fill = "Z-score",
             title = "Cluster Profile Heatmap (raw means, z-score color)") +
        theme_minimal(base_size = 12) +
        theme(panel.grid = element_blank())
  })

  output$clust_table <- renderDT({
    cl       <- cl_active()
    var3     <- cl_var3()
    col_name <- if (input$cluster_choice == "dep") "Avg PHQ-9" else "Avg Flourish"

    tbl <- cl$df_clust %>%
      group_by(Cluster = cluster_group) %>%
      summarise(N          = n(),
                `Avg Age`  = round(mean(age), 2),
                `Avg AI_3` = round(mean(AI_3), 2),
                !!col_name := round(mean(.data[[var3]]), 2),
                .groups = "drop") %>%
      arrange(`Avg Age`)

    datatable(tbl, rownames = FALSE, width = "100%",
              options = list(dom = "t", pageLength = 10, ordering = FALSE))
  })
}

shinyApp(ui = ui, server = server)
