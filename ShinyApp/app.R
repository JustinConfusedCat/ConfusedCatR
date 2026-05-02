# ============================================================
# HMS K-means Cluster Analysis Shiny App
# Author: Justin Liu, MPHc, Cornell University
# Data:   Healthy Minds Study 2024-25
# ============================================================

library(shiny)
library(tidyverse)
library(cluster)
library(factoextra)
library(moments)
library(ggcorrplot)
library(DT)

# ── Data prep ────────────────────────────────────────────────

hms_raw <- read.csv("HMS_Small.csv")

hms <- hms_raw %>%
  mutate(AI_use = 7 - AI_3)   # reverse: 6 = almost constantly, 1 = never

d1 <- hms %>% drop_na(age, AI_use, flourish)
d2 <- hms %>% drop_na(age, AI_use, deprawsc)
d3 <- hms %>% drop_na(age, AI_use, anx_score)

analysis_configs <- list(
  "1" = list(
    label        = "Analysis 1: Age + AI Use + Flourishing",
    outcome      = "flourish",
    out_lab      = "Flourishing Score",
    data         = d1,
    features     = c("age", "AI_use", "flourish"),
    feat_lab     = c("Age", "AI Use Frequency", "Flourishing Score"),
    profile_vars = c("deprawsc", "anx_score"),
    profile_lab  = c("Depression (PHQ-9)", "Anxiety (GAD-7)")
  ),
  "2" = list(
    label        = "Analysis 2: Age + AI Use + Depression",
    outcome      = "deprawsc",
    out_lab      = "Depression Score (PHQ-9)",
    data         = d2,
    features     = c("age", "AI_use", "deprawsc"),
    feat_lab     = c("Age", "AI Use Frequency", "Depression (PHQ-9)"),
    profile_vars = c("flourish", "anx_score"),
    profile_lab  = c("Flourishing Score", "Anxiety (GAD-7)")
  ),
  "3" = list(
    label        = "Analysis 3: Age + AI Use + Anxiety",
    outcome      = "anx_score",
    out_lab      = "Anxiety Score (GAD-7)",
    data         = d3,
    features     = c("age", "AI_use", "anx_score"),
    feat_lab     = c("Age", "AI Use Frequency", "Anxiety (GAD-7)"),
    profile_vars = c("flourish", "deprawsc"),
    profile_lab  = c("Flourishing Score", "Depression (PHQ-9)")
  )
)

var_meta <- list(
  age       = list(label = "Age (years)",                 range = "18–63",  na = sum(is.na(hms$age)),      type = "continuous",
                   desc  = "Respondent age in years. Most respondents are 18–22."),
  AI_3      = list(label = "AI Use Frequency (AI_3)",     range = "1–6",    na = sum(is.na(hms$AI_3)),     type = "ordinal",
                   desc  = "How often the respondent uses generative AI. Original scale: 1 = Almost constantly, 6 = Never."),
  AI_use    = list(label = "AI Use Frequency (reversed)", range = "1–6",    na = sum(is.na(hms$AI_use)),   type = "ordinal",
                   desc  = "Reverse-coded AI_3: 6 = Almost constantly, 1 = Never. Used in clustering so higher = more frequent use."),
  flourish  = list(label = "Flourishing Score",           range = "8–56",   na = sum(is.na(hms$flourish)), type = "continuous",
                   desc  = "Diener Flourishing Scale (8-item sum). Higher = greater well-being."),
  deprawsc  = list(label = "Depression Score (PHQ-9)",    range = "0–27",   na = sum(is.na(hms$deprawsc)), type = "continuous",
                   desc  = "PHQ-9 raw sum. Higher = more severe depression. ≥10 = moderate-to-severe."),
  anx_score = list(label = "Anxiety Score (GAD-7)",       range = "0–21",   na = sum(is.na(hms$anx_score)),type = "continuous",
                   desc  = "GAD-7 sum. Higher = more severe anxiety. ≥10 = moderate-to-severe.")
)

# ── Palette system ────────────────────────────────────────────

palette_choices <- c(
  "Blue Set (Default)"       = "BlueSet",
  "Classic (Blue-Red)"       = "Default",
  "Viridis"                  = "Viridis",
  "Plasma"                   = "Plasma",
  "Color-blind: Okabe-Ito"   = "OkabeIto",
  "Color-blind: Wong"        = "Wong",
  "Rainbow"                  = "Rainbow",
  "Pastel"                   = "Pastel"
)

get_palette <- function(name, n) {
  pals <- list(
    BlueSet   = c("#1565C0","#0097A7","#42A5F5","#004D7A","#26C6DA","#7986CB"),
    Default   = c("#E63946","#457B9D","#2A9D8F","#F4A261","#6A4C93","#A8DADC"),
    Viridis   = scales::viridis_pal(option = "D")(6),
    Plasma    = scales::viridis_pal(option = "C")(6),
    OkabeIto  = c("#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00"),
    Wong      = c("#009E73","#E69F00","#56B4E9","#F0E442","#0072B2","#D55E00"),
    Rainbow   = rainbow(6),
    Pastel    = c("#FFB3BA","#FFDFBA","#FFFFBA","#BAFFC9","#BAE1FF","#D4BAFF")
  )
  pal <- pals[[name]]
  if (is.null(pal)) pal <- pals[["Default"]]
  pal[seq_len(n)]
}

# ── Helper functions ──────────────────────────────────────────

run_kmeans <- function(mat_scaled, k, seed = 123) {
  set.seed(seed)
  kmeans(mat_scaled, centers = k, nstart = 25, iter.max = 100)
}

compute_wss <- function(mat_scaled, kmax = 8) {
  map_dbl(2:kmax, function(k) {
    set.seed(123); kmeans(mat_scaled, centers = k, nstart = 25, iter.max = 100)$tot.withinss
  })
}

compute_sil <- function(mat_scaled, kmax = 6) {
  map_dbl(2:kmax, function(k) {
    set.seed(123)
    km  <- kmeans(mat_scaled, centers = k, nstart = 25, iter.max = 100)
    sil <- silhouette(km$cluster, dist(mat_scaled))
    mean(sil[, 3])
  })
}

theme_hms <- function() {
  theme_minimal(base_size = 13) +
    theme(plot.title    = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(color = "grey40", size = 11),
          legend.position = "bottom")
}

# ── UI ───────────────────────────────────────────────────────

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: 'Helvetica Neue', Helvetica, sans-serif; }
    h2 { color: #1d3557; font-weight: 700; }
    h3 { color: #457b9d; font-weight: 600; margin-top: 18px; }
    h4 { color: #333; font-weight: 600; }
    .info-block { padding: 14px 18px; margin-bottom: 14px;
                  background: #fff; border-left: 4px solid #457b9d;
                  border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,.08); }
    .cluster-card { padding: 14px 18px; margin-bottom: 12px;
                    background: #f8f9fa; border-radius: 6px;
                    border: 1px solid #dee2e6; }
    .cluster-card h4 { margin-top: 0; }
    .n-badge { background:#e8f5e9; color:#2e7d32; padding:2px 8px;
               border-radius:10px; font-size:12px; font-weight:600; }
    .assump-box { background:#f0f7ff; border:1px solid #b8d4f0;
                  border-radius:6px; padding:14px 18px; margin-bottom:16px; }
    .result-ok  { color:#2e7d32; font-weight:600; }
    .result-warn{ color:#e65100; font-weight:600; }
    .logo-bar { display:flex; align-items:center; justify-content:flex-end;
                gap:20px; padding:4px 0; }
    .logo-bar img { height:62px; width:auto; object-fit:contain; }
    .interp-cluster { padding:14px 18px; margin-bottom:12px; border-radius:6px;
                      border-left:5px solid #457b9d; background:#fafafa; }
    .interp-cluster h4 { margin-top:0; color:#1d3557; }
    .badge-high { background:#e3f2fd; color:#0d47a1; padding:1px 7px;
                  border-radius:10px; font-size:12px; font-weight:600; }
    .badge-low  { background:#fce4ec; color:#b71c1c; padding:1px 7px;
                  border-radius:10px; font-size:12px; font-weight:600; }
    .badge-avg  { background:#f3f3f3; color:#555;    padding:1px 7px;
                  border-radius:10px; font-size:12px; }
  "))),

  # ── Custom header with logos ──────────────────────────────
  div(style = "padding: 12px 15px 0 15px;",
    fluidRow(
      column(8,
        h2("Mental Health, Age, and AI Use Among College Students",
           style = "margin-bottom:2px;"),
        p("A K-means Cluster Analysis of the 2024–25 Healthy Minds Study",
          style = "color:#6c757d; font-size:15px; margin-top:0;")
      ),
      column(4,
        div(class = "logo-bar",
          tags$img(src = "cornell_logo_simple_black.svg", alt = "Cornell"),
          tags$img(src = "nyubranding-logoshort.png",     alt = "NYU"),
          tags$img(src = "HMN-logo-150x150.png",          alt = "HMS")
        )
      )
    )
  ),

  tabsetPanel(id = "main_tabs",

    # ── TAB: ABOUT ──────────────────────────────────────────
    tabPanel("About", br(),
      fluidRow(column(8, offset = 1,

        div(class="info-block",
          h3("About This App"),
          p("This interactive Shiny application explores how ", strong("age"),
            " and ", strong("generative AI use frequency"), " co-occur with three mental
             health outcomes — flourishing, depression (PHQ-9), and anxiety (GAD-7) —
             among college students surveyed in the 2024–25 Healthy Minds Study.
             K-means cluster analysis is used to identify naturally occurring student
             subgroups across these dimensions."),
          p("Use the tabs above to navigate through the analysis:"),
          tags$table(style = "width:100%; border-collapse:collapse; font-size:13px; margin-top:8px;",
            tags$thead(
              tags$tr(style="background:#1d3557; color:white;",
                tags$th(style="padding:8px 12px; text-align:left;", "Page"),
                tags$th(style="padding:8px 12px; text-align:left;", "What you will find")
              )
            ),
            tags$tbody(
              tags$tr(style="background:#f8f9fa;",
                tags$td(style="padding:8px 12px; font-weight:600; white-space:nowrap;", "About"),
                tags$td(style="padding:8px 12px;", "Author information, data source, AI disclosure, and this navigation guide.")
              ),
              tags$tr(
                tags$td(style="padding:8px 12px; font-weight:600; white-space:nowrap;", "Research"),
                tags$td(style="padding:8px 12px;", "Research question, analytical methods, and the author's hypothesized student profiles before seeing the cluster results.")
              ),
              tags$tr(style="background:#f8f9fa;",
                tags$td(style="padding:8px 12px; font-weight:600; white-space:nowrap;", "Data Explorer"),
                tags$td(style="padding:8px 12px;", "Interactive histograms, boxplots, and descriptive statistics for each variable. Choose any variable and color palette to explore its distribution.")
              ),
              tags$tr(
                tags$td(style="padding:8px 12px; font-weight:600; white-space:nowrap;", "Assumptions"),
                tags$td(style="padding:8px 12px;", "Pre-analysis checks required before K-means: clustering tendency (Hopkins statistic), feature correlations, and outlier detection — with explanations of why each matters.")
              ),
              tags$tr(style="background:#f8f9fa;",
                tags$td(style="padding:8px 12px; font-weight:600; white-space:nowrap;", "Cluster Analysis"),
                tags$td(style="padding:8px 12px;",
                  "Three interactive analyses (Age + AI + Flourishing / Depression / Anxiety).
                   For each, select the number of clusters k and explore:", br(),
                  tags$em("Optimal K"), " — Elbow and Silhouette plots to guide k selection;", br(),
                  tags$em("PCA Visualization"), " — 2D projection of cluster assignments;", br(),
                  tags$em("Cluster Profiles"), " — boxplots of each variable by cluster;", br(),
                  tags$em("Summary Table"), " — heatmap of cluster means, means ± SD table, and ANOVA results.")
              ),
              tags$tr(
                tags$td(style="padding:8px 12px; font-weight:600; white-space:nowrap;", "Conclusions"),
                tags$td(style="padding:8px 12px;", "The author's recommended k = 5 solution for Analysis 2, with narrative descriptions of each of the five student profiles and discussion of limitations.")
              )
            )
          )
        ),

        div(class="info-block",
          h3("Author & Acknowledgements"),
          h4("Author"),
          p(strong("Justin Liu"), ", MPHc"),
          p("Master of Public Health Candidate, Cornell University"),
          p(tags$a(href="https://github.com/JustinConfusedCat/JustinL_R",
                   target="_blank", "GitHub Repository")),
          hr(),
          h4("Course Instructor"),
          p(strong("Amandine Gamble"), ", PhD, DVM, MSc"),
          p("Assistant Professor, Department of Public & Ecosystem Health"),
          p("Cornell University College of Veterinary Medicine"),
          hr(),
          h4("Research Project Advisors (NYU)"),
          p(strong("Besa H. Bauta"), ", PhD, MPH, MSW, LMSW"),
          p("Adjunct Assistant Professor, Silver School of Social Work, New York University"),
          br(),
          p(strong("Hyungrok Do"), ", PhD"),
          p("Assistant Professor, Department of Population Health, NYU Grossman School of Medicine")
        ),

        div(class="info-block",
          h3("Data Source"),
          p(strong("Healthy Minds Study (HMS) 2024–25 Student Survey")),
          tags$ul(
            tags$li("Annual web-based survey on mental health among U.S. college and graduate students."),
            tags$li("Since 2007, fielded at 675+ institutions with 935,000+ respondents."),
            tags$li("Random sampling from enrolled student rosters; non-response weights applied."),
            tags$li("This app uses an institutional subset (n = 936).")
          ),
          p(strong("Citation: "),
            em("Healthy Minds Network (2025). Healthy Minds Study Among Colleges and Universities,
                2024-2025 [Data set]. Healthy Minds Network, University of Michigan,
                University of California Los Angeles, Boston University, and Wayne State University.")),
          p(tags$a(href="https://healthymindsnetwork.org/hms/", target="_blank",
                   "healthymindsnetwork.org/hms/"))
        ),

        div(class="info-block",
          h3("AI Disclosure"),
          p("This app was developed with assistance from Claude (Anthropic), an AI coding assistant,
             for code generation and Shiny app structure. All analytic decisions, interpretations,
             and research design are the author's own.")
        ),

        div(class="info-block", style="border-left-color:#adb5bd;",
          h4("Data Note"),
          p("This app uses a subset of the 2024–25 HMS institutional dataset (n = 936).
             Results reflect patterns within this sample and may not generalize to the
             full national HMS population.")
        )
      ))
    ),

    # ── TAB: RESEARCH ───────────────────────────────────────
    tabPanel("Research", br(),
      fluidRow(column(8, offset = 1,

        div(class="info-block",
          h3("Research Question"),
          p("How do ", strong("age"), " and ", strong("generative AI use frequency"),
            " co-occur with mental health outcomes — specifically flourishing, depression,
             and anxiety — among college students? Are there identifiable subgroups of
             students with meaningfully distinct profiles across these dimensions?"),
          p("Specifically, this project asks:"),
          tags$ol(
            tags$li("Do students who use generative AI more frequently differ systematically
                     in their mental health outcomes (flourishing, depression, anxiety)
                     compared to infrequent users?"),
            tags$li("Does age interact with AI use patterns to produce distinct
                     mental health profiles within this college sample?"),
            tags$li("What is the optimal number of subgroups that meaningfully
                     characterizes this population, and what are their defining characteristics?")
          )
        ),

        div(class="info-block",
          h3("Methods"),
          h4("Data & Variables"),
          p("Data are drawn from the 2024–25 Healthy Minds Study (HMS) institutional
             dataset (n = 936, listwise complete cases per analysis). Five variables are used:"),
          tags$table(style="width:100%; border-collapse:collapse; font-size:13px; margin-top:6px;",
            tags$thead(tags$tr(style="background:#457b9d; color:white;",
              tags$th(style="padding:7px 10px; text-align:left;", "Variable"),
              tags$th(style="padding:7px 10px; text-align:left;", "Role"),
              tags$th(style="padding:7px 10px; text-align:left;", "Scale")
            )),
            tags$tbody(
              tags$tr(style="background:#f8f9fa;",
                tags$td(style="padding:7px 10px; font-weight:600;", "age"),
                tags$td(style="padding:7px 10px;", "Clustering feature (all 3 analyses)"),
                tags$td(style="padding:7px 10px;", "Years (continuous)")
              ),
              tags$tr(
                tags$td(style="padding:7px 10px; font-weight:600;", "AI_3 (reversed)"),
                tags$td(style="padding:7px 10px;", "Clustering feature (all 3 analyses)"),
                tags$td(style="padding:7px 10px;", "1 = Never → 6 = Almost constantly")
              ),
              tags$tr(style="background:#f8f9fa;",
                tags$td(style="padding:7px 10px; font-weight:600;", "flourish"),
                tags$td(style="padding:7px 10px;", "Outcome — Analysis 1"),
                tags$td(style="padding:7px 10px;", "Diener Flourishing Scale, 8–56")
              ),
              tags$tr(
                tags$td(style="padding:7px 10px; font-weight:600;", "deprawsc"),
                tags$td(style="padding:7px 10px;", "Outcome — Analysis 2"),
                tags$td(style="padding:7px 10px;", "PHQ-9, 0–27")
              ),
              tags$tr(style="background:#f8f9fa;",
                tags$td(style="padding:7px 10px; font-weight:600;", "anx_score"),
                tags$td(style="padding:7px 10px;", "Outcome — Analysis 3"),
                tags$td(style="padding:7px 10px;", "GAD-7, 0–21")
              )
            )
          ),
          br(),
          h4("Analytical Approach"),
          tags$ol(
            tags$li(strong("Pre-analysis checks: "), "Hopkins statistic (clustering tendency),
                     Pearson correlation matrix (feature independence), Z-score outlier detection."),
            tags$li(strong("Three K-means analyses: "), "Each uses age + AI use as shared
                     features, combined with one outcome variable. All features are z-score
                     standardized prior to clustering (", code("set.seed(123)"), ", nstart = 25)."),
            tags$li(strong("K selection: "), "Elbow plot (WSS) and average Silhouette width
                     are computed for k = 2–6 (WSS up to k = 8). The user selects k interactively."),
            tags$li(strong("Cluster profiling: "), "Cluster means are reported with ± SD and
                     visualized via heatmap (Z-score coloring), boxplots, and PCA scatter plots.
                     Between-cluster differences are assessed with one-way ANOVA, interpreted
                     with attention to effect size rather than p-values alone (see Optimal K tab).")
          )
        ),

        div(class="info-block",
          h3("Hypothesized Student Profiles"),
          p("Prior to examining the cluster results, the author hypothesized — based on
             prior literature on AI adoption, college mental health, and the demographic
             composition of this sample — that ", strong("five meaningful subgroups"),
             " would emerge from Analysis 2 (Age + AI Use + Depression), reflecting a
             crossing of AI use intensity and depression severity:"),
          br(),

          # Brief mention of older group
          div(style="padding:10px 14px; margin-bottom:10px; background:#f8f9fa;
                      border-left:3px solid #adb5bd; border-radius:4px;",
            p(strong("Profile 0 — Non-Traditional Older Students (expected: very small group)"),
              style="margin:0 0 4px 0;"),
            p("A very small number of significantly older respondents (age > 30), likely returning
               students or continuing education enrollees, expected to be demographically
               distinct from the primary undergraduate sample. Due to their anticipated small
               size, this group is acknowledged but not a primary focus of analysis.",
              style="margin:0; font-size:13px; color:#555;")
          ),

          # Four main profiles
          div(style="padding:10px 14px; margin-bottom:10px; background:#e3f2fd;
                      border-left:3px solid #1565C0; border-radius:4px;",
            p(strong("Profile 1 — Low AI Use, High Depression (~25–30% expected)"),
              style="margin:0 0 4px 0;"),
            p("Students who rarely or never use generative AI and who experience elevated
               depression symptoms (PHQ-9 in the moderate-to-severe range). This group
               may lack access to or comfort with digital productivity tools while simultaneously
               bearing disproportionate mental health burden. We expect this group to also
               show elevated anxiety and lower flourishing as profile variables.",
              style="margin:0; font-size:13px; color:#1a237e;")
          ),

          div(style="padding:10px 14px; margin-bottom:10px; background:#e8f5e9;
                      border-left:3px solid #2e7d32; border-radius:4px;",
            p(strong("Profile 2 — Low-to-Moderate AI Use, Low Depression (~35–45% expected)"),
              style="margin:0 0 4px 0;"),
            p("The expected modal or majority group: students who use AI at a moderate,
               functional level (several times per week) and report minimal depression
               symptoms. This profile likely represents students who have adopted AI as a
               productivity tool without signs of mental health distress. We expect this
               group to show the highest flourishing scores among the four primary profiles.",
              style="margin:0; font-size:13px; color:#1b5e20;")
          ),

          div(style="padding:10px 14px; margin-bottom:10px; background:#e3f2fd;
                      border-left:3px solid #0097A7; border-radius:4px;",
            p(strong("Profile 3 — High AI Use, Low Depression (~15–25% expected)"),
              style="margin:0 0 4px 0;"),
            p("Students who are heavy, near-daily generative AI users with low depression
               scores. We hypothesize this group leverages AI primarily as a cognitive
               amplifier or efficiency tool. The co-occurrence of high AI use and good mental
               health suggests AI adoption per se does not predict poorer outcomes — context
               and purpose of use may matter more.",
              style="margin:0; font-size:13px; color:#004D40;")
          ),

          div(style="padding:10px 14px; margin-bottom:10px; background:#fce4ec;
                      border-left:3px solid #C62828; border-radius:4px;",
            p(strong("Profile 4 — High AI Use, High Depression (~10–15% expected)"),
              style="margin:0 0 4px 0;"),
            p("A smaller but clinically significant group: students who use generative AI
               very frequently yet carry a substantial depression burden comparable to
               Profile 1. Two non-mutually-exclusive mechanisms are anticipated: (1) students
               in distress may turn to AI for emotional support or symptom information,
               and (2) maladaptive or excessive AI use may reinforce social withdrawal.
               This group is expected to also show elevated anxiety, making it a priority
               for further investigation.",
              style="margin:0; font-size:13px; color:#7f0000;")
          ),

          br(),
          p(em("Note: These profiles are articulated as hypotheses before examining cluster
                outputs. The Cluster Analysis and Conclusions tabs present the empirical results
                and compare them to these expectations. K-means is exploratory and unsupervised —
                clusters are not guaranteed to map exactly onto these anticipated profiles."),
            style="font-size:12px; color:#6c757d;")
        )
      ))
    ),

    # ── TAB: DATA EXPLORER ──────────────────────────────────
    tabPanel("Data Explorer", br(),
      sidebarLayout(
        sidebarPanel(width = 3,
          h4("Variable Selection"),
          selectInput("eda_var", "Choose a variable:",
            choices = c("Age" = "age", "AI Use (original AI_3)" = "AI_3",
                        "AI Use (reversed)" = "AI_use",
                        "Flourishing (flourish)" = "flourish",
                        "Depression (deprawsc)" = "deprawsc",
                        "Anxiety (anx_score)" = "anx_score")),
          checkboxInput("eda_norm", "Show normal reference curve", value = FALSE),
          hr(),
          h4("Color Palette"),
          selectInput("eda_palette", NULL, choices = palette_choices, selected = "BlueSet"),
          hr(),
          uiOutput("eda_var_info")
        ),
        mainPanel(width = 9,
          fluidRow(
            column(7, plotOutput("eda_hist", height = "300px")),
            column(5, plotOutput("eda_box",  height = "300px"))
          ),
          br(),
          h4("Descriptive Statistics"),
          tableOutput("eda_stats")
        )
      )
    ),

    # ── TAB: ASSUMPTIONS ────────────────────────────────────
    tabPanel("Assumptions", br(),
      sidebarLayout(
        sidebarPanel(width = 3,
          h4("Analysis"),
          radioButtons("assump_analysis", NULL,
            choices = c("Analysis 1: Flourishing" = "1",
                        "Analysis 2: Depression"  = "2",
                        "Analysis 3: Anxiety"     = "3")),
          hr(),
          p(em("Select an analysis to view checks for its 3-variable feature set."),
            style = "font-size:12px; color:#6c757d;")
        ),
        mainPanel(width = 9,
          # ── Explanation panel ──
          div(class = "assump-box",
            h4("Why These Checks?"),
            p("K-means clustering has three key assumptions that should be verified before
               running the analysis:"),
            tags$ol(
              tags$li(
                strong("Clustering Tendency (Hopkins Statistic): "),
                "K-means assumes the data has meaningful natural cluster structure.
                 If data points are uniformly distributed, any clustering will produce
                 arbitrary groupings with no real meaning. The Hopkins statistic (H)
                 tests this: ", strong("H > 0.5"), " indicates clustering tendency;
                 ", strong("H > 0.75"), " indicates strong structure. A value near 0.5
                 suggests random data — clustering would not be meaningful."
              ),
              tags$li(
                strong("Feature Correlations: "),
                "Highly correlated features can distort K-means by overweighting
                 one underlying dimension. For example, if age and AI use are strongly
                 correlated (r > 0.7), the cluster space may collapse into a single axis,
                 producing clusters that essentially reflect only one variable.
                 The correlation matrix reveals whether features carry independent
                 information."
              ),
              tags$li(
                strong("Outlier Detection (Z-score |z| > 3): "),
                "K-means is highly sensitive to extreme values because it minimizes
                 squared distances — outliers can heavily pull centroids away from the
                 true cluster centers and create singleton clusters (n = 1–5) that
                 represent only the outliers. This dataset includes a small number of
                 respondents aged 40+, which warrants attention when interpreting results."
              )
            )
          ),
          hr(),
          h4("Hopkins Statistic — Clustering Tendency"),
          wellPanel(
            verbatimTextOutput("hopkins_out"),
            p("Values > 0.5 suggest clustering tendency; > 0.75 indicates strong structure.",
              style = "font-size:12px; color:#6c757d; margin:0;")
          ),
          h4("Feature Correlation Matrix"),
          plotOutput("corr_plot", height = "360px"),
          h4("Outlier Overview (|Z-score| > 3)"),
          tableOutput("outlier_table")
        )
      )
    ),

    # ── TAB: CLUSTER ANALYSIS ────────────────────────────────
    tabPanel("Cluster Analysis", br(),
      sidebarLayout(
        sidebarPanel(width = 3,
          h4("Analysis"),
          radioButtons("ca_analysis", NULL,
            choices = c("Analysis 1: Age + AI + Flourishing" = "1",
                        "Analysis 2: Age + AI + Depression"  = "2",
                        "Analysis 3: Age + AI + Anxiety"     = "3")),
          hr(),
          h4("Number of Clusters"),
          sliderInput("k_val", "k:", min = 2, max = 6, value = 3, step = 1),
          hr(),
          h4("Color Palette"),
          selectInput("ca_palette", NULL, choices = palette_choices, selected = "BlueSet"),
          hr(),
          uiOutput("ca_sample_info")
        ),
        mainPanel(width = 9,
          tabsetPanel(
            tabPanel("Optimal K", br(),
              fluidRow(
                column(6, plotOutput("elbow_plot",      height = "300px")),
                column(6, plotOutput("silhouette_plot", height = "300px"))
              ),
              br(),
              div(class = "assump-box",
                h4("How to Read These Plots and Choose k"),
                fluidRow(
                  column(6,
                    strong("Elbow Plot (Within-Cluster Sum of Squares, WSS)"),
                    tags$ul(
                      tags$li("WSS measures total within-cluster variance — the smaller, the tighter the clusters."),
                      tags$li("WSS always decreases as k increases (more clusters always fit better), so the goal is to find the point of ",
                               strong("diminishing returns"), "."),
                      tags$li("Look for the \"elbow\" — the k where the curve bends and flattens. Adding more clusters beyond that point gives little additional compression."),
                      tags$li("The red dashed line marks your currently selected k.")
                    )
                  ),
                  column(6,
                    strong("Silhouette Plot (Average Silhouette Width)"),
                    tags$ul(
                      tags$li("The silhouette score measures how similar each point is to its own cluster compared to the nearest neighboring cluster."),
                      tags$li("Values range from −1 to 1: higher = better-separated clusters."),
                      tags$li("General benchmarks: > 0.7 = strong structure; 0.5–0.7 = reasonable; 0.25–0.5 = weak; < 0.25 = no meaningful structure."),
                      tags$li("Choose k that ", strong("maximizes"), " the silhouette score.")
                    )
                  )
                ),
                hr(),
                strong("Choosing the Best k — Decision Rule"),
                tags$ol(
                  tags$li(strong("Agree:"), " If both plots point to the same k, that is your answer."),
                  tags$li(strong("Disagree:"), " Prefer the k with the highest silhouette score, but verify it also shows a visible elbow."),
                  tags$li(strong("Interpretability:"), " Always cross-check with the Cluster Profiles tab. A statistically optimal k is only useful if the clusters are substantively meaningful."),
                  tags$li(strong("Avoid k = 2 by default:"), " Silhouette often favors k = 2 because two groups are maximally separated; evaluate whether a coarser split is scientifically useful.")
                ),
                hr(),
                p(tags$b("Note on significance testing: "),
                  "K-means is an exploratory, not inferential, method. No formal significance test is required for the clustering itself.
                   The ANOVA results in the Summary Table tab show whether clusters differ on each variable, but these p-values should be
                   interpreted cautiously: because K-means was designed to maximize between-cluster differences, post-hoc ANOVA
                   p-values are artificially inflated (circular reasoning). Focus on ",
                  tags$b("effect sizes"), " (how large the differences are) and ",
                  tags$b("substantive interpretability"), " (whether the cluster profiles make real-world sense).",
                  style = "margin-bottom:0;")
              )
            ),
            tabPanel("PCA Visualization", br(),
              plotOutput("pca_plot", height = "420px"),
              p("PCA reduces the 3 standardized clustering features to 2 dimensions.
                 Each point is one student, colored by cluster assignment.",
                style = "color:#6c757d; font-size:12px; margin-top:8px;")
            ),
            tabPanel("Cluster Profiles", br(),
              h4("Clustering Features by Cluster"),
              plotOutput("profile_plot",  height = "340px"),
              br(),
              h4("Profile Variables (non-clustering outcomes)"),
              plotOutput("profile_plot2", height = "260px")
            ),
            tabPanel("Summary Table", br(),
              h4("Cluster Profile Heatmap"),
              p("Tile color = Z-score relative to other clusters (blue = above average,
                 red = below average). Numbers show actual cluster means.",
                style = "color:#6c757d; font-size:12px;"),
              plotOutput("heatmap_plot", height = "380px"),
              br(),
              h4("Cluster Means ± SD"),
              DTOutput("summary_dt"),
              br(),
              h4("ANOVA — Between-Cluster Differences"),
              verbatimTextOutput("anova_out")
            )
          )
        )
      )
    ),

    # ── TAB: CONCLUSIONS ────────────────────────────────────
    tabPanel("Conclusions", br(),
      fluidRow(column(9, offset = 1,

        div(class = "info-block",
          h3("Author's Recommendation: k = 5, Analysis 2"),
          p("After exploring all three analyses across a range of cluster solutions (k = 2–6),
             the author recommends ", strong("k = 5 applied to Analysis 2 (Age + AI Use + Depression)"),
             " as the most interpretively meaningful segmentation of this sample."),
          p("The silhouette and elbow plots support k values between 3 and 5 for this analysis.
             At k = 5, the solution reveals a coherent ",
             strong("2 × 2 structure"), " — crossing AI use intensity (high vs. low) with
             depression severity (high vs. low) — plus a small but demographically distinct
             group of older, non-traditional students. This structure is theoretically
             grounded, actionable, and sufficiently differentiated to inform public health
             intervention design."),
          p("Note: K-means cluster numbering is arbitrary and may vary across runs.
             The profiles below are described by their characteristics, not their numeric labels.",
            style = "font-size:12px; color:#6c757d;")
        ),

        h3("The Five Student Profiles"),
        p("Based on Analysis 2 (Age + AI Use + Depression) with k = 5. Profiles are derived
           from within-sample means; causal inferences cannot be made from cross-sectional data.",
          style = "color:#6c757d; font-size:12px; margin-bottom:16px;"),

        # Static cluster summary table
        div(class = "info-block",
          h4("Cluster Overview"),
          tags$table(
            style = "width:100%; border-collapse:collapse; font-size:13px;",
            tags$thead(
              tags$tr(style = "background:#1d3557; color:white;",
                tags$th(style="padding:8px;", "Profile"),
                tags$th(style="padding:8px;", "n"),
                tags$th(style="padding:8px;", "Mean Age"),
                tags$th(style="padding:8px;", "AI_3 (1=constantly, 6=never)"),
                tags$th(style="padding:8px;", "PHQ-9 Depression"),
                tags$th(style="padding:8px;", "AI Use Level"),
                tags$th(style="padding:8px;", "Depression Level")
              )
            ),
            tags$tbody(
              tags$tr(style="background:#f8f9fa;",
                tags$td(style="padding:8px; font-weight:600;", "Older Non-Traditional"),
                tags$td(style="padding:8px;", "~7"), tags$td(style="padding:8px;", "42.4"),
                tags$td(style="padding:8px;", "5.1"), tags$td(style="padding:8px;", "6.7"),
                tags$td(style="padding:8px; color:#888;", "Low"),
                tags$td(style="padding:8px; color:#888;", "Low-Moderate")
              ),
              tags$tr(
                tags$td(style="padding:8px; font-weight:600;", "Low AI, High Depression"),
                tags$td(style="padding:8px;", "~240"), tags$td(style="padding:8px;", "20.3"),
                tags$td(style="padding:8px;", "5.1"), tags$td(style="padding:8px;", "14.8"),
                tags$td(style="padding:8px; color:#888;", "Low"),
                tags$td(style="padding:8px; color:#C62828; font-weight:600;", "High")
              ),
              tags$tr(style="background:#f8f9fa;",
                tags$td(style="padding:8px; font-weight:600;", "Low AI, Low Depression"),
                tags$td(style="padding:8px;", "~362"), tags$td(style="padding:8px;", "19.9"),
                tags$td(style="padding:8px;", "5.0"), tags$td(style="padding:8px;", "5.0"),
                tags$td(style="padding:8px; color:#888;", "Low–Moderate"),
                tags$td(style="padding:8px; color:#2E7D32; font-weight:600;", "Low")
              ),
              tags$tr(
                tags$td(style="padding:8px; font-weight:600;", "High AI, Low Depression"),
                tags$td(style="padding:8px;", "~191"), tags$td(style="padding:8px;", "19.8"),
                tags$td(style="padding:8px;", "2.3"), tags$td(style="padding:8px;", "4.2"),
                tags$td(style="padding:8px; color:#1565C0; font-weight:600;", "High"),
                tags$td(style="padding:8px; color:#2E7D32; font-weight:600;", "Low")
              ),
              tags$tr(style="background:#f8f9fa;",
                tags$td(style="padding:8px; font-weight:600;", "High AI, High Depression"),
                tags$td(style="padding:8px;", "~99"), tags$td(style="padding:8px;", "19.8"),
                tags$td(style="padding:8px;", "2.2"), tags$td(style="padding:8px;", "14.7"),
                tags$td(style="padding:8px; color:#1565C0; font-weight:600;", "High"),
                tags$td(style="padding:8px; color:#C62828; font-weight:600;", "High")
              )
            )
          )
        ),

        # Cluster narratives
        div(class = "cluster-card",
          h4("Cluster 1 — Non-Traditional Older Students (n ≈ 7, < 1%)"),
          tags$span("Mean age: 42.4 | AI use: infrequent | Depression: low-moderate",
                    style = "font-size:12px; color:#6c757d;"),
          br(), br(),
          p("This very small group is demographically distinct from the rest of the sample —
             likely returning students, graduate students, or continuing education enrollees.
             They use AI infrequently and report moderate depression scores. Due to the
             extremely small cluster size (n ≈ 7), results for this group should be interpreted
             with caution and are not the focus of this analysis.")
        ),

        div(class = "cluster-card", style = "border-left: 4px solid #C62828;",
          h4("Cluster 2 — Low AI Use, High Depression (n ≈ 240, ~27%)"),
          tags$span("Mean age: 20.3 | AI use: less than weekly | PHQ-9 ≈ 15 (moderate-to-severe)",
                    style = "font-size:12px; color:#6c757d;"),
          br(), br(),
          p("This group of typical college-age students uses generative AI infrequently
             (less than weekly on average) yet carries the highest depression burden in the
             sample, with PHQ-9 scores in the moderate-to-severe range. Their low AI engagement
             does not appear to be protecting their mental health; they may lack access to,
             or comfort with, digital tools that peers use for productivity and support.
             This group likely represents a priority population for mental health outreach
             and screening programs.")
        ),

        div(class = "cluster-card", style = "border-left: 4px solid #2E7D32;",
          h4("Cluster 3 — Low AI Use, Low Depression (n ≈ 362, ~40%) — Largest Group"),
          tags$span("Mean age: 19.9 | AI use: several times per week | PHQ-9 ≈ 5 (minimal)",
                    style = "font-size:12px; color:#6c757d;"),
          br(), br(),
          p("The largest cluster, representing roughly 40% of the sample, consists of young
             students who use AI moderately (several times per week) and report minimal
             depression symptoms. This group likely represents the 'modal' college student —
             adopting AI tools at a functional level for coursework and daily tasks without
             signs of mental health distress. Their well-being may partly reflect selective
             adoption: using AI as a productivity tool rather than as a coping mechanism.")
        ),

        div(class = "cluster-card", style = "border-left: 4px solid #1565C0;",
          h4("Cluster 4 — High AI Use, Low Depression (n ≈ 191, ~21%)"),
          tags$span("Mean age: 19.8 | AI use: several times per day | PHQ-9 ≈ 4 (minimal)",
                    style = "font-size:12px; color:#6c757d;"),
          br(), br(),
          p("These students are heavy, near-daily generative AI users who simultaneously
             report very low depression scores — the lowest in the sample. Their profile
             suggests that intensive AI engagement can co-exist with strong mental wellbeing.
             This group may be leveraging AI as a cognitive and productivity amplifier,
             experiencing its efficiency benefits without adverse psychological effects.
             They warrant further study to understand what distinguishes them from the
             equally high AI-use group that does show elevated depression (Cluster 5).")
        ),

        div(class = "cluster-card", style = "border-left: 4px solid #6A1B9A;",
          h4("Cluster 5 — High AI Use, High Depression (n ≈ 99, ~11%)"),
          tags$span("Mean age: 19.8 | AI use: several times per day | PHQ-9 ≈ 15 (moderate-to-severe)",
                    style = "font-size:12px; color:#6c757d;"),
          br(), br(),
          p("The smallest young cluster, yet one of the most clinically significant.
             These students use AI as frequently as Cluster 4 but carry a depression
             burden comparable to Cluster 2. Two plausible — and non-mutually-exclusive —
             explanations exist: (1) students in mental health distress may turn to AI tools
             (such as ChatGPT) for emotional support or information about symptoms,
             or (2) excessive or maladaptive AI use may itself contribute to social
             isolation and mental health decline. Cross-sectional data cannot
             distinguish these pathways, but this group represents an important
             target for future longitudinal investigation.")
        ),

        div(class = "info-block", style = "border-left-color:#adb5bd; margin-top:16px;",
          h4("Limitations & Future Directions"),
          tags$ul(
            tags$li("All data are cross-sectional; no causal claims can be made."),
            tags$li("K-means assumes spherical clusters of similar size — a limitation
                     when one cluster has n = 7."),
            tags$li("AI_3 is an ordinal variable treated as quasi-continuous."),
            tags$li("Institutional sampling may limit generalizability."),
            tags$li("Future work should examine cluster stability across bootstrap samples
                     and replicate findings with longitudinal data.")
          )
        )
      ))
    )
  )
)

# ── SERVER ───────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── EDA ───────────────────────────────────────────────────
  eda_data <- reactive(hms[[input$eda_var]])

  output$eda_var_info <- renderUI({
    m <- var_meta[[input$eda_var]]
    div(hr(), h4("Variable Info"),
        p(strong(m$label)), p(m$desc),
        p(strong("Range: "), m$range),
        p(strong("Missing: "), m$na),
        p(strong("Type: "), m$type))
  })

  output$eda_hist <- renderPlot({
    var  <- input$eda_var
    vals <- na.omit(eda_data())
    m    <- var_meta[[var]]
    col1 <- get_palette(input$eda_palette, 1)

    if (m$type == "ordinal") {
      lbs <- if (var == "AI_3") {
        c("1"="Almost\nconstantly","2"="Several\ntimes/day","3"="~Once/day",
          "4"="Several\ntimes/wk","5"="Less\noften","6"="Never")
      } else {
        c("1"="Never","2"="Less\noften","3"="Several\ntimes/wk",
          "4"="~Once/day","5"="Several\ntimes/day","6"="Almost\nconstantly")
      }
      ggplot(data.frame(x = factor(vals)), aes(x = x)) +
        geom_bar(fill = col1, color = "white", width = 0.7) +
        scale_x_discrete(labels = lbs) +
        labs(title = paste("Distribution of", m$label), x = m$label, y = "Count") +
        theme_hms()
    } else {
      p <- ggplot(data.frame(x = vals), aes(x = x)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30,
                       fill = col1, color = "white", alpha = 0.85) +
        geom_density(color = "#1d3557", linewidth = 1) +
        labs(title = paste("Distribution of", m$label), x = m$label, y = "Density") +
        theme_hms()
      if (input$eda_norm)
        p <- p + stat_function(fun = dnorm,
                               args = list(mean = mean(vals), sd = sd(vals)),
                               color = "#E63946", linetype = "dashed", linewidth = 1)
      p
    }
  })

  output$eda_box <- renderPlot({
    var  <- input$eda_var
    vals <- na.omit(eda_data())
    m    <- var_meta[[var]]
    col1 <- get_palette(input$eda_palette, 1)
    z    <- abs(scale(vals)[, 1])
    ggplot(data.frame(y = vals, x = "", out = z > 3), aes(x = x, y = y)) +
      geom_boxplot(fill = col1, color = "#333", alpha = 0.6,
                   outlier.shape = NA, width = 0.4) +
      geom_jitter(aes(color = out), width = 0.15, alpha = 0.4, size = 1.5) +
      scale_color_manual(values = c("FALSE" = col1, "TRUE" = "#E63946"),
                         labels = c("Normal", "Outlier (|z|>3)"), name = "") +
      labs(title = paste("Boxplot of", m$label), x = "", y = m$label) +
      theme_hms() + theme(axis.text.x = element_blank())
  })

  output$eda_stats <- renderTable({
    vals <- na.omit(eda_data())
    data.frame(N = length(vals), Mean = round(mean(vals),2), SD = round(sd(vals),2),
               Median = round(median(vals),2), Min = round(min(vals),2),
               Max = round(max(vals),2), Skewness = round(skewness(vals),3),
               Missing = sum(is.na(eda_data())))
  })

  # ── ASSUMPTIONS ───────────────────────────────────────────
  assump_cfg <- reactive(analysis_configs[[input$assump_analysis]])
  assump_mat <- reactive(scale(assump_cfg()$data[, assump_cfg()$features]))

  output$hopkins_out <- renderPrint({
    mat <- assump_mat(); cfg <- assump_cfg()
    h   <- get_clust_tendency(mat, n = min(150, nrow(mat) - 1), seed = 123)
    cat("Analysis:", cfg$label, "\n")
    cat("Features:", paste(cfg$features, collapse = ", "), "\n")
    cat("n (complete cases):", nrow(mat), "\n\n")
    cat(sprintf("Hopkins Statistic: %.4f\n", h$hopkins_stat))
    interp <- if (h$hopkins_stat > 0.75) "Strong clustering tendency (H > 0.75) [PASS]"
              else if (h$hopkins_stat > 0.5) "Moderate clustering tendency (H > 0.5) [PASS]"
              else "Weak clustering tendency (H <= 0.5) -- clusters may not be meaningful [WARN]"
    cat("Interpretation:", interp, "\n")
  })

  output$corr_plot <- renderPlot({
    cfg    <- assump_cfg()
    df_cor <- cfg$data[, cfg$features]
    colnames(df_cor) <- cfg$feat_lab
    r      <- cor(df_cor, use = "complete.obs")
    ggcorrplot(r, method = "circle", type = "lower", lab = TRUE, lab_size = 4,
               colors = c("#E63946", "white", "#457b9d"),
               title = paste("Correlation Matrix —", cfg$label),
               ggtheme = theme_hms())
  })

  output$outlier_table <- renderTable({
    cfg <- assump_cfg()
    df  <- cfg$data[, cfg$features]
    out <- map_dfr(cfg$features, function(v) {
      z <- abs(scale(df[[v]])[, 1]); idx <- which(z > 3)
      if (!length(idx)) return(NULL)
      data.frame(Variable = v, N_outliers = length(idx),
                 Min_value = round(min(df[[v]][idx]), 2),
                 Max_value = round(max(df[[v]][idx]), 2))
    })
    if (!nrow(out)) data.frame(Result = "No outliers detected (|z| ≤ 3 for all features)") else out
  })

  # ── CLUSTER ANALYSIS ──────────────────────────────────────
  ca_cfg    <- reactive(analysis_configs[[input$ca_analysis]])
  ca_scaled <- reactive(scale(ca_cfg()$data[, ca_cfg()$features]))
  ca_km     <- reactive(run_kmeans(ca_scaled(), input$k_val))

  ca_df <- reactive({
    df <- ca_cfg()$data
    df$cluster <- factor(ca_km()$cluster,
                         labels = paste("Cluster", seq_len(input$k_val)))
    df
  })

  pal <- reactive(get_palette(input$ca_palette, input$k_val))

  output$ca_sample_info <- renderUI({
    cfg <- ca_cfg()
    div(hr(),
        p(strong("n = "), nrow(cfg$data), class = "n-badge"),
        p(em(paste("Complete cases:", paste(cfg$features, collapse = ", "))),
          style = "font-size:11px; color:#6c757d;"))
  })

  output$elbow_plot <- renderPlot({
    wss <- compute_wss(ca_scaled())
    df  <- data.frame(k = 2:8, wss = wss)
    ggplot(df, aes(x = k, y = wss)) +
      geom_line(color = "#457b9d", linewidth = 1) +
      geom_point(size = 3, color = "#1d3557") +
      geom_vline(xintercept = input$k_val, linetype = "dashed", color = "#E63946", alpha = 0.7) +
      labs(title = "Elbow Plot (WSS)", subtitle = "Look for the 'elbow' inflection",
           x = "Number of Clusters (k)", y = "Total Within-Cluster SS") +
      scale_x_continuous(breaks = 2:8) + theme_hms()
  })

  output$silhouette_plot <- renderPlot({
    sil <- compute_sil(ca_scaled())
    df  <- data.frame(k = 2:6, sil = sil)
    ggplot(df, aes(x = k, y = sil)) +
      geom_line(color = "#2A9D8F", linewidth = 1) +
      geom_point(size = 3, color = "#1d3557") +
      geom_vline(xintercept = min(input$k_val, 6), linetype = "dashed",
                 color = "#E63946", alpha = 0.7) +
      labs(title = "Silhouette Width", subtitle = "Higher = better cluster separation",
           x = "Number of Clusters (k)", y = "Average Silhouette Width") +
      scale_x_continuous(breaks = 2:6) + theme_hms()
  })

  output$pca_plot <- renderPlot({
    cfg <- ca_cfg(); km <- ca_km(); mat <- ca_scaled()
    colnames(mat) <- cfg$feat_lab
    fviz_cluster(km, data = mat, palette = pal(),
                 ellipse.type = "convex", ggtheme = theme_hms(),
                 main = paste("PCA Cluster Plot —", cfg$label)) +
      theme(legend.position = "bottom")
  })

  output$profile_plot <- renderPlot({
    cfg  <- ca_cfg(); df <- ca_df()
    labs <- setNames(cfg$feat_lab, cfg$features)
    long <- df %>%
      select(cluster, all_of(cfg$features)) %>%
      pivot_longer(-cluster, names_to = "variable", values_to = "value") %>%
      mutate(variable = factor(variable, levels = cfg$features,
                               labels = unname(labs[cfg$features])))
    ggplot(long, aes(x = cluster, y = value, fill = cluster)) +
      geom_boxplot(alpha = 0.75, outlier.size = 1, outlier.alpha = 0.4) +
      facet_wrap(~ variable, scales = "free_y", nrow = 1) +
      scale_fill_manual(values = pal()) +
      labs(title = "Clustering Features by Cluster", x = NULL, y = "Value") +
      theme_hms() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1))
  })

  output$profile_plot2 <- renderPlot({
    cfg <- ca_cfg(); df <- ca_df()
    pv  <- cfg$profile_vars[cfg$profile_vars %in% colnames(df)]
    pl  <- cfg$profile_lab[cfg$profile_vars %in% colnames(df)]
    if (!length(pv)) { plot.new(); text(0.5,0.5,"No profile variables."); return() }
    long <- df %>% select(cluster, all_of(pv)) %>% drop_na() %>%
      pivot_longer(-cluster, names_to = "variable", values_to = "value") %>%
      mutate(variable = factor(variable, levels = pv, labels = pl))
    ggplot(long, aes(x = cluster, y = value, fill = cluster)) +
      geom_boxplot(alpha = 0.75, outlier.size = 1, outlier.alpha = 0.4) +
      facet_wrap(~ variable, scales = "free_y", nrow = 1) +
      scale_fill_manual(values = pal()) +
      labs(title = "Other Mental Health Outcomes by Cluster (profile variables)",
           subtitle = "Not used in clustering — shown for context", x = NULL, y = "Value") +
      theme_hms() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1))
  })

  # ── Heatmap ───────────────────────────────────────────────
  output$heatmap_plot <- renderPlot({
    cfg  <- ca_cfg(); df <- ca_df(); k <- input$k_val
    svars <- cfg$features; slabs <- cfg$feat_lab

    # Compute per-cluster means + n
    n_tbl  <- df %>% count(cluster)
    m_wide <- df %>%
      group_by(cluster) %>%
      summarise(across(all_of(svars), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
      left_join(n_tbl, by = "cluster") %>%
      arrange(age)   # sort by mean age (ascending → oldest at top when we rev y)

    # Z-score each feature across clusters
    m_z <- m_wide %>%
      mutate(across(all_of(svars), ~as.numeric(scale(.x))))

    # Pivot both to long and join
    long_m <- m_wide %>%
      pivot_longer(all_of(svars), names_to = "var", values_to = "mean_val") %>%
      mutate(var_lab = factor(var, levels = svars, labels = slabs))

    long_z <- m_z %>%
      pivot_longer(all_of(svars), names_to = "var", values_to = "z_val") %>%
      mutate(var_lab = factor(var, levels = svars, labels = slabs))

    joined <- left_join(
      long_m %>% select(cluster, n, var_lab, mean_val),
      long_z %>% select(cluster, var_lab, z_val),
      by = c("cluster", "var_lab")
    ) %>%
      mutate(clust_lab = paste0(cluster, " (n=", n, ")"))

    # Order y so oldest cluster appears at top.
    # ggplot y-axis: factor level 1 = bottom, last level = top.
    # So arrange ascending (youngest first = bottom), oldest last = top.
    age_order <- joined %>%
      filter(as.character(var_lab) == slabs[1]) %>%
      arrange(mean_val) %>%
      pull(clust_lab)
    joined$clust_lab <- factor(joined$clust_lab, levels = age_order)

    ggplot(joined, aes(x = var_lab, y = clust_lab, fill = z_val)) +
      geom_tile(color = "white", linewidth = 1.2) +
      geom_text(aes(label = round(mean_val, 2)), size = 4.5, fontface = "bold") +
      scale_fill_gradient2(low = "#C62828", mid = "white", high = "#1565C0",
                           midpoint = 0, name = "Z-score",
                           limits = c(-2, 2), oob = scales::squish) +
      labs(title = "Cluster profiles (means by cluster)",
           subtitle = "Sorted by average age; tile color = standardized deviation; number = actual mean",
           x = NULL, y = NULL) +
      theme_hms() +
      theme(panel.grid = element_blank(),
            axis.text  = element_text(size = 12),
            axis.text.x = element_text(angle = 15, hjust = 1))
  })

  # ── Summary table ─────────────────────────────────────────
  output$summary_dt <- renderDT({
    cfg <- ca_cfg(); df <- ca_df()
    av  <- c(cfg$features, cfg$profile_vars)
    al  <- c(cfg$feat_lab, cfg$profile_lab)
    av  <- av[av %in% colnames(df)]; al <- al[seq_along(av)]

    tbl <- df %>%
      group_by(cluster) %>%
      summarise(n = n(),
                across(all_of(av),
                       list(mean = ~round(mean(.x, na.rm=TRUE),2),
                            sd   = ~round(sd(.x,   na.rm=TRUE),2)),
                       .names = "{.col}__{.fn}"),
                .groups = "drop") %>%
      mutate(across(ends_with("__mean"), function(m) {
        sd_col <- sub("__mean$","__sd", cur_column())
        paste0(m, " ± ", get(sd_col))
      })) %>%
      select(cluster, n, ends_with("__mean")) %>%
      rename_with(~{ b <- sub("__mean$","",.x); i <- match(b,av)
                     ifelse(!is.na(i), al[i], .x) }, ends_with("__mean"))

    datatable(tbl, options = list(dom="t", paging=FALSE), rownames=FALSE,
              class="display compact")
  })

  output$anova_out <- renderPrint({
    cfg <- ca_cfg(); df <- ca_df()
    av  <- c(cfg$features, cfg$profile_vars); al <- c(cfg$feat_lab, cfg$profile_lab)
    av  <- av[av %in% colnames(df)]; al <- al[seq_along(av)]
    cat("One-way ANOVA: variable ~ cluster\n")
    cat(strrep("-", 55), "\n")
    for (i in seq_along(av)) {
      tryCatch({
        s <- summary(aov(as.formula(paste(av[i],"~ cluster")), data=df))[[1]]
        f <- round(s[["F value"]][1],3); p <- s[["Pr(>F)"]][1]
        pf <- if(p < .001) "< 0.001" else round(p,4)
        cat(sprintf("%-35s F = %7.3f  p %s\n", al[i], f, pf))
      }, error = function(e) cat(al[i], ": error\n"))
    }
  })
}

shinyApp(ui, server)
