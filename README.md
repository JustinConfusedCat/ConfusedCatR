# AI/Technology Use and Mental Health Among College Students
### Healthy Minds Study 2024–25 — Analysis Project

> **Course:** VTPEH 6270 — Cornell University, Spring 2026  
> **Author:** Justin Jialang Liu, MPHc — Cornell University  
> **Contact:** jl4673@cornell.edu · [GitHub @JustinConfusedCat](https://github.com/JustinConfusedCat)

---

## Project Overview

This project explores whether and how self-reported **generative AI use frequency** is associated with **mental health outcomes** — specifically depression (PHQ-9), anxiety (GAD-7), and psychological flourishing — in a sample of U.S. college students from the 2024–25 Healthy Minds Study (HMS).

The analysis combines:
- Descriptive and inferential statistics (Spearman correlation, Kruskal-Wallis + Dunn post-hoc, multiple linear regression)
- Exploratory K-means cluster analysis identifying five student subgroups defined by age, AI use, and one mental health indicator
- An interactive Shiny dashboard for visualization and exploration

---

## Live Shiny App

**[https://justinliucu.shinyapps.io/HMS_ShinyApp/](https://justinliucu.shinyapps.io/HMS_ShinyApp/)**

The app includes five sections:
| Tab | Contents |
|-----|----------|
| **About** | Author, data source, AI disclosure, page navigation guide |
| **Research** | Research question, methods, and hypothesized student profiles |
| **Data Explorer** | Interactive variable distribution plots (histogram, boxplot, descriptive stats) |
| **Assumptions** | K-means pre-analysis checks: Hopkins statistic, correlation matrix, outlier detection |
| **Cluster Analysis** | Three K-means analyses (Age + AI + Flourishing/Depression/Anxiety) with interactive k selection, PCA plot, cluster profiles, and summary heatmap |
| **Conclusions** | Author's recommended k = 5 solution for Analysis 2 with narrative profiles of each student subgroup |

---

## Repository Structure

```
ConfusedCatR/
│
├── README.md
│
├── HMS Data/                         # Data files and documentation
│   ├── HMS_Small.csv                 # Analytic subset (n = 936; age, AI_3,
│   │                                 #   deprawsc, anx_score, flourish)
│   ├── Codebook/                     # HMS codebooks (2020–21 through 2024–25)
│   └── Questionnaire/                # HMS survey questionnaires (2020–25)
│
├── ShinyApp/                         # Final Shiny application
│   ├── app.R                         # Complete app source code
│   ├── HMS_Small.csv
│   ├── www/                          # Static assets (Cornell, NYU, HMS logos)
│   └── rsconnect/                    # Deployment config (shinyapps.io)
│
├── Final Report/                     # Final written report
│   ├── Final_Report_AI_MentalHealth.Rmd   # R Markdown source
│   ├── Final_Report_AI_MentalHealth.pdf   # Rendered PDF
│   ├── HMS_Small.csv
│   └── references.bib
│
└── Check Points Archive/             # Course checkpoint submissions
    ├── Check Point #02/              # Dataset selection (BRFSS → HMS pivot)
    ├── Check Point #03/              # Dataset exploration & EDA
    ├── Check Point #04/              # Data simulation
    ├── Check Point #05/              # GitHub repository setup
    ├── Check Point #06/              # Statistical analyses (CP06 version)
    └── Check Point #07/              # Shiny app (earlier checkpoint version)
```

---

## Research Question

**Primary:** Is intentional generative AI use frequency (AI_3) associated with depressive symptom severity (PHQ-9; `deprawsc`) among university students, and does this association persist after adjusting for age?

**Secondary:** Are the same associations present for anxiety (GAD-7; `anx_score`) and psychological flourishing (Diener scale; `flourish`)?

---

## Data

**Source:** [Healthy Minds Study (HMS) 2024–25](https://healthymindsnetwork.org/research/data-for-researchers/), an annual web-based survey of U.S. college students conducted by the Healthy Minds Network.

> Due to data sharing restrictions, the full HMS dataset is not included in this repository. Access the data through the HMS Network's researcher data portal. The file `HMS_Small.csv` is a pre-cleaned institutional subset (n = 936) included for reproducibility of this project's analyses.

**Key variables:**

| Variable | Type | Description |
|----------|------|-------------|
| `age` | Continuous | Student age in years |
| `AI_3` | Ordinal (1–6) | Intentional generative AI use frequency (1 = Almost constantly, 6 = Never) |
| `deprawsc` | Count (0–27) | PHQ-9 total depression score |
| `anx_score` | Count (0–21) | GAD-7 total anxiety score |
| `flourish` | Discrete (8–56) | Diener Flourishing Scale sum |

**Citation:**
> Healthy Minds Network (2025). *Healthy Minds Study Among Colleges and Universities, 2024-2025* [Data set]. Healthy Minds Network, University of Michigan, University of California Los Angeles, Boston University, and Wayne State University.

---

## Methods Summary

**Statistical analyses (Final Report):**
1. Shapiro-Wilk normality test + visual checks → non-parametric primary tests
2. Spearman rank correlation (AI_3 vs. each mental health outcome; age vs. outcomes)
3. Kruskal-Wallis test with η² effect size
4. Dunn's post-hoc pairwise comparisons (Benjamini-Hochberg correction)
5. Multiple linear regression (deprawsc ~ AI_3 + age) with 95% CI and residual diagnostics

**Cluster analysis (Shiny App):**
- Three separate K-means analyses (age + AI_use + one mental health outcome)
- K selection via Elbow (WSS) and Silhouette plots (k = 2–8)
- Pre-analysis checks: Hopkins statistic, correlation matrix, outlier detection
- User-interactive k slider and color palette options

---

## Key Findings

- Students who use generative AI less frequently reported **higher PHQ-9 depression scores** (Spearman ρ = 0.13, p < 0.001), **higher anxiety**, and **lower flourishing** — consistent across all three outcomes.
- This association persisted after adjusting for age (β = 0.39 per one-level decrease in AI frequency, 95% CI [0.14, 0.65], p < 0.001).
- However, AI use frequency explained only ~1% of variance in depression scores (R² = 0.010), underscoring the **modest practical magnitude** of this relationship.
- K-means clustering (k = 5, Analysis 2) identified four primary student subgroups crossing AI use intensity and depression severity, plus a small group of older non-traditional students.

---

## Acknowledgements

**Course Instructor:** Amandine Gamble, PhD, DVM, MSc — Assistant Professor, Department of Public & Ecosystem Health, Cornell University College of Veterinary Medicine

**Research Project Advisors (NYU):**
- Besa H. Bauta, PhD, MPH, MSW, LMSW — Adjunct Assistant Professor, Silver School of Social Work, NYU
- Hyungrok Do, PhD — Assistant Professor, Department of Population Health, NYU Grossman School of Medicine

---

## AI Tool Disclosure

AI tools (Claude by Anthropic) were used to support parts of this workflow, including generating initial code drafts, structuring report sections, and building the Shiny app. All code, statistical interpretations, and conclusions were reviewed and verified by the author. Analytical decisions and research design are the author's own.

---

## References

- Auerbach, R. P., et al. (2018). Mental disorder comorbidity and suicidal thoughts and behaviors in the WHO world mental health surveys. *International Journal of Methods in Psychiatric Research*, 28(2), e1752.
- Benjamini, Y., & Hochberg, Y. (1995). Controlling the false discovery rate. *Journal of the Royal Statistical Society: Series B*, 57(1), 289–300.
- Diener, E., et al. (2010). New well-being measures. *Social Indicators Research*, 97(2), 143–156.
- Dunn, O. J. (1964). Multiple comparisons using rank sums. *Technometrics*, 6(3), 241–252.
- Hubbard, K. R., et al. (2024). Generative artificial intelligence in higher education. *Policy Futures in Education*, 22(3), 349–366.
- Kroenke, K., Spitzer, R. L., & Williams, J. B. W. (2001). The PHQ-9. *Journal of General Internal Medicine*, 16(9), 606–613.
- Lee, P., Bubeck, S., & Petro, J. (2023). Benefits, limits, and risks of GPT-4. *New England Journal of Medicine*, 388(13), 1233–1239.
- Spitzer, R. L., et al. (2006). A brief measure for assessing generalized anxiety disorder: The GAD-7. *Archives of Internal Medicine*, 166(10), 1092–1097.
- Tomczak, M., & Tomczak, E. (2014). The need to report effect size estimates revisited. *Trends in Sport Sciences*, 1(21), 19–25.
- Twenge, J. M., et al. (2018). Increases in depressive symptoms and links to increased new media screen time. *Clinical Psychological Science*, 6(1), 3–17.
