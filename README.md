# ConfusedCatR
Repository for testing my Git/GitHub setup

# AI/Tech Use and Mental Health Patterns (HMS 2024–2025)

## Project title and brief description
This project explores how self-reported **generative AI/technology use** relates to **mental health and well-being** outcomes in a student sample. Using cleaned HMS 2024–2025 survey variables, the analysis examines associations and identifies potential **behavioral/mental-health profiles** via descriptive statistics, correlation analyses, and exploratory clustering.

Due to the big file size, datasets are not included in this Github repository. Access through:

https://healthymindsnetwork.org/research/data-for-researchers/

A subetted dataset HMS24_Small is included in data folder, use as example.


## Author(s) and affiliations
**Justin Jialang Liu**  
Cornell University — MPH (Public Health)

## Contact information
- GitHub: https://github.com/JustinConfusedCat  
- Email: jl4673@cornell.edu

## Research question/objectives
**Primary objective:**  
Assess whether **AI/tech use frequency** is associated with **mental health severity and well-being**.

**Operational questions:**
1. How are **age**, **AI_3 (AI use frequency)**, and mental health measures (**deprawsc**, **anx_score**) correlated?
2. Do participants cluster into distinct profiles based on **age + AI/tech use + mental health/well-being**?
3. How do cluster characteristics differ across outcomes (e.g., flourish vs depression vs anxiety)?

## Data source and description
**Data source:** Harvard/Healthy Minds Study (HMS) 2024–2025 public-use dataset (institution characteristics file), imported as `HMS24`.  
**Key variables used (cleaned):**
- `age` (years)
- `AI_3` (intentional generative AI use frequency in the past week; ordinal 1–6)
- `deprawsc` (PHQ-9 total score, 0–27; NA if any PHQ-9 item missing)
- `anx_score` (GAD-7 total score, 0–21; created during cleaning)
- `flourish` (well-being scale; created during cleaning; filtered to plausible range)

**Pre-processing notes:**  
Analyses use a cleaned subset `HMS24_sub` with valid codes for AI use and plausible ranges for mental health/well-being measures. Missingness is assessed and handled via complete-case selection when required (e.g., clustering).

## AI tool disclosure
AI tools (e.g., ChatGPT) were used to support parts of the workflow, including clarifying analysis approaches, drafting/debugging R code, and improving documentation. All code and outputs were reviewed and manually adjusted by the author to ensure correctness and reproducibility.

## References/citations
- Healthy Minds Network. *Healthy Minds Study (HMS).* (Dataset documentation and codebook for HMS 2024–2025).  
- Kroenke, K., Spitzer, R. L., & Williams, J. B. W. (2001). The PHQ-9: Validity of a brief depression severity measure. *Journal of General Internal Medicine*, 16(9), 606–613.  
- Spitzer, R. L., Kroenke, K., Williams, J. B. W., & Löwe, B. (2006). A brief measure for assessing generalized anxiety disorder: The GAD-7. *Archives of Internal Medicine*, 166(10), 1092–1097.

> Note: This repository contains analysis code and derived outputs only. Please consult the data provider’s terms of use for data access and sharing restrictions.
