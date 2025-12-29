# Heat Beyond Percentiles: Exploring Risk of Preterm Birth in Santiago, Chile (1992-2020) Using a Novel Metric :sunny: :baby:

[![DOI](https://zenodo.org/badge/855782998.svg)](https://doi.org/10.5281/zenodo.17316761)
![GitHub Repo stars](https://img.shields.io/github/stars/ClimChange-NewbornHealth/HeatWave_PretermBirth_Chile)
![GitHub watchers](https://img.shields.io/github/watchers/ClimChange-NewbornHealth/HeatWave_PretermBirth_Chile)
![GitHub forks](https://img.shields.io/github/forks/ClimChange-NewbornHealth/HeatWave_PretermBirth_Chile)
![GitHub commit activity](https://img.shields.io/github/commit-activity/t/ClimChange-NewbornHealth/HeatWave_PretermBirth_Chile)
![GitHub contributors](https://img.shields.io/github/contributors/ClimChange-NewbornHealth/HeatWave_PretermBirth_Chile)
![GitHub last commit](https://img.shields.io/github/last-commit/ClimChange-NewbornHealth/HeatWave_PretermBirth_Chile)
![GitHub language count](https://img.shields.io/github/languages/count/ClimChange-NewbornHealth/HeatWave_PretermBirth_Chile)
![GitHub top language](https://img.shields.io/github/languages/top/ClimChange-NewbornHealth/HeatWave_PretermBirth_Chile)
![GitHub License](https://img.shields.io/github/license/ClimChange-NewbornHealth/HeatWave_PretermBirth_Chile)
![GitHub repo file or directory count](https://img.shields.io/github/directory-file-count/ClimChange-NewbornHealth/HeatWave_PretermBirth_Chile)
![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/ClimChange-NewbornHealth/HeatWave_PretermBirth_Chile)

## :moneybag: Funding

**Fondecyt Nº 11240322**: Climate change and urban health: how air pollution, temperature, and city structure relate to preterm birth

## :busts_in_silhouette: Research Team

:mailbox_with_mail: **Estela Blanco** (<estela.blanco@uc.cl>) - **Principal Investigator**

:mailbox_with_mail: **José Daniel Conejeros** (<jdconejeros@uc.cl>) - **Research Assistant / Repository Manager**

**Research Collaborators**: 💪 Álvaro González-Reyes, 💪 Pamela Smith, 💪 Paola Rubilar, & 💪 Pablo Sarricolea

## :pushpin: Publication

Blanco, E., Conejeros, J.D., González-Reyes, Á. et al. Heat beyond percentiles: exploring preterm birth risks in Santiago, Chile (1991–2019). Int Arch Occup Environ Health 99, 5 (2026). <https://doi.org/10.1007/s00420-025-02196-x>

---

## :dart: Project Overview

### Background

Climate change is increasing the frequency, intensity, and duration of heat waves globally, with potential adverse effects on maternal and neonatal health. Preterm birth (delivery before 37 weeks of gestation) is a leading cause of neonatal mortality and long-term health complications. While several studies have examined the relationship between heat exposure and preterm birth, most research has been conducted in high-income countries with limited evidence from Latin America, particularly from regions with diverse climate zones.

### Objective

To evaluate the association between exposure to heat waves during pregnancy and the risk of preterm birth among singleton births in the Metropolitan Region of Santiago, Chile, from 1992 to 2020.

### Methods

We conducted a population-based retrospective cohort study using:
- **Study Population**: Singleton births in the Metropolitan Region of Santiago, Chile (1992-2020)
- **Sample Size**: 2,760,141 births after exclusion criteria
- **Exposure**: Multiple heat wave definitions including:
  - Absolute thresholds (30°C, 31°C, 32°C, 33°C, 34°C)
  - Percentile-based thresholds (P90, P95, P99 of daily maximum temperature)
  - Excess Heat Factor (EHF) based on daily mean temperature (TAD)
  - Duration: 2, 3, and 4 consecutive days
- **Outcome**: Preterm birth (<37 weeks), with subcategories:
  - Very preterm (28-32 weeks)
  - Moderately preterm (32-33 weeks)
  - Late preterm (34-37 weeks)
- **Statistical Analysis**: Cox proportional hazards models with gestational age as the time scale
- **Temperature Data**: Daily maximum and minimum temperature from CR2MET v2.5 gridded dataset (1980-2021)
- **Exposure Windows**: Last week and last month of gestation during summer months (November-March)
- **Covariates**: Sex, maternal and paternal age, education, employment status, year of birth, and socioeconomic vulnerability index

### Key Findings

Heat wave exposure during the last week and last month of pregnancy was associated with increased risk of preterm birth. The association varied by heat wave definition, duration, and preterm birth subtype. The most consistent associations were observed for:
- Heat waves defined by percentile-based thresholds (P90, P95, P99)
- Longer duration heat waves (3-4 consecutive days)
- Late preterm births (34-37 weeks)

---

## ![R](https://skillicons.dev/icons?i=r) Code Structure

### Data Processing Scripts

- `0.1 Functions.R` - Custom functions for descriptive statistics and variable construction
- `0.2 Settings.R` - Package installation, loading, and global settings
- `1.0 Births_process_data.R` - Birth data cleaning and preparation
- `1.1 Births_weeks_process_data.R` - Gestational week expansion and cohort definition
- `2.0 HW_process_data.R` - Heat wave detection and classification
- `2.1 Histogram_HW_percentile.R` - Descriptive analysis of heat wave percentiles
- `3.0 Join_HW_BW.R` - Merge heat wave and birth data

### Analytical Scripts

- `4.0 Descriptive_analysis_lw_lm.R` - Descriptive statistics for last week/month exposure
- `4.1 Descriptive HW.R` - Heat wave descriptive analysis and visualization
- `5.0 Logit Models.R` - Logistic regression models
- `6.0 Cox models LW.R` - Cox models for last week exposure
- `6.1 Cox models LM.R` - Cox models for last month exposure
- `6.2 Figure cox model lw-lm.R` - Visualization of Cox model results
- `6.3 ICC COX models random intercept.R` - Intraclass correlation analysis
- `6.4 Cox models by com.R` - Cox models stratified by municipality
- `7.0 AFT models LW.R` - Accelerated failure time models
- `8.0 Cox models LW Celcius.R` - Cox models with Celsius thresholds (last week)
- `8.1 Cox models LM Celcius.R` - Cox models with Celsius thresholds (last month)
- `9.0 Cox models LW_WLB.R` - Cox models excluding Lo Barnechea (last week)
- `9.1 Cox models LM_WLB.R` - Cox models excluding Lo Barnechea (last month)
- `9.2 Figure cox model lw-lm_WLB.R` - Visualization excluding Lo Barnechea

---

## :chart_with_upwards_trend: Principal Findings

### Figure 1. Heat Wave Exposure in Metropolitan Santiago (1992-2020)

**Panel A**: Mean number of heat waves across time by different definitions
**Panel B**: Spatial distribution of heat wave frequency by municipality

![](/Output/Descriptives/HW_trends_by_correlation.png)

*Note*: Heat waves defined as: 30°C for 3 consecutive days (HW 30C 3D), 90th percentile for 3 days (HW P90 3D), 95th percentile for 3 days (HW P95 3D), 99th percentile for 3 days (HW P99 3D), and Excess Heat Factor for 3 days (HW EHF 3D). Data from 33 municipalities in Metropolitan Santiago.

### Figure 2. Preterm Birth Trends in Metropolitan Santiago (1992-2020)

![](/Output/Descriptives/Preterm_trends_summer.png)

*Note*: Prevalence (per 100 births) of different preterm birth categories over time. Analysis restricted to births occurring during summer months (November-March). Sample includes singleton births ≥28 weeks gestation in Metropolitan Santiago (N=2,760,141).

### Figure 3. Association Between Heat Waves and Preterm Birth Risk

**Last Week Exposure (Panel A)** and **Last Month Exposure (Panel B)**

![](/Output/Models/PTB_COX_LM_LW.png)

*Note*: Hazard Ratios (HR) and 95% confidence intervals for preterm birth (<37 weeks) according to different heat wave definitions and durations. Models adjusted for sex, maternal and paternal age, education, employment status, year of birth, and socioeconomic vulnerability. Reference category: no heat wave exposure.

---

## :file_folder: Data Availability

### Input Data Sources

1. **Birth Records**: Chilean Ministry of Health vital statistics (1992-2020)
   - Location: `Data/Input/Nacimientos/`
   - Variables: Gestational age, birth weight, parental characteristics, municipality of residence

2. **Temperature Data**: CR2MET v2.5 gridded daily temperature dataset
   - Location: `Data/Input/HW/`
   - Source: Center for Climate and Resilience Research (CR2), Universidad de Chile
   - Variables: Daily maximum and minimum temperature (1980-2021)
   - Spatial resolution: Municipality level

3. **Socioeconomic Vulnerability Index (SOVI)**
   - Location: `Data/Input/SOVI/`
   - Source: Chilean Ministry of Social Development

4. **Normalized Difference Vegetation Index (NDVI)**
   - Location: `Data/Input/NDVI/`
   - Seasons: Summer and winter NDVI (2002-2022)

### Processed Datasets

Main analytical datasets are available in `Data/Output/`:
- `births_1992_2020.RData` - Cleaned birth records
- `births_1992_2020_weeks.RData` - Expanded gestational weeks
- `births_1992_2020_last_week_hw.RData` - Last week exposure data
- `births_1992_2020_last_month_hw.RData` - Last month exposure data
- `hw_data_1980_2021.RData` - Processed heat wave data

**Note**: Due to data privacy regulations, individual-level birth records cannot be publicly shared. Aggregated results and code are available in this repository.

---

## :computer: Reproducibility

### System Requirements

- R version ≥4.0.0
- Required packages (automatically installed via `0.2 Settings.R`):
  - Data manipulation: `tidyverse`, `data.table`, `janitor`
  - Spatial analysis: `chilemapas`, `sf`, `rnaturalearth`
  - Survival analysis: `survival`, `flexsurv`, `survminer`, `coxme`
  - Visualization: `ggplot2`, `patchwork`, `ggpubr`, `RColorBrewer`
  - Temperature analysis: `zoo`, `dlnm`
  - And more (see `0.2 Settings.R` for complete list)

### Running the Analysis

The analysis pipeline follows this sequence:

1. **Data Processing** (Run in order):
   ```r
   source("Code/0.1 Functions.R")
   source("Code/0.2 Settings.R")
   source("Code/1.0 Births_process_data.R")
   source("Code/1.1 Births_weeks_process_data.R")
   source("Code/2.0 HW_process_data.R")
   source("Code/3.0 Join_HW_BW.R")
   ```

2. **Descriptive Analysis**:
   ```r
   source("Code/4.0 Descriptive_analysis_lw_lm.R")
   source("Code/4.1 Descriptive HW.R")
   source("Code/2.1 Histogram_HW_percentile.R")
   ```

3. **Statistical Models**:
   ```r
   source("Code/6.0 Cox models LW.R")      # Last week exposure
   source("Code/6.1 Cox models LM.R")      # Last month exposure
   source("Code/6.2 Figure cox model lw-lm.R")
   ```

4. **Sensitivity Analyses**:
   ```r
   source("Code/7.0 AFT models LW.R")      # Alternative model specification
   source("Code/9.0 Cox models LW_WLB.R")  # Excluding Lo Barnechea
   source("Code/6.4 Cox models by com.R")  # Municipality-specific models
   ```

### Notes on Computation Time

- **Data expansion** (`1.1 Births_weeks_process_data.R`): ~1.5 hours (creates 109M+ rows)
- **Heat wave detection** (`2.0 HW_process_data.R`): ~10 minutes
- **Data merging** (`3.0 Join_HW_BW.R`): ~6-10 minutes per dataset
- **Cox models** (`6.0`, `6.1`): ~20-30 minutes each (parallelized)
- **Total pipeline**: ~3-4 hours on a modern desktop computer


---

## :open_book: Codebook

### Birth Variables

- `id`: Unique birth identifier
- `com`: Municipality code
- `name_com`: Municipality name
- `weeks`: Gestational age in weeks
- `date_nac`: Date of birth
- `sex`: Infant sex (Boy/Girl)
- `tbw`: Birth weight in grams
- `birth_preterm`: Preterm birth indicator (<37 weeks)
- `birth_very_preterm`: Very preterm (28-32 weeks)
- `birth_moderately_preterm`: Moderately preterm (32-33 weeks)
- `birth_late_preterm`: Late preterm (34-37 weeks)

### Maternal Variables

- `age_group_mom`: Maternal age groups (≤20, 20-29, 30-39, 40-49, ≥50)
- `educ_group_mom`: Maternal education (None, Primary, Secondary, College)
- `job_group_mom`: Maternal employment (Not working, Employed)

### Paternal Variables

- `age_group_dad`: Paternal age groups (≤20, 20-29, 30-39, 40-49, ≥50, Unknown)
- `educ_group_dad`: Paternal education (None, Primary, Secondary, College, Unknown)
- `job_group_dad`: Paternal employment (Not working, Employed, Unknown)

### Heat Wave Variables

- `HW_[temp]_[duration]d_bin`: Binary indicator of heat wave exposure (0/1)
- `HW_[temp]_[duration]d_count`: Count of heat wave days during exposure window
- `HW_EHF_TAD_[duration]d_bin`: Binary EHF-based heat wave indicator
- `vulnerability`: Socioeconomic vulnerability index (Low, Medium-low, Medium-high)

Where `[temp]` = 30C, 31C, 32C, 33C, 34C, p90, p95, p99, and `[duration]` = 2, 3, or 4 days.

---

## :microscope: Methods Detail

### Heat Wave Detection Algorithm

Heat waves were identified using multiple approaches:

1. **Absolute Temperature Threshold**:
   - Tmax > threshold (30°C, 31°C, etc.) for N consecutive days

2. **Percentile-Based Threshold**:
   - Tmax > P90/P95/P99 (calculated by municipality, 1980-2021) for N consecutive days

3. **Excess Heat Factor (EHF)**:
   - EHIsigi: Compares current temperature to 95th percentile
   - EHIaccli: Compares current temperature to recent 30-day average
   - EHF = EHIsigi × max(1, EHIaccli)
   - Based on daily mean temperature (TAD = mean of Tmax and next-day Tmin)

### Exclusion Criteria

Births were excluded if:
- Maternal age <12 or >50 years
- Gestational age <28 weeks
- Multiple births (twins, triplets, etc.)
- Birth weight outside plausible ranges for gestational age (Alexander et al., 1996 criteria)
- Gestational period started before 1991-01-01
- Birth occurred after 2020-02-25 (to avoid cohort bias)

---

## :file_cabinet: Repository Structure

```
HeatWave_PretermBirth_Chile/
├── Code/                           # Analysis scripts
│   ├── 0.1 Functions.R
│   ├── 0.2 Settings.R
│   ├── 1.0 - 2.1 ...              # Data processing
│   ├── 3.0 ...                    # Data merging
│   ├── 4.0 - 4.1 ...              # Descriptive analysis
│   ├── 5.0 - 9.2 ...              # Statistical models
│   └── old/                       # Archived scripts
├── Code_DMC/                       # Alternative analysis (DMC definition)
├── Data/
│   ├── Input/                     # Raw data (not publicly available)
│   └── Output/                    # Processed datasets
├── Output/
│   ├── Descriptives/              # Descriptive statistics and plots
│   ├── Models/                    # Model results and plots
│   └── Presentation/              # Lab presentations
├── Paper/                          # Manuscript and supplementary materials
│   ├── HW_PTB_Manuscript_v12072025.docx
│   └── HW_PTB_Supplementary_Material_12072025.docx
└── README.md                       # This file
```

---

## :warning: Important Notes

### Data Privacy

Individual-level birth records are confidential and cannot be shared publicly due to Chilean data protection regulations. Researchers interested in accessing the data should contact the Chilean Ministry of Health.

### Temperature Data

Daily temperature data from CR2MET v2.5 is publicly available at:
- [Center for Climate and Resilience Research (CR2)](http://www.cr2.cl/datos-productos-grillados/)

### Citation

If you use this code or methodology, please cite:

> Blanco E, González-Reyes A, Smith P, Rubilar P, Sarricolea P, Conejeros JD. Heat Beyond Percentiles: Exploring Risk of Preterm Birth in Santiago, Chile (1992-2020) Using a Novel Metric. *Under Review*. 2025.

---

## :email: Contact

For questions about the code or methodology:
- **Estela Blanco**: <estela.blanco@uc.cl>
- **José Daniel Conejeros**: <jdconejeros@uc.cl>

For data access inquiries:
- Chilean Ministry of Health: [https://www.minsal.cl](https://www.minsal.cl)

---

## :page_facing_up: License

This project is licensed under the terms specified in the LICENSE file.

---

## :handshake: Acknowledgments

This research was supported by Fondecyt de Iniciación en Investigación Nº 11240322. We thank the Chilean Ministry of Health for access to birth records and the Center for Climate and Resilience Research (CR2) for providing temperature data.

Temperature data source: 
- CR2MET v2.5: Center for Climate and Resilience Research, Universidad de Chile

