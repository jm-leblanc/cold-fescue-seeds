# Data and Code for _Seed storage conditions shape germination outcomes: Differing responses in two closely-related fescue species_ (LeBlanc and McFarlane 2025)

This repository contains all the code and data associated with the manuscript _Seed storage conditions shape germination outcomes: Differing responses in two closely-related fescue species_, which is currently under review for publication. The files here are structured to reproduce the results, figures, and tables in the supplementary materials of the manuscript.

## Files in this Repository

### Code Files

- **2020_TtE-curves-and-figure.R**  
  This script generates Figure 1 in the supplementary material. It uses the data in `Cleaned_2020-germ-per-day_Wide.csv`.

- **Facet-plots_Proper-model-fit.R**  
  This script generates panels 1A and 1C of Figure 1 in the main text.

- **Log-Reg_Forest-plots-visualization.R**  
  This script generates panels 1B and 1D of Figure 1 in the main text, as well as Tables 3 and 4 in the supplementary material.

- **Time-to-Event-curves_Basic-code_GitHub.R**  
  This script generates Figure 2 in the supplementary material.

### Data Files

- **Cleaned_2020-germ-per-day_Wide.csv**  
  Contains count data for a 2020 comparative germination experiment between _F. hallii_ and _F. campestris_

- **Discrep-adjusted_Cold-seeds-counts.csv**  
  The full dataset of our 2024 germination experiment, containing seed counts, treatment groups, counts of viable and moldy seeds, etc. This data is summarized in Table 2 of the supplementary material.

- **F-camp-rows_Long-format.csv**  
  2024 Germination count data for *Festuca campestris*, used in the scripts `Time-to-Event-curves_Basic-code_GitHub.R` and `Facet-plots_Proper-model-fit.R`.

- **F-hallii-rows_Long-format.csv**  
  2024 Germination count data for *Festuca hallii*, used in the scripts `Time-to-Event-curves_Basic-code_GitHub.R` and `Facet-plots_Proper-model-fit.R`.

- **FC-cleaned-counts_For-Log-Reg.csv**  
  2024 Count data for *Festuca campestris*, used in the script `Log-Reg_Forest-plots-visualization.R`.

- **FH-cleaned-counts_For-Log-Reg.csv**  
  2024 Count data for *Festuca hallii*, used in the script `Log-Reg_Forest-plots-visualization.R`.
