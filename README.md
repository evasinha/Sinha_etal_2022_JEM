_your zenodo badge here_

# Sinha-etal\_2022_JEM

**Implication of imposing fertilizer limitations on energy, agriculture, and land systems**

Eva Sinha1<sup>1\*</sup>, Katherine V. Calvin<sup>2</sup>, Page G. Kyle<sup>2</sup>, Mohamad I. Hejazi2<sup>2</sup>, Stephanie T. Waldhoff<sup>2</sup>, Maoyi Huang<sup>1</sup>, Srishti Vishwakarma<sup>3</sup>, and Xin Zhang<sup>3</sup>

<sup>1 </sup> Pacific Northwest National Laboratory, Richland, WA, 99352, USA.  
<sup>2 </sup> Joint Global Change Research Institute, College Park, MD, 20740, USA.  
<sup>3 </sup> University of Maryland Center for Environmental Science, Frostburg, MD, 21532, USA

\* corresponding author:  eva.sinha@pnnl.gov

## Abstract
Since the 1950's, global fertilizer usage has increased by more than 800% resulting in detrimental impacts to the environment. The projected increase in crop production due to increasing demands for food, feed, biofuel, and other uses, may further increase fertilizer usage. Studies have examined achieving agricultural intensification in environmentally sustainable ways, however, they have not focused on the whole-system economic aspects of changes in fertilizer usage over the long term. We utilize the Global Change Analysis Model (GCAM) to explore the impact of reducing global fertilizer usage on land use change, agricultural commodity price and production, energy production, and greenhouse gas emissions. We find that constrained fertilizer availability results in reduced global cropland area, particularly land used for bioenergy production, and expanded forested area. These results are driven by price impacts which lead to shifts in agricultural production between commodity types, regions, and technologies, and which lead to decreased agricultural commodity demands.


## Journal reference
Sinha, E., Calvin, K. V., Kyle, P. G., Hejazi, M. I., Waldhoff, S. T., Huang, M., et al. (2022). Implication of imposing fertilizer limitations on energy, agriculture, and land systems. Journal of Environmental Management, 305, 114391. https://doi.org/10.1016/j.jenvman.2021.114391


## Contributing modeling software
| Model | Version | Repository Link | DOI |
|-------|---------|-----------------|-----|
| GCAM | version 5.2 | `https://github.com/JGCRI/gcam-core/releases/tag/gcam-v5.2` | `http://doi.org/10.5281/zenodo.3528353` |

## Reproduce my experiment
1. Install [GCAM 5.2](#https://github.com/JGCRI/gcam-core/releases/tag/gcam-v5.2) required to conduct the experiement.
2. Run the following scripts in the `workflow` directory to re-create this experiment:

| Script Name | Description | How to Run |
| --- | --- | --- |
| `run-scenarios-gcam.sh` | Script to run GCAM with the provided configuration files. This script and the four configuration files should be placed in `gcam-core/exe` folder | `./run-scenarios-gcam.sh` |
| `fert_constrain_create_proj_file.R` | Script for creating project data by reading outputs of GCAM runs that are stored in GCAM database  | `Rscript fert_constrain_create_proj_file.R` |


## Reproduce my figures
Use the following scripts found in the `workflow` directory to reproduce our figures and compare your outputs to those from the publication.

| Script Name | Description | How to Run |
| --- | --- | --- |
| `Figure1.R` | Script to generate Figure 1 | `Rscript Figure1.R` |
| `Figure2.R` | Script to generate Figure 2 | `Rscript Figure2.R` |
| `Figure3.R` | Script to generate Figure 3 | `Rscript Figure3.R` |
| `Figure4.R` | Script to generate Figure 4 | `Rscript Figure4.R` |
| `Figure5.R` | Script to generate Figure 5 | `Rscript Figure5.R` |
| `Figure7.R` | Script to generate Figure 7 | `Rscript Figure7.R` |
| `FigureS1.R` | Script to generate Figure S1 | `Rscript FigureS1.R` |
| `FigureS2.R` | Script to generate Figure S2 | `Rscript FigureS2.R` |
| `FigureS3.R` | Script to generate Figure S3 | `Rscript FigureS3.R` |
| `FigureS5.R` | Script to generate Figure S5 | `Rscript FigureS5.R` |
| `FigureS6.R` | Script to generate Figure S6 | `Rscript FigureS6.R` |
| `FigureS10.R` | Script to generate Figure S10 | `Rscript FigureS10.R` |
| `FigureS11.R` | Script to generate Figure S11 | `Rscript FigureS11.R` |

