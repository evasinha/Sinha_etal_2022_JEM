# Author - Eva Sinha, Pacific Northwest National Lab, eva.sinha@pnnl.gov

library(rgcam)
library(ggpubr) # ggarrange
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(readxl)
library(RColorBrewer)
library(colorRamps)
library(geofacet)
library(gridExtra)

source('read_summarise_queries.R')
source('plot_vector.R')
source('color_pal_labels.R')

#________________________________________________________
# Load a previously generated project data file
# prj.name       <- 'fert_constrain.dat'
# prj.name       <- 'fert_io_coeff_SA.dat'
prj.name       <- 'fert_const_updated_IO_coeff.dat'
prj            <- loadProject(prj.name)

# List all scenarios
print(listScenarios(prj))

# Query fertilizer consumption and land allocation by agricultural technologies
# to estimate fertilizer application rate [Units: kgN/ha]
fert.rate.table  <- estimate_fert_application_rate(prj,
                                                   query_fert_consump = 'fertilizer consumption by ag tech',
                                                   query_land         = 'detailed land allocation')


# Sum fertilizer usage and area by scenariom region, sector, abd year)
fert.rate.table  <- fert.rate.table %>%
  group_by(scenario, region, sector, year) %>%
  summarise(fert_MtN = sum(fert_MtN),
            area_thous_km2 = sum(area_thous_km2)) %>%
  mutate(fert_kgN_ha = fert_MtN*10^9 / (area_thous_km2 * 1000*100))

# Drop unused levels
fert.rate.table$scenario      <- droplevels(fert.rate.table$scenario)

# Add long label for sector and region
fert.rate.table$sector <- param_labeller('variable', fert.rate.table$sector)
fert.rate.table$region <- param_labeller('variable', fert.rate.table$region)

# Selected scenarios for plotting
select_scenarios <- c('Reference', 
                      'Global constraint (15%) on fertilizer') 

# Define path to output file
out.folder <- '../figures/'
out.fname  <- 'FigureS10.pdf'
out.fname  <- paste(out.folder, out.fname, sep='')
# Delete existing file
unlink(out.fname)
# Start pdf device driver for saving plots
pdf(out.fname, height=8.5, width=11)

fert.rate.table <- fert.rate.table %>%
  filter(year >= 2015,
         scenario %in% select_scenarios)

plot_change(filter(fert.rate.table, region == 'USA'),
            xaxis.var  = 'sector',
            value.var  = 'fert_kgN_ha',
            select_yr  = 2100, 
            facet_ncol = 4,
            y_lab      = expression(paste(Fertilizer~use~intensity~'[',kgN~ha^-1,']')), 
            plot_title = 'USA - change in fertilizer use intensity from Reference scenario for 2100',
            leg_keyword = 'fertilizer use intensity')

plot_change(filter(fert.rate.table, region == 'China'),
            xaxis.var  = 'sector',
            value.var  = 'fert_kgN_ha',
            select_yr  = 2100, 
            facet_ncol = 4,
            y_lab      = expression(paste(Fertilizer~use~intensity~'[',kgN~ha^-1,']')), 
            plot_title = 'China - change in fertilizer use intensity from Reference scenario for 2100',
            leg_keyword = 'fertilizer use intensity')

plot_change(filter(fert.rate.table, region == 'India'),
            xaxis.var  = 'sector',
            value.var  = 'fert_kgN_ha',
            select_yr  = 2100, 
            facet_ncol = 4,
            y_lab      = expression(paste(Fertilizer~use~intensity~'[',kgN~ha^-1,']')), 
            plot_title = 'India - change in fertilizer use intensity from Reference scenario for 2100',
            leg_keyword = 'fertilizer use intensity')

plot_change(filter(fert.rate.table, sector == 'Biomass'),
            xaxis.var  = 'region',
            value.var  = 'fert_kgN_ha',
            select_yr  = 2100, 
            facet_ncol = 4,
            y_lab      = expression(paste(Fertilizer~use~intensity~'[',kgN~ha^-1,']')), 
            plot_title = 'Biomass - change in fertilizer use intensity from Reference scenario for 2100',
            leg_keyword = 'fertilizer use intensity')

plot_change_facet(filter(fert.rate.table, sector %in% c('Corn', 'Oil crop','Sugar crop')), 
                  xaxis.var  = 'region',
                  value.var  = 'fert_kgN_ha',
                  facet.var  = 'sector',
                  select_yr  = 2100, 
                  facet_ncol = 4,
                  y_lab      = expression(paste(Fertilizer~use~intensity~'[',kgN~ha^-1,']')), 
                  plot_title = 'Change in fertilizer use intensity from Reference scenario for 2100',
                  leg_keyword = 'fertilizer use intensity')

plot_change(filter(fert.rate.table, sector == 'Corn'),
            xaxis.var  = 'region',
            value.var  = 'fert_kgN_ha',
            select_yr  = 2100, 
            facet_ncol = 4,
            y_lab      = expression(paste(Fertilizer~use~intensity~'[',kgN~ha^-1,']')), 
            plot_title = 'Corn - change in fertilizer use intensity from Reference scenario for 2100',
            leg_keyword = 'fertilizer use intensity')

plot_change(filter(fert.rate.table, sector == 'Rice'),
            xaxis.var  = 'region',
            value.var  = 'fert_kgN_ha',
            select_yr  = 2100, 
            facet_ncol = 4,
            y_lab      = expression(paste(Fertilizer~use~intensity~'[',kgN~ha^-1,']')), 
            plot_title = 'Rice - change in fertilizer use intensity from Reference scenario for 2100',
            leg_keyword = 'fertilizer use intensity')


plot_change(filter(fert.rate.table, sector == 'Sugar crop'),
            xaxis.var  = 'region',
            value.var  = 'fert_kgN_ha',
            select_yr  = 2100, 
            facet_ncol = 4,
            y_lab      = expression(paste(Fertilizer~use~intensity~'[',kgN~ha^-1,']')), 
            plot_title = 'Sugar crop - change in fertilizer use intensity from Reference scenario for 2100',
            leg_keyword = 'fertilizer use intensity')


plot_change(filter(fert.rate.table, sector == 'Wheat'),
            xaxis.var  = 'region',
            value.var  = 'fert_kgN_ha',
            select_yr  = 2100, 
            facet_ncol = 4,
            y_lab      = expression(paste(Fertilizer~use~intensity~'[',kgN~ha^-1,']')), 
            plot_title = 'Wheat - change in fertilizer use intensity from Reference scenario for 2100',
            leg_keyword = 'fertilizer use intensity')

dev.off()
