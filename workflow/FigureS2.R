# Author - Eva Sinha, Pacific Northwest National Lab, eva.sinha@pnnl.gov

library(rgcam)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(geofacet)

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

# ----- Query export by crop commodity
export.crop.commodity <- getQuery(prj, query = 'Crop commodity exports (query all regions from USA region)')

# Update region from which trade occurs
export.crop.commodity <- export.crop.commodity %>%
  mutate(region = sapply(str_split(technology, ' '), function(x){ifelse(length(x)==3, x[1], paste(x[1], x[2], sep=' '))}))

# ----- Query supply by crop commodity
crop.supply.data.table <- getQuery(prj, query = 'Domestic supply by crop commodity (domestic and imports)')

# Add long label for scenarios
export.crop.commodity  <- add_long_label_scenarios(export.crop.commodity)
crop.supply.data.table <- add_long_label_scenarios(crop.supply.data.table)

# Drop unused levels
export.crop.commodity$scenario  <- droplevels(export.crop.commodity$scenario)
crop.supply.data.table$scenario   <- droplevels(crop.supply.data.table$scenario)

# Select regions and scenarios for plotting
select_region    <- c('China','India','USA')
# Selected scenarios for plotting
select_scenarios <- c('Reference', 
                      'Global constraint (15%) on fertilizer')

# Select for specified regions and future years only
export.crop.commodity <- export.crop.commodity %>% 
  filter(scenario %in% select_scenarios,
         region %in% select_region,
         year >= 2015)

# Select for specified regions and future years only
crop.supply.data.table <- crop.supply.data.table %>% 
  filter(scenario %in% select_scenarios,
         region %in% select_region,
         year >= 2015)

# Create plot data tibble
plot.data <- bind_rows((export.crop.commodity %>% 
                         filter(sector=='traded rice') %>%
                         select(Units, scenario, region, year, value) %>%
                          mutate(Desc = 'Regional rice export [Mt]')),
                       (crop.supply.data.table %>% 
                          filter(technology=='imported rice') %>%
                          select(Units, scenario, region, year, value) %>%
                          mutate(Desc = 'Regional rice import [Mt]')))

# Define path to output file
out.folder <- '../figures/'
out.fname  <- 'FigureS2.pdf'
out.fname  <- paste(out.folder, out.fname, sep='')
# Delete existing file
unlink(out.fname)
# Start pdf device driver for saving plots
pdf(out.fname, height=8.5, width=14)

# Make line plot with facet grid
p1 <- plot_line_facet_grid(filter(plot.data, 
                                  scenario %in% c('Reference', 'Global constraint (15%) on fertilizer')), 
                           x_col       = 'year',
                           y_col       = 'value', 
                           group_col   = 'scenario',
                           color_col   = 'scenario',
                           linetype    = NULL,
                           facet_row   = 'Desc', 
                           facet_col   = 'region', 
                           facet_scale = 'free_y', 
                           leg_nrow    = 1, 
                           y_lab       = NULL, 
                           plot_title  = NULL)
  
print(p1)

dev.off()