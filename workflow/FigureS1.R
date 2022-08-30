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

# ----- Query fertilizer consumption
fert.cons.data.table <- estimate_value_query(prj, query='aggregated fertilizer consumption')

# ----- Query fertilizer prices
fert.price.data.table <- getQuery(prj, query = 'fertilizer prices')

# Convert price to 2010
fert.price.data.table <- fert.price.data.table %>%
  mutate(value = value*3.22*1000, Units = '2010$/ton') # from 1975$/kg to 2010$/ton

# ----- Query aggregated land allocation
agg.land.alloc.data.table <- getQuery(prj, query = 'aggregated land allocation')

# Convert land area value to million km2
agg.land.alloc.data.table <- agg.land.alloc.data.table %>%
  mutate(value = value/1000, Units = 'million km2') # from thous km2 to million km2

# Query fertilizer consumption and land allocation by agricultural technologies
# to estimate fertilizer application rate [Units: kgN/ha]
agg.fert.rate.table <- estimate_agg_fert_application_rate(prj, 
                                                          query_fert_consump = 'aggregated fertilizer consumption', 
                                                          query_land         = 'aggregated land allocation')

# Add long label for scenarios
fert.cons.data.table      <- add_long_label_scenarios(fert.cons.data.table)
fert.price.data.table     <- add_long_label_scenarios(fert.price.data.table)
agg.land.alloc.data.table <- add_long_label_scenarios(agg.land.alloc.data.table)
agg.fert.rate.table       <- add_long_label_scenarios(agg.fert.rate.table)

# Drop unused levels
fert.cons.data.table$scenario      <- droplevels(fert.cons.data.table$scenario)
fert.price.data.table$scenario     <- droplevels(fert.price.data.table$scenario)
agg.land.alloc.data.table$scenario <- droplevels(agg.land.alloc.data.table$scenario)
agg.fert.rate.table$scenario       <- droplevels(agg.fert.rate.table$scenario)

# Selected scenarios for plotting
select_scenarios <- c('Reference', 
                      'Global constraint (15%) on fertilizer') 
                      # 'Global constraint (15%) - fertilizer io flat', 
                      # 'Global constraint (15%) - fertilizer io steep',
                      # 'Global constraint (15%) - fertilizer io regional')

# Define path to output file
out.folder <- '../figures/'
out.fname  <- 'FigureS9_USA.pdf'
out.fname  <- paste(out.folder, out.fname, sep='')
# Delete existing file
unlink(out.fname)
# Start pdf device driver for saving plots
pdf(out.fname, height=8.5,width=11, onefile=FALSE)

fert.cons.data.table <- filter(fert.cons.data.table, 
                               sector == 'crops',
                               region %in% c('USA'),
                               year >= 2015,
                               scenario %in% select_scenarios)

fert.price.data.table <- filter(fert.price.data.table, 
                                region %in% c('USA'),
                                year >= 2015,
                                scenario %in% select_scenarios)

agg.land.alloc.data.table <- filter(agg.land.alloc.data.table,
                                    landleaf == 'crops',
                                    region %in% c('USA'),
                                    year >= 2015,
                                    scenario %in% select_scenarios)

agg.fert.rate.table <- filter(agg.fert.rate.table,
                              region %in% c('USA'),
                              year >= 2015,
                              scenario %in% select_scenarios)

p1 <- plot_line(fert.cons.data.table, 
                x_col       = 'year',
                y_col       = 'value', 
                group_col   = 'scenario',
                color_col   = 'scenario',
                leg_nrow    = 1,  
                x_lab       = NULL,
                y_lab       = expression(Mt~N~yr^-1),
                plot_title  = 'USA - crops fertilizer consumption') 

p2 <- plot_line(fert.price.data.table, 
                x_col       = 'year',
                y_col       = 'value', 
                group_col   = 'scenario',
                color_col   = 'scenario',
                leg_nrow    = 1,  
                x_lab       = NULL,
                y_lab       = expression(paste(2010~'$',~ton^-1)),
                plot_title  = 'USA - fertilizer price') 

p3 <- plot_line(agg.land.alloc.data.table, 
                x_col       = 'year',
                y_col       = 'value', 
                group_col   = 'scenario',
                color_col   = 'scenario',
                leg_nrow    = 1,  
                x_lab       = NULL,
                y_lab       = expression(million~km^2),
                plot_title  = 'USA - cropland area') 

p4 <- plot_line(agg.fert.rate.table, 
                x_col       = 'year',
                y_col       = 'fert_kgN_ha', 
                group_col   = 'scenario',
                color_col   = 'scenario',
                leg_nrow    = 1,  
                x_lab       = NULL,
                y_lab       = expression(paste(Fertilizer~application~rate~'[',kgN~ha^-1,']')),
                plot_title  = 'USA - crops fertilizer application rate') 

figure <- ggarrange(p1, p2, p3, p4,
                    labels        = 'AUTO',
                    font.label    = list(size=15,family='Helvetica',color='black'),
                    ncol          = 2,
                    nrow          = 2,
                    common.legend = TRUE,
                    legend        = 'bottom')

print(figure)

dev.off()
