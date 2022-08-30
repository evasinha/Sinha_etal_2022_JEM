# Author - Eva Sinha, Pacific Northwest National Lab, eva.sinha@pnnl.gov

library(rgcam)
library(dplyr)
library(tidyr)
# library(tidyverse)
library(stringr)
library(ggplot2)
library(ggpubr)
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
print(listQueries(prj))

co2.emis.data.table    <- getQuery(prj, query='CO2 emissions by region')
nonco2.emis.data.table <- getQuery(prj, query='nonCO2 emissions by region')
lu.emis.data.table     <- getQuery(prj, query='LUC emissions by region')

# Subset production data to only keep data for crops and add ag_tech column
lu.emis.data.table  <- lu.emis.data.table %>%
  mutate(landleaf = sapply(str_split(landleaf, '_'), function(x){x[1]}))

# Sum across all land leafs
tot.lu.emis <- lu.emis.data.table %>%
                      group_by(Units, scenario, region, year) %>% 
                      summarise(value = sum(value))

# Estimate values for global region and add as additional rows
co2.emis.data.table <- bind_rows(co2.emis.data.table, co2.emis.data.table %>% 
                                    group_by(Units, scenario, year) %>% 
                                    summarize(region = 'Global',
                                              value = sum(value)))

nonco2.emis.data.table <- bind_rows(nonco2.emis.data.table, nonco2.emis.data.table %>% 
                                   group_by(Units, scenario, ghg, year) %>% 
                                   summarize(region = 'Global',
                                             value = sum(value)))

lu.emis.data.table <- bind_rows(lu.emis.data.table, lu.emis.data.table %>% 
                                  group_by(Units, scenario, landleaf, year) %>% 
                                  summarize(region = 'Global',
                                            value = sum(value)))

tot.lu.emis <- bind_rows(tot.lu.emis, tot.lu.emis %>% 
                                      group_by(Units, scenario, year) %>% 
                                      summarize(region = 'Global',
                                                value = sum(value)))

# Subset data for plotting
co2.emis.data.table <- co2.emis.data.table %>%
                       filter(region %in% c('Global','China','India','USA'),
                              scenario %in% c('Reference_SSP2-4p5', 'fert_global_const_15_SSP2-4p5', 
                                              # 'fert_global_const_15_fert_io_flat', 'fert_global_const_15_fert_io_steep', 'fert_global_const_15_fert_io_regional',
                                              'carbon_tax_25_5', 'carbon_tax_25_5_fert_global_const_15'))

nonco2.emis.data.table <- nonco2.emis.data.table %>%
                          filter(region %in% c('Global','China','India','USA'),
                                 scenario %in% c('Reference_SSP2-4p5', 'fert_global_const_15_SSP2-4p5', 
                                                 # 'fert_global_const_15_fert_io_flat', 'fert_global_const_15_fert_io_steep', 'fert_global_const_15_fert_io_regional', 
                                                 'carbon_tax_25_5', 'carbon_tax_25_5_fert_global_const_15'))

lu.emis.data.table <- lu.emis.data.table %>%
  filter(region %in% c('Global','China','India','USA'),
         scenario %in% c('Reference_SSP2-4p5', 'fert_global_const_15_SSP2-4p5', 
                         # 'fert_global_const_15_fert_io_flat', 'fert_global_const_15_fert_io_steep', 'fert_global_const_15_fert_io_regional', 
                         'carbon_tax_25_5', 'carbon_tax_25_5_fert_global_const_15'))

tot.lu.emis <- tot.lu.emis %>%
                      filter(region %in% c('Global','China','India','USA'),
                             scenario %in% c('Reference_SSP2-4p5', 'fert_global_const_15_SSP2-4p5', 
                                             # 'fert_global_const_15_fert_io_flat', 'fert_global_const_15_fert_io_steep', 'fert_global_const_15_fert_io_regional', 
                                             'carbon_tax_25_5', 'carbon_tax_25_5_fert_global_const_15'))

# Estimate percentage change compared to Reference scenario
co2.emis.data.table <- co2.emis.data.table %>%
                       group_by(Units, region, year) %>%
                       filter(year >= 2015) %>%
                       mutate(change_ref = (value - value[which(scenario=='Reference_SSP2-4p5')]),
                              per_change_ref = 100*(value - value[which(scenario=='Reference_SSP2-4p5')])/abs(value[which(scenario=='Reference_SSP2-4p5')]))

lu.emis.data.table <- lu.emis.data.table %>%
  group_by(Units, region, landleaf, year) %>%
  filter(year >= 2015) %>%
  mutate(change_ref = (value - value[which(scenario=='Reference_SSP2-4p5')]),
         per_change_ref = 100*(value - value[which(scenario=='Reference_SSP2-4p5')])/abs(value[which(scenario=='Reference_SSP2-4p5')]))

tot.lu.emis <- tot.lu.emis %>%
                      group_by(Units, region, year) %>%
                      filter(year >= 2015) %>%
                      mutate(change_ref = (value - value[which(scenario=='Reference_SSP2-4p5')]),
                             per_change_ref = 100*(value - value[which(scenario=='Reference_SSP2-4p5')])/abs(value[which(scenario=='Reference_SSP2-4p5')]))

# Estimate value in GTC
co2.emis.data.table$value_GTC <- co2.emis.data.table$value/1000
tot.lu.emis$value_GTC <- tot.lu.emis$value/1000

# Reorder factor levels
co2.emis.data.table$region    <- factor(co2.emis.data.table$region, levels=c('Global','China','India','USA'))
nonco2.emis.data.table$region <- factor(nonco2.emis.data.table$region, levels=c('Global','China','India','USA'))
lu.emis.data.table$region     <- factor(lu.emis.data.table$region, levels=c('Global','China','India','USA'))
tot.lu.emis$region            <- factor(tot.lu.emis$region, levels=c('Global','China','India','USA'))

# Add long label for scenarios
co2.emis.data.table    <- add_long_label_scenarios(co2.emis.data.table)
nonco2.emis.data.table <- add_long_label_scenarios(nonco2.emis.data.table)
lu.emis.data.table     <- add_long_label_scenarios(lu.emis.data.table)
tot.lu.emis            <- add_long_label_scenarios(tot.lu.emis)

# Drop unused levels
co2.emis.data.table$scenario    <- droplevels(co2.emis.data.table$scenario)
nonco2.emis.data.table$scenario <- droplevels(nonco2.emis.data.table$scenario)
lu.emis.data.table$scenario     <- droplevels(lu.emis.data.table$scenario)
tot.lu.emis$scenario            <- droplevels(tot.lu.emis$scenario)

print(filter(co2.emis.data.table, region=='Global', year==2100))
print(filter(nonco2.emis.data.table, region=='Global', ghg=='NOx', year==2100))
print(filter(lu.emis.data.table, region=='Global', year==2100, 
             landleaf %in% c('biomassGrass', 'UnmanagedForest', 'Corn', 'Rice', 'Forest'),
             scenario %in% c('Reference', 'Global constraint (15%) on fertilizer', 'Low carbon')))
print(filter(tot.lu.emis, region=='Global', year %in% c(2060, 2100)))

print(filter(co2.emis.data.table, 
             year==2100, 
             region %in% c('Global','China','India','USA'), 
             scenario %in% c('Reference','Global constraint (15%) on fertilizer')))

print(filter(co2.emis.data.table,
             year==2100, 
             region == 'Global', 
             scenario == 'Global constraint (15%) on fertilizer'))

print(filter(tot.lu.emis, 
             year==2100, 
             region %in% c('Global','China','India','USA'), 
             scenario %in% c('Reference','Global constraint (15%) on fertilizer')))

print(filter(tot.lu.emis,
             year==2100, 
             region == 'Global', 
             scenario == 'Global constraint (15%) on fertilizer'))

# Define path to output file
out.folder <- '../figures/'
out.fname  <- 'FigureS6.pdf'
out.fname  <- paste(out.folder, out.fname, sep='')
# Delete existing file
unlink(out.fname)
# Start pdf device driver for saving plots
pdf(out.fname, height=8.5,width=11) 

p1 <- plot_point_line(filter(co2.emis.data.table, region %in% c('Global'),
                             scenario %in% c('Reference',
                                             'Global constraint (15%) on fertilizer',
                                             'Low carbon',
                                             'Low carbon with global constraint (15%)'),
                             year >=2005),
                      x_col      = 'year',
                      y_col      = 'value_GTC',
                      group_col  = 'scenario',
                      color_col  = 'scenario',
                      leg_nrow   = 2,
                      y_lab      = expression(GtC~yr^-1), 
                      plot_title = 'Global fossil fuel and industrial CO2 emissions',
                      y_min      = 0,
                      y_max      = 20,
                      y_inc      = 4)

print (p1)

p2 <- plot_point_line(filter(tot.lu.emis, region %in% c('Global'),
                             scenario %in% c('Reference',
                                             'Global constraint (15%) on fertilizer',
                                             'Low carbon',
                                             'Low carbon with global constraint (15%)'),
                             year >=2005),
                      x_col      = 'year',
                      y_col      = 'value_GTC',
                      group_col  = 'scenario',
                      color_col  = 'scenario',
                      leg_nrow   = 2,
                      y_lab      = expression(GtC~yr^-1), 
                      plot_title = 'Land use change CO2 emissions',
                      y_min      = -0.2,
                      y_max      = 1,
                      y_inc      = 0.2)

print (p2)

p3 <- plot_point_line(filter(co2.emis.data.table, region %in% c('Global'),
                             scenario %in% c('Reference',
                                             'Global constraint (15%) on fertilizer',
                                             'Low carbon',
                                             'Low carbon with global constraint (15%)'),
                             year >=2005),
                      x_col      = 'year',
                      y_col      = 'value_GTC',
                      group_col  = 'scenario',
                      color_col  = 'scenario',
                      leg_nrow   = 2,
                      y_lab      = expression(GtC~yr^-1), 
                      plot_title = 'Global fossil fuel and industrial CO2 emissions',
                      y_min      = 0,
                      y_max      = 20,
                      y_inc      = 4)

print (p3)

p4 <- plot_point_line(filter(tot.lu.emis, region %in% c('Global'),
                             scenario %in% c('Reference',
                                             'Global constraint (15%) on fertilizer',
                                             'Low carbon',
                                             'Low carbon with global constraint (15%)'),
                             year >=2005),
                      x_col      = 'year',
                      y_col      = 'value_GTC',
                      group_col  = 'scenario',
                      color_col  = 'scenario',
                      leg_nrow   = 2,
                      y_lab      = expression(GtC~yr^-1), 
                      plot_title = 'Land use change CO2 emissions',
                      y_min      = -0.2,
                      y_max      = 1,
                      y_inc      = 0.2)

print (p4)

# Grid table
table.co2.emis <- co2.emis.data.table %>%
  filter(region=='Global', year==2100) %>%
  select(-value_GTC)

table.lu.emis <- tot.lu.emis %>%
  filter(region=='Global', year==2100) %>%
  select(-value_GTC)

table.co2.emis$value          <- format(table.co2.emis$value, digits = 2)
table.co2.emis$change_ref     <- format(table.co2.emis$change_ref, digits = 2)
table.co2.emis$per_change_ref <- format(table.co2.emis$per_change_ref, digits = 2)

table.lu.emis$value           <- format(table.lu.emis$value, digits = 2)
table.lu.emis$change_ref      <- format(table.lu.emis$change_ref, digits = 2)
table.lu.emis$per_change_ref  <- format(table.lu.emis$per_change_ref, digits = 2)

grid.arrange(text_grob('Global CO2 emissions for 2100', size=15,family='Helvetica',color='black',face='bold'),
             tableGrob(table.co2.emis, rows = NULL, theme = ttheme_minimal()),
             text_grob('Global land use change emissions for 2100', size=15,family='Helvetica',color='black',face='bold'),
             tableGrob(table.lu.emis, rows = NULL, theme = ttheme_minimal()),
             ncol=1)

p5 <- plot_point_line(filter(nonco2.emis.data.table, 
                             ghg == 'NOx',
                             region %in% c('Global'),
                             scenario %in% c('Reference',
                                             'Global constraint (15%) on fertilizer',
                                             'Low carbon',
                                             'Low carbon with global constraint (15%)'),
                             year >=2005),
                      x_col      = 'year',
                      y_col      = 'value',
                      group_col  = 'scenario',
                      color_col  = 'scenario',
                      leg_nrow   = 2,
                      y_lab      = expression(Tg~yr^-1), 
                      plot_title = 'NOx emissions',
                      y_min      = 0,
                      y_max      = 200,
                      y_inc      = 50)

print (p5)

p6 <- plot_point_line(filter(nonco2.emis.data.table, 
                             ghg == 'NH3',
                             region %in% c('Global'),
                             scenario %in% c('Reference',
                                             'Global constraint (15%) on fertilizer',
                                             'Low carbon',
                                             'Low carbon with global constraint (15%)'),
                             year >=2005),
                      x_col      = 'year',
                      y_col      = 'value',
                      group_col  = 'scenario',
                      color_col  = 'scenario',
                      leg_nrow   = 2,
                      y_lab      = expression(Tg~yr^-1), 
                      plot_title = 'NH3 emissions',
                      y_min      = 0,
                      y_max      = 35,
                      y_inc      = 15)

print (p6)

p7 <- plot_point_line(filter(nonco2.emis.data.table, 
                             ghg == 'N2O',
                             region %in% c('Global'),
                             scenario %in% c('Reference',
                                             'Global constraint (15%) on fertilizer',
                                             'Low carbon',
                                             'Low carbon with global constraint (15%)'),
                             year >=2005),
                      x_col      = 'year',
                      y_col      = 'value',
                      group_col  = 'scenario',
                      color_col  = 'scenario',
                      leg_nrow   = 2,
                      y_lab      = expression(Tg~yr^-1), 
                      plot_title = 'N2O emissions',
                      y_min      = 0,
                      y_max      = 4,
                      y_inc      = 2)

print (p7)

dev.off()
