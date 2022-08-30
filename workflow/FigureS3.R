# Author - Eva Sinha, Pacific Northwest National Lab, eva.sinha@pnnl.gov

library(rgcam)
library(dplyr)
library(tidyr)
library(ggplot2)
library(colorRamps)
library(gridExtra)

source('read_summarise_queries.R')
source('plot_vector.R')
source('color_pal_labels.R')

# Function details
# plot_yield_timeseries          - Make plot of timeseries of yield for crops
# plot_timeseries_all_regions    - Make timeseries plot of specified query for all regions
# plot_timeseries_select_regions - Make timeseries plot of specified query for select regions
# plot_per_change                - Make plot of percentage change in value for top five countries 
#                                  with largest increase and top five countries with largest decrease

#________________________________________________________
# Make plot of timeseries of yield for crops
plot_yield_timeseries <- function(yield.data.table, select_scenarios, y_lab, plot_title){
  
  # Subset data for plotting
  plot.data  <- filter(yield.data.table, region %in% c('Global','China','India','USA'), scenario %in% select_scenarios, year >=2005)
  
   # Reorder factor levels
  plot.data$region   <- factor(plot.data$region, levels=c('Global','China','India','USA'))
  
  # Make point and line plot from data
  p1 <- plot_line_facet(plot.data, 
                        x_col      = 'year',
                        y_col      = 'yield',
                        group_col  = 'scenario',
                        color_col  = 'scenario',
                        linetype_col = NULL,
                        facet_var  = 'region', 
                        facet_ncol = 1,
                        facet_scale = 'free_y',
                        leg_nrow   = 3,
                        x_lab      = NULL,
                        y_lab, 
                        plot_title)
  
  print (p1)
  
}

#________________________________________________________
# Make timeseries plot of specified query for all regions
plot_timeseries_all_regions <- function(prj, query, select.scenario, y_lab, plot_title){
  
  # Retrieve particular query for all scenarios in the dataset, formatted as a single table
  query.data.table <- getQuery(prj, query)
  print(nrow(query.data.table))
  
  # Subset data for plotting
  plot.data  <- filter(query.data.table, scenario==select.scenario, year >=2005)
  
  # Identify top 10 regions with highest value in 2100
  top_10_region <- top_n(filter(plot.data, year==2100), 10, value)$region
  
  # Make point and line plot from data
  p1 <- plot_line(filter(plot.data, region %in% top_10_region), 
                  x_col      = 'year',
                  y_col      = 'value',
                  group_col  = 'region',
                  color_col  = 'region',
                  leg_nrow   = 3,
                  x_lab      = NULL,
                  y_lab, 
                  plot_title = paste(plot_title, ' - ', select.scenario, ' scenario', sep=''))
  
  print (p1)
}

#________________________________________________________
# Make timeseries plot of specified query for select regions
plot_timeseries_select_regions <- function(prj, select_scenarios, query, y_lab, plot_title, out.fpath, out.fname,
                                           subselect.colName=NULL, subselect.value=NULL,
                                           sum.colName=NULL){
  
  # Retrieve particular query for all scenarios in the dataset, formatted as a single table
  query.data.table <- getQuery(prj, query)
  print(nrow(query.data.table))
  
  if (!is.null(subselect.colName)){
    
    query.data.table <- query.data.table %>%
                        filter(!!sym(subselect.colName) %in% subselect.value) %>% # only keep select rows
                        select(-subselect.colName) # drop column for making subset
    
    # print(nrow(query.data.table))
  }
  
  if (!is.null(sum.colName)){
    
    query.data.table <- query.data.table %>%
                        group_by(Units, scenario, region, year) %>% 
                        summarise(value = sum(value))
    
    # print(nrow(query.data.table))
  }
  
  # Estimate values for global region and add as additional rows
  query.data.table <- bind_rows(query.data.table, query.data.table %>% 
                                  group_by(scenario, year) %>% 
                                  summarize(region = 'Global',
                                            value  = sum(value)))
  
  # Subset data for plotting
  plot.data  <- filter(query.data.table, region %in% c('Global','China','India','USA'), scenario %in% select_scenarios, year >=2005)
  
  # Add long label
  plot.data$scenario <- param_labeller('variable', plot.data$scenario)
  
  # Reorder factor levels
  plot.data$region   <- factor(plot.data$region, levels=c('Global','China','India','USA'))
  plot.data$scenario <- factor(plot.data$scenario, levels=c('Reference',
                                                            'Global constraint (15%) on fertilizer',
                                                            # 'Global constraint (15%) - fertilizer io flat', 
                                                            # 'Global constraint (15%) - fertilizer io steep',
                                                            # 'Global constraint (15%) - fertilizer io regional',
                                                            'Low carbon',
                                                            'Low carbon with global constraint (15%)'))
  
  # Make point and line plot from data
  p1 <- plot_line_facet(plot.data, 
                        x_col      = 'year',
                        y_col      = 'value',
                        group_col  = 'scenario',
                        color_col  = 'scenario',
                        linetype_col = NULL,
                        facet_var  = 'region', 
                        facet_ncol = 1,
                        facet_scale = 'free_y',
                        leg_nrow   = 3,
                        x_lab      = NULL,
                        y_lab, 
                        plot_title)
  
  print (p1)
  
  # Save output in file
  write.csv(plot.data, paste(out.fpath, out.fname, sep=''))
  
}

#________________________________________________________

# Load a previously generated project data file
# prj.name       <- 'fert_constrain.dat'
# prj.name       <- 'fert_io_coeff_SA.dat'
prj.name       <- 'fert_const_updated_IO_coeff.dat'
prj            <- loadProject(prj.name)

# Query land and agricultural production and estimate yield for crops [Units: kg/ha]
yield.data.table <- estimate_yield(prj,
                                   query_land       = 'aggregated land allocation',
                                   query_production = 'aggregated ag production by crop type')

# Estimate value of variable by subsetting query data and summing across specified type
ag.prod.data.table <- estimate_value_subset_sum_query(prj, 
                                                      query             = 'aggregated ag production by crop type', 
                                                      subselect.colName = 'sector',
                                                      subselect.value   = 'crops',
                                                      sum.colName       = 'output')

food.cons.data.table <- estimate_value_subset_sum_query(prj, 
                                                        query       = 'food consumption by type (general)', 
                                                        sum.colName = 'sector')

# Estimate value of fertilizer consumption query
fert.cons.data.table <- estimate_value_query(prj, query='fertilizer consumption by region')

# Estimate value of fertilizer production query
fert.prod.data.table <- estimate_value_query(prj, query='fertilizer production by region')

# Estimate values for the global region from fertilizer production table
# and add as additional rows to the consumption table
fert.cons.data.table <- bind_rows(fert.cons.data.table, fert.prod.data.table %>% 
                                    group_by(Units, scenario, sector, year) %>% 
                                    summarize(region = 'Global',
                                              value = sum(value)) %>% 
                                    rename(input = sector))

print(fert.cons.data.table %>% 
        group_by(Units, scenario, region, input) %>%
        filter(region=='Global', scenario=='Reference', year>=2015) %>% 
        mutate(per_change_2015 = 100*(value - value[year==2015])/value[year==2015]))

print(fert.cons.data.table %>% 
        group_by(Units, region, input) %>%
        filter(region == 'Global', year %in% c(2060, 2100), 
               scenario %in% c('Reference', 'Global constraint (15%) on fertilizer')) %>% 
        mutate(change = (value - value[scenario == 'Reference'])))

print('Cumulative change in fertilizer usage between global constraint and reference scenario')
print(fert.cons.data.table %>% 
        group_by(Units, region, input) %>%
        filter(region == 'Global', 
               scenario %in% c('Reference', 'Global constraint (15%) on fertilizer')) %>% 
        mutate(change = (value - value[scenario == 'Reference'])) %>%
        filter(scenario == 'Global constraint (15%) on fertilizer') %>%
        summarise(cum_change = sum(change)))

print(fert.cons.data.table %>% 
        group_by(Units, region, input) %>%
        filter(region == 'Global', year %in% c(2060, 2100)) %>% 
        mutate(per_change = 100*(value - value[scenario == 'Reference'])/value[scenario=='Reference']))

# Reorder factor levels
fert.cons.data.table$region   <- factor(fert.cons.data.table$region, levels=c('Global','China','India','USA'))

# Estimate value of variable by subsetting query data and summing across specified type
forest.area.data.table <- estimate_value_subset_sum_query(prj,
                                                          query             = 'aggregated land allocation', 
                                                          subselect.colName = 'landleaf',
                                                          subselect.value   = 'forest (unmanaged)')

cropland.area.data.table <- estimate_value_subset_sum_query(prj,
                                                            query             = 'aggregated land allocation', 
                                                            subselect.colName = 'landleaf',
                                                            subselect.value   = 'crops')

biomass.prod.data.table <- estimate_value_query(prj, query = 'purpose-grown biomass production')


# Define path to output file
out.folder <- '../figures/'
out.fname  <- 'Fertilizer_reference.pdf'
out.fname  <- paste(out.folder, out.fname, sep='')
# Delete existing file
unlink(out.fname)
# Start pdf device driver for saving plots
pdf(out.fname, height=8.5,width=11) 

# Make timeseries plot of specified query for all regions
plot_timeseries_all_regions(prj,
                            query      = 'fertilizer consumption by region',
                            select.scenario = 'Reference_SSP2-4p5',
                            y_lab      = expression(Mt~N~yr^-1), 
                            plot_title = 'Fertilizer consumption')

plot_timeseries_all_regions(prj,
                            query      = 'fertilizer consumption by region',
                            select.scenario = 'fert_global_const_15_SSP2-4p5',
                            y_lab      = expression(Mt~N~yr^-1), 
                            plot_title = 'Fertilizer consumption')

# plot_timeseries_all_regions(prj,
#                             query      = 'fertilizer consumption by region',
#                             select.scenario = 'fert_global_const_15_fert_io_flat',
#                             y_lab      = expression(Mt~N~yr^-1), 
#                             plot_title = 'Fertilizer consumption')
# 
# plot_timeseries_all_regions(prj,
#                             query      = 'fertilizer consumption by region',
#                             select.scenario = 'fert_global_const_15_fert_io_steep',
#                             y_lab      = expression(Mt~N~yr^-1), 
#                             plot_title = 'Fertilizer consumption')
# 
# plot_timeseries_all_regions(prj,
#                             query      = 'fertilizer consumption by region',
#                             select.scenario = 'fert_global_const_15_fert_io_regional',
#                             y_lab      = expression(Mt~N~yr^-1), 
#                             plot_title = 'Fertilizer consumption')

plot_timeseries_all_regions(prj,
                            query      = 'fertilizer consumption by region',
                            select.scenario = 'carbon_tax_25_5',
                            y_lab      = expression(Mt~N~yr^-1), 
                            plot_title = 'Fertilizer consumption')

plot_timeseries_all_regions(prj,
                            query      = 'fertilizer consumption by region',
                            select.scenario = 'carbon_tax_25_5_fert_global_const_15',
                            y_lab      = expression(Mt~N~yr^-1), 
                            plot_title = 'Fertilizer consumption')

dev.off()

# Define path to output file
out.folder <- '../figures/'
out.fname  <- 'Fertilizer_reference_scenario.pdf'
out.fname  <- paste(out.folder, out.fname, sep='')
# Delete existing file
unlink(out.fname)
# Start pdf device driver for saving plots
pdf(out.fname, height=8.5,width=11) 

# Make point and line plot from data
p1 <- plot_point_line(filter(fert.cons.data.table, region %in% c('Global'),
                             scenario %in% c('Reference'),
                             year >=2005),
                      x_col      = 'year',
                      y_col      = 'value',
                      group_col  = 'region',
                      color_col  = 'region',
                      leg_nrow   = 1,
                      y_lab      = expression(Mt~N~yr^-1), 
                      plot_title = 'Fertilizer consumption',
                      y_min      = 0,
                      y_max      = 160,
                      y_inc      = 20)

print (p1)

# Make point and line plot from data
p2 <- plot_point_line(filter(fert.cons.data.table, region %in% c('China','India','USA'),
                             scenario %in% c('Reference'),
                             year >=2005),
                      x_col      = 'year',
                      y_col      = 'value',
                      group_col  = 'region',
                      color_col  = 'region',
                      leg_nrow   = 1,
                      y_lab      = expression(Mt~N~yr^-1), 
                      plot_title = 'Fertilizer consumption',
                      y_min      = 0,
                      y_max      = 45,
                      y_inc      = 5)

print (p2)

dev.off()

# Define path to output file
out.folder <- '../figures/'
out.fname  <- 'FigureS3.pdf'
out.fname  <- paste(out.folder, out.fname, sep='')
# Delete existing file
unlink(out.fname)
# Start pdf device driver for saving plots
pdf(out.fname, height=8.5,width=11) 

# Make point and line plot from data
p1 <- plot_line_facet(filter(fert.cons.data.table, region %in% c('China','India','USA'),
                             scenario %in% c('Reference',
                                             'Global constraint (15%) on fertilizer'),
                                             # 'Global constraint (15%) - fertilizer io flat', 
                                             # 'Global constraint (15%) - fertilizer io steep',
                                             # 'Global constraint (15%) - fertilizer io regional'),
                             year >=2005), 
                      x_col      = 'year',
                      y_col      = 'value',
                      group_col  = 'region',
                      color_col  = 'region',
                      linetype_col = NULL,
                      facet_var  = 'scenario', 
                      facet_ncol = 2,
                      facet_scale = 'fixed',
                      leg_nrow   = 1,
                      x_lab      = NULL,
                      y_lab      = expression(Mt~N~yr^-1), 
                      plot_title = 'Fertilizer consumption')

print (p1)

p2 <- plot_line_facet(filter(fert.cons.data.table, region %in% c('Global','China','India','USA'),
                             scenario %in% c('Reference',
                                             'Global constraint (15%) on fertilizer'),
                             year >=2005), 
                      x_col      = 'year',
                      y_col      = 'value',
                      group_col  = 'scenario',
                      color_col  = 'scenario',
                      linetype_col = NULL,
                      facet_var  = 'region', 
                      facet_ncol = 2,
                      facet_scale = 'free_y',
                      leg_nrow   = 1,
                      x_lab      = NULL,
                      y_lab      = expression(Mt~N~yr^-1), 
                      plot_title = 'Fertilizer consumption')

print (p2)

table.data <- fert.cons.data.table %>%
              filter(region %in% c('Global'),
                     scenario %in% c('Reference', 'Global constraint (15%) on fertilizer'),
                     year >=2020) %>%
              group_by(Units, region, input, year) %>%
              mutate(change_ref = (value - value[scenario == 'Reference'])) %>%
              filter(scenario == 'Global constraint (15%) on fertilizer')


print(paste('Cumulative decrease in fertilizer usage from 2020 to 2100', format(sum(table.data$change_ref), digits=2)))

table.data$value           <- format(table.data$value, digits = 2)
table.data$change_ref      <- format(table.data$change_ref, digits = 2)

grid.arrange(text_grob('Global constraint (15%) on fertilizer ferilizer usage', size=10,family='Helvetica',color='black',face='bold'),
             tableGrob(table.data, rows = NULL, theme = ttheme_minimal()),
             ncol=1)

p3 <- plot_point_line(filter(fert.cons.data.table, region %in% c('Global'),
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
                            y_lab      = expression(Mt~N~yr^-1), 
                            plot_title = 'Fertilizer consumption',
                            y_min      = 0,
                            y_max      = 240,
                            y_inc      = 20)

print (p3)

print(filter(fert.cons.data.table, region %in% c('Global'),
             scenario %in% c('Reference',
                             'Global constraint (15%) on fertilizer',
                             'Low carbon',
                             'Low carbon with global constraint (15%)'),
             year == 2100))

p5 <- plot_line_facet(filter(food.cons.data.table, region %in% c('China','India','USA'),
                             scenario %in% c('Reference',
                                             'Global constraint (15%) on fertilizer'),
                             year >=2005), 
                      x_col      = 'year',
                      y_col      = 'value',
                      group_col  = 'region',
                      color_col  = 'region',
                      linetype_col = NULL,
                      facet_var  = 'scenario', 
                      facet_ncol = 2,
                      facet_scale = 'fixed',
                      leg_nrow   = 1,
                      x_lab      = NULL,
                      y_lab      = 'Pcal', 
                      plot_title = 'Food consumption')

print (p5)

dev.off()


# Define path to output file
out.fname  <- 'Trade_offs_fertilizer_constrain.pdf'
out.fname  <- paste(out.folder, out.fname, sep='')
# Delete existing file
unlink(out.fname)
# Start pdf device driver for saving plots
pdf(out.fname, height=11,width=8.5) 

select_scenarios <- c('Reference_SSP2-4p5', 'fert_global_const_15_SSP2-4p5', 
                      'carbon_tax_25_5', 'carbon_tax_25_5_fert_global_const_15')

select_scenarios_long_labels <- c('Reference', 'Global constraint (15%) on fertilizer', 
                                  'Low carbon', 'Low carbon with global constraint (15%)')

# Make timeseries plot of specified query for select regions
plot_timeseries_select_regions(prj, select_scenarios,
                               query      = 'fertilizer consumption by region', 
                               y_lab      = expression(Mt~N~yr^-1), 
                               plot_title = 'Fertilizer consumption',
                               out.fpath  = '../output_tables/', 
                               out.fname  = 'fert_consumption.csv')

plot_timeseries_select_regions(prj, select_scenarios,
                               query      = 'fertilizer production by region', 
                               y_lab      = expression(Mt~N~yr^-1), 
                               plot_title = 'Fertilizer production',
                               out.fpath  = '../output_tables/', 
                               out.fname  = 'fert_production.csv')

plot_timeseries_select_regions(prj, select_scenarios,
                               query             = 'aggregated land allocation', 
                               y_lab             = expression(thousands~km^2), 
                               plot_title        = 'Unmanaged forest cover',
                               out.fpath         = '../output_tables/', 
                               out.fname         = 'land_allocation_unmanaged_forest.csv',
                               subselect.colName = 'landleaf',
                               subselect.value   = 'forest (unmanaged)')

plot_timeseries_select_regions(prj, select_scenarios,
                               query             = 'aggregated land allocation', 
                               y_lab             = expression(thousands~km^2), 
                               plot_title        = 'Cropland area',
                               out.fpath         = '../output_tables/', 
                               out.fname         = 'land_allocation_cropland.csv',
                               subselect.colName = 'landleaf',
                               subselect.value   = 'crops')

plot_timeseries_select_regions(prj, select_scenarios,
                               query             = 'prices by sector', 
                               y_lab             = expression(paste(1975~'$',~kg^-1)), 
                               plot_title        = 'Wheat prices',
                               out.fpath         = '../output_tables/', 
                               out.fname         = 'wheat_prices.csv',
                               subselect.colName = 'sector',
                               subselect.value   = 'Wheat')

plot_timeseries_select_regions(prj, select_scenarios,
                               query             = 'food consumption by type (general)', 
                               y_lab             = 'Pcal', 
                               plot_title        = 'Food consumption',
                               out.fpath         = '../output_tables/', 
                               out.fname         = 'food_consumption.csv',
                               sum.colName       = 'sector')

plot_timeseries_select_regions(prj, select_scenarios,
                               query             = 'aggregated ag production by crop type', 
                               y_lab             = 'Mt', 
                               plot_title        = 'Agricultural production',
                               out.fpath         = '../output_tables/', 
                               out.fname         = 'ag_production.csv',
                               subselect.colName = 'sector',
                               subselect.value   = 'crops',
                               sum.colName       = 'output')

plot_timeseries_select_regions(prj, select_scenarios,
                               query      = 'CO2 emissions by region', 
                               y_lab      = expression(Mt~CO[2]~yr^-1), 
                               plot_title = expression(Energy~system~CO[2]~emissions),
                               out.fpath  = '../output_tables/', 
                               out.fname  = 'CO2_emissions.csv')

plot_timeseries_select_regions(prj, select_scenarios,
                               query      = 'nonCO2 emissions by region',
                               y_lab      = expression(Tg~yr^-1),
                               plot_title = expression(N[2]~O~emissions),
                               out.fpath  = '../output_tables/',
                               out.fname  = 'N2O_emissions.csv',
                               subselect.colName = 'ghg',
                               subselect.value   = c('N2O', 'N2O_AGR', 'N2O_AWB'),
                               sum.colName       = 'value')

plot_timeseries_select_regions(prj, select_scenarios,
                               query      = 'nonCO2 emissions by region',
                               y_lab      = expression(Tg~yr^-1),
                               plot_title = expression(NH[3]~emissions),
                               out.fpath  = '../output_tables/',
                               out.fname  = 'NH3_emissions.csv',
                               subselect.colName = 'ghg',
                               subselect.value   = c('NH3', 'NH3_AGR', 'NH3_AWB'),
                               sum.colName       = 'value')

plot_timeseries_select_regions(prj, select_scenarios,
                               query      = 'nonCO2 emissions by region',
                               y_lab      = expression(Tg~yr^-1),
                               plot_title = expression(NO[x]~emissions),
                               out.fpath  = '../output_tables/',
                               out.fname  = 'NOx_emissions.csv',
                               subselect.colName = 'ghg',
                               subselect.value   = c('NOx', 'NOx_AGR', 'NOx_AWB'),
                               sum.colName       = 'value')

plot_timeseries_select_regions(prj, select_scenarios,
                               query      = 'LUC emissions by region', 
                               y_lab      = expression(Mt~CO[2]~yr^-1), 
                               plot_title = expression(Land-use~change~CO[2]~emissions),
                               out.fpath   = '../output_tables/', 
                               out.fname   = 'LUC_emissions.csv',
                               sum.colName = 'landleaf')

plot_timeseries_select_regions(prj, select_scenarios,
                               query      = 'purpose-grown biomass production', 
                               y_lab      = '[EJ]', 
                               plot_title = 'Purpose-grown biomass production',
                               out.fpath   = '../output_tables/', 
                               out.fname   = 'biomass_production.csv')

plot_timeseries_select_regions(prj, select_scenarios,
                               query      = 'regional biomass consumption', 
                               y_lab      = '[EJ]', 
                               plot_title = 'Regional biomass consumption',
                               out.fpath   = '../output_tables/', 
                               out.fname   = 'biomass_consumption.csv')

plot_timeseries_select_regions(prj, select_scenarios,
                               query      = 'fertilizer prices', 
                               y_lab      = expression(paste(1975~'$',~kg^-1)), 
                               plot_title = 'Fertilizer prices',
                               out.fpath  = '../output_tables/', 
                               out.fname  = 'fert_prices.csv')

# Make plot of timeseries of yield for crops
plot_yield_timeseries(yield.data.table, select_scenarios_long_labels,
                      y_lab            = expression(kg~ha^-1),
                      plot_title       = 'Crop yield')

dev.off()


# Define path to output file
out.fname  <- 'Yield_vs_fert_regions.pdf'
out.fname  <- paste(out.folder, out.fname, sep='')
# Delete existing file
unlink(out.fname)
# Start pdf device driver for saving plots
pdf(out.fname, height=8.5,width=14) 

plot.data <- inner_join(yield.data.table, fert.cons.data.table, by=c('scenario','region','year')) %>%
             filter(region %in% c('Africa_Northern','Argentina','China','Canada','Colombia',
                                  'India','Japan','Mexico','Middle East','Pakistan','USA'),
                    year >= 2005)

p1 <- plot_point_facet_cont_color(filter(plot.data, scenario %in% c('Reference','Global constraint (15%) on fertilizer')), 
                                  x_col      = 'value',
                                  y_col      = 'yield',
                                  color_col  = 'year',
                                  shape_col  = 'scenario',
                                  facet_var  = 'region', 
                                  facet_ncol = 4, 
                                  facet_scale  = 'free', 
                                  leg_nrow     = 2,
                                  color_breaks = seq(2005, 2100, 15),
                                  x_lab      = expression(paste(Fertilizer~application~rate~'[',kgN~ha^-1,']')),
                                  y_lab      = expression(paste(Yield~'[',Mg~ha^-1,']')),
                                  plot_title = 'Yield vs. nitrogen fertilizer rate')


print(p1)

p2 <- plot_point_facet_cont_color(filter(plot.data, scenario %in% c('Reference','Global constraint (15%) on fertilizer'),
                                         region %in% c('China','India','USA')), 
                                  x_col      = 'value',
                                  y_col      = 'yield',
                                  color_col  = 'year',
                                  shape_col  = 'scenario',
                                  facet_var  = 'region', 
                                  facet_ncol = 2, 
                                  facet_scale  = 'free', 
                                  leg_nrow     = 2,
                                  color_breaks = seq(2005, 2100, 15),
                                  x_lab      = expression(paste(Fertilizer~application~rate~'[',kgN~ha^-1,']')),
                                  y_lab      = expression(paste(Yield~'[',Mg~ha^-1,']')),
                                  plot_title = 'Yield vs. nitrogen fertilizer rate')


print(p2)

dev.off()

# Define path to output file
out.fname  <- 'Trade_offs_fertilizer_constrain_per_change.pdf'
out.fname  <- paste(out.folder, out.fname, sep='')
# Delete existing file
unlink(out.fname)
# Start pdf device driver for saving plots
pdf(out.fname, height=8.5,width=14) 

# Make plot of percentage change in value for top five countries 
# with largest increase and top five countries with largest decrease
plot_per_change(filter(yield.data.table, scenario %in% select_scenarios_long_labels), 
                xaxis.var  = 'region',
                value.var  = 'yield',
                select_yr  = 2100, 
                facet_ncol = 5,
                y_lab      = 'Percentage change in yield', 
                plot_title = 'Percentage change in yield from Reference scenario for 2100')

plot_per_change(filter(ag.prod.data.table, scenario %in% select_scenarios_long_labels), 
                xaxis.var  = 'region',
                value.var  = 'value',
                select_yr  = 2100, 
                facet_ncol = 5,
                y_lab      = 'Percentage change in agricultural production', 
                plot_title = 'Percentage change in agricultural production from Reference scenario for 2100')

plot_per_change(filter(fert.cons.data.table, scenario %in% select_scenarios_long_labels, region != 'Global'), 
                xaxis.var  = 'region',
                value.var  = 'value',
                select_yr  = 2100, 
                facet_ncol = 5,
                y_lab      = 'Percentage change in fertilizer consumption', 
                plot_title = 'Percentage change in fertilizer consumption from Reference scenario for 2100')

plot_per_change(filter(forest.area.data.table, scenario %in% select_scenarios_long_labels), 
                xaxis.var  = 'region',
                value.var  = 'value',
                select_yr  = 2100, 
                facet_ncol = 5,
                y_lab      = 'Percentage change in unmanaged forested area', 
                plot_title = 'Percentage change in unmanaged forested area from Reference scenario for 2100')

plot_per_change(filter(cropland.area.data.table, scenario %in% select_scenarios_long_labels), 
                xaxis.var  = 'region',
                value.var  = 'value',
                select_yr  = 2100, 
                facet_ncol = 5,
                y_lab      = 'Percentage change in cropland area', 
                plot_title = 'Percentage change in cropland area from Reference scenario for 2100')

plot_per_change(filter(biomass.prod.data.table, scenario %in% c('Reference', 'Global constraint (15%) on fertilizer')),
                                                                # 'Global constraint (15%) - fertilizer io flat', 
                                                                # 'Global constraint (15%) - fertilizer io steep',
                                                                # 'Global constraint (15%) - fertilizer io regional')), 
                xaxis.var  = 'region',
                value.var  = 'value',
                select_yr  = 2100, 
                facet_ncol = 5,
                y_lab      = 'Percentage change in purpose-grown biomass production', 
                plot_title = 'Percentage change in purpose-grown biomass production from Reference scenario for 2100')

dev.off()
