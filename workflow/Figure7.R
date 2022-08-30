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
# Summarize price data for crops by adding rows for global region
# and estimating percentage change since 2015 (first year among the future years)
summarize_crop_price_data <- function(crop.price.data, select_scenarios){
  
  # # Estimate values for global region and add as additional rows
  # crop.price.data <- bind_rows(crop.price.data, crop.price.data %>% 
  #                                 group_by(Units, scenario, sector, year) %>% 
  #                                 summarize(region = 'Global',
  #                                           value = sum(value)))
  
  # Estimate percentage change compared to Reference scenario
  crop.price.data <- crop.price.data %>%
                     group_by(region, year) %>%
                     filter(year >= 2015) %>%
                     mutate(change_ref      = (value - value[which(scenario=='Reference_SSP2-4p5')]),
                            perc_change_ref = 100*(value - value[which(scenario=='Reference_SSP2-4p5')])/value[which(scenario=='Reference_SSP2-4p5')])
  
  # Subset data for plotting
  crop.price.data <- crop.price.data %>% 
                     filter(scenario %in% select_scenarios)
  
  # Add long label for scenarios
  crop.price.data <- add_long_label_scenarios(crop.price.data)
  
  # Drop unused levels
  crop.price.data$scenario <- droplevels(crop.price.data$scenario)
  
  # Print a summary table for four regions for 2100
  print(spread( (crop.price.data %>% filter(year==2100) %>%  select(-value, -change_ref)), key=region, value=perc_change_ref))
  
  return(crop.price.data)  
  
}

#________________________________________________________
# Load a previously generated project data file
# prj.name       <- 'fert_constrain.dat'
# prj.name       <- 'fert_io_coeff_SA.dat'
prj.name       <- 'fert_const_updated_IO_coeff.dat'
prj            <- loadProject(prj.name)

# List all scenarios
print(listScenarios(prj))

# First identify which crop has the highest agricultural production at the global scale
ag.prod.data.table <- getQuery(prj, query = 'ag production by crop type')

ag.prod.data.table <- ag.prod.data.table %>%
                      group_by(Units, scenario, sector, output, year) %>% 
                      summarize(region = 'Global', value = sum(value)) %>%  # Estimate ag. prod for the global region
                      filter(scenario == 'Reference_SSP2-4p5', year == 2100) %>%     # Keep select rows
                      arrange(desc(value))                                  # Sort tibble in descending order by value

print(ag.prod.data.table) # Corn has the highest agricultural production in 2100 at a global scale

# Retrieve prices by sector for all scenarios in the dataset, formatted as a single table
price.data.table <- getQuery(prj, query = 'prices by sector')

# Subset for agricultural commodities and convert value to 2010
price.data.table <- price.data.table %>%
                    filter(sector %in% c('Corn', 'FiberCrop', 'OilCrop', 'Rice', 'SugarCrop', 'Wheat')) %>%
                    mutate(value = value*3.22*1000, Units = '2010$/ton') # from 1975$/kg to 2010$/ton
                   
# Subset price for wheat, biomass, and electricity
corn.price.data      <- price.data.table %>% filter(sector == 'Corn')
fibercrop.price.data <- price.data.table %>% filter(sector == 'FiberCrop')
oilcrop.price.data   <- price.data.table %>% filter(sector == 'OilCrop')
rice.price.data      <- price.data.table %>% filter(sector == 'Rice')
sugarcrop.price.data <- price.data.table %>% filter(sector == 'SugarCrop')
wheat.price.data     <- price.data.table %>% filter(sector == 'Wheat')

# Select regions and scenarios for plotting
select_region <- c('China','India','USA')
select_scenarios <- c('Reference_SSP2-4p5', 'fert_global_const_15_SSP2-4p5',
                      'carbon_tax_25_5', 'carbon_tax_25_5_fert_global_const_15')
                      # 'fert_global_const_15_fert_io_flat', 'fert_global_const_15_fert_io_steep', 'fert_global_const_15_fert_io_regional')

# Summarize price data for crops by adding rows for global region
# and estimating percentage change since 2015 (first year among the future years)
corn.price.data      <- summarize_crop_price_data(corn.price.data, select_scenarios)
fibercrop.price.data <- summarize_crop_price_data(fibercrop.price.data, select_scenarios)
oilcrop.price.data   <- summarize_crop_price_data(oilcrop.price.data, select_scenarios)
rice.price.data      <- summarize_crop_price_data(rice.price.data, select_scenarios)
sugarcrop.price.data <- summarize_crop_price_data(sugarcrop.price.data, select_scenarios)
wheat.price.data     <- summarize_crop_price_data(wheat.price.data, select_scenarios)

# Define path to output file
out.folder <- '../figures/'
out.fname  <- 'Figure7.pdf'
out.fname  <- paste(out.folder, out.fname, sep='')
# Delete existing file
unlink(out.fname)
# Start pdf device driver for saving plots
pdf(out.fname, height=8.5,width=11)

select_scenarios <- c('Reference', 
                      'Global constraint (15%) on fertilizer')

p1 <- plot_line_facet(filter(corn.price.data, region %in% c('China','India','USA'),
                             scenario %in% select_scenarios,
                             year >=2005),
                      x_col      = 'year',
                      y_col      = 'value',
                      group_col  = 'scenario',
                      color_col  = 'scenario',
                      linetype_col = NULL,
                      facet_var  = 'region', 
                      facet_ncol = 2,
                      facet_scale = 'fixed',
                      leg_nrow   = 1,
                      x_lab      = NULL,
                      y_lab      = expression(paste(2010~'$',~ton^-1)), 
                      plot_title = 'Corn prices')

print (p1)

p2 <- plot_line_facet(filter(fibercrop.price.data, region %in% c('China','India','USA'),
                             scenario %in% select_scenarios,
                             year >=2005),
                      x_col      = 'year',
                      y_col      = 'value',
                      group_col  = 'scenario',
                      color_col  = 'scenario',
                      linetype_col = NULL,
                      facet_var  = 'region', 
                      facet_ncol = 2,
                      facet_scale = 'fixed',
                      leg_nrow   = 1,
                      x_lab      = NULL,
                      y_lab      = expression(paste(2010~'$',~ton^-1)), 
                      plot_title = 'Fibercrop prices')

print (p2)

p3 <- plot_line_facet(filter(oilcrop.price.data, region %in% c('China','India','USA'),
                             scenario %in% select_scenarios,
                             year >=2005),
                      x_col      = 'year',
                      y_col      = 'value',
                      group_col  = 'scenario',
                      color_col  = 'scenario',
                      linetype_col = NULL,
                      facet_var  = 'region', 
                      facet_ncol = 2,
                      facet_scale = 'fixed',
                      leg_nrow   = 1,
                      x_lab      = NULL,
                      y_lab      = expression(paste(2010~'$',~ton^-1)), 
                      plot_title = 'OilCrop prices')

print (p3)

print(filter(rice.price.data, 
             year %in% c(2055, 2060, 2065), 
             sector == 'Rice', 
             region == 'China', 
             scenario %in% c('Reference','Global constraint (15%) on fertilizer')))

print(filter(rice.price.data, 
             year %in% c(2055, 2060, 2065), 
             sector == 'Rice', 
             region == 'India', 
             scenario %in% c('Reference','Global constraint (15%) on fertilizer')))


p4 <- plot_line_facet(filter(rice.price.data, region %in% c('China','India','USA'),
                             scenario %in% select_scenarios,
                             year >=2005),
                      x_col      = 'year',
                      y_col      = 'value',
                      group_col  = 'scenario',
                      color_col  = 'scenario',
                      linetype_col = NULL,
                      facet_var  = 'region', 
                      facet_ncol = 2,
                      facet_scale = 'fixed',
                      leg_nrow   = 1,
                      x_lab      = NULL,
                      y_lab      = expression(paste(2010~'$',~ton^-1)), 
                      plot_title = 'Rice prices')

print (p4)

p5 <- plot_line_facet(filter(sugarcrop.price.data, region %in% c('China','India','USA'),
                             scenario %in% select_scenarios,
                             year >=2005),
                      x_col      = 'year',
                      y_col      = 'value',
                      group_col  = 'scenario',
                      color_col  = 'scenario',
                      linetype_col = NULL,
                      facet_var  = 'region', 
                      facet_ncol = 2,
                      facet_scale = 'fixed',
                      leg_nrow   = 1,
                      x_lab      = NULL,
                      y_lab      = expression(paste(2010~'$',~ton^-1)), 
                      plot_title = 'SugarCrop prices')

print (p5)

p6 <- plot_line_facet(filter(wheat.price.data, region %in% c('China','India','USA'),
                             scenario %in% select_scenarios,
                             year >=2005),
                      x_col      = 'year',
                      y_col      = 'value',
                      group_col  = 'scenario',
                      color_col  = 'scenario',
                      linetype_col = NULL,
                      facet_var  = 'region', 
                      facet_ncol = 2,
                      facet_scale = 'fixed',
                      leg_nrow   = 1,
                      x_lab      = NULL,
                      y_lab      = expression(paste(2010~'$',~ton^-1)), 
                      plot_title = 'Wheat prices')

print (p6)

p7 <- plot_line_facet(filter(wheat.price.data, region %in% c('China','India','USA'),
                             scenario %in% c(select_scenarios,
                                             'Low carbon',
                                             'Low carbon with global constraint (15%)'),
                             year >=2005),
                      x_col      = 'year',
                      y_col      = 'value',
                      group_col  = 'scenario',
                      color_col  = 'scenario',
                      linetype_col = NULL,
                      facet_var  = 'region', 
                      facet_ncol = 2,
                      facet_scale = 'fixed',
                      leg_nrow   = 1,
                      x_lab      = NULL,
                      y_lab      = expression(paste(2010~'$',~ton^-1)), 
                      plot_title = 'Wheat prices')

print (p7)

dev.off()
