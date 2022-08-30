# Author - Eva Sinha, Pacific Northwest National Lab, eva.sinha@pnnl.gov

library(rgcam)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(readxl)
library(RColorBrewer)
library(colorRamps)

source('read_summarise_queries.R')
source('plot_vector.R')
source('color_pal_labels.R')

#________________________________________________________
# Add subregion column, irrigation, and agricultural technology column
add_S_SR_T <- function (data.table){
  
  data.table  <- data.table %>%
    mutate(sector    = sapply(str_split(technology, '_'), function(x){ifelse(length(x)==4, x[1], paste(x[1], x[2], sep='_'))}),
           subregion = sapply(str_split(technology, '_'), function(x){ifelse(length(x)==4, x[2], x[3])}),
           ag_tech   = sapply(str_split(technology, '_'), function(x){ifelse(length(x)==4, paste(x[3], x[4], sep='_'), paste(x[4], x[5], sep='_'))}))
  
  # Add long label
  data.table$ag_tech <- param_labeller('variable', data.table$ag_tech)
  
  # Reorder factor levels
  data.table$ag_tech <- factor(data.table$ag_tech, levels=c('Irrigated high-yield',
                                                            'Irrigated low-yield',
                                                            'Rainfed high-yield',
                                                            'Rainfed low-yield'))
  
  return(data.table)
}

#________________________________________________________
# Make timeseries of for particular region, sector, and subregion 
# facetted by ag_tech and color by scenario
plot_timeseries_selected <- function(plot.data.table, select_scenarios, select_ag_tech,
                                     select_regions, select_subregion, select_sector, 
                                     x_col, y_col, color_var, facet_var, y_lab,  plot_title) {
  
  # Subset data for plotting
  plot.data.table <- plot.data.table %>%
    filter(scenario  %in% select_scenarios,
           ag_tech   %in% select_ag_tech,
           region    %in% select_regions, 
           subregion %in% select_subregion,
           sector    %in% select_sector)
  
  print(nrow(plot.data.table))
  
  # Make point and line plot from data
  p1 <- plot_line_facet(filter(plot.data.table, year>=2015), 
                        x_col,
                        y_col,
                        group_col   = 'scenario',
                        color_var,
                        linetype    = NULL,
                        facet_var, 
                        facet_ncol  = 2,  
                        facet_scale = 'fixed',
                        leg_nrow    = 1,
                        x_lab       = NULL,
                        y_lab ,
                        plot_title  = paste(plot_title, '\nRegion - ', select_regions, '; Sector - ', select_sector, '; Subregion - ', select_subregion, sep=''))
  
  print (p1)
  
}

#________________________________________________________
# Make timeseries of for particular region, sector, and subregion 
# facetted by ag_tech and color by scenario
plot_yield_fert_selected <- function(fert.cons.data.table, land.alloc.data.table, yield.data.table, prod.data.table, 
                                     select_scenarios, select_ag_tech,
                                     select_regions, select_subregion, select_sector, 
                                     x_col, y_col, color_var, facet_var, x_lab, y_lab,  plot_title) {
  
  # Combine fert and cropland area and estimation fertilizer application rate
  plot.data.table <- inner_join(select(fert.cons.data.table, -Units), 
                                select(land.alloc.data.table, -Units), 
                                by=c('scenario','region','technology','year','sector','subregion','ag_tech')) %>%
    mutate(fert_kgN_ha = fert_MtN*10^9 / (area_thous_km2 * 1000*100))
  
  # Combine fertilizer application rate and yield
  plot.data.table <- inner_join(plot.data.table, 
                                select(yield.data.table, -Units), 
                                by=c('scenario','region','sector','subsector','technology','year','subregion','ag_tech'))
  
  # Combine agricultural production
  plot.data.table <- inner_join(plot.data.table, 
                                select(prod.data.table, -Units), 
                                by=c('scenario','region','sector','subsector','technology','year','subregion','ag_tech'))
  
  # Subset data for plotting
  plot.data.table <- plot.data.table %>%
    filter(scenario  %in% select_scenarios,
           ag_tech   %in% select_ag_tech,
           region    %in% select_regions, 
           subregion %in% select_subregion,
           sector    %in% select_sector)
  
  # Make point and line plot from data
  p1 <- plot_line_facet(filter(plot.data.table, year>=2015), 
                        x_col       = 'fert_kgN_ha',
                        y_col       = 'yield_Mg_ha',
                        group_col   = 'scenario',
                        color_var,
                        linetype    = NULL,
                        facet_var, 
                        facet_ncol  = 2,  
                        facet_scale = 'fixed',
                        leg_nrow    = 1,  
                        x_lab,
                        y_lab ,
                        plot_title  = paste(plot_title, '\nRegion - ', select_regions, '; Sector - ', select_sector, '; Subregion - ', select_subregion, sep=''))
  
  p1 <- p1 + scale_x_continuous(breaks= scales::pretty_breaks())
  
  print (p1)
  
  # Sum fertilizer usage and area for subregion and estimate fertilizer application rate
  plot.data.table <- plot.data.table %>%
    group_by(scenario, region, sector, subsector, year, subregion) %>%
    summarise(fert_MtN       = sum(fert_MtN),
              area_thous_km2 = sum(area_thous_km2),
              prod_Mt        = sum(prod_Mt)) %>%
    mutate(fert_kgN_ha = fert_MtN*10^9 / (area_thous_km2 * 1000*100),
           yield_Mg_ha = prod_Mt*10^6 / (area_thous_km2 * 1000*100))
  
  print(plot.data.table %>% filter(year %in% c(2060, 2100)))
  
  # Make point and line plot from data
  p2 <- plot_line(filter(plot.data.table, year>=2015), 
                  x_col       = 'fert_kgN_ha',
                  y_col       = 'yield_Mg_ha',
                  group_col   = 'scenario',
                  color_var,
                  leg_nrow    = 1, 
                  x_lab,
                  y_lab,
                  plot_title  = paste(plot_title, '\nRegion - ', select_regions, '; Sector - ', select_sector, '; Subregion - ', select_subregion, sep=''))
  
  p2 <- p2 + scale_x_continuous(breaks= scales::pretty_breaks()) + theme(aspect.ratio = 1)
  
  print (p2)
  
  # Make point and line plot from data
  p3 <- plot_point_facet_cont_color(filter(plot.data.table, year>=2015), 
                                    x_col       = 'fert_kgN_ha',
                                    y_col       = 'yield_Mg_ha',
                                    color_col   = 'year',
                                    shape_col   = NULL,
                                    facet_var   = 'scenario',
                                    facet_ncol  = 2, 
                                    facet_scale = 'fixed',
                                    leg_nrow   = 1,
                                    color_breaks = seq(2015, 2100, 10),
                                    x_lab,
                                    y_lab,
                                    plot_title = paste(plot_title, '\nRegion - ', select_regions, '; Sector - ', select_sector, '; Subregion - ', select_subregion, sep=''))
  
  p3 <- p3 + geom_point(size=4) + 
    theme(aspect.ratio = 1,
          panel.grid.major = element_line(colour = 'grey80'))

  print (p3)
  
}

#________________________________________________________

# Load a previously generated project data file
# prj.name       <- 'fert_constrain.dat'
# prj.name       <- 'fert_io_coeff_SA.dat'
prj.name       <- 'fert_const_updated_IO_coeff.dat'
prj            <- loadProject(prj.name)

# List all scenarios
print(listScenarios(prj))

# ----- Query fertilizer consumption
fert.cons.data.table  <- getQuery(prj, query='fertilizer consumption by ag tech')

# ----- Query aggregated land allocation
land.alloc.data.table <- getQuery(prj, query = 'detailed land allocation')

land.alloc.data.table <- rename(land.alloc.data.table, technology = landleaf)

# ----- Query yield by technology
yield.data.table     <- getQuery(prj, query = 'ag tech yield')

# ----- Query agricultural production by technology
prod.data.table  <- getQuery(prj, query = 'ag production by tech')

# Convert yield to kg/ha and rename column
yield.data.table <- yield.data.table %>%
  mutate(value = value * 10) %>%
  rename(yield_Mg_ha = value)

# Add subregion column, irrigation, and agricultural technology column
fert.cons.data.table  <- add_S_SR_T(fert.cons.data.table)
land.alloc.data.table <- add_S_SR_T(land.alloc.data.table)
yield.data.table      <- add_S_SR_T(yield.data.table)
prod.data.table       <- add_S_SR_T(prod.data.table)

# Add long label for scenarios
fert.cons.data.table  <- add_long_label_scenarios(fert.cons.data.table)
land.alloc.data.table <- add_long_label_scenarios(land.alloc.data.table)
yield.data.table      <- add_long_label_scenarios(yield.data.table)
prod.data.table       <- add_long_label_scenarios(prod.data.table)

# Drop unused levels
fert.cons.data.table$scenario  <- droplevels(fert.cons.data.table$scenario)
land.alloc.data.table$scenario <- droplevels(land.alloc.data.table$scenario)
yield.data.table$scenario      <- droplevels(yield.data.table$scenario)
prod.data.table$scenario       <- droplevels(prod.data.table$scenario)

# Selection for plotting
select_scenarios <- c('Reference', 'Global constraint (15%) on fertilizer') 
select_ag_tech   <- c('Rainfed high-yield','Rainfed low-yield')
select_regions   <- 'China'     # 'USA'
select_subregion <- 'XunJiang'  # 'MissppRN'
select_sector    <- 'SugarCrop' # 'Corn'

# Define path to output file
out.folder <- '../figures/'
# out.fname  <- 'FigureS11_Corn_MissppRN.pdf'
out.fname  <- 'FigureS11_SugarCrop_XunJiang.pdf'
out.fname  <- paste(out.folder, out.fname, sep='')
# Delete existing file
unlink(out.fname)
# Start pdf device driver for saving plots
pdf(out.fname, height=8.5,width=11)

plot_yield_fert_selected(fert.cons.data.table = rename(fert.cons.data.table, fert_MtN = value),
                         land.alloc.data.table = rename(land.alloc.data.table, area_thous_km2 = value),
                         yield.data.table,
                         prod.data.table = rename(prod.data.table, prod_Mt = value),
                         select_scenarios,
                         select_ag_tech,
                         select_regions,
                         select_subregion,
                         select_sector,
                         color_var  = 'scenario',
                         facet_var  = 'ag_tech',
                         x_lab      = expression(paste(Fertilizer~consumption~'[',kgN~ha^-1,']')),
                         y_lab      = expression(paste(Yield~'[',Mg~ha^-1,']')),
                         plot_title = 'Yield vs. fertilizer consumption')

plot_timeseries_selected(fert.cons.data.table,
                         select_scenarios,
                         select_ag_tech,
                         select_regions,
                         select_subregion,
                         select_sector,
                         x_col      = 'year',
                         y_col      = 'value',
                         color_var  = 'scenario',
                         facet_var  = 'ag_tech',
                         y_lab      = expression(paste(Fertilizer~consumption~'[',MtN~yr^-1,']')),
                         plot_title = 'Timeseries of fertilizer consumption')

plot_timeseries_selected(yield.data.table,
                         select_scenarios,
                         select_ag_tech,
                         select_regions,
                         select_subregion,
                         select_sector,
                         x_col      = 'year',
                         y_col      = 'yield_Mg_ha',
                         color_var  = 'scenario',
                         facet_var  = 'ag_tech',
                         y_lab      = expression(paste(Yield~'[',Mg~ha^-1,']')),
                         plot_title = 'Timeseries of yield')

plot_timeseries_selected(land.alloc.data.table,
                         select_scenarios,
                         select_ag_tech,
                         select_regions,
                         select_subregion,
                         select_sector,
                         x_col      = 'year',
                         y_col      = 'value',
                         color_var  = 'scenario',
                         facet_var  = 'ag_tech',
                         y_lab      = expression(paste(Cropland~area~'[',thous~km^2,']')),
                         plot_title = 'Timeseries of cropland area')

plot_timeseries_selected(prod.data.table,
                         select_scenarios,
                         select_ag_tech,
                         select_regions,
                         select_subregion,
                         select_sector,
                         x_col      = 'year',
                         y_col      = 'value',
                         color_var  = 'scenario',
                         facet_var  = 'ag_tech',
                         y_lab      = 'Production [Mt]',
                         plot_title = 'Timeseries of agricultural production')
  
dev.off()
