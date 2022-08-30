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

# Function details
# plot_xy_curve_facet_scenario   - Make scatterplot of for particular region, sector, and subregion 
#                                  facetted by scenario
# plot_timeseries_facet_scenario - Make timeseries of for particular region, sector, and subregion 
#                                  facetted by scenario

#________________________________________________________
# Make scatterplot of for particular region, sector, and subregion 
# facetted by scenario
plot_xy_curve_facet_scenario <- function(x.data.table, y.data.table, field.data,
                                         select_regions, select_sector, 
                                         select_subregion, select_ag_tech,
                                         x_col, y_col, size_var, facet_var, x_lab, y_lab,  plot_title) {
  
  # Subset data for plotting
  x.data.table  <- filter(x.data.table, region %in% select_regions)
  y.data.table  <- filter(y.data.table, region %in% select_regions)
  
  # Merge into a single table for plotting
  plot.data.table <- inner_join(x.data.table, y.data.table, by=c('scenario','region','sector','technology','year','ag_tech'))
  
  # Add subregion column, irrigation, and agricultural technology column
  plot.data.table  <- plot.data.table %>%
    mutate(sector    = sapply(str_split(technology, '_'), function(x){ifelse(length(x)==4, x[1], paste(x[1], x[2], sep='_'))}),
           subregion = sapply(str_split(technology, '_'), function(x){ifelse(length(x)==4, x[2], x[3])}),
           ag_tech   = sapply(str_split(technology, '_'), function(x){ifelse(length(x)==4, paste(x[3], x[4], sep='_'), paste(x[4], x[5], sep='_'))}))
  
  # Only keep data for a single sector and single subregion
  plot.data.table <- filter(plot.data.table, sector %in% select_sector, subregion  %in% select_subregion, ag_tech %in% select_ag_tech)
  
  # Add long label
  plot.data.table$ag_tech <- param_labeller('variable', plot.data.table$ag_tech)
  
  # Reorder factor levels
  plot.data.table$ag_tech <- factor(plot.data.table$ag_tech, levels=c('Irrigated high-yield',
                                                                      'Irrigated low-yield',
                                                                      'Rainfed high-yield',
                                                                      'Rainfed low-yield'))
  
  # Make point and line plot from data
  p1 <- plot_point_facet_cont_color(plot.data.table,
                                    x_col,
                                    y_col,
                                    color_col  = 'year',
                                    shape_col  = 'ag_tech',
                                    facet_var,
                                    facet_ncol = 4, 
                                    facet_scale = 'fixed',
                                    leg_nrow   = 1,
                                    color_breaks = seq(1975, 2100, 20),
                                    x_lab,
                                    y_lab,
                                    plot_title = paste(plot_title, '\nRegion - ', paste(select_regions, collapse=', '), '; Sector - ', select_sector, '; Subregion - ', paste(select_subregion, collapse=', '), sep=''))
  
  if(is.null(size_var)) {
    p1 <- p1 + geom_point(size=4) + theme(aspect.ratio = 1)
  } else {
    p1 <- p1 + geom_point(aes_string(size=size_var)) + theme(aspect.ratio = 1) + 
      scale_size_manual(values = c(3, 4, 5, 6)) +
      guides(size = guide_legend(title=NULL, nrow=2))
  }
  
  if(!is.null(field.data)) {
    p1 <- p1 +  geom_point(data=field.data, size=3, color='black', shape=16)
  }
  
  print (p1)
  
}

#________________________________________________________
# Make timeseries of for particular region, sector, and subregion 
# facetted by scenario
plot_timeseries_facet_scenario <- function(plot.data.table, 
                                           select_regions, select_sector, 
                                           select_subregion, select_ag_tech,
                                           x_col, y_col, facet_var, x_lab, y_lab,  plot_title) {
  
  # Subset data for plotting
  plot.data.table  <- filter(plot.data.table, region %in% select_regions)
  
  # Add subregion column, irrigation, and agricultural technology column
  plot.data.table  <- plot.data.table %>%
    mutate(sector    = sapply(str_split(technology, '_'), function(x){ifelse(length(x)==4, x[1], paste(x[1], x[2], sep='_'))}),
           subregion = sapply(str_split(technology, '_'), function(x){ifelse(length(x)==4, x[2], x[3])}),
           ag_tech   = sapply(str_split(technology, '_'), function(x){ifelse(length(x)==4, paste(x[3], x[4], sep='_'), paste(x[4], x[5], sep='_'))}))
  
  # Only keep data for a single sector and single subregion
  plot.data.table <- filter(plot.data.table, sector == select_sector, subregion == select_subregion, ag_tech %in% select_ag_tech)
  
  # Add long label
  plot.data.table$ag_tech <- param_labeller('variable', plot.data.table$ag_tech)
  
  # Reorder factor levels
  plot.data.table$ag_tech <- factor(plot.data.table$ag_tech, levels=c('Irrigated high-yield',
                                                                      'Irrigated low-yield',
                                                                      'Rainfed high-yield',
                                                                      'Rainfed low-yield'))
  
  # Make point and line plot from data
  p1 <- plot_point_facet_cont_color(plot.data.table,
                                    x_col,
                                    y_col,
                                    color_col  = 'year',
                                    shape_col  = 'ag_tech',
                                    facet_var,
                                    facet_ncol = 4, 
                                    facet_scale = 'fixed',
                                    leg_nrow   = 1,
                                    color_breaks = seq(1975, 2100, 20),
                                    x_lab,
                                    y_lab,
                                    plot_title = paste(plot_title, '\nRegion - ', select_regions, '; Sector - ', select_sector, '; Subregion - ', select_subregion, sep=''))
  
  p1 <- p1 + geom_point(size=4) + theme(aspect.ratio = 1)
  
  print (p1)
  
}

#________________________________________________________
# Make timeseries of for particular region, sector, and subregion 
# facetted by ag_tech and color by scenario
plot_timeseries_color_scenario <- function(plot.data.table, 
                                           select_regions, select_sector, 
                                           select_subregion, select_ag_tech,
                                           x_col, y_col, color_var, facet_var, x_lab, y_lab,  plot_title) {
  
  # Subset data for plotting
  plot.data.table  <- filter(plot.data.table, region %in% select_regions)
  
  # Add subregion column, irrigation, and agricultural technology column
  plot.data.table  <- plot.data.table %>%
    mutate(sector    = sapply(str_split(technology, '_'), function(x){ifelse(length(x)==4, x[1], paste(x[1], x[2], sep='_'))}),
           subregion = sapply(str_split(technology, '_'), function(x){ifelse(length(x)==4, x[2], x[3])}),
           ag_tech   = sapply(str_split(technology, '_'), function(x){ifelse(length(x)==4, paste(x[3], x[4], sep='_'), paste(x[4], x[5], sep='_'))}))
  
  # Only keep data for a single sector and single subregion
  plot.data.table <- filter(plot.data.table, sector == select_sector, subregion == select_subregion, ag_tech %in% select_ag_tech)
  
  # Add long label
  plot.data.table$ag_tech <- param_labeller('variable', plot.data.table$ag_tech)
  
  # Reorder factor levels
  plot.data.table$ag_tech <- factor(plot.data.table$ag_tech, levels=c('Irrigated high-yield',
                                                                      'Irrigated low-yield',
                                                                      'Rainfed high-yield',
                                                                      'Rainfed low-yield'))
  
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
                        x_lab,
                        y_lab,
                        plot_title  = paste(plot_title, '\nRegion - ', select_regions, '; Sector - ', select_sector, '; Subregion - ', select_subregion, sep=''))
  
  p1 <- p1 + theme(aspect.ratio = 1)
  
  print (p1)
  
  # Sum fertilizer usage and area for subregion and estimate fertilizer application rate
  plot.data.table <- plot.data.table %>%
    group_by(scenario, region, sector, year, subregion) %>%
    summarise(fert_MtN = sum(fert_MtN),
              area_thous_km2 = sum(area_thous_km2)) %>%
  mutate(fert_kgN_ha = fert_MtN*10^9 / (area_thous_km2 * 1000*100))
  
  # Make point and line plot from data
  p2 <- plot_line(filter(plot.data.table, year>=2015), 
                  x_col,
                  y_col,
                  group_col   = 'scenario',
                  color_var,
                  leg_nrow    = 1,  
                  x_lab       = NULL,
                  y_lab,
                  plot_title  = paste(plot_title, '\nRegion - ', select_regions, '; Sector - ', select_sector, '; Subregion - ', select_subregion, sep=''))
  
  p2 <- p2 + theme(aspect.ratio = 1)
  
  print (p2)
}

#________________________________________________________
# Load a previously generated project data file
prj.name       <- 'fert_const_updated_IO_coeff.dat'
prj            <- loadProject(prj.name)

# List all scenarios
print(listScenarios(prj))

# Query fertilizer consumption and land allocation by agricultural technologies
# to estimate fertilizer application rate [Units: kgN/ha]
fert.rate.table  <- estimate_fert_application_rate(prj,
                                                  query_fert_consump = 'fertilizer consumption by ag tech',
                                                  query_land         = 'detailed land allocation')

# Query yield by technology and select for specific crops [Units: XX]
yield.data.table <- estimate_yield_ag_tech_select_crop(prj, query_yield = 'ag tech yield')

# Query agricultural production and estimate for specific crop [Units: Mt]
prod.data.table  <- estimate_prod_select_crop(prj, query_production = 'ag production by tech')

# Subset production data to only keep data for crops and add ag_tech column
prod.data.table  <- prod.data.table %>%
                    select(-subsector) %>%
                    filter( ! sector %in% c('Forest', 'Pasture')) %>%
                    mutate(ag_tech = substr(technology, nchar(technology)-1, nchar(technology)))

# Read yield vs N fertilizer application data based on field measurements
in.folder  <- '../Yield_fert_data/'
f.name     <- 'literature_cropyield_Nfert.csv'
field.data <- read.csv(paste(in.folder, f.name, sep=''))
field.data <- filter(field.data, region == 'USA') %>% 
              transform(year     = 2013,
                        ag_tech  = 'Unknown',
                        subregion = 'MissppRN')

# Define path to output file
out.folder <- '../figures/'
out.fname  <- 'Figure1.pdf'
out.fname  <- paste(out.folder, out.fname, sep='')
# Delete existing file
unlink(out.fname)
# Start pdf device driver for saving plots
pdf(out.fname, height=8.5, width=17)


select_scenarios <- c('Reference', 
                      'Global constraint (15%) on fertilizer')
                      # 'Global constraint (15%) - fertilizer io flat', 
                      # 'Global constraint (15%) - fertilizer io steep')

# Make scatterplot of for particular region, sector, and subregion 
# facetted by scenario
plot_xy_curve_facet_scenario(x.data.table = filter(fert.rate.table,  scenario == 'Reference') %>% 
                                            select(-fert_MtN, -area_thous_km2),
                             y.data.table = filter(yield.data.table,  scenario == 'Reference'),
                             field.data,
                             select_regions   = 'USA',
                             select_sector    = 'Corn',
                             select_subregion = 'MissppRN',
                             select_ag_tech   = c('RFD_hi', 'RFD_lo'),
                             x_col      = 'fert_kgN_ha',
                             y_col      = 'yield_Mg_ha',
                             size_var   = NULL, 
                             facet_var  = 'scenario',
                             x_lab      = expression(paste(Fertilizer~application~rate~'[',kgN~ha^-1,']')),
                             y_lab      = expression(paste(Yield~'[',Mg~ha^-1,']')),
                             plot_title = 'Yield vs. nitrogen fertilizer application rate')

plot_xy_curve_facet_scenario(x.data.table = filter(fert.rate.table,  scenario == 'Reference') %>% 
                               select(-fert_MtN, -area_thous_km2),
                             y.data.table = filter(yield.data.table,  scenario == 'Reference'),
                             field.data,
                             select_regions   = c('USA', 'China'),
                             select_sector    = 'Corn',
                             select_subregion = c('MissppRN', 'Yangtze'),
                             select_ag_tech   = c('RFD_hi', 'RFD_lo'),
                             x_col      = 'fert_kgN_ha',
                             y_col      = 'yield_Mg_ha',
                             size_var   = NULL, 
                             facet_var  = 'subregion',
                             x_lab      = expression(paste(Fertilizer~application~rate~'[',kgN~ha^-1,']')),
                             y_lab      = expression(paste(Yield~'[',Mg~ha^-1,']')),
                             plot_title = 'Yield vs. nitrogen fertilizer application rate')

plot_xy_curve_facet_scenario(x.data.table = filter(fert.rate.table,  scenario %in% select_scenarios) %>% 
                                            select(-fert_MtN, -area_thous_km2),
                             y.data.table = filter(yield.data.table,  scenario %in% select_scenarios),
                             field.data,
                             select_regions   = 'USA',
                             select_sector    = 'Corn',
                             select_subregion = 'MissppRN',
                             select_ag_tech   = c('RFD_hi', 'RFD_lo'),
                             x_col      = 'fert_kgN_ha',
                             y_col      = 'yield_Mg_ha',
                             size_var   = NULL, 
                             facet_var  = 'scenario',
                             x_lab      = expression(paste(Fertilizer~application~rate~'[',kgN~ha^-1,']')),
                             y_lab      = expression(paste(Yield~'[',Mg~ha^-1,']')),
                             plot_title = 'Yield vs. nitrogen fertilizer application rate')


plot_xy_curve_facet_scenario(x.data.table = filter(fert.rate.table,  scenario %in% select_scenarios) %>% 
                               select(-fert_kgN_ha, -area_thous_km2),
                             y.data.table = filter(prod.data.table,  scenario %in% select_scenarios),
                             field.data   = NULL,
                             select_regions   = 'USA',
                             select_sector    = 'Corn',
                             select_subregion = 'MissppRN',
                             select_ag_tech   = c('RFD_hi', 'RFD_lo'),
                             x_col      = 'fert_MtN',
                             y_col      = 'prod_Mt',
                             size_var   = NULL,
                             facet_var  = 'scenario',
                             x_lab      = expression(paste(Fertilizer~consumption~'[',MtN~yr^-1,']')),
                             y_lab      = expression(paste(Agricultural~production~'[',Mt~yr^-1,']')),
                             plot_title = 'Agricultural production vs. N fertilizer consumption')

plot_xy_curve_facet_scenario(x.data.table = filter(fert.rate.table,  scenario %in% select_scenarios) %>% 
                                            select(-fert_MtN, -area_thous_km2),
                             y.data.table = filter(yield.data.table,  scenario %in% select_scenarios),
                             field.data   = NULL,
                             select_regions   = 'USA',
                             select_sector    = 'biomassGrass',
                             select_subregion = 'MissppRN',
                             select_ag_tech   = c('RFD_hi', 'RFD_lo'),
                             x_col      = 'fert_kgN_ha',
                             y_col      = 'yield_Mg_ha',
                             size_var   = NULL,
                             facet_var  = 'scenario',
                             x_lab      = expression(paste(Fertilizer~application~rate~'[',kgN~ha^-1,']')),
                             y_lab      = expression(paste(Yield~'[',Mg~ha^-1,']')),
                             plot_title = 'Yield vs. nitrogen fertilizer application rate')

plot_xy_curve_facet_scenario(x.data.table = filter(fert.rate.table,  scenario %in% select_scenarios) %>% 
                                                 select(-fert_kgN_ha, -area_thous_km2),
                             y.data.table = filter(prod.data.table,  scenario %in% select_scenarios),
                             field.data   = NULL,
                             select_regions   = 'USA',
                             select_sector    = 'biomassGrass',
                             select_subregion = 'ArkWhtRedR',
                             select_ag_tech   = c('RFD_hi', 'RFD_lo'),
                             x_col      = 'fert_MtN',
                             y_col      = 'prod_Mt',
                             size_var   = NULL,
                             facet_var  = 'scenario',
                             x_lab      = expression(paste(Fertilizer~consumption~'[',MtN~yr^-1,']')),
                             y_lab      = expression(paste(Agricultural~production~'[',Mt~yr^-1,']')),
                             plot_title = 'Agricultural production vs. N fertilizer consumption')

# Make timeseries of for particular region, sector, and subregion 
# facetted by scenario
plot_timeseries_facet_scenario(plot.data.table = filter(fert.rate.table,  scenario %in% select_scenarios) %>% 
                                                 select(-fert_kgN_ha, -fert_MtN),
                               select_regions   = 'USA',
                               select_sector    = 'Corn',
                               select_subregion = 'MissppRN',
                               select_ag_tech = c('RFD_hi', 'RFD_lo'),
                               x_col      = 'year',
                               y_col      = 'area_thous_km2',
                               facet_var  = 'scenario',
                               x_lab      = NULL,
                               y_lab      = expression(paste(Area~'[',thousands~km^2,']')),
                               plot_title = 'Timeseries of area')

plot_timeseries_facet_scenario(plot.data.table = filter(fert.rate.table,  scenario %in% select_scenarios) %>% 
                                 select(-area_thous_km2, -fert_kgN_ha),
                               select_regions   = 'USA',
                               select_sector    = 'Corn',
                               select_subregion = 'MissouriR',
                               select_ag_tech = c('RFD_hi', 'RFD_lo'),
                               x_col      = 'year',
                               y_col      = 'fert_MtN',
                               facet_var  = 'scenario',
                               x_lab      = NULL,
                               y_lab      = expression(paste(Fertilizer~consumption~'[',MtN~yr^-1,']')),
                               plot_title = 'Timeseries of fertilizer consumption')

plot_timeseries_color_scenario(plot.data.table = filter(fert.rate.table,  scenario %in% select_scenarios),
                               select_regions   = 'USA',
                               select_sector    = 'Corn',
                               select_subregion = 'MissouriR',
                               select_ag_tech = c('RFD_hi', 'RFD_lo'),
                               x_col      = 'year',
                               y_col      = 'fert_kgN_ha',
                               color_var  = 'scenario',
                               facet_var  = 'ag_tech',
                               x_lab      = NULL,
                               y_lab      = expression(paste(Fertilizer~application~rate~'[',kgN~ha^-1,']')),
                               plot_title = 'Timeseries of fertilizer application rate')

plot_timeseries_facet_scenario(plot.data.table = filter(fert.rate.table,  scenario %in% select_scenarios) %>% 
                                 select(-area_thous_km2, -fert_kgN_ha),
                               select_regions   = 'India',
                               select_sector    = 'Rice',
                               select_subregion = 'KrishnaR',
                               select_ag_tech = c('RFD_hi', 'RFD_lo'),
                               x_col      = 'year',
                               y_col      = 'fert_MtN',
                               facet_var  = 'scenario',
                               x_lab      = NULL,
                               y_lab      = expression(paste(Fertilizer~consumption~'[',MtN~yr^-1,']')),
                               plot_title = 'Timeseries of fertilizer consumption')



dev.off()
