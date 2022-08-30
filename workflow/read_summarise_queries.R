# Author - Eva Sinha, Pacific Northwest National Lab, eva.sinha@pnnl.gov

library(rgcam)
library(dplyr)
library(tidyr)
library(forcats) # fct_reorder

# Function details
# add_long_label_scenario            - Add long label for scenarios
# estimate_value_subset_sum_query    - Estimate value of variable by subsetting query data and summing across specified type
# estimate_value_query               - Estimate value of query
# estimate_yield                     - Query land and agrcultural production and estimate yield for crops [Units: kg/ha]
# estimate_yield_select_crop         - Query land allocation and agricultural production and estimate
#                                      yield data for specific crop [Units: Mg/ha]
# estimate_yield_ag_tech_select_crop - Query yield by technology and select for specific crops [Units: XX]
# estimate_prod_select_crop          - Query agricultural production and estimate for specific crop [Units: Mt]
# estimate_land_alloc_select_crop    - Query land allocation and estimate for specific crop [Units: thous km2]
# estimate_fert_consump_select_crop  - Query fertilizer consumption and estimate for specific crop [Units: MtN]
# estimate_agg_fert_application_rate - Query fertilizer consumption and land allocation by agricultural technologies
#                                      to estimate fertilizer application rate [Units: kgN/ha]
# estimate_fert_application_rate     - Query fertilizer consumption and land allocation by agricultural technologies
#                                      to estimate fertilizer application rate [Units: kgN/ha]
# summarize_change_reference         - Summarize percentage change since 2015 (first year among the future years)

# Make theme_bw the active theme to be automatically applied to every plot
# and adjust all sizes of the theme
theme_set(theme_bw(base_size = 15, base_family = 'Helvetica'))

# Update various text size in the theme
theme_update(panel.grid    = element_blank(),          # Remove all grid lines,
             text          = element_text(size=15, family='Helvetica', color='black'),
             plot.title    = element_text(size=15, family='Helvetica', color='black'),
             axis.title.x  = element_text(size=15, family='Helvetica', color='black'), 
             axis.title.y  = element_text(size=15, family='Helvetica', color='black'), 
             axis.text.x   = element_text(size=15, family='Helvetica', color='black'),
             axis.text.y   = element_text(size=15, family='Helvetica', color='black'),
             strip.text    = element_text(size=15, family='Helvetica', color='black'),
             legend.text   = element_text(size=15, family='Helvetica', color='black'))

#________________________________________________________
# https://github.com/dgrtwo/drlib/blob/master/R/reorder_within.R
reorder_within <- function(x, by, within, fun = mean, sep = '___', ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

#________________________________________________________
scale_x_reordered <- function(..., sep = '___') {
  reg <- paste0(sep, '.+$')
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, '', x), ...)
}

#________________________________________________________
scale_y_reordered <- function(..., sep = '___') {
  reg <- paste0(sep, '.+$')
  ggplot2::scale_y_discrete(labels = function(x) gsub(reg, '', x), ...)
}

#________________________________________________________
# Add long label for scenarios
add_long_label_scenarios <- function(data.table){
  
  # Add long label
  data.table$scenario <- param_labeller('variable', data.table$scenario)
  
  # Reorder factor levels
  data.table$scenario <- factor(data.table$scenario, levels=c('Reference [SSP1-4p5]',
                                                              'Reference',
                                                              'Reference [SSP2-3p7]',
                                                              'Reference [SSP2-6p0]',
                                                              'Reference [SSP3-4p5]',
                                                              'Reference [SSP4-4p5]',
                                                              'Reference [SSP5-4p5]',
                                                              'Reference - fertilizer io flat',
                                                              'Reference - fertilizer io steep',
                                                              'Reference - fertilizer io regional',
                                                              'Reference with lo tech yield',
                                                              'Global constraint (15%) on fertilizer',
                                                              'Global constraint (15%) on fertilizer [SSP2-3p7]',
                                                              'Global constraint (15%) on fertilizer [SSP2-6p0]',
                                                              'Global constraint (15%) on fertilizer [SSP1-4p5]',
                                                              'Global constraint (15%) on fertilizer [SSP3-4p5]',
                                                              'Global constraint (15%) on fertilizer [SSP4-4p5]',
                                                              'Global constraint (15%) on fertilizer [SSP5-4p5]',
                                                              'Global constraint (15%) - fertilizer io flat',
                                                              'Global constraint (15%) - fertilizer io steep',
                                                              'Global constraint (15%) - fertilizer io regional',
                                                              'Global constraint (15%) on fertilizer with lo tech yield',
                                                              'Regional constraint (10%) on fertilizer',
                                                              'Regional constraint (10%) on fertilizer with lo tech yield',
                                                              'Regional constraint (15%) on fertilizer',
                                                              'Regional constraint (15%) on fertilizer with lo tech yield',
                                                              'Regional constraint (20%) on fertilizer',
                                                              'Regional constraint (20%) on fertilizer with lo tech yield',
                                                              'Low carbon',
                                                              'Low carbon with global constraint (15%)'))
  
  return(data.table)
}

#________________________________________________________
# Estimate value of variable by subsetting query data and summing across specified type
estimate_value_subset_sum_query <- function(prj, query,
                                            subselect.colName=NULL, subselect.value=NULL,
                                            sum.colName=NULL){
  
  # Retrieve particular query for all scenarios in the dataset, formatted as a single table
  query.data.table <- getQuery(prj, query)
  
  if (!is.null(subselect.colName)){
    
    query.data.table <- query.data.table %>%
                        filter(!!sym(subselect.colName) %in% subselect.value) %>% # only keep select rows
                        select(-subselect.colName) # drop column for making subset
    
    # print(nrow(query.data.table))
  }
  
  if (!is.null(sum.colName)){
    
    query.data.table <- query.data.table %>%
                        group_by(Units, scenario, region, year) %>% # Summing production across various crop types
                        summarise(value = sum(value))
    
    # print(nrow(query.data.table))
  }
  
  # Add long label for scenarios
  query.data.table <- add_long_label_scenarios(query.data.table)
 
  return(query.data.table)
}

#________________________________________________________
# Estimate value of query
estimate_value_query <- function(prj, query){
  
  # Retrieve particular query for all scenarios in the dataset, formatted as a single table
  query.data.table <- getQuery(prj, query)
  # print(nrow(query.data.table))
  
  # Add long label for scenarios
  query.data.table <- add_long_label_scenarios(query.data.table)
  
  return(query.data.table)
}

#________________________________________________________
# Query land and agrcultural production and estimate yield for crops [Units: kg/ha]
estimate_yield <- function(prj, query_land, query_production){
  
  # Retrieve particular query for all scenarios in the dataset, formatted as a single table
  # nrow(land.data.table) ~ num_scenario * num_region * num_landleaf * num_year
  # nrow(prod.data.table) ~ num_scenario * num_region * num_sector * num_year
  land.data.table <- getQuery(prj, query_land)
  prod.data.table <- getQuery(prj, query_production)
  
  # Only keep data for crops
  # nrow(land.data.table) = num_scenario * num_region * num_year
  # nrow(prod.data.table) ~ num_scenario * num_region * num_crops * num_year
  land.data.table <- filter(land.data.table, landleaf == 'crops')
  prod.data.table <- filter(prod.data.table, sector == 'crops')
  
  # Drop column used for making subset
  land.data.table <- select(land.data.table, -landleaf)
  prod.data.table <- select(prod.data.table, -sector)
  
  # Rename value column
  land.data.table <- rename(land.data.table, area_thous_km2 = value)
  prod.data.table <- rename(prod.data.table, prod_Mt = value)
  
  # Drop Units column
  land.data.table <- select(land.data.table, -Units)
  prod.data.table <- select(prod.data.table, -Units)
  
  # Sum production for various crop types
  # nrow(prod.data.table) ~ num_scenario * num_region * num_year
  prod.data.table <- prod.data.table %>%
                     group_by(scenario, region, year) %>% 
                     summarise(prod_Mt = sum(prod_Mt))
  
  # Merge two tables
  yield.data.table <- full_join(land.data.table, prod.data.table, by=c('scenario','region','year'))
  
  # Estimate values for global region and add as additional rows
  yield.data.table <- bind_rows(yield.data.table, yield.data.table %>% 
                                  group_by(scenario, year) %>% 
                                  summarize(region = 'Global',
                                            area_thous_km2 = sum(area_thous_km2),
                                            prod_Mt = sum(prod_Mt)))
  
  # Estimate yield [Units: kg/ha]
  yield.data.table$yield <- (yield.data.table$prod_Mt * 10^9)/(yield.data.table$area_thous_km2*1000*100)
  
  # Drop prod_Mt and area_thous_km2 columnds
  yield.data.table <- select(yield.data.table, -prod_Mt, -area_thous_km2)
  
  # Add long label for scenarios
  yield.data.table <- add_long_label_scenarios(yield.data.table)
  
  return(yield.data.table)
  
}

#________________________________________________________
# Query land allocation and agricultural production and estimate
# yield data for specific crop [Units: Mg/ha]
estimate_yield_select_crop <- function(prj, query_land, query_production, select_crops){
  
  # Retrieve particular query for all scenarios in the dataset, formatted as a single table
  # nrow(land.data.table) ~ num_scenario * num_region * num_landleaf * num_year
  # nrow(prod.data.table) ~ num_scenario * num_region * num_sector * num_year
  land.data.table <- getQuery(prj, query_land)
  prod.data.table <- getQuery(prj, query_production)
  
  # For land allocation combine biomass_grass and biomass_tree into biomass
  land.data.table <- bind_rows((land.data.table %>%
                                  filter(landleaf != 'biomass_grass' & landleaf != 'biomass_tree')),
                               (land.data.table %>%
                                  filter(landleaf == 'biomass_grass' | landleaf == 'biomass_tree') %>%
                                  group_by(Units, scenario, region, year) %>%
                                  summarise(landleaf='biomass',
                                            value=sum(value))))
  
  # Agricultural production for biomass is provided in units of EJ. Converting it to Mt units
  # Based on Energy content of biomass [GJ/ton] defined in GCAM (constants.R)
  # aglu.BIO_ENERGY_CONTENT_GJT <- 17.5
  # 1 EJ = 1000/17.5 Mt
  prod.data.table <- bind_rows((prod.data.table %>%
                                  filter(sector != 'biomass')),
                               (prod.data.table %>%
                                  filter(sector=='biomass') %>%
                                  mutate(value = (1000/17.5)*value,
                                         Units = 'Mt')))
  
  # Only keep data for select crop
  # nrow(land.data.table) = num_scenario * num_region * num_year
  # nrow(prod.data.table) ~ num_scenario * num_region * num_crops * num_year
  land.data.table <- filter(land.data.table, landleaf %in% select_crops)
  prod.data.table <- filter(prod.data.table, sector %in% select_crops)
  
  # Rename landleaf column in land.data.table
  land.data.table <- rename(land.data.table, sector = landleaf)
  
  # Rename value column
  land.data.table <- rename(land.data.table, area_thous_km2 = value)
  prod.data.table <- rename(prod.data.table, prod_Mt = value)
  
  # Drop Units column
  land.data.table <- select(land.data.table, -Units)
  prod.data.table <- select(prod.data.table, -Units, -output)
  
  # Merge two tables
  yield.data.table <- inner_join(land.data.table, prod.data.table, by=c('scenario','region','sector','year'))
  
  # Estimate yield [Mg/ha]
  # yield.data.table$yield <- (yield.data.table$prod_Mt * 10^9)/(yield.data.table$area_thous_km2*1000) # [kg/km2]
  yield.data.table$yield <- (yield.data.table$prod_Mt * 10^6)/(yield.data.table$area_thous_km2*1000*100)
  
  # Drop prod_Mt and area_thous_km2 columnds
  yield.data.table <- select(yield.data.table, -prod_Mt, -area_thous_km2)
  
  # Add long label for scenarios
  yield.data.table <- add_long_label_scenarios(yield.data.table)
  
  return(yield.data.table)
  
}

#________________________________________________________
# Query yield by technology and select for specific crops [Units: XX]
estimate_yield_ag_tech_select_crop <- function(prj, query_yield){
  
  # Retrieve particular query for all scenarios in the dataset, formatted as a single table
  yield.data.table <- getQuery(prj, query_yield)
  # land.data.table  <- getQuery(prj, query_land)
  
  # Convert yield to Mg/ha
  yield.data.table$value <- 10 * yield.data.table$value
  
  # Rename value column
  yield.data.table <- rename(yield.data.table, yield_Mg_ha = value)
  # land.data.table  <- rename(land.data.table, area_thous_km2 = value, technology = landleaf)
  
  # Drop columns
  yield.data.table <- select(yield.data.table, -Units)
  # land.data.table  <- select(land.data.table, -Units)
  
  # # Merge two tables
  # yield.data.table <- inner_join(yield.data.table, land.data.table, by=c('scenario','region','technology','year'))
  
  # Add column for fertilizer technology (hi or lo)
  yield.data.table <- yield.data.table %>%
                      mutate(ag_tech = substr(technology, nchar(technology)-1, nchar(technology)))
  
  # Add long label for scenarios
  yield.data.table <- add_long_label_scenarios(yield.data.table)
  
  return(yield.data.table)
  
}

#________________________________________________________
# Query agricultural production and estimate for specific crop [Units: Mt]
estimate_prod_select_crop <- function(prj, query_production){
  
  # Retrieve particular query for all scenarios in the dataset, formatted as a single table
  prod.data.table <- getQuery(prj, query_production)
  
  # Agricultural production for biomass is provided in units of EJ. Converting it to Mt units
  # Based on https://www.extension.iastate.edu/AGDm/wholefarm/pdf/c6-88.pdf
  # 1 EJ of switchgrass = 58.56431 Mt
  prod.data.table <- bind_rows((prod.data.table %>%
                                  filter(sector != 'biomass')),
                               (prod.data.table %>%
                                  filter(sector=='biomass') %>%
                                  mutate(value = 58.56431*value,
                                         Units = 'Mt')))
  
  # Rename value column
  prod.data.table <- rename(prod.data.table, prod_Mt = value)
  
  # Drop Units column
  prod.data.table <- select(prod.data.table, -Units, -output)
  
  # Add long label for scenarios
  prod.data.table <- add_long_label_scenarios(prod.data.table)
  
  return(prod.data.table)
  
}

#________________________________________________________
# Query land allocation and estimate for specific crop [Units: thous km2]
estimate_land_alloc_select_crop <- function(prj, query_land, select_crops){
  
  # Retrieve particular query for all scenarios in the dataset, formatted as a single table
  land.data.table <- getQuery(prj, query_land)
  
  # For land allocation combine biomass_grass and biomass_tree into biomass
  land.data.table <- bind_rows((land.data.table %>%
                                  filter(landleaf != 'biomass_grass' & landleaf != 'biomass_tree')),
                               (land.data.table %>%
                                  filter(landleaf == 'biomass_grass' | landleaf == 'biomass_tree') %>%
                                  group_by(Units, scenario, region, year) %>%
                                  summarise(landleaf='biomass',
                                            value=sum(value))))
  
  # Only keep data for select crop
  land.data.table <- filter(land.data.table, landleaf %in% select_crops)
  
  # Rename value column
  land.data.table <- rename(land.data.table, sector = landleaf)
  land.data.table <- rename(land.data.table, area_thous_km2 = value)
  
  # Drop Units and input column
  land.data.table <- select(land.data.table, -Units)
  
  # Add long label for scenarios
  land.data.table <- add_long_label_scenarios(land.data.table)
  
  return(land.data.table)
  
}

#________________________________________________________
# Query fertilizer consumption and estimate for specific crop [Units: MtN]
estimate_fert_consump_select_crop <- function(prj, query_fert_consump, select_crops){
  
  # Retrieve particular query for all scenarios in the dataset, formatted as a single table
  # nrow(fert.data.table) ~ num_scenario * num_region * num_sector * num_year
  fert.data.table <- getQuery(prj, query_fert_consump)
  
  # Only keep data for select crop
  # nrow(fert.data.table) = num_scenario * num_region * num_year
  fert.data.table <- filter(fert.data.table, sector %in% select_crops)
  
  # Rename value column
  fert.data.table <- rename(fert.data.table, fert_MtN = value)
  
  # Drop Units and input column
  fert.data.table <- select(fert.data.table, -Units, -input)
  
  # Add long label for scenarios
  fert.data.table <- add_long_label_scenarios(fert.data.table)
  
  return(fert.data.table)
  
}

#________________________________________________________
# Query fertilizer consumption and land allocation by agricultural technologies
# to estimate fertilizer application rate [Units: kgN/ha]
estimate_agg_fert_application_rate <- function(prj, query_fert_consump, query_land){
  
  # Retrieve particular query for all scenarios in the dataset, formatted as a single table
  fert.data.table <- getQuery(prj, query_fert_consump)
  land.data.table <- getQuery(prj, query_land)
  
  # Only keep data for crops
  fert.data.table <- filter(fert.data.table, sector == 'crops')
  land.data.table <- filter(land.data.table, landleaf == 'crops')
  
  # Rename value column
  fert.data.table <- rename(fert.data.table, fert_MtN = value)
  land.data.table <- rename(land.data.table, area_thous_km2 = value)
  
  # Drop columns
  fert.data.table <- select(fert.data.table, -Units, -sector, -input)
  land.data.table <- select(land.data.table, -Units, -landleaf)
  
  # Merge two tables
  fert.rate.table <- inner_join(fert.data.table, land.data.table, by=c('scenario','region','year'))
  
  # Estimate values for global region and add as additional rows
  fert.rate.table <- bind_rows(fert.rate.table, fert.rate.table %>% 
                                  group_by(scenario, year) %>% 
                                  summarize(region = 'Global',
                                            fert_MtN = sum(fert_MtN),
                                            area_thous_km2 = sum(area_thous_km2)))
  
  # Add column fertilizer application rate [kgN/ha]
  fert.rate.table <- fert.rate.table %>%
                     mutate(fert_kgN_ha = fert_MtN*10^9 / (area_thous_km2 * 1000*100))
  
  return(fert.rate.table)
  
}

#________________________________________________________
# Query fertilizer consumption and land allocation by agricultural technologies
# to estimate fertilizer application rate [Units: kgN/ha]
estimate_fert_application_rate <- function(prj, query_fert_consump, query_land){
  
  # Retrieve particular query for all scenarios in the dataset, formatted as a single table
  fert.data.table <- getQuery(prj, query_fert_consump)
  land.data.table <- getQuery(prj, query_land)
  
  # Rename value column
  fert.data.table <- rename(fert.data.table, fert_MtN = value)
  land.data.table <- rename(land.data.table, area_thous_km2 = value, technology = landleaf)
  
  # Drop columns
  fert.data.table <- select(fert.data.table, -Units, -subsector, -input)
  land.data.table <- select(land.data.table, -Units)
  
  # Merge two tables
  fert.rate.table <- inner_join(fert.data.table, land.data.table, by=c('scenario','region','technology','year'))
  
  # Add column for fertilizer technology (hi or lo) and fertilizer application rate [kgN/ha]
  # fert.rate.table <- fert.rate.table %>%
  #                    mutate(ag_tech = substr(technology, nchar(technology)-1, nchar(technology)),
  #                           fert_gN_m2 = fert_MtN*10^12 / (area_thous_km2 * 1000*10^6))
  fert.rate.table <- fert.rate.table %>%
                     mutate(ag_tech = substr(technology, nchar(technology)-1, nchar(technology)),
                            fert_kgN_ha = fert_MtN*10^9 / (area_thous_km2 * 1000*100))
  
  # Add long label for scenarios
  fert.rate.table <- add_long_label_scenarios(fert.rate.table)
  
  return(fert.rate.table)
  
}

#________________________________________________________
# Summarize percentage change since 2015 (first year among the future years)
summarize_change_reference <- function(input.data, select_scenarios){
  
  # Estimate percentage change compared to Reference scenario
  input.data <- input.data %>%
    group_by(region, year) %>%
    filter(year >= 2015) %>%
    mutate(change_ref      = (value - value[which(scenario=='Reference_SSP2-4p5')]),
           perc_change_ref = 100*(value - value[which(scenario=='Reference_SSP2-4p5')])/value[which(scenario=='Reference_SSP2-4p5')])
  
  # Subset data for plotting
  input.data <- input.data %>% 
    filter(scenario %in% select_scenarios)
  
  # Add long label for scenarios
  input.data <- add_long_label_scenarios(input.data)
  
  # Drop unused levels
  input.data$scenario <- droplevels(input.data$scenario)
  
  # Print a summary table for four regions for 2100
  print(spread( (input.data %>% filter(year==2100) %>%  select(-value, -change_ref)), key=region, value=perc_change_ref))
  
  return(input.data)  
  
}

#________________________________________________________
# Make plot of percentage change in value for top five countries 
# with largest increase and top five countries with largest decrease
plot_per_change <- function(plot.data.table, xaxis.var, value.var, select_yr, facet_ncol, y_lab, plot_title){
  
  # Only keep data for select years
  # nrow(plot.data.table) = num_scenario * num_region
  plot.data.table <- plot.data.table %>% 
    rename(value = value.var, xaxis = xaxis.var) %>%  # Rename variable
    filter(year == select_yr) %>%  # Only keep rows for selected year
    select(-year)                  # Drop year column
  
  # Estimate percentage change in value compared to the reference scenario
  plot.data.table <- plot.data.table %>%
    group_by(xaxis) %>%
    mutate(per_change = 100*(value - value[scenario == 'Reference'])/value[scenario == 'Reference']) %>%
    filter(scenario != 'Reference') %>% # Remove rows for Reference scenarios
    select(-value)                      # Drop value column
  
  print(plot.data.table %>% group_by(scenario) %>% top_n(10, per_change) %>% mutate(Desc='Largest increase'))
  print(plot.data.table %>% group_by(scenario) %>% top_n(-10, per_change) %>% mutate(Desc='Largest decrease'))
  
  # Identify top 5 xaxis with largest and smallest percentage increase in value compared to Reference
  plot.data <- bind_rows((plot.data.table %>% group_by(scenario) %>% top_n(5, per_change) %>% mutate(Desc='Largest increase')),  # top 5 increasing
                         (plot.data.table %>% group_by(scenario) %>% top_n(-5, per_change) %>% mutate(Desc='Largest decrease'))) # top 5 decreasing
  
  # Make column plot showing percentage increase and decrease facetted and ordered (descending) by scenario
  p1 <- ggplot(plot.data, aes(x=reorder_within(xaxis, -per_change, scenario), y=per_change, fill=Desc)) + 
    geom_col(width=0.5)  +
    geom_text(aes(label=round(per_change,1), vjust=0), size=6, family='Helvetica', color='black') +
    facet_wrap(~ scenario, scales='free_x', ncol=facet_ncol) +
    scale_x_reordered() +
    labs(y     = y_lab,
         title = plot_title) +
    guides(fill=guide_legend(title=NULL)) + 
    theme_bw() +  # remove background
    theme(panel.grid       = element_blank(),          # Remove all grid lines,
          text             = element_text(size=15,family='Helvetica',color='black'),
          strip.text       = element_text(size=15,family='Helvetica',color='black'),
          strip.background = element_rect(fill=NA),     # Remove background color of facet label but keep black border
          legend.position  = 'bottom',
          legend.text      = element_text(size=15,family='Helvetica',color='black'),
          plot.title       = element_text(size=15,family='Helvetica',color='black'),
          axis.title.y     = element_text(size=15,family='Helvetica',color='black'),  
          axis.title.x     = element_blank(),
          axis.text.y      = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x      = element_text(size=15,family='Helvetica',color='black', angle=90, vjust=0.5, hjust=1))
  
  print(p1)
  
}

#________________________________________________________
# Make plot of percentage change in value for top five countries 
# with largest increase and top five countries with largest decrease
plot_per_change_position_dodge <- function(plot.data.table, xaxis.var, value.var, select_yr, leg_nrow, y_lab, plot_title){
  
  # Only keep data for select years
  # nrow(plot.data.table) = num_scenario * num_region
  plot.data.table <- plot.data.table %>% 
    rename(value = value.var, xaxis = xaxis.var) %>%  # Rename variable
    filter(year == select_yr) %>%  # Only keep rows for selected year
    select(-year)                  # Drop year column
  
  # Estimate percentage change in value compared to the reference scenario
  plot.data.table <- plot.data.table %>%
    group_by(xaxis) %>%
    mutate(per_change = 100*(value - value[scenario == 'Reference'])/value[scenario == 'Reference']) %>%
    filter(scenario != 'Reference') %>% # Remove rows for Reference scenarios
    select(-value)                      # Drop value column
  
  print(plot.data.table %>% group_by(scenario) %>% top_n(10, per_change) %>% mutate(Desc='Largest increase'))
  print(plot.data.table %>% group_by(scenario) %>% top_n(-10, per_change) %>% mutate(Desc='Largest decrease'))
  
  # Identify top 5 xaxis with largest and smallest percentage increase in value compared to Reference
  plot.data <- bind_rows((plot.data.table %>% group_by(scenario) %>% top_n(5, per_change) %>% mutate(Desc='Largest increase')),  # top 5 increasing
                         (plot.data.table %>% group_by(scenario) %>% top_n(-5, per_change) %>% mutate(Desc='Largest decrease'))) # top 5 decreasing
  
  # Make column plot showing percentage increase and decrease facetted and ordered (descending) by scenario
  p1 <- ggplot(plot.data, aes(x=fct_reorder(xaxis, per_change, .desc=TRUE), y=per_change, fill=scenario)) + 
    geom_col(position = 'dodge')  +
    scale_x_reordered() +
    scale_fill_manual(values=color_pal) +
    labs(y     = y_lab,
         title = plot_title) +
    guides(fill=guide_legend(title=NULL, nrow=leg_nrow)) + 
    theme(strip.background = element_rect(fill=NA),     # Remove background color of facet label but keep black border
          legend.position  = 'bottom',
          axis.title.x     = element_blank(),
          axis.text.x      = element_text(angle=90, vjust=0.5, hjust=1))
  
  print(p1)
  
}

#________________________________________________________
# Make plot of change in value for top five countries 
# with largest increase and top five countries with largest decrease
plot_change <- function(plot.data.table, xaxis.var, value.var, select_yr, facet_ncol, y_lab, plot_title, leg_keyword){
  
  # Only keep data for select years
  # nrow(plot.data.table) = num_scenario * num_region
  plot.data.table <- plot.data.table %>% 
                     rename(value = value.var, xaxis = xaxis.var) %>%  # Rename variable
                     filter(year == select_yr) %>%  # Only keep rows for selected year
                     select(-year)                  # Drop year column
  
  # Estimate percentage change in value compared to the reference scenario
  plot.data.table <- plot.data.table %>%
    group_by(xaxis) %>%
    mutate(change = (value - value[scenario == 'Reference'])) %>%
    filter(scenario != 'Reference') %>% # Remove rows for Reference scenarios
    select(-value)                      # Drop value column
  
  # Identify top 5 xaxis variables with largest and smallest percentage increase in value compared to Reference
  plot.data <- bind_rows((plot.data.table %>% group_by(scenario) %>% top_n(5, change) %>% mutate(Desc='Largest increase')),  # top 5 increasing
                         (plot.data.table %>% group_by(scenario) %>% top_n(-5, change) %>% mutate(Desc='Largest decrease'))) # top 5 decreasing
  
  plot.data$change_dir <- paste('Increase in', leg_keyword, sep=' ')
  plot.data[which(plot.data$change < 0), 'change_dir'] <- paste('Decrease in', leg_keyword, sep=' ')
  
  # Make column plot showing percentage increase and decrease facetted and ordered (descending) by scenario
  p1 <- ggplot(plot.data, aes(x=reorder_within(xaxis, -change, scenario), y=change, fill=change_dir)) + 
    geom_col(width=0.5)  +
    geom_text(aes(label=round(change,1), vjust=0), size=6, family='Helvetica', color='black') +
    facet_wrap(~ scenario, scales='free_x', ncol=facet_ncol) +
    scale_x_reordered() +
    labs(y     = y_lab,
         title = plot_title) +
    guides(fill=guide_legend(title=NULL)) + 
    theme_bw() +  # remove background
    theme(panel.grid       = element_blank(),          # Remove all grid lines,
          text             = element_text(size=15,family='Helvetica',color='black'),
          strip.text       = element_text(size=15,family='Helvetica',color='black'),
          strip.background = element_rect(fill=NA),     # Remove background color of facet label but keep black border
          legend.position  = 'bottom',
          legend.text      = element_text(size=15,family='Helvetica',color='black'),
          plot.title       = element_text(size=15,family='Helvetica',color='black'),
          axis.title.y     = element_text(size=15,family='Helvetica',color='black'),  
          axis.title.x     = element_blank(),
          axis.text.y      = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x      = element_text(size=15,family='Helvetica',color='black', angle=90, vjust=0.5, hjust=1))
  
  print(p1)
  
}

#________________________________________________________
# Make plot of change in value for top five countries 
# with largest increase and top five countries with largest decrease
plot_change_position_dodge <- function(plot.data.table, xaxis.var, value.var, select_yr, leg_nrow, y_lab, plot_title){
  
  # Only keep data for select years
  # nrow(plot.data.table) = num_scenario * num_region
  plot.data.table <- plot.data.table %>% 
    rename(value = value.var, xaxis = xaxis.var) %>%  # Rename variable
    filter(year == select_yr) %>%  # Only keep rows for selected year
    select(-year)                  # Drop year column
  
  # Estimate percentage change in value compared to the reference scenario
  plot.data.table <- plot.data.table %>%
    group_by(xaxis) %>%
    mutate(change = (value - value[scenario == 'Reference'])) %>%
    filter(scenario != 'Reference') %>% # Remove rows for Reference scenarios
    select(-value)                      # Drop value column
  
  # Identify top 5 xaxis variables with largest and smallest percentage increase in value compared to Reference
  plot.data <- bind_rows((plot.data.table %>% group_by(scenario) %>% top_n(5, change) %>% mutate(Desc='Largest increase')),  # top 5 increasing
                         (plot.data.table %>% group_by(scenario) %>% top_n(-5, change) %>% mutate(Desc='Largest decrease'))) # top 5 decreasing
  
  plot.data$change_dir <- 'Increase in production'
  plot.data[which(plot.data$change < 0), 'change_dir'] <- 'Decrease in production'
    
  # Make column plot showing percentage increase and decrease facetted and ordered (descending) by scenario
  p1 <- ggplot(plot.data, aes(x=fct_reorder(xaxis, -change), y=change, fill=scenario)) + 
    geom_col(position = 'dodge')  +
    scale_x_reordered() +
    scale_fill_manual(values=color_pal) +
    labs(y     = y_lab,
         title = plot_title) +
    guides(fill=guide_legend(title=NULL, nrow=leg_nrow)) + 
    theme(strip.background = element_rect(fill=NA),     # Remove background color of facet label but keep black border
          legend.position  = 'bottom',
          axis.title.x     = element_blank(),
          axis.text.x      = element_text(angle=90, vjust=0.5, hjust=1))
  
  print(p1)
  
}


#________________________________________________________
# Make plot of change in value for different sectors
plot_change_facet <- function(plot.data.table, xaxis.var, value.var, facet.var, select_yr, facet_ncol, y_lab, plot_title, leg_keyword){
  
  # Only keep data for select years
  # nrow(plot.data.table) = num_scenario * num_region
  plot.data.table <- plot.data.table %>% 
    rename(value = value.var, xaxis = xaxis.var, fvar = facet.var) %>%  # Rename variable
    filter(year == select_yr) %>%  # Only keep rows for selected year
    select(-year)                  # Drop year column
  
  # Estimate percentage change in value compared to the reference scenario
  plot.data.table <- plot.data.table %>%
    group_by(xaxis, fvar) %>%
    mutate(change = (value - value[scenario == 'Reference'])) %>%
    filter(scenario != 'Reference') %>% # Remove rows for Reference scenarios
    select(-value)                      # Drop value column
  
  # Identify top 5 xaxis variables with largest and smallest percentage increase in value compared to Reference
  plot.data <- bind_rows((plot.data.table %>% group_by(scenario, fvar) %>% top_n(5, change) %>% mutate(Desc='Largest increase')),  # top 5 increasing
                         (plot.data.table %>% group_by(scenario, fvar) %>% top_n(-5, change) %>% mutate(Desc='Largest decrease'))) # top 5 decreasing
  
  plot.data$change_dir <- paste('Increase in', leg_keyword, sep=' ')
  plot.data[which(plot.data$change < 0), 'change_dir'] <- paste('Decrease in', leg_keyword, sep=' ')
  
  # Make column plot showing percentage increase and decrease facetted and ordered (descending) by facet variable
  p1 <- ggplot(plot.data, aes(x=reorder_within(xaxis, -change, fvar), y=change, fill=change_dir)) + 
    geom_col(width=0.5)  +
    facet_wrap(~ fvar, scales='free', ncol=facet_ncol) +
    scale_x_reordered() +
    labs(y     = y_lab,
         title = plot_title) +
    guides(fill=guide_legend(title=NULL)) + 
    theme_bw() +  # remove background
    theme(panel.grid       = element_blank(),          # Remove all grid lines,
          text             = element_text(size=15,family='Helvetica',color='black'),
          strip.text       = element_text(size=15,family='Helvetica',color='black'),
          strip.background = element_rect(fill=NA),     # Remove background color of facet label but keep black border
          legend.position  = 'bottom',
          legend.text      = element_text(size=15,family='Helvetica',color='black'),
          plot.title       = element_text(size=15,family='Helvetica',color='black'),
          axis.title.y     = element_text(size=15,family='Helvetica',color='black'),  
          axis.title.x     = element_blank(),
          axis.text.y      = element_text(size=15,family='Helvetica',color='black'),
          axis.text.x      = element_text(size=15,family='Helvetica',color='black', angle=90, vjust=0.5, hjust=1))
  
  print(p1)
  
}