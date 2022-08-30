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

# Retrieve aggregated land allocation query for all scenarios in the dataset, formatted as a single table
land.data.table <- getQuery(prj, query = 'aggregated land allocation')

# Estimate values for global region and add as additional rows
land.data.table <- bind_rows(land.data.table, land.data.table %>% 
                                              group_by(Units, scenario, landleaf, year) %>% 
                                              summarize(region = 'Global',
                                                        value = sum(value)))

# Convert land area value to million km2
land.data.table <- land.data.table %>%
    mutate(value = value/1000, Units = 'million km2') # from thous km2 to million km2

# Subset data for plotting
land.data.table <- land.data.table %>% 
                   filter(year >= 2015,
                          scenario %in% c('Reference_SSP2-4p5', 'fert_global_const_15_SSP2-4p5', 
                                          'carbon_tax_25_5', 'carbon_tax_25_5_fert_global_const_15'))
                                          # 'Ref_fert_io_flat', 'Ref_fert_io_steep', 'Ref_fert_io_regional',
                                          # 'fert_global_const_15_fert_io_flat', 'fert_global_const_15_fert_io_steep','fert_global_const_15_fert_io_regional'))

# Add long label for scenarios
land.data.table <- add_long_label_scenarios(land.data.table)
land.data.table$landleaf <- param_labeller('variable', land.data.table$landleaf)

# Drop unused levels
land.data.table$scenario <- droplevels(land.data.table$scenario)

land.data.table <- land.data.table %>%
                   group_by(Units, scenario, region, year) %>%
                   mutate(frac_area = value/sum(value))

print(land.data.table %>% 
        group_by(Units, scenario, region, landleaf) %>%
        filter(region=='Global', scenario=='Reference', landleaf == 'Crops') %>% 
        mutate(per_change_2015 = 100*(value - value[year==2015])/value[year==2015]))

print(land.data.table %>% 
        group_by(Units, scenario, region, landleaf) %>%
        filter(region=='Global', scenario=='Reference', landleaf == 'Forest (unmanaged)') %>% 
        mutate(per_change_2015 = 100*(value - value[year==2015])/value[year==2015]))

print(land.data.table %>% 
        group_by(Units, scenario, region, landleaf) %>%
        filter(region=='Global', scenario=='Reference', landleaf == 'Biomass') %>% 
        mutate(per_change_2015 = 100*(value - value[year==2015])/value[year==2015]))

print(land.data.table %>% 
        group_by(Units, region, landleaf, year) %>%
        filter(region=='Global', year %in% c(2060,2100), landleaf == 'Crops') %>% 
        mutate(per_change = 100*(value - value[scenario=='Reference'])/value[scenario=='Reference']))

print(land.data.table %>% 
        group_by(Units, region, landleaf, year) %>%
        filter(region=='Global', year %in% c(2060,2100), landleaf == 'Forest (unmanaged)') %>% 
        mutate(per_change = 100*(value - value[scenario=='Reference'])/value[scenario=='Reference']))

print(land.data.table %>% 
        group_by(Units, region, landleaf, year) %>%
        filter(region=='Global', year %in% c(2060,2100), landleaf == 'Biomass') %>% 
        mutate(per_change = 100*(value - value[scenario=='Reference'])/value[scenario=='Reference']))


print(land.data.table %>% 
          group_by(Units, region, landleaf, year) %>%
          filter(region=='Global', year %in% c(2060, 2080, 2100), landleaf == 'Crops',
                 scenario %in% c('Reference')))
                 # scenario %in% c('Reference', 'Reference - fertilizer io flat', 'Reference - fertilizer io steep', 'Reference - fertilizer io regional')))

print(land.data.table %>% 
          group_by(Units, region, landleaf, year) %>%
          filter(region=='Global', year %in% c(2060, 2080, 2100), landleaf == 'Forest (unmanaged)',
                 scenario %in% c('Reference')))
                 # scenario %in% c('Reference', 'Reference - fertilizer io flat', 'Reference - fertilizer io steep', 'Reference - fertilizer io regional')))

print(land.data.table %>% 
          group_by(Units, region, landleaf, year) %>%
          filter(region=='Global', year %in% c(2060, 2080, 2100), landleaf == 'Biomass',
                 scenario %in% c('Reference')))
                 # scenario %in% c('Reference', 'Reference - fertilizer io flat', 'Reference - fertilizer io steep', 'Reference - fertilizer io regional')))

# Define path to output file
out.folder <- '../figures/'
out.fname  <- 'Landuse_reference_scenario.pdf'
out.fname  <- paste(out.folder, out.fname, sep='')
# Delete existing file
unlink(out.fname)
# Start pdf device driver for saving plots
pdf(out.fname, height=8.5,width=11) 

p1 <- plot_area(filter(land.data.table, region %in% c('Global'),
                             scenario %in% c('Reference'),
                             year >=2005),
                      x_col      = 'year',
                      y_col      = 'frac_area',
                      group_col  = 'landleaf',
                      fill_col  = 'landleaf',
                      leg_nrow   = 1,
                      y_lab      = NULL, 
                      plot_title = 'Land cover')

p1 <- p1 + scale_fill_manual(values=color_pal) +
    scale_y_continuous(breaks=seq(from=0, to=1, by=0.1), expand=c(0.01,0.01), labels = scales::percent)

print (p1)

land.data.table <- land.data.table %>% 
                   filter(region %in% c('Global','China','India','USA'), 
                          scenario %in% c('Reference','Global constraint (15%) on fertilizer',
                                          'Low carbon','Low carbon with global constraint (15%)'))
                                          # 'Global constraint (15%) - fertilizer io flat',
                                          # 'Global constraint (15%) - fertilizer io steep',
                                          # 'Global constraint (15%) - fertilizer io regional'))
    
# Reorder factor levels
land.data.table$region <- factor(land.data.table$region, levels=c('Global','China','India','USA'))

# Estimate values for combined cropland and biomass area and add as additional rows
land.data.table <- bind_rows(land.data.table, land.data.table %>% 
                               group_by(Units, scenario, region, year) %>% 
                               filter(landleaf %in% c('Crops', 'Biomass')) %>%
                               summarize(landleaf = 'Crops + Biomass',
                                         value = sum(value)))

p2 <- plot_line_facet(filter(land.data.table, landleaf %in% c('Forest (unmanaged)', 'Crops', 'Biomass'), 
                             region == 'Global', 
                             scenario %in% c('Reference','Global constraint (15%) on fertilizer')), 
                      x_col       = 'year',
                      y_col       = 'value', 
                      group_col   = 'scenario',
                      color_col   = 'scenario',
                      linetype    = NULL,
                      facet_var   = 'landleaf', 
                      facet_ncol  = 2,  
                      facet_scale = 'free_y',
                      leg_nrow    = 1,  
                      x_lab       = NULL,
                      y_lab       = expression(million~km^2),
                      plot_title  = 'Global land area') 

print(p2)

p3 <- plot_line_facet(filter(land.data.table, landleaf %in% c('Forest (unmanaged)', 'Crops', 'Biomass'), 
                             region == 'Global'), 
                      x_col       = 'year',
                      y_col       = 'value', 
                      group_col   = 'scenario',
                      color_col   = 'scenario',
                      linetype    = NULL,
                      facet_var   = 'landleaf', 
                      facet_ncol  = 2,  
                      facet_scale = 'free_y',
                      leg_nrow    = 3,  
                      x_lab       = NULL,
                      y_lab       = expression(million~km^2),
                      plot_title  = 'Global land area') 

print(p3)

p4 <- plot_line_facet(filter(land.data.table, landleaf == 'Forest (unmanaged)'), 
                      x_col       = 'year',
                      y_col       = 'value', 
                      group_col   = 'scenario',
                      color_col   = 'scenario',
                      linetype    = NULL,
                      facet_var   = 'region', 
                      facet_ncol  = 2,  
                      facet_scale = 'free_y',
                      leg_nrow    = 3,
                      x_lab       = NULL,
                      y_lab       = expression(million~km^2),
                      plot_title  = 'Unmanaged forest') 

print(p4)

p5 <- plot_line_facet(filter(land.data.table, landleaf == 'Forest (managed)'), 
                      x_col       = 'year',
                      y_col       = 'value', 
                      group_col   = 'scenario',
                      color_col   = 'scenario',
                      linetype    = NULL,
                      facet_var   = 'region', 
                      facet_ncol  = 2,  
                      facet_scale = 'free_y',
                      leg_nrow    = 3,
                      x_lab       = NULL,
                      y_lab       = expression(million~km^2),
                      plot_title  = 'Managed forest') 

print(p5)

p6 <- plot_line_facet(filter(land.data.table, landleaf == 'Crops'), 
                      x_col       = 'year',
                      y_col       = 'value', 
                      group_col   = 'scenario',
                      color_col   = 'scenario',
                      linetype    = NULL,
                      facet_var   = 'region', 
                      facet_ncol  = 2,  
                      facet_scale = 'free_y',
                      leg_nrow    = 3,
                      x_lab       = NULL,
                      y_lab       = expression(million~km^2),
                      plot_title  = 'Cropland') 

print(p6)

p7 <- plot_line_facet(filter(land.data.table, landleaf == 'Biomass'), 
                      x_col       = 'year',
                      y_col       = 'value', 
                      group_col   = 'scenario',
                      color_col   = 'scenario',
                      linetype    = NULL,
                      facet_var   = 'region', 
                      facet_ncol  = 2,  
                      facet_scale = 'free_y',
                      leg_nrow    = 3,
                      x_lab       = NULL,
                      y_lab       = expression(million~km^2),
                      plot_title  = 'Biomass') 

print(p7)

p8 <- plot_line_facet(filter(land.data.table, landleaf == 'Crops + Biomass'), 
                      x_col       = 'year',
                      y_col       = 'value', 
                      group_col   = 'scenario',
                      color_col   = 'scenario',
                      linetype    = NULL,
                      facet_var   = 'region', 
                      facet_ncol  = 2,  
                      facet_scale = 'free_y',
                      leg_nrow    = 3,
                      x_lab       = NULL,
                      y_lab       = expression(million~km^2),
                      plot_title  = 'Crops + Biomass') 

print(p8)

dev.off()

# Define path to output file
out.folder <- '../figures/'
out.fname  <- 'Figure2.pdf'
out.fname  <- paste(out.folder, out.fname, sep='')
# Delete existing file
unlink(out.fname)
# Start pdf device driver for saving plots
pdf(out.fname, height=8.5,width=17) 

plot.data <- filter(land.data.table, landleaf %in% c('Forest (unmanaged)', 'Crops', 'Biomass'), 
                    region == 'Global',
                    scenario %in% c('Reference','Global constraint (15%) on fertilizer',
                                    'Low carbon','Low carbon with global constraint (15%)'))

# Replacing all Low carbon values with NA for a duplicate tibble so as to get the right legend in the combined figure
plot.data.2 <- plot.data
plot.data.2[which(plot.data.2$scenario %in% c('Low carbon','Low carbon with global constraint (15%)')), 'value'] <- NA

p1 <- plot_line_facet(plot.data.2, 
                      x_col       = 'year',
                      y_col       = 'value', 
                      group_col   = 'scenario',
                      color_col   = 'scenario',
                      linetype    = NULL,
                      facet_var   = 'landleaf', 
                      facet_ncol  = 3,  
                      facet_scale = 'free_y',
                      leg_nrow    = 1,
                      x_lab       = NULL,
                      y_lab       = expression(million~km^2),
                      plot_title  = NULL) 

p2 <- plot_line_facet(plot.data, 
                      x_col       = 'year',
                      y_col       = 'value', 
                      group_col   = 'scenario',
                      color_col   = 'scenario',
                      linetype    = NULL,
                      facet_var   = 'landleaf', 
                      facet_ncol  = 3,  
                      facet_scale = 'free_y',
                      leg_nrow    = 1,
                      x_lab       = NULL,
                      y_lab       = expression(million~km^2),
                      plot_title  = NULL) 

figure <- ggarrange(p1, p2,
                    labels        = 'AUTO',
                    font.label    = list(size=15,family='Helvetica',color='black'),
                    ncol          = 1,
                    nrow          = 2,
                    common.legend = TRUE,
                    legend        = 'bottom')

print(figure)

dev.off()





# Define path to output file
out.folder <- '../figures/'
out.fname  <- 'Landuse_biomass_reference.pdf'
out.fname  <- paste(out.folder, out.fname, sep='')
# Delete existing file
unlink(out.fname)
# Start pdf device driver for saving plots
pdf(out.fname, height=8.5,width=11) 


land.data.table <- land.data.table %>% 
  filter(region %in% c('Global','China','India','USA'), 
         scenario %in% c('Reference'))

# Reorder factor levels
land.data.table$region <- factor(land.data.table$region, levels=c('Global','China','India','USA'))

# Estimate values for combined cropland and biomass area and add as additional rows
land.data.table <- bind_rows(land.data.table, land.data.table %>% 
                               group_by(Units, scenario, region, year) %>% 
                               filter(landleaf %in% c('Crops', 'Biomass')) %>%
                               summarize(landleaf = 'Crops + Biomass',
                                         value = sum(value)))

p7 <- plot_line_facet(filter(land.data.table, landleaf == 'Biomass'), 
                      x_col       = 'year',
                      y_col       = 'value', 
                      group_col   = 'scenario',
                      color_col   = 'scenario',
                      linetype    = NULL,
                      facet_var   = 'region', 
                      facet_ncol  = 2,  
                      facet_scale = 'free_y',
                      leg_nrow    = 3,
                      x_lab       = NULL,
                      y_lab       = expression(million~km^2),
                      plot_title  = 'Biomass') 

print(p7)

dev.off()