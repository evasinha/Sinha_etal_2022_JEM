# Author - Eva Sinha, Pacific Northwest National Lab, eva.sinha@pnnl.gov

library(rgcam)
library(ggpubr)
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
print(listQueries(prj))

# ----- Query agricultural production and estimate for specific crop [Units: Mt]
ag.prod.data.table  <- estimate_prod_select_crop(prj, query_production = 'ag production by tech')

# Subset production data to only keep data for crops and add ag_tech column
ag.prod.data.table  <- ag.prod.data.table %>%
  select(-subsector) %>%
  filter( ! sector %in% c('Forest', 'Pasture')) %>%
  mutate(fert_tech = substr(technology, nchar(technology)-1, nchar(technology)),
         subregion = sapply(str_split(technology, '_'), function(x){ifelse(length(x)==4, x[2], x[3])}),
         ag_tech   = sapply(str_split(technology, '_'), function(x){ifelse(length(x)==4, paste(x[3], x[4], sep='_'), paste(x[4], x[5], sep='_'))}))

# Sum agricultural production
ag.prod.data.table <- ag.prod.data.table %>%
  group_by(scenario, region, sector, year, ag_tech) %>%
  summarize(value = sum(prod_Mt)) %>%
  mutate(Units = 'Mt')

# ----- Query prices by sector 
price.data.table <- getQuery(prj, query = 'prices by sector')

# Subset for agricultural commodities and convert value to 2010
price.data.table <- price.data.table %>%
  mutate(value = value*3.22*1000, Units = '2010$/ton') # from 1975$/kg to 2010$/ton

# ----- Query food consumption by type
food.cons.data.table <- getQuery(prj, query = 'food consumption by type (specific)')

# ----- Query land allocation by crop
land.alloc.data.table <- getQuery(prj, query = 'land allocation by crop')

# ----- Query demand by crop commodity and type
crop.demand.data.table <- getQuery(prj, query = 'Demand by crop commodity and type (Food,Feed,Nonfood,Other)')

# Add long label for scenarios
price.data.table       <- add_long_label_scenarios(price.data.table)
food.cons.data.table   <- add_long_label_scenarios(food.cons.data.table)
land.alloc.data.table  <- add_long_label_scenarios(land.alloc.data.table)
crop.demand.data.table <- add_long_label_scenarios(crop.demand.data.table)

# Drop unused levels
price.data.table$scenario       <- droplevels(price.data.table$scenario)
food.cons.data.table$scenario   <- droplevels(food.cons.data.table$scenario)
land.alloc.data.table$scenario  <- droplevels(land.alloc.data.table$scenario)
crop.demand.data.table$scenario <- droplevels(crop.demand.data.table$scenario)

# Selected scenarios for plotting
select_scenarios <- c('Reference', 
                      'Global constraint (15%) on fertilizer')
                      # 'Global constraint (15%) - fertilizer io flat', 
                      # 'Global constraint (15%) - fertilizer io steep',
                      # 'Global constraint (15%) - fertilizer io regional')

# Subset agricultural production for Corn
USA.ag.prod <- ag.prod.data.table %>%
  filter(scenario %in% select_scenarios,
         year >= 2015,
         region   == 'USA',
         sector   == 'Corn',
         ag_tech %in% c('RFD_lo', 'RFD_hi')) %>%
  mutate(Desc = ifelse(ag_tech == 'RFD_hi', 'Ag. prod. hi-yield tech.', 'Ag. prod. lo-yield tech.')) %>%
  select(-ag_tech)

# Subset food consumption for Corn
USA.food.cons <- food.cons.data.table %>%
  filter(scenario %in% select_scenarios,
         year >= 2015,
         region   == 'USA',
         subsector   == 'Corn') %>%
  select(-output) %>%
  mutate(Desc = 'Total food demand')

# Subset price for corn
USA.corn.price.data      <- price.data.table %>% 
  filter(scenario %in% select_scenarios,
         year >= 2015,
         region == 'USA',
         sector == 'Corn') %>%
  mutate(Desc = 'Price')

# Subset land allocation for corn
USA.corn.land.alloc      <- land.alloc.data.table %>% 
  filter(scenario %in% select_scenarios,
         year >= 2015,
         region == 'USA',
         landleaf == 'Corn') %>%
  mutate(Desc = 'Land allocation')

# Subset land allocation for corn
USA.corn.demand      <- crop.demand.data.table %>% 
  filter(scenario %in% select_scenarios,
         year >= 2015,
         region == 'USA',
         input == 'regional corn') %>%
  mutate(Desc = 'Crop demand')

print('USA agricultural production for the Reference and global constraint scenario in 2100')
print(filter(USA.ag.prod, year == 2100, scenario %in% c('Reference','Global constraint (15%) on fertilizer')))

print('USA corn demand for the Reference scenario in 2100')
print(filter(USA.corn.demand, year == 2100, scenario %in% c('Reference','Global constraint (15%) on fertilizer')))

# Define path to output file
out.folder <- '../figures/'
out.fname  <- 'Figure5.pdf'
out.fname  <- paste(out.folder, out.fname, sep='')
# Delete existing file
unlink(out.fname)
# Start pdf device driver for saving plots
pdf(out.fname, height=8.5,width=11)

p1 <- plot_line(filter(USA.ag.prod, Desc=='Ag. prod. hi-yield tech.'),
                x_col      = 'year',
                y_col      = 'value',
                group_col  = 'scenario',
                color_col  = 'scenario',
                leg_nrow   = 1,
                x_lab       = NULL,
                y_lab      = 'Mt', 
                plot_title = 'USA corn prod. hi-yield tech.')

p1 <- p1 + ylim(100, 300) + theme(axis.text.x = element_blank(), axis.ticks = element_blank())

p2 <- plot_line(filter(USA.ag.prod, Desc=='Ag. prod. lo-yield tech.'),
                      x_col      = 'year',
                      y_col      = 'value',
                      group_col  = 'scenario',
                      color_col  = 'scenario',
                      leg_nrow   = 1,
                      x_lab      = NULL,
                      y_lab      = 'Mt', 
                      plot_title = 'USA corn prod. lo-yield tech.')

p2 <- p2 + ylim(100, 300) + theme(axis.text.x = element_blank(), axis.ticks = element_blank())

p3 <- plot_line(USA.corn.price.data,
                x_col      = 'year',
                y_col      = 'value',
                group_col  = 'scenario',
                color_col  = 'scenario',
                leg_nrow   = 1,
                x_lab      = NULL,
                y_lab      = expression(paste(2010~'$',~ton^-1)), 
                plot_title = 'USA corn price')
 
p3 <- p3 + ylim(200, 260)

p4 <- plot_line(USA.corn.land.alloc,
                x_col      = 'year',
                y_col      = 'value',
                group_col  = 'scenario',
                color_col  = 'scenario',
                leg_nrow   = 1,
                x_lab      = NULL,
                y_lab      = expression(paste(thous~km^2)), 
                plot_title = 'USA corn land allocation')

p4 <- p4 + ylim(280, 320)

figure <- ggarrange(p1, p2, p3, p4,
                    labels        = 'AUTO',
                    font.label    = list(size=15,family='Helvetica',color='black'),
                    ncol          = 2,
                    nrow          = 2,
                    common.legend = TRUE,
                    legend        = 'bottom')

print(figure)
# annotate_figure(figure,
#                 top = text_grob('Region - USA; Sector - Corn', face='bold', size=14))

p5 <- plot_line_facet(USA.corn.demand,
                      x_col      = 'year',
                      y_col      = 'value',
                      group_col  = 'scenario',
                      color_col  = 'scenario',
                      linetype_col = NULL,
                      facet_var  = 'technology',
                      facet_ncol = 2,
                      facet_scale = 'free_y',
                      leg_nrow   = 1,
                      x_lab      = NULL,
                      y_lab      = 'Demand [Mt]',
                      plot_title = 'Region - USA; Sector - Corn')

print (p5)

# USA rice demand
p6 <- plot_line_facet(filter(crop.demand.data.table,
                             scenario %in% select_scenarios,
                             year >= 2015,
                             region == 'USA',
                             input == 'regional rice'),
                      x_col      = 'year',
                      y_col      = 'value',
                      group_col  = 'scenario',
                      color_col  = 'scenario',
                      linetype_col = NULL,
                      facet_var  = 'technology',
                      facet_ncol = 2,
                      facet_scale = 'free_y',
                      leg_nrow   = 1,
                      x_lab      = NULL,
                      y_lab      = 'Demand [Mt]',
                      plot_title = 'Region - USA; Sector - Rice')

print (p6)

# USA wheat demand
p7 <- plot_line_facet(filter(crop.demand.data.table,
                             scenario %in% select_scenarios,
                             year >= 2015,
                             region == 'USA',
                             input == 'regional wheat'),
                      x_col      = 'year',
                      y_col      = 'value',
                      group_col  = 'scenario',
                      color_col  = 'scenario',
                      linetype_col = NULL,
                      facet_var  = 'technology',
                      facet_ncol = 2,
                      facet_scale = 'free_y',
                      leg_nrow   = 1,
                      x_lab      = NULL,
                      y_lab      = 'Demand [Mt]',
                      plot_title = 'Region - USA; Sector - Wheat')

print (p7)

# USA wheat demand
p8 <- plot_line_facet(filter(crop.demand.data.table,
                             scenario %in% select_scenarios,
                             year >= 2015,
                             region == 'USA',
                             input == 'regional oilcrop'),
                      x_col      = 'year',
                      y_col      = 'value',
                      group_col  = 'scenario',
                      color_col  = 'scenario',
                      linetype_col = NULL,
                      facet_var  = 'technology',
                      facet_ncol = 2,
                      facet_scale = 'free_y',
                      leg_nrow   = 1,
                      x_lab      = NULL,
                      y_lab      = 'Demand [Mt]',
                      plot_title = 'Region - USA; Sector - Oil crop')

print (p8)

# China corn demand
p9 <- plot_line_facet(filter(crop.demand.data.table,
                             scenario %in% select_scenarios,
                             year >= 2015,
                             region == 'China',
                             input == 'regional corn'),
                      x_col      = 'year',
                      y_col      = 'value',
                      group_col  = 'scenario',
                      color_col  = 'scenario',
                      linetype_col = NULL,
                      facet_var  = 'technology',
                      facet_ncol = 2,
                      facet_scale = 'free_y',
                      leg_nrow   = 1,
                      x_lab      = NULL,
                      y_lab      = 'Demand [Mt]',
                      plot_title = 'Region - China; Sector - Corn')

print (p9)


# China rice demand
p10 <- plot_line_facet(filter(crop.demand.data.table,
                             scenario %in% select_scenarios,
                             year >= 2015,
                             region == 'China',
                             input == 'regional rice'),
                      x_col      = 'year',
                      y_col      = 'value',
                      group_col  = 'scenario',
                      color_col  = 'scenario',
                      linetype_col = NULL,
                      facet_var  = 'technology',
                      facet_ncol = 2,
                      facet_scale = 'free_y',
                      leg_nrow   = 1,
                      x_lab      = NULL,
                      y_lab      = 'Demand [Mt]',
                      plot_title = 'Region - China; Sector - Rice')

print (p10)

# China wheat demand
p11 <- plot_line_facet(filter(crop.demand.data.table,
                             scenario %in% select_scenarios,
                             year >= 2015,
                             region == 'China',
                             input == 'regional wheat'),
                      x_col      = 'year',
                      y_col      = 'value',
                      group_col  = 'scenario',
                      color_col  = 'scenario',
                      linetype_col = NULL,
                      facet_var  = 'technology',
                      facet_ncol = 2,
                      facet_scale = 'free_y',
                      leg_nrow   = 1,
                      x_lab      = NULL,
                      y_lab      = 'Demand [Mt]',
                      plot_title = 'Region - China; Sector - Wheat')

print (p11)

# China wheat demand
p12 <- plot_line_facet(filter(crop.demand.data.table,
                              scenario %in% select_scenarios,
                              year >= 2015,
                              region == 'China',
                              input == 'regional oilcrop'),
                       x_col      = 'year',
                       y_col      = 'value',
                       group_col  = 'scenario',
                       color_col  = 'scenario',
                       linetype_col = NULL,
                       facet_var  = 'technology',
                       facet_ncol = 2,
                       facet_scale = 'free_y',
                       leg_nrow   = 1,
                       x_lab      = NULL,
                       y_lab      = 'Demand [Mt]',
                       plot_title = 'Region - China; Sector - Oil crop')

print (p12)

# Make line plot with facet grid
p13 <- plot_line_facet_grid(filter(food.cons.data.table,
                                  scenario %in% select_scenarios,
                                  year      >= 2015,
                                  region    %in% c('China','USA'),
                                  subsector %in% c( 'Corn', 'Rice', 'Wheat','OilCrop')), 
                           x_col       = 'year',
                           y_col       = 'value', 
                           group_col   = 'scenario',
                           color_col   = 'scenario',
                           linetype    = NULL,
                           facet_row   = 'subsector', 
                           facet_col   = 'region', 
                           facet_scale = 'free_y', 
                           leg_nrow    = 1, 
                           y_lab       = NULL, 
                           plot_title  = 'Food consumption [Pcal]')

print(p13)

# Make line plot with facet grid
p14 <- plot_line_facet_grid(filter(price.data.table,
                                   scenario %in% select_scenarios,
                                   year >= 2015,
                                   region %in% c('China','USA'),
                                   sector %in% c( 'Corn', 'Rice', 'Wheat','OilCrop')), 
                            x_col       = 'year',
                            y_col       = 'value', 
                            group_col   = 'scenario',
                            color_col   = 'scenario',
                            linetype    = NULL,
                            facet_row   = 'sector', 
                            facet_col   = 'region', 
                            facet_scale = 'free_y', 
                            leg_nrow    = 1, 
                            y_lab       = NULL, 
                            plot_title  = expression(paste(Food~price,' [',2010~'$',~ton^-1,']')))

print(p14)

# Sum agricultural production
ag.prod.data.table <- ag.prod.data.table %>%
  group_by(scenario, region, sector, year) %>%
  summarize(value = sum(value)) 

# Make line plot with facet grid
p13 <- plot_line_facet_grid(filter(ag.prod.data.table,
                                   scenario %in% select_scenarios,
                                   year >= 2015,
                                   region %in% c('China','USA'),
                                   sector %in% c( 'Corn', 'Rice', 'Wheat', 'OilCrop')), 
                            x_col       = 'year',
                            y_col       = 'value', 
                            group_col   = 'scenario',
                            color_col   = 'scenario',
                            linetype    = NULL,
                            facet_row   = 'sector', 
                            facet_col   = 'region', 
                            facet_scale = 'free_y', 
                            leg_nrow    = 1, 
                            y_lab       = NULL, 
                            plot_title  = expression(paste(Agricultural~production,' [',Mt,']')))

print(p13)

dev.off()