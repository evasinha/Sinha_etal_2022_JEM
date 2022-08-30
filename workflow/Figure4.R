# Author - Eva Sinha, Pacific Northwest National Lab, eva.sinha@pnnl.gov

library(rgcam)
library(ggpubr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
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
print(listQueries(prj))

# ----- Query demand by crop commodity and type
crop.demand.data.table <- getQuery(prj, query = 'Demand by crop commodity and type (Food,Feed,Nonfood,Other)')

# ----- Query feed consumption by region
feed.cons.data.table <- getQuery(prj, query = 'feed consumption by region')

# ----- Query aggregated land allocation
agg.land.alloc.data.table <- getQuery(prj, query = 'aggregated land allocation')

# Convert land area value to million km2
agg.land.alloc.data.table <- agg.land.alloc.data.table %>%
  mutate(value = value/1000, Units = 'million km2') # from thous km2 to million km2

# ----- Query meat and dairy prices
meat.price.data.table <- getQuery(prj, query = 'meat and dairy prices')

# Convert value to 2010
meat.price.data.table <- meat.price.data.table %>%
  mutate(value = value*3.22*1000, Units = '2010$/ton') # from 1975$/kg to 2010$/ton

# ----- Query meat and dairy production
meat.prod.data.table <- getQuery(prj, query = 'meat and dairy production by type')

# ----- Query food consumption
food.cons.data.table <- getQuery(prj, query = 'food consumption by type (specific)')

# Add long label for scenarios
crop.demand.data.table    <- add_long_label_scenarios(crop.demand.data.table)
feed.cons.data.table      <- add_long_label_scenarios(feed.cons.data.table)
agg.land.alloc.data.table <- add_long_label_scenarios(agg.land.alloc.data.table)
meat.price.data.table     <- add_long_label_scenarios(meat.price.data.table)
meat.prod.data.table      <- add_long_label_scenarios(meat.prod.data.table)
food.cons.data.table      <- add_long_label_scenarios(food.cons.data.table)

# Drop unused levels
crop.demand.data.table$scenario    <- droplevels(crop.demand.data.table$scenario)
feed.cons.data.table$scenario      <- droplevels(feed.cons.data.table$scenario)
agg.land.alloc.data.table$scenario <- droplevels(agg.land.alloc.data.table$scenario)
meat.price.data.table$scenario     <- droplevels(meat.price.data.table$scenario)
meat.prod.data.table$scenario      <- droplevels(meat.prod.data.table$scenario)
food.cons.data.table$scenario      <- droplevels(food.cons.data.table$scenario)

# Selected scenarios for plotting
select_scenarios <- c('Reference', 
                      'Global constraint (15%) on fertilizer')
                      # 'Global constraint (15%) - fertilizer io flat', 
                      # 'Global constraint (15%) - fertilizer io steep',
                      # 'Global constraint (15%) - fertilizer io regional')

# Combine data into a single tibble
plot.data <- bind_rows(meat.price.data.table %>% mutate(Desc = 'Price'), 
                       meat.prod.data.table %>% select(-output) %>% mutate(Desc = 'Production'),
                       food.cons.data.table %>% select(-sector, -technology, -output) %>% rename(sector = subsector) %>% mutate(Desc = 'Consumption'))

# Subset corn demand
USA.corn.demand      <- crop.demand.data.table %>% 
  filter(scenario %in% select_scenarios,
         year >= 2015,
         region == 'USA',
         input == 'regional corn') %>%
  mutate(Desc = 'Crop demand')

# Subset USA feed consumption
USA.feed.cons <- feed.cons.data.table %>%
  filter(scenario %in% select_scenarios,
         year >=2015,
         region   == 'USA')

# Subset USA land allocation
USA.agg.land.alloc <- agg.land.alloc.data.table %>%
  filter(scenario %in% select_scenarios,
         year >=2015,
         region   == 'USA')

# Subset USA meat price
USA.meat.price <- meat.price.data.table %>%
  filter(scenario %in% select_scenarios,
         year >=2015,
         region == 'USA')

# Subset USA meat production
USA.meat.prod <- meat.prod.data.table %>%
  filter(scenario %in% select_scenarios,
         year >=2015,
         region == 'USA')

# Subset food consumption in USA
USA.food.cons <- food.cons.data.table %>%
  filter(scenario %in% select_scenarios,
         year >=2015,
         region == 'USA')

print('USA land allocation for the Reference and global constraint scenario in 2060')
print(USA.agg.land.alloc %>%
        filter(year == 2060, 
               scenario %in% select_scenarios,
               landleaf %in% c('crops','grass','pasture (grazed)', 'pasture (other)')) %>% 
        mutate(per_change = 100*(value - value[scenario=='Reference'])/value[scenario=='Reference']))


# Define path to output file
out.folder <- '../figures/'
out.fname  <- 'Figure4.pdf'
out.fname  <- paste(out.folder, out.fname, sep='')
# Delete existing file
unlink(out.fname)
# Start pdf device driver for saving plots
pdf(out.fname, height=8.5,width=17)

p1 <- plot_line(filter(USA.corn.demand, technology=='FeedCrops'),
                      x_col      = 'year',
                      y_col      = 'value',
                      group_col  = 'scenario',
                      color_col  = 'scenario',
                      leg_nrow   = 1,
                      x_lab      = NULL, 
                      y_lab      = 'Mt',
                      plot_title = 'Feed crops')

p1 <- p1 + ylim(100, 220) + theme(axis.text.x = element_blank(), axis.ticks = element_blank())

p2 <- plot_line(filter(USA.corn.demand, technology=='regional corn for ethanol'),
                x_col      = 'year',
                y_col      = 'value',
                group_col  = 'scenario',
                color_col  = 'scenario',
                leg_nrow   = 1,
                x_lab      = NULL,
                y_lab      = 'Mt',
                plot_title = 'Regional corn for ethanol')

p2 <- p2 + ylim(80, 160) + theme(axis.text.x = element_blank(), axis.ticks = element_blank())

p3 <- plot_line(filter(USA.corn.demand, technology=='FoodDemand_Staples'),
                x_col      = 'year',
                y_col      = 'value',
                group_col  = 'scenario',
                color_col  = 'scenario',
                leg_nrow   = 1,
                x_lab      = NULL,
                y_lab      = 'Mt',
                plot_title = 'Food crops')

p3 <- p3 + ylim(0, 8) + theme(axis.text.x = element_blank(), axis.ticks = element_blank())

p4 <- plot_line(filter(USA.corn.demand, technology=='NonFoodDemand_Crops'),
                x_col      = 'year',
                y_col      = 'value',
                group_col  = 'scenario',
                color_col  = 'scenario',
                leg_nrow   = 1,
                x_lab      = NULL,
                y_lab      = 'Mt',
                plot_title = 'Non food crops')

p4 <- p4 + ylim(60, 100) + theme(axis.text.x = element_blank(), axis.ticks = element_blank())

p5 <- plot_line(filter(USA.feed.cons, input=='FeedCrops'),
                      x_col      = 'year',
                      y_col      = 'value',
                      group_col  = 'scenario',
                      color_col  = 'scenario',
                      leg_nrow   = 1,
                      x_lab      = NULL,
                      y_lab      = 'Mt',
                      plot_title = 'Feed crops consumption')

p5 <- p5 + ylim(200, 360)

p6 <- plot_line(filter(USA.feed.cons, input=='FodderHerb_Residue'),
                 x_col      = 'year',
                 y_col      = 'value',
                 group_col  = 'scenario',
                 color_col  = 'scenario',
                 leg_nrow   = 1,
                 x_lab      = NULL,
                 y_lab      = 'Mt',
                 plot_title = 'Fodder herb residue consumption')

p6 <- p6 + ylim(80, 180)

p7 <- plot_line(filter(USA.feed.cons, input=='Pasture_FodderGrass'),
                x_col      = 'year',
                y_col      = 'value',
                group_col  = 'scenario',
                color_col  = 'scenario',
                leg_nrow   = 1,
                x_lab      = NULL,
                y_lab      = 'Mt',
                plot_title = 'Pasture fooder grass consumption')

p7 <- p7 + ylim(200, 420)

p8 <- plot_line(filter(USA.agg.land.alloc, landleaf=='crops'),
                x_col      = 'year',
                y_col      = 'value',
                group_col  = 'scenario',
                color_col  = 'scenario',
                leg_nrow   = 1,
                x_lab      = NULL,
                y_lab      = expression(million~km^2),
                plot_title = 'USA - Cropland')

p8 <- p8 + ylim(1.1, 1.4)

figure <- ggarrange(p1, p2, p3, p4, p5, p6, p7, p8,
                  labels        = 'AUTO',
                  font.label    = list(size=15,family='Helvetica',color='black'),
                  ncol          = 4,
                  nrow          = 2,
                  common.legend = TRUE,
                  legend        = 'bottom')

figure <- annotate_figure(figure, top=text_grob('USA - Corn demand (top row); USA - Animal feed (bottom row)', size=15,family='Helvetica',color='black',face='bold'))

print(figure)

p9 <- plot_line_facet(filter(USA.agg.land.alloc, landleaf %in% c('crops','grass','pasture (grazed)', 'pasture (other)')),
                      x_col      = 'year',
                      y_col      = 'value',
                      group_col  = 'scenario',
                      color_col  = 'scenario',
                      linetype_col = NULL,
                      facet_var  = 'landleaf',
                      facet_ncol = 2,
                      facet_scale = 'free_y',
                      leg_nrow   = 1,
                      x_lab      = NULL,
                      y_lab      = expression(million~km^2),
                      plot_title = 'Region - USA')

print (p9)


# Facet grid does not allow free scale in both directions. Thus, values not readable.
# p9 <- plot_line_facet_grid(filter(plot.data,
#                                   scenario %in% select_scenarios,
#                                   year >= 2015,
#                                   region == 'USA',
#                                   sector %in% c('Beef', 'Dairy', 'Pork', 'Poultry')), 
#                             x_col       = 'year',
#                             y_col       = 'value', 
#                             group_col   = 'scenario',
#                             color_col   = 'scenario',
#                             linetype    = NULL,
#                             facet_row   = 'Desc', 
#                             facet_col   = 'sector', 
#                             facet_scale = 'free', 
#                             leg_nrow    = 2, 
#                             y_lab       = NULL, 
#                             plot_title  = 'USA')
# 
# print(p9)

p9 <- plot_line(filter(USA.meat.price, sector == 'Beef'),
                x_col      = 'year',
                y_col      = 'value',
                group_col  = 'scenario',
                color_col  = 'scenario',
                leg_nrow   = 1,
                x_lab      = NULL,
                y_lab      = expression(paste(2010~'$',~ton^-1)), 
                plot_title = 'USA beef price')

p9 <- p9 + ylim(3800, 5200) + theme(axis.text.x = element_blank(), axis.ticks = element_blank())

p10 <- plot_line(filter(USA.meat.price, sector == 'Pork'),
                x_col      = 'year',
                y_col      = 'value',
                group_col  = 'scenario',
                color_col  = 'scenario',
                leg_nrow   = 1,
                x_lab      = NULL,
                y_lab      = expression(paste(2010~'$',~ton^-1)), 
                plot_title = 'USA pork price')

p10 <- p10 + ylim(1600, 2200) + theme(axis.text.x = element_blank(), axis.ticks = element_blank())

p11 <- plot_line(filter(USA.meat.price, sector == 'Poultry'),
                x_col      = 'year',
                y_col      = 'value',
                group_col  = 'scenario',
                color_col  = 'scenario',
                leg_nrow   = 1,
                x_lab      = NULL,
                y_lab      = expression(paste(2010~'$',~ton^-1)), 
                plot_title = 'USA poultry price')

p11 <- p11 + ylim(1400, 1800) + theme(axis.text.x = element_blank(), axis.ticks = element_blank())

p12 <- plot_line(filter(USA.meat.price, sector == 'Dairy'),
                 x_col      = 'year',
                 y_col      = 'value',
                 group_col  = 'scenario',
                 color_col  = 'scenario',
                 leg_nrow   = 1,
                 x_lab      = NULL,
                 y_lab      = expression(paste(2010~'$',~ton^-1)), 
                 plot_title = 'USA dairy price')

p12 <- p12 + ylim(380, 440) + theme(axis.text.x = element_blank(), axis.ticks = element_blank())

p13 <- plot_line(filter(USA.meat.prod, sector == 'Beef'),
                 x_col      = 'year',
                 y_col      = 'value',
                 group_col  = 'scenario',
                 color_col  = 'scenario',
                 leg_nrow   = 1,
                 x_lab      = NULL,
                 y_lab      = 'Mt', 
                 plot_title = 'USA beef production')

p13 <- p13 + ylim(10, 26) + theme(axis.text.x = element_blank(), axis.ticks = element_blank())

p14 <- plot_line(filter(USA.meat.prod, sector == 'Pork'),
                 x_col      = 'year',
                 y_col      = 'value',
                 group_col  = 'scenario',
                 color_col  = 'scenario',
                 leg_nrow   = 1,
                 x_lab      = NULL,
                 y_lab      = 'Mt', 
                 plot_title = 'USA pork production')

p14 <- p14 + ylim(10, 16) + theme(axis.text.x = element_blank(), axis.ticks = element_blank())

p15 <- plot_line(filter(USA.meat.prod, sector == 'Poultry'),
                 x_col      = 'year',
                 y_col      = 'value',
                 group_col  = 'scenario',
                 color_col  = 'scenario',
                 leg_nrow   = 1,
                 x_lab      = NULL,
                 y_lab      = 'Mt', 
                 plot_title = 'USA poultry production')

p15 <- p15 + ylim(25, 42) + theme(axis.text.x = element_blank(), axis.ticks = element_blank())

p16 <- plot_line(filter(USA.meat.prod, sector == 'Dairy'),
                 x_col      = 'year',
                 y_col      = 'value',
                 group_col  = 'scenario',
                 color_col  = 'scenario',
                 leg_nrow   = 1,
                 x_lab      = NULL,
                 y_lab      = 'Mt', 
                 plot_title = 'USA dairy production')

p16 <- p16 + ylim(90, 160) + theme(axis.text.x = element_blank(), axis.ticks = element_blank())

p17 <- plot_line(filter(USA.food.cons, subsector == 'Beef'),
                x_col      = 'year',
                y_col      = 'value',
                group_col  = 'scenario',
                color_col  = 'scenario',
                leg_nrow   = 1,
                x_lab      = NULL,
                y_lab      = 'Pcal', 
                plot_title = 'USA beef consumption')

p17 <- p17 + ylim(15, 30)

p18 <- plot_line(filter(USA.food.cons, subsector == 'Pork'),
                x_col      = 'year',
                y_col      = 'value',
                group_col  = 'scenario',
                color_col  = 'scenario',
                leg_nrow   = 1,
                x_lab      = NULL,
                y_lab      = 'Pcal', 
                plot_title = 'USA pork consumption')

p18 <- p18 + ylim(25, 45)

p19 <- plot_line(filter(USA.food.cons, subsector == 'Poultry'),
                x_col      = 'year',
                y_col      = 'value',
                group_col  = 'scenario',
                color_col  = 'scenario',
                leg_nrow   = 1,
                x_lab      = NULL,
                y_lab      = 'Pcal', 
                plot_title = 'USA poultry consumption')

p19 <- p19 + ylim(25, 45)

p20 <- plot_line(filter(USA.food.cons, subsector == 'Dairy'),
                 x_col      = 'year',
                 y_col      = 'value',
                 group_col  = 'scenario',
                 color_col  = 'scenario',
                 leg_nrow   = 1,
                 x_lab      = NULL,
                 y_lab      = 'Pcal', 
                 plot_title = 'USA dairy consumption')

p20 <- p20 + ylim(45, 70)

figure <- ggarrange(p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20,
                    labels        = 'AUTO',
                    font.label    = list(size=15,family='Helvetica',color='black'),
                    ncol          = 4,
                    nrow          = 3,
                    common.legend = TRUE,
                    legend        = 'bottom')

print(figure)

# Figures for China
p21 <- plot_line(filter(meat.price.data.table,
                       scenario %in% select_scenarios,
                       year >=2015,
                       region == 'China', 
                       sector == 'Beef'),
                x_col      = 'year',
                y_col      = 'value',
                group_col  = 'scenario',
                color_col  = 'scenario',
                leg_nrow   = 1,
                x_lab      = NULL,
                y_lab      = expression(paste(2010~'$',~ton^-1)), 
                plot_title = 'China beef price')

p21 <- p21 + ylim(6000, 8000) + theme(axis.text.x = element_blank(), axis.ticks = element_blank())

p22 <- plot_line(filter(meat.price.data.table,
                        scenario %in% select_scenarios,
                        year >=2015,
                        region == 'China', 
                        sector == 'Pork'),
                 x_col      = 'year',
                 y_col      = 'value',
                 group_col  = 'scenario',
                 color_col  = 'scenario',
                 leg_nrow   = 1,
                 x_lab      = NULL,
                 y_lab      = expression(paste(2010~'$',~ton^-1)), 
                 plot_title = 'China pork price')

p22 <- p22 + ylim(3600, 4200) + theme(axis.text.x = element_blank(), axis.ticks = element_blank())

p23 <- plot_line(filter(meat.price.data.table,
                        scenario %in% select_scenarios,
                        year >=2015,
                        region == 'China', 
                        sector == 'Poultry'),
                 x_col      = 'year',
                 y_col      = 'value',
                 group_col  = 'scenario',
                 color_col  = 'scenario',
                 leg_nrow   = 1,
                 x_lab      = NULL,
                 y_lab      = expression(paste(2010~'$',~ton^-1)), 
                 plot_title = 'China poultry price')

p23 <- p23 + ylim(2200, 2450) + theme(axis.text.x = element_blank(), axis.ticks = element_blank())

p24 <- plot_line(filter(meat.price.data.table,
                        scenario %in% select_scenarios,
                        year >=2015,
                        region == 'China', 
                        sector == 'Dairy'),
                 x_col      = 'year',
                 y_col      = 'value',
                 group_col  = 'scenario',
                 color_col  = 'scenario',
                 leg_nrow   = 1,
                 x_lab      = NULL,
                 y_lab      = expression(paste(2010~'$',~ton^-1)), 
                 plot_title = 'China dairy price')

p24 <- p24 + ylim(550, 700) + theme(axis.text.x = element_blank(), axis.ticks = element_blank())

p25 <- plot_line(filter(meat.prod.data.table,
                        scenario %in% select_scenarios,
                        year >=2015,
                        region == 'China',
                        sector == 'Beef'),
                 x_col      = 'year',
                 y_col      = 'value',
                 group_col  = 'scenario',
                 color_col  = 'scenario',
                 leg_nrow   = 1,
                 x_lab      = NULL,
                 y_lab      = 'Mt', 
                 plot_title = 'China beef production')

p25 <- p25 + ylim(5, 10) + theme(axis.text.x = element_blank(), axis.ticks = element_blank())

p26 <- plot_line(filter(meat.prod.data.table,
                        scenario %in% select_scenarios,
                        year >=2015,
                        region == 'China',
                        sector == 'Pork'),
                 x_col      = 'year',
                 y_col      = 'value',
                 group_col  = 'scenario',
                 color_col  = 'scenario',
                 leg_nrow   = 1,
                 x_lab      = NULL,
                 y_lab      = 'Mt', 
                 plot_title = 'China pork production')

p26 <- p26 + ylim(50, 80) + theme(axis.text.x = element_blank(), axis.ticks = element_blank())

p27 <- plot_line(filter(meat.prod.data.table,
                        scenario %in% select_scenarios,
                        year >=2015,
                        region == 'China',
                        sector == 'Poultry'),
                 x_col      = 'year',
                 y_col      = 'value',
                 group_col  = 'scenario',
                 color_col  = 'scenario',
                 leg_nrow   = 1,
                 x_lab      = NULL,
                 y_lab      = 'Mt', 
                 plot_title = 'China poultry production')

p27 <- p27 + ylim(45, 70) + theme(axis.text.x = element_blank(), axis.ticks = element_blank())

p28 <- plot_line(filter(meat.prod.data.table,
                        scenario %in% select_scenarios,
                        year >=2015,
                        region == 'China',
                        sector == 'Dairy'),
                 x_col      = 'year',
                 y_col      = 'value',
                 group_col  = 'scenario',
                 color_col  = 'scenario',
                 leg_nrow   = 1,
                 x_lab      = NULL,
                 y_lab      = 'Mt', 
                 plot_title = 'China dairy production')

p28 <- p28 + ylim(35, 55) + theme(axis.text.x = element_blank(), axis.ticks = element_blank())

p29 <- plot_line(filter(food.cons.data.table,
                        scenario %in% select_scenarios,
                        year      >=2015,
                        region    == 'China',
                        subsector == 'Beef'),
                 x_col      = 'year',
                 y_col      = 'value',
                 group_col  = 'scenario',
                 color_col  = 'scenario',
                 leg_nrow   = 1,
                 x_lab      = NULL,
                 y_lab      = 'Pcal', 
                 plot_title = 'China beef consumption')

p29 <- p29 + ylim(10, 16)

p30 <- plot_line(filter(food.cons.data.table,
                        scenario %in% select_scenarios,
                        year      >=2015,
                        region    == 'China',
                        subsector == 'Pork'),
                 x_col      = 'year',
                 y_col      = 'value',
                 group_col  = 'scenario',
                 color_col  = 'scenario',
                 leg_nrow   = 1,
                 x_lab      = NULL,
                 y_lab      = 'Pcal', 
                 plot_title = 'China pork consumption')

p30 <- p30 + ylim(170, 250)

p31 <- plot_line(filter(food.cons.data.table,
                        scenario %in% select_scenarios,
                        year      >=2015,
                        region    == 'China',
                        subsector == 'Poultry'),
                 x_col      = 'year',
                 y_col      = 'value',
                 group_col  = 'scenario',
                 color_col  = 'scenario',
                 leg_nrow   = 1,
                 x_lab      = NULL,
                 y_lab      = 'Pcal', 
                 plot_title = 'China poultry consumption')

p31 <- p31 + ylim(55, 85)

p32 <- plot_line(filter(food.cons.data.table,
                        scenario %in% select_scenarios,
                        year      >=2015,
                        region    == 'China',
                        subsector == 'Dairy'),
                 x_col      = 'year',
                 y_col      = 'value',
                 group_col  = 'scenario',
                 color_col  = 'scenario',
                 leg_nrow   = 1,
                 x_lab      = NULL,
                 y_lab      = 'Pcal', 
                 plot_title = 'China dairy consumption')

p32 <- p32 + ylim(22, 34)

figure <- ggarrange(p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32,
                    labels        = 'AUTO',
                    font.label    = list(size=15,family='Helvetica',color='black'),
                    ncol          = 4,
                    nrow          = 3,
                    common.legend = TRUE,
                    legend        = 'bottom')

print(figure)

dev.off()