# Author - Eva Sinha, Pacific Northwest National Lab, eva.sinha@pnnl.gov

library(rgcam)
library(ggpubr)
library(gridExtra)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)


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

# ----- Query energy price
energy.price.data.table <- getQuery(prj, query = 'final energy prices')

# Convert value to 2010
energy.price.data.table <- energy.price.data.table %>%
  mutate(value = value*3.22, Units = '2010$/GJ') # from 1975$/GJ to 2010$/GJ

# Estimate change compared to the reference scenario
energy.price.data.table <- energy.price.data.table %>%
  group_by(Units, region, fuel, year) %>%
  filter(year >= 2015) %>%
  mutate(change_ref = (value - value[which(scenario=='Reference_SSP2-4p5')]),
         per_change_ref = 100*(value - value[which(scenario=='Reference_SSP2-4p5')])/value[which(scenario=='Reference_SSP2-4p5')])

# ----- Query primary energy consumption by region
# energy.cons.data.table <- getQuery(prj, query = 'primary energy consumption by region (avg fossil efficiency)')
energy.cons.data.table <- getQuery(prj, query = 'primary energy consumption by region (direct equivalent)')

# Estimate total energy consumption by each region and add as additional rows
energy.cons.data.table <- bind_rows(energy.cons.data.table, energy.cons.data.table %>% 
                                      group_by(Units, scenario, region, year) %>% 
                                      summarize(fuel = 'Total energy consumption',
                                                value = sum(value)))

# Estimate values for global region and add as additional rows
energy.cons.data.table <- bind_rows(energy.cons.data.table, energy.cons.data.table %>% 
                                      group_by(Units, scenario, fuel, year) %>% 
                                      summarize(region = 'Global',
                                                value = sum(value)))

# Estimate change compared to the reference scenario
energy.cons.data.table <- energy.cons.data.table %>%
  group_by(Units, region, fuel, year) %>%
  filter(year >= 2015) %>%
  mutate(change_ref = (value - value[which(scenario=='Reference_SSP2-4p5')]),
         per_change_ref = 100*(value - value[which(scenario=='Reference_SSP2-4p5')])/value[which(scenario=='Reference_SSP2-4p5')])

# ----- Query gas consumption by sector
gas.cons.data.table <- getQuery(prj, query = 'gas consumption by sector')

# Estimate values for global region and add as additional rows
gas.cons.data.table <- bind_rows(gas.cons.data.table, gas.cons.data.table %>% 
                                   group_by(Units, scenario, sector, input, year) %>% 
                                   summarize(region = 'Global',
                                             value = sum(value)))

# Estimate change compared to the reference scenario
gas.cons.data.table <- gas.cons.data.table %>%
  group_by(Units, region, sector, year) %>%
  filter(year >= 2015, sector == 'N fertilizer') %>%
  mutate(change_ref = (value - value[which(scenario=='Reference_SSP2-4p5')]),
         per_change_ref = 100*(value - value[which(scenario=='Reference_SSP2-4p5')])/value[which(scenario=='Reference_SSP2-4p5')])

# Add long label for scenarios
energy.price.data.table <- add_long_label_scenarios(energy.price.data.table)
energy.cons.data.table  <- add_long_label_scenarios(energy.cons.data.table)
gas.cons.data.table     <- add_long_label_scenarios(gas.cons.data.table)

# Drop unused levels
energy.price.data.table$scenario <- droplevels(energy.price.data.table$scenario)
energy.cons.data.table$scenario  <- droplevels(energy.cons.data.table$scenario)
gas.cons.data.table$scenario     <- droplevels(gas.cons.data.table$scenario)

# Selected scenarios for plotting
select_scenarios <- c('Reference', 
                      'Global constraint (15%) on fertilizer')
                      # 'Global constraint (15%) - fertilizer io flat', 
                      # 'Global constraint (15%) - fertilizer io steep',
                      # 'Global constraint (15%) - fertilizer io regional')

# Subset energy price
energy.price.data.table      <- energy.price.data.table %>% 
  filter(scenario %in% select_scenarios,
         year     >= 2015)

# Subset Global energy consumption
Global.energy.cons      <- energy.cons.data.table %>%
  filter(scenario %in% select_scenarios,
         year     >= 2015,
         region   == 'Global')

# Subset Global gas consumption
Global.gas.cons      <- gas.cons.data.table %>%
  filter(scenario %in% select_scenarios,
         year     >= 2015,
         region   == 'Global')

print('USA energy price for the Reference and global constraint scenario in 2100')
print(filter(energy.price.data.table, 
             year %in% c(2060, 2100), 
             region %in% c('China', 'India', 'USA'),
             fuel %in% c('delivered biomass'),
             scenario %in% c('Reference','Global constraint (15%) on fertilizer')))

print('Global gas consumption for the Reference and global constraint scenario in 2100')
print(filter(Global.gas.cons, 
             year %in% c(2060, 2100),
             scenario %in% c('Reference','Global constraint (15%) on fertilizer')))

print('Global energy consumption for the Reference and global constraint scenario in 2100')
print(filter(Global.energy.cons, 
             year == 2060, 
             # fuel %in% c('a oil', 'b natural gas' ,'c coal' ,'d biomass', 'j traditional biomass','Total energy consumption'),
             scenario %in% c('Global constraint (15%) on fertilizer')))

print(filter(Global.energy.cons, 
             year == 2100, 
             fuel %in% c('a oil', 'b natural gas' ,'c coal' ,'d biomass', 'j traditional biomass','Total energy consumption'),
             scenario %in% c('Global constraint (15%) on fertilizer')))

print('Global energy consumption for the high bionergy scenario in 2060 and 2100')
print(filter(energy.cons.data.table,
             scenario %in% c('Low carbon'),
             region   == 'Global',
             year %in% c(2100),
             fuel %in% c('a oil', 'b natural gas' ,'c coal' ,'d biomass', 'j traditional biomass','Total energy consumption')))
             # fuel %in% c('Total energy consumption')))

# Define path to output file
out.folder <- '../figures/'
out.fname  <- 'FigureS5.pdf'
out.fname  <- paste(out.folder, out.fname, sep='')
# Delete existing file
unlink(out.fname)
# Start pdf device driver for saving plots
pdf(out.fname, height=11,width=17)

# ----- China, India, and USA plots
p1 <- plot_line(filter(energy.price.data.table, region=='China', fuel == 'delivered biomass'),
                x_col      = 'year',
                y_col      = 'change_ref',
                group_col  = 'scenario',
                color_col  = 'scenario',
                leg_nrow   = 1,
                x_lab      = NULL,
                y_lab      = expression(paste(2010~'$',~GJ^-1)),
                plot_title = 'China - delivered biomass energy price')

p1 <- p1 + theme(axis.text.x = element_blank(), axis.ticks = element_blank())

p2 <- plot_line(filter(energy.price.data.table, region=='India', fuel == 'delivered biomass'),
                x_col      = 'year',
                y_col      = 'change_ref',
                group_col  = 'scenario',
                color_col  = 'scenario',
                leg_nrow   = 1,
                x_lab      = NULL,
                y_lab      = expression(paste(2010~'$',~GJ^-1)),
                plot_title = 'India - delivered biomass energy price')

p2 <- p2 + theme(axis.text.x = element_blank(), axis.ticks = element_blank())

p3 <- plot_line(filter(energy.price.data.table, region=='USA', fuel == 'delivered biomass'),
                x_col      = 'year',
                y_col      = 'change_ref',
                group_col  = 'scenario',
                color_col  = 'scenario',
                leg_nrow   = 1,
                x_lab      = NULL,
                y_lab      = expression(paste(2010~'$',~GJ^-1)),
                plot_title = 'USA - delivered biomass energy price')

p3 <- p3 + theme(axis.text.x = element_blank(), axis.ticks = element_blank())

# ----- Global plots
p4 <- plot_line(Global.gas.cons,
                x_col      = 'year',
                y_col      = 'change_ref',
                group_col  = 'scenario',
                color_col  = 'scenario',
                leg_nrow   = 1,
                x_lab      = NULL,
                y_lab      = 'EJ',
                plot_title = 'Global - gas consumption for N fertilizers')

p4 <- p4 + theme(axis.text.x = element_blank(), axis.ticks = element_blank())

p5 <- plot_line(filter(Global.energy.cons, fuel == 'd biomass'),
                x_col      = 'year',
                y_col      = 'change_ref',
                group_col  = 'scenario',
                color_col  = 'scenario',
                leg_nrow   = 1,
                x_lab      = NULL,
                y_lab      = 'EJ',
                plot_title = 'Global - energy consumption from Biomass')

p5 <- p5 + theme(axis.text.x = element_blank(), axis.ticks = element_blank())

p6 <- plot_line(filter(Global.energy.cons, fuel == 'j traditional biomass'),
                x_col      = 'year',
                y_col      = 'change_ref',
                group_col  = 'scenario',
                color_col  = 'scenario',
                leg_nrow   = 1,
                x_lab      = NULL,
                y_lab      = 'EJ',
                plot_title = 'Global - energy consumption from traditional biomass')

p6 <- p6 + theme(axis.text.x = element_blank(), axis.ticks = element_blank())

p7 <- plot_line(filter(Global.energy.cons, fuel == 'a oil'),
                x_col      = 'year',
                y_col      = 'change_ref',
                group_col  = 'scenario',
                color_col  = 'scenario',
                leg_nrow   = 1,
                x_lab      = NULL,
                y_lab      = 'EJ',
                plot_title = 'Global - energy consumption from oil')

p8 <- plot_line(filter(Global.energy.cons, fuel == 'b natural gas'),
                 x_col      = 'year',
                 y_col      = 'change_ref',
                 group_col  = 'scenario',
                 color_col  = 'scenario',
                 leg_nrow   = 1,
                 x_lab      = NULL,
                 y_lab      = 'EJ',
                 plot_title = 'Global - energy consumption from natural gas')

p9 <- plot_line(filter(Global.energy.cons, fuel == 'c coal'),
                x_col      = 'year',
                y_col      = 'change_ref',
                group_col  = 'scenario',
                color_col  = 'scenario',
                leg_nrow   = 1,
                x_lab      = NULL,
                y_lab      = 'EJ',
                plot_title = 'Global - energy consumption from coal')

figure <- ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9,
                    labels        = 'AUTO',
                    font.label    = list(size=15,family='Helvetica',color='black'),
                    ncol          = 3,
                    nrow          = 3,
                    common.legend = TRUE,
                    legend        = 'bottom')
print(figure)
# annotate_figure(figure,
#                 top = text_grob('Change compared to the Reference scenario', face='bold', size=14))

p10 <- plot_line(filter(Global.energy.cons, fuel == 'Total energy consumption'),
                 x_col      = 'year',
                 y_col      = 'change_ref',
                 group_col  = 'scenario',
                 color_col  = 'scenario',
                 leg_nrow   = 1,
                 x_lab      = NULL,
                 y_lab      = 'EJ',
                 plot_title = 'Global - total energy consumption \nChange compared to the Reference scenario')

print(p10)


# Grid table
table.tot.energy <- Global.energy.cons %>%
  filter(region=='Global', year %in% c(2060, 2100),
         fuel %in% c('a oil', 'b natural gas' ,'c coal' ,'d biomass', 'j traditional biomass','Total energy consumption'),
         scenario %in% c('Reference','Global constraint (15%) on fertilizer'))

table.tot.energy$value          <- format(table.tot.energy$value, digits = 2)
table.tot.energy$change_ref     <- format(table.tot.energy$change_ref, digits = 2)
table.tot.energy$per_change_ref <- format(table.tot.energy$per_change_ref, digits = 2)

grid.arrange(text_grob('Energy consumption for 2100', size=15,family='Helvetica',color='black',face='bold'),
             tableGrob(table.tot.energy, rows = NULL, theme = ttheme_minimal()),
             ncol=1)

dev.off()