# Author - Eva Sinha, Pacific Northwest National Lab, eva.sinha@pnnl.gov

library(rgcam)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(readxl)
library(RColorBrewer)
library(colorRamps)
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

# Retrieve agricultural production for all scenarios in the dataset, formatted as a single table
ag.prod.data.table <- getQuery(prj, query = 'ag production by crop type')

# Estimate values for global region and add as additional rows
ag.prod.data.table <- bind_rows(ag.prod.data.table, ag.prod.data.table %>% 
                                  group_by(Units, scenario, sector, output, year) %>% 
                                  summarize(region = 'Global',
                                            value = sum(value)))

# Add long label for scenarios
ag.prod.data.table <- add_long_label_scenarios(ag.prod.data.table)
ag.prod.data.table$sector <- param_labeller('variable', ag.prod.data.table$sector)
ag.prod.data.table$region <- param_labeller('variable', ag.prod.data.table$region)

select_scenarios <- c('Reference', 'Global constraint (15%) on fertilizer')
                      # 'Global constraint (15%) - fertilizer io flat', 
                      # 'Global constraint (15%) - fertilizer io steep',
                      # 'Global constraint (15%) - fertilizer io regional')

print(filter(ag.prod.data.table, scenario=='Reference', year==2080, region!='Global', sector=='Corn') %>% top_n(1, value))
print(filter(ag.prod.data.table, scenario=='Reference', year==2080, region!='Global', sector=='Oil crop') %>% top_n(1, value))
print(filter(ag.prod.data.table, scenario=='Reference', year==2080, region!='Global', sector=='Rice') %>% top_n(1, value))
print(filter(ag.prod.data.table, scenario=='Reference', year==2080, region!='Global', sector=='Sugar crop') %>% top_n(1, value))
print(filter(ag.prod.data.table, scenario=='Reference', year==2080, region!='Global', sector=='Wheat') %>% top_n(1, value))
print(filter(ag.prod.data.table, scenario=='Reference', year==2080, region!='Global', sector=='Biomass') %>% top_n(1, value))

print(ag.prod.data.table %>%
        filter(scenario %in% select_scenarios, year==2100, region!='Global', sector=='Rice') %>% 
        group_by(Units, region, sector, output, year) %>%
        mutate(change = (value - value[scenario == 'Reference'])) %>%
        ungroup() %>%
        top_n(-5, change))

print(ag.prod.data.table %>%
        filter(scenario %in% select_scenarios, year==2100, region!='Global', sector=='Rice') %>% 
        group_by(Units, region, sector, output, year) %>%
        mutate(change = (value - value[scenario == 'Reference'])) %>%
        ungroup() %>%
        top_n(5, change))

# Define path to output file
out.folder <- '../figures/'
out.fname  <- 'Figure3.pdf'
out.fname  <- paste(out.folder, out.fname, sep='')
# Delete existing file
unlink(out.fname)
# Start pdf device driver for saving plots
pdf(out.fname, height=8.5, width=11)

plot_change(filter(ag.prod.data.table, scenario %in% c('Reference','Global constraint (15%) on fertilizer'), region=='Global', !(sector %in% c('Forest','Biomass'))), 
            xaxis.var  = 'sector',
            value.var  = 'value',
            select_yr  = 2100, 
            facet_ncol = 4,
            y_lab      = expression(paste(Agricultural~production~change~'[',Mt~yr^-1,']')), 
            plot_title = 'Global - change in agricultural production from Reference scenario for 2100',
            leg_keyword = 'production')

plot_per_change(filter(ag.prod.data.table, scenario %in% c('Reference','Global constraint (15%) on fertilizer'), region=='Global', !(sector %in% c('Forest','Biomass'))),
                xaxis.var  = 'sector',
                value.var  = 'value',
                select_yr  = 2100,
                facet_ncol = 4,
                y_lab      = '% change in agricultural production',
                plot_title = 'Global - % change in agricultural production from Reference scenario for 2100')

plot_per_change_position_dodge(filter(ag.prod.data.table, scenario %in% select_scenarios, region=='Global', !(sector %in% c('Forest','Biomass'))), 
                               xaxis.var  = 'sector',
                               value.var  = 'value',
                               select_yr  = 2100,
                               leg_nrow   = 2,
                               y_lab      = '% change in agricultural production', 
                               plot_title = 'Global - % change in agricultural production from Reference scenario for 2100')

plot_change(filter(ag.prod.data.table, scenario %in% c('Reference','Global constraint (15%) on fertilizer'), region=='China', !(sector %in% c('Forest','Biomass'))), 
            xaxis.var  = 'sector',
            value.var  = 'value',
            select_yr  = 2100, 
            facet_ncol = 4,
            y_lab      = expression(paste(Agricultural~production~change~'[',Mt~yr^-1,']')), 
            plot_title = 'China - change in agricultural production from Reference scenario for 2100',
            leg_keyword = 'production')

plot_change(filter(ag.prod.data.table, scenario %in% c('Reference','Global constraint (15%) on fertilizer'), region=='India', !(sector %in% c('Forest','Biomass'))), 
            xaxis.var  = 'sector',
            value.var  = 'value',
            select_yr  = 2100, 
            facet_ncol = 4,
            y_lab      = expression(paste(Agricultural~production~change~'[',Mt~yr^-1,']')), 
            plot_title = 'India - change in agricultural production from Reference scenario for 2100',
            leg_keyword = 'production')

plot_change(filter(ag.prod.data.table, scenario %in% c('Reference','Global constraint (15%) on fertilizer'), region=='USA', !(sector %in% c('Forest','Biomass'))), 
            xaxis.var  = 'sector',
            value.var  = 'value',
            select_yr  = 2100, 
            facet_ncol = 4,
            y_lab      = expression(paste(Agricultural~production~change~'[',Mt~yr^-1,']')), 
            plot_title = 'USA - change in agricultural production from Reference scenario for 2100',
            leg_keyword = 'production')

plot_change(filter(ag.prod.data.table, scenario %in% c('Reference','Global constraint (15%) on fertilizer'), sector=='Corn', region != 'Global'),
            xaxis.var  = 'region',
            value.var  = 'value',
            select_yr  = 2100,
            facet_ncol = 4,
            y_lab      = expression(paste(Agricultural~production~change~'[',Mt~yr^-1,']')),
            plot_title = 'Corn - change in agricultural production from Reference scenario for 2100',
            leg_keyword = 'production')

plot_change_facet(filter(ag.prod.data.table, 
                   scenario %in% c('Reference','Global constraint (15%) on fertilizer'), 
                   sector %in% c('Corn', 'Oil crop','Sugar crop'), 
                   region != 'Global'), 
            xaxis.var  = 'region',
            value.var  = 'value',
            facet.var  = 'sector',
            select_yr  = 2100, 
            facet_ncol = 4,
            y_lab      = expression(paste(Agricultural~production~change~'[',Mt~yr^-1,']')), 
            plot_title = 'Change in agricultural production from Reference scenario for 2100',
            leg_keyword = 'production')

plot_change(filter(ag.prod.data.table, scenario %in% c('Reference','Global constraint (15%) on fertilizer'), sector=='Oil crop', region != 'Global'),
            xaxis.var  = 'region',
            value.var  = 'value',
            select_yr  = 2100,
            facet_ncol = 4,
            y_lab      = expression(paste(Agricultural~production~change~'[',Mt~yr^-1,']')),
            plot_title = 'OilCrop - change in agricultural production from Reference scenario for 2100',
            leg_keyword = 'production')

plot_change(filter(ag.prod.data.table, scenario %in% c('Reference','Global constraint (15%) on fertilizer'), sector=='Rice', region != 'Global'),
            xaxis.var  = 'region',
            value.var  = 'value',
            select_yr  = 2100,
            facet_ncol = 4,
            y_lab      = expression(paste(Agricultural~production~change~'[',Mt~yr^-1,']')),
            plot_title = 'Rice - change in agricultural production from Reference scenario for 2100',
            leg_keyword = 'production')

plot_per_change(filter(ag.prod.data.table, scenario %in% c('Reference','Global constraint (15%) on fertilizer'), sector=='Rice', region != 'Global'),
                xaxis.var  = 'region',
                value.var  = 'value',
                select_yr  = 2100,
                facet_ncol = 4,
                y_lab      = '% change in agricultural production',
                plot_title = 'Rice - % change in agricultural production from Reference scenario for 2100')

plot_change_position_dodge(filter(ag.prod.data.table, scenario %in% select_scenarios, sector=='Rice', region != 'Global'),
            xaxis.var  = 'region',
            value.var  = 'value',
            select_yr  = 2100,
            leg_nrow   = 2,
            y_lab      = expression(paste(Agricultural~production~change~'[',Mt~yr^-1,']')),
            plot_title = 'Rice - change in agricultural production from Reference scenario for 2100')

plot_change(filter(ag.prod.data.table, scenario %in% c('Reference','Global constraint (15%) on fertilizer'), sector=='Sugar crop', region!='Global'),
            xaxis.var  = 'region',
            value.var  = 'value',
            select_yr  = 2100,
            facet_ncol = 4,
            y_lab      = expression(paste(Agricultural~production~change~'[',Mt~yr^-1,']')),
            plot_title = 'SugarCrop - change in agricultural production from Reference scenario for 2100',
            leg_keyword = 'production')

plot_change(filter(ag.prod.data.table, scenario %in% c('Reference','Global constraint (15%) on fertilizer'), sector=='Wheat', region != 'Global'),
            xaxis.var  = 'region',
            value.var  = 'value',
            select_yr  = 2100,
            facet_ncol = 4,
            y_lab      = expression(paste(Agricultural~production~change~'[',Mt~yr^-1,']')),
            plot_title = 'Wheat - change in agricultural production from Reference scenario for 2100',
            leg_keyword = 'production')

plot_change(filter(ag.prod.data.table, scenario %in% c('Reference','Global constraint (15%) on fertilizer'), sector=='Biomass', region != 'Global'),
            xaxis.var  = 'region',
            value.var  = 'value',
            select_yr  = 2100,
            facet_ncol = 4,
            y_lab      = expression(paste(Agricultural~production~change~'[',Mt~yr^-1,']')),
            plot_title = 'Biomass - change in agricultural production from Reference scenario for 2100',
            leg_keyword = 'production')

dev.off()
