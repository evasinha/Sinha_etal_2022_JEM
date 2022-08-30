# Author - Eva Sinha, Pacific Northwest National Lab, eva.sinha@pnnl.gov

#________________________________________________________
color_pal = c( 'Reference'                                                = 'black',
               'Reference [SSP2-3p7]'                                     = 'blue',
               'Reference [SSP2-6p0]'                                     = 'orange',
               'Reference [SSP1-4p5]'                                     = 'red',
               'Reference [SSP3-4p5]'                                     = 'darkgreen',
               'Reference [SSP4-4p5]'                                     = 'green',
               'Reference [SSP5-4p5]'                                     = 'magenta',
               'Reference - fertilizer io flat'                           = 'gray75',
               'Reference - fertilizer io steep'                          = 'gray50',
               'Reference - fertilizer io regional'                       = 'gray25',
               'Reference with lo tech yield'                             = 'brown',
               'Global constraint (15%) on fertilizer'                    = 'green',
               'Global constraint (15%) on fertilizer [SSP2-3p7]'         = 'green',
               'Global constraint (15%) on fertilizer [SSP2-6p0]'         = 'green',
               'Global constraint (15%) on fertilizer [SSP1-4p5]'         = 'green',
               'Global constraint (15%) on fertilizer [SSP3-4p5]'         = 'green',
               'Global constraint (15%) on fertilizer [SSP4-4p5]'         = 'green',
               'Global constraint (15%) on fertilizer [SSP5-4p5]'         = 'green',
               'Global constraint (15%) - fertilizer io flat'             = 'greenyellow',
               'Global constraint (15%) - fertilizer io steep'            = 'darkgreen',
               'Global constraint (15%) - fertilizer io regional'         = 'orange',
               'Global constraint (15%) on fertilizer with lo tech yield'  = 'darkgreen',
               'Regional constraint (10%) on fertilizer'                   = 'orange',
               'Regional constraint (10%) on fertilizer with lo tech yield'= 'magenta',
               'Regional constraint (15%) on fertilizer'                   = 'red',
               'Regional constraint (15%) on fertilizer with lo tech yield'= 'orange',
               'Regional constraint (20%) on fertilizer'                   = 'gold',
               'Regional constraint (20%) on fertilizer with lo tech yield'= 'goldenrod4',
               'Low carbon'                                         = 'blue',
               'Carbon tax (25-5) with lo tech yield'                      = 'cyan',
               'Low carbon with global constraint (15%)'            = 'cyan',
               'high-yield'                     = 'red',
               'low-yield'                      = 'blue',
               'Irrigated high-yield'         = 'red',
               'Irrigated low-yield'          = 'blue',
               'Rainfed high-yield'           = 'green',
               'Rainfed low-yield'            = 'magenta',
               'Corn'                           = 'orange',
               'FodderGrass'                    = 'red',
               'SugarCrop'                      = 'magenta',
               'Rice'                           = 'blue',
               'Wheat'                          = 'green',
               'Maize'                          = 'red',
               'Miscanthus'                     = 'magenta',
               
               'Africa_Eastern'                  = 'magenta',
               'Africa_Northern'                 = 'cyan',
               'Africa_Southern'                 = 'magenta',                
               'Africa_Western'                  = 'magenta',
               'Argentina'                       = 'magenta',
               'Australia_NZ'                    = 'magenta',                   
               'Brazil'                          = 'magenta',
               'Canada'                          = 'darkgoldenrod',
               'Central America and Caribbean'   = 'magenta', 
               'Central Asia'                    = 'magenta',
               'China'                           = 'plum1',
               'EU-12'                           = 'coral',
               'EU-15'                           = 'brown',
               'European Free Trade Association' = 'magenta',
               'Europe_Eastern'                  = 'magenta',
               'Europe_Non_EU'                   = 'magenta',
               'India'                           = 'deepskyblue',
               'Indonesia'                       = 'magenta',
               'Japan'                           = 'magenta',
               'Mexico'                          = 'magenta',                        
               'Middle East'                     = 'purple',
               'Pakistan'                        = 'gold',
               'Russia'                          = 'palegreen',                        
               'South Africa'                    = 'magenta',
               'South America_Northern'          = 'magenta',
               'South America_Southern'          = 'magenta',         
               'South Asia'                      = 'magenta',
               'Southeast Asia'                  = 'seagreen1',
               'South Korea'                     = 'magenta',                   
               'USA'                             = 'darkgreen',
               'Global'                          = 'orange',
                
               'Forest (unmanaged)'              = 'darkgreen',
               'Forest (managed)'                = 'lightgreen',
               'Crops'                           = 'yellow',
               'Urban'                           = 'brown',
               'Grass'                           = 'gold',
               'Pasture (grazed)'                = 'khaki1',
               'Pasture (other)'                 = 'khaki4',
               
               'Feed crops'                      = 'darkgreen',
               'Food demand crops'               = 'lightgreen',
               'Non-food demand crops'           = 'orange',
               'Regional biomass oil'            = 'blue',
               'Regional corn for ethanol'       = 'brown',
               'Regional sugar for ethanol'      = 'red',
               
               'domestic corn'                   = 'orange',
               'imported corn'                   = 'blue',
               
               'domestic fibercrop'               = 'orange',
               'imported fibercrop'               = 'blue',
               
               'domestic oilcrop'                 = 'orange',
               'imported oilcrop'                 = 'blue',
               
               'domestic rice'                   = 'orange',
               'imported rice'                   = 'blue',
               
               'domestic sugarcrop'               = 'orange',
               'imported sugarcrop'               = 'blue',
               
               'domestic wheat'                   = 'orange',
               'imported wheat'                   = 'blue',
               
               'Oil'                              = 'brown4',

               'Biomass'                          = 'darkgreen',
               'Coal'                             = 'purple4',
               'Gas'                              = 'skyblue',
               'Natural gas'                      = 'skyblue',
               'Geothermal'                       = 'mediumorchid',
               'Hydro'                            = 'blue',
               'Nuclear'                          = 'orange',
               'Refined liquids'                  = 'brown',
               'Rooftop PV'                       = 'green',
               'Traditional biomass'              = 'green',
               'Solar'                            = 'yellow',
               'Wind'                             = 'hotpink',
               
               'Biomass Liquids'                  = 'darkgreen',
               'Coal-to-Liquids'                  = 'purple4', 
               'Gas-to-Liquids'                   = 'skyblue',  
               'Oil Refining'                     = 'brown')

#________________________________________________________ 
shape_pal = c( 'Cadoux et al., 2012'  = 21,
               'Chen et al., 2014'    = 22,
               'Guo et al., 2017'     = 23,
               'Ju et al., 2009'      = 24,
               'Ma et al., 2019'      = 25,
               'Swayer & Baker, 2014' = 25,
               'Sharma et al., 2019'  = 25)

#________________________________________________________          
param_labeller <- function(var, value){
  
  value <- as.character(value)
  
  if (var=='variable') {
    value[value=='Reference_lotechyield']            <- 'Reference with lo tech yield'
    value[value=='Reference_SSP2-4p5']               <- 'Reference'
    value[value=='Reference_SSP2-3p7']               <- 'Reference [SSP2-3p7]'
    value[value=='Reference_SSP2-6p0']               <- 'Reference [SSP2-6p0]'
    value[value=='Reference_SSP1-4p5']               <- 'Reference [SSP1-4p5]'
    value[value=='Reference_SSP3-4p5']               <- 'Reference [SSP3-4p5]'
    value[value=='Reference_SSP4-4p5']               <- 'Reference [SSP4-4p5]'
    value[value=='Reference_SSP5-4p5']               <- 'Reference [SSP5-4p5]'
    value[value=='fert_global_const_15_SSP2-4p5']    <- 'Global constraint (15%) on fertilizer'
    value[value=='fert_global_const_15_SSP2-3p7']    <- 'Global constraint (15%) on fertilizer [SSP2-3p7]'
    value[value=='fert_global_const_15_SSP2-6p0']    <- 'Global constraint (15%) on fertilizer [SSP2-6p0]'
    value[value=='fert_global_const_15_SSP1-4p5']    <- 'Global constraint (15%) on fertilizer [SSP1-4p5]'
    value[value=='fert_global_const_15_SSP3-4p5']    <- 'Global constraint (15%) on fertilizer [SSP3-4p5]'
    value[value=='fert_global_const_15_SSP4-4p5']    <- 'Global constraint (15%) on fertilizer [SSP4-4p5]'
    value[value=='fert_global_const_15_SSP5-4p5']    <- 'Global constraint (15%) on fertilizer [SSP5-4p5]'
    value[value=='fert_global_const_15_lotechyield'] <- 'Global constraint (15%) on fertilizer with lo tech yield'
    value[value=='fert_reg_const_10']                <- 'Regional constraint (10%) on fertilizer'
    value[value=='fert_reg_const_10_lotechyield']    <- 'Regional constraint (10%) on fertilizer with lo tech yield'
    value[value=='fert_reg_const_15']                <- 'Regional constraint (15%) on fertilizer'
    value[value=='fert_reg_const_15_lotechyield']    <- 'Regional constraint (15%) on fertilizer with lo tech yield'
    value[value=='fert_reg_const_20']                <- 'Regional constraint (20%) on fertilizer'
    value[value=='fert_reg_const_20_lotechyield']    <- 'Regional constraint (20%) on fertilizer with lo tech yield'
    value[value=='carbon_tax_25_5']                  <- 'Low carbon'
    value[value=='carbon_tax_25_5_lotechyield']      <- 'Carbon tax (25-5) with lo tech yield'
    value[value=='carbon_tax_25_5_fert_global_const_15']  <- 'Low carbon with global constraint (15%)'
    
    value[value=='Ref_fert_io_flat']                   <- 'Reference - fertilizer io flat'
    value[value=='Ref_fert_io_steep']                  <- 'Reference - fertilizer io steep'
    value[value=='Ref_fert_io_regional']               <- 'Reference - fertilizer io regional'
    value[value=='fert_global_const_15_fert_io_flat']  <- 'Global constraint (15%) - fertilizer io flat'
    value[value=='fert_global_const_15_fert_io_steep'] <- 'Global constraint (15%) - fertilizer io steep'
    value[value=='fert_global_const_15_fert_io_regional'] <- 'Global constraint (15%) - fertilizer io regional'
    
    value[value=='IRR']             <- 'Irrigated'
    value[value=='RFD']             <- 'Rainfed'
    value[value=='hi']              <- 'high-yield'
    value[value=='lo']              <- 'low-yield'
    
    value[value=='IRR_hi']          <- 'Irrigated high-yield'
    value[value=='IRR_lo']          <- 'Irrigated low-yield'
    value[value=='RFD_hi']          <- 'Rainfed high-yield'
    value[value=='RFD_lo']          <- 'Rainfed low-yield'
    
    value[value=='FeedCrops']                  <- 'Feed crops'
    value[value=='FoodDemand_Crops']           <- 'Food demand crops'
    value[value=='NonFoodDemand_Crops']        <- 'Non-food demand crops'
    value[value=='regional biomassOil']        <- 'Regional biomass oil'
    value[value=='regional corn for ethanol']  <- 'Regional corn for ethanol'
    value[value=='regional sugar for ethanol'] <- 'Regional sugar for ethanol'
    
    value[value=='regional corn']          <- 'Regional corn'
    value[value=='regional oilcrop']       <- 'Regional oilcrop'
    value[value=='regional rice']          <- 'Regional rice'
    value[value=='regional sugarcrop']     <- 'Regional sugarcrop'
    value[value=='regional wheat']         <- 'Regional wheat'
    
    value[value=='a oil']                  <- 'Oil'
    value[value=='b natural gas']          <- 'Natural gas'
    value[value=='c coal']                 <- 'Coal'
    value[value=='d biomass']              <- 'Biomass'
    value[value=='e nuclear']              <- 'Nuclear'
    value[value=='f hydro']                <- 'Hydro'
    value[value=='g wind']                 <- 'Wind'
    value[value=='h solar']                <- 'Solar'
    value[value=='i geothermal']           <- 'Geothermal'
    value[value=='j traditional biomass']  <- 'Traditional biomass'
    
    value[value=='biomass']                <- 'Biomass'
    value[value=='coal']                   <- 'Coal'
    value[value=='gas']                    <- 'Gas'
    value[value=='geothermal']             <- 'Geothermal'
    value[value=='hydro']                  <- 'Hydro'
    value[value=='nuclear']                <- 'Nuclear'
    value[value=='refined liquids']        <- 'Refined liquids'
    value[value=='rooftop_pv']             <- 'Rooftop PV'
    value[value=='solar']                  <- 'Solar'
    value[value=='wind']                   <- 'Wind'
    
    value[value=='biomass liquids']        <- 'Biomass Liquids' 
    value[value=='coal to liquids']        <- 'Coal-to-Liquids'  
    value[value=='gas to liquids']         <- 'Gas-to-Liquids'  
    value[value=='oil refining']           <- 'Oil Refining'
    
    value[value=='biomassGrass']           <- 'Biomass grass' 
    value[value=='biomassTree']            <- 'Biomass tree'
    value[value=='crops']                  <- 'Crops' 
    value[value=='forest (managed)']       <- 'Forest (managed)'
    value[value=='forest (unmanaged)']     <- 'Forest (unmanaged)'    
    value[value=='grass']                  <- 'Grass'
    value[value=='otherarable']            <- 'Other arable'
    value[value=='pasture (grazed)']       <- 'Pasture (grazed)'
    value[value=='pasture (other)']        <- 'Pasture (other)'
    value[value=='rock and desert']        <- 'Rock and desert'
    value[value=='RootTuber']              <- 'Root tuber'
    value[value=='shrubs']                 <- 'Shrubs'
    value[value=='tundra']                 <- 'Tundra'
    value[value=='urban']                  <- 'Urban'
    
    value[value=='FiberCrop']              <- 'Fiber crop'
    value[value=='FodderGrass']            <- 'Fodder grass'
    value[value=='FodderHerb']             <- 'Fodder herb'
    value[value=='MiscCrop']               <- 'Misc crop'
    value[value=='OilCrop']                <- 'Oil crop'
    value[value=='OtherGrain']             <- 'Other grain'
    value[value=='Palm fruit']             <- 'Palm Fruit'
    value[value=='RootTuber']              <- 'Root tuber'
    value[value=='SugarCrop']              <- 'Sugar crop'
    
    value[value=='Africa_Eastern']         <- 'Africa (eastern)'
    value[value=='Africa_Western']         <- 'Africa (western)'
    value[value=='Africa_Northern']        <- 'Africa (northern)'
    value[value=='Africa_Southern']        <- 'Africa (southern)'
    value[value=='Australia_NZ']           <- 'Australia & New Zealand'
    value[value=='Europe_Eastern']         <- 'Europe (eastern)'
    value[value=='Europe_Non_EU']          <- 'Europe (Non EU)'
    value[value=='South America_Northern'] <- 'South America (northern)'
    value[value=='South America_Southern'] <- 'South America (southern)'
    
  }
  return(value)
}
#________________________________________________________
