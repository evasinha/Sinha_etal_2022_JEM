# Author - Eva Sinha, Pacific Northwest National Lab, eva.sinha@pnnl.gov

library(rgcam)
library(dplyr)

# Function details
# create_proj_file - Create project data set by reading local GCAM database

# http://127.0.0.1:26047/library/rgcam/doc/rgcam-usecases.html
# http://127.0.0.1:26047/library/rgcam/doc/rgcam-usecases.Rmd
#________________________________________________________
# Create project data set by reading local GCAM database
create_proj_file <- function(file.path, dbFile, prj.name, scenario.names, query.fname){
  
  for (ind in 1:length(dbFile)){
     
     # Create a connection that can be used to run queries on a local GCAM database
     conn <- localDBConn(dbPath=file.path, dbFile[ind])
     print("Created connection to local GCAM database") 
  
     # Add a scenario from a GCAM output database to a project data set
     prj <- addScenario(conn, prj.name, scenario=scenario.names, clobber=TRUE, queryFile=query.fname)
     print("Added a scenario from a GCAM output database") 
  }
  print(prj)
  
  # List the scenarios in the project data set
  scenarios <- listScenarios(prj)
  print(scenarios)
  
  # List the queries available for a scenario
  queries <- listQueries(prj, 'Reference_SSP2-4p5')
  print(queries)
  
  return(prj)
}

#________________________________________________________
file.path      <- '../../gcam-core/output/'
dbFile         <- c('database_basexdb_reference_SSP2-4p5','database_basexdb_fert_global_const_15_SSP2-4p5','database_basexdb_carbon_tax', 'database_basexdb_carbon_tax_fert_global_const_15')

prj.name       <- 'fert_const_updated_IO_coeff.dat'
scenario.names <- c('Reference_SSP2-4p5','fert_global_const_15_SSP2-4p5','carbon_tax_25_5','carbon_tax_25_5_fert_global_const_15')
query.fname    <- 'queries_select.xml'

# Create project data set by reading local GCAM database
prj <- create_proj_file(file.path, dbFile, prj.name, scenario.names, query.fname)
