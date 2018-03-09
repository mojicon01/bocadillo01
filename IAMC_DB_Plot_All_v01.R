#--------------------------------------------------------------------
# PLOTTING FUNCTION: PLOT ALL
#--------------------------------------------------------------------
#====================================================================
# Plot all types
#====================================================================

IAMC_DB_Plot_All <- function(D,R,region=levels(D$region), scenario=levels(D$scenario),
                             target_year=levels(D$period),variable=levels(D$variable), PRINT_OUT=F, DEBUG=T){

  p_list2 <- list()

  p_Out <- IAMC_DB_Plot_Area(D,R,region,scenario)
  p_list2 <- c(p_list2,p_Out)

  p_Out <- IAMC_DB_Plot_Bar(D,R,region,target_year)
  p_list2 <- c(p_list2,p_Out)

  p_Out <- IAMC_DB_Plot_Line(D,region,variable,scenario)
  p_list2 <- c(p_list2,p_Out)

  p_Out <- IAMC_DB_Plot_Box(D,region,variable,target_year)
  p_list2 <- c(p_list2,p_Out)

  p_Out <- IAMC_DB_Plot_Point(D,region,variable,target_year)
  p_list2 <- c(p_list2,p_Out)


  return(p_list2)
}


# END
