#--------------------------------------------------------------------
# ALGEBRAIC OPERATIONS WITH SCENARIOS
#--------------------------------------------------------------------
#====================================================================
# Performs algebraic operations among scenarios (sum, substract, multiply, divide)
#     for a given variable.
#     The function arguments are the input dataframe, equation (algebraic 
#     expression), name of the target and the new variables, and the 
#     units of the new variable.
#====================================================================

#' @title A function to perform algebraic operations among scenarios for a given variable.
#' @description Algebraic operations are possible based on data in IAMC format.
#' @param D A dataframe of IAMC data.
#' @param var_name The target variable to perform the algebraic operation.
#' @param formula_string The algebraic operation as an equation .
#' @param var_new_name The name of the new variable calculated by the algebraic expression.
#' @param unit The units of the new variable calculated by the algebraic expression.
#' @return A dataframe with the values of the new variable.
#' @example IAMC_DB_Plot_Bar(AR5_Sample_data, AR5_Rule_table)
#' @export p_list1


Algebra_scen <- function(D,var_name,formula_string,var_new_name="",unit=""){
  
  # function (D,formula_string, unit=""){
  #var_name <- "Consumption"
  #var_new_name <- "Consumption Loss"
  #formula_string <- "NDC&2050-80% = NDC&2050-80% - Baseline "
  
  operator <- stringr::str_trim(stringr::str_extract(formula_string,"( [\\+\\-\\*/] )")) # get algebraic operator (+, -, *, /)
  temp <- stringr::str_split(formula_string ," [=\\+\\-\\*/] ")[[1]] # split equation into elements.
  s_new <- stringr::str_trim(temp[1])  # Left-hand side of equation.
  s1 <- stringr::str_trim(temp[2])  # First element of right-hand side of equation.
  s2 <- stringr::str_trim(temp[3])
  # print(operator); print(v_new); print(v1); print(v2)
  i1 <- (D$scenario== s1 & D$variable == var_name)
  i2 <- (D$scenario== s2 & D$variable == var_name)
  
  D2 <- merge(D[i1,],D[i2,],by=c("model","variable","region","period"))
  
  D2$value = eval(parse(text=sprintf("D2$value.x %s D2$value.y",operator)))
  D2$scenario <- s_new
  
  if (operator=="+" | operator=="-" ) { D2$unit <- D$unit[i1][1] }
  else { D2$unit <- ""}
  if(var_new_name!=""){
    D2$variable <- var_new_name
  }
  
  D2[,c("model","scenario","region","variable","unit","period","value")]

  }

#END
