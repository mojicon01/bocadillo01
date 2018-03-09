#--------------------------------------------------------------------
# ALGEBRAIC OPERATIONS WITH VARIABLES
#--------------------------------------------------------------------
#====================================================================
# Performs algebraic operations with two variables (sum, substract, multiply, divide)
#     for a given scenario.
#     The function arguments are the input dataframe, equation (algebraic 
#     expression), name of the target and the new variables, and the 
#     units of the new variable.
#====================================================================

#' @title A function to perform algebraic operations among scenarios for a given variable.
#' @description Algebraic operations are possible based on data in IAMC format.
#' @param D A dataframe of IAMC data.
#' @param formula_string The algebraic operation as an equation .
#' @param unit The units of the new variable calculated by the algebraic expression.
#' @return A dataframe with the values of the new variable.
#' @example IAMC_DB_Plot_Bar(AR5_Sample_data, AR5_Rule_table)
#' @export p_list1


Algebra_var <- function (D,formula_string, unit=""){
  
  # str_extract, str_split, str_trim from https://heavywatal.github.io/rstats/stringr.html
  operator <- stringr::str_extract(formula_string,"([\\+\\-\\*/])")  # get +, -, *, /
  temp <- stringr::str_split(formula_string ,"[=\\+\\-\\*/]")[[1]] # split 
  v_new <- stringr::str_trim(temp[1]) 
  v1 <- stringr::str_trim(temp[2]) 
  v2 <- stringr::str_trim(temp[3]) 
  
  #  print(operator);   print(v_new);    print(v1);     print(v2)
  
  i1 <- D$variable== v1
  i2 <- D$variable== v2
  
  D2 <- merge(D[i1,],D[i2,],by=c("model","scenario","region","period"))
  
  D2$value = eval(parse(text=sprintf("D2$value.x %s D2$value.y",operator)))
  
  D2$variable <- v_new
  
  if (operator=="+" | operator=="-" ) { D2$unit <- D$unit[i1][1] }
  else { D2$unit <- ""}
  
  D2[,c("model","scenario","region","variable","unit","period","value")]
  
}
