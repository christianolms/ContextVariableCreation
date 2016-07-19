require(foreach)
require(data.table)
#' Adding (panel) context variables for all given time_dep_variables, to consider panel effects for objects determined by group_variable.
#' For each name (characters) in time_dep_variables, the context variable according to group_variable is attached to the result data.table.
#' The result data.table either contains every column of df.
#' A context variable for a column in df (for example income) contains the object specific mean of all repeated measurements (of income). The objects determined by the  group_variable (e.g. employeeID determines a single employee).
#' If time_dep_variables is not set (its length is equals 0), time_dep_variables is computed by taking all columns of df and excluding depend_variable and time_indep_variables
#' @param df a data.frame
#' @param group_variable the variable that determines the objects (or people) under consideration. This will something like personID, customerID. Type: character
#' Further, either time_dep_variables or (depend_variable AND time_indep_variables) MUST be provided.
#' @param time_dep_variables a character vector containing the (column) names for all time dependent columns in df.
#' @param depend_variable the name of the dependent (in terms of linear regression) variable. If time_dep_variables is set, this parameter is ignored.
#' @param time_indep_variables Variables that are constant (at least approx.) over all repeated measurements according to the objects determined by group_variable. If time_dep_variables is set, this parameter is ignored.
#' @return data.table containing all columns of df and one context variable for every column in time_dep_variables (see the parameter description how the content of time_dep_variables is determined).
#' @export
create_context_variables_for_panel = function(df,
                                              group_variable,
                                              depend_variable=c(),
                                              time_indep_variables=c(),
                                              time_dep_variables=c()){

  if(length(time_dep_variables) == 0){
    print("Calculate time_dep_vars from time_indep_variables and depend_variable ")
    time_dep_variables = setdiff(setdiff(setdiff(names(df), group_variable), time_indep_variables), depend_variable)
  }
  else{
    time_dep_variables = setdiff(time_dep_variables, group_variable)#ensure group variable is not in time_dep_variables
  }
  dt = data.table(df)
  #compute additional context variables
  df_cv = foreach(colname = time_dep_variables, .combine=function(df1,df2){merge(df1,df2,by=group_variable)}) %do% {
    new_colname = paste(colname,"_cv",sep = "")
    x = dt[,.(cv=mean(get(colname))),get(group_variable)]
    names(x) = c(group_variable,new_colname)
    x
  }
  #attach and return extended data frame
  df_extended = merge(dt,df_cv,by=group_variable)
  df_extended
}
