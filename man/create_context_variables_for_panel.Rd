% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_context_variables.r
\name{create_context_variables_for_panel}
\alias{create_context_variables_for_panel}
\title{Adding (panel) context variables for all given time_dep_variables, to consider panel effects for objects determined by group_variable.
For each name (characters) in time_dep_variables, the context variable according to group_variable is attached to the result data.table.
The result data.table either contains every column of df.
A context variable for a column in df (for example income) contains the object specific mean of all repeated measurements (of income). The objects determined by the  group_variable (e.g. employeeID determines a single employee).
If time_dep_variables is not set (its length is equals 0), time_dep_variables is computed by taking all columns of df and excluding depend_variable and time_indep_variables}
\usage{
create_context_variables_for_panel(df, group_variable, depend_variable = c(),
  time_indep_variables = c(), time_dep_variables = c())
}
\arguments{
\item{df}{a data.frame}

\item{group_variable}{the variable that determines the objects (or people) under consideration. This will something like personID, customerID. Type: character
Further, either time_dep_variables or (depend_variable AND time_indep_variables) MUST be provided.}

\item{depend_variable}{the name of the dependent (in terms of linear regression) variable. If time_dep_variables is set, this parameter is ignored.}

\item{time_indep_variables}{Variables that are constant (at least approx.) over all repeated measurements according to the objects determined by group_variable. If time_dep_variables is set, this parameter is ignored.}

\item{time_dep_variables}{a character vector containing the (column) names for all time dependent columns in df.}
}
\value{
data.table containing all columns of df and one context variable for every column in time_dep_variables (see the parameter description how the content of time_dep_variables is determined).
}
\description{
Adding (panel) context variables for all given time_dep_variables, to consider panel effects for objects determined by group_variable.
For each name (characters) in time_dep_variables, the context variable according to group_variable is attached to the result data.table.
The result data.table either contains every column of df.
A context variable for a column in df (for example income) contains the object specific mean of all repeated measurements (of income). The objects determined by the  group_variable (e.g. employeeID determines a single employee).
If time_dep_variables is not set (its length is equals 0), time_dep_variables is computed by taking all columns of df and excluding depend_variable and time_indep_variables
}

