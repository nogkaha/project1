#defining a fun to match values of vars from a lookup table.
#the defult lookup table is "factor_labels"

#arguments:
#df = which data frame to change
#var_to get= which var in this DF to change to the textual values.

get_labels<-function(df,var_to_get,look_in=factor_labels) {
  x<-subset(look_in,new_var_name==var_to_get,select=-new_var_name)
  var_out<-x[match(df[[var_to_get]],x$q_level),2] 
  }


