create_plot_residuals_target = function(df){
  return(ggplot(aes(x=target, y=residuals), data=df)+geom_point())
}

make_plot_residual_frame = function(residuals, target){
  return(data.frame(cbind(target = target, residuals = residuals)))

}

plot_residuals = function(residuals, target){
df = make_plot_residual_frame(residuals = residuals,  target = target)
return(create_plot_residuals_target(df))
}
