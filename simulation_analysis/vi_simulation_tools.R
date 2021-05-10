library(tidyverse)

double_sigmoid = function(doy, V_min, V_amp, m1, m2, m3, m4){
  first_sigmoid  = 1 / (1 + exp(m1 + (m2*doy)))
  second_sigmoid = 1 / (1 + exp(m3 + (m4*doy)))
  return(V_min + V_amp(first_sigmoid - second_sigmoid))
}

elmore_double_sigmoid = function(doy, m1, m2, m3, m4, m5, m6, m7){
  # Eq. 4 in Elmore et al. 2012, https://doi.org/10.1111/j.1365-2486.2011.02521.x
  first_sigmoid  = 1 / (1 + exp((m3-doy)/m4))
  second_sigmoid = 1 / (1 + exp((m5-doy)/m6))
  return(m1 + (m2 - (m7*doy))*(first_sigmoid - second_sigmoid))
}


############################
############################
# Function for calculating transition dates for a single year via
# a loess smoothing curve
extract_phenology = function(df, 
                             percent_threshold = c(0.1,0.25,0.5),
                             amplitude_threshold = 0.1,
                             to_return = 'df'){
  # print(head(df,1))
  qa = 0
  df = arrange(df, doy)
  full_year = data.frame(doy = -90:455)
  #smoothed_points = predict(loess(vi ~ doy, span=loess_span, data=df), newdata = full_year)
  smoothed_points = predict(smooth.spline(df$doy, df$vi), x = full_year$doy)$y
  smoothed_points = smoothed_points[!is.na(smoothed_points)]
  
  meets_amplitude_threshold = max(smoothed_points) - min(smoothed_points) >= amplitude_threshold
  if(!meets_amplitude_threshold){
    qa = qa + 1
  } 
  
  scaled_vi = (smoothed_points - min(smoothed_points)) / (max(smoothed_points) - min(smoothed_points))
  peak = full_year$doy[which.max(scaled_vi)]
  
  phenology_df = data.frame()
  for(threshold in percent_threshold){
    # onset is the day where the VI curve crosses the threshold the final time before increasing to the peak
    spring_low = min(scaled_vi[full_year$doy<peak])
    sos = max(full_year$doy[full_year$doy < peak & scaled_vi <= (threshold+spring_low)])
    # end is the day where the VI curve crosses the threshold the first time while decreasing from the peak.
    fall_low = min(scaled_vi[full_year$doy>peak])
    eos = min(full_year$doy[full_year$doy > peak & scaled_vi <= (threshold+fall_low)])
    
    season_length = eos - sos
    
    phenology_df = rbind(phenology_df,
                      data.frame(peak = peak, sos = sos, 
                                 eos = eos, season_length = season_length,
                                 threshold = threshold,
                                 method = 'percent_max_threshold',
                                 qa = qa))
    
  }
  
  # Using the maximum rate of change method
  full_year$first_derivative = c(0,diff(scaled_vi))
  sos = full_year %>%
    filter(doy<peak) %>%
    filter(first_derivative==max(first_derivative)) %>%
    pull(doy)
  eos = full_year %>%
    filter(doy>peak) %>%
    filter(first_derivative==min(first_derivative)) %>%
    pull(doy)
  if(length(sos)!=1) stop('max change rate sos failed')
  if(length(eos)!=1) stop('max change rate eos failed')
  #sos = full_year$doy[which.max(first_derivative)]
  #eos = full_year$doy[which.min(first_derivative)]
  season_length = eos - sos
  
  phenology_df = rbind(phenology_df,
                       data.frame(peak = peak, sos = sos, 
                                  eos = eos, season_length = season_length,
                                  threshold = NA,
                                  method = 'max_change_rate',
                                  qa = qa))
  
  if(to_return == 'plot'){
    # return a diagnostic plot with all original points, the smooth line, and resulting phenology
  
    x = full_year %>%
      left_join(df, by='doy')
    x$smoothed_points = smoothed_points
    
    phenology_df_long = phenology_df %>%
      pivot_longer(c(peak, sos, eos), names_to = 'metric', values_to='doy')
    
    p = ggplot(x, aes(x=doy)) + 
      geom_point(aes(y=vi)) + 
      geom_line(aes(y=smoothed_points)) +
      geom_hline(yintercept = min(smoothed_points) + 0.1, linetype='dotted') +
      geom_vline(data = phenology_df_long, aes(xintercept=doy, color=interaction(method)), size=1) +
      scale_color_brewer(palette = 'Dark2')
    
    return(p)
  } else if(to_return == 'df'){
  #print(unique(df$pixel_id))
    return(phenology_df)
  }
  
}
