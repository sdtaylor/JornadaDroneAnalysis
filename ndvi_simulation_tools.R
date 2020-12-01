library(tidyverse)

# Return NDVI/EVI values based on 4 points of a step function.
# Optionally with normally distributed noise.
calculate_vi_curve = function(winter_vi,  # off season ndvi
                                peak_vi,    # ndvi value at peak
                                fall_vi,    # ndvi value at the onset of fall, 2nd highest value
                                # doy values
                                spring_onset,   # doy ndvi starts to rise from winter
                                peak,           # doy of peak
                                fall_onset,     # doy when ndvi drops toward winter values
                                winter_onset,   # doy ndvi reaches winter values
                                #
                                doy          = 1:365,
                                noise_sd     = 0.01  # st. dev for noise around straight lines
){
  vi = dplyr::case_when(
    doy <= spring_onset                    ~ winter_vi,
    doy > spring_onset & doy <= peak       ~ doy * (peak_vi-winter_vi)/(peak - spring_onset)   +  (peak_vi - peak * (peak_vi-winter_vi)/(peak - spring_onset)),
    doy > peak & doy <= fall_onset         ~ doy * (fall_vi-peak_vi)/(fall_onset - peak) +        (fall_vi - fall_onset * (fall_vi-peak_vi)/(fall_onset - peak)),
    doy > fall_onset & doy <= winter_onset ~ doy * (winter_vi-fall_vi)/(winter_onset-fall_onset) +  (winter_vi - winter_onset *  (winter_vi-fall_vi)/(winter_onset-fall_onset)),
    doy > winter_onset                     ~ winter_vi
  )
  
  return(vi + rnorm(length(vi),mean=0, sd=noise_sd))
}

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
  full_year = data.frame(doy = -60:450)
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
    # onset is the *first* day where the percent of the max is > threshold, and was before the date of peak
    sos = min(full_year$doy[full_year$doy < peak & scaled_vi > threshold])
    # end is the *last* day where the percent of the max is > threshold, and was after the date of peak
    eos = max(full_year$doy[full_year$doy > peak & scaled_vi > threshold])
    
    season_length = eos - sos
    
    phenology_df = rbind(phenology_df,
                      data.frame(peak = peak, sos = sos, 
                                 eos = eos, season_length = season_length,
                                 threshold = threshold,
                                 qa = qa))
    
  }
  
  if(to_return == 'plot'){
    # return a diagnostic plot with all original points, the smooth line, and resulting phenology
  
    x = full_year %>%
      left_join(df, by='doy')
    x$smoothed_points = smoothed_points
    
    phenology_df_long = phenology_df %>%
      pivot_longer(c(peak, sos, eos, season_length), names_to = 'metric', values_to='doy')
    
    p = ggplot(x, aes(x=doy)) + 
      geom_point(aes(y=vi)) + 
      geom_line(aes(y=smoothed_points)) +
      geom_hline(yintercept = max(smoothed_points) - min(smoothed_points), linetype='dotted') +
      geom_vline(data = phenology_df_long, aes(xintercept=doy, color=metric))
    
    return(p)
  } else if(to_return == 'df'){
  #print(unique(df$pixel_id))
    return(phenology_df)
  }
  
}

# a 365 pure shrub ndvi/evi curve
random_shrub_vi = function(){
  calculate_vi_curve(winter_vi  = 0.1,  # off season evi
                     peak_vi    = 0.3,  # evi value at peak
                     fall_vi    = 0.21, # evi value at the onset of fall, 2nd highest value
                     # doy values
                     spring_onset = 104,   # apr 15
                     peak         = 152,  # june 1
                     fall_onset   = 274,  # oct 1
                     winter_onset = 335,  # dec 1
                     #
                     doy          = 1:365,
                     noise_sd     = 0.02)  # from drone imagery using ndvi/2. more/less consistant throughout season
}

# a 365 pure grass EVI curve
random_grass_vi = function(){
  calculate_vi_curve(winter_vi  = 0.1,  # off season evi
                     peak_vi    = 0.4,  # evi value at peak 
                     fall_vi    = 0.2,  # evi value at the onset of fall, 2nd highest value
                     # doy values
                     spring_onset = 196,   # july 15, note grass onset is highly variable
                     peak         = 244,   # Aug 1,
                     fall_onset   = 274,  # oct 1
                     winter_onset = 305,  # nov 1
                     #
                     doy          = 1:365,
                     noise_sd     = 0.015)  # sd based on modis winter/spring variation around 0.1 EVI
}

# a 365 pure soil ndvi curve
# soil has no season so it's just a constant ndvi + noise
# values from from drone imagery using ndvi/2. more/less consistant throughout season
random_soil_vi = function(base_vi = 0.05, noise_sd=0.01){
  # soil is just constant ndvi with some noise
  vi = rep(base_vi, 365)
  return(vi + rnorm(length(vi),mean=0, sd=noise_sd))
}


# return a data.frame of potential peaks indicating their validity
# and resulting transition dates. 
# requires a data.frame with columns c('doy','vi')
find_peaks = function(full_vi_series){
  
  #-----------------
  # overall parameters
  #-----------------
  search_window_initial_offset = 30
  search_window_max_offset     = 185
  
  initial_amplitude_threshold   = 0.1
  timseries_amplitude_threshold_percent = 0.35
  
  start_of_ts = min(full_vi_series$doy)
  end_of_ts   = max(full_vi_series$doy)
  
  #-----------------
  # the smoothing spline
  #-----------------
  spline = smooth.spline(full_vi_series$doy, full_vi_series$vi)
  
  full_vi_series$smoothed = predict(spline)$y
  full_vi_series$first_deriv = c(NA,diff(full_vi_series$smoothed))
  full_vi_series$first_deriv[1] = full_vi_series$first_deriv[2]
  
  # initial peak finder of when the 1st derivated from from positive to negative
  full_vi_series$is_peak = F
  for(i in 1:(length(full_vi_series$first_deriv)-1)){
    if(full_vi_series$first_deriv[i] > 0 & full_vi_series$first_deriv[i+1] < 0){
      full_vi_series$is_peak[i] = T
    }
  }
  
  peaks = full_vi_series %>%
    filter(is_peak) %>%
    arrange(doy) %>%
    mutate(peak_id = row_number())
  
  # peaks are evaluated from lowest to highest
  peak_eval_order = peaks %>%
    arrange(smoothed) %>%
    pull(peak_id)
  
  meets_amplitude_threshold1 = function(peak_vi, lowpoint_vi){peak_vi - lowpoint_vi >= initial_amplitude_threshold}
  
  min_vi = min(full_vi_series$smoothed)
  max_vi = max(full_vi_series$smoothed)
  # whether the peak is 35% of more of the total timeseries amplitude
  meets_amplitude_threshold2 = function(peak_vi, lowpoint_vi){peak_vi - lowpoint_vi >= (max_vi-min_vi)*timseries_amplitude_threshold_percent}
  
  peaks$peak_status   = 0  # 0: unevaluted, 1: valid peak, 2: invalid peak
  peaks$peak_valid    = FALSE
  peaks$peak_valid_reason = NA
  peaks$peak_rise_doy = NA
  peaks$peak_fall_doy = NA
  peaks$peak_rise_vi  = NA
  peaks$peak_fall_vi  = NA
  
  
  # 0 - valid
  # 1 - onset search window mismatch
  # 2 - 
  
  # validate each of the initial peaks
  for(peak_i in peak_eval_order){
    candidate_peak_doy = peaks$doy[peak_i]
    peak_valid = TRUE
    
    #-----------------
    # evaluating the preceding low point
    #-----------------
    if(peak_i>1){
      prior_peak_doy = peaks %>%              # the day of the most prior peak, unless it's been marked invalid already.
        filter(peak_status %in% c(0,1), 
               doy < candidate_peak_doy) %>%
        pull(doy) %>%
        max()
    } else {
      prior_peak_doy = NA
    }
    # don't let negative numbers happen
    max_greenup_period = max(candidate_peak_doy -  search_window_max_offset, start_of_ts)
    
    search_window_start = max(prior_peak_doy, start_of_ts, max_greenup_period, na.rm=T)
    search_window_end   = max(candidate_peak_doy - search_window_initial_offset, start_of_ts)
    
    #
    if(search_window_end < search_window_start){
      search_window_end = search_window_start
      print(paste0('search window mismatch: ',peak_i))
      peak_valid = FALSE
    }
    
    candiate_peak_vi = full_vi_series %>%
      filter(doy == candidate_peak_doy) %>%
      pull(smoothed)
    
    search_window_low_vi = full_vi_series %>%
      filter(doy %in% search_window_start:search_window_end) %>%
      filter(smoothed == min(smoothed)) 
    
    if(!meets_amplitude_threshold1(candiate_peak_vi, search_window_low_vi$smoothed)){
      print('failed amplitude check 1 (>= 0.1)')
      peak_valid = FALSE
    } else if(!meets_amplitude_threshold2(candiate_peak_vi, search_window_low_vi$smoothed)){
      print('failed amplitude check 2 (>= 35% of ts amplitude)')
      peak_valid = FALSE
    } 
    
    peaks$peak_rise_doy[peak_i] = search_window_low_vi$doy
    peaks$peak_rise_vi[peak_i]  = search_window_low_vi$smoothed
    
    # evaluating the subsequent low point
    if(peak_i < nrow(peaks)){
      next_peak_doy = peaks %>%              # the day of the next peak, unless it's been marked invalid already.
        filter(peak_status %in% c(0,1), 
               doy > candidate_peak_doy) %>%
        pull(doy) %>%
        min()
      
    } else {
      next_peak_doy = NA
    }
    
    # dont let numbers beyond the timeseries end happend
    max_greenup_period = min(candidate_peak_doy + search_window_max_offset, end_of_ts)
    
    search_window_end = min(next_peak_doy, end_of_ts, max_greenup_period, na.rm=T)
    search_window_start   = min(candidate_peak_doy + search_window_initial_offset, end_of_ts)
    
    if(search_window_end < search_window_start){
      search_window_end = search_window_start + 1
      print('search window mismatch')
      peak_valid = FALSE
    }
    
    candiate_peak_vi = full_vi_series %>%
      filter(doy == candidate_peak_doy) %>%
      pull(smoothed)
    
    search_window_low_vi = full_vi_series %>%
      filter(doy %in% search_window_start:search_window_end) %>%
      filter(smoothed == min(smoothed, na.rm=T)) 
    
    if(!meets_amplitude_threshold1(candiate_peak_vi, search_window_low_vi$smoothed)){
      print('failed amplitude check 1 (>= 0.1)')
      peak_valid = FALSE
    } else if(!meets_amplitude_threshold2(candiate_peak_vi, search_window_low_vi$smoothed)){
      print('failed amplitude check 2 (>= 35% of ts amplitude)')
      peak_valid = FALSE
    } 
    
    peaks$peak_fall_doy[peak_i] = search_window_low_vi$doy
    peaks$peak_fall_vi[peak_i]  = search_window_low_vi$smoothed

    peaks$peak_status[peak_i] = ifelse(peak_valid,1,2)
    peaks$peak_valid[peak_i]  = peak_valid
    
  }
  
  return(peaks)
}
