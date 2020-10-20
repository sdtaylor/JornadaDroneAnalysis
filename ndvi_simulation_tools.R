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
  
  peaks$peak_valid = FALSE
  peaks$peak_rise_doy = NA
  peaks$peak_fall_doy = NA
  peaks$peak_rise_vi  = NA
  peaks$peak_fall_vi  = NA
  
  
  # validate each of the initial peaks
  for(peak_i in peak_eval_order){
    candidate_peak_doy = peaks$doy[peak_i]
    
    #-----------------
    # evaluating the preceding low point
    #-----------------
    if(peak_i>1){
      prior_peak_doy = peaks$doy[peak_i-1]
    } else {
      prior_peak_doy = NA
    }
    start_of_ts = 1
    max_greenup_period = candidate_peak_doy -  search_window_max_offset
    
    search_window_start = max(prior_peak_doy, start_of_ts, max_greenup_period, na.rm=T)
    search_window_end   = candidate_peak_doy - search_window_initial_offset
    
    if(search_window_end < search_window_start){
      print('search window mismatch')
      next
    }
    
    candiate_peak_vi = full_vi_series %>%
      filter(doy == candidate_peak_doy) %>%
      pull(smoothed)
    
    search_window_low_vi = full_vi_series %>%
      filter(doy %in% search_window_start:search_window_end) %>%
      filter(smoothed == min(smoothed)) 
    
    if(!meets_amplitude_threshold1(candiate_peak_vi, search_window_low_vi$smoothed)){
      print('failed amplitude check 1 (>= 0.1)')
    } else if(!meets_amplitude_threshold2(candiate_peak_vi, search_window_low_vi$smoothed)){
      print('failed amplitude check 2 (>= 35% of ts amplitude)')
    } else {
      peaks$peak_valid[peak_i] = TRUE
    }
    
    peaks$peak_rise_doy = search_window_low_vi$doy
    peaks$peak_rise_vi  = search_window_low_vi$smoothed
    
    # evaluating the subsequent low point
    if(peak_i < length(peaks)){
      next_peak_doy = peaks$doy[peak_i+1]
    } else {
      next_peak_doy = NA
    }
    
    max_greenup_period = candidate_peak_doy + search_window_max_offset
    
    search_window_end = min(next_peak_doy, end_of_ts, max_greenup_period, na.rm=T)
    search_window_start   = candidate_peak_doy + search_window_initial_offset
    
    if(search_window_end < search_window_start){
      print('search window mismatch')
      next
    }
    
    candiate_peak_vi = full_vi_series %>%
      filter(doy == candidate_peak_doy) %>%
      pull(smoothed)
    
    search_window_low_vi = full_vi_series %>%
      filter(doy %in% search_window_start:search_window_end) %>%
      filter(smoothed == min(smoothed)) 
    
    if(!meets_amplitude_threshold1(candiate_peak_vi, search_window_low_vi$smoothed)){
      print('failed amplitude check 1 (>= 0.1)')
    } else if(!meets_amplitude_threshold2(candiate_peak_vi, search_window_low_vi$smoothed)){
      print('failed amplitude check 2 (>= 35% of ts amplitude)')
    } else {
      peaks$peak_valid[peak_i] = TRUE
    }
    
    
    peaks$peak_fall_doy = search_window_low_vi$doy
    peaks$peak_fall_vi  = search_window_low_vi$smoothed
    
  }
  
  return(peaks)
}
