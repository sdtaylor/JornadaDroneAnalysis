library(tidyverse)

# This implements the modis phenology V6 algorithm

# return a data.frame of potential peaks indicating their validity
# and resulting transition dates. 
# requires a data.frame with columns c('doy','ndvi')
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
  spline = smooth.spline(full_vi_series$doy, full_vi_series$ndvi)
  
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
  
  meets_amplitude_threshold1 = function(peak_ndvi, lowpoint_ndvi){peak_ndvi - lowpoint_ndvi >= initial_amplitude_threshold}
  
  min_vi = min(full_vi_series$smoothed)
  max_vi = max(full_vi_series$smoothed)
  # whether the peak is 35% of more of the total timeseries amplitude
  meets_amplitude_threshold2 = function(peak_ndvi, lowpoint_ndvi){peak_ndvi - lowpoint_ndvi >= (max_vi-min_vi)*timseries_amplitude_threshold_percent}
  
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
    
    candiate_peak_ndvi = full_vi_series %>%
      filter(doy == candidate_peak_doy) %>%
      pull(smoothed)
    
    search_window_low_ndvi = full_vi_series %>%
      filter(doy %in% search_window_start:search_window_end) %>%
      filter(smoothed == min(smoothed)) 
    
    if(!meets_amplitude_threshold1(candiate_peak_ndvi, search_window_low_ndvi$smoothed)){
      print('failed amplitude check 1 (>= 0.1)')
    } else if(!meets_amplitude_threshold2(candiate_peak_ndvi, search_window_low_ndvi$smoothed)){
      print('failed amplitude check 2 (>= 35% of ts amplitude)')
    } else {
      peaks$peak_valid[peak_i] = TRUE
    }
    
    peaks$peak_rise_doy = search_window_low_ndvi$doy
    peaks$peak_rise_vi  = search_window_low_ndvi$smoothed
    
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
    
    candiate_peak_ndvi = full_vi_series %>%
      filter(doy == candidate_peak_doy) %>%
      pull(smoothed)
    
    search_window_low_ndvi = full_vi_series %>%
      filter(doy %in% search_window_start:search_window_end) %>%
      filter(smoothed == min(smoothed)) 
    
    if(!meets_amplitude_threshold1(candiate_peak_ndvi, search_window_low_ndvi$smoothed)){
      print('failed amplitude check 1 (>= 0.1)')
    } else if(!meets_amplitude_threshold2(candiate_peak_ndvi, search_window_low_ndvi$smoothed)){
      print('failed amplitude check 2 (>= 35% of ts amplitude)')
    } else {
      peaks$peak_valid[peak_i] = TRUE
    }
    
  
    peaks$peak_fall_doy = search_window_low_ndvi$doy
    peaks$peak_fall_vi  = search_window_low_ndvi$smoothed
    
  }
  
  return(peaks)
}

peaks = find_peaks(full_ndvi)

ggplot(full_ndvi, aes(x=doy)) +
  geom_point(aes(y=ndvi)) +
  geom_vline(data=peaks, aes(xintercept=doy,color=peak_valid))


