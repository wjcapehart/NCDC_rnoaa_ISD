storage_directory = "H:/Climate R Directory"
storage_directory = ""

# setwd (storage_directory)
library("rnoaa")
library("isdparser")
library("lubridate")
library("ncdf4")
library("dplyr")
library("openair")

stations_near_targ = isd_stations_search(lat    =  -20.5759777777778,  # degrees_north
                                         lon    = 16.8898888888889,  # degrees_east
                                         radius =   100.)  # km

#stations_near_targ = isd_stations_search(lat    = 44,  # degrees_north
#                                         lon    = -103,  # degrees_east
#                                         radius =   300.)  # km

print(stations_near_targ)

station_list_number = 1 # indicing starts at 1   # add manually
station_list_number_label = 1 # indicing starts at 1   # add manually



YYYYstart = substr(x     = toString(stations_near_targ$begin[station_list_number]), 
                   start = 1, 
                   stop  = 4)

MMstart   = substr(x     = toString(stations_near_targ$begin[station_list_number]), 
                   start = 5, 
                   stop  = 6)

DDstart   = substr(x     = toString(stations_near_targ$begin[station_list_number]), 
                   start = 7, 
                   stop  = 8)


YYYYend   = substr(x     = toString(stations_near_targ$end[station_list_number]), 
                   start = 1, 
                   stop  = 4)

MMend     = substr(x     = toString(stations_near_targ$end[station_list_number]), 
                   start = 5, 
                   stop  = 6)

DDend     = substr(x     = toString(stations_near_targ$end[station_list_number]), 
                   start = 7, 
                   stop  = 8)






start_date_for_full_record = as.POSIXct(paste(YYYYstart,
                                              "-",
                                              MMstart,
                                              "-",
                                              DDstart,
                                              " 00:00:00 UTC",
                                              sep=""),
                                        tz = "UTC")

end_date_for_full_record   = as.POSIXct(paste(YYYYend,
                                              "-",
                                              MMend,
                                              "-",
                                              DDend,
                                              " 23:00:00 UTC",
                                              sep=""),
                                        tz = "UTC")




for (target_year in year(start_date_for_full_record):year(end_date_for_full_record))   {                        
  
  
  file_title_string = "KRAP"
  name_of_station   = "RAPID CITY REGIONAL AIRPORT"
  
  
  
  target_usaf = stations_near_targ$usaf[station_list_number]  
  target_wban = stations_near_targ$wban[station_list_number]  
  
  station_lon = stations_near_targ$longitude[station_list_number]
  station_lat = stations_near_targ$latitude[station_list_number]
  station_alt = stations_near_targ$elev_m[station_list_number]
  
  station_archive_name = stations_near_targ$station_name[station_list_number]
  
  station_icao_name = stations_near_targ$icao[station_list_number]
  
  station_name_label = paste(name_of_station, 
                             target_year)
  
  station_id_name = paste(stations_near_targ$usaf[station_list_number_label],
                          "-",
                          stations_near_targ$wban[station_list_number_label],
                          "__",
                          stations_near_targ$icao[station_list_number_label],
                          "__",
                          stations_near_targ$station_name[station_list_number_label],
                          "_",
                          stations_near_targ$state[station_list_number_label],
                          "_",
                          stations_near_targ$ctry[station_list_number_label],
                          sep="")
  
  station_id_name = gsub(" ", "_", station_id_name)
  
  
  output_file_name_prefix = paste("ISD_",
                           station_id_name,
                           "___",
                           sep="")
  
  targ_data = isd(usaf     = target_usaf,  # your usaf number
                  wban     = target_wban,  # your wban number
                  year     = target_year,  # your year
                  progress = TRUE)         # shows prograss as you go
  
  colnames(targ_data)
  targ_data$date_time = ymd_hm(sprintf("%s %s",
                                       as.character(targ_data$date),
                                       targ_data$time))
  
  targ_data$date = targ_data$date_time 
  
  targ_data$temperature[targ_data$temperature == "+9999"]                   = NA 
  
  targ_data$temperature_dewpoint[targ_data$temperature_dewpoint == "+9999"] = NA
  
  targ_data$air_pressure[targ_data$air_pressure == "99999"]                 = NA
  
  targ_data$wind_speed[targ_data$wind_speed == "9999"]                      = NA
  
  targ_data$wind_direction[targ_data$wind_direction == "999"]               = NA
  
  precip_workspace_time_interval = as.numeric(targ_data$AA1_period_quantity_hrs)
  
  precip_workspace_depth = as.numeric(targ_data$AA1_depth)
  
  precip_workspace_depth[precip_workspace_depth == 9999] = NA 
  
  precip_workspace_depth_01hrly =  precip_workspace_depth

  
  precip_workspace_depth_01hrly[precip_workspace_time_interval != 01]  =  NA

  
  targ_data$precip_01hr = precip_workspace_depth_01hrly

  
  targ_data = isd_transform(targ_data)
  
  # patch the wind direction so it's 0 degrees when the wind speed is missing
  
  targ_data$wind_direction[targ_data$wind_speed == 0]             = 0
  
  targ_data$GF1_total_cloud_cover_fraction = as.numeric(targ_data$GF1_coverage)
  
  # 00: None, SKC or CLR
  
  targ_data$GF1_total_cloud_cover_fraction[targ_data$GF1_total_cloud_cover_fraction==00] = 0.00
  
  # 01: One okta - 1/10 or less but not zero
  
  targ_data$GF1_total_cloud_cover_fraction[targ_data$GF1_total_cloud_cover_fraction==01] = 1.0 / 8.0
  
  # 02: Two oktas - 2/10 - 3/10, or FEW
  
  targ_data$GF1_total_cloud_cover_fraction[targ_data$GF1_total_cloud_cover_fraction==02] = 2.0 / 8.0
  
  # 03: Three oktas - 4/10
  
  targ_data$GF1_total_cloud_cover_fraction[targ_data$GF1_total_cloud_cover_fraction==03] = 3.0 / 8.0
  
  # 04: Four oktas - 5/10, or SCT
  
  targ_data$GF1_total_cloud_cover_fraction[targ_data$GF1_total_cloud_cover_fraction==04] = 4.0 / 8.0
  
  # 05: Five oktas - 6/10
  
  targ_data$GF1_total_cloud_cover_fraction[targ_data$GF1_total_cloud_cover_fraction==05] = 5.0 / 8.0
  
  # 06: Six oktas - 7/10 - 8/10
  
  targ_data$GF1_total_cloud_cover_fraction[targ_data$GF1_total_cloud_cover_fraction==06] = 6.0 / 8.0
  
  # 07: Seven oktas - 9/10 or more but not 10/10, or BKN
  
  targ_data$GF1_total_cloud_cover_fraction[targ_data$GF1_total_cloud_cover_fraction==07] = 7.0 / 8.0
  
  # 08: Eight oktas - 10/10, or OVC
  
  targ_data$GF1_total_cloud_cover_fraction[targ_data$GF1_total_cloud_cover_fraction==08] = 8.0 / 8.0
  
  # 09: Sky obscured, or cloud amount cannot be estimated
  
  targ_data$GF1_total_cloud_cover_fraction[targ_data$GF1_total_cloud_cover_fraction==09] = 8.0 / 8.0
  
  # 10: Partial obscuration
  
  targ_data$GF1_total_cloud_cover_fraction[targ_data$GF1_total_cloud_cover_fraction==10] = 4.0 / 8.0
  
  # 11: Thin scattered
  
  targ_data$GF1_total_cloud_cover_fraction[targ_data$GF1_total_cloud_cover_fraction==11] = 2.0 / 8.0
  
  # 12: Scattered
  
  targ_data$GF1_total_cloud_cover_fraction[targ_data$GF1_total_cloud_cover_fraction==12] = 4.0 / 8.0
  
  # 13: Dark scattered
  
  targ_data$GF1_total_cloud_cover_fraction[targ_data$GF1_total_cloud_cover_fraction==13] = 5.0 / 8.0
  
  # 14: Thin broken
  
  targ_data$GF1_total_cloud_cover_fraction[targ_data$GF1_total_cloud_cover_fraction==14] = 6.0 / 8.0
  
  # 15: Broken
  
  targ_data$GF1_total_cloud_cover_fraction[targ_data$GF1_total_cloud_cover_fraction==15] = 7.0 / 8.0
  
  # 16: Dark broken
  
  targ_data$GF1_total_cloud_cover_fraction[targ_data$GF1_total_cloud_cover_fraction==16] = 8.0 / 8.0
  
  # 17: Thin overcast
  
  targ_data$GF1_total_cloud_cover_fraction[targ_data$GF1_total_cloud_cover_fraction==17] = 4.0 / 8.0
  
  # 18: Overcast
  
  targ_data$GF1_total_cloud_cover_fraction[targ_data$GF1_total_cloud_cover_fraction==18] = 8.0 / 8.0
  
  # 19: Dark overcast
  
  targ_data$GF1_total_cloud_cover_fraction[targ_data$GF1_total_cloud_cover_fraction==19] = 8.0 / 8.0
  
  # 99: Missing
  
  targ_data$GF1_total_cloud_cover_fraction[targ_data$GF1_total_cloud_cover_fraction==99] = NA
  
  
  ###############
  
  
  start_date = as.POSIXct(paste(target_year,
                                "-01-01 00:00:00 UTC",
                                sep=""),
                          tz = "UTC")
  
  
  end_date   = as.POSIXct(paste((target_year),
                                "-12-31 23:00:00 UTC",
                                sep=""),
                          tz = "UTC")
  
  hour_time = seq.POSIXt(from = max(c(start_date,
                                      start_date_for_full_record)),
                         to   = min(c(end_date,
                                      end_date_for_full_record)),
                         by   = "1 hour",
                         tz   = "UTC")
  
 
  time_start_in_reference_seconds = as.numeric(interval(start = "2006-01-01 00:00:00 UTC",
                                              end   = min(hour_time),
                                              tzone = tz(start)) )
  
  time_end_in_reference_seconds   = as.numeric(interval(start = "2006-01-01 00:00:00 UTC",
                                              end   = max(hour_time),
                                              tzone = tz(start)) )
  
  hourly_interval = as.double(3600)
  
  time_in_netcdf_units = seq(from = time_start_in_reference_seconds, 
                             to   = time_end_in_reference_seconds, 
                             by   =  hourly_interval)  
  
  targ_time_series                  = data.frame(date = hour_time)
  
  targ_time_series$temperature_degC = approx(x      = targ_data$date_time,
                                             y      = targ_data$temperature,
                                             method = "linear",
                                             xout   = hour_time)$y
  
  targ_time_series$dewpoint_degC    = approx(x      = targ_data$date_time,
                                             y      = targ_data$temperature_dewpoint,
                                             method = "linear",
                                             xout   = hour_time)$y 
  
  targ_time_series$cloud_fraction   = approx(x      = targ_data$date_time,
                                             y      = targ_data$GF1_total_cloud_cover_fraction,
                                             method = "linear",
                                             xout   = hour_time)$y 
  
    targ_time_series$press_msl_hPa    = approx(x      = targ_data$date_time,
                                             y      = targ_data$air_pressure,
                                             method = "linear",
                                             xout   = hour_time)$y
  
  targ_time_series$wind_spd_ms      = approx(x      = targ_data$date_time,
                                             y      = targ_data$wind_speed,
                                             method = "linear",
                                             xout   = hour_time)$y
  
  targ_time_series$wind_dir_degrees  = approx(x     = targ_data$date_time,
                                              y      = targ_data$wind_direction,
                                              method = "linear",
                                              xout   = hour_time)$y

    targ_time_series$ISD_precip_01hr   = approx(x      = targ_data$date_time,
                                                y      = targ_data$precip_01hr,
                                                method = "constant",
                                                xout   = hour_time)$y


  
  output_file_name = paste(storage_directory, 
                           output_file_name_prefix,
                           "_HOURLY_",
                           target_year,
                           ".csv",
                           sep="")
  
  write.table(x    = targ_time_series, 
              file = output_file_name, 
              sep  =", ",
              row.names = FALSE)
  
  targ_time_series_raw                      = data.frame(date = targ_data$date_time)
  
  targ_time_series_raw$data_product_code    = targ_data$type_code
  
  targ_time_series_raw$temperature          = targ_data$temperature
  targ_time_series_raw$temperature_dewpoint = targ_data$temperature_dewpoint
  targ_time_series_raw$cloud_cover_fraction = targ_data$GF1_total_cloud_cover_fraction
  targ_time_series_raw$air_pressure         = targ_data$air_pressure
  
  targ_time_series_raw$wind_speed       = targ_data$wind_speed
  targ_time_series_raw$wind_dir_degrees = targ_data$wind_direction
  
  targ_time_series_raw$ISD_precip_01hr = targ_data$precip_01hr
  
  
  output_file_name = paste(storage_directory, 
                           output_file_name_prefix,
                           "_RAW_",
                           target_year,
                           ".csv",
                           sep="")
  
  write.table(x    = targ_time_series_raw, 
              file = output_file_name, 
              sep  =", ",
              row.names = FALSE)
  
  netcdf_time_dim  = ncdim_def(name  = "time",
                               units = "hours since 2006-01-01 00:00:00", 
                               val   = as.double(time_in_netcdf_units)/as.double(3600),
                               unlim = TRUE,
                               calendar="standard")
  fill_value = 9.96921e+36
  
  #--------------------------------------------------------
  
  netcdf_lat      = ncvar_def(nam      = "latitude",
                              units    = "degrees_north",
                              dim      = list(),
                              longname = "Latitude",
                              prec     ="single")
  
  netcdf_lon      = ncvar_def(nam      = "longitude",
                              units    = "degrees_east",
                              dim      = list(),
                              longname = "Longitue",
                              prec     ="single")

  netcdf_alt      = ncvar_def(nam      = "altitude",
                              units    = "m",
                              dim      = list(),
                              longname = "Elevation",
                              prec     ="single")

  
  #--------------------------------------------------------
  
    
  netcdf_temp      = ncvar_def(nam      = "air_temperature",
                               units    = "degC",
                               dim      = netcdf_time_dim,
                               missval  = fill_value,
                               longname = "2-m Air Temperature",
                               prec     ="single")
  
  netcdf_dewpoint  = ncvar_def(nam      = "dew_point_temperature",
                               units    = "degC",
                               dim      = netcdf_time_dim,
                               missval  = fill_value,
                               longname = "2-m Dew Point Temperature",
                               prec     ="single")
  
  netcdf_mslp      = ncvar_def(nam      = "air_pressure_at_mean_sea_level",
                               units    = "hPa",
                               dim      = netcdf_time_dim,
                               missval  = fill_value,
                               longname = "Air Pressure Reduced to Mean Sea Level",
                               prec     ="single")
  
  netcdf_cloud     = ncvar_def(nam      = "cloud_area_fraction",
                               units    = "fraction",
                               dim      = netcdf_time_dim,
                               missval  = fill_value,
                               longname = "Message-Derived Cloud Cover Fraction",
                               prec     ="single")
  
  netcdf_windspeed = ncvar_def(nam      = "wind_speed",
                               units    = "m s-1",
                               dim      = netcdf_time_dim,
                               missval  = fill_value,
                               longname = "10-m Wind Speed",
                               prec     ="single")
  
  netcdf_winddir   = ncvar_def(nam      = "wind_from_direction",
                               units    = "degrees_from",
                               dim      = netcdf_time_dim,
                               missval  = fill_value,
                               longname = "10-m Wind Source Direction",
                               prec     ="single")
  
  netcdf_prec      = ncvar_def(nam      = "precipitation_amount",
                               units    = "kg m-2",
                               dim      = netcdf_time_dim,
                               missval  = fill_value,
                               longname = "Hourly Precipitation",
                               prec     ="single")
  
  #--------------------------------------------------------
  
  netcdf_output_file_name = paste(output_file_name_prefix,
                                  "___",
                                  target_year,
                                  ".nc",
                                  sep="")
  
  nc_hourly = nc_create(filename = netcdf_output_file_name, 
                        vars     = list(netcdf_lat,
                                        netcdf_lon,
                                        netcdf_alt,
                                        netcdf_temp,
                                        netcdf_dewpoint,
                                        netcdf_mslp,
                                        netcdf_cloud,
                                        netcdf_windspeed,
                                        netcdf_winddir,
                                        netcdf_prec), 
                        force_v4 = FALSE, 
                        verbose  = FALSE )

##############################################  
    
  ncatt_put(nc         = nc_hourly, 
            varid      = 0, 
            attname    = "Title", 
            attval     = paste("NCEI ISD/DS3505 Data Hourly Output for ",
                               name_of_station,
                               sep=""), 
            prec       = NA, 
            verbose    = TRUE, 
            definemode = FALSE )
  
  ncatt_put(nc         = nc_hourly, 
            varid      = 0, 
            attname    = "WBAN_Number", 
            attval     = as.integer(target_wban), 
            prec       = NA, 
            verbose    = FALSE, 
            definemode = FALSE )
  
  ncatt_put(nc         = nc_hourly, 
            varid      = 0, 
            attname    = "USAF_Number", 
            attval     = as.integer(target_usaf), 
            prec       = NA, 
            verbose    = FALSE, 
            definemode = FALSE )
  
  ncatt_put(nc         = nc_hourly, 
            varid      = 0, 
            attname    = "Station_Name", 
            attval     = name_of_station, 
            prec       = NA, 
            verbose    = FALSE, 
            definemode = FALSE )
        
  ncatt_put(nc         = nc_hourly,
            varid      = 0,
            attname    = "featureType",
            attval     = "timeSeries",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE)

            
            
            
            
            
#########################################           
#########################################           
  #########################################           
  
            
  
  ncatt_put(nc         = nc_hourly, 
            varid      = 0, 
            attname    = "Station_Latitude", 
            attval     = station_lat, 
            prec       = NA, 
            verbose    = FALSE, 
            definemode = FALSE )
  
  ncatt_put(nc         = nc_hourly, 
            varid      = 0, 
            attname    = "Station_Latitude", 
            attval     = station_lon, 
            prec       = NA, 
            verbose    = FALSE, 
            definemode = FALSE )


  
  ##############################################  
  ##############################################  
  ##############################################  
  
  
  ncatt_put(nc         = nc_hourly, 
            varid      = netcdf_prec, 
            attname    = "standard_name", 
            attval     = "precipitation_amount", 
            prec       = NA, 
            verbose    = FALSE, 
            definemode = FALSE )
  
  ncatt_put(nc         = nc_hourly, 
            varid      = netcdf_prec, 
            attname    = "description", 
            attval     = "Hourly Precipitation", 
            prec       = NA, 
            verbose    = FALSE, 
            definemode = FALSE )
  
  ncatt_put(nc         = nc_hourly, 
            varid      = netcdf_prec, 
            attname    = "cell_methods",
            attval     = "time: sum",
            prec       = NA, 
            verbose    = FALSE, 
            definemode = FALSE )

  ncatt_put(nc         = nc_hourly,
            varid      = netcdf_prec,
            attname    = "coordinates",
            attval     = "time latitude longitude altitude station_name",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )  
    
  ncatt_put(nc         = nc_hourly,
            varid      = netcdf_prec,
            attname    = "comment",
            attval     = "Precipitation amounts taken from FM-15 & SAO Messages",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )

  
  ##############################################  
  
  
  ncatt_put(nc         = nc_hourly, 
            varid      = netcdf_winddir, 
            attname    = "standard_name", 
            attval     = "wind_from_direction", 
            prec       = NA, 
            verbose    = FALSE, 
            definemode = FALSE )
  
  ncatt_put(nc         = nc_hourly, 
            varid      = netcdf_winddir, 
            attname    = "description", 
            attval     = "10-m Wind Source Direction", 
            prec       = NA, 
            verbose    = FALSE, 
            definemode = FALSE )
  
  ncatt_put(nc         = nc_hourly,
            varid      = netcdf_winddir,
            attname    = "coordinates",
            attval     = "time latitude longitude altitude station_name",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )   
  
  ##############################################  
  
  ncatt_put(nc         = nc_hourly, 
            varid      = netcdf_windspeed, 
            attname    = "standard_name", 
            attval     = "wind_speed", 
            prec       = NA, 
            verbose    = FALSE, 
            definemode = FALSE )
  
  ncatt_put(nc         = nc_hourly, 
            varid      = netcdf_windspeed, 
            attname    = "description", 
            attval     = "10-m Wind Speed", 
            prec       = NA, 
            verbose    = FALSE, 
            definemode = FALSE )
  
  ncatt_put(nc         = nc_hourly,
            varid      = netcdf_windspeed,
            attname    = "coordinates",
            attval     = "time latitude longitude altitude station_name",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )   
  
  ##############################################  
  
  
  ncatt_put(nc         = nc_hourly, 
            varid      = netcdf_cloud, 
            attname    = "standard_name", 
            attval     = "cloud_area_fraction", 
            prec       = NA, 
            verbose    = FALSE, 
            definemode = FALSE )
  
  ncatt_put(nc         = nc_hourly, 
            varid      = netcdf_cloud, 
            attname    = "description", 
            attval     = "Message-Derived Cloud Cover Fraction", 
            prec       = NA, 
            verbose    = FALSE, 
            definemode = FALSE )

  ncatt_put(nc         = nc_hourly,
            varid      = netcdf_cloud,
            attname    = "coordinates",
            attval     = "time latitude longitude altitude station_name",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )   
  
  ##############################################  
  
    
  ncatt_put(nc         = nc_hourly, 
            varid      = netcdf_mslp, 
            attname    = "standard_name", 
            attval     = "air_pressure_at_mean_sea_level", 
            prec       = NA, 
            verbose    = FALSE, 
            definemode = FALSE )
  
  ncatt_put(nc         = nc_hourly, 
            varid      = netcdf_mslp, 
            attname    = "description", 
            attval     = "Air Pressure Reduced to Mean Sea Level", 
            prec       = NA, 
            verbose    = FALSE, 
            definemode = FALSE )

  ncatt_put(nc         = nc_hourly,
            varid      = netcdf_mslp,
            attname    = "coordinates",
            attval     = "time latitude longitude altitude station_name",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )  
    
  ##############################################  
  
  
  ncatt_put(nc         = nc_hourly, 
            varid      = netcdf_dewpoint, 
            attname    = "standard_name", 
            attval     = "dew_point_temperature", 
            prec       = NA, 
            verbose    = FALSE, 
            definemode = FALSE )
  
  ncatt_put(nc         = nc_hourly, 
            varid      = netcdf_dewpoint, 
            attname    = "description", 
            attval     = "2-m Dew Point Temperature", 
            prec       = NA, 
            verbose    = FALSE, 
            definemode = FALSE )

  ncatt_put(nc         = nc_hourly,
            varid      = netcdf_dewpoint,
            attname    = "coordinates",
            attval     = "time latitude longitude altitude station_name",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )  
  
  
  ##############################################  
  
  
    
  ncatt_put(nc         = nc_hourly, 
            varid      = netcdf_temp, 
            attname    = "standard_name", 
            attval     = "air_temperature", 
            prec       = NA, 
            verbose    = FALSE, 
            definemode = FALSE )
  
  ncatt_put(nc         = nc_hourly, 
            varid      = netcdf_temp, 
            attname    = "description", 
            attval     = "2-m Air Temperature", 
            prec       = NA, 
            verbose    = FALSE, 
            definemode = FALSE )
  
  ncatt_put(nc         = nc_hourly,
            varid      = netcdf_temp,
            attname    = "coordinates",
            attval     = "time latitude longitude altitude station_name",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )  
  
  ##############################################  
  ##############################################  
  ##############################################  
  
  
  ncatt_put(nc         = nc_hourly, 
            varid      = netcdf_alt, 
            attname    = "standard_name", 
            attval     = "altitude", 
            prec       = NA, 
            verbose    = FALSE, 
            definemode = FALSE )
  
  ncatt_put(nc         = nc_hourly, 
            varid      = netcdf_alt, 
            attname    = "description", 
            attval     = "Elevation", 
            prec       = NA, 
            verbose    = FALSE, 
            definemode = FALSE )

 
   
   ##############################################  
  
  ncatt_put(nc         = nc_hourly, 
            varid      = netcdf_lon, 
            attname    = "standard_name", 
            attval     = "longitiude", 
            prec       = NA, 
            verbose    = FALSE, 
            definemode = FALSE )
  
  ncatt_put(nc         = nc_hourly, 
            varid      = netcdf_lon, 
            attname    = "description", 
            attval     = "Longitude", 
            prec       = NA, 
            verbose    = FALSE, 
            definemode = FALSE )
  
  
  ##############################################  
  
  ncatt_put(nc         = nc_hourly, 
            varid      = netcdf_lat, 
            attname    = "standard_name", 
            attval     = "latitude", 
            prec       = NA, 
            verbose    = FALSE, 
            definemode = FALSE )
  
  ncatt_put(nc         = nc_hourly, 
            varid      = netcdf_lat, 
            attname    = "description", 
            attval     = "Latitude", 
            prec       = NA, 
            verbose    = FALSE, 
            definemode = FALSE )
  
  ncvar_put(nc      = nc_hourly, 
            varid   = netcdf_lat, 
            vals    = station_lat, 
            verbose = FALSE ) 
  
  
  ##############################################  
  ##############################################  
  ##############################################  
  ##############################################  
  ##############################################  
  ##############################################  
  
  ncvar_put(nc      = nc_hourly, 
            varid   = netcdf_lon, 
            vals    = station_lon, 
            verbose = FALSE ) 
  
  ncvar_put(nc      = nc_hourly, 
            varid   = netcdf_alt, 
            vals    = station_alt, 
            verbose = FALSE ) 
  
  ncvar_put(nc      = nc_hourly, 
            varid   = netcdf_temp, 
            vals    = targ_time_series$temperature_degC, 
            verbose = FALSE ) 
  
  ncvar_put(nc      = nc_hourly, 
            varid   = netcdf_dewpoint, 
            vals    = targ_time_series$dewpoint_degC, 
            verbose = FALSE ) 
  
  ncvar_put(nc      = nc_hourly, 
            varid   = netcdf_mslp, 
            vals    = targ_time_series$press_msl_hPa, 
            verbose = FALSE ) 
  
  ncvar_put(nc      = nc_hourly, 
            varid   = netcdf_cloud, 
            vals    = targ_time_series$cloud_fraction, 
            verbose = FALSE ) 
  
  ncvar_put(nc      = nc_hourly, 
            varid   = netcdf_windspeed, 
            vals    = targ_time_series$wind_spd_ms, 
            verbose = FALSE )
  
  ncvar_put(nc      = nc_hourly, 
            varid   = netcdf_winddir, 
            vals    = targ_time_series$wind_dir_degrees, 
            verbose = FALSE )
  
  ncvar_put(nc      = nc_hourly, 
            varid   = netcdf_prec, 
            vals    = targ_time_series$ISD_precip_01hr, 
            verbose = FALSE )
  
  nc_close( nc_hourly )
}
