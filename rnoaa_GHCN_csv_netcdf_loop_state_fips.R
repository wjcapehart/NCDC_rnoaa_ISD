

# Plain R Script (can't do this in a notebook that 
#    loops through a designated region classification)
#      [state, county, country, watershed]
#    and extracts data from NCDC.  Saves Data as
#      CSV and netCDF.

library("rnoaa")
library("isdparser")
library("lubridate")
library("ncdf4")
library("dplyr")
# library("openair")
library("rlist")
library("readxl")
library("tidyverse")
library("tidycensus")


ghcn_metadata = read_excel(path = "~/GitHub/NCDC_rnoaa_ISD/GHCN_Metadata.xlsx")



# my common loc ids : SD->FIPS:46
#                     AK->FIPS:02
#                     NC->FIPS:37
#                     CA->FIPS:06
#                     WI->FIPS:55
#                     PA->FIPS:42
#                     NM->FIPS:35
#                     NAMIBIA->FIPS:WA 
#                     Mongolia->FIPS:MG 
#                     CHEYENNE->HUC:101202 &
#                               HUC:101201
#                     CHEYENNE->HUC:101202 &
#                               HUC:101201
#                     Pennington->FIPS:46103
#                     Buncombe->FIPS:37021
#                     Onslow->FIPS:37133

target_data_directory_root = "/Users/wjc/GitHub/NCDC_rnoaa_ISD/GHCN_DATA/USA/PA/"
target_fipcode = "42"
ncdc_ids = ncdc_stations(locationid = str_c("FIPS:",target_fipcode, sep=""),
                         datasetid  = 'GHCND',
                         limit      = 1000)


dir.create(path = str_c(target_data_directory_root,"netCDF", sep=""),
           recursive = TRUE)
dir.create(path = str_c(target_data_directory_root,"RData", sep=""),
           recursive = TRUE)




rdata_bigfile_name = "GHCND-GLOBAL_"

n_stations = ncdc_ids$meta$pageCount

if (n_stations == 1000)  {
  
  print("max threshold of stations reached") }





data(fips_codes)

fips_codes  = fips_codes %>% 
  filter(state_code == target_fipcode) %>%
  mutate(state_county_code = str_c(state_code,county_code, sep=""))


print(fips_codes)

this_fip = fips_codes$state_county_code[1]
n_county = 0
for (this_fip in fips_codes$state_county_code) 
{
  n_county = n_county + 1
  print(str_c("COUNTY ",this_fip," (",n_county, " of ",nrow(fips_codes),")",sep=""))
  ncdc_ids = ncdc_stations(locationid = str_c("FIPS:",this_fip, sep=""),
                           datasetid  = 'GHCND',
                           limit      = 1000)
  
  
  
  ncdc_ids = ncdc_ids$data
  
  print(ncdc_ids)
  
  
  
  ghcn_station_information = ncdc_ids
  
  
  total_number_of_stations = length(ncdc_ids$name)
  
  print(str_c("Total Number of Stations in Dataset ",total_number_of_stations))
  
  if (total_number_of_stations>0)
  {
    
  
  print("")
  print(" Begin Looping")
  print("")
  
  
  
  
  ghcn_station_information$mindate = as.Date(ghcn_station_information$mindate)
  ghcn_station_information$maxdate = as.Date(ghcn_station_information$maxdate)
  
  indexlist = 1:total_number_of_stations
  
  print(ghcn_station_information)
  
  ncounty_stations = 0
  
  for (ncdc_index in 1:total_number_of_stations ) 
  { 
      

      station_name_label     = ncdc_ids$name[ncdc_index]
      station_latitude       = ncdc_ids$latitude[ncdc_index]
      station_longitude      = ncdc_ids$longitude[ncdc_index]
      station_altitude       = ncdc_ids$elevation[ncdc_index]
    
      ncdc_id_code = ncdc_ids$id[ncdc_index]
    
      ghcn_station_code = unlist(strsplit(x      = ncdc_id_code,
                                          split  = ":"))
    
      ncdc_start_yymmdd = ncdc_ids$mindate[ncdc_index]
      ncdc_end_yymmdd   = ncdc_ids$maxdate[ncdc_index]
    
      Date = seq(from = as.Date(ncdc_start_yymmdd),
                 to   = as.Date(ncdc_end_yymmdd),
                 by   = "days")
    
      ncdc_data = ghcnd(stationid = ghcn_station_code[2])
    
      available_datafields = str_trim(unique(ncdc_data$element), side = c("both"))
    
      sorted_data = ghcnd_splitvars(ncdc_data)
    
      filename_station_label = ncdc_ids$name[ncdc_index]
      filename_station_label = gsub(" US",  "",  filename_station_label)
      filename_station_label = gsub(  ",",  "",  filename_station_label)
      filename_station_label = gsub(  " ", "_",  filename_station_label)
    
      filename_ghcn_label = ncdc_ids$id[ncdc_index]
      filename_ghcn_label = gsub(":", "-", filename_ghcn_label)
    
      file_title_string = paste(filename_ghcn_label,
                                "__",
                                filename_station_label,
                                sep="")
    
      remove(ncdc_data)
    
      print("--------------------------")
      print(str_c("County #",this_fip," (",n_county, " of ",nrow(fips_codes),")",sep=""))
      
      print(paste("Station # ",
                  ncdc_index,
                  " of ",
                  total_number_of_stations,
                  sep = ""))
      print(filename_station_label)    
    
       ghcn_station = data.frame(  station_name           = as.character(ncdc_ids$name[ncdc_index]),
                                   station_latitude       = ncdc_ids$latitude[ncdc_index],
                                   station_longitude      = ncdc_ids$longitude[ncdc_index],
                                   station_altitude       = ncdc_ids$elevation[ncdc_index],
                                   ncdc_id_code           = as.character(ncdc_id_code),
                                   time                   = Date)
       
       print(available_datafields)
    
    
      if ("TMAX" %in% available_datafields)  {
        tmax_full_field          = sorted_data$tmax
          ordered                = order(tmax_full_field$date)
          tmax_full_field$tmax[] = tmax_full_field$tmax[ordered] / 10
          tmax_full_field$date[] = tmax_full_field$date[ordered]
          
          sub = data.frame(  station_name           = as.character(ncdc_ids$name[ncdc_index]),
                             station_latitude       = ncdc_ids$latitude[ncdc_index],
                             station_longitude      = ncdc_ids$longitude[ncdc_index],
                             station_altitude       = ncdc_ids$elevation[ncdc_index],
                             ncdc_id_code           = as.character(ncdc_id_code),
                             time                   = tmax_full_field$date,
                             tmax                   = tmax_full_field$tmax)
          
          ghcn_station = full_join(ghcn_station, 
                                   sub, 
                                   by=c("station_name",
                                        "station_latitude",
                                        "station_longitude",
                                        "station_altitude",
                                        "ncdc_id_code",
                                        "time") )
          
    
          
                             
          
      }
      if ("TMIN" %in% available_datafields)  {
        tmin_full_field          = sorted_data$tmin
          ordered                = order(tmin_full_field$date)
          tmin_full_field$tmin[] = tmin_full_field$tmin[ordered] / 10
          tmin_full_field$date[] = tmin_full_field$date[ordered]
          
          sub = data.frame(  station_name           = as.character(ncdc_ids$name[ncdc_index]),
                             station_latitude       = ncdc_ids$latitude[ncdc_index],
                             station_longitude      = ncdc_ids$longitude[ncdc_index],
                             station_altitude       = ncdc_ids$elevation[ncdc_index],
                             ncdc_id_code           = as.character(ncdc_id_code),
                             time                   = tmin_full_field$date,
                             tmin                   = tmin_full_field$tmin)
          
          ghcn_station = full_join(ghcn_station, 
                                   sub, 
                                   by=c("station_name",
                                        "station_latitude",
                                        "station_longitude",
                                        "station_altitude",
                                        "ncdc_id_code",
                                        "time") )    
            }
      
      if ("TAVG" %in% available_datafields)  {
        tavg_full_field          = sorted_data$tavg
          ordered                = order(tavg_full_field$date)
          tavg_full_field$tavg[] = tavg_full_field$tavg[ordered] / 10
          tavg_full_field$date[] = tavg_full_field$date[ordered]
          
          sub = data.frame(  station_name           = as.character(ncdc_ids$name[ncdc_index]),
                             station_latitude       = ncdc_ids$latitude[ncdc_index],
                             station_longitude      = ncdc_ids$longitude[ncdc_index],
                             station_altitude       = ncdc_ids$elevation[ncdc_index],
                             ncdc_id_code           = as.character(ncdc_id_code),
                             time                   = tavg_full_field$date,
                             tavg                   = tavg_full_field$tavg)
    
          ghcn_station = full_join(ghcn_station, 
                                   sub, 
                                   by=c("station_name",
                                        "station_latitude",
                                        "station_longitude",
                                        "station_altitude",
                                        "ncdc_id_code",
                                        "time") )
            }
      
      if ("PRCP" %in% available_datafields)  {
        prcp_full_field          = sorted_data$prcp
          ordered                = order(prcp_full_field$date)
          prcp_full_field$prcp[] = prcp_full_field$prcp[ordered] / 10
          prcp_full_field$date[] = prcp_full_field$date[ordered]
          
          sub = data.frame(  station_name           = as.character(ncdc_ids$name[ncdc_index]),
                             station_latitude       = ncdc_ids$latitude[ncdc_index],
                             station_longitude      = ncdc_ids$longitude[ncdc_index],
                             station_altitude       = ncdc_ids$elevation[ncdc_index],
                             ncdc_id_code           = as.character(ncdc_id_code),
                             time                   = prcp_full_field$date,
                             prcp                   = prcp_full_field$prcp)
          
          ghcn_station = full_join(ghcn_station, 
                                   sub, 
                                   by=c("station_name",
                                        "station_latitude",
                                        "station_longitude",
                                        "station_altitude",
                                        "ncdc_id_code",
                                        "time") )
      }
    
      if ("SNOW" %in% available_datafields)  {
    
        snow_full_field          = sorted_data$snow
          ordered                = order(snow_full_field$date)
          snow_full_field$snow[] = snow_full_field$snow[ordered]
          snow_full_field$date[] = snow_full_field$date[ordered]
          
          sub = data.frame(  station_name           = as.character(ncdc_ids$name[ncdc_index]),
                             station_latitude       = ncdc_ids$latitude[ncdc_index],
                             station_longitude      = ncdc_ids$longitude[ncdc_index],
                             station_altitude       = ncdc_ids$elevation[ncdc_index],
                             ncdc_id_code           = as.character(ncdc_id_code),
                             time                   = snow_full_field$date,
                             snow                   = snow_full_field$snow)
          
          ghcn_station = full_join(ghcn_station, 
                                   sub, 
                                   by=c("station_name",
                                        "station_latitude",
                                        "station_longitude",
                                        "station_altitude",
                                        "ncdc_id_code",
                                        "time") )
      }
    
      if ("SNWD" %in% available_datafields)  {
    
        snwd_full_field          = sorted_data$snwd
          ordered                = order(snwd_full_field$date)
          snwd_full_field$snwd[] = snwd_full_field$snwd[ordered]
          snwd_full_field$date[] = snwd_full_field$date[ordered]
          
          sub = data.frame(  station_name           = as.character(ncdc_ids$name[ncdc_index]),
                             station_latitude       = ncdc_ids$latitude[ncdc_index],
                             station_longitude      = ncdc_ids$longitude[ncdc_index],
                             station_altitude       = ncdc_ids$elevation[ncdc_index],
                             ncdc_id_code           = as.character(ncdc_id_code),
                             time                   = snwd_full_field$date,
                             snwd                   = snwd_full_field$snwd)
          
          ghcn_station = full_join(ghcn_station, 
                                   sub, 
                                   by=c("station_name",
                                        "station_latitude",
                                        "station_longitude",
                                        "station_altitude",
                                        "ncdc_id_code",
                                        "time") )
      }
    
    
      if ("WESD" %in% available_datafields)  {
        wesd_full_field          = sorted_data$wesd
          ordered                = order(wesd_full_field$date)
          wesd_full_field$wesd[] = wesd_full_field$wesd[ordered] / 10
          wesd_full_field$date[] = wesd_full_field$date[ordered]
          
          sub = data.frame(  station_name           = as.character(ncdc_ids$name[ncdc_index]),
                             station_latitude       = ncdc_ids$latitude[ncdc_index],
                             station_longitude      = ncdc_ids$longitude[ncdc_index],
                             station_altitude       = ncdc_ids$elevation[ncdc_index],
                             ncdc_id_code           = as.character(ncdc_id_code),
                             time                   = wesd_full_field$date,
                             wesd                   = wesd_full_field$wesd)
          
          ghcn_station = full_join(ghcn_station, 
                                   sub, 
                                   by=c("station_name",
                                        "station_latitude",
                                        "station_longitude",
                                        "station_altitude",
                                        "ncdc_id_code",
                                        "time") )
      }
    
    
      if ("WESF" %in% available_datafields)  {
    
        wesf_full_field          = sorted_data$wesf
          ordered                = order(wesf_full_field$date)
          wesf_full_field$wesf[] = wesf_full_field$wesf[ordered] / 10
          wesf_full_field$date[] = wesf_full_field$date[ordered]
          
          sub = data.frame(  station_name           = as.character(ncdc_ids$name[ncdc_index]),
                             station_latitude       = ncdc_ids$latitude[ncdc_index],
                             station_longitude      = ncdc_ids$longitude[ncdc_index],
                             station_altitude       = ncdc_ids$elevation[ncdc_index],
                             ncdc_id_code           = as.character(ncdc_id_code),
                             time                   = wesf_full_field$date,
                             wesf                   = wesf_full_field$wesf)
          
          ghcn_station = full_join(ghcn_station, 
                                   sub, 
                                   by=c("station_name",
                                        "station_latitude",
                                        "station_longitude",
                                        "station_altitude",
                                        "ncdc_id_code",
                                        "time") )
      }
    
      if ("AWND" %in% available_datafields)  {
    
        awnd_full_field          = sorted_data$awnd
          ordered                = order(awnd_full_field$date)
          awnd_full_field$awnd[] = awnd_full_field$awnd[ordered] / 10
          awnd_full_field$date[] = awnd_full_field$date[ordered]
          
          sub = data.frame(  station_name           = as.character(ncdc_ids$name[ncdc_index]),
                             station_latitude       = ncdc_ids$latitude[ncdc_index],
                             station_longitude      = ncdc_ids$longitude[ncdc_index],
                             station_altitude       = ncdc_ids$elevation[ncdc_index],
                             ncdc_id_code           = as.character(ncdc_id_code),
                             time                   = awnd_full_field$date,
                             awnd                   = awnd_full_field$awnd)
          
          ghcn_station = full_join(ghcn_station, 
                                   sub, 
                                   by=c("station_name",
                                        "station_latitude",
                                        "station_longitude",
                                        "station_altitude",
                                        "ncdc_id_code",
                                        "time") )
      }
    
    
      if ("AWDR" %in% available_datafields)  {
        print("----> AWDR")
        
        awdr_full_field          = sorted_data$awdr
          ordered                = order(awdr_full_field$date)
          awdr_full_field$awdr[] = awdr_full_field$awdr[ordered]
          awdr_full_field$date[] = awdr_full_field$date[ordered]
          
          sub = data.frame(  station_name           = as.character(ncdc_ids$name[ncdc_index]),
                             station_latitude       = ncdc_ids$latitude[ncdc_index],
                             station_longitude      = ncdc_ids$longitude[ncdc_index],
                             station_altitude       = ncdc_ids$elevation[ncdc_index],
                             ncdc_id_code           = as.character(ncdc_id_code),
                             time                   = awdr_full_field$date,
                             awdr                   = awdr_full_field$awdr)
          
          ghcn_station = full_join(ghcn_station, 
                                   sub, 
                                   by=c("station_name",
                                        "station_latitude",
                                        "station_longitude",
                                        "station_altitude",
                                        "ncdc_id_code",
                                        "time") )
      }
    
    
    
      remove(sorted_data)
      remove(ordered)
    

       print(ncdc_index)
    
      if (ncdc_index == 1) {
        ghcn_stations = ghcn_station
      } else {
        ghcn_stations = bind_rows(ghcn_stations,
                                  ghcn_station)
      }
      
      
      
      
      save(gchn_station = ghcn_station,
           file = paste(target_data_directory_root,
                        "RData/",
                        file_title_string,
                        ".Rdata",
                        sep=""))
      
      remove(ghcn_station)
    
    
      Days_from_1970_01_01 = as.numeric( as.Date(Date) )
    
    
      if ("TMAX" %in% available_datafields)  {
    
        ts_in  =   ts(data   = tmax_full_field$tmax,
                      start  = as.Date(tmax_full_field$date[1]),
                      end    = as.Date(tmax_full_field$date[length(tmax_full_field$date)]),
                      deltat = 1,
                      ts.eps = getOption("ts.eps")
                    )
    
        tmax  = window(x      = ts_in,
                       start  = as.numeric(as.Date(ncdc_start_yymmdd)),
                       end    = as.numeric(as.Date(ncdc_end_yymmdd)),
                       deltat = 1,
                       extend = TRUE)
    
        remove(tmax_full_field,
               ts_in)
    
      }
    
      if ("TMIN" %in% available_datafields)  {
    
        ts_in  =   ts(data   = tmin_full_field$tmin,
                      start  = as.Date(tmin_full_field$date[1]),
                      end    = as.Date(tmin_full_field$date[length(tmin_full_field$date)]),
                      deltat = 1,
                      ts.eps = getOption("ts.eps")
                    )
    
        tmin  = window(x      = ts_in,
                       start  = as.numeric(as.Date(ncdc_start_yymmdd)),
                       end    = as.numeric(as.Date(ncdc_end_yymmdd)),
                       deltat = 1,
                       extend = TRUE)
    
        remove(tmin_full_field,
               ts_in)
    
      }
    
    
      if ("TAVG" %in% available_datafields)  {
    
        ts_in  =   ts(data   = tavg_full_field$tavg,
                      start  = as.Date(tavg_full_field$date[1]),
                      end    = as.Date(tavg_full_field$date[length(tavg_full_field$date)]),
                      deltat = 1,
                      ts.eps = getOption("ts.eps")    )
    
        tavg  = window(x      = ts_in,
                       start  = as.numeric(as.Date(ncdc_start_yymmdd)),
                       end    = as.numeric(as.Date(ncdc_end_yymmdd)),
                       deltat = 1,
                       extend = TRUE)
    
        remove(tavg_full_field,
               ts_in)
      }
    
    
      if ("PRCP" %in% available_datafields)  {
    
        ts_in  =   ts(data   = prcp_full_field$prcp,
                      start  = as.Date(prcp_full_field$date[1]),
                      end    = as.Date(prcp_full_field$date[length(prcp_full_field$date)]),
                      deltat = 1,
                      ts.eps = getOption("ts.eps")    )
    
        prcp  = window(x      = ts_in,
                       start  = as.numeric(as.Date(ncdc_start_yymmdd)),
                       end    = as.numeric(as.Date(ncdc_end_yymmdd)),
                       deltat = 1,
                       extend = TRUE)
    
        remove(prcp_full_field,
               ts_in)
      }
    
      if ("SNOW" %in% available_datafields)  {
    
        ts_in  =   ts(data   = snow_full_field$snow,
                      start  = as.Date(snow_full_field$date[1]),
                      end    = as.Date(snow_full_field$date[length(snow_full_field$date)]),
                      deltat = 1,
                      ts.eps = getOption("ts.eps")    )
    
        snow  = window(x      = ts_in,
                       start  = as.numeric(as.Date(ncdc_start_yymmdd)),
                       end    = as.numeric(as.Date(ncdc_end_yymmdd)),
                       deltat = 1,
                       extend = TRUE)
    
        remove(snow_full_field,
               ts_in)
    
      }
    
      if ("SNWD" %in% available_datafields)  {
    
        ts_in  =   ts(data   = snwd_full_field$snwd,
                      start  = as.Date(snwd_full_field$date[1]),
                      end    = as.Date(snwd_full_field$date[length(snwd_full_field$date)]),
                      deltat = 1,
                      ts.eps = getOption("ts.eps")    )
    
        snwd  = window(x      = ts_in,
                       start  = as.numeric(as.Date(ncdc_start_yymmdd)),
                       end    = as.numeric(as.Date(ncdc_end_yymmdd)),
                       deltat = 1,
                       extend = TRUE)
    
        remove(snwd_full_field,
               ts_in)
    
      }
    
      if ("WESD" %in% available_datafields)  {
    
        ts_in  =   ts(data   = wesd_full_field$wesd,
                      start  = as.Date(wesd_full_field$date[1]),
                      end    = as.Date(wesd_full_field$date[length(wesd_full_field$date)]),
                      deltat = 1,
                      ts.eps = getOption("ts.eps")    )
    
        wesd  = window(x      = ts_in,
                       start  = as.numeric(as.Date(ncdc_start_yymmdd)),
                       end    = as.numeric(as.Date(ncdc_end_yymmdd)),
                       deltat = 1,
                       extend = TRUE)
    
        remove(wesd_full_field,
               ts_in)
    
      }
    
      if ("WESF" %in% available_datafields)  {
    
        ts_in  =   ts(data   = wesf_full_field$wesf,
                      start  = as.Date(wesf_full_field$date[1]),
                      end    = as.Date(wesf_full_field$date[length(wesf_full_field$date)]),
                      deltat = 1,
                      ts.eps = getOption("ts.eps")    )
    
        wesf  = window(x      = ts_in,
                       start  = as.numeric(as.Date(ncdc_start_yymmdd)),
                       end    = as.numeric(as.Date(ncdc_end_yymmdd)),
                       deltat = 1,
                       extend = TRUE)
    
        remove(wesf_full_field,
               ts_in)
    
      }
    
    
      if ("AWND" %in% available_datafields)  {
    
        ts_in  =   ts(data   = awnd_full_field$awnd,
                      start  = as.Date(awnd_full_field$date[1]),
                      end    = as.Date(awnd_full_field$date[length(awnd_full_field$date)]),
                      deltat = 1,
                      ts.eps = getOption("ts.eps")    )
    
        awnd  = window(x      = ts_in,
                       start  = as.numeric(as.Date(ncdc_start_yymmdd)),
                       end    = as.numeric(as.Date(ncdc_end_yymmdd)),
                       deltat = 1,
                       extend = TRUE)
    
        remove(awnd_full_field,
               ts_in)
    
      }
    
    
      if ("AWDR" %in% available_datafields)  {
    
        ts_in  =   ts(data   = awdr_full_field$awdr,
                      start  = as.Date(awdr_full_field$date[1]),
                      end    = as.Date(awdr_full_field$date[length(awdr_full_field$date)]),
                      deltat = 1,
                      ts.eps = getOption("ts.eps")    )
    
        awdr  = window(x      = ts_in,
                       start  = as.numeric(as.Date(ncdc_start_yymmdd)),
                       end    = as.numeric(as.Date(ncdc_end_yymmdd)),
                       deltat = 1,
                       extend = TRUE)
    
        remove(awdr_full_field,
               ts_in)
    
      }
    
    
    
    
    
    
      if (1 < 0) {    
    
      if ("TMAX" %in% available_datafields)  {
        plot(x       = Date,
             y       = tmax,
             type    = "p",
             pch     = ".",   # as points
             col     = "red",
             lwd     = 1.5,
             cex.lab = 1.25,
             xlab    = "Date",
             ylab    = "Max Temperature (degC)",
             main    = station_name_label)
      }
    
    
    
    
    
    
    
      if ("TMIN" %in% available_datafields)  {
        plot(x       = Date,
             y       = tmin,
             type    = "p",
             pch     = ".",   # as points
             col     = "darkgoldenrod1",
             lwd     = 1.5,
             cex.lab = 1.25,
             xlab    = "Date",
             ylab    = "Min Temperature (degC)",
             main    = station_name_label)
      }
    
    
    
    
    
    
      if ("TAVG" %in% available_datafields)  {
        plot(x       = Date,
             y       = tavg,
             type    = "p",
             pch     = ".",   # as points
             col     = "orange",
             lwd     = 1.5,
             cex.lab = 1.25,
             xlab    = "Date",
             ylab    = "Mean Temperature (degC)",
             main    = station_name_label)
      }
    
    
    
    
    
      if ("PRCP" %in% available_datafields)  {
        plot(x       = Date,
             y       = prcp,
             type    = "p",
             pch     = ".",   # as points
             col     = "green",
             lwd     = 1.5,
             cex.lab = 1.25,
             xlab    = "Date",
             ylab    = "Daily Precip (mm)",
             main    = station_name_label)
      }
    
    
    
    
    
    
      if ("SNOW" %in% available_datafields)  {
        plot(x       = Date,
             y       = snow,
             type    = "p",
             pch     = ".",   # as points
             col     = "cyan",
             lwd     = 1.5,
             cex.lab = 1.25,
             xlab    = "Date",
             ylab    = "Daily Snowfall (mm)",
             main    = station_name_label)
      }
    
    
    
    
    
    
      if ("SNWD" %in% available_datafields)  {
        plot(x       = Date,
             y       = snwd,
             type    = "p",
             pch     = ".",   # as points
             col     = "darkblue",
             lwd     = 1.5,
             cex.lab = 1.25,
             xlab    = "Date",
             ylab    = "Daily Snowfall (mm)",
             main    = station_name_label)
      }
    
    
    
    
    
    
      if ("WESD" %in% available_datafields)  {
        plot(x       = Date,
             y       = wesd,
             type    = "p",
             pch     = ".",   # as points
             col     = "blue",
             lwd     = 1.5,
             cex.lab = 1.25,
             xlab    = "Date",
             ylab    = "Snow Water Equivalent Depth (mm)",
             main    = station_name_label)
      }
    
    
    
    
    
    
    
      if ("AWDR" %in% available_datafields)  {
        plot(x       = Date,
             y       = awdr,
             type    = "p",
             pch     = ".",   # as points
             col     = "blue",
             lwd     = 1.5,
             cex.lab = 1.25,
             xlab    = "Date",
             ylab    = "Wind Direction (degrees from)",
             main    = station_name_label)
      }
    
    
    
    
    
    
      if ("WESF" %in% available_datafields)  {
        plot(x       = Date,
             y       = wesf,
             type    = "p",
             pch     = ".",   # as points
             col     = "deepskyblue4",
             lwd     = 1.5,
             cex.lab = 1.25,
             xlab    = "Date",
             ylab    = "Snowfall Water Equivalent (mm)",
             main    = station_name_label)
      }
    
    
    
    
    
    
      if ("AWND" %in% available_datafields)  {
        plot(x       = Date,
             y       = awnd,
             type    = "p",
             pch     = ".",   # as points
             col     = "deeppink2",
             lwd     = 1.5,
             cex.lab = 1.25,
             xlab    = "Date",
             ylab    = "Average Wind Speed (m s-1)",
             main    = station_name_label)
      }}
    
    
    
    
    
    
    
      targ_time_series_raw = data.frame(date = Date)
    
      if ("TMAX" %in% available_datafields)
        targ_time_series_raw$Max_Temperature       = tmax
    
      if ("TMIN" %in% available_datafields)
        targ_time_series_raw$Min_Temperature       = tmin
    
      if ("TAVG" %in% available_datafields)
        targ_time_series_raw$Mean_Temperature      = tavg
    
      if ("PRCP" %in% available_datafields)
        targ_time_series_raw$Precipitation         = prcp
    
      if ("SNOW" %in% available_datafields)
        targ_time_series_raw$SnowFall              = snow
    
      if ("SNWD" %in% available_datafields)
        targ_time_series_raw$SnowDepth             = snwd
    
      if ("WESD" %in% available_datafields)
        targ_time_series_raw$Snowdepth_Water_Equiv = wesd
    
      if ("WESF" %in% available_datafields)
        targ_time_series_raw$Snowfall_Water_Equiv  = wesf
    
      if ("AWND" %in% available_datafields)
        targ_time_series_raw$Mean_Wind_Speed            = awnd
    
      if ("AWDR" %in% available_datafields)
        targ_time_series_raw$Mean_Wind_From_Direction   = awdr
    
    
      #output_file_name = paste(target_data_directory_root,
      #                         "CSV/",
      #                         file_title_string,
      #                         ".csv",
      #                         sep="")
    
      #write.csv(x    = targ_time_series_raw,
      #          file = output_file_name,
      #          row.names = FALSE)
    
    
      remove(targ_time_series_raw)
    
    
    
    
    
      netcdf_output_file_name = paste(target_data_directory_root,
                                      "netCDF/",
                                      file_title_string,
                                      ".nc",
                                      sep="")
    
      
      netcdf_time_dim  = ncdim_def(name  = "time",
                                   units = "days since 1970-01-01 00:00:00",
                                   val   = Days_from_1970_01_01,
                                   unlim = TRUE,
                                   calendar="standard")
    
      netcdf_name_dim  = ncdim_def(name  = "name_strlen",
                                   units = "",
                                   val   = 1:nchar(file_title_string),
                                   unlim = FALSE,
                                   create_dimvar=FALSE)
    
      netcdf_bounds_dim  = ncdim_def(name  = "bnds",
                                     units = "",
                                     val   = 1:2,
                                     unlim = FALSE,
                                     create_dimvar = FALSE)
      
      
    
    
      fill_value = 9.96921e+36
    
      fill_value_double = 9.969209968386869e+36
      
      netcdf_stn = ncvar_def(nam      = "station_name",
                             units    = "",
                             dim      = netcdf_name_dim,
                             longname = "station name",
                             prec     = "char")
    
    
      netcdf_lat = ncvar_def(nam      = "latitude",
                             units    = "degrees_north",
                             dim      = list(),
                             longname = "Latitude",
                             prec     = "single")
    
      netcdf_lon = ncvar_def(nam      = "longitude",
                             units    = "degrees_east",
                             dim      = list(),
                             longname = "Longitude",
                             prec     = "single")
    
      netcdf_alt = ncvar_def(nam      = "altitude",
                             units    = "m",
                             dim      = list(),
                             longname = "Elevation",
                             prec     = "single")
    
      bnds = 1:2
      
      time_bounds = array( 0,  
                           dim      = c(2,length(Days_from_1970_01_01)), 
                           dimnames = list(bnds,Days_from_1970_01_01))
      
      time_bounds[1,] = Days_from_1970_01_01
      time_bounds[2,] = Days_from_1970_01_01 + 1
      
      netcdf_time_bounds   = ncvar_def(nam      = "time_bnds",
                                       units    = "days since 1970-01-01 00:00:00",
                                       dim      = list(netcdf_bounds_dim,
                                                       netcdf_time_dim),
                                       longname = "Time Bounds",
                                       prec     = "double")  
    
      netcdf_available_variables = list(netcdf_time_bounds,
                                        netcdf_lat,
                                        netcdf_lon,
                                        netcdf_alt,
                                        netcdf_stn)
    
    
      if ("TMAX" %in% available_datafields) {
        netcdf_tmax = ncvar_def(nam      = "maximum_air_temperature",
                                units    = "degC",
                                dim      = netcdf_time_dim,
                                missval  = fill_value,
                                longname = "2-m Maximum Daily Air Temperature",
                                prec     = "single")
        netcdf_available_variables = list.append(netcdf_available_variables,
                                                 netcdf_tmax)
      }
    
      if ("TMIN" %in% available_datafields) {
        netcdf_tmin = ncvar_def(nam      = "minimum_air_temperature",
                                units    = "degC",
                                dim      = netcdf_time_dim,
                                missval  = fill_value,
                                longname = "2-m Minimium Daily Air Temperature",
                                prec     = "single")
        netcdf_available_variables = list.append(netcdf_available_variables,
                                                 netcdf_tmin)
      }
    
    
      if ("TAVG" %in% available_datafields) {
        netcdf_tavg = ncvar_def(nam      = "mean_air_temperature",
                                units    = "degC",
                                dim      = netcdf_time_dim,
                                missval  = fill_value,
                                longname = "2-m Mean Daily Air Temperature",
                                prec     = "single")
        netcdf_available_variables = list.append(netcdf_available_variables,
                                                 netcdf_tavg)
      }
    
      if ("PRCP" %in% available_datafields) {
        netcdf_prcp = ncvar_def(nam      = "precipitation_amount",
                                units    = "kg m-2",
                                dim      = netcdf_time_dim,
                                missval  = fill_value,
                                longname = "Daily Total Precipitation",
                                prec     = "single")
        netcdf_available_variables = list.append(netcdf_available_variables,
                                                 netcdf_prcp)
      }
    
      if ("SNOW" %in% available_datafields) {
        netcdf_snow = ncvar_def(nam      = "thickness_of_snowfall_amount",
                                units    = "m",
                                dim      = netcdf_time_dim,
                                missval  = fill_value,
                                longname = "Daily Total Snowfall",
                                prec     = "single")
        netcdf_available_variables = list.append(netcdf_available_variables,
                                                 netcdf_snow)
      }
    
      if ("SNWD" %in% available_datafields) {
        netcdf_snwd = ncvar_def(nam      = "surface_snow_thickness",
                                units    = "m",
                                dim      = netcdf_time_dim,
                                missval  = fill_value,
                                longname = "Snow Depth on Surface",
                                prec     = "single")
        netcdf_available_variables = list.append(netcdf_available_variables,
                                                 netcdf_snwd)
      }
    
    
      if ("WESD" %in% available_datafields) {
        netcdf_wesd = ncvar_def(nam      = "liquid_water_content_of_surface_snow",
                                units    = "kg m-2",
                                dim      = netcdf_time_dim,
                                missval  = fill_value,
                                longname = "Liquid Snow Water Equivalent Depth on Surface",
                                prec     = "single")
        netcdf_available_variables = list.append(netcdf_available_variables,
                                                 netcdf_wesd)
      }
    
      if ("WESF" %in% available_datafields) {
        netcdf_wesf = ncvar_def(nam      = "liquid_water_equivalent_snowfall_amount",
                                units    = "kg m-2",
                                dim      = netcdf_time_dim,
                                missval  = fill_value,
                                longname = "Liquid Snowfall Water Equivalent Depth on Surface",
                                prec     = "single")
        netcdf_available_variables = list.append(netcdf_available_variables,
                                                 netcdf_wesf)
      }
    
    
      if ("AWND" %in% available_datafields) {
        netcdf_awnd = ncvar_def(nam      = "mean_wind_speed",
                                units    = "m s-1",
                                dim      = netcdf_time_dim,
                                missval  = fill_value,
                                longname = "Mean Daily Wind Speed",
                                prec     = "single")
        netcdf_available_variables = list.append(netcdf_available_variables,
                                                 netcdf_awnd)
      }
    
    
      if ("AWDR" %in% available_datafields) {
        netcdf_awdr = ncvar_def(nam      = "mean_wind_from_direction",
                                units    = "degrees_from",
                                dim      = netcdf_time_dim,
                                missval  = fill_value,
                                longname = "Mean Daily Wind Origin Direction",
                                prec     = "single")
        netcdf_available_variables = list.append(netcdf_available_variables,
                                                 netcdf_awdr)
      }
    
    
    
    
    
    
    
    
      nc_ghcn = nc_create(filename = netcdf_output_file_name,
                            vars     = netcdf_available_variables,
                            force_v4 = FALSE,
                            verbose  = FALSE )
    
    
    
    
    
    
    ncatt_put(nc         = nc_ghcn,
              varid      = 0,
              attname    = "Title",
              attval     = paste("NCEI Data Hourly Output for ",
                                 station_name_label,
                                 sep=""),
              prec       = NA,
              verbose    = FALSE,
              definemode = FALSE )
    
    ncatt_put(nc         = nc_ghcn,
              varid      = 0,
              attname    = "GHCN_Station_Code",
              attval     = ncdc_id_code,
              prec       = NA,
              verbose    = FALSE,
              definemode = FALSE )
    
    ncatt_put(nc         = nc_ghcn,
              varid      = 0,
              attname    = "Station_Name",
              attval     = station_name_label,
              prec       = NA,
              verbose    = FALSE,
              definemode = FALSE )
    
    ncatt_put(nc         = nc_ghcn,
              varid      = 0,
              attname    = "Station_Latitude",
              attval     = station_latitude,
              prec       = NA,
              verbose    = FALSE,
              definemode = FALSE )
    
    ncatt_put(nc         = nc_ghcn,
              varid      = 0,
              attname    = "Station_Longitude",
              attval     = station_longitude,
              prec       = NA,
              verbose    = FALSE,
              definemode = FALSE )
    
    ncatt_put(nc         = nc_ghcn,
              varid      = 0,
              attname    = "Station_Elevation_in_Meters",
              attval     = station_altitude,
              prec       = NA,
              verbose    = FALSE,
              definemode = FALSE )
    
    ncatt_put(nc         = nc_ghcn,
              varid      = 0,
              attname    = "featureType",
              attval     = "timeSeries",
              prec       = NA,
              verbose    = FALSE,
              definemode = FALSE)
    
    ncatt_put(nc         = nc_ghcn,
               varid      = 0,
               attname    = "Conventions",
               attval     = "CF-1.6",
               prec       = NA,
               verbose    = FALSE,
               definemode = FALSE)
    
    
      ncatt_put(nc         = nc_ghcn,
                varid      = netcdf_stn,
                attname    = "description",
                attval     = "station name",
                prec       = NA,
                verbose    = FALSE,
                definemode = FALSE )
    
       ncatt_put(nc         = nc_ghcn,
                 varid      = netcdf_stn,
                 attname    = "cf_role",
                 attval     = "timeseries_id",
                 prec       = NA,
                 verbose    = FALSE,
                 definemode = FALSE )
    
    
      ncatt_put(nc         = nc_ghcn,
                varid      = netcdf_alt,
                attname    = "standard_name",
                attval     = "altitude",
                prec       = NA,
                verbose    = FALSE,
                definemode = FALSE )
    
      ncatt_put(nc         = nc_ghcn,
                varid      = netcdf_alt,
                attname    = "axis",
                attval     = "Z",
                prec       = NA,
                verbose    = FALSE,
                definemode = FALSE )
      
       ncatt_put(nc         = nc_ghcn,
                 varid      = netcdf_alt,
                 attname    = "positive",
                 attval     = "up",
                 prec       = NA,
                 verbose    = FALSE,
                 definemode = FALSE )
    
      ncatt_put(nc         = nc_ghcn,
                varid      = netcdf_alt,
                attname    = "description",
                attval     = "Elevation",
                prec       = NA,
                verbose    = FALSE,
                definemode = FALSE )
    
      ncatt_put(nc         = nc_ghcn,
                varid      = netcdf_lon,
                attname    = "standard_name",
                attval     = "longitude",
                prec       = NA,
                verbose    = FALSE,
                definemode = FALSE )
      
      
      ncatt_put(nc         = nc_ghcn,
                varid      = netcdf_lon,
                attname    = "axis",
                attval     = "X",
                prec       = NA,
                verbose    = FALSE,
                definemode = FALSE )
    
      ncatt_put(nc         = nc_ghcn,
                varid      = netcdf_lon,
                attname    = "description",
                attval     = "Longitude",
                prec       = NA,
                verbose    = FALSE,
                definemode = FALSE )
    
      ncatt_put(nc         = nc_ghcn,
                varid      = netcdf_lat,
                attname    = "standard_name",
                attval     = "latitude",
                prec       = NA,
                verbose    = FALSE,
                definemode = FALSE )
    
      ncatt_put(nc         = nc_ghcn,
                varid      = netcdf_lat,
                attname    = "description",
                attval     = "Latitude",
                prec       = NA,
                verbose    = FALSE,
                definemode = FALSE )
    
      ncatt_put(nc         = nc_ghcn,
                varid      = netcdf_lat,
                attname    = "axis",
                attval     = "Y",
                prec       = NA,
                verbose    = FALSE,
                definemode = FALSE )
      
      ncatt_put(nc         = nc_ghcn,
                varid      = "time",
                attname    = "description",
                attval     = "time",
                prec       = NA,
                verbose    = FALSE,
                definemode = FALSE )
      
      ncatt_put(nc         = nc_ghcn,
                varid      = "time",
                attname    = "bounds",
                attval     = "time_bnds",
                prec       = NA,
                verbose    = FALSE,
                definemode = FALSE )    
    
      ncatt_put(nc         = nc_ghcn,
                varid      = "time",
                attname    = "axis",
                attval     = "T",
                prec       = NA,
                verbose    = FALSE,
                definemode = FALSE )    
      
      ncatt_put(nc         = nc_ghcn,
                varid      = netcdf_time_bounds,
                attname    = "description",
                attval     = "Time Bounds",
                prec       = NA,
                verbose    = FALSE,
                definemode = FALSE )  
      
      ncatt_put(nc         = nc_ghcn,
                varid      = netcdf_time_bounds,
                attname    = "standard_name",
                attval     = "time",
                prec       = NA,
                verbose    = FALSE,
                definemode = FALSE )  
      
    
      
      
      
      
      
    
      if ("TMAX" %in% available_datafields) {
          ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_tmax,
                  attname    = "standard_name",
                  attval     = "air_temperature",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
          ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_tmax,
                  attname    = "cell_methods",
                  attval     = "time: maximum",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
          ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_tmax,
                  attname    = "description",
                  attval     = "2-m Maximum Daily Air Temperature",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
          ncatt_put(nc         = nc_ghcn,
             varid      = netcdf_tmax,
             attname    = "coordinates",
             attval     = "time latitude longitude altitude station_name",
             prec       = NA,
             verbose    = FALSE,
             definemode = FALSE )
    
      }
    
      if ("TMIN" %in% available_datafields) {
        ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_tmin,
                  attname    = "standard_name",
                  attval     = "air_temperature",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
        ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_tmin,
                  attname    = "cell_methods",
                  attval     = "time: minimum",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
        ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_tmin,
                  attname    = "description",
                  attval     = "2-m Minimium Daily Air Temperature",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
    
         ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_tmin,
            attname    = "coordinates",
            attval     = "time latitude longitude altitude station_name",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
      }
    
      if ("TAVG" %in% available_datafields) {
        ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_tavg,
                  attname    = "standard_name",
                  attval     = "air_temperature",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
        ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_tavg,
                  attname    = "cell_methods",
                  attval     = "time: mean",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
        ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_tavg,
                  attname    = "description",
                  attval     = "2-m Mean Daily Air Temperature",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
          ncatt_put(nc         = nc_ghcn,
                    varid      = netcdf_tavg,
                    attname    = "coordinates",
                    attval     = "time latitude longitude altitude station_name",
                    prec       = NA,
                    verbose    = FALSE,
                    definemode = FALSE )
      }
    
    
    
    
      if ("PRCP" %in% available_datafields) {
        ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_prcp,
                  attname    = "standard_name",
                  attval     = "precipitation_amount",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
        ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_prcp,
                  attname    = "cell_methods",
                  attval     = "time: sum",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
        ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_prcp,
                  attname    = "description",
                  attval     = "Daily Total Precipitation",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
          ncatt_put(nc         = nc_ghcn,
                   varid      = netcdf_prcp,
                   attname    = "coordinates",
                   attval     = "time latitude longitude altitude station_name",
                   prec       = NA,
                   verbose    = FALSE,
                   definemode = FALSE )
    
        }
    
      if ("SNOW" %in% available_datafields) {
        ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_snow,
                  attname    = "standard_name",
                  attval     = "thickness_of_snowfall_amount",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
        ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_snow,
                  attname    = "cell_methods",
                  attval     = "time: sum",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
        ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_snow,
                  attname    = "description",
                  attval     = "Daily Total Snowfall",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
          ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_snow,
                  attname    = "coordinates",
                  attval     = "time latitude longitude altitude station_name",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
      }
    
      if ("SNWD" %in% available_datafields) {
        ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_snwd,
                  attname    = "standard_name",
                  attval     = "surface_snow_thickness",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
        ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_snwd,
                  attname    = "cell_methods",
                  attval     = "time: point",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
        ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_snwd,
                  attname    = "description",
                  attval     = "Snow Depth on Surface",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
         ncatt_put(nc         = nc_ghcn,
                 varid      = netcdf_snwd,
                 attname    = "coordinates",
                 attval     = "time latitude longitude altitude station_name",
                 prec       = NA,
                 verbose    = FALSE,
                 definemode = FALSE )
    
      }
    
    
      if ("WESD" %in% available_datafields) {
        ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_wesd,
                  attname    = "standard_name",
                  attval     = "liquid_water_content_of_surface_snow",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
        ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_wesd,
                  attname    = "cell_methods",
                  attval     = "time: point",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
        ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_wesd,
                  attname    = "description",
                  attval     = "Liquid Snow Water Equivalent Depth on Surface",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
         ncatt_put(nc         = nc_ghcn,
                varid      = netcdf_wesd,
                attname    = "coordinates",
                attval     = "time latitude longitude altitude station_name",
                prec       = NA,
                verbose    = FALSE,
                definemode = FALSE )
    
    
      }
    
    
      if ("WSEF" %in% available_datafields) {
        ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_wesf,
                  attname    = "standard_name",
                  attval     = "snowfall_amount",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
        ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_wesf,
                  attname    = "cell_methods",
                  attval     = "time: sum",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
        ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_wesf,
                  attname    = "description",
                  attval     = "Liquid Snowfall Water Equivalent",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
         ncatt_put(nc         = nc_ghcn,
                varid      = netcdf_wesf,
                attname    = "coordinates",
                attval     = "time latitude longitude altitude station_name",
                prec       = NA,
                verbose    = FALSE,
                definemode = FALSE )
      }
    
    
      if ("AWND" %in% available_datafields) {
        ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_awnd,
                  attname    = "standard_name",
                  attval     = "wind_speed",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
        ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_awnd,
                  attname    = "cell_methods",
                  attval     = "time: mean",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
        ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_awnd,
                  attname    = "description",
                  attval     = "Mean Daily Wind Speed",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
         ncatt_put(nc         = nc_ghcn,
                varid      = netcdf_awnd,
                attname    = "coordinates",
                attval     = "time latitude longitude altitude station_name",
                prec       = NA,
                verbose    = FALSE,
                definemode = FALSE )
    
      }
    
      if ("AWDR" %in% available_datafields) {
        ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_awdr,
                  attname    = "standard_name",
                  attval     = "wind_from_direction",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
        ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_awdr,
                  attname    = "cell_methods",
                  attval     = "time: mean",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
        ncatt_put(nc         = nc_ghcn,
                  varid      = netcdf_awdr,
                  attname    = "description",
                  attval     = "Mean Daily Wind Origin Direction",
                  prec       = NA,
                  verbose    = FALSE,
                  definemode = FALSE )
    
         ncatt_put(nc         = nc_ghcn,
                varid      = netcdf_awdr,
                attname    = "coordinates",
                attval     = "time latitude longitude altitude station_name",
                prec       = NA,
                verbose    = FALSE,
                definemode = FALSE )
    
    
      }
    
    
    
    
      ncvar_put(nc      = nc_ghcn,
                varid   = netcdf_time_bounds,
                vals    = time_bounds,
                verbose = FALSE )
    
      ncvar_put(nc      = nc_ghcn,
                varid   = netcdf_lat,
                vals    = station_latitude,
                verbose = FALSE )
      
      remove(netcdf_lat,
             station_latitude)
    
      ncvar_put(nc      = nc_ghcn,
                varid   = netcdf_lon,
                vals    = station_longitude,
                verbose = FALSE )
      remove(netcdf_lon,
             station_longitude)
    
      ncvar_put(nc      = nc_ghcn,
                varid   = netcdf_alt,
                vals    = station_altitude,
                verbose = FALSE )
    
      ncvar_put(nc      = nc_ghcn,
                varid   = netcdf_stn,
                vals    = file_title_string,
                verbose = FALSE )
    
      remove(netcdf_alt,
             station_altitude)
    
    
      if ("TMAX" %in% available_datafields) {
        ncvar_put(nc      = nc_ghcn,
                  varid   = netcdf_tmax,
                  vals    = tmax,
                  verbose = FALSE )
        remove(netcdf_tmax,
               tmax)
        }
    
      if ("TMIN" %in% available_datafields) {
        ncvar_put(nc      = nc_ghcn,
                  varid   = netcdf_tmin,
                  vals    = tmin,
                  verbose = FALSE )
        remove(netcdf_tmin,
               tmin)
        }
    
    
      if ("TAVG" %in% available_datafields) {
        ncvar_put(nc      = nc_ghcn,
                  varid   = netcdf_tavg,
                  vals    = tavg,
                  verbose = FALSE )
        remove(netcdf_tavg,
               tavg)
        }
    
    
      if ("PRCP" %in% available_datafields) {
        ncvar_put(nc      = nc_ghcn,
                  varid   = netcdf_prcp,
                  vals    = prcp,
                  verbose = FALSE )
        remove(netcdf_prcp,
               prcp)
        }
    
      if ("SNOW" %in% available_datafields) {
        ncvar_put(nc      = nc_ghcn,
                  varid   = netcdf_snow,
                  vals    = (snow/1000.0),
                  verbose = FALSE )
        remove(netcdf_snow,
               snow)
        }
    
      if ("SNWD" %in% available_datafields) {
        ncvar_put(nc      = nc_ghcn,
                  varid   = netcdf_snwd,
                  vals    = (snwd/1000.0),
                  verbose = FALSE )
        remove(netcdf_snwd,
               snwd)
      }
    
      if ("WESD" %in% available_datafields) {
        ncvar_put(nc      = nc_ghcn,
                  varid   = netcdf_wesd,
                  vals    = wesd,
                  verbose = FALSE )
        remove(netcdf_wesd,
               wesd)
      }
    
      if ("WESF" %in% available_datafields) {
        ncvar_put(nc      = nc_ghcn,
                  varid   = netcdf_wesf,
                  vals    = wesf,
                  verbose = FALSE )
        remove(netcdf_wesf,
               wesf)
      }
    
    
      if ("AWND" %in% available_datafields) {
        ncvar_put(nc      = nc_ghcn,
                  varid   = netcdf_awnd,
                  vals    = awnd,
                  verbose = FALSE )
        remove(netcdf_awnd,
               awnd)
      }
    
    
      if ("AWDR" %in% available_datafields) {
        ncvar_put(nc      = nc_ghcn,
                  varid   = netcdf_awdr,
                  vals    = awdr,
                  verbose = FALSE )
        remove(netcdf_awdr,
               awdr)
      }
    
      nc_close( nc_ghcn )
    
      remove(nc_ghcn,
             netcdf_time_dim)
    

    
    
    
    
      remove(Date,
             Days_from_1970_01_01,
             available_datafields,
             file_title_string,
             filename_ghcn_label,
             filename_station_label,
             fill_value,
             ghcn_station_code,
             ncdc_end_yymmdd,
             ncdc_start_yymmdd,
             netcdf_available_variables,
             output_file_name,
             ncdc_id_code,
             netcdf_output_file_name,
             station_name_label)
      
      
    }
    

  
  }
}
