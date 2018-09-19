# PaleoHydro

## Investigation of preconditions of major drought events

After forking the repo, run init.R to create directory structure and prepare data for analysis. Note that directories 'data' and 'results' are not included in the repo.

In 'preconditions.R' there is the main code for plotting the cumulative anomalies and normalised deficit volumes of a single or median of multiple grid cells. The resulting comparison scripts can be found in 'bin/2018'.

'event_properties.R' is an on-going investigation of potential changes in the properties drought events

An older approach can be found in 'single_year_events.R', with density plots for the preconditions of different single-year events

To see the position of PT_ID use 'space_aid.R'.

### Names of variables in file mstat_rQ20_rs_len0_met_001.Rds:

p, p3, q, s are the deficit volumes for precip, 3-month precip, runoff and soil moisture, 

u_p, u_pet and u_t are the excess volume of precipitation, mean excess PET and mean excess temperature above 80th percentile

a* - are absolute values, n* - are standardized values

MX  - maximum deficit runoff for an event

t* - is the actual threshold

EVE indicates event

EID is the event ID - specific for each grid box (i.e. EID in the same year can vary over grid boxes)
EID is not unique for each event - serial number of event at each grid cell (below q20) EID

Event is always when at least one of deficit volumes for p3, q, s is > 0, the period with all p3, q, s = 0 ends the event 
### In mstat_short_met1 used in all analyses here the variable names change as follows:
p, p3, q, s are the absolute values for precip, 3-month precip, runoff and soil moisture and
dv_p, dv_p3, dv_q, dv_s are the deficit volumes for precip, 3-month precip, runoff and soil moisture, 
ev_t, ev_pet are the excess volumes for temperature and pet

Data can be found at:

https://owncloud.cesnet.cz/index.php/s/dqLxqzFLpySZsVe

https://owncloud.cesnet.cz/index.php/s/jVvX6z3JR4ocelZ
