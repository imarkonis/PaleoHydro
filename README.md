# PaleoHydro

## Investigation of preconditions of major drought events

After forking the repo, run init.R to create directory structure and prepare data for analysis. Note that directories data, results are not included in the repo as well as file source/paths.R.

Then run scripts get_events.R, preconditions.R and recovery.R.

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
