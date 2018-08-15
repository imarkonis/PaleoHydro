# PaleoHydro

## Investigation of preconditions of major drought events

Run init.R to create directory structure. Directories data, results are not included in the repo as well as file source/paths.R.

### Names of variables:

p, p3, q, s are the deficit volumes for precip, 3-month precip, runoff and soil moisture, 

u_p, u_pet and u_t are the excess volume of precipitation, mean excess PET and mean excess temperature above 80th percentile

a* - are absolute values, n* - are standardized values

MX  - maximum deficit runoff for an event

t* - is the actual threshold

EVE indicates event

EID is the event ID - specific for each grid box (i.e. EID in the same year can vary over grid boxes)

Event is always when at least one of deficit volumes for p3, q, s is > 0, the period with all p3, q, s = 0 ends the event 
