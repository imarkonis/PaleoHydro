## Investigation of preconditions of major drought events 

### Script info

After forking the repo, run `init.R` to create directory structure and clean data for analysis. Note that directories `data` and `results` are not included in the repo.

`data_prep` creates and changes variable names (see *Variable names* below). Determines drought events.

`space_aid.R` helps to determine the location of a given `PT_ID`.

`plot_cumulative.R` examples for plotting the cumulative sums and deficit volumes of a single or median of multiple grid cells for drought events and a certain period before/after. 

`single_year_events.R` older approach; compares the grid cell distributions (as in `results/figs/distributions`) for vegetation period droughts. (there is some bug to fix). 

`build_som.R` application of self-organizing map algorithm.

`build_som_2018.R` similar to previous but only for the first 7 months of year (2018 comparison).

`som_analysis.R` analysis of the classification results. 

`som_changes.R` presentation of the most significant changes in clusters during the last 100 years. 

### Variables names 

In file `mstat_rQ20_rs_len0_met_001.Rds` the following notation is uesd:

`p`, `p3`, `q`, `s` are the deficit volumes for precip, 3-month precip, runoff and soil moisture, 

`u_p`, `u_pet` and `u_t` are the excess volume of precipitation, mean excess PET and mean excess temperature above 80th percentile

`a*` are absolute values, `n*` are standardized values

`MX` is maximum deficit runoff for an event

`t*` is the actual threshold

`EVE` indicates event

`EID` is the event ID - specific for each grid box (i.e. `EID` in the same year can vary over grid boxes)
`EID` is not unique for each event - serial number of event at each grid cell (below q20)

Event is always when at least one of deficit volumes for `p3`, `q`, `s` is > 0, the period with all `p3`, `q`, `s` = 0 ends the event. 

In file `mstat_short_met1.R`, used in all analyses, **variable names change** as follows:

`p`, `p3`, `q`, `s` are the absolute values for precip, 3-month precip, runoff and soil moisture and
`dv_p`, `dv_p3`, `dv_q`, `dv_s` are the deficit volumes for precip, 3-month precip, runoff and soil moisture, 
`ev_t`, `ev_pet` are the excess volumes for temperature and pet

### Source functions

In `functions.R`:

`get_periods`, `get_periods_par` extract the period of drought for an individual point, including the period before its beginning and after its ending.  

`cumsum_events` adds a new column with the cumulative sum of each variable anomaly for each period.



In `graphics.R`:

### Datasets

Data used in the analyses can be found at:

https://owncloud.cesnet.cz/index.php/s/dqLxqzFLpySZsVe

https://owncloud.cesnet.cz/index.php/s/jVvX6z3JR4ocelZ

Fresh model data:

https://oc.ufz.de/index.php/s/shxr8xxFFolxrJ8
password: drought2018



