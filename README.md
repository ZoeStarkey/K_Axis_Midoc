# K_axis_midoc

Repository for data, data processing scripts and analysis scripts for midwater trawl sampling on the Kerguelen Axis voyage

## Workflow
### Data processing and summary scripts
|                   Script                  |                                                                                                                             description                                                                                                                             |
|-------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| '01_import_net_logger_and_voyage_track.R' | imports data from midoc on-board logger (including CTD and records of when nets triggered) and from Aurora Australis nav files and extracts start and end locations and times for each shot and cod-end, calculates volumes swept, and assigns crepuscular category |
| '02_import_catch.R'                       | imports 'sample data' and 'codend data' and generates summaries of biomass of taxonomic groups caught per cod-end                                                                                                                                                   |
| '03_shot_summary_stats.R'                 | calculates basic summary info like average speed, distances covered and volumes swept for tows                                                                                                                                                                      |
| '04_md_acoustic.R'                        | calculates integrated NASC (acoustic signal) for each midoc cod-end                                                                                                                                                                                                 |
| '05_catch_env.R'                          | aligns midoc data with environmental data and produces midoc_stations_envdata table                                                                                                                                                                                 |

### Scripts specific to Kerguelen Plateau Symposium Extended Abstract
- '/KPS_symposium_extended_abstract/KPS_EA_fig1_map.R'
- '/KPS_symposium_extended_abstract/KPS_EA_catch_bubbles.R'
- '/KPS_symposium_extended_abstract/KPS_EA_models.R'

### Scripts specific to paper for Deep Sea Research Special Issue
- coming soon

## key outputs and column definitions
'midoc_stations_envdata' is produced as a 'master table' with many key variables for analyses of midoc data. Column definitions for this file are in the table below

|          Column          |                                                                                        Definition                                                                                       |
|--------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| midoc.stn                |                                                                                                                                                                                         |
| start_time               | time when midoc turned on for deployment                                                                                                                                                |
| end_time                 | time when midoc switched off at end of deployment                                                                                                                                       |
| lat_start                | latitude (from position on voyage track, matched for time) at start_time                                                                                                                |
| lon_start                | longitude (from position on voyage track, matched for time) at start_time                                                                                                               |
| lat_end                  | latitude (from position on voyage track, matched for time) at end_time                                                                                                                  |
| lon_end                  | longitude (from position on voyage track, matched for time) at end_time                                                                                                                 |
| trackdistm               | great circle distance along voyage track from start to end                                                                                                                              |
| mean_grnd_spd            | mean ship speed for tow                                                                                                                                                                 |
| midoc.n                  | midoc deployment number                                                                                                                                                                 |
| no.problems              | flag indicating whether deployment was 'clean' (no problems with codends firing etc)                                                                                                    |
| zone.notes               | notes on assignment of oceanographic zone                                                                                                                                               |
| DNC.visual               | Day/Night/crepuscular category (assigned by visually from plot with sunrise/sunset times overlaid on tows)                                                                              |
| notes for DNC comparison | notes for day/night/crepuscular categorization                                                                                                                                          |
| zone                     | oceanographic zone                                                                                                                                                                      |
| Tmin                     | water column minimum temperature (oceanographic variable from Sophie Bestley)                                                                                                           |
| Tmin_depth               | depth of Tmin (oceanographic variable from Sophie Bestley)                                                                                                                              |
| Tmax                     | water column maximum temperature (oceanographic variable from Sophie Bestley)                                                                                                           |
| SML                      | surface mixed layer depth (oceanographic variable from Sophie Bestley)                                                                                                                  |
| Smax                     | maximum salinity (oceanographic variable from Sophie Bestley)                                                                                                                           |
| O2_min                   | depth of Oxygen mimimum (oceanographic variable from Sophie Bestley)                                                                                                                    |
| days_since_melt          | days since ice melted at location (lat lon at start)                                                                                                                                    |
| distance_to_ice_m        | distance to nearest ice (>20% concentration, from lat lon at start, remotely sensed ice from raadtools )                                                                                |
| distance_to_edge_m       | distance to the "ice edge" (the outer contiguous 20% concentration contour, from lat lon at start, remotely sensed ice from raadtools; note that stations may be *inside* the ice edge) |
| sea_ice_conc             | sea ice concentration at location of start remotely sensed ice from raadtools (note that station may have 0% concentration but still be inside the ice edge, as defined above)          |
| chl_rs                   | mean concentration over the past 30 days, for a 1NM buffer around the lat-lon at start (remotely sensed chlorophyll, calculated using Johnson algorithm with raadtools) for             |
| intChl                   | in-situ integrated water column chlorophyll concentration, from Karen Westwood (*must discuss with Karen et al before using in papers*)                                                                                                                                                                                        |

'midoc_cod_ends_checked' has key summary information for each cod-end in

|     Column    |                                         definition                                         |
|---------------|--------------------------------------------------------------------------------------------|
| midoc.stn     | midoc deployment number                                                                    |
| CE            | cod end (1 to 6)                                                                           |
| start_time    | start time for cod end (UTC)                                                               |
| end_time      | end time for cod end (UTC)                                                                 |
| lat_start     | latitude (from position on voyage track, matched for time) at start                        |
| lon_start     | longitude (from position on voyage track, matched for time) at start                       |
| lat_end       | codend end position (as for start)                                                         |
| lon_end       | codend end position (as for start)                                                         |
| trackdistm    | horizontal distance, in m, traveled along voyage track during codend                       |
| mean_grnd_spd | mean speed over surface of ship during codend                                              |
| min.dep       | minimum depth of net for codend                                                            |
| max.dep       | maximum depth of net for codend                                                            |
| mid.dep       | mid depth for codend                                                                       |
| swept_m3      | volume swept in cubic metres, calculated as trackdistm * 188 (nominal area of mouth of net) |


## Dependencies
- '/source data/midoc_acoustic/' is omitted from git repository due to its size; accessible [here](https://www.dropbox.com/sh/m2ozo4oxbrwsvwe/AACwVxMETeDggcz68QMCu2i_a?dl=0) 

## TODO
- documented in individual scripts for now