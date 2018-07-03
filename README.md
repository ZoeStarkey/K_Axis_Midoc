# K_axis_midoc

Repository for data, data processing scripts and analysis scripts for midwater trawl sampling on the Kerguelen Axis voyage

## Workflow
### Data processing and summary scripts
- '01_import_net_logger_and_voyage_track.R' imports data from midoc on-board logger (including CTD and records of when nets triggered) and from Aurora Australis nav files and extracts start and end locations and times for each shot and cod-end, calculates volumes swept, and assigns crepuscular category.
- '02_import_catch.R': imports 'sample data' and 'codend data' and generates summaries of biomass of taxonomic groups caught per cod-end
- '03_shot_summary_stats.R': calculates basic summary info like average speed, distances covered and volumes swept for tows
- '04_md_acoustic.R': calculates integrated NASC (acoustic signal) for each midoc cod-end
- '05_catch_env.R'

### Scripts specific to Kerguelen Plateau Symposium Extended Abstract
- '/KPS_symposium_extended_abstract/KPS_EA_fig1_map.R'
- '/KPS_symposium_extended_abstract/KPS_EA_catch_bubbles.R'
- '/KPS_symposium_extended_abstract/KPS_EA_models.R'

### Scripts specific to paper for Deep Sea Research Special Issue
- coming soon

## Dependencies
- '/source data/midoc_acoustic/' is omitted from git repository due to its size; accessible [here](https://www.dropbox.com/sh/m2ozo4oxbrwsvwe/AACwVxMETeDggcz68QMCu2i_a?dl=0) 

## TODO
- documented in individual scripts for now