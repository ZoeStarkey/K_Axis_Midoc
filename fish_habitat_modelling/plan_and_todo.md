Plan for Walters, Trebilco, Hindell et al. fish habitat modelling paper

- Data from: K-axis; HIPPIES; French voyages

- for each sampling station collate:
    - biomass and abundance for K. andersonii vs. all other myctophids
	- sample station detais including:
         - latlon
         - depth range (max of shot)
         - TOD (maybe a new solar angle variable)
         - volume swept: could be used as an offset 
	

# TODO:
	- RT: pull together biomass and station data for K-axis and HIPPIES
    - AW: pull together French data
    - Hindell: envdata from raadtools

# RELEVANT FILES (already in this repo)
    - /K_axis_midoc/derived data/codend_fish_biomass.rds: summaries of biomass per cod-end for main fish groups
    - /GitHub/K_axis_midoc/derived data/midoc_stations_envdata.rda: has satellite ice and chl metrics (from raadtools) along with in-situ oceanographic variables (from Bestley) and integrated water-column chlorophyll (Westwood) for each station (full column definitions in main README for repository)
    - /K_axis_midoc/source data/HIPPIES DB/ 

# SCRIPTS
    - 01_get_kax_fish_data.R


# plan for models
- compare models with bm/n per volume swept vs. total bm/n with volume swept offset term.