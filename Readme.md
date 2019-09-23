R routines to retrieve and reference occurrence records on HydroBASINS polygons to obtain occurrence ranges

Scripts order:

1) fishbase_reference_species_list.R: retrieve freshwater fish species scientific names available from fishbase and Tedesco et al., 2018 (including a synonyms check)

2) iucn_retrieve_synonyms.R: retrieve synonyms based on iucn database

3) compile_occurrence_records.R: collects, cleans and merges occurrence records from different datasets

4) reference2HB.R: references occurrence records to HydroBASINS polygons

5) ranges_diagnostics.R: runs diagnostics on obtained species ranges (compare to IUCN, viz)