# occ2range4fish

R routines to retrieve and reference occurrence records on HydroBASINS polygons to obtain occurrence ranges


1. **fishbase_species_list_and_synonyms.R** Retrieve freshwater fish species scientific names available from fishbase and Tedesco et al., 2018 (including a synonyms check)

2. **iucn_species_list_and_synonyms.R** Retrieve synonyms based on iucn database

3. **compile_occurrence_records.R** Collects, cleans and merges occurrence records from different datasets

4. **reference2HB.R** References occurrence records to HydroBASINS polygons

5. **ranges_diagnostics.R** Runs diagnostics on obtained species ranges (compare to IUCN, viz)

6. **merge_polygons.R** Merges the referenced occurrences in MULTIPOLYGON features (one multipoly per species) and exports a GPKG feature collection
