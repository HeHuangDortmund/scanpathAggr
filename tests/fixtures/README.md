# Scanpath aggregation regression baseline

`rgeos-0.6-4.rds` contains output from the unmodified scanpathAggr commit
`3ea5c0e051e58ddd049d6216d4554dfb116475ce` installed on Windows with R 4.6.2.
The reference used the conda-forge build of rgeos 0.6-4 with GEOS 3.13.0,
dbscan 1.3.0, concaveman 1.2.0, data.table 1.18.6.1, and sp 2.2-4.

For each bundled dataset, the reference ran `scanpathAggr(input, 3)`
and `scanpathAggr(input, 3, simplify = TRUE)`. The test compares fixation
coordinates in pixels and the area of each AOI's symmetric difference in
square pixels. This allows floating-point differences and changes in polygon
vertex order while checking the geometry. The tests load the recorded
results directly and run with the current package dependencies.
