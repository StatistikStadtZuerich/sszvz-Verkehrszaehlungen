# sszvz 0.1.2

* Bugfix: allow additional label as vehicle type

# sszvz 0.1.1

* Save timestamps where we only need the hours as a hms object from the hms package (e.g. "00:00:00") and use this for tagesgang plot

# sszvz 0.1.0

* Initial version for shiny apps with two plots.
* Reading functions: Read bike and car counts for the city of Zurich from the OGD portal in terms of parquet files to have all data from one file; sum bike counts to hourly values.
* Wrangling functions: Wrangle bike and car counts to aggregate data for a development of the median daily count over the years as well as the median hourly count over the course of a day.
* Plotting functions: Create plots of the counting locations as well as the wrangled data. 
