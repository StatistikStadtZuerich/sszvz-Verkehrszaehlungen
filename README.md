# sszVZ Verkehrszählungen

sszvz is an R package to read, wrangle and plot traffic count data from the city of Zurich. It is currently heavily geared towards the use in shiny applications from Statistik Stadt Zürich and does not provide more general features.

## Installation

The easiest way to get sszvz is to install it from this repo:

``` r
# install.packages("devtools")
devtools::install_github("StatistikStadtZuerich/sszvz")
```

Alternatively, download the files (by clicking 'Clone or download' / 'Download Zip'), extract it to any location on your computer, e.g. to your Desktop and then run:

``` r
remotes::install_local("<path_to_location>/sszvz-main")
```

## Version

To check your version of sszvz, run:

``` r
packageVersion("sszvz")
```

## Examples

### Data reading functions

#### Cars

Read the data from the car counting locations ("Motorisierter Individualverkehr", [as provided on the OGD portal](https://data.stadt-zuerich.ch/dataset/sid_dav_verkehrszaehlung_miv_od2031)):

``` r
read_miv_data()
```

#### Bikes

Simlarly, read the data from the bike counting locations ("Velozählstellen", [as provided on the OGD portal](https://data.stadt-zuerich.ch/dataset/ted_taz_verkehrszaehlungen_werte_fussgaenger_velo)). These data are summed up to hourly values and are already corrected (a correction factor is provided in the [location dataset](https://data.stadt-zuerich.ch/dataset/geo_standorte_der_automatischen_fuss__und_velozaehlungen))

``` r
read_velo_data()
```

In both cases, the parquet files are read rather than the yearly CSVs.

#### Other data

- Two functions can be used to read the data on the counting locations, `read_miv_standorte` and `read_velo_standorte`
- Two functions can be used to read those counting locations, but only return the ones that are still active: `read_miv_loc_current` and `read_velo_loc_current`
- `read_common_shapes` reads geodata for the "Stadtkreise", "Stadtquartiere" and the shape of the lake of Zurich within the city of Zurich; these shapes can be used by the plotting functions.
- `read_feiertage` reads the [OGD dataset](https://data.stadt-zuerich.ch/dataset/ssd_schulferien) on school and other holidays, keeping bank holidays; this can be used to categorise days to weekdays, Saturdays and Sundays/bank holidays. Data with bank holidays previous to what is available from OGD is saved as package data and combined with the OGD dataset to cover all years for which bike/car counts are available.

### Data wrangling functions

`prep_miv_data` and `prep_velo_data` are both wrappers around `prep_data`; these separate functions are needed as in the case of bike counting locations, it is possible that the directions within the same locations have been switched when a new counter was installed. `prep_velo_data` makes sure that the directions are consistent over time.

`prep_data` returns the following:

- a dataframe named `loc_dir` which can serve as a location/direction lookup
- a dataframe named `df_location` containing all location data (for bikes, this can include more than one row per location, as each new counter is added separately)
- a dataframe named `df_tagesgang` which contains median hourly counts of the last 365 days, aggregated by type of day (weekday/Saturday/Sunday or bank holiday), location, direction, and hour of the day
- a dataframe named `df_jahresentwicklung` which contains median daily counts aggregated by type of day (weekday/Saturday/Sunday or bank holiday), location, direction and year
- a list named `cards_info`, which in turn contains two dataframes; one to provide the number of vehicles on the last valid day by location and direction, and the other to provide the number of vehicles this year so far by location and direction

### Data plotting functions

The data plotting functions work with the output of the data-reading/-wrangling functions:

- `plot_tagesgang` takes the `df_tagesgang` output from the `prep_data` function, filtered to only contain one location and direction, and plots the data as a line chart over the course of a day
- `plot_jahresentwicklung` takes the `df_jahresentwicklung` output from the `prep_data` function, filtered to only contain one location and direction, and plots the development as a line chart over the years
- `plot_locations_stadtkreise` plots the counting locations on top of the "Stadtkreise"
- `plot_locations_stadtkarte` plots the counting locations on top of the city's raster map

## Developer Notes: Tests

Some tests use a predefined, saved dataset to run faster. This dataset is generated in `data-raw/generate_test_fixture.R` and saved in `tests/testthat/fixtures`. If the structure of the data or the output of the reading functions change, this dataset has to be re-generated.

Nevertheless, the tests still take quite some time and need internet access (to the OGD portal). There are times when the parquet files are not available for technical reasons (e.g. before noon); during these times, the tests will fail.
