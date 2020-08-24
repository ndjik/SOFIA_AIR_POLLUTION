1\) Exploratory statistics
================

This document consists of the exploratory statistics for the
dissertation. That is, it includes: - reading in some data (which will
also be used for later analysis) - temporal
    analyses

### Libraries

``` r
library(sf)
```

    ## Linking to GEOS 3.8.1, GDAL 3.1.1, PROJ 6.3.1

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.1
    ## ✓ tidyr   1.1.1     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ─────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(raster)
```

    ## Loading required package: sp

    ## 
    ## Attaching package: 'raster'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract

``` r
library(geohashTools)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:raster':
    ## 
    ##     intersect, union

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(dplyr)
library(xts)
```

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## 
    ## Attaching package: 'xts'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     first, last

``` r
library(ggplot2)
library(tidyr)
```

# City Morphology

### (neighbourhood level)

Contains data about heating, demography and education. This data will be
used to: - crop data (as shows administrative boundaries of Sofia) In
further analysis, it will also be used to: - produce maps as contains
data on heating and the shapes of the neighbourhoods of Sofia.

It has a lot of variables, with each row representing one neighbourhood.

``` r
morpho <-st_read('city_morpho/GR_UNITS_20190122.shp')
```

``` r
head(morpho)
```

    ## Simple feature collection with 6 features and 122 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: 693314.1 ymin: 4719447 xmax: 698198.3 ymax: 4721723
    ## projected CRS:  WGS 84 / UTM zone 34N
    ##   OBJECTID                            RegName     Rajon NBuild_Y_ NJ16_eq_1_
    ## 1       44               Бистрица - резервата Панчарево         0          0
    ## 2       56                ж.г. София - вилидж Панчарево      2010          0
    ## 3       57      хидропарк Искър - част Герман Панчарево         0          0
    ## 4       58 Панчарево - Черешови градини изток Панчарево      1973          1
    ## 5       60       с. Панчарево, м. Детски град Панчарево      1973          0
    ## 6       62                     Парк Камбаните Панчарево         0          0
    ##   NJ16_eq_2_ NJ16_eq_3_ NJ17_eq_1_ NJ17_eq_3_ NJ17_eq_4_ NJ17_eq_6_ NJ17_eq_7_
    ## 1          0          0          0          0          0          0          0
    ## 2          1          4          0          1          0          0          0
    ## 3          0          0          0          0          0          0          0
    ## 4         17         15          1          2          0          9         15
    ## 5          4          8          0          0          1          0          2
    ## 6          0          0          0          0          0          0          0
    ##   NJ17_eq4i_ NJ17_eq_8_ NJ17_eq_9_ NN_HouseH_ NN_People_ Nj12_1_ Nj12_2_
    ## 1          0          0          0          0          0       0       0
    ## 2          0          0          4          5          4       2       0
    ## 3          0          0          0          0          0       0       0
    ## 4         24          0          6         33         82      12       5
    ## 5          3          2          7         12         12       7       0
    ## 6          0          0          0          0          0       0       0
    ##   Nj12_3_ Nj12_4_ Nj12_5_ Nmale_TOT Nfemale_TO Nage_0_14 Nage_15_24 Nage_25_34
    ## 1       0       0       0         0          0         0          0          0
    ## 2       3       0       0         3          1         1          1          0
    ## 3       0       0       0         0          0         0          0          0
    ## 4      10       2       4        42         40         9         13          5
    ## 5       1       3       1         8          4         2          0          3
    ## 6       0       0       0         0          0         0          0          0
    ##   Nage_35_44 Nage_45_54 Nage_55_64 Nage_65_ NCoB_BG_ NCoB_EU_ NCoBnonEU_
    ## 1          0          0          0        0        0        0          0
    ## 2          0          2          0        0        4        0          0
    ## 3          0          0          0        0        0        0          0
    ## 4         15         15         11       14       69        0          0
    ## 5          1          2          1        3        2        0         -1
    ## 6          0          0          0        0        0        0          0
    ##   Nedu1_sum_ Nedu2_sum_ Nedu3_sum_ Nedu4_sum_ Nedu5_sum_ UN_Length     UN_Area
    ## 1          0          0          0          0          0  1973.000    9793.192
    ## 2          0          0          0          0          0  4577.163  613139.204
    ## 3          0          0          0          0          0  5043.585  860679.317
    ## 4          4         19         25          7          0  4132.099  447445.372
    ## 5          0          3          0          0          0  6281.036 1163938.939
    ## 6          0          0          0          0          0  6999.185  607316.294
    ##    BUI_Length   BUI_Area    BUI_RZP TNum_POI TNum_BUI Nedu_All Nage_15_64
    ## 1   150.02561   286.5848   521.2391        0        5        0          0
    ## 2  8482.49564 25790.5314 50538.1841        3      158        0          3
    ## 3   768.24521  1733.9604  2191.6196        0       20        0          0
    ## 4  6622.39014 18496.6299 29320.0186       21      165       55         59
    ## 5 13233.71530 39755.2405 72177.3655       11      344        3          7
    ## 6    81.95279   108.0969   108.0969        0        4        0          0
    ##   F0_14_Perc F15_64_Per F65_Perc NJ17_4i_pe    GL_Dens  GL_OpSp   GL_DensRZP
    ## 1    0.00000    0.00000  0.00000    0.00000 2.92636734 97.07363 0.0532246355
    ## 2   25.00000   75.00000  0.00000    0.00000 4.20630931 95.79369 0.0824253021
    ## 3    0.00000    0.00000  0.00000    0.00000 0.20146416 99.79854 0.0025463835
    ## 4   10.97561   71.95122 17.07317   72.72727 4.13382975 95.86617 0.0655275938
    ## 5   16.66667   58.33333 25.00000   25.00000 3.41557785 96.58442 0.0620112990
    ## 6    0.00000    0.00000  0.00000    0.00000 0.01779911 99.98220 0.0001779911
    ##     Pop_SqKm Nedu1_perc Nedu2_perc Nedu3_perc Nedu4_5_pe BUI1_TN BUI1_Perc
    ## 1   0.000000   0.000000    0.00000    0.00000    0.00000       3  60.00000
    ## 2   6.523804   0.000000    0.00000    0.00000    0.00000     122  77.21519
    ## 3   0.000000   0.000000    0.00000    0.00000    0.00000       5  25.00000
    ## 4 183.262595   7.272727   34.54545   45.45455   12.72727      67  40.60606
    ## 5  10.309819   0.000000  100.00000    0.00000    0.00000     191  55.52326
    ## 6   0.000000   0.000000    0.00000    0.00000    0.00000       1  25.00000
    ##     BUI1_Area BUI1_AreaP    BUI1_RZP BUI1_RZPPe BUI2_TN BUI2_Perc BUI2_Area
    ## 1   234.65431   81.87954   469.30862   90.03711       0 0.0000000    0.0000
    ## 2 21993.43513   85.27717 43012.04213   85.10801       2 1.2658228 1955.7065
    ## 3   443.32826   25.56738   900.98752   41.11058       0 0.0000000    0.0000
    ## 4  6240.08678   33.73634 12897.23949   43.98783       1 0.6060606  316.9041
    ## 5 14389.16518   36.19439 27410.39074   37.97644       5 1.4534884 1066.2297
    ## 6    55.71955   51.54592    55.71955   51.54592       0 0.0000000    0.0000
    ##   BUI2_AreaP  BUI2_RZP BUI2_RZPPe BUI3_TN BUI3_Perc  BUI3_Area BUI3_AreaP
    ## 1   0.000000    0.0000   0.000000       0 0.0000000    0.00000 0.00000000
    ## 2   7.583041 5684.7522  11.248430       1 0.6329114   16.94261 0.06569313
    ## 3   0.000000    0.0000   0.000000       0 0.0000000    0.00000 0.00000000
    ## 4   1.713307  633.8082   2.161691       7 4.2424242 1423.67255 7.69692943
    ## 5   2.681985 3013.9935   4.175815       6 1.7441860  502.26127 1.26338380
    ## 6   0.000000    0.0000   0.000000       0 0.0000000    0.00000 0.00000000
    ##     BUI3_RZP BUI3_RZPPe BUI4_TN BUI4_Perc  BUI4_Area BUI4_AreaP   BUI4_RZP
    ## 1    0.00000 0.00000000       0  0.000000    0.00000  0.0000000     0.0000
    ## 2   16.94261 0.03352437       0  0.000000    0.00000  0.0000000     0.0000
    ## 3    0.00000 0.00000000       0  0.000000    0.00000  0.0000000     0.0000
    ## 4 2103.48775 7.17423743       2  1.212121   89.12062  0.4818209   226.7056
    ## 5  755.98887 1.04740436       9  2.616279 8671.55279 21.8123515 22888.0462
    ## 6    0.00000 0.00000000       0  0.000000    0.00000  0.0000000     0.0000
    ##   BUI4_RZPPe BUI5_TN BUI5_Perc BUI5_Area BUI5_AreaP BUI5_RZP BUI5_RZPPe BUI6_TN
    ## 1   0.000000       0  0.000000    0.0000   0.000000    0.000   0.000000       0
    ## 2   0.000000       0  0.000000    0.0000   0.000000    0.000   0.000000       1
    ## 3   0.000000       0  0.000000    0.0000   0.000000    0.000   0.000000       0
    ## 4   0.773211       2  1.212121  535.7263   2.896345 1505.637   5.135185       0
    ## 5  31.710836      28  8.139535 2935.0423   7.382781 4239.277   5.873416       3
    ## 6   0.000000       0  0.000000    0.0000   0.000000    0.000   0.000000       0
    ##   BUI6_Perc  BUI6_Area BUI6_AreaP   BUI6_RZP BUI6_RZPPe BUI7_TN  BUI7_Area
    ## 1 0.0000000    0.00000  0.0000000    0.00000 0.00000000       2   51.93047
    ## 2 0.6329114   45.29299  0.1756187   45.29299 0.08962132      25 1438.71552
    ## 3 0.0000000    0.00000  0.0000000    0.00000 0.00000000      15 1290.63209
    ## 4 0.0000000    0.00000  0.0000000    0.00000 0.00000000      49 7395.57926
    ## 5 0.8720930 3756.52861  9.4491407 4997.06539 6.92331364      67 4820.84587
    ## 6 0.0000000    0.00000  0.0000000    0.00000 0.00000000       3   52.37737
    ##   BUI7_AreaP   BUI7_RZP BUI7_RZPPe BUI8_TN BUI8_Area BUI8_AreaP  BUI8_RZP
    ## 1  18.120457   51.93047   9.962889       0    0.0000   0.000000    0.0000
    ## 2   5.578464 1438.71552   2.846789       7  340.4386   1.320014  340.4386
    ## 3  74.432618 1290.63209  58.889421       0    0.0000   0.000000    0.0000
    ## 4  39.983388 9060.02241  30.900466      14 1747.5358   9.447860 2078.5265
    ## 5  12.126315 5087.40579   7.048478      30 3464.8890   8.715553 3636.4723
    ## 6  48.454084   52.37737  48.454084       0    0.0000   0.000000    0.0000
    ##   BUI8_RZPPe BUI78_TN BUI78_Area BUI78_AreP   BUI78_RZP BUI78_RZPP BUI60_TN
    ## 1  0.0000000        2   51.93047  18.120457    51.93047   9.962889        0
    ## 2  0.6736266       32 1779.15417   6.898478  1779.15417   3.520416        1
    ## 3  0.0000000       15 1290.63209  74.432618  1290.63209  58.889421        0
    ## 4  7.0891034       63 9143.11504  49.431248 11138.54886  37.989570        0
    ## 5  5.0382447       97 8285.73491  20.841868  8723.87811  12.086723        0
    ## 6  0.0000000        3   52.37737  48.454084    52.37737  48.454084        0
    ##   BUI60_Area BUI60_RZP BUI60_RZPP BUI10_TN BUI10_Area BUI10_AreP BUI10_RZP
    ## 1    0.00000   0.00000 0.00000000        0          0          0         0
    ## 2   45.29299  45.29299 0.08962132        0          0          0         0
    ## 3    0.00000   0.00000 0.00000000        0          0          0         0
    ## 4    0.00000   0.00000 0.00000000        0          0          0         0
    ## 5    0.00000   0.00000 0.00000000        0          0          0         0
    ## 6    0.00000   0.00000 0.00000000        0          0          0         0
    ##   BUI10_RZPP BUI10RZPP_    BUI1_KINT                       geometry
    ## 1          0          0 4.792192e-02 MULTIPOLYGON (((694054.5 47...
    ## 2          0          0 7.015053e-02 MULTIPOLYGON (((696454.2 47...
    ## 3          0          0 1.046833e-03 MULTIPOLYGON (((698141.8 47...
    ## 4          0          0 2.882417e-02 MULTIPOLYGON (((697583.6 47...
    ## 5          0          0 2.354968e-02 MULTIPOLYGON (((696341.1 47...
    ## 6          0          0 9.174717e-05 MULTIPOLYGON (((696295.9 47...

# NOVEMBER THROUGH MARCH

### AirBulgaria/Luftdaten data

Contains PM2.5, PM10 and meteorological data for the whole of Bulgaria

``` r
read_csv("winterpol1920/airtube-data-BG-201911.csv.gz") -> nov
```

    ## Parsed with column specification:
    ## cols(
    ##   time = col_datetime(format = ""),
    ##   geohash = col_character(),
    ##   P1 = col_double(),
    ##   P2 = col_double(),
    ##   temperature = col_double(),
    ##   humidity = col_double(),
    ##   pressure = col_double()
    ## )

``` r
nov %>% write_csv("airtube-data-BG-201911.csv")

read_csv("winterpol1920/airtube-data-BG-201912.csv.gz") -> dec
```

    ## Parsed with column specification:
    ## cols(
    ##   time = col_datetime(format = ""),
    ##   geohash = col_character(),
    ##   P1 = col_double(),
    ##   P2 = col_double(),
    ##   temperature = col_double(),
    ##   humidity = col_double(),
    ##   pressure = col_double()
    ## )

``` r
dec %>% write_csv("airtube-data-BG-201912.csv")

read_csv("winterpol1920/airtube-data-BG-202001.csv.gz") -> jan
```

    ## Parsed with column specification:
    ## cols(
    ##   time = col_datetime(format = ""),
    ##   location_id = col_character(),
    ##   geohash = col_character(),
    ##   P1 = col_double(),
    ##   P2 = col_double(),
    ##   temperature = col_double(),
    ##   humidity = col_double(),
    ##   pressure = col_double()
    ## )

``` r
jan %>% write_csv("airtube-data-BG-202001.csv")

read_csv("winterpol1920/airtube-data-BG-202002.csv.gz") -> feb
```

    ## Parsed with column specification:
    ## cols(
    ##   time = col_datetime(format = ""),
    ##   location_id = col_character(),
    ##   geohash = col_character(),
    ##   P1 = col_double(),
    ##   P2 = col_double(),
    ##   temperature = col_double(),
    ##   humidity = col_double(),
    ##   pressure = col_double()
    ## )

``` r
feb %>% write_csv("airtube-data-BG-202002.csv")

read_csv("winterpol1920/airtube-data-BG-202003.csv.gz") -> mar
```

    ## Parsed with column specification:
    ## cols(
    ##   time = col_datetime(format = ""),
    ##   location_id = col_character(),
    ##   geohash = col_character(),
    ##   P1 = col_double(),
    ##   P2 = col_double(),
    ##   temperature = col_double(),
    ##   humidity = col_double(),
    ##   pressure = col_double()
    ## )

``` r
mar %>% write_csv("airtube-data-BG-202003.csv")
```

``` r
head(mar)
```

    ## # A tibble: 6 x 8
    ##   time                location_id geohash    P1    P2 temperature humidity
    ##   <dttm>              <chr>       <chr>   <dbl> <dbl>       <dbl>    <dbl>
    ## 1 2020-03-01 00:00:00 5097        sxey0g…    24    19           4       79
    ## 2 2020-03-01 00:00:00 9781        sx8dgj…    83    47           2       67
    ## 3 2020-03-01 00:00:00 5999        sxey0f…    44    22           3       98
    ## 4 2020-03-01 00:00:00 10473       sx9705…     4     2           1       69
    ## 5 2020-03-01 00:00:00 2894        sx3xjb…    11    10           2       86
    ## 6 2020-03-01 00:00:00 27817       sx86yy…    23    11           0       67
    ## # … with 1 more variable: pressure <dbl>

``` r
# remove location ID from jan and feb and mar (extra feature present in 2020 data)
jan <-  subset(jan, select = -c(location_id))
feb <- subset(feb, select = -c(location_id))
mar <- subset(mar, select = -c(location_id))

# Merging data into 'd'
d <- rbind(nov, dec)
d <- rbind(d, jan)
d <- rbind(d, feb)
d <- rbind(d, mar)

# Converting geohashed coordinated into longlat 
coords <- gh_decode(d$geohash)
d$lat <- coords$latitude
d$long <- coords$longitude

# Focusing data around Sofia 
sofia <- d[ which(d$lat< 42.79
                  & d$lat > 42.61), ]
sofia <- sofia[ which(sofia$long< 23.46
                      & sofia$long > 23.20), ]

# Transforming crs of pollution data to that of City Morphology 
sofia <- st_as_sf(sofia, coords = c("long", "lat"), crs = 4326) # projecting data because data is not projected 
sofia<-st_transform(sofia, crs = 32634) #transforming to crs of City Morphology 

# Cutting air pollution that is outside of polygon
sofia <-st_crop(sofia, morpho)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
# Clean the pollution data 
sofia2 <- sofia[ which(sofia$P2<500 & sofia$P2 > 2 & sofia$humidity < 70 & sofia$humidity > 20
                       & sofia$pressure > 86000 & sofia$pressure < 110000 & sofia$temperature > -10 &sofia$temperature < 25), ] 

# Counting the frequency of each sensor (how many obs each sensor has). Data stored in a df called 'counting'
counting <- data.frame(table(sofia2$geohash))
#counting<- count(sofia2df, geohash)
kept <- counting[ which(counting$Freq>119), ] # keeping ones, that worked at least 5 days (continusouly or discontinuously) (119 observations, 24*5 = 120) 
# there probably is a different way with lubridate 
names(kept)[names(kept) == "Var1"] <- "geohash"

# Merge into pollution from nov through to mar
pol_nm <- merge(sofia2, kept, all=F) 
```

### Daily mean

``` r
# Daily mean 
pol_nm$date <- date(pol_nm$time)
dailymean_nm <- pol_nm %>% 
  group_by(date) %>% 
  summarise_if(is.numeric, mean)
p <- ggplot(dailymean_nm, aes(x=date, y=P2)) +
  geom_line() + 
  xlab("winter 2019/2020") + 
  theme_minimal()
```

``` r
p
```

![](1_exploratory_statistics_files/figure-gfm/Nov%20Mar%205-1.png)<!-- -->

### Hourly mean

``` r
pol_nm$hour <- hour(pol_nm$time)
hourlymean_nm <-pol_nm  %>% 
  group_by(hour) %>% 
  summarise_if(is.numeric, mean)
# will plot later
```

# December-January (hourly mean only)

Will apply the same methods as above, and will plot later

``` dec
d <- rbind(dec, jan)

coords <- gh_decode(d$geohash)
d$lat <- coords$latitude
d$long <- coords$longitude

# Focusing data around Sofia 
sofia <- d[ which(d$lat< 42.79
                  & d$lat > 42.61), ]
sofia <- sofia[ which(sofia$long< 23.46
                      & sofia$long > 23.20), ]

# Transforming crs of pollution data to that of City Morphology 
sofia <- st_as_sf(sofia, coords = c("long", "lat"), crs = 4326) # projecting data because data is not projected 
sofia<-st_transform(sofia, crs = 32634) #transforming to crs of city morphology 

# Cutting air pollution that is outside of polygon
sofia <-st_crop(sofia, morpho)

# Removing high and low values of PM2.5 and for observations when high humidity
sofia2 <- sofia[ which(sofia$P2<500 & sofia$P2 > 2 & sofia$humidity < 70 & sofia$humidity > 20
                       & sofia$pressure > 86000 & sofia$pressure < 110000 & sofia$temperature > -10 &sofia$temperature < 25), ] 

# Counting the frequency of each sensor (how many obs each sensor has). Data stored in a df called 'counting'
counting <- data.frame(table(sofia2$geohash))
#counting<- count(sofia2df, geohash)
kept <- counting[ which(counting$Freq>119), ]
# keeping ones, that worked at least 5 days 
names(kept)[names(kept) == "Var1"] <- "geohash"
# merge 
pol_dj<- merge(sofia2, kept, all=F)

pol_dj$hour <- hour(pol_dj$time) 
hourlymean_dj <- pol_dj  %>% 
  group_by(hour) %>% 
  summarise_if(is.numeric, mean)
# will plot later
```

# November-February-March (hourly mean only)

Same methods as above and will plot
later

``` nov
# Calculating the hourly average for the two most polluted months: Dec and Jan: 
d <- rbind(nov, feb)
d <- rbind(d, mar)
# Converting geohashed coordinated into longlat 
coords <- gh_decode(d$geohash)
d$lat <- coords$latitude
d$long <- coords$longitude

# Focusing data around Sofia 
sofia <- d[ which(d$lat< 42.79
                  & d$lat > 42.61), ]
sofia <- sofia[ which(sofia$long< 23.46
                      & sofia$long > 23.20), ]

# Transforming crs of pollution data to that of City Morphology 
sofia <- st_as_sf(sofia, coords = c("long", "lat"), crs = 4326) # projecting data because data is not projected 
sofia<-st_transform(sofia, crs = 32634) #transforming to crs of city morphology 

# Cutting air pollution that is outside of polygon

sofia <-st_crop(sofia, morpho)

# Removing high and low values of PM2.5 and for observations when high humidity
sofia2 <- sofia[ which(sofia$P2<500 & sofia$P2 > 2 & sofia$humidity < 70 & sofia$humidity > 20
                       & sofia$pressure > 86000 & sofia$pressure < 110000 & sofia$temperature > -10 &sofia$temperature < 25), ] 


# Counting the frequency of each sensor (how many obs each sensor has). Data stored in a df called 'counting'
counting <- data.frame(table(sofia2$geohash))
#counting<- count(sofia2df, geohash)
kept <- counting[ which(counting$Freq>119), ]
# keeping ones, that worked at least 2 days 
names(kept)[names(kept) == "Var1"] <- "geohash"
# merge 
pol_nfm<- merge(sofia2, kept, all=F) 

pol_nfm$hour <- hour(pol_nfm$time)
hourlymean_nfm <- pol_nfm  %>% 
  group_by(hour) %>% 
  summarise_if(is.numeric, mean)
# plot later
```

# Hourly means plot

``` hourly
hour<- hourlymean_nm$hour
nm_means <- hourlymean_nm$P2
dj_means <- hourlymean_dj$P2
nfm_means <- hourlymean_nfm$P2

df = data.frame(hour, nm_means, dj_means, nfm_means)

dfplot <- df %>% gather(key, value, -hour)
```

``` hourly
ggplot(dfplot, mapping = aes(x = hour, y = value, color = key) ) + geom_line() + ylab('PM2.5') + xlab('Time') + theme_minimal() 
```

# Number of sensors in winter 2019-20

There 357 sensors (after cleaning), as 357 rows in the aggregated
dataframe.

``` nbr
sensors_agg_nm <- aggregate(pol_nm, list(pol_nm$geohash), mean)
```
