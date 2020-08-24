3\) Dataframe for regression
================

This prepares the dataframe that will be used for the LUR, by reading
the data from different sources. *This code is COMPUTATIONALLY
INTENSIVE*.

NB: need the ‘sensors\_agg\_dj’ (from file 2) and ‘morpho’ (from file 1)

# Data preparation

Copying the dec-jan pollution data + creating buffers

``` r
winterpol <- sensors_agg_dj

buf25 <- st_buffer(winterpol, dist = 25) 
buf25$buf_area25 <- st_area(buf25)

buf50 <- st_buffer(winterpol, dist = 50) 
buf50$buf_area50 <- st_area(buf50)

buf100 <- st_buffer(winterpol, dist = 100) 
buf100$buf_area100 <- st_area(buf100)

buf300 <- st_buffer(winterpol, dist = 300) 
buf300$buf_area300 <- st_area(buf300)

buf500 <- st_buffer(winterpol, dist = 500) 
buf500$buf_area500 <- st_area(buf500)

buf1000 <- st_buffer(winterpol, dist = 1000)  
buf1000$buf_area1000 <- st_area(buf1000)
```

# OSM land use data

This section intersects the pollution point data (with buffers) to the
land use
data

``` r
BGlu_sf <- st_read("OSM_data.shp/gis_osm_landuse_a_free_1.shp", stringsAsFactors = FALSE)
BGlu_sf<-st_transform(BGlu_sf, crs = 32634) #transforming CRS of OSM LU data
sofialu_sf<-st_crop(BGlu_sf, morpho)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

### Aggregating land use categories

``` r
# retail = commercial 
sofialu_sf$fclass[sofialu_sf$fclass=='retail'] <- 'commercial'

# scrub = scrub and grass
# grass = scrub and grass
sofialu_sf$fclass[sofialu_sf$fclass=='scrub'] <- 'urban green'
sofialu_sf$fclass[sofialu_sf$fclass=='grass'] <- 'urban green'

# Forest = urban green
# Park = urban green
# Cemetery = urban green
# Meadow = urban green 
# heath = urban green
sofialu_sf$fclass[sofialu_sf$fclass=='forest'] <- 'urban green'
sofialu_sf$fclass[sofialu_sf$fclass=='park'] <- 'urban green'
sofialu_sf$fclass[sofialu_sf$fclass=='cemetery'] <- 'urban green'
sofialu_sf$fclass[sofialu_sf$fclass=='meadow'] <- 'urban green'
sofialu_sf$fclass[sofialu_sf$fclass=='heath'] <- 'urban green'

# Also: calculating the areas of the different land uses (for future use)
sofialu_sf$lu_areas <- st_area(sofialu_sf)
```

### 100m buffer (radius = 100m)

``` r
int100lu <- st_intersection(buf100, sofialu_sf) 
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
int100lu$lu_areas100 <- st_area(int100lu) #calculating the areas of each land use

# removing columns we do not need 
myvars <- c("Group.1", "fclass", "lu_areas100")
int100lu <- int100lu[myvars]

# Aggregating by land use by sensor as after the intersection, one sensor can have multiple commercial areas (that are counted as separate as they do not 'touch' each other)

# commercial areas 
com100 <- int100lu[ which(int100lu$fclass=='commercial'), ] 
com100 <- com100 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

# industrial 
ind100 <- int100lu[ which(int100lu$fclass=='industrial'), ]
ind100 <- ind100 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

# residential
res100 <-int100lu[ which(int100lu$fclass=='residential'), ] 
res100 <- res100 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

# scrub and grass
gre100 <- int100lu[ which(int100lu$fclass=='urban green'), ] 
gre100 <- gre100 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

# renaming the area columns (with according buffer information)
names(com100)[2] <- "com_area100"
names(ind100)[2] <- "ind_area100"
names(res100)[2] <- "res_area100"
names(gre100)[2] <- "gre_area100"

# removing geometries for merging
st_geometry(com100) <- NULL 
st_geometry(ind100) <- NULL 
st_geometry(res100) <- NULL 
st_geometry(gre100) <- NULL 

# Merge to original sensor data 
winterpol2 <- merge(winterpol, com100, by = 'Group.1', all= TRUE) # to keep unmatched rows 
winterpol2 <- merge(winterpol2, ind100, by = 'Group.1', all= TRUE)
winterpol2 <- merge(winterpol2, res100, by = 'Group.1', all= TRUE)
winterpol2 <- merge(winterpol2, gre100, by = 'Group.1', all= TRUE)

# removing units 
winterpol2$buf_area100 <- as.vector(winterpol2$buf_area100)
winterpol2$com_area100 <- as.vector(winterpol2$com_area100)
winterpol2$ind_area100 <- as.vector(winterpol2$ind_area100)
winterpol2$res_area100 <- as.vector(winterpol2$res_area100)
winterpol2$gre_area100 <- as.vector(winterpol2$gre_area100)

# removing the 0s
winterpol2[is.na(winterpol2)] <- 0
```

### 300m buffer

``` r
int300lu <- st_intersection(buf300, sofialu_sf) 
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
# intersecting the pollution point data with buffers to the land use data

int300lu$lu_areas300 <- st_area(int300lu) 
# removing columns we do not need 
myvars <- c("Group.1", "fclass", "lu_areas300")
int300lu <- int300lu[myvars]

# commercial areas 
com300 <- int300lu[ which(int300lu$fclass=='commercial'), ] 
com300 <- com300 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

# industrial 
ind300 <- int300lu[ which(int300lu$fclass=='industrial'), ] 
ind300 <- ind300 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

# residential
res300 <-int300lu[ which(int300lu$fclass=='residential'), ] 
res300 <- res300 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

# scrub and grass
gre300 <- int300lu[ which(int300lu$fclass=='urban green'), ]
gre300 <- gre300 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

# renaming the area columns 
names(com300)[2] <- "com_area300"
names(ind300)[2] <- "ind_area300"
names(res300)[2] <- "res_area300"
names(gre300)[2] <- "gre_area300"

# removing geometries for merging
st_geometry(com300) <- NULL 
st_geometry(ind300) <- NULL 
st_geometry(res300) <- NULL 
st_geometry(gre300) <- NULL 

# Merge to original sensor data 
winterpol2 <- merge(winterpol2, com300, by = 'Group.1', all= TRUE) # to keep unmatched rows 
winterpol2 <- merge(winterpol2, ind300, by = 'Group.1', all= TRUE)
winterpol2 <- merge(winterpol2, res300, by = 'Group.1', all= TRUE)
winterpol2 <- merge(winterpol2, gre300, by = 'Group.1', all= TRUE)

# replacing NAs with 0s 
winterpol2[is.na(winterpol2)] <- 0

# removing units 
winterpol2$buf_area300 <- as.vector(winterpol2$buf_area300)
winterpol2$com_area300 <- as.vector(winterpol2$com_area300)
winterpol2$ind_area300 <- as.vector(winterpol2$ind_area300)
winterpol2$res_area300 <- as.vector(winterpol2$res_area300)
winterpol2$gre_area300 <- as.vector(winterpol2$gre_area300)
```

### 500m buffer

``` r
int500lu <- st_intersection(buf500, sofialu_sf) #intersecting
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
int500lu$lu_areas500 <- st_area(int500lu) #calculating area of each land use 

# removing columns we do not need 
myvars <- c("Group.1", "fclass", "lu_areas500")
int500lu <- int500lu[myvars]

# commercial areas 
com500 <- int500lu[ which(int500lu$fclass=='commercial'), ] #100 
com500 <- com500 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) #76 sensors 

# industrial 
ind500 <- int500lu[ which(int500lu$fclass=='industrial'), ] #60
ind500 <- ind500 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) #46

# residential
res500 <-int500lu[ which(int500lu$fclass=='residential'), ] #396
res500 <- res500 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) #302

# scrub and grass
gre500 <- int500lu[ which(int500lu$fclass=='urban green'), ] #503
gre500 <- gre500 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) #176


# renaming the area columns 
names(com500)[2] <- "com_area500"
names(ind500)[2] <- "ind_area500"
names(res500)[2] <- "res_area500"
names(gre500)[2] <- "gre_area500"

# removing geometries for merging
st_geometry(com500) <- NULL 
st_geometry(ind500) <- NULL 
st_geometry(res500) <- NULL 
st_geometry(gre500) <- NULL 

# Merge to original sensor data 
winterpol2 <- merge(winterpol2, com500, by = 'Group.1', all= TRUE) # to keep unmatched rows 
winterpol2 <- merge(winterpol2, ind500, by = 'Group.1', all= TRUE)
winterpol2 <- merge(winterpol2, res500, by = 'Group.1', all= TRUE)
winterpol2 <- merge(winterpol2, gre500, by = 'Group.1', all= TRUE)

# replacing NAs with 0s 
winterpol2[is.na(winterpol2)] <- 0

# removing units 
winterpol2$buf_area500 <- as.vector(winterpol2$buf_area500)
winterpol2$com_area500 <- as.vector(winterpol2$com_area500)
winterpol2$ind_area500 <- as.vector(winterpol2$ind_area500)
winterpol2$res_area500 <- as.vector(winterpol2$res_area500)
winterpol2$gre_area500 <- as.vector(winterpol2$gre_area500)
```

\#\#\#1000m
    buffer

``` r
int1000lu <- st_intersection(buf1000, sofialu_sf) # intersecting data
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
int1000lu$lu_areas1000 <- st_area(int1000lu) #calculating the areas of each land use

# removing columns we do not need 
myvars <- c("Group.1", "fclass", "lu_areas1000")
int1000lu <- int1000lu[myvars]

# commercial areas 
com1000 <- int1000lu[ which(int1000lu$fclass=='commercial'), ] #100 
com1000 <- com1000 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) #76 sensors 

# industrial 
ind1000 <- int1000lu[ which(int1000lu$fclass=='industrial'), ] #60
ind1000 <- ind1000 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) #46

# residential
res1000 <-int1000lu[ which(int1000lu$fclass=='residential'), ] #396
res1000 <- res1000 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) #302

# scrub and grass
gre1000 <- int1000lu[ which(int1000lu$fclass=='urban green'), ] #503
gre1000 <- gre1000 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) #176

# renaming the area columns 
names(com1000)[2] <- "com_area1000"
names(ind1000)[2] <- "ind_area1000"
names(res1000)[2] <- "res_area1000"
names(gre1000)[2] <- "gre_area1000"

# removing geometries for merging
st_geometry(com1000) <- NULL 
st_geometry(ind1000) <- NULL 
st_geometry(res1000) <- NULL 
st_geometry(gre1000) <- NULL 

# Merge to original sensor data 
winterpol2 <- merge(winterpol2, com1000, by = 'Group.1', all= TRUE) # to keep unmatched rows 
winterpol2 <- merge(winterpol2, ind1000, by = 'Group.1', all= TRUE)
winterpol2 <- merge(winterpol2, res1000, by = 'Group.1', all= TRUE)
winterpol2 <- merge(winterpol2, gre1000, by = 'Group.1', all= TRUE)

# replacing NAs with 0s 
winterpol2[is.na(winterpol2)] <- 0

# removing units 
winterpol2$buf_area1000 <- as.vector(winterpol2$buf_area1000)
winterpol2$com_area1000 <- as.vector(winterpol2$com_area1000)
winterpol2$ind_area1000 <- as.vector(winterpol2$ind_area1000)
winterpol2$res_area1000 <- as.vector(winterpol2$res_area1000)
winterpol2$gre_area1000 <- as.vector(winterpol2$gre_area1000)

# Removing the column time, as we do not need it 
winterpol2 <- subset(winterpol2, select = -c(time))
```

*WE HAVE NOW FINISHED CALCULATING THE LAND USE PREDICTORS (in winterpol2
df)*

# OSM road density data

\*STARTING WITH WINTERPOL2¨

### Reading in the data

``` r
BGroads_sf <- st_read("OSM_data.shp/gis_osm_roads_free_1.shp")
BGroads_sf <- st_as_sf(BGroads_sf, wkt=geometry, crs = 4326) # projecting data because data is unprojected

# Transforming CRS of road data to that of Sofia 
BGroads_sf<-st_transform(BGroads_sf, crs = 32634)

# cropping data
sofiaroads_sf<-st_crop(BGroads_sf, morpho)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
sofiaroads_sf$fclass <- as.character(sofiaroads_sf$fclass) # transforming from fact to character

# merging some categories 
sofiaroads_sf$fclass[sofiaroads_sf$fclass=='unclassified'] <- 'uncl and res'
sofiaroads_sf$fclass[sofiaroads_sf$fclass=='residential'] <- 'uncl and res'
```

### 25m buffer

``` r
int25ro <- st_intersection(buf25, sofiaroads_sf)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
int25ro$len25 <- st_length(int25ro)

myvars <- c("Group.1", "fclass", "len25")
int25ro <- int25ro[myvars]

# motorway 
mot25 <- int25ro[ which(int25ro$fclass=='motorway'), ]
# no sensors within 25m of motorway

# trunk
tru25 <- int25ro[ which(int25ro$fclass=='trunk'), ] #no sensors within 25m of trunk 

# primary 
pri25 <- int25ro[ which(int25ro$fclass=='primary'), ]
pri25 <- pri25 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

# secondary 
sec25 <- int25ro[ which(int25ro$fclass=='secondary'), ]
sec25 <- sec25 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

# tertiary 
ter25 <- int25ro[ which(int25ro$fclass=='tertiary'), ]
ter25 <- ter25 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

# uncl and res 
unc25 <- int25ro[ which(int25ro$fclass=='uncl and res'), ]
unc25 <- unc25 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

names(pri25)[2] <- "pri_len25"
names(sec25)[2] <- "sec_len25"
names(ter25)[2] <- "ter_len25"
names(unc25)[2] <- "unc_len25"

# removing geometries
st_geometry(pri25) <- NULL 
st_geometry(sec25) <- NULL 
st_geometry(ter25) <- NULL 
st_geometry(unc25) <- NULL 

# merging with winter pollution sensor data 
winterpol3 <- merge(winterpol2, pri25, by = 'Group.1', all= TRUE)
winterpol3 <- merge(winterpol3, sec25, by = 'Group.1', all= TRUE)
winterpol3 <- merge(winterpol3, ter25, by = 'Group.1', all= TRUE)
winterpol3 <- merge(winterpol3, unc25, by = 'Group.1', all= TRUE)

# removing units 
winterpol3$len25<- as.vector(winterpol3$len25)
#winterpol3$tru_len25 <- as.vector(winterpol3$tru_len25)
winterpol3$pri_len25 <- as.vector(winterpol3$pri_len25)
winterpol3$sec_len25 <- as.vector(winterpol3$sec_len25)
winterpol3$ter_len25 <- as.vector(winterpol3$ter_len25)
winterpol3$unc_len25 <- as.vector(winterpol3$unc_len25)

# replacing NAs with zeros 
winterpol3[is.na(winterpol3)] <- 0
```

### 50m buffer

``` r
int50ro <- st_intersection(buf50, sofiaroads_sf)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
int50ro$len50 <- st_length(int50ro)

myvars <- c("Group.1", "fclass", "len50")
int50ro <- int50ro[myvars]

# motorway # no motorways actually 
mot50 <- int50ro[ which(int50ro$fclass=='motorway'), ]
# the sensors are not near motorways

# trunk
tru50 <- int50ro[ which(int50ro$fclass=='trunk'), ]

# primary 
pri50 <- int50ro[ which(int50ro$fclass=='primary'), ]
pri50 <- pri50 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

# secondary 
sec50 <- int50ro[ which(int50ro$fclass=='secondary'), ]
sec50 <- sec50 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

# tertiary 
ter50 <- int50ro[ which(int50ro$fclass=='tertiary'), ]
ter50 <- ter50 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

# uncl and res 
unc50 <- int50ro[ which(int50ro$fclass=='uncl and res'), ]
unc50 <- unc50 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

names(tru50)[2] <- "tru_len50"
names(pri50)[2] <- "pri_len50"
names(sec50)[2] <- "sec_len50"
names(ter50)[2] <- "ter_len50"
names(unc50)[2] <- "unc_len50"

# removing geometries
st_geometry(tru50) <- NULL 
st_geometry(pri50) <- NULL 
st_geometry(sec50) <- NULL 
st_geometry(ter50) <- NULL 
st_geometry(unc50) <- NULL 

# merging with winter pollution sensor data 
winterpol3 <- merge(winterpol3, pri50, by = 'Group.1', all= TRUE)
winterpol3 <- merge(winterpol3, sec50, by = 'Group.1', all= TRUE)
winterpol3 <- merge(winterpol3, ter50, by = 'Group.1', all= TRUE)
winterpol3 <- merge(winterpol3, unc50, by = 'Group.1', all= TRUE)

# removing units 
winterpol3$pri_len50 <- as.vector(winterpol3$pri_len50)
winterpol3$sec_len50 <- as.vector(winterpol3$sec_len50)
winterpol3$ter_len50 <- as.vector(winterpol3$ter_len50)
winterpol3$unc_len50 <- as.vector(winterpol3$unc_len50)

winterpol3[is.na(winterpol3)] <- 0
```

### 100m buffer

``` r
int100ro <- st_intersection(buf100, sofiaroads_sf)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
int100ro$len100 <- st_length(int100ro)

myvars <- c("Group.1", "fclass", "len100")
int100ro <- int100ro[myvars]

# motorway # no motorways actually 
mot100 <- int100ro[ which(int100ro$fclass=='motorway'), ]
# the sensors are not near motorways, this is why you cannot find motorways here 

# trunk
tru100 <- int100ro[ which(int100ro$fclass=='trunk'), ]
tru100 <- tru100 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum)  

# primary 
pri100 <- int100ro[ which(int100ro$fclass=='primary'), ]
pri100 <- pri100 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

# secondary 
sec100 <- int100ro[ which(int100ro$fclass=='secondary'), ]
sec100 <- sec100 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum)

# tertiary 
ter100 <- int100ro[ which(int100ro$fclass=='tertiary'), ]
ter100 <- ter100 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

# uncl and res 
unc100 <- int100ro[ which(int100ro$fclass=='uncl and res'), ]
unc100 <- unc100 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

names(tru100)[2] <- "tru_len100"
names(pri100)[2] <- "pri_len100"
names(sec100)[2] <- "sec_len100"
names(ter100)[2] <- "ter_len100"
names(unc100)[2] <- "unc_len100"

# removing geometries
st_geometry(tru100) <- NULL 
st_geometry(pri100) <- NULL 
st_geometry(sec100) <- NULL 
st_geometry(ter100) <- NULL 
st_geometry(unc100) <- NULL 

# merging with winter pollution sensor data 
winterpol3 <- merge(winterpol3, tru100, by = 'Group.1', all= TRUE) 
winterpol3 <- merge(winterpol3, pri100, by = 'Group.1', all= TRUE)
winterpol3 <- merge(winterpol3, sec100, by = 'Group.1', all= TRUE)
winterpol3 <- merge(winterpol3, ter100, by = 'Group.1', all= TRUE)
winterpol3 <- merge(winterpol3, unc100, by = 'Group.1', all= TRUE)

# removing units 
winterpol3$len100<- as.vector(winterpol3$len100)
winterpol3$tru_len100 <- as.vector(winterpol3$tru_len100)
winterpol3$pri_len100 <- as.vector(winterpol3$pri_len100)
winterpol3$sec_len100 <- as.vector(winterpol3$sec_len100)
winterpol3$ter_len100 <- as.vector(winterpol3$ter_len100)
winterpol3$unc_len100 <- as.vector(winterpol3$unc_len100)

# replacing NAs with zeros 
winterpol3[is.na(winterpol3)] <- 0
```

### 300m buffer

``` r
int300ro <- st_intersection(buf300, sofiaroads_sf)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
int300ro$len300 <- st_length(int300ro)

myvars <- c("Group.1", "fclass", "len300")
int300ro <- int300ro[myvars]

# motorway # no motorways actually 
mot300 <- int300ro[ which(int300ro$fclass=='motorway'), ]
# no sensors within 300m of motorway

# trunk
tru300 <- int300ro[ which(int300ro$fclass=='trunk'), ]
tru300 <- tru300 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

# primary 
pri300 <- int300ro[ which(int300ro$fclass=='primary'), ]
pri300 <- pri300 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

# secondary 
sec300 <- int300ro[ which(int300ro$fclass=='secondary'), ]
sec300 <- sec300 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

# tertiary 
ter300 <- int300ro[ which(int300ro$fclass=='tertiary'), ]
ter300 <- ter300 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum)

# uncl and res 
unc300 <- int300ro[ which(int300ro$fclass=='uncl and res'), ]
unc300 <- unc300 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

names(tru300)[2] <- "tru_len300"
names(pri300)[2] <- "pri_len300"
names(sec300)[2] <- "sec_len300"
names(ter300)[2] <- "ter_len300"
names(unc300)[2] <- "unc_len300"

# removing geometries
st_geometry(tru300) <- NULL 
st_geometry(pri300) <- NULL 
st_geometry(sec300) <- NULL 
st_geometry(ter300) <- NULL 
st_geometry(unc300) <- NULL 

# merging with winter pollution sensor data 
winterpol3 <- merge(winterpol3, tru300, by = 'Group.1', all= TRUE) 
winterpol3 <- merge(winterpol3, pri300, by = 'Group.1', all= TRUE)
winterpol3 <- merge(winterpol3, sec300, by = 'Group.1', all= TRUE)
winterpol3 <- merge(winterpol3, ter300, by = 'Group.1', all= TRUE)
winterpol3 <- merge(winterpol3, unc300, by = 'Group.1', all= TRUE)

# removing units 
winterpol3$len300<- as.vector(winterpol3$len300)
winterpol3$tru_len300 <- as.vector(winterpol3$tru_len300)
winterpol3$pri_len300 <- as.vector(winterpol3$pri_len300)
winterpol3$sec_len300 <- as.vector(winterpol3$sec_len300)
winterpol3$ter_len300 <- as.vector(winterpol3$ter_len300)
winterpol3$unc_len300 <- as.vector(winterpol3$unc_len300)

# replacing NAs with zeros 
winterpol3[is.na(winterpol3)] <- 0
```

### 500m buffer

``` r
int500ro <- st_intersection(buf500, sofiaroads_sf)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
int500ro$len500 <- st_length(int500ro)

myvars <- c("Group.1", "fclass", "len500")
int500ro <- int500ro[myvars]

# motorway # no motorways actually 
mot500 <- int500ro[ which(int500ro$fclass=='motorway'), ]
# the sensors are not near motorways, this is why you cannot find motorways here 

# trunk
tru500 <- int500ro[ which(int500ro$fclass=='trunk'), ]
tru500 <- tru500 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

# primary 
pri500 <- int500ro[ which(int500ro$fclass=='primary'), ]
pri500 <- pri500 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

# secondary 
sec500 <- int500ro[ which(int500ro$fclass=='secondary'), ]
sec500 <- sec500 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum)

# tertiary 
ter500 <- int500ro[ which(int500ro$fclass=='tertiary'), ]
ter500 <- ter500 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

# uncl and res 
unc500 <- int500ro[ which(int500ro$fclass=='uncl and res'), ]
unc500 <- unc500 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

names(tru500)[2] <- "tru_len500"
names(pri500)[2] <- "pri_len500"
names(sec500)[2] <- "sec_len500"
names(ter500)[2] <- "ter_len500"
names(unc500)[2] <- "unc_len500"

# removing geometries
st_geometry(tru500) <- NULL 
st_geometry(pri500) <- NULL 
st_geometry(sec500) <- NULL 
st_geometry(ter500) <- NULL 
st_geometry(unc500) <- NULL 

# merging with winter pollution sensor data 
winterpol3 <- merge(winterpol3, tru500, by = 'Group.1', all= TRUE) 
winterpol3 <- merge(winterpol3, pri500, by = 'Group.1', all= TRUE)
winterpol3 <- merge(winterpol3, sec500, by = 'Group.1', all= TRUE)
winterpol3 <- merge(winterpol3, ter500, by = 'Group.1', all= TRUE)
winterpol3 <- merge(winterpol3, unc500, by = 'Group.1', all= TRUE)

# removing units 
winterpol3$tru_len500 <- as.vector(winterpol3$tru_len500)
winterpol3$pri_len500 <- as.vector(winterpol3$pri_len500)
winterpol3$sec_len500 <- as.vector(winterpol3$sec_len500)
winterpol3$ter_len500 <- as.vector(winterpol3$ter_len500)
winterpol3$unc_len500 <- as.vector(winterpol3$unc_len500)

# replacing NAs with zeros 
winterpol3[is.na(winterpol3)] <- 0
```

### 1000m buffer

``` r
int1000ro <- st_intersection(buf1000, sofiaroads_sf)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
int1000ro$len1000 <- st_length(int1000ro)

myvars <- c("Group.1", "fclass", "len1000")
int1000ro <- int1000ro[myvars]

# motorway # no motorways actually 
mot1000 <- int1000ro[ which(int1000ro$fclass=='motorway'), ]
mot1000 <- mot1000 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum)

# trunk
tru1000 <- int1000ro[ which(int1000ro$fclass=='trunk'), ]
tru1000 <- tru1000 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

# primary 
pri1000 <- int1000ro[ which(int1000ro$fclass=='primary'), ]
pri1000 <- pri1000 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum)

# secondary 
sec1000 <- int1000ro[ which(int1000ro$fclass=='secondary'), ]
sec1000 <- sec1000 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum)

# tertiary 
ter1000 <- int1000ro[ which(int1000ro$fclass=='tertiary'), ]
ter1000 <- ter1000 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

# uncl and res 
unc1000 <- int1000ro[ which(int1000ro$fclass=='uncl and res'), ]
unc1000 <- unc1000 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum)

names(mot1000)[2] <- "mot_len1000"
names(tru1000)[2] <- "tru_len1000"
names(pri1000)[2] <- "pri_len1000"
names(sec1000)[2] <- "sec_len1000"
names(ter1000)[2] <- "ter_len1000"
names(unc1000)[2] <- "unc_len1000"

# removing geometries
st_geometry(mot1000) <- NULL 
st_geometry(tru1000) <- NULL 
st_geometry(pri1000) <- NULL 
st_geometry(sec1000) <- NULL 
st_geometry(ter1000) <- NULL 
st_geometry(unc1000) <- NULL 

# merging with winter pollution sensor data 
winterpol3 <- merge(winterpol3, mot1000, by = 'Group.1', all= TRUE) 
winterpol3 <- merge(winterpol3, tru1000, by = 'Group.1', all= TRUE) 
winterpol3 <- merge(winterpol3, pri1000, by = 'Group.1', all= TRUE)
winterpol3 <- merge(winterpol3, sec1000, by = 'Group.1', all= TRUE)
winterpol3 <- merge(winterpol3, ter1000, by = 'Group.1', all= TRUE)
winterpol3 <- merge(winterpol3, unc1000, by = 'Group.1', all= TRUE)

# removing units 
winterpol3$mot_len1000 <- as.vector(winterpol3$mot_len1000)
winterpol3$tru_len1000 <- as.vector(winterpol3$tru_len1000)
winterpol3$pri_len1000 <- as.vector(winterpol3$pri_len1000)
winterpol3$sec_len1000 <- as.vector(winterpol3$sec_len1000)
winterpol3$ter_len1000 <- as.vector(winterpol3$ter_len1000)
winterpol3$unc_len1000 <- as.vector(winterpol3$unc_len1000)

# replacing NAs with zeros 
winterpol3[is.na(winterpol3)] <- 0
```

*WE HAVE NOW FINISHED ADDING THE ROAD DENSITY PREDICTORS (in winterpol3
df)*

# Heating, demography and local urban mophology

### City Morphology (Building level)

``` r
# This is the City Morphology data at the building level 
morpho_b <- st_read("city_morpho_buildinglevel/Сгради_20190122.shp") 
# will neeed it for: 
# --> types of heating
# --> population density (population)
# --> hhold density (nbr of hholds per buildings)
# --> number of floors of building 

# changing the CRS
morpho_b <-st_transform(morpho_b, crs = 32634)

# cropping to sofia 
morpho_b <-st_crop(morpho_b, morpho)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

### 100m buffer

``` r
int100morph <- st_intersection(buf100, morpho_b) 
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
###### Heating, floorcount ######
myvars <- c("Group.1", "appcount", "floorcount", "nj17_eq_1_", "nj17_eq_3_", "nj17_eq4i_", "nn_househ_", "nn_people_")
sub4mean100 <- int100morph[myvars]

morph_agg100 <- sub4mean100  %>% # it will be AVERAGE nbr of hholds OR population per buffer
  group_by(Group.1) %>% 
  summarise_if(is.numeric, mean)

# changing variable names 
names(morph_agg100)[names(morph_agg100) == "appcount"] <- "flat_av100" 
names(morph_agg100)[names(morph_agg100) == "floorcount"] <- "flo_av100" 
names(morph_agg100)[names(morph_agg100) == "nj17_eq_1_"] <- "central_av100" 
names(morph_agg100)[names(morph_agg100) == "nj17_eq_3_"] <- "electric_av100" 
names(morph_agg100)[names(morph_agg100) == "nj17_eq4i_"] <- "nafta_av100" 
names(morph_agg100)[names(morph_agg100) == "nn_househ_"] <- "hh_av100" 
names(morph_agg100)[names(morph_agg100) == "nn_people_"] <- "ppl_av100" 

# merging
st_geometry(morph_agg100) <- NULL 
winterpol4 <- merge(winterpol3, morph_agg100, by = 'Group.1', all= TRUE)

###### population ######
myvars <- c("Group.1", "nn_people_")
sub4sum100 <- int100morph[myvars]

# 
morph_agg1002 <- sub4sum100 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

names(morph_agg1002)[names(morph_agg1002) == "nn_people_"] <- "pop_100" 

# remove geomemtry 
st_geometry(morph_agg1002) <- NULL 

# merge
winterpol4 <- merge(winterpol4, morph_agg1002, by = 'Group.1', all= TRUE)

###### nbr of buildings ######
buildings100 <- count(int100morph, Group.1) 

names(buildings100)[names(buildings100) == "n"] <- "bui_100"
st_geometry(buildings100) <- NULL 
winterpol4 <- merge(winterpol4, buildings100, by = 'Group.1', all= TRUE)
```

### 300m buffer

``` r
int300morph <- st_intersection(buf300, morpho_b) 
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
###### Heating, floorcount ######
myvars <- c("Group.1", "appcount", "floorcount", "nj17_eq_1_", "nj17_eq_3_", "nj17_eq4i_", "nn_househ_", "nn_people_")
sub4mean300 <- int300morph[myvars]

morph_agg300 <- sub4mean300  %>% # it will be AVERAGE nbr of hholds OR population per buffer
  group_by(Group.1) %>% 
  summarise_if(is.numeric, mean)

# changing variable names 
names(morph_agg300)[names(morph_agg300) == "appcount"] <- "flat_av300" 
names(morph_agg300)[names(morph_agg300) == "floorcount"] <- "flo_av300" 
names(morph_agg300)[names(morph_agg300) == "nj17_eq_1_"] <- "central_av300" 
names(morph_agg300)[names(morph_agg300) == "nj17_eq_3_"] <- "electric_av300" 
names(morph_agg300)[names(morph_agg300) == "nj17_eq4i_"] <- "nafta_av300" 
names(morph_agg300)[names(morph_agg300) == "nn_househ_"] <- "hh_av300" 
names(morph_agg300)[names(morph_agg300) == "nn_people_"] <- "ppl_av300" 

# merging
st_geometry(morph_agg300) <- NULL 
winterpol4 <- merge(winterpol4, morph_agg300, by = 'Group.1', all= TRUE)

###### population ######
myvars <- c("Group.1", "nn_people_")
sub4sum300 <- int300morph[myvars]

morph_agg3002 <- sub4sum300 %>%
  group_by(Group.1) %>% 
  summarise_if(is.numeric, sum) 

names(morph_agg3002)[names(morph_agg3002) == "nn_people_"] <- "pop_300" 

# remove geomemtry 
st_geometry(morph_agg3002) <- NULL 

# merge
winterpol4 <- merge(winterpol4, morph_agg3002, by = 'Group.1', all= TRUE)

###### nbr of buildings ######

buildings300 <- count(int300morph, Group.1) 

names(buildings300)[names(buildings300) == "n"] <- "bui_300"
st_geometry(buildings300) <- NULL 
winterpol4 <- merge(winterpol4, buildings300, by = 'Group.1', all= TRUE)
```

### 50m

``` r
# for building density and average nbr of floors --> smaller buffer: 50 (and 25, next section) 
int50morph <- st_intersection(buf50, morpho_b) 
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
###### floor average ######
myvars <- c("Group.1", "floorcount")
sub4mean50 <- int50morph[myvars]

morph_agg50 <- sub4mean50  %>% 
  group_by(Group.1) %>% 
  summarise_if(is.numeric, mean)

# changing variable names 
names(morph_agg50)[names(morph_agg50) == "floorcount"] <- "flo_av50" 

# merging
st_geometry(morph_agg50) <- NULL 
winterpol4 <- merge(winterpol4, morph_agg50, by = 'Group.1', all= TRUE)

###### nbr of buildings ######
buildings50 <- count(int50morph, Group.1) 

names(buildings50)[names(buildings50) == "n"] <- "bui_50"
st_geometry(buildings50) <- NULL 
winterpol4 <- merge(winterpol4, buildings50, by = 'Group.1', all= TRUE)
```

### 25m buffer

``` r
int25morph <- st_intersection(buf25, morpho_b) 
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
###### floor average ######
myvars <- c("Group.1", "floorcount")
sub4mean25 <- int25morph[myvars]

morph_agg25 <- sub4mean25  %>% # it will be AVERAGE nbr of hholds OR population per buffer
  group_by(Group.1) %>% 
  summarise_if(is.numeric, mean)

# changing variable names 
names(morph_agg25)[names(morph_agg25) == "floorcount"] <- "flo_av25" 

# merging
st_geometry(morph_agg25) <- NULL 
winterpol4 <- merge(winterpol4, morph_agg25, by = 'Group.1', all= TRUE)

###### nbr of buildings ######
buildings25 <- count(int25morph, Group.1) 
names(buildings25)[names(buildings25) == "n"] <- "bui_25"
st_geometry(buildings25) <- NULL 
winterpol4 <- merge(winterpol4, buildings25, by = 'Group.1', all= TRUE)
```

*WE HAVE NOW FINISHED ADDING THE HEAING, POPULATION AND MORPHOLOGY
PREDICTORS (in winterpol4 df)*

\#Altitude data

``` r
# Reading in topographic data (altitude) from Data Science Society. 
topo <- read.csv("altitude/sofia_topo.txt", header = F, sep = ",")
names(topo)[which(names(topo) == "V1")] <- "lat"
names(topo)[which(names(topo) == "V2")] <- "lon"

topo <- topo[c("lon", "lat", "V3")]
topo <- topo[-c(1), ]
topo$lat <- as.character(topo$lat)
topo$lon <- as.character(topo$lon)
topo$lat <- as.numeric(topo$lat)
topo$lon <- as.numeric(topo$lon)

# Transforming crs of topography data to that of Sofia shapefile
topo <- st_as_sf(topo, coords = c("lon", "lat"), crs = 4326) # Assigning it WGS84 CRS, because 'topo" is not projected
topo <-st_transform(topo, crs = 32634) # Assignign it CRS of Sofia shapefile

topo_sp <- as(topo, 'Spatial') # Converting topography to SPDF 

# Calculating the extent of the SPDF
e <- extent(bbox(topo_sp))

# Converting SPDF to normal DF to then transform the DF to a raster 
DF <- as.data.frame(topo_sp)

r <- raster(e, ncol=10, nrow=2) # Convert 'e' to raster

# Converting the data of the topo DF to numeric. 
# Need to convert to character first, because data was originally in vector form. 
DF$V3 <- as.character(DF$V3)
DF$V3 <- as.numeric(DF$V3) 
DF$coords.x1 <- as.character(DF$coords.x1)
DF$coords.x1 <- as.numeric(DF$coords.x1)
DF$coords.x2 <- as.character(DF$coords.x2)
DF$coords.x2 <- as.numeric(DF$coords.x2)

# Creating a raster of topography data, using the topo DF and the extent e, already in raster form. Cols 2 and 3 are the coordinated, col is the value (the altitude). 
x <- rasterize(DF[, (2:3)], r, DF[,1], fun=mean)

# Cropping the raster to the extent of the Sofia shapefile. 
#altmask <- mask(x, shapes_sp)

altmask <- mask(x, morpho)

# 25
av_alt<- raster::extract(altmask, buf25, fun=mean, na.rm=TRUE) 

# adding the to the df 
av_alt <- as.data.frame(av_alt)

# Merging to Sofia shapefile. The data in the altidude DF is already ordered, so 'unlist' is enough to merge the altitude data to Sofia shapefile. 
winterpol5 <- winterpol4 
winterpol5$alt25 <- unlist(av_alt)
```

*WE HAVE NOW FINISHED ADDING THE ALTITUDE PREDICTORS (in winterpol5 df)*

\#Finishing up

``` r
# Put the long and lat variables in the model 
# removing that extra dirty row: 
winterpol6 <- winterpol5
coords <- st_coordinates(winterpol6)
winterpol7 <- cbind(winterpol6, coords)

st_geometry(winterpol7) <- NULL 

# removing columns that we do not need in the regression
winterpol7 <- subset(winterpol7, select = -c(Group.1, geohash, P1))
```

``` r
names(winterpol7)
```

    ##  [1] "P2"             "temperature"    "humidity"       "pressure"      
    ##  [5] "Freq"           "com_area100"    "ind_area100"    "res_area100"   
    ##  [9] "gre_area100"    "com_area300"    "ind_area300"    "res_area300"   
    ## [13] "gre_area300"    "com_area500"    "ind_area500"    "res_area500"   
    ## [17] "gre_area500"    "com_area1000"   "ind_area1000"   "res_area1000"  
    ## [21] "gre_area1000"   "pri_len25"      "sec_len25"      "ter_len25"     
    ## [25] "unc_len25"      "pri_len50"      "sec_len50"      "ter_len50"     
    ## [29] "unc_len50"      "tru_len100"     "pri_len100"     "sec_len100"    
    ## [33] "ter_len100"     "unc_len100"     "tru_len300"     "pri_len300"    
    ## [37] "sec_len300"     "ter_len300"     "unc_len300"     "tru_len500"    
    ## [41] "pri_len500"     "sec_len500"     "ter_len500"     "unc_len500"    
    ## [45] "mot_len1000"    "tru_len1000"    "pri_len1000"    "sec_len1000"   
    ## [49] "ter_len1000"    "unc_len1000"    "flat_av100"     "flo_av100"     
    ## [53] "central_av100"  "electric_av100" "nafta_av100"    "hh_av100"      
    ## [57] "ppl_av100"      "pop_100"        "bui_100"        "flat_av300"    
    ## [61] "flo_av300"      "central_av300"  "electric_av300" "nafta_av300"   
    ## [65] "hh_av300"       "ppl_av300"      "pop_300"        "bui_300"       
    ## [69] "flo_av50"       "bui_50"         "flo_av25"       "bui_25"        
    ## [73] "alt25"          "X"              "Y"

``` r
ncol(winterpol7)
```

    ## [1] 75

*DATAFRAME WITH PREDICTORS IS FINISHED: 75 variables, 74 PREDICTORS AND
1 OUTCOME VARIABLE (P2)*
