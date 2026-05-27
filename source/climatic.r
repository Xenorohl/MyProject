######################################################

##### Packages

######################################################

library(Rchelsa)
library(terra)
library(dplyr)
library(ggplot2)
library(pals)
library(ggthemes)
library(extrafont)
library(paletteer)

# install Rchelsa via github
# devtools::install_git("https://gitlabext.wsl.ch/karger/rchelsa.git")
# install.packages("pals")





######################################################

##### Create a spatial object

######################################################

pts_v = terra::vect(
    matrix_full_eco_elev,
    geom = c("longitude", "latitude"),
    crs = "EPGS:4326"
) 



### extract simple coordinates as a standard dataframe

coords_df = as.data.frame(terra::geom(pts_v)[, c("x", "y")]) %>%
    rename(
        longitude = x,
        latitude = y
    ) %>%
    mutate(occurence_id = matrix_full_eco_elev$species)

head(coords_df)
nrow(coords_df)
coords_df_unique = distinct(    # distinct allow to remove the duplicates
    coords_df,                  # choosing with dataset 
    longitude,
    latitude,                   # selecting only the longitude and latitude (so it removes duplicates of coordinates, basically remove duplicates of points on the map)
    .keep_all = T
)                               # this must be done for further step (cant transform the extracted Chelsa data to matrix if you have duplicate)
nrow(coords_df_unique)
head(coords_df_unique)





######################################################

##### Extraction of annual precipitation values for 2021 (non existant for later than 21) 

######################################################

annual_prec = getChelsa(
    var = "pr",     # "pr" for precipitation
    coords = coords_df_unique %>% select(longitude, latitude),  # setting the coordinates
    startdate = as.Date("2021-01-01"),
    enddate = as.Date("2021-12-15"),
    dataset = "chelsa-monthly"
)

head(annual_prec)
nrow(annual_prec)
names(annual_prec)



### remove the time column with dplyr, then convert to matrix 

mat_prec = annual_prec %>%
    select(-time) %>%
    as.matrix()
    # no more error message of duplicate (was before because of the duplicates, this explains the "distinct()" before at line 49-54)

head(mat_prec) 
nrow(mat_prec)



### calculating the mean of 2021 

prec_mean = colMeans(mat_prec, na.rm = TRUE) # making a mean of the column (12 rows, mean of precipitation for each observation point since column names are observations points)

# prec_df = data.frame(
#     occurence_id = matrix_full_eco_elev$species,
#     prec_mean_annual = as.numeric(prec_mean)
# )

    # error because of different lengths 
    # must remove all the duplicates of coordinates of matrix_full_eco_elev too... 
    # instead of removing around the half of the observation, I will just copy matrix_full_eco_elev into a new df
    # the new df will have the unique purpose of doing climatic graphs, nuthing more 

climax = matrix_full_eco_elev # the idea was to create another matrix just for climatic data, but as explained later it doesnt make sense (more details below)

climax = distinct(
    climax,
    longitude,
    latitude,
    .keep_all = TRUE
)   # function distinct() allow to remove all duplicates, here only the duplicates of geographic coordinates (longitude, latitude)
nrow(climax)
length(prec_mean) # same number as nrow(climax)



prec_df = data.frame(
    prec_mean = as.numeric(prec_mean)
)

prec_df





######################################################

##### Extracting temperature data 

######################################################

temperature_annual = getChelsa(
    var = "tasmax",
    coords = coords_df_unique %>% select(longitude, latitude),
    startdate = as.Date("2021-01-01"),
    enddate = as.Date("2021-12-15"),
    dataset = "chelsa-monthly"
)



### removing the time column + converting to matrix 

mat_temp = temperature_annual %>%
    select(-time) %>%
    as.matrix()



### calculating mean of 2021 and converting into df 

temp_mean = colMeans(mat_temp, na.rm = T)

temp_df = data.frame(
    temp_mean = as.numeric(temp_mean)
)

head(temp_df)
temp_df$temp_mean = temp_df$temp_mean - 273.15 # converting to Celsius
head(temp_df) # all good



######################################################

##### Extracting cloud cover data ##### Could be interesting to see if clouds could impact the mushrooms, like impacting the sun exposure 

######################################################

clouds_cover = getChelsa( 
    var = "clt",
    coords = coords_df_unique %>% select(longitude, latitude),
    startdate = as.Date("2021-01-01"),
    enddate = as.Date("2021-12-15"),
    dataset = "chelsa-monthly"
)



### removing the time column and converting to matrix 

mat_clouds = clouds_cover %>%
    select(-time) %>%
    as.matrix()



### calculating mean of 2021 and converting to dataframe

clouds_mean = colMeans(mat_clouds)

clouds_df = data.frame(
    clouds_mean = as.numeric(clouds_mean)
)





######################################################

##### Future climatic data, but making a mean for 2050 and not picking one specific day 

######################################################

### creating a vector with all the desired date 

futur_date_5 = c()
futur_date_15 = c()
futur_date_25 = c()

for(i in 1:12){ 

    date_5 = paste0("2050-", i ,"-05") 
    date_15 = paste0("2050-", i ,"-15")
    date_25 = paste0("2050-", i ,"-25")

    futur_date_5 = c(futur_date_5, date_5)
    futur_date_15 = c(futur_date_15, date_15)
    futur_date_25 = c(futur_date_25, date_25)

}   # I decided to take 3 day for every month and making a mean of every value in the end. I took 5-15-25 so i have a good spread on each month and globally on the year 

futur_date_5 # indeed every 5th of each month 
futur_date_15 # every 15th of each month

futur_date = c(
    futur_date_5, 
    futur_date_15, 
    futur_date_25
)

futur_date
length(futur_date) # 36 is ok, 3 date for each month, so I can make a mean for futur climate, otherwise it doesnt make any sense to compare the mean of 2021 with one particular day of 2050



### extracting the temperature for each date (36 different in total)

for(i in futur_date){

    assign(
        paste0("futur_", i),

        getChelsa(
        var = "tas",
        coords = coords_df_unique %>% select(longitude, latitude),
        date = as.Date(i),
        dataset = "chelsa-climatologies",
        ssp = "ssp370", # intermediate scenario
        forcing = "IPSL-CM6A-LR"
        )
    ) 

} # this loop was done after the lecture on loops, I didnt know that we could use something else as x:y in for(i in x:y)



### removing time column and converting to matrix (done with a little help of Chatgpt : I forgot to put assign() in the beginning so the value couldnt get stored inside a chr object)
    # beside chatgpt, I've done it with my understanding of loops alone

for(i in 1:12){

    assign(
        paste0("futur_", i ,"_05"), 

        get(paste0("futur_2050-", i ,"-05")) %>%
            select(-time) %>%
            as.matrix()
    )

    assign(
        paste0("futur_", i ,"_15"),

        get(paste0("futur_2050-", i ,"-15")) %>%
            select(-time) %>%
            as.matrix()
    )

    assign(
        paste0("futur_", i ,"_25"), 

        get(paste0("futur_2050-", i ,"-25")) %>%
            select(-time) %>%
            as.matrix()
    )
}   # this loops convert the raw data of chelsa into matrix, and removes the "time" column. For each futur temperature dataset 

# quick check for the first object 

futur_1_05 # seems ok, gotta not forget to convert it to Celsius later, will do it in the end
futur_12_05 # also ok 



### converting to dataframe each matrix (chatgpt helped with the function setNames, otherwise every dataframe would have the same column names between each other)

for(i in 1:12){
   
    tag05 = paste0("fut_05_", i)
    tag15 = paste0("fut_15_", i)
    tag25 = paste0("fut_25_", i)

    assign(
        paste0("futur_05_", i ,"_df"),

        setNames(
            data.frame(
                as.numeric(get(paste0("futur_", i ,"_05")))
            ),
            tag05
        )
    )

    assign(
        paste0("futur_15_", i ,"_df"),

        setNames(
            data.frame(
                as.numeric(get(paste0("futur_", i ,"_15")))
            ),
            tag15
        )
    )

       assign(
        paste0("futur_25_", i ,"_df"),

        setNames(
            data.frame(
                as.numeric(get(paste0("futur_", i ,"_25")))
            ),
            tag25
        )
    )

}

head(futur_05_1_df)     
head(futur_05_2_df) # IT WOOOOORKED !!!!!



### creating a mean for 2050, so I just have one value for 2050 

temp_fut_df = data.frame(

    futur_05_1_df, futur_05_2_df, futur_05_3_df, futur_05_4_df, futur_05_5_df, futur_05_6_df,
    futur_05_7_df, futur_05_8_df, futur_05_9_df, futur_05_10_df, futur_05_11_df, futur_05_12_df,

    futur_15_1_df, futur_15_2_df, futur_15_3_df, futur_15_4_df, futur_15_5_df, futur_15_6_df, 
    futur_15_7_df, futur_15_8_df, futur_15_9_df, futur_15_10_df, futur_15_11_df, futur_15_12_df,

    futur_25_1_df, futur_25_2_df, futur_25_3_df, futur_25_4_df, futur_25_5_df, futur_25_6_df, 
    futur_25_7_df, futur_25_8_df, futur_25_9_df, futur_25_10_df, futur_25_11_df, futur_25_12_df

    )   # creating the dataframe on the future temperature to later calculate the mean 

head(temp_fut_df)
ncol(temp_fut_df) # 36 columns, nice 

fut_temp = rowMeans(temp_fut_df) # mean of future temperature (done on Rows because I want a mean for every observation point)
    # can do a mean of every value at one time since mean(c(mean(c(1,2,3)), mean(c(1,2,3)), mean(c(1,2,3)))) == mean(c(1,2,3,1,2,3,1,2,3)) = TRUE

fut_temp_df = data.frame(
    fut_temp = as.numeric(fut_temp)
)

head(fut_temp_df)
fut_temp_df$fut_temp = fut_temp_df$fut_temp - 273.15 # Celsius conversion 
head(fut_temp_df)

# head(temp_df) return this
# 1 -0.5000000
# 2  0.3166667
# 3  1.0916667
# 4  3.5250000
# 5  3.2583333
# 6  0.8583333

# for ssp126, head(fut_temp_df) return this : 
# 1 -2.9250000
# 2 -1.4333333
# 3 -0.8416667
# 4  1.7500000
# 5  1.3750000
# 6 -0.9833333

# for ssp370, head(fut_temp_df) return this :
# 1 -2.12500000
# 2 -0.64166667
# 3 -0.04166667
# 4  2.55000000
# 5  2.17500000
# 6 -0.18333333

# for ssp585, head(fut_temp_df) return this ;
# 1 -1.39166667
# 2  0.08333333
# 3  0.69166667
# 4  3.23333333
# 5  2.87500000
# 6  0.54166667

# max(fut_temp_df$fut_temp) is 16.56 (for ssp585)
# max(temp_df) is 17.05

# with all that results, it seems like 2021 was a pretty hot year, because the worst case scenario (ssp585) has lower temperature than 2021
# It is probably due to making an annual mean of temperature (which isnt relevant realistically)



######################################################

##### Merging the new data with the new dataframe climax 

######################################################

climax = data.frame(
    climax,
    temp_df,
    prec_df,
    clouds_df,
    fut_temp_df
)  

head(climax) # seems pretty fine 
nrow(climax) # more than 2500 observations that are lost due to duplication 
    # Side note : 
    # The idea was to create "climax" as a matrix with the climatic data (and the other one) 
    # Then, it could have been cool to take the original dataframe (matrix_full_eco_elev), and take the opposite of distinct (the ~2500 data lost)
    # After that I would have convert the "opposite" dataframe to a matrix and join the two 
    # It would have mean more than 2500 observation with N/A values for climatic data 
    # The problem is it is not relevant... duplicate for geolocalisation must be taken away, otherwise it means a too strong bias for the other variables (soil, landcover, climate_re, elevation, ...)
    # Since gbif data are degraded, it is not relevant to keep all the duplicates 

    # so I will just rename the final df 

matrix_full_eco_elev_clim = climax




######################################################

##### Plotting the new data

######################################################

### Plotting the temperature on switzerland 

CH = ne_countries(
    scale = 10,
    country = "switzerland",
    returnclass = "sf"
) # as explained in ./source/ecosystem.r

# current with precipitation
x11()
ggplot(data = CH) + 
    geom_sf(
        fill = "aliceblue",
        color = "grey" 
    ) + 
    geom_point(
        data = matrix_full_eco_elev_clim,
        aes(
            x = longitude,
            y = latitude,
            color = temp_mean,
            size = prec_mean
        )
    ) + 
    scale_colour_viridis_c() +
    labs(
        title = "Precipiation and temperature over the Swiss maps, for each observations, in 2021",
        x = "Longitude",
        y = "Latitude",
        color = "Mean temperature [°C]",
        size = "Mean precipitation [kg/m2]"
    ) +
    theme_minimal() 

# futur with altitude
x11()
ggplot(data = CH) + 
    geom_sf(
        fill = "aliceblue",
        color = "grey" 
    ) + 
    geom_point(
        data = matrix_full_eco_elev_clim,
        aes(
            x = longitude,
            y = latitude,
            color = fut_temp,
            size = elevation
        )
    ) + 
    scale_colour_viridis_c() +
    labs(
        title = "2050 temperature in Switzerland, for each obseravtion",
        x = "Longitude",
        y = "Latitude",
        color = "Mean temperature [°C]",
        size = "Altitude [m]"
    ) +
    theme_minimal() 



### Also for cloud covering 

x11()
ggplot(data = CH) + 
    geom_sf(
        fill = "#f5efd1",
        color = "grey"
    ) + 
    geom_point(
        data = matrix_full_eco_elev_clim,
        aes(
            x = longitude,
            y = latitude,
            color = clouds_mean,
            size = elevation
        ),
        alpha = 0.8,
        shape = 20
    ) +   
    scale_color_paletteer_c(    # just changing of viridis, this one looks cool too
        "pals::coolwarm"
    ) +
    labs(
        x = "Longitude",
        y = "Latitude", 
        title = "Clouds mean cover on each observation site, for different altitude",
        size = "Altitude [m]",
        color = "Clouds cover [%]"
    ) + 
    theme_minimal()

# checking if clouds are positively correlated to the rain 
x11()
ggplot(
    data = matrix_full_eco_elev_clim,
    aes(
        x = clouds_mean,
        y = prec_mean
    )
    ) + 
    geom_smooth(
        method = "lm"
    ) +
    labs(
        x = "Percent of cloud cover",
        y = "Precipitation [kg/m2]",
        title = "Correlation between clouds cover and precipiation"
    ) + 
    theme_minimal()
    # it is positive, ok 