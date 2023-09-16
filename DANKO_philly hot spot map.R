setwd("~/Documents/Penn Masters/CRIM 602")

library(sqldf)
library(hexbin)
library(ggmap)

# source of philly crime data - bind 2020 and 2021
# https://www.opendataphilly.org/dataset/crime-incidents

philly2020 <- read.csv("incidents_part1_part2 2020.csv")
philly2021 <- read.csv("incidents_part1_part2 2021.csv")

dim(philly2020)
dim(philly2021)

phillycrime <- rbind(philly2020,philly2021)
dim(phillycrime)

write.csv(phillycrime,"phillycrime2020_2021.csv")


scan(what="", file="phillycrime2020_2021.csv",
     nlines=5, sep=";")

con <- dbConnect(SQLite(), dbname="phillycrime.db")
dbWriteTable(con, "crime",
             "phillycrime2020_2021.csv",
             row.names=FALSE,
             header=TRUE,
             sep=",")

#check work
a <- read.table("phillycrime2020_2021.csv",
                sep=",",nrows=30000,header=TRUE)
tail(a)

######################## DO I NEED?##############################################
sure.you.want.to.rebuild.database <- FALSE

if(sure.you.want.to.rebuild.database) 
{
  con <- dbConnect(SQLite(), dbname="phillycrime.db")
  
  variabletypes <- dbDataType(con, a)

    if(dbExistsTable(con, "crime")) dbRemoveTable(con, "crime")
  dbWriteTable(con, "crime",
               "phillycrime2020_2021.csv",
               row.names=FALSE,
               header=TRUE,
               field.types=variabletypes,
               sep=",")
  dbListFields(con,"crime")
  dbDisconnect(con)
}

con <- dbConnect(SQLite(), dbname="phillycrime.db")

res <- dbSendQuery(con, "
                   SELECT *
                   FROM crime
                   ")
dbFetch(res, n = 10)
dbClearResult(res)

res <- dbSendQuery(con, "
                   SELECT ucr_general, text_general_code
                   FROM crime
                   GROUP BY ucr_general
                   ")
dbFetch(res)
dbClearResult(res)

res <- dbSendQuery(con, "
                   SELECT dc_dist
                   FROM   crime
                   WHERE  ucr_general='1800'")
a <- dbFetch(res, n=-1)
table(a$dc_dist)
dbClearResult(res)

res <- dbSendQuery(con, "
   SELECT MIN(lat)  AS min_lat,
          MAX(lat)  AS max_lat,
          MIN(lng) AS min_lng,
          MAX(lng) AS max_lng,
          dc_dist
   FROM crime
   GROUP BY dc_dist
   ORDER BY dc_dist")
dbFetch(res, n=-1)
dbClearResult(res)

dbSendQuery(con, "
   UPDATE crime SET point_x=NULL
   WHERE (point_x='') OR (point_x=0)")
dbSendQuery(con, "
   UPDATE crime SET point_y=NULL
   WHERE (point_y='') OR (point_y=0)")
dbSendQuery(con, "
   UPDATE crime SET lat=NULL
   WHERE (lat='') OR (ABS(lat-0.0)<0.01)")
dbSendQuery(con, "
   UPDATE crime SET lng=NULL
   WHERE (lng='') OR (ABS(lng-0.0)<0.01)")

res <- dbSendQuery(con, "
   SELECT point_x,point_y,lat,lng
   FROM crime")
a <- dbFetch(res, n = -1)
nrow(a)
a[1:3,]
dbClearResult(res)

#plotting - no map
i <- sample(1:nrow(a), 10000)
plot(a$lng[i],a$lat[i],
     pch=".",
     xlab="Longitude",ylab="Latitude")

dbSendQuery(con, "
   UPDATE crime SET lng=NULL
   WHERE lng < -76")
dbSendQuery(con, "
   UPDATE crime SET lat=NULL
   WHERE lat < 39")

#rerun plot if first time going through script - if db and crime tables not created already
res <- dbSendQuery(con, "
   SELECT point_x,point_y,lat,lng
   FROM crime")
a <- dbFetch(res, n = -1)
nrow(a)
a[1:3,]
dbClearResult(res)

i <- sample(1:nrow(a), 10000)
plot(a$lng[i],a$lat[i],
     pch=".",
     xlab="Longitude",ylab="Latitude")

#to get hot spot map with all crime types, set up with this: 
#res <- dbSendQuery(con, "
#                   SELECT lat,lng
#                   FROM crime")
#a <- dbFetch(res, n = -1)
#dbClearResult(res)

#to get hot spot map specifically for drug crimes, set up with this:
res <- dbSendQuery(con, "
                   SELECT lat, lng
                   FROM crime
                   WHERE text_general_code = '\"Narcotic / Drug Law Violations\"'")
#can also use WHERE ucr_general=1800
a <- dbFetch(res, n = -1)
dbClearResult(res)

##########################  MAP  ######################################
philly.map <- ggmap(get_map(c(-75.3, 39.89, -75, 40.1),
                             scale="auto",source="stamen"))
philly.map

#basic map
i <- sample(1:nrow(a),1000)
philly.map +
   geom_point(aes(x=lng,y=lat), data=a[i,],
              alpha=0.5, color="blue", size = 1)

philly.map +
   coord_cartesian() +
   stat_binhex(aes(x=lng, y=lat),
               bins=60, alpha = 0.5, data = a)

i <- sample(1:nrow(a),5000)
philly.map +
   stat_density2d(aes(x=lng,y=lat,fill=..level..,alpha=..level..),
                  bins = 20, data = a[i,], geom = 'polygon') +
   scale_fill_gradient('Crime Density',low="green",high="red") +
   scale_alpha(range = c(.4, .75), guide = "none") +
   guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10))

