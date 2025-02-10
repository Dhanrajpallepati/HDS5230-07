library(readxl)
library(geosphere)
library(microbenchmark)

# Read the data
df <- read_excel("clinics.xls")
# Ensure locLat and locLong are numeric
df$locLat <- as.numeric(df$locLat)
df$locLong <- as.numeric(df$locLong)

# Check for NA values after conversion
sum(is.na(df$locLat))
sum(is.na(df$locLong))

df <- df[!is.na(df$locLat) & !is.na(df$locLong), ]

# View the first few rows
head(df)

haversine <- function(lat1, lon1, lat2, lon2) {
  R <- 3959 # Earth's radius in miles
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  c <- 2 * asin(sqrt(a))
  
  return(R * c)
}


haversine_loop <- function(df) {
  distances <- numeric(nrow(df))
  for (i in 1:nrow(df)) {
    distances[i] <- haversine(40.671, -73.985, df$locLat[i], df$locLong[i])
  }
  return(distances)
}


haversine_apply <- function(df) {
  distances <- apply(df, 1, function(row) {
    haversine(40.671, -73.985, as.numeric(row["locLat"]), as.numeric(row["locLong"]))
  })
  return(distances)
}


haversine_vectorized <- function(df) {
  coords1 <- cbind(rep(-73.985, nrow(df)), rep(40.671, nrow(df)))
  coords2 <- cbind(df$locLong, df$locLat)
  return(distHaversine(coords1, coords2) * 0.000621371)
}
results <- microbenchmark(
  for_loop = haversine_loop(df),
  apply_method = haversine_apply(df),
  vectorized = haversine_vectorized(df),
  times = 5
)

print(results)
