glimpse(tripdata)

any(duplicated(tripdata$trip_id))

tripdata <- subset(tripdata, select = -c(ride_id))

# mengubah tipe data kolom started_at and ended_at menjadi dateTime ----
tripdata$started_at <- as.POSIXct(tripdata$started_at,
                                  format='%Y-%m-%d %H:%M:%S')

tripdata$ended_at <- as.POSIXct(tripdata$ended_at,
                                format='%Y-%m-%d %H:%M:%S')

# melihat jumlah missing value dan sebarannya ----
summary(tripdata)

# library lubridate untuk mengambil data bulan pada data dateTime
library(lubridate)

colnames(tripdata)[colSums(is.na(tripdata)) > 0]

tripdata %>%
  filter(is.na(end_lng)) %>%
  nrow()

tripdata %>%
  filter(start_station_id == "") %>%
  nrow()

tripdata %>%
  filter(end_station_id == "") %>%
  nrow()

# ==================== cek nilai kosong di baris tiap nilai rideable_type
tripdata %>%
  filter(is.na(end_lng)) %>% group_by(rideable_type) %>%
  summarize(jumlah_na = length(rideable_type))

tripdata %>%
  filter(end_station_id == "") %>% group_by(rideable_type) %>%
  summarize(jumlah_id = length(rideable_type))

tripdata %>%
  filter(end_station_name == "") %>% group_by(rideable_type) %>%
  summarize(jumlah_nama = length(rideable_type))

tripdata %>%
  group_by(rideable_type) %>%
  summarize(length(rideable_type))

# ==================== cek nilai yang kosong di baris tiap nilai kolom bulan
tripdata %>%
  filter(is.na(end_lng)) %>% group_by(month(started_at)) %>%
  summarize(jumlah_na = length(month(started_at)))

tripdata %>%
  filter(end_station_id == "") %>% group_by(month(started_at)) %>%
  summarize(jumlah_id = length(month(started_at)))

tripdata %>%
  filter(end_station_name == "") %>% group_by(month(started_at)) %>%
  summarize(jumlah_nama = length(month(started_at)))

tripdata %>%
  group_by(month(started_at)) %>%
  summarize(length(month(started_at)))

# ==================== cek nilai kosong di baris tiap nilai member_casual
tripdata %>%
  filter(is.na(end_lng)) %>% group_by(member_casual) %>%
  summarize(jumlah_na = length(member_casual))

tripdata %>%
  filter(end_station_id == "") %>% group_by(member_casual) %>%
  summarize(jumlah_id = length(member_casual))

tripdata %>%
  filter(start_station_name == "") %>% group_by(member_casual) %>%
  summarize(jumlah_nama = length(member_casual))

tripdata %>%
  group_by(member_casual) %>%
  summarize(length(member_casual))

# menghapus baris yang memiliki nilai yang kosong ----
tripdata <- drop_na(tripdata)

# membuat kolom baru yang berisi jarak tempus pengendara sepeda ----
tripdata <- tripdata %>%
  mutate(jarak_tempuh = 100*sqrt((end_lat - start_lat)**2
                                 + (end_lng - start_lng)**2))

# membuat kolom baru yang berisi waktu pemakaian sepeda ----
tripdata <- tripdata %>%
  mutate(waktu_pakai = ended_at - started_at)