library(lubridate)

# analisis penggunaan sepeda tiap hari ----
tripdata_day <- tripdata %>%
  drop_na() %>%
  group_by(hari = weekdays(started_at), member_casual) %>%
  summarize(jumlah = length(member_casual))

tripdata_day$hari <- factor(tripdata_day$hari, 
                            levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday',
                                       'Friday', 'Saturday', 'Sunday'))

tripdata_day %>%
  ggplot(aes(x=hari, y=jumlah, group=member_casual, color=member_casual)) +
  geom_line(size=1.2) +
  labs(title="Penggunaan sepeda tiap hari pada tahun 2021") +
  theme(legend.position = "bottom")

# analisis penggunaan sepeda tiap bulan ----
tripdata_month <- tripdata %>%
  drop_na() %>%
  group_by(bulan = month(started_at), member_casual) %>%
  summarize(jumlah = length(member_casual))

options(scipen=10000)

tripdata_month %>%
  ggplot(aes(x=factor(bulan), y=jumlah, fill=member_casual)) +
  geom_bar(stat="identity", position = "dodge") +
  facet_grid(~member_casual) +
  xlab("bulan") +
  labs(title="Penggunaan sepeda tiap bulan pada tahun 2021") +
  guides(fill=FALSE)

# analisis penggunaan sepeda berdasarkan jenis sepeda ----
tripdata_ridetype <- tripdata %>%
  drop_na() %>%
  group_by(rideable_type, member_casual) %>%
  summarize(jumlah = length(rideable_type))

tripdata_ridetype %>%
  ggplot(aes(x=rideable_type, y=jumlah, fill=member_casual)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label = jumlah), vjust = -.5,
            size = 3.5, position = position_dodge(.9)) +
  labs(title="Penggunaan jenis sepeda pada tahun 2021") +
  ylim(0, 2100000)

tripdata_ridetype %>%
  ggplot(aes(x=rideable_type, y=jumlah, fill=member_casual)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Penggunaan jenis sepeda pada tahun 2021") +
  ylim(0, 2100000)

# analisis penggunaan sepeda berdasarkan waktu pakai rata-rata ----
tripdata_waktupakai <- tripdata %>%
  drop_na() %>%
  group_by(member_casual) %>%
  summarize(waktu = mean(waktu_pakai / 60))

tripdata_waktupakai %>%
  ggplot(aes(x=member_casual, y=as.double(waktu), fill=member_casual)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Rata-rata waktu pemakaian sepeda tahun 2021") +
  guides(fill=FALSE) +
  ylab("waktu (menit)") +
  xlab("")

# Analisis penggunaan sepeda berdasarkan jarak tempuh rata-rata ----
library(geosphere)

tripdata$jarak_tempuh <- distHaversine(cbind(tripdata$start_lat, tripdata$start_lng),
                                      cbind(tripdata$end_lat, tripdata$end_lng))

tripdata_jarak <- tripdata %>%
  drop_na() %>%
  group_by(member_casual) %>%
  summarize(jarak_tempuh = mean(jarak_tempuh))

tripdata_jarak %>%
  ggplot(aes(x=member_casual, y=jarak_tempuh, fill=member_casual)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Rata-rata jarak tempuh pengguna sepeda tahun 2021") +
  guides(fill=FALSE) +
  ylab("jarak (meter)") +
  xlab("")

# Analisis perbandingan jumlah tipe pengguna tahun 2021 ----
tripdata %>%
  drop_na() %>%
  group_by(member_casual) %>%
  summarize(jumlah = length(member_casual)) %>%
    ggplot(aes(x=member_casual, y=jumlah, fill=member_casual)) +
    geom_bar(stat="identity") +
    labs(title="Jumlah tipe pengguna tahun 2021") +
    guides(fill=FALSE) +
    xlab("")

# Analisis tingkat kesibukan stasiun tahun 2021 ----

tripdata_top10stationmember <- tripdata %>%
  drop_na() %>%
  filter(member_casual == 'member') %>%
  group_by(start_station_name) %>%
  summarize(jumlah_aktivitas = length(start_station_name)) %>%
  top_n(11, jumlah_aktivitas)

tripdata_top10stationmember <- tripdata_top10stationmember[2:11,]

tripdata_top10stationmember %>%
  mutate(start_station_name = fct_reorder(start_station_name, jumlah_aktivitas)) %>%
  ggplot(aes(x=start_station_name, y=jumlah_aktivitas)) +
  geom_bar(orientation="h", stat="identity", fill="cyan4") +
  coord_flip() +
  xlab("nama stasiun") +
  labs(title="Stasiun paling banyak digunakan pengguna member 2021") +
  theme(plot.title = element_text(size=11, face="bold")) +
  guides(fill=FALSE)

# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
tripdata_top10stationcasual <- tripdata %>%
  drop_na() %>%
  filter(member_casual == 'casual') %>%
  group_by(start_station_name) %>%
  summarize(jumlah_aktivitas = length(start_station_name)) %>%
  top_n(11, jumlah_aktivitas)

tripdata_top10stationcasual <- tripdata_top10stationcasual[2:11,]

tripdata_top10stationcasual %>%
  mutate(start_station_name = fct_reorder(start_station_name, jumlah_aktivitas)) %>%
  ggplot(aes(x=start_station_name, y=jumlah_aktivitas, fill="red")) +
  geom_bar(orientation="h", stat="identity") +
  coord_flip() +
  xlab("nama stasiun") +
  labs(title="Stasiun paling banyak digunakan pengguna casual 2021") +
  theme(plot.title = element_text(size=11, face="bold")) +
  guides(fill=FALSE)
