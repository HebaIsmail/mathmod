#Задание 1
#Вариант 7
#для региона 79 рассчитайте урожайность пшеницы в 2013 году, 
#взяв для рассчета средние суммы активных температур за текущий год,
#с 14 ближайших метеостанций но убирая из рассчета активных температур дни 
#с температурой выше 30 градусов
#48.784109, 132.929726 - еврейская автономная область, биробиджан

library(tidyverse)
library(rnoaa)
#station_data = ghcnd_stations()
station_data = read.csv("station_data.csv")
birobidzhan = data.frame(id="BIROBIDZHAN", latitude = 48.784109, longitude = 132.929726)
birobidzhan_around = meteo_nearby_stations(lat_lon_df = birobidzhan, station_data = station_data,
                                           limit = 14, var = c("PRCP", "TAVG"),
                                           year_min=2013, year_max = 2013)
all_birobidzhan_data = vector();
for (i in 1:14) 
{
  birobidzhan_id = birobidzhan_around[["BIROBIDZHAN"]][["id"]][i]
  all_birobidzhan_data = c(all_birobidzhan_data, meteo_tidy_ghcnd(stationid = birobidzhan_id))
  f_birobidzhan_data = filter(all_birobidzhan_data, date>='2013-01-01' & date<='2013-12-31')
}
dbm = vector()
dbm[1] = filter(f_birobidzhan_data, date>='2013-01-01' & date<='2013-01-31')[4]
dbm[2] = filter(f_birobidzhan_data, date>='2013-02-01' & date<='2013-02-28')[4]
dbm[3] = filter(f_birobidzhan_data, date>='2013-03-01' & date<='2013-03-31')[4]
dbm[4] = filter(f_birobidzhan_data, date>='2013-04-01' & date<='2013-04-30')[4]
dbm[5] = filter(f_birobidzhan_data, date>='2013-05-01' & date<='2013-05-31')[4]
dbm[6] = filter(f_birobidzhan_data, date>='2013-06-01' & date<='2013-06-30')[4]
dbm[7] = filter(f_birobidzhan_data, date>='2013-07-01' & date<='2013-07-31')[4]
dbm[8] = filter(f_birobidzhan_data, date>='2013-08-01' & date<='2013-08-31')[4]
dbm[9] = filter(f_birobidzhan_data, date>='2013-09-01' & date<='2013-09-30')[4]
dbm[10] = filter(f_birobidzhan_data, date>='2013-10-01' & date<='2013-10-31')[4]
dbm[11] = filter(f_birobidzhan_data, date>='2013-11-01' & date<='2013-11-30')[4]
dbm[12] = filter(f_birobidzhan_data, date>='2013-12-01' & date<='2013-12-31')[4]
#найдем сумму температур больше 5 по месяцам
s = vector()
for (i in 1:12) {
  s = c(s, sum(dbm[[i]][dbm[[i]]>50], na.rm = TRUE)/10)
}
#найдем коэффициент d для каждого месяца, в соотвестствии с условием: среднедневные температуры не больше 30 %
dm = vector()
for (i in 1:12) {
  dm = c(dm, length(dbm[[i]][dbm[[i]]<30])/length(dbm[[i]]))
}
#перенесем колонки таблицы для расчета в векторы
af = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bf = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
d = c(dm[1],dm[2], dm[3], dm[4], dm[5], dm[6],dm[7],dm[8], dm[9], dm[10], dm[11], dm[12])
#Зададим переменные для расчета
fert = vector()
fert_summ = 0 #обнулим сумму месячных урожайностей
Kf = 300 #Коэффициент использования ФАР
Qj = 1600 # калорийность урожая культуры
Lj = 2.2 # сумма частей основной и побочной продукции
Ej = 25 # стандартная влажность культуры
for(i in 1:12)
{
  fert[i] = af[i] + bf[i] * 1.0 * s[i]
  fert_summ = fert_summ + ((fert[i] * d[i]) * Kf) / (Qj * Lj * (100-Ej))
}
# урожайность пшеницы: 
Yield = 10^6 * fert_summ; Yield
