#----
# Script ini adalah perintah yang digunakan untuk membuat dataset latihan Workshop lme 
# Script ini digunakan untuk membuat 2 dataset, yaitu kemandirian siswa PAUD dan perilaku tidak beradab

#----
# muat paketnya dulu (pasang dulu apabila perlu)
library(tidyverse)
library(readr)

# Dataset 1: Kemandirian siswa PAUD ---------------------------------------
# Seorang peneliti mencoba untuk mengobservasi faktor-faktor yang membuat kemandirian siswa PAUD di Surabaya bervariasi
# Peneliti menduga bahwa kepribadian ibu (neuroticism), trust in organismic development (percaya bahwa anak berkembang secara natural), dan pendapatan rumah tangga (household income) mungkin punya pengaruh pada kemandirian siswa
# Penelitian ini dilakukan di 5 PAUD di Surabaya, dimana ukuran sampel di masing-masing sekolah adalah 80

#----
# tentuin n masing-masing subgroup
set.seed(334455)
nwalmur=80


#----
# tentukan slope masing2 variabel prediktor

neu=-0.52 # slope utk neuroticism
trust=0.43 # slope utk trust in organismic development
hi=-0.74 # slope utk household income

#----
# tentukan parameter di masing2 sekolah

# Sekolah 1
a1 <- rnorm(nwalmur, 20, 4)
b1 <- rnorm(nwalmur, 17, 5)
c1 <- rnorm(nwalmur, 25, 3)

# Sekolah 2
a2 <- rnorm(nwalmur, 13, 2)
b2 <- rnorm(nwalmur, 16, 3)
c2 <- rnorm(nwalmur, 10, 3)

# Sekolah 3
a3 <- rnorm(nwalmur, 15, 4)
b3 <- rnorm(nwalmur, 18, 3)
c3 <- rnorm(nwalmur, 17, 3)

# Sekolah 4
a4 <- rnorm(nwalmur, 25, 5)
b4 <- rnorm(nwalmur, 21, 6)
c4 <- rnorm(nwalmur, 28, 3)

# Sekolah 5
a5 <- rnorm(nwalmur, 15, 3)
b5 <- rnorm(nwalmur, 16, 2)
c5 <- rnorm(nwalmur, 22, 3)

#----
# Bikin persamaan regresinya

sekolah1 <- a1*neu + b1*trust + c1*hi + rnorm(nwalmur, sd=3) # persamaan garis regresi di sekolah 1
sekolah2 <- a2*neu + b2*trust - c2*hi + rnorm(nwalmur, sd=4) # persamaan garis regresi di sekolah 2
sekolah3 <- a3*neu + b3*trust + c3*hi + rnorm(nwalmur, sd=2) # persamaan garis regresi di sekolah 3
sekolah4 <- a4*neu + b4*trust - c4*hi + rnorm(nwalmur, sd=5) # persamaan garis regresi di sekolah 4
sekolah5 <- a5*neu + b5*trust + c5*hi + rnorm(nwalmur, sd=3) # persamaan garis regresi di sekolah 5

#----
# Bikin dataframe

sekolah1 <- data.frame(neu=a1, trust=b1, hi=c1, mandiri=sekolah1)
sekolah2 <- data.frame(neu=a2, trust=b2, hi=c2, mandiri=sekolah2)
sekolah3 <- data.frame(neu=a3, trust=b3, hi=c3, mandiri=sekolah3)
sekolah4 <- data.frame(neu=a4, trust=b4, hi=c4, mandiri=sekolah4)
sekolah5 <- data.frame(neu=a5, trust=b5, hi=c5, mandiri=sekolah5)

rm(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,hi,neu,trust)

#----
# Disatukan dalam satu dataset

datasekolah <- rbind(sekolah1,sekolah2,sekolah3,sekolah4,sekolah5)
View(datasekolah)

#----
# Tambahkan variabel id
datasekolah$id <- seq(1:nrow(datasekolah)) # kasih nomor urut partisipan
datasekolah$idsekolah <- c(rep(1, nwalmur), rep(2, nwalmur), rep(3, nwalmur),
                           rep(4, nwalmur), rep(5, nwalmur)) # kasih nomor urut sekolah


#----
# Cek scatterplot per sekolah

theme_set(theme_bw(base_size = 12, base_family = "")) # set tema ggplot2

# neuroticsm

neu.plot <- ggplot(data=datasekolah, aes(x=neu, y=mandiri, group=idsekolah)) +
  facet_grid( ~ idsekolah) +
  geom_point(aes(colour=idsekolah)) +
  geom_smooth(method="lm", se=TRUE, aes(colour=idsekolah)) +
  xlab("Neuroticism Ibu") + ylab("Kemandirian Siswa PAUD") +
  theme(legend.position="none")
neu.plot

# trust in organismic development

trust.plot <- ggplot(data=datasekolah, aes(x=trust, y=mandiri, group=idsekolah)) +
  facet_grid( ~ idsekolah) +
  geom_point(aes(colour=idsekolah)) +
  geom_smooth(method="lm", se=TRUE, aes(colour=idsekolah)) +
  xlab("Trust in Organismic Development Ibu") + ylab("Kemandirian Siswa PAUD") +
  theme(legend.position="none")
trust.plot

# household income

hi.plot <- ggplot(data=datasekolah, aes(x=hi, y=mandiri, group=idsekolah)) +
  facet_grid( ~ idsekolah) +
  geom_point(aes(colour=idsekolah)) +
  geom_smooth(method="lm", se=TRUE, aes(colour=idsekolah)) +
  xlab("Pendapatan Keluarga per Bulan") + ylab("Kemandirian Siswa PAUD") +
  theme(legend.position="none")
hi.plot

#----
# simpan dalam bentuk csv

write_csv(datasekolah, path="dataset-sekolah.csv")
rm(nwalmur,datasekolah,hi.plot,neu.plot,trust.plot,sekolah1,sekolah2,sekolah3,sekolah4,sekolah5)

# Dataset 2: Perilaku tidak beradab di kantor ---------------------------------------

# Peneliti ingin menginvestigasi penyebab pekerja melakukan perilaku tidak beradab (workplace incivility), yang meliputi, bullying, agresi verbal, penyiksaan emosional (emotional abuse), dll.
# Peneliti menduga ada kaitan antara conscientiousness, persepsi atas workplace informality, dan jarak kuasa (power distance - level 2)
# Penelitian ini dilakukan di 3 organisasi yang berbeda di Surabaya

#----
# tentuin n masing-masing subgroup
npekerja=150

#----
# tentukan slope masing2 variabel prediktor

con=-0.42 # slope utk conscientiousness
inf=0.35 # slope utk workplace informality
pow=-0.86 # slope utk power distance

#----
# tentukan parameter di masing2 organisasi

# Organisasi 1
a1 <- rnorm(npekerja, 22, 5)
b1 <- rnorm(npekerja, 18, 3)
c1=13

# Organisasi 2
a2 <- rnorm(npekerja, 38, 3)
b2 <- rnorm(npekerja, 10, 4)
c2=25 

# Organisasi 3
a3 <- rnorm(npekerja, 32, 4)
b3 <- rnorm(npekerja, 14, 2)
c3=18

#----
# Bikin persamaan regresinya

org1 <- a1*con - b1*inf + c1*pow + rnorm(npekerja, sd=4) # persamaan garis regresi di organisasi 1
org2 <- -a2*con + b2*inf + c2*pow + rnorm(npekerja, sd=2) # persamaan garis regresi di organisasi 2
org3 <- a3*con + b3*inf + c3*pow + rnorm(npekerja, sd=3) # persamaan garis regresi di organisasi 3

#----
# Bikin dataframe

org1 <- data.frame(con=a1, inf=b1, pow=c1, incivil=org1)
org2 <- data.frame(con=a2, inf=b2, pow=c2, incivil=org2)
org3 <- data.frame(con=a3, inf=b3, pow=c3, incivil=org3)

rm(a1,a2,a3,b1,b2,b3,c1,c2,c3,con,inf,pow)

#----
# Disatukan dalam satu dataset

dataorg <- rbind(org1,org2,org3)
rm(org1,org2,org3)
View(dataorg)

#----
# Tambahkan variabel id
dataorg$id <- seq(1:nrow(dataorg)) # kasih nomor urut partisipan
dataorg$idorg <- c(rep(1, npekerja), rep(2, npekerja), rep(3, npekerja)) # kasih nomor urut organisasi

#----
# Cek scatterplot per organisasi

theme_set(theme_bw(base_size = 12, base_family = "")) # set tema ggplot2

# conscientiousness

con.plot <- ggplot(data=dataorg, aes(x=con, y=incivil, group=idorg)) +
  facet_grid( ~ idorg) +
  geom_point(aes(colour=idorg)) +
  geom_smooth(method="lm", se=TRUE, aes(colour=idorg)) +
  xlab("Conscientiousness") + ylab("Perilaku Tidak Beradab") +
  theme(legend.position="none")
con.plot

# persepsi atas workplace informality

inf.plot <- ggplot(data=dataorg, aes(x=inf, y=incivil, group=idorg)) +
  facet_grid( ~ idorg) +
  geom_point(aes(colour=idorg)) +
  geom_smooth(method="lm", se=TRUE, aes(colour=idorg)) +
  xlab("Persepsi atas Workplace Informality") + ylab("Perilaku Tidak Beradab") +
  theme(legend.position="none")
inf.plot

#----
# simpan dalam bentuk csv

write_csv(dataorg, path="dataset-organisasi.csv")
rm(npekerja,con.plot,inf.plot,dataorg)
