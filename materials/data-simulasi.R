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
nwalmur <- 80

#----
# Mekanisme Simpson's Paradox untuk neu dan hi:
# Intercept sekolah berkorelasi POSITIF dengan mean neu DAN mean hi di tiap sekolah.
# Akibatnya, OLS pooled menunjukkan korelasi positif untuk kedua prediktor tsb
# (karena sekolah dengan neu/hi tinggi juga punya baseline mandiri tinggi).
# MLM yang memperhitungkan struktur nested memulihkan slope negatif yang sebenarnya.
#
# trust: konsisten — slope positif baik dalam OLS maupun MLM.

#----
# Slope within-school per prediktor (= efek yang dipulihkan MLM)
# neu dan hi: negatif, bervariasi antar sekolah (random slopes)
# trust: positif, konsisten di semua sekolah

neu_slopes  <- c(-0.4, -0.6, -0.3, -0.7, -0.5)
hi_slopes   <- c(-0.6, -0.4, -0.8, -0.5, -0.7)
trust_slope <- 0.43

# Intercept sekolah — semakin tinggi nomor sekolah, semakin tinggi intercept
# (berkorelasi positif dengan mean neu dan mean hi di sekolah tsb)
int <- c(10, 20, 30, 40, 50)

#----
# Tentukan parameter di masing-masing sekolah
# Sekolah 1: neu rendah (10), hi rendah (10), intercept rendah (10)
a1 <- rnorm(nwalmur, 10, 3)
b1 <- rnorm(nwalmur, 17, 5)
c1 <- rnorm(nwalmur, 10, 3)

# Sekolah 2: neu sedang-rendah (15), hi sedang-rendah (15), intercept 20
a2 <- rnorm(nwalmur, 15, 3)
b2 <- rnorm(nwalmur, 16, 3)
c2 <- rnorm(nwalmur, 15, 3)

# Sekolah 3: neu sedang (20), hi sedang (20), intercept 30
a3 <- rnorm(nwalmur, 20, 3)
b3 <- rnorm(nwalmur, 18, 3)
c3 <- rnorm(nwalmur, 20, 3)

# Sekolah 4: neu sedang-tinggi (25), hi sedang-tinggi (25), intercept 40
a4 <- rnorm(nwalmur, 25, 3)
b4 <- rnorm(nwalmur, 21, 6)
c4 <- rnorm(nwalmur, 25, 3)

# Sekolah 5: neu tinggi (30), hi tinggi (30), intercept tinggi (50)
a5 <- rnorm(nwalmur, 30, 3)
b5 <- rnorm(nwalmur, 16, 2)
c5 <- rnorm(nwalmur, 30, 3)

#----
# Bikin persamaan regresinya
# mandiri = intercept_sekolah + neu*slope_neu + trust*slope_trust + hi*slope_hi + error

sekolah1 <- int[1] + a1*neu_slopes[1] + b1*trust_slope + c1*hi_slopes[1] + rnorm(nwalmur, sd=3)
sekolah2 <- int[2] + a2*neu_slopes[2] + b2*trust_slope + c2*hi_slopes[2] + rnorm(nwalmur, sd=4)
sekolah3 <- int[3] + a3*neu_slopes[3] + b3*trust_slope + c3*hi_slopes[3] + rnorm(nwalmur, sd=2)
sekolah4 <- int[4] + a4*neu_slopes[4] + b4*trust_slope + c4*hi_slopes[4] + rnorm(nwalmur, sd=5)
sekolah5 <- int[5] + a5*neu_slopes[5] + b5*trust_slope + c5*hi_slopes[5] + rnorm(nwalmur, sd=3)

#----
# Bikin dataframe

sekolah1 <- data.frame(neu=a1, trust=b1, hi=c1, mandiri=sekolah1)
sekolah2 <- data.frame(neu=a2, trust=b2, hi=c2, mandiri=sekolah2)
sekolah3 <- data.frame(neu=a3, trust=b3, hi=c3, mandiri=sekolah3)
sekolah4 <- data.frame(neu=a4, trust=b4, hi=c4, mandiri=sekolah4)
sekolah5 <- data.frame(neu=a5, trust=b5, hi=c5, mandiri=sekolah5)

rm(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,
   int,neu_slopes,hi_slopes,trust_slope)

#----
# Disatukan dalam satu dataset

datasekolah <- rbind(sekolah1,sekolah2,sekolah3,sekolah4,sekolah5)

#----
# Tambahkan variabel id
datasekolah$id <- seq(1:nrow(datasekolah)) # kasih nomor urut partisipan
datasekolah$idsekolah <- c(rep(1, nwalmur), rep(2, nwalmur), rep(3, nwalmur),
                           rep(4, nwalmur), rep(5, nwalmur)) # kasih nomor urut sekolah

#----
# Cek scatterplot per sekolah

theme_set(theme_bw(base_size = 12, base_family = "")) # set tema ggplot2

# neuroticism
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

write_csv(datasekolah, file="dataset-sekolah.csv")
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

org1 <- a1*con - b1*inf - c1*pow + rnorm(npekerja, sd=4) # persamaan garis regresi di organisasi 1
org2 <- -a2*con + b2*inf - c2*pow + rnorm(npekerja, sd=2) # persamaan garis regresi di organisasi 2
org3 <- a3*con + b3*inf - c3*pow + rnorm(npekerja, sd=3) # persamaan garis regresi di organisasi 3

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


# Dataset 3: Kecemasan selama masa ujian (Repeated Measures) -----------------
# Skenario: Nisa mengukur kecemasan 120 mahasiswa dari 4 fakultas sebanyak 4 kali:
#   T1 = 2 minggu sebelum UAS
#   T2 = 1 minggu sebelum UAS
#   T3 = Saat UAS berlangsung
#   T4 = 2 minggu sesudah UAS
# Variabel level-2 (person): neuro (neuroticism, skor 10-70)
# Variabel level-1 (observasi): cemas (kecemasan, 0-100), t (waktu, 1-4)
# Struktur kovarians residual yang diharapkan: Compound Symmetry
# ICC target ~0.40

set.seed(778899)

n_person_rm <- 120
n_waktu     <- 4

# --- parameter level-2 (person-level)
neuro_mean  <- 40
neuro_sd    <- 10

# --- fixed effects
# intercept = rata-rata cemas di T1, lalu delta relatif terhadap T1
beta0_rm   <- 50               # baseline di T1
delta_t    <- c(0, 15, 25, -5) # T1, T2, T3, T4 (rata-rata)
beta_neuro <- 0.4              # setiap +1 neuro -> +0.4 cemas (baseline)

# --- variance components
sigma_u0_rm <- 8   # SD random intercept antar individu
sigma_e_rm  <- 6   # SD residual Level-1 (iid -> menghasilkan Compound Symmetry)

# ICC = sigma_u0^2 / (sigma_u0^2 + sigma_e^2)
cat("ICC (dataset-rm) =", round(sigma_u0_rm^2 / (sigma_u0_rm^2 + sigma_e_rm^2), 3), "\n")

# --- buat data level-2
data_person_rm <- data.frame(
  id       = 1:n_person_rm,
  fakultas = rep(1:4, each = n_person_rm / 4),
  neuro    = round(rnorm(n_person_rm, neuro_mean, neuro_sd), 1),
  u0       = rnorm(n_person_rm, 0, sigma_u0_rm)
)

# --- buat data long (satu baris per observasi)
data_rm <- data_person_rm[rep(1:n_person_rm, each = n_waktu), ]
data_rm$t <- rep(1:n_waktu, times = n_person_rm)

data_rm$cemas <- beta0_rm +
                 delta_t[data_rm$t] +
                 beta_neuro * (data_rm$neuro - neuro_mean) +
                 data_rm$u0 +
                 rnorm(nrow(data_rm), 0, sigma_e_rm)

# bulatkan dan batasi 0-100
data_rm$cemas <- round(pmax(0, pmin(100, data_rm$cemas)), 1)

# hapus kolom bantu u0, urutkan kolom
data_rm <- data_rm[, c("id", "fakultas", "t", "cemas", "neuro")]
rownames(data_rm) <- NULL

# --- cek: rata-rata cemas per waktu
cat("Rata-rata cemas per time point:\n")
print(aggregate(cemas ~ t, data = data_rm, FUN = mean))

# --- cek plot trajektori
rm_plot <- ggplot(data_rm, aes(x = t, y = cemas, group = id)) +
  geom_line(alpha = 0.15, colour = "steelblue") +
  stat_summary(aes(group = 1), fun = mean, geom = "line",
               colour = "#E6282B", linewidth = 1.5) +
  scale_x_continuous(breaks = 1:4,
                     labels = c("T1\n(2 mgg sblm)", "T2\n(1 mgg sblm)",
                                "T3\n(Saat UAS)", "T4\n(2 mgg stlh)")) +
  labs(x = "Waktu", y = "Skor Kecemasan",
       title = "Trajektori kecemasan per individu (merah = rata-rata)") +
  theme_bw()
rm_plot

# --- simpan
write_csv(data_rm, file = "dataset-rm.csv")
rm(data_person_rm, data_rm, rm_plot,
   n_person_rm, n_waktu, neuro_mean, neuro_sd,
   beta0_rm, delta_t, beta_neuro, sigma_u0_rm, sigma_e_rm)


# Dataset 4: Experience Sampling Method (ESM) - Afek positif dan stres harian
# Skenario: Joko merekrut 80 mahasiswa, mengisi survei 5x/hari selama 7 hari
#   = 35 beep per orang, 2.800 observasi total
# Variabel level-2 (person): er (emotion regulation, skor 10-95)
# Variabel level-1 (beep): pa (afek positif, 0-100), stres (0-100)
# Efek yang disimulasikan:
#   - Within-person: stres_centered -> pa (negatif, ~-0.35)
#   - Between-person: stres_mean -> pa (negatif, ~-0.40)
#   - Cross-level interaction: stres_centered x er -> pa (buffer, ~+0.008)
# Struktur kovarians residual: AR(1) dengan rho ~0.35
# ICC target ~0.30

set.seed(112233)

n_person_esm  <- 80
n_hari        <- 7
n_beep_hari   <- 5
n_obs_person  <- n_hari * n_beep_hari  # 35 per orang

# --- parameter level-2
er_mean    <- 50
er_sd      <- 15

# --- fixed effects
beta0_esm          <- 60     # rata-rata pa keseluruhan
beta_within_stres  <- -0.35  # within-person: stres_centered -> pa
beta_between_stres <- -0.40  # between-person: stres_mean -> pa (grand mean centered)
beta_er_main       <- 0.25   # er -> pa (between-person)
beta_cli           <- 0.008  # cross-level interaction: stres_centered x er

# --- variance components
sigma_u0_esm <- 10    # SD random intercept
sigma_u1_esm <- 0.15  # SD random slopes (variabilitas reaktivitas stres antar orang)
sigma_e_esm  <- 8     # SD inovasi AR(1)
rho_ar1      <- 0.35  # koefisien autokorelasi AR(1)

# --- parameter stres
stres_grand_mean <- 45
stres_person_sd  <- 15   # SD rata-rata stres antar orang
stres_within_sd  <- 12   # SD fluktuasi stres dalam orang

cat("ICC (dataset-esm) =",
    round(sigma_u0_esm^2 / (sigma_u0_esm^2 + sigma_e_esm^2 / (1 - rho_ar1^2)), 3), "\n")

# --- buat data level-2
data_person_esm <- data.frame(
  id         = 1:n_person_esm,
  er         = round(pmax(10, pmin(95, rnorm(n_person_esm, er_mean, er_sd))), 1),
  stres_mean_person = round(pmax(5, pmin(90, rnorm(n_person_esm, stres_grand_mean, stres_person_sd))), 1),
  u0         = rnorm(n_person_esm, 0, sigma_u0_esm),
  u1         = rnorm(n_person_esm, 0, sigma_u1_esm)
)

# --- fungsi untuk membuat residual AR(1)
buat_ar1 <- function(n, rho, sigma) {
  e <- numeric(n)
  e[1] <- rnorm(1, 0, sigma)
  for (k in 2:n) {
    e[k] <- rho * e[k-1] + rnorm(1, 0, sigma * sqrt(1 - rho^2))
  }
  e
}

# --- buat data per orang, lalu gabungkan
esm_list <- vector("list", n_person_esm)

for (i in 1:n_person_esm) {
  p <- data_person_esm[i, ]

  # fluktuasi stres sesaat orang ini (bervariasi di sekitar rata-ratanya)
  stres_raw     <- round(pmax(0, pmin(100, rnorm(n_obs_person, p$stres_mean_person, stres_within_sd))), 1)
  stres_centered <- stres_raw - p$stres_mean_person

  # bagian fixed + random dari pa
  pa_fixed <- beta0_esm +
    (beta_within_stres + p$u1) * stres_centered +
    beta_between_stres * (p$stres_mean_person - stres_grand_mean) +
    beta_er_main * (p$er - er_mean) +
    beta_cli * stres_centered * (p$er - er_mean) +
    p$u0

  # tambahkan residual AR(1)
  resid <- buat_ar1(n_obs_person, rho_ar1, sigma_e_esm)

  pa_raw <- round(pmax(0, pmin(100, pa_fixed + resid)), 1)

  esm_list[[i]] <- data.frame(
    id         = p$id,
    er         = p$er,
    hari       = rep(1:n_hari, each = n_beep_hari),
    beep       = rep(1:n_beep_hari, times = n_hari),
    beep_total = 1:n_obs_person,
    stres      = stres_raw,
    pa         = pa_raw
  )
}

data_esm <- do.call(rbind, esm_list)
rownames(data_esm) <- NULL

# --- cek struktur
cat("Total observasi:", nrow(data_esm), "\n")
cat("Rata-rata pa   :", round(mean(data_esm$pa), 2), "\n")
cat("Rata-rata stres:", round(mean(data_esm$stres), 2), "\n")

# cek rata-rata autokorelasi lag-1 per orang (harus mendekati rho_ar1)
acf_cek <- sapply(split(data_esm$pa, data_esm$id),
                  function(x) cor(head(x, -1), tail(x, -1)))
cat("Rata-rata autokorelasi lag-1:", round(mean(acf_cek), 3),
    "(target:", rho_ar1, ")\n")

# --- cek plot: trajektori 9 orang pertama
esm_plot1 <- ggplot(subset(data_esm, id <= 9),
                    aes(x = beep_total, y = pa)) +
  facet_wrap(~ id, ncol = 3) +
  geom_line(colour = "#14497F", alpha = 0.8) +
  labs(x = "Beep ke-", y = "Afek Positif (pa)",
       title = "Trajektori pa: 9 partisipan pertama") +
  theme_bw()
esm_plot1

# cek plot: hubungan stres-pa (raw)
esm_plot2 <- ggplot(data_esm, aes(x = stres, y = pa)) +
  geom_point(alpha = 0.05, colour = "#14497F") +
  geom_smooth(method = "lm", colour = "#E6282B") +
  labs(x = "Stres sesaat", y = "Afek positif sesaat",
       title = "Hubungan stres-pa (level individual, tanpa centering)") +
  theme_bw()
esm_plot2

# --- simpan
write_csv(data_esm, file = "dataset-esm.csv")
rm(esm_list, data_person_esm, data_esm, buat_ar1, acf_cek, esm_plot1, esm_plot2,
   i, p, stres_raw, stres_centered, pa_fixed, resid, pa_raw,
   n_person_esm, n_hari, n_beep_hari, n_obs_person,
   er_mean, er_sd, beta0_esm, beta_within_stres, beta_between_stres,
   beta_er_main, beta_cli, sigma_u0_esm, sigma_u1_esm, sigma_e_esm, rho_ar1,
   stres_grand_mean, stres_person_sd, stres_within_sd)
