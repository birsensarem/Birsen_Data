getwd()

############################################################
# VERI MADENCILIGI ODEV 2
# R SCRIPT
# Veri Seti: Palmer Penguins
# Kaynak:
# https://raw.githubusercontent.com/allisonhorst/palmerpenguins/master/inst/extdata/penguins.csv
############################################################

#-----------------------------------------------------------
# 1. Gerekli paketler
#-----------------------------------------------------------
# install.packages(c("readr", "dplyr", "ggplot2", "corrplot", "e1071"))

library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(e1071)

#-----------------------------------------------------------
# 2. Veriyi iceri aktarma
#-----------------------------------------------------------
veri <- read_csv("https://raw.githubusercontent.com/allisonhorst/palmerpenguins/master/inst/extdata/penguins.csv")

cat("Verinin ilk 6 satiri:\n")
print(head(veri))

cat("\nVeri yapisi:\n")
print(str(veri))

#-----------------------------------------------------------
# 3. Gerekli degiskenleri secme
#-----------------------------------------------------------
veri <- veri %>%
  select(species, island, sex, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)

#-----------------------------------------------------------
# 4. Degisken isimlerini Turkcelestirme
#-----------------------------------------------------------
veri <- veri %>%
  rename(
    tur = species,
    ada = island,
    cinsiyet = sex,
    gaga_uzunlugu_mm = bill_length_mm,
    gaga_derinligi_mm = bill_depth_mm,
    yuzgec_uzunlugu_mm = flipper_length_mm,
    vucut_kutlesi_g = body_mass_g  )

cat("\nYeni degisken isimleri:\n")
print(names(veri))

#-----------------------------------------------------------
# 5. Eksik veri kontrolu
#-----------------------------------------------------------
cat("\nEksik veri sayilari:\n")
print(colSums(is.na(veri)))

# Sayisal degiskenleri medyan ile doldurma
veri$gaga_uzunlugu_mm[is.na(veri$gaga_uzunlugu_mm)] <- median(veri$gaga_uzunlugu_mm, na.rm = TRUE)
veri$gaga_derinligi_mm[is.na(veri$gaga_derinligi_mm)] <- median(veri$gaga_derinligi_mm, na.rm = TRUE)
veri$yuzgec_uzunlugu_mm[is.na(veri$yuzgec_uzunlugu_mm)] <- median(veri$yuzgec_uzunlugu_mm, na.rm = TRUE)
veri$vucut_kutlesi_g[is.na(veri$vucut_kutlesi_g)] <- median(veri$vucut_kutlesi_g, na.rm = TRUE)

# Kategorik degiskenleri mod ile doldurma
mod_bul <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
  }

veri$tur[is.na(veri$tur)] <- mod_bul(veri$tur)
veri$ada[is.na(veri$ada)] <- mod_bul(veri$ada)
veri$cinsiyet[is.na(veri$cinsiyet)] <- mod_bul(veri$cinsiyet)

cat("\nEksik veri doldurulduktan sonraki durum:\n")
print(colSums(is.na(veri)))

#-----------------------------------------------------------
# 6. Temel istatistiksel ozetler
#-----------------------------------------------------------
sayisal_veri <- veri %>%
  select(gaga_uzunlugu_mm, gaga_derinligi_mm, yuzgec_uzunlugu_mm, vucut_kutlesi_g)

cat("\nTemel istatistiksel ozetler:\n")
print(summary(sayisal_veri))

#-----------------------------------------------------------
# 7. Carpiklik ve basiklik hesaplama
#-----------------------------------------------------------
cat("\nCarpiklik degerleri:\n")
print(apply(sayisal_veri, 2, skewness))

cat("\nBasiklik degerleri:\n")
print(apply(sayisal_veri, 2, kurtosis))

#-----------------------------------------------------------
# 8. Aykiri deger analizi (IQR)
#-----------------------------------------------------------
aykiri_kontrol <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_deger <- Q3 - Q1
  alt_sinir <- Q1 - 1.5 * IQR_deger
  ust_sinir <- Q3 + 1.5 * IQR_deger
  x < alt_sinir | x > ust_sinir
}

aykiri_satir <- aykiri_kontrol(veri$gaga_uzunlugu_mm) |
  aykiri_kontrol(veri$gaga_derinligi_mm) |
  aykiri_kontrol(veri$yuzgec_uzunlugu_mm) |
  aykiri_kontrol(veri$vucut_kutlesi_g)

cat("\nAykiri deger iceren satir sayisi:\n")
print(sum(aykiri_satir))

veri_temiz <- veri[!aykiri_satir, ]

cat("\nAykiri degerler cikarildiktan sonraki gozlem sayisi:\n")
print(nrow(veri_temiz))

#-----------------------------------------------------------
# 9. Histogram
#-----------------------------------------------------------
ggplot(veri_temiz, aes(x = vucut_kutlesi_g)) +
  geom_histogram(bins = 20) +
  labs(
    title = "Vucut Kutlesi Dagilimi",
    x = "Vucut Kutlesi (gram)",
    y = "Frekans"
  )

#-----------------------------------------------------------
# 10. Korelasyon matrisi
#-----------------------------------------------------------
korelasyon_matrisi <- cor(veri_temiz %>%
                            select(gaga_uzunlugu_mm, gaga_derinligi_mm, yuzgec_uzunlugu_mm, vucut_kutlesi_g))

cat("\nKorelasyon matrisi:\n")
print(korelasyon_matrisi)

plot.new()
dev.off()


corrplot(korelasyon_matrisi, method = "color", addCoef.col = "black")


#-----------------------------------------------------------
# 11. Basit model: Lineer regresyon
# Hedef degisken: vucut_kutlesi_g
#-----------------------------------------------------------
set.seed(123)

n <- nrow(veri_temiz)
egitim_indeks <- sample(1:n, size = round(0.7 * n))

egitim_veri <- veri_temiz[egitim_indeks, ]
test_veri <- veri_temiz[-egitim_indeks, ]

model <- lm(vucut_kutlesi_g ~ gaga_uzunlugu_mm + gaga_derinligi_mm + yuzgec_uzunlugu_mm,
            data = egitim_veri)

cat("\nModel ozeti:\n")
print(summary(model))

#-----------------------------------------------------------
# 12. Tahmin ve performans olcumu
#-----------------------------------------------------------
tahmin <- predict(model, newdata = test_veri)

MAE <- mean(abs(test_veri$vucut_kutlesi_g - tahmin))
SSE <- sum((test_veri$vucut_kutlesi_g - tahmin)^2)
SST <- sum((test_veri$vucut_kutlesi_g - mean(test_veri$vucut_kutlesi_g))^2)
R2 <- 1 - (SSE / SST)

cat("\nModel performans metrikleri:\n")
cat("MAE =", MAE, "\n")
cat("R2 =", R2, "\n")

#-----------------------------------------------------------
# 13. If kosulu
#-----------------------------------------------------------
if (R2 > 0.70) {
  cat("\nModelin aciklayicilik duzeyi yuksektir.\n")
} else {
  cat("\nModelin aciklayicilik duzeyi sinirlidir. Gelistirme gerekebilir.\n")
}

#-----------------------------------------------------------
#-----------------------------------------------------------
# 14. While dongusu
#-----------------------------------------------------------
sayac <- 1
toplam <- 0

while (sayac <= 5) {
  toplam <- toplam + sayac
  sayac <- sayac + 1
}

cat("\nWhile dongusu sonucu (1'den 5'e kadar toplami):\n")
print(toplam)

############################################################
# SCRIPT SONU
############################################################


