# -----------------------------------------------------------------------------
# |   Seminarska po Biznis i statistika                                       |
# |   Analiza na berzata(akciite) na Amazon                                   |
# |   Dimitar Jovanovski 203099                                               |  
# |   Predmet: Biznis i statistika                                            |
# -----------------------------------------------------------------------------


# Vmetnuvanje na databazata AMZN.csv
amzn <- read.csv("AMZN.csv", header=TRUE, sep=",")


# PRV DEL, zadaca 1 -------------------------------------------------------------------------

# MNOZESTVO Open: --------------------------------------------------------------
# prvin ke se pravi analiza na kolonata Open(prvata transakcija na akcijata) t.e pocetna cena
# MIN vrednost na kolonata Open
min_open <- min(amzn$Open)

# MAX vrednost na kolonata Open
max_open <- max(amzn$Open)

# opseg na Open kolonata
opseg_open <- max_open - min_open

# br na klasi
klasi_open <- sqrt( nrow(amzn) )

# golemina na klasa
gol_klasi_open <- opseg_open/klasi_open

# definiranje na granicni tocki(idni intervali)
seq_open <- seq( 1, round(max_open), by=round(opseg_open/10)+1)

# definiranje na intervalite t.e granicite i nivnite frekvencii na pojavuvanja
frekvencii_open <- cut( amzn$Open, seq_open )
amzn$intervali_open <- cut( amzn$Open, seq_open )

# prikaz na tabela so raspredelba na cestoti(frekvencii)
tab_na_cestoti_open <- transform( table(frekvencii_open) )

# Presmetuva sredishna tocka od kraevite na 1 interval
midcut<-function( x, from, to, by ){
  ## cut the data into bins...
  x <- cut( x, seq(from,to,by), include.lowest=T)
  ## make a named vector of the midpoints, names=binnames
  vec=seq(from+by/2, to-by/2,by)
  names(vec)=levels(x)
  ## use the vector to map the names of the bins to the midpoint values
  unname(vec[x])
}
amzn$sredna_t_open <- midcut( amzn$Open, round(min_open), round(max_open), round(opseg_open/10)+1 )
--------------------------------------------------------------------------------


# MNOZESTVO Close: -------------------------------------------------------------
# prvin ke se pravi analiza na kolonata Close(poslednata transakcija na akcijata) t.e krajna cena
# MIN vrednost na kolonata Close
min_close <- min(amzn$Close)

# MAX vrednost na kolonata Close
max_close <- max(amzn$Close)

# opseg na Close kolonata
opseg_close <- max_close - min_close

# br na klasi
klasi_close <- sqrt( nrow(amzn) )

# golemina na klasa
gol_klasi_close <- opseg_close / klasi_close

# kreiranje na intervalite(sekvenciite)
seq_close <- seq( round(min_close), round(max_close), by=round(gol_klasi_close) )

# definiranje na intervalite t.e granicite i nivnite frekvencii na pojavuvanja
frekvencii_close <- cut( amzn$Close, seq_close )
amzn$intervali_close <- cut( amzn$Open, seq_open )

# prikaz na tabela so raspredelba na cestoti(frekvencii)
tab_na_cestoti_close <- transform( table(frekvencii_close) )

# Sredna tocka od itnervalite(na Close kolonata)
amzn$sredna_t_close <- midcut( amzn$Close, round(min_close), round(max_close), round(gol_klasi_close) )
# --------------------------------------------------------------------------------


# RELATIVNI FREKVENCII(za Open & Close)-----------------------------------------
tab_na_cestoti_open$relative_Freq <- tab_na_cestoti_open$Freq / nrow(amzn)
tab_na_cestoti_close$relative_Freq <- tab_na_cestoti_close$Freq / nrow(amzn)

# KUMULATIVNI FREKVENCII(za Open & Close)---------------------------------------
tab_na_cestoti_open$cumulative_freq <- cumsum(tab_na_cestoti_open$Freq)
tab_na_cestoti_close$cumulative_freq <- cumsum(tab_na_cestoti_close$Freq)

# HISTOGRAM---------------------------------------------------------------------
colors = c("red", "yellow", "green", "violet") 
hist(amzn$sredna_t_open, col=colors, xlab="Средни точки", ylab="Број на појавувања", main="Хистограм за колона Open" )

#-------------------------------------------------------------------------------

# POLIGON ----------------------------------------------------------------------
plot( tab_na_cestoti_open$frekvencii_open, tab_na_cestoti_open$relative_Freq, type="b", col="red", main="График за Релативна Фреквенција", xlab="Интервали", ylab="Фреквенции" )
lines( tab_na_cestoti_open$frekvencii_open, tab_na_cestoti_open$relative_Freq, type="l", col="red", main="График за Релативна Фреквенција", xlab="Интервали", ylab="Фреквенции" )

plot( tab_na_cestoti_close$frekvencii_close, tab_na_cestoti_close$relative_Freq, type="b", col="red", main="График за Релативна Фреквенција", xlab="Интервали", ylab="Фреквенции" )
lines( tab_na_cestoti_close$frekvencii_close, tab_na_cestoti_close$relative_Freq, type="l", col="red", main="График за Релативна Фреквенција", xlab="Интервали", ylab="Фреквенции" )
# ------------------------------------------------------------------------------


# PRV DEL, zadaca 2 -------------------------------------------------------------------------
# vo tab na cestoti za relativnite frekvencii kje se pravi stelbo list dijagram
stem_tab_rel_open <- stem( tab_na_cestoti_open$relative_Freq )
stem_tab_rel_close <- stem( tab_na_cestoti_close$relative_Freq )
# vo tab na cestoti za kumulativni frekvencii kje se pravi stelbo list dijagram
stem_tab_cum_open <- stem( tab_na_cestoti_open$cumulative_freq )
stem_tab_cum_close <- stem( tab_na_cestoti_close$cumulative_freq )
# -------------------------------------------------------------------------------------------

# PRV DEL, zadaca 3 -------------------------------------------------------------------------
zav_prvin_posledni_ceni <- plot( amzn$Open, amzn$Close, main="Коорелација меѓу почетни и крајни цени", xlab="Почетни цени", ylab="Крајни цени", col="red")
# -------------------------------------------------------------------------------------------

# PRV DEL, zadaca 4 -------------------------------------------------------------------------
# moda, medijana i prosek na regularna, kumulativna i relativna cestota
mean( tab_na_cestoti_open$Freq )
median( tab_na_cestoti_open$Freq )
sort( table(tab_na_cestoti_open$Freq), decreasing=TRUE )
# -------------------------------------------------------------------------------------------

# PRV DEL, zadaca 5 -------------------------------------------------------------------------
q1_open <- quantile( amzn$Open, 0.25 )                      # Q1 od pocetna cena na akcii
q2_open <- median( amzn$Open )                              # Q2 od pocetna cena na akcii
q3_open <- quantile( amzn$Open, 0.75 )                      # Q3 od pocetna cena na akcii

q1_close <- quantile( amzn$Close, 0.25 )                    # Q1 od krajna cena na akcii
q2_close <- median( amzn$Close )                            # Q2 od krajna cena na akcii
q3_close <- quantile( amzn$Close, 0.75 )                    # Q3 od krajna cena na akcii

raspon_open <- max(amzn$Open) - min(amzn$Open)              # raspon za prvicnite ceni na akciite
raspon_close <- max(amzn$Close) - min(amzn$Close)           # raspon za krajnite ceni na akciite

interkvart_open <- q3_open - q1_open                        # Q3 - Q1
print( paste( "IQR_Open = ", intervali_open ) )
interkvart_close <- q3_close - q1_close                     # Q3 - Q1
print( paste( "IQR_Close = ", intervali_close ) )

# -------------------------------------------------------------------------------------------

# PRV DEL, zadaca 6 -------------------------------------------------------------------------
# Standardna devijacija i disperzija
sd_open <- sd( amzn$Open, na.rm=TRUE )
print( "Raspredelba na pocetnite ceni na akciite: " )
print( paste( "SD = ", sd_open ) )
print( paste( "SD^2 = ", sd_open^2 ) )

sd_close <- sd( amzn$Close, na.rm=TRUE )
print( "Raspredelba na krajnite ceni na akciite: " )
print( paste( "SD = ", sd_close ) )
print( paste( "SD^2 = ", sd_close^2 ) )

# vizuelna reprezentacija za raspredelba na pocetnite ceni na akciite
png( file="dnorm-pocetni-ceni-akcii.png" )
plot(    amzn$Open, dnorm( amzn$Open, mean(amzn$Open), sd(amzn$Open) )    )
dev.off()

# vizuelna reprezentacija za raspredelba na krajnite ceni na akciite
png( file="dnorm-krajni-ceni-akcii.png" )
plot(    amzn$Close, dnorm( amzn$Close, mean(amzn$Close), sd(amzn$Close) )    )
dev.off()

# -------------------------------------------------------------------------------------------

# PRV DEL, zadaca 7 -------------------------------------------------------------------------
cor( amzn$Open, amzn$Close )
# -------------------------------------------------------------------------------------------

# VTOR DEL, zadaca 1 ------------------------------------------------------------------------
# interval na doverba-----------------------------------
interval_doverba <- function( volume ) {
  CI <- 0.95       # INTERVAL NA SIGURNOST(%) od 95%
  alfa <- 1-CI     # nivoto na prifatliva greshka(%)
  MoE <- qnorm(1-alfa/2) * sd(volume)/sqrt(length(volume))
  min.interval <- mean(volume) - MoE
  max.interval <- mean(volume) + MoE
  
  # pecati interval
  print( c(min.interval, max.interval) )
  print(mean(amzn$Volume))
}
interval_doverba( sample( amzn$Volume, size=1000 ) )
# -------------------------------------------------------------------------------------------

# VTOR DEL, zadaca 2 ------------------------------------------------------------------------
# hipotezi-----------------------------------
# H0: EX = 7.5M
# Ha: EX != 7.5M
EX <- 7500000
gr <- qnorm(1-alfa/2)                                                           # se naogja granicata
print( c(-1*gr, gr) )                                                           # se pecati intervalot(leva i desna granica)
z <- ( (mean(amzn$Volume-EX)) / sd(amzn$Volume) ) * sqrt(length(amzn$Volume))   # determinantna vrednost vo intervalot

# proverka koja hipoteza e tocna
if( z>(-1*gr) & z<gr ) {
  print("H0: T")
} else {
  print("Ha: T")
}
# -------------------------------------------------------------------------------------------

# VTOR DEL, zadaca 3 ------------------------------------------------------------------------
# test za raspredelba------------------------
png( file="raspredelba-volume.png" )
hist(amzn$Volume, breaks=49, xlim=c(487000, 105000000), ylim=c(0, 1100), xlab="интервали", ylab="Фреквенции", main=" ")
dev.off()
# -------------------------------------------------------------------------------------------

# VTOR DEL, zadaca 4 ------------------------------------------------------------------------
# test na hipotezi za nezavisnost------------

# ---/---

# -------------------------------------------------------------------------------------------

# VTOR DEL, zadaca 5 ------------------------------------------------------------------------
# linearna regresija-------------------------
plot( amzn$Open, amzn$High )    # generiranje grafik za koorelacija
r <- lm(amzn$High~amzn$Open)    # model za linearna reg.
abline(r, col='red')            # linija niz grafikot za koorelacija
names(r)
# manuelno da se vnese X i so modelot da se predvidi vrednosta na Y
predict( r, list(pocetna_cena=15), level=0.95, interval="prediction" )[1]

# -------------------------------------------------------------------------------------------


