# --- UKAZKA PRACE S BINOMICKYM ROZDELENIM ---

# nastaveni parametru rozdeleni
n = 10   # pocet pokusu
p = 0.3  # pravdepodobnost uspechu

# Vykresleni pravdepodobnostni funkce:
# Vytvorime vektor hodnot 0, ..., n,
x = seq(0, n) 
# v nich vypocteme hodnoty pravdepodobnostni funkce 
# binomickeho rozdelni s parametry n, p
prf = dbinom(x, n, p)
# a vykreslime graf. 
plot(x, prf, t='p', ylab = 'p(x)', 
     main=sprintf('Pravd. funkce Bi(%d, %g)', n, p))
# Parametr t='p' zajistil vykresleni jednotlivych bodu.

# Vykresleni distribucni funkce:
# Distribucni funkci vykreslime jako funkci realne promenne,
# proto vytvorime vektor hodnot na delsim intervalu obsahujicim
# interval <0, n> a vzdalenost mezi jednotlivymi body zvolime mnohem 
# mensi nez 1.
xs = seq(-n/2, n+n/2, 0.01) 
# Vypocteme vektor hodnot distribucni funkce
distrf = pbinom(xs, n, p)
# a vykreslime graf. 
plot(xs, distrf, t='l', ylab = 'F(x)', 
     main=sprintf('Distr. funkce Bi(%d, %g)', n, p))
# Parametr t='l' zajistil vykresleni spojiteho grafu.
# Funkce je ve skutecnosti nespojita a po castech konstantni, 
# coz R z vypoctenych hodnot nepozna. Proto graf vypada jako "schody".

# Vypocet kvantilu:
# q-kvantil je nejnizsi hodnota k, pro kterou plati P(X<=k) >= q,
# tj. nejnizzsi hodnota, pro kterou distribucni funkce nabyva
# hodnoty alespon q.
q = 0.7 # volba pravdepodobnosti 
k = qbinom(q, n, p) # vypoct kvantilu
# Muzeme overit spravnost:
# Hodnota distribucni funkce v bode k skutecne dosahuje alespon q.
pbinom(k, n, p) 
# Zmensime-li k o libovlne malou hodnotu, distribucni funkce nedosahuje q.
pbinom(k-0.000001, n, p) 

#############################################################################

# --- UKAZKA PRACE S NORMALNIM ROZDELENIM ---
# nastaveni parametru rozdeleni
m = 10 # stredni hodnota
s = 3  # smerodatna odchylka
# Vytvorime vektor hodnot, tentokrat "huste" v dost dlouhem intervalu,
x = seq(m-5*s, m+5*s, 0.01) 
# v nich vypocteme hodnoty hustoty pravdepodobnosti 
# normalniho rozdelni se str. hodnotou m a smerodatnou odchylkou s
hust = dnorm(x, m, s)
# a vykreslime graf. 
plot(x, hust, t='l', ylab = 'f(x)', 
     main=sprintf('Hustota pr. N(%g, %g)', m, s**2))
# Normalni rozdeleni se zpravidla v textu uvadi s parametry
# str. hodnota a rozptyl (proto s**2 v titulku grafu),
# R ale ocekava parametry str. hodnota a smerodatna odchylka.

# Vykresleni distribucni funkce:
# Vypocteme vektor hodnot distribucni funkce
distrf = pnorm(x, m, s)
# a vykreslime graf .
plot(x, distrf, t='l', ylab = 'F(x)', 
     main=sprintf('Distr. funkce N(%g, %g)', m, s**2))

# Vypocet kvantilu:
q = 0.7 # volba pravdepodobnosti 
k = qnorm(q, m, s) # vypoct kvantilu
# Muzeme overit spravnost:
# Hodnota distribucni funkce v k tentokrat presne nabyva hodnoty q,
# protoze distribucni funkce je v tomto pripade spojita a prosta funkce.
pnorm(k, m, s) 

# Generovani nahodnych vzorku:
N = 10000 # pocet vzorku
r = rnorm(N, m, s) # vygenerovani N nahodnych vzorku z normalniho rozdeleni
# Overime, ze vse funguje podle ocekavani.
# Vykreslime histogram. Dat je dost, muzeme volit vetsi pocet intervalu. 
hist(r, breaks=20, freq=FALSE, xlab = 'x', ylab='rel. cetnosti', 
     main=sprintf('Histogram dat z N(%s, %s)', m, s)) 
# Parametr freq=FALSE zajistil, ze plocha histogramu je rovna 1
# a muzeme tak histogram porovnat se skutecnou hustotou.
lines(x, hust, col='red')
# Pouziti prikazu lines misto plot zajistilo, ze se graf zakreslil
# do stavajiciho grafu s histogramem.



