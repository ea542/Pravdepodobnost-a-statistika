# exponencialni rozdeleni, centralni limitni veta

lam = 1/5   # parametr intenzity exp. rozdeleni
n = 510
    # pocet scitanych velicin
t = 100000   # pocet vzorku

re = rowSums(matrix(rexp(n*t, lam), ncol=n)) # t vzorku souctu n exponencialnich  velicin

h = hist(re, breaks=100, freq=F, 
         main = paste("normalizovaný histogram součtu ",  n, " exp. veličin"),
         ylab = "hustota")  # histogram souctu  
x = seq(min(re), max(re), length=200)
# hustota normalniho rozdeleni aproximujiciho soucty pomoci CLV
yn = dnorm(x, n/lam, (n/(lam^2))^0.5)  
lines(x, yn, col='red')
# presna hustota souctu (gamma rozdeleni)
yg = dgamma(x, shape=n, rate=lam)
lines(x, yg, col='blue')
legend("topright", c("aprox. CLV", "skutečná"), lty=c(1,1), col=c("red", "blue"), inset=.05, title="Hustoty pravdepodobnosti")
