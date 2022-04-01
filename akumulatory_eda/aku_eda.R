# Ukazka zakladnich moznosti exploracni analyzy pomoci nastroju standardni knihovny jazyka R. 
# Datovy soubor: aku.csv

# nacteni dat ze souboru csv do dataframe ("datove tabulky")
aku = read.csv("aku.csv", sep=";", quote="\"")
View(aku) # zobrazeni dat 
names(aku) # vypis nazvu sloupcu v dataframe
# zmeni nazvy sloupcu v dataframe
names(aku) = c("cislo", "vyrobce", "kap5", "kap100") 

# K sloupcum dataframe lze pristupovat pomoci symbolu $.
aku$kap5 # vypise sloupec kap5 z dataframe aku
# Pro usnadneni prace s daty lze datove sloupce zpristupnit jako promenne.
attach(aku) # zpristupni sloupce dataframe jako promenne (kap5 misto aku$kap5 apod.)
kap5 # vypise sloupec kap5 z dataframe aku

# Exploracni analyza kvalitativni promenne (vyrobce)
summary(vyrobce) # zobrazi souhrnne informace o vyrobcich
tab.vyrobce = table(vyrobce) # tabulka abs. cetnosti vyrobcu
tab.vyrobce # zobrazi tabulku cetnosti
pie(tab.vyrobce, main='Vyrobce') # kolacovy graf, argumentem funkce je tabulka cetnosti
# sloupcovy graf, argumentem funkce je opet tabulka cetnosti
barplot(tab.vyrobce, xlab="vyrobce", ylab="pocet", col = c('blue', 'red', 'green', 'yellow'))

# Exploracni analyza kvantitativni promenne (kapacita po 5 cyklech)
mean(kap5) # vyberovy prumer
sd(kap5) # smerodatna odchylka
summary(kap5) # zobrazi souhrnne informace o kapacitach po 5 cyklech
hist(kap5, breaks = 10, xlab='kapacita', ylab='pocet', main='Kapacita po 5 cyklech') # histogram 
boxplot(kap5, main='Kapacita po 5 cyklech', ylab='mAh') # krabicovy graf

# Exploracni analyza kvantitativni promenne (kapacita po 100 cyklech akumulatoru vyrobce A)
summary(kap100[vyrobce=='A']) # zobrazi souhrnne informace o kapacitach po 100 cyklech akumulatoru vyrobce A
hist(kap100[vyrobce=='A'], breaks = 10, xlab='kapacita', ylab='pocet', main='Kapacita po 100 cyklech') # histogram 
boxplot(kap100, main='Kapacita po 100 cyklech', ylab='mAh') # krabicovy graf

# Krabicove grafy naznacuji pritomnost odlehlych pozorovani.
# detekce odlehlych pozorovani pomoci vnitrnich hradeb pro kap5
IQR5=quantile(kap5, 0.75)-quantile(kap5, 0.25) # interkvartilove rozpeti
dm5=quantile(kap5, 0.25)-1.5*IQR5 # dolni mez vnitrnich hradeb
hm5=quantile(kap5, 0.75)+1.5*IQR5 # hormi mez vnitrnich hradeb
aku[kap5<dm5 | kap5>hm5,] # zobrazi akumulatory s kap5 mimo vnitrni hradby

# Akumulatory s velmi nizkou kapacitou (pod 1500 mAh) se jevi jako poskozene.
# Poskozene akumulatory budou vyrazeny ze souboru.
aku_ = aku  # ulozime puvodni dataframe 
aku = aku[kap5>=1500,]  # do aku ulozime ocistena data  
attach(aku) # R upozorni, ze promenne cislo, kap100, kap5 a vyrobce jsou prepsany
# To bylo nasim cilem, takze varovani ignorujeme.

# Exploracni anlyzu provedeme znovu pro data bez odlehlych pozorovani.
# ...
# ...
# ...


# Exploracni analyza zavislosti kvantitativni promenne na kvalitativni
# Zavisi kapacita po 5 cyklech na vyrobci?
tapply(kap5, vyrobce, summary)   # funkci summary aplikuje na kap5 zvlast pro akumulatory jednotlivych vyrobcu 
# krabicove grafy kapacity po 5 cyklech zvlast pro jednotlive vyrobce
boxplot(kap5~vyrobce, main="Kapacita po 5 cyklech", ylab='mAh') 
# Z toho, jak jsou jednotlive grafy proti sobe navzajem posunuty,
# muzeme usuzovat, ze kapacity akamulatoru jednotlivych vyrobcu se lisi.

# Exploracni analyza zavislosti dvou kvantitativnich promennych 
# Zavisi kapacita po 100 cyklech na kapacite po 5 cyklech (spolecne pro vsechny vyrobce)?
plot(kap5, kap100)
# V datech je patrny trend - cim vyssi kapacita po 5 cyklech, tim vyssi kapacita po 100 cyklech.
# Z toho lze usuzovat, ze kapacita po 100 cyklech na kapacite po 5 cyklech zavisi.
 
# Exploracni analyza zavislosti dvou kvalitativnich promennych
# Zavisi pokles kapacity mezi 5. a 100. cyklem o vice nez 10 % puvodni kapacity na vyrobci?
pokles.abs = kap5-kap100 # absolutni pokles mezi 5. a 100. cyklem
pokles.rel = pokles.abs/kap5 # relativni pokles mezi 5. a 100. cyklem
# pokles mezi 5. a 100. cyklem o vice nez 10% puvodni kapacity
pokles.nad10p = (pokles.rel>0.1) # pokles nad 10 % (vektor hodnot True/False)
# tabulka s pocty akumulatoru, u kterych doslo/nedoslo k poklesu nad 10 % v zavislsti na vyrobci    
tab.pokles = table(vyrobce, pokles.nad10p) 
# mozaikovy graf 
mosaicplot(tab.pokles, main='Pokles kapcity o vice nez 10%', ylab='pokles o vice nez 10 % ')  
# Mozaikovy graf vykazuje vyraznou nepravidelnost. Timto naznacuje, ze pocty akumulatoru, 
# u nich dochazi k poklesu o vice nez 10 procent, se u jednotlivych vyrobcu lisi.  

# POZOR: Na zaklade exploracni analyzy sice muzeme vyslovit domenky o moznych zavislostech,
# ale k jejich pripadnemu prokazani je potreba provest statisticke testy.
