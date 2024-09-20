
tonKgGr n=(kilos,gramos)
    where
        kilos = n * 1000
        gramos = n * 1000000


kmaM metros = metros * 1000

entero n1 n2| n1>=100000 && n1 <=999999 && n2>=100000 && n2<=999999 = n2*1000000+n1
    |otherwise = 0


distancia vueltas km = vueltas * km



rotar x n = (x `mod` 10^n)*10^(length(show x) -n) + (x `div` 10^n) 

terPos p s | s >= 0 && s <= 999999 = ((p `div` 1000) *10^8) + ((s + 10^(6 - length(show s))*0)*1000) + (p `mod` 1000)



partirNum num=(a, b)
    where
        a = num `div` 10^6 * 100 + (num `mod` 100)
        b = (num `div` 100) `mod` 10000

sumatoria 0 = 0
sumatoria num = (num `mod` 10) + sumatoria(num `div` 10)

clave n1 n2 n3 n4 = (b, a, d, c)
    where
        a = extraer n1
        b = extraer n2
        c = extraer n3
        d = extraer n4  

extraer 0 = 0
extraer x = (9 - (x `mod` 10)) + 10 * extraer (x `div` 10)

