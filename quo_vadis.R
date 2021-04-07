# Tresc zadania ----
# Mamy dan¹ zero-indeksowana tablicê int A[0..n-1].
# ¯abka, która stoi na polu o indeksie i, wykonuje skok na pole o indeksie k = i + A[i].
# Pocz¹tkowo ¿abka stoi na polu o indeksie 0.
# Wyjœcie: liczba skoków, po których ¿abka wyskoczy poza zakres tablicy A lub -1 je¿eli nie wyskoczy nigdy.

# Definicja funkcji ----
quo_vadis <- function(tablica) {
  
  tablica$wynik <- as.numeric(row.names(tablica)) + tablica$skok
  rozmiar <- nrow(tablica)
  
  if (min(tablica$wynik) > 0 & max(tablica$wynik) <= rozmiar) {
    'Zabka nigdy nie wyskoczy. Nie ma takiej pozycji, z ktorej by wyskoczyla.'
  }
  
  else {
    for (i in 1:rozmiar) {
      if (i == 1) {
        temp <- c()
        pozycja = 1
      }
      
      if (pozycja == pozycja + tablica$skok[pozycja]) {
        print(paste0('Zabka nigdy nie wyskoczy. Stanie w miejscu na ', pozycja, ' pozycji.'))
        break
      }
      
      pozycja = pozycja + tablica$skok[pozycja]
      
      if (pozycja <= 0 | pozycja > rozmiar) {
        print(paste0('Zabka wyskoczy po ', i, ' skokach.'))
        break
      }
      
      if (pozycja %in% temp) {
        print('Zabka nigdy nie wyskoczy. Zapetli sie.')
        break
      }
      
      temp[i] <- pozycja
    }
  }
}

# Test funkcji ----
tablica <- data.frame(skok = sample(-2:5, 20, replace=TRUE))
quo_vadis(tablica)