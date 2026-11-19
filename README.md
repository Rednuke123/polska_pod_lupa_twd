# polska_pod_lupa_twd
## Marta Dziarhauka – wydatki i pogoda

### Wykres 1: „Na co wydajemy więcej przed świętami?”
kody/02_retail/retail.R

Dane: GUS, Bank Danych Lokalnych – indeksy sprzedaży detalicznej
(poprzedni miesiąc = 100) dla 16 województw, lata 2021–2025.
https://bdl.stat.gov.pl/bdl/dane/podgrup/tablica

Co zrobiłam: wybrałam cztery kategorie towarów (książki, prasa i prezenty;
żywność i napoje; meble, RTV, AGD; paliwa) i usunęłam dane utajnione przez GUS.
Dla każdego miesiąca policzyłam medianę województw, a potem medianę z pięciu lat
oraz wartości minimalną i maksymalną. Od indeksu odjęłam 100, żeby otrzymać
zmianę sprzedaży w procentach.

Dlaczego taki wykres: słupki dla każdego miesiąca pokazują, że grudzień
wyraźnie odstaje od reszty roku, a wyróżnienie go kolorem pozwala od razu
porównać kategorie. Kreski min–max pokazują, że wzrost w grudniu powtarza się
co roku, a nie jest przypadkiem jednego roku. Osobne panele dla kategorii
pozwalają zestawić towary, które w grudniu rosną, z paliwami, które spadają.

### Wykres 2: „Czy święta są jeszcze białe?”
kody/03_weather/download_weather.R, kody/03_weather/weather.R

Dane: IMGW-PIB, dane synoptyczne dobowe, lata 1996–2026.
https://danepubliczne.imgw.pl/data/dane_pomiarowo_obserwacyjne/dane_meteorologiczne/dobowe/synop/

Co zrobiłam: skrypt download_weather.R pobiera archiwa IMGW i wybiera
pomiary pokrywy śnieżnej z 25.12 i 1.01. Pomiar wykonywany jest rano, więc
25.12 pokazuje śnieg w noc wigilijną, a 1.01 – w noc sylwestrową. Pominęłam
stacje wysokogórskie (Kasprowy Wierch, Śnieżka, Hala Gąsienicowa), na których
śnieg leży prawie zawsze. Za „białą” uznałam stację z co najmniej 1 cm śniegu
i policzyłam odsetek takich stacji w każdym sezonie oraz średnią dla każdej
dekady.

Dlaczego taki wykres: wykres gantelkowy pokazuje dwie rzeczy naraz:
ile śniegu było w danym sezonie i czy w ciągu tygodnia między Wigilią
a Nowym Rokiem śnieg spadł, czy stopniał. Średnie dla dekad wyraźnie pokazują
trend: odsetek białych stacji spada mniej więcej o połowę co 10 lat
(61% → 28% → 14%).

### Paleta
Wspólne kolory wykresów są w kody/paleta.R: bordo oznacza Wigilię
i grudzień, złoto – Nowy Rok, szałwia – pozostałe dni i miesiące.