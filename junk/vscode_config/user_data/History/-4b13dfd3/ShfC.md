## Środowisko lokalne
```
> apt-get install m4 opam spim
> opam init
> opam update
> opam switch 4.13.1
> eval `opam config env`
> tools/opam-pin.sh # przypina wersje bibliotek
> opam install ocamlgraph dune cmdliner menhir ocamlformat
```

## Dostępne narzędzia
- `make install` -  zrobi symlink `./zi`
- `make test`    -  odpala testy z pluginem mod_student.cma
- `make test_without_plugin` - odpala testy __bez__ pluginu mod_student.cma
- `tools/format.sh` - automatyczne formatowanie wszystkich plików źródłowych
- `tools/check-format.sh` - sprawdza czy pliki źródłowe są sformatowane

## Praca z Github Classroom
Pracują Państwo na gałęzi `master`, natomiast gałąź `feedback` pozwala na porównanie Państwa rozwiązania ze stanem początkowym. Dodatkowo otwarty jest pull-request na którym mogą Państwo zadawać pytania odnośnie Państwa kodu.

Po wypchnięciu commita uruchamia się automatyczna sprawdzaczka która wykonuje następujące kroki:

- Sprawdza formatowanie kodu źródłowego
- Kompiluje kompilator
- Uruchamia wszystkie testy

Każdy z tych kroków musi się udać aby sprawdzaczka uznała, że Państwa rozwiązanie jest poprawne.

## Wtyczka `zisdk/mod_uwr.cma`
Wtyczka ta zawiera skompilowane już moduły wykorzystywane przez kompilator.
Korzysta ona z biblioteki `ocamlgraph` w wersji przedstawionej w tabeli w następnej sekcji.
Aby Państwa kod działał lokalnie, muszą Państwo używać biblioteki w tej samej wersji, co jest zapewnione poprzez przypięcie wersji przez skrypt `tools/opam-pin.sh`.

## Wersje bibliotek w środowisku testowym (nieaktualne, bo nie mamy środowiska testowego)
|               |     |
| ---           | --- 
| `ocaml`       | 4.13.1 
| `cmdliner`    | 1.1.1
| `dune`        | 3.4.1
| `menhir`      | 20220210
| `ocamlgraph`  | 2.0.0
| `ocamlformat` | 0.24.1

## Formatowanie plików
Korzystamy z narzędzia `ocamlformat`.

## Modyfikowanie plików
Prosimy o modyfikowanie jedynie:

- `source/mod_student/lexer.mll`
- `source/mod_student/parser.mly`

W przypadku zmian w pozostałych plikach Państwa rozwiązanie nie będzie akceptowane.

## Testy
Testy na których sprawdzane są Państwa rozwiązania znajdują się w katalogu `tests`. Jeśli mają Państwo pomysły na dodatkowe przypadki testowe prosimy o otwieranie pull-requestów do repozytorium z __szablonem__ a nie do Państwa prywatnego repozytorium.

## Błędy
W przypadku błędów zarówno w kodzie biblioteki, zachowaniu naszej wtyczki, jak i testach, prosimy o otwieranie issue w repozytorium z __szablonem__.
