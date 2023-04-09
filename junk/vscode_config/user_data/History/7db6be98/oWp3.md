# Resolution for (F)GF

Kod w scali, kompiluje się `sbt` (opisane poniżej).
`main` oblicza zbiór klauzul postaci CNF dla formuły GF, dla przykładu z oryginalnej pracy o rezolucji dla GF. Wypisuje postać po każdym kroku obliczeń. Załączam output programu w pliku main.out.

## notki
 - każdy etap transformacji definiuje swój typ postaci formuły wynikowej.
 To duża redundancja liczona linijkami kodu, ale uważam mniej skomplikowania i ukrytych niezmienników.
 - todo: faktyczny proces rezolucji (to będzie proste)
 - todo: testy

## sbt project compiled with Scala 3

### Usage

This is a normal sbt project. You can compile code with `sbt compile`, run it with `sbt run`, and `sbt console` will start a Scala 3 REPL.

For more information on the sbt-dotty plugin, see the
[scala3-example-project](https://github.com/scala/scala3-example-project/blob/main/README.md).

Run TESTS with `sbt test`.