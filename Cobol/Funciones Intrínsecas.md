# Funciones Intrínsecas en COBOL

Las funciones intrínsecas son funciones integradas en COBOL y que nos permiten realizar operaciones matemáticas, de manipulación de cadenas, de fechas, entre otras sin la necesidad de codificarlas uno mismo. A continuación, se detallará una lista de las funciones intrínsecas más comunes en COBOL y su clasificación.

1. kk

# Cómo se utilizan las Funciones Intrínsecas

Para utilizar una función intrínseca en COBOL, se debe seguir la siguiente estructura:

```cobol
FUNCTION function-name (argument-1, argument-2, ..., argument-n)
```

Donde `function-name` es el nombre de la función intrínseca que se desea utilizar y `argument-1, argument-2, ..., argument-n` son los argumentos que recibe la función.

No es necesario utilizar las comas como separadores de argumentos, también se pueden utilizar espacios en blanco. En este Cheat Sheet, todas las funciones intrínsecas se utilizarán con comas como separadores de argumentos para mayor legibilida.

```cobol
DISPLAY FUNCTION function-name(argument-1 argument-2 ... argument-n).
```

Podemos utilizar las funciones intrínsecas en cualquier parte de nuestro código, la cual será reemplazada por el valor que retorne la función.

```cobol
ADD function(arg) TO variable
```

# Tipos de Funciones Intrínsecas

Las funciones intrínsecas de COBOL se dividen en 6 categorías según su finalidad:

- **Matemáticas**
- **Estadísticas**
- **Fecha y Hora**
- **Financieras**
- **Manejo de Caracteres**
- **Generales**

# Todas las Funciones Intrínsecas

A continuación, se detallará una tabla de todas las funciones intrínsecas de COBOL y su clasificación.

| Matemáticas | Fecha y Hora                | Financieras   | Manejo de Caracteres | Estadísticas       |
| ----------- | --------------------------- | ------------- | -------------------- | ------------------ |
| ABS         | COMBINED-DATETIME           | ANNUITY       | CHAR                 | MAX                |
| ACOS        | CURRENT-DATE                | PRESENT-VALUE | CONCATENATE          | MEAN               |
| ASIN        | DATE-OF-INTEGER             |               | DISPLAY-OF           | MEDIAN             |
| ATAN        | DATE-TO-YYYYMMDD            |               | LENGTH               | MIDRANGE           |
| COS         | DAY-OF-INTEGER              |               | LENGTH-AN            | MIN                |
| FACTORIAL   | DAY-TO-YYYYDDD              |               | LOWER-CASE           | RANGE              |
| LOG         | FORMATTED-CURRENT-DATE      |               | NATIONAL-OF          | STANDARD-DEVIATION |
| LOG10       | FORMATTED-DATE              |               | ORD                  | SUM                |
| MOD         | FORMATTED-DATETIME          |               | ORD-MAX              | VARIANCE           |
| PI          | FORMATTED-TIME              |               | ORD-MIN              |                    |
| RANDOM      | INTEGER-OF-DATE             |               | REVERSE              |                    |
| REM         | INTEGER-OF-DAY              |               | STORED-CHAR-LENGTH   |                    |
| SIGN        | INTEGER-OF-FORMATTED-DATE   |               | SUBSTITUTE           |                    |
| SIN         | LOCALE-DATE                 |               | SUBSTITUTE-CASE      |                    |
| SQRT        | LOCALE-TIME                 |               | TRIM                 |                    |
| TAN         | LOCALE-TIME-FROM-SECONDS    |               | UPPER-CASE           |                    |
|             | SECONDS-FROM-FORMATTED-TIME |               |                      |                    |
|             | SECONDS-PAST-MIDNIGHT       |               |                      |                    |
|             | YEAR-TO-YYYY                |               |                      |                    |


Existen otras funciones intrínsecas que no se encuentran en la tabla, las cuales se categorizarían como funciones **Generales**, las que se pueden visualizar a continuación:

| Generales          |              |                    |                |
| ------------------ | --------------------- | --------------------------- | ----------------------- |
| BOOLEAN-OF-INTEGER | MODULE-CALLER-ID      | NUMVAL                      | TEST-NUMVAL-C           |
| INTEGER            | MODULE-DATE           | NUMVAL-C                    | TEST-NUMVAL-F           |
| INTEGER-OF-BOOLEAN | MODULE-FORMATTED-DATE | NUMVAL-F                    | TEST-DAY-YYYYDDD        |
| MODULE-ID          | MODULE-PATH           | NUMERIC-DECIMAL-POINT       | TEST-DATE-YYYYMMDD      |
| MODULE-SOURCE      | MODULE-TIME           | NUMERIC-THOUSANDS-SEPARATOR | TEST-FORMATTED-DATETIME |
| INTEGER-PART       | LOCALE-COMPARE        | STANDARD-COMPARE            | WHEN-COMPILED           |

FALTA TEST-NUMVAL


## Funciones Matemáticas

### ABS

La función `ABS` retorna el valor absoluto de un número.

Argumentos:
- Número.

```cobol
DISPLAY FUNCTION ABS(-5).
```

Resultado:

```cobol
5
```

Tabla de ejemplo

| ABS | Salida |
| --- | ------ |
| 5   | 5      |
| 0   | 0      |
| -5  | 5      |


### ACOS

Argumentos:
- Número.

La función `ACOS` retorna el arco coseno de un número.

```cobol
DISPLAY FUNCTION ACOS(0.5).
```

Resultado:

```
1.047197551
```

### ASIN

La función `ASIN` retorna el arco seno de un número.

Argumentos:
- Número.

```cobol
DISPLAY FUNCTION ASIN(0.5).
```

Resultado:

```
0.523598776
```

### ATAN

La función `ATAN` retorna el arco tangente de un número.

Argumentos:
- Número.

```cobol
DISPLAY FUNCTION ATAN(0.5).
```

Resultado:

```
0.463647609
```

### COS

La función `COS` retorna el coseno de un número.

Argumentos:
- Número.

```cobol
DISPLAY FUNCTION COS(0.5).
```

Resultado:

```
0.877582561
```

### FACTORIAL

La función `FACTORIAL` retorna el factorial de un número.

Argumentos:
- Número.

```cobol
DISPLAY FUNCTION FACTORIAL(5).
```

Resultado:

```
120
```

### LOG

La función `LOG` retorna el logaritmo natural de un número.

Argumentos:
- Número.

```cobol
DISPLAY FUNCTION LOG(5).
```

Resultado:

```
1.609437912
```


### LOG10

La función `LOG10` retorna el logaritmo base 10 de un número.

Argumentos:
- Número.

```cobol
DISPLAY FUNCTION LOG10(5).
```

Resultado:

```
0.69897
```

### MOD

La función `MOD` retorna el resto o residuo de una división.

Argumentos:
- Número dividendo.
- Número divisor.

```cobol
DISPLAY FUNCTION MOD(10, 3).
```

Resultado:

```cobol
1
```

### PI

La función `PI` retorna el valor de PI.

```cobol
DISPLAY FUNCTION PI.
```

Resultado:

```cobol
3.141592653...
```

### RANDOM

La función `RANDOM` retorna un número aleatorio.

Argumentos:
- _Opcional_: Número entero que representa la semilla del generador de números aleatorios.

```cobol
DISPLAY FUNCTION RANDOM.
```

Resultado:

```cobol
0.001251258888515885
```

### REM

La función `REM` es un sinónimo de `MOD` y retorna el resto o residuo de una división.

Argumentos:
- Número dividendo.
- Número divisor.

```cobol
DISPLAY FUNCTION REM(10, 3).
```

Resultado:

```cobol
1
```

### SIGN

La función `SIGN` retorna el signo de un número. Si el número es positivo, retorna 1. Si el número es negativo, retorna -1. Si el número es 0, retorna 0.

Argumentos:
- Número.

```cobol
DISPLAY FUNCTION SIGN(-5).
```

Resultado:

```cobol
-1
```

### SIN

La función `SIN` retorna el seno de un número.

Argumentos:
- Número.

```cobol
DISPLAY FUNCTION SIN(0.5).
```

Resultado:

```
0.479425538
```

### SQRT

La función `SQRT` retorna la raíz cuadrada de un número.

Argumentos:
- Número.

```cobol
DISPLAY FUNCTION SQRT(25).
```

Resultado:

```
5
```

### TAN

La función `TAN` retorna la tangente de un número.

Argumentos:
- Número.

```cobol
DISPLAY FUNCTION TAN(0.5).
```

Resultado:

```
0.546302489
```

### Resumen de Funciones Matemáticas

```cobol
DISPLAY FUNCTION ABS(-5).      *> 5
DISPLAY FUNCTION ACOS(0.5).    *> 1.047197551...
DISPLAY FUNCTION ASIN(0.5).    *> .523598776...
DISPLAY FUNCTION ATAN(0.5).    *> .463647609...
DISPLAY FUNCTION COS(0.5).     *> .877582561...
DISPLAY FUNCTION FACTORIAL(5). *> 120
DISPLAY FUNCTION LOG(5).       *> 1.609437912...
DISPLAY FUNCTION LOG10(5).     *> .69897...
DISPLAY FUNCTION MOD(10, 3).   *> 1
DISPLAY FUNCTION PI.           *> 3.141592653...
DISPLAY FUNCTION REM(10, 3).   *> 1
DISPLAY FUNCTION SIN(0.5).     *> .479425538...
DISPLAY FUNCTION SQRT(25).     *> 5
DISPLAY FUNCTION TAN(0.5).     *> .546302489...
DISPLAY FUNCTION MOD(10, 3).   *> 1
DISPLAY FUNCTION REM(10, 3).   *> 1
```

## Funciones de Fecha y Hora

### COMBINED-DATETIME

La función `COMBINED-DATETIME` retorna la fecha y hora combinadas.

Argumentos:
- **Número entero que representa la fecha.** Un número entero positivo que representa una cantidad de días posteriores al 31 de diciembre de 1600 en el calendario gregoriano. El rango válido es de 1 a 3.067.671, que corresponde a fechas que van desde el 1 de enero de 1601 hasta el 31 de diciembre de 9999. Se puede obtener con la función [`INTEGER-OF-DATE`](#integer-of-date).
- **Número entero que representa la hora.** Un número positivo que representa una cantidad de segundos posteriores a la medianoche. El rango válido es de 0 a 86.399, que corresponde a horas que van desde las 00:00:00 hasta las 23:59:59. Se puede obtener con la función [`SECONDS-FROM-FORMATTED-TIME`](#seconds-from-formatted-time).

```cobol
DISPLAY FUNCTION COMBINED-DATETIME(143951, 18867.812479168304).
```

Resultado:

```
143951.18867812479168304
```

### CURRENT-DATE

La función `CURRENT-DATE` retorna la fecha actual.

```cobol
DISPLAY FUNCTION CURRENT-DATE.
```

Resultado:

```
2024021213325776+0300
```

Esto puede ser usado para obtener la fecha y hora actual en un formato más legible.

```cobol
DISPLAY FUNCTION LOCALE-DATE(FUNCTION CURRENT-DATE).
DISPLAY FUNCTION LOCALE-TIME(FUNCTION CURRENT-DATE).
```

Resultado:

```
12/2/2024
20:24:02
```

### DATE-OF-INTEGER

La función `DATE-OF-INTEGER` retorna la fecha a partir de un número entero.

Argumentos:
- **Número entero que representa la fecha.** Un número entero positivo que representa una cantidad de días posteriores al 31 de diciembre de 1600 en el calendario gregoriano. El rango válido es de 1 a 3.067.671, que corresponde a fechas que van desde el 1 de enero de 1601 hasta el 31 de diciembre de 9999.

```cobol
DISPLAY FUNCTION DATE-OF-INTEGER(000154607).
```

Resultado:

```
20240419
```

Este resultado tiene el formato `YYYYMMDD`.

### DATE-TO-YYYYMMDD

La función `DATE-TO-YYYYMMDD` convierte fechas de un formato con año de dos dígitos a uno con año de cuatro dígitos.

Argumentos:
- Fecha en formato `YYMMDD`.
- _Opcional_: Define el final de un intervalo de 100 años, ayudando a determinar en qué siglo se coloca el año de dos dígitos. Por defecto, el valor es 50.

```cobol
DISPLAY FUNCTION DATE-TO-YYYYMMDD(240419,30).
DISPLAY FUNCTION DATE-TO-YYYYMMDD(560419,30).
```

Resultado siendo el año actual 2024:

```
020240419 *> Representa la fecha 2024-04-19
019560419 *> Representa la fecha 1956-04-19
```

### DAY-OF-INTEGER

La función `DAY-OF-INTEGER` retorna el día a partir de un número entero.

Argumentos:
- Número entero que representa la fecha. Se puede obtener con la función [`INTEGER-OF-DATE`](#integer-of-date).

```cobol
DISPLAY FUNCTION DAY-OF-INTEGER(000154607).
```

Resultado:

```
2024110
```

Este resultado tiene el formato `YYYYDDD`, donde `DDD` representa el día del año, en este caso es el día número 110 del año 2024.

### DAY-TO-YYYYDDD

La función `DAY-TO-YYYYDDD` convierte el formato `YYDDD` al formato con año de cuatro dígitos `YYYYDDD`.

Argumentos:
- Fecha en formato `YYDDD`.
- _Opcional_: Define el final de un intervalo de 100 años, ayudando a determinar en qué siglo se coloca el año de dos dígitos. Por defecto, el valor es 50.

```cobol
DISPLAY FUNCTION DAY-TO-YYYYDDD(24101,30).
DISPLAY FUNCTION DAY-TO-YYYYDDD(56101,30).
```

Resultado siendo el año actual 2024:

```
2024101 *> Representa el día 101 del año 2024
1956101 *> Representa el día 101 del año 1956
```

### FORMATTED-CURRENT-DATE

La función `FORMATTED-CURRENT-DATE` retorna la fecha actual en un formato específico.

```cobol
DISPLAY FUNCTION FORMATTED-CURRENT-DATE("YYYYMMDDThhmmss.ss+hhmm").
DISPLAY FUNCTION FORMATTED-CURRENT-DATE("YYYYDDDThhmmss").
```

Resultado:

```
20240212T141936.03+0300
2024043T141936
```

Se puede acceder a una guia de formatos clickeando [aquí](https://www.ibm.com/docs/en/cobol-zos/6.4?topic=sf-format-arguments-return-values-date-time-intrinsic-functions#INFFORM__date_and_time_format).

### FORMATTED-DATE

La función `FORMATTED-DATE` retorna la fecha en un formato específico.

Argumentos:
- Formato de la fecha. [Ver formatos](https://www.ibm.com/docs/en/cobol-zos/6.4?topic=sf-format-arguments-return-values-date-time-intrinsic-functions#INFFORM__date_and_time_format).
- Entero que representa la fecha. Se puede obtener con la función [`INTEGER-OF-DATE`](#integer-of-date).

```cobol
DISPLAY FUNCTION FORMATTED-DATE(
  "YYYY-MM-DD",
  154607
).
```

Resultado:

```
2024-04-19
```

### FORMATTED-DATETIME

La función `FORMATTED-DATETIME` retorna la fecha y hora en un formato específico.

Argumentos:
- **Formato** de la fecha. [Ver formatos](https://www.ibm.com/docs/en/cobol-zos/6.4?topic=sf-format-arguments-return-values-date-time-intrinsic-functions#INFFORM__date_and_time_format).
- Número entero que representa la **fecha**. Se puede obtener con la función [`INTEGER-OF-DATE`](#integer-of-date).
- Número entero que representa la **hora**. Se puede obtener con la función [`SECONDS-FROM-FORMATTED-TIME`](#seconds-from-formatted-time).
- _Opcional_: Un número entero que especifique la diferencia con respecto al tiempo universal coordinado (**UTC**) expresado en minutos. La magnitud del valor debe ser menor o igual a 1439.

```cobol
DISPLAY FUNCTION FORMATTED-DATETIME(
  "YYYY-MM-DDThh:mm:ss",
  154607,
  18867.812479168304
).
```

Resultado:

```
2024-04-19T05:14:27
```

Devuelve la fecha del segundo argumento y la hora del tercer argumento en el formato del primer argumento.

### FORMATTED-TIME

La función `FORMATTED-TIME` retorna la hora en un formato específico.

Argumentos:
- **Formato** de la hora. [Ver formatos](https://www.ibm.com/docs/en/cobol-zos/6.4?topic=sf-format-arguments-return-values-date-time-intrinsic-functions#INFFORM__date_and_time_format).
- Número entero que representa la **hora**. Se puede obtener con la función [`SECONDS-FROM-FORMATTED-TIME`](#seconds-from-formatted-time).

```cobol
DISPLAY FUNCTION FORMATTED-TIME(
  "hh:mm:ss",
  18867.812479168304
).
```

Resultado:

```
05:14:27
```

### INTEGER-OF-DATE

La función `INTEGER-OF-DATE` retorna el número entero que representa la fecha.

Argumentos:
- Fecha en formato `YYYYMMDD`.

```cobol
DISPLAY FUNCTION INTEGER-OF-DATE(20240419).
```

Resultado:

```
000154607
```

### INTEGER-OF-DAY

La función `INTEGER-OF-DAY` retorna el número entero que representa el día del año.

Argumentos:
- Fecha en formato `YYYYDDD`.

```cobol
DISPLAY FUNCTION INTEGER-OF-DAY(2024101).
```

Resultado:

```
000154598
```

### INTEGER-OF-FORMATTED-DATE

La función `INTEGER-OF-FORMATTED-DATE` retorna el número entero que representa la fecha a partir de un formato específico.

Argumentos:
- Formato de la fecha. [Ver formatos](https://www.ibm.com/docs/en/cobol-zos/6.4?topic=sf-format-arguments-return-values-date-time-intrinsic-functions#INFFORM__date_and_time_format).
- Fecha en el formato especificado.

```cobol
DISPLAY FUNCTION INTEGER-OF-FORMATTED-DATE(
  "YYYY-MM-DD",
  "2024-04-19"
).
```

Resultado:

```
000154607
```

### LOCALE-DATE

La función `LOCALE-DATE` retorna la fecha en el formato local, mas legible para el usuario. Por ejemplo, en español la fecha se mostraría en el formato `DD/MM/YYYY`.

Argumentos:
- Número entero que representa la fecha en el formato `YYYYMMDD`.

```cobol
DISPLAY FUNCTION LOCALE-DATE(20240419).
DISPLAY FUNCTION LOCALE-DATE(FUNCTION CURRENT-DATE).
```

Resultado en español:

```
19/4/2024  *> Fecha 154607
12/2/2024   *> Fecha actual
```

### LOCALE-TIME

La función `LOCALE-TIME` retorna la hora en el formato local, mas legible para el usuario.

Argumentos:
- Número entero que representa la hora en el formato `hhmmss`.

```cobol
DISPLAY FUNCTION LOCALE-TIME(182334).
```

Resultado:

```
18:23:34
```

### LOCALE-TIME-FROM-SECONDS

La función `LOCALE-TIME-FROM-SECONDS` retorna la hora en el formato local recibiendo la cantidad de segundos desde la medianoche.

Argumentos:
- Número entero que representa la hora en cantidad de segundos desde la medianoche. Es un valor que puede ir desde 0 hasta 86.399.

```cobol
DISPLAY FUNCTION LOCALE-TIME-FROM-SECONDS(2).
DISPLAY FUNCTION LOCALE-TIME-FROM-SECONDS(86399).
```

Resultado:

```
00:00:02
23:59:59
```

### SECONDS-FROM-FORMATTED-TIME

La función `SECONDS-FROM-FORMATTED-TIME` retorna la cantidad de segundos desde la medianoche a partir de un formato específico de la hora.

Argumentos:
- Formato de la hora. [Ver formatos](https://www.ibm.com/docs/en/cobol-zos/6.4?topic=sf-format-arguments-return-values-date-time-intrinsic-functions#INFFORM__date_and_time_format).
- Hora en el formato especificado.

```cobol
DISPLAY FUNCTION SECONDS-FROM-FORMATTED-TIME(
  "hhmmss",
  234612
).
```

Resultado:

```
000085572
```

### SECONDS-PAST-MIDNIGHT

La función `SECONDS-PAST-MIDNIGHT` retorna la cantidad de segundos desde la medianoche.

```cobol
DISPLAY FUNCTION SECONDS-PAST-MIDNIGHT.
```

Resultado:

```
000072068
```

### YEAR-TO-YYYY

La función `YEAR-TO-YYYY` convierte el formato `YY` al formato con año de cuatro dígitos `YYYY`.

Argumentos:
- Año en formato `YY`.
- _Opcional_: Define el final de un intervalo de 100 años, ayudando a determinar en qué siglo se coloca el año de dos dígitos. Por defecto, el valor es 50.

```cobol
DISPLAY FUNCTION YEAR-TO-YYYY(24,30).
DISPLAY FUNCTION YEAR-TO-YYYY(56,30).
```

Resultado siendo el año actual 2024:

```
000002024 *> Representa el año 2024
000001956 *> Representa el año 1956
```

### Resumen de Funciones de Fecha y Hora

```cobol
DISPLAY FUNCTION COMBINED-DATETIME(143951, 18867.812479168304). *> 143951.18867812479168304
DISPLAY FUNCTION CURRENT-DATE. *> 2024021213325776+0300
DISPLAY FUNCTION LOCALE-DATE(FUNCTION CURRENT-DATE). *> 12/2/2024
DISPLAY FUNCTION LOCALE-TIME(FUNCTION CURRENT-DATE). *> 20:24:02
DISPLAY FUNCTION DATE-OF-INTEGER(000154607). *> 20240419
DISPLAY FUNCTION DATE-TO-YYYYMMDD(240419,30). *> 020240419, 019560419
DISPLAY FUNCTION DAY-OF-INTEGER(000154607). *> 2024110
DISPLAY FUNCTION DAY-TO-YYYYDDD(24101,30). *> 2024101, 1956101
DISPLAY FUNCTION FORMATTED-CURRENT-DATE("YYYYMMDDThhmmss.ss+hhmm"). *> 20240212T141936.03+0300
DISPLAY FUNCTION FORMATTED-DATE("YYYY-MM-DD", 154607). *> 2024-04-19
DISPLAY FUNCTION FORMATTED-DATETIME("YYYY-MM-DDThh:mm:ss", 154607, 18867.812479168304). *> 2024-04-19T05:14:27
DISPLAY FUNCTION FORMATTED-TIME("hh:mm:ss", 18867.812479168304). *> 05:14:27
DISPLAY FUNCTION INTEGER-OF-DATE(20240419). *> 000154607
DISPLAY FUNCTION INTEGER-OF-DAY(2024101). *> 000154598
DISPLAY FUNCTION INTEGER-OF-FORMATTED-DATE("YYYY-MM-DD", "2024-04-19"). *> 000154607
DISPLAY FUNCTION LOCALE-DATE(20240419). *> 19/4/2024
DISPLAY FUNCTION LOCALE-DATE(FUNCTION CURRENT-DATE). *> 12/2/2024
DISPLAY FUNCTION LOCALE-TIME(182334). *> 18:23:34
DISPLAY FUNCTION LOCALE-TIME-FROM-SECONDS(2). *> 00:00:02
DISPLAY FUNCTION LOCALE-TIME-FROM-SECONDS(86399). *> 23:59:59
DISPLAY FUNCTION SECONDS-FROM-FORMATTED-TIME("hhmmss", 234612). *> 000085572
DISPLAY FUNCTION SECONDS-PAST-MIDNIGHT. *> 000072068
DISPLAY FUNCTION YEAR-TO-YYYY(24,30). *> 000002024
DISPLAY FUNCTION YEAR-TO-YYYY(56,30). *> 000001956
```

## Funciones Financieras

### ANNUITY

La función `ANNUITY` devuelve un valor numérico que aproxima la relación entre una anualidad pagada al final de cada período, para un número determinado de períodos, a una tasa de interés determinada, y un valor inicial de uno.

Argumentos:
- Tasa de interés.
- Número de períodos.


```cobol
DISPLAY FUNCTION ANNUITY(0, 4).
```

Resultado:

```
0000000.25
```


Cuando el valor del argumento 1 es cero, el valor devuelto por la función es la aproximación de:

```
1 / argumento-2
```

Cuando el valor del argumento 1 no es cero, el valor de la función es la aproximación de:

```
argumento-1 / (1 - (1 + argumento-1 ) ** (- argumento-2 ))
```

### PRESENT-VALUE

La función `PRESENT-VALUE` devuelve un valor que se aproxima al valor presente de una serie de montos futuros de fin de período especificados a una tasa de descuento también especificada.

Argumentos:
- Tasa de descuento. Debe ser numérico.
- Serie de montos futuros de fin de período. Deben ser numéricos.

```cobol
DISPLAY FUNCTION PRESENT-VALUE(0.1, 100, 200, 300).
```

Resultado:

```
481.5927873779...
```

### Resumen de Funciones Financieras

```cobol
DISPLAY FUNCTION ANNUITY(0, 4).                       *> 0000000.25
DISPLAY FUNCTION PRESENT-VALUE(0.1, 100, 200, 300).   *> 481.5927873779...
```

## Funciones de Manejo de Caracteres

### CHAR

La función `CHAR` retorna el carácter que corresponde a un número ASCII.

Argumentos:
- Número entero que representa el carácter ASCII. Valores desde 1 hasta 256.

```cobol
DISPLAY FUNCTION CHAR(66).
```

> Los valores ASCII en COBOL van desde 1 hasta 256, no del 0 al 255 como en otros lenguajes. Por lo tanto, siempre que se quiera un simbolo ASCII, se le debe sumar 1 al valor que se quiere obtener.


Resultado:

```
A
```

### CONCATENATE

La función `CONCATENATE` retorna la concatenación de dos o más cadenas.

Argumentos:
- Cadena 1.
- Cadena 2.
- _Opcional_: Cadena 3, Cadena 4, ..., Cadena n.

```cobol
DISPLAY FUNCTION CONCATENATE("Hola"," ","mundo","!"," Como estas?").
```

Resultado:

```
Hola mundo! Como estas?
```

### LENGTH

La función `LENGTH` retorna la longitud de una cadena.

Argumentos:
- Cadena.

```cobol
DISPLAY FUNCTION LENGTH("Hola mundo!").
```

Resultado:

```
11
```

Lo importante de esta función es entender que si le pasamos una variable en lugar de una cadena directamente, nos retornará la longitud en memoria que ocupa la variable, no la longitud de la cadena que contiene.

```cobol
77  CADENA  PIC X(15) VALUE "Hola mundo!".

DISPLAY FUNCTION LENGTH(CADENA).
```

Resultado:

```
15
```

### LOWER-CASE

La función `LOWER-CASE` retorna la cadena en minúsculas.

Argumentos:
- Cadena.

```cobol
DISPLAY FUNCTION LOWER-CASE("Hola mundo!").
```

Resultado:

```
hola mundo!
```

### NATIONAL-OF

BORRARLA

### ORD

La función `ORD` retorna el valor ASCII de un carácter.

Argumentos:
- Carácter.

```cobol
DISPLAY FUNCTION ORD("A").
```

Resultado:

```
66
```

### ORD-MAX

La función `ORD-MAX` retorna la posición del valor ASCII máximo de una lista de caracteres.

Argumentos:
- Carácter 1.
- Carácter 2.
- _Opcional_: Carácter 3, Carácter 4, ..., Carácter n.

```cobol
DISPLAY FUNCTION ORD-MAX("H","Z","A").
```

Resultado:

```
2
```

En este caso el valor de "Z" es el mayor, por lo que retorna 2 que es la posición de "Z" en la lista que se le pasó.

### ORD-MIN

La función `ORD-MIN` retorna la posición del valor ASCII mínimo de una lista de caracteres.

Argumentos:
- Carácter 1.
- Carácter 2.
- _Opcional_: Carácter 3, Carácter 4, ..., Carácter n.

```cobol
DISPLAY FUNCTION ORD-MIN("H","Z","A").
```

Resultado:

```
3
```

En este caso el valor de "A" es el menor, por lo que retorna 3 que es la posición de "A" en la lista que se le pasó.

### REVERSE

La función `REVERSE` retorna la cadena invertida.

Argumentos:
- Cadena.

```cobol
DISPLAY FUNCTION REVERSE("Hola mundo!").
```

Resultado:

```
!odnum aloH
```

### STORED-CHAR-LENGTH

La función `STORED-CHAR-LENGTH` retorna la longitud de una cadena almacenada.

Argumentos:
- Cadena.

```cobol
77  CADENA  PIC X(15) VALUE "Hola mundo!".

DISPLAY FUNCTION STORED-CHAR-LENGTH("Hola mundo!").
```

Resultado:

```
11
```

### SUBSTITUTE

La función `SUBSTITUTE` retorna la cadena con todas las ocurrencias de un carácter reemplazadas por otro.

Argumentos:
- Cadena.

```cobol
DISPLAY FUNCTION SUBSTITUTE("Hola mundo!","o","0").
```

Resultado:

```
H0la mund0!
```

### SUBSTITUTE-CASE

La función `SUBSTITUTE-CASE` retorna la cadena con todas las ocurrencias de un carácter reemplazadas por otro, sin importar si son mayúsculas o minúsculas.

Argumentos:
- Cadena.

```cobol
DISPLAY FUNCTION SUBSTITUTE-CASE("Mi gatita Mimi esta mauyando.","m","l").
```

Resultado:

```
li gatita lili esta lauyando.
```

### TRIM

La función `TRIM` retorna la cadena sin espacios en blanco al principio y al final.

Argumentos:
- Cadena.

```cobol
DISPLAY FUNCTION TRIM("    Hola mundo!    ").
```

Resultado:

```
Hola mundo!
```

### UPPER-CASE

La función `UPPER-CASE` retorna la cadena en mayúsculas.

Argumentos:
- Cadena.

```cobol
DISPLAY FUNCTION UPPER-CASE("Hola mundo!").
```

Resultado:

```
HOLA MUNDO!
```

### Resumen de Funciones de Manejo de Caracteres

```cobol
DISPLAY FUNCTION CHAR(66).                                                  *> A
DISPLAY FUNCTION CONCATENATE("Hola"," ","mundo","!"," Como estas?").        *> Hola mundo! Como estas?
DISPLAY FUNCTION LENGTH("Hola mundo!").                                     *> 11
DISPLAY FUNCTION LOWER-CASE("Hola mundo!"). *> hola mundo!
DISPLAY FUNCTION ORD("A").                                                  *> 66
DISPLAY FUNCTION ORD-MAX("H","Z","A").                                      *> 2
DISPLAY FUNCTION ORD-MIN("H","Z","A").                                      *> 3
DISPLAY FUNCTION REVERSE("Hola mundo!").                                    *> !odnum aloH
DISPLAY FUNCTION STORED-CHAR-LENGTH("Hola mundo!").                         *> 11
DISPLAY FUNCTION SUBSTITUTE("Hola mundo!","o","0").                         *> H0la mund0!
DISPLAY FUNCTION SUBSTITUTE-CASE("Mi gatita Mimi esta mauyando.","m","l").  *> li gatita lili esta lauyando.
DISPLAY FUNCTION TRIM("    Hola mundo!    ").                               *> Hola mundo!
DISPLAY FUNCTION UPPER-CASE("Hola mundo!").                                 *> HOLA MUNDO!
```

## Funciones Estadísticas

### MAX

La función `MAX` retorna el valor máximo de una lista de números.

Argumentos:
- Número 1.
- Número 2.
- _Opcional_: Número 3, Número 4, ..., Número n.

```cobol
DISPLAY FUNCTION MAX(1, 2, 6, 4, 5).
```

Resultado:

```
6
```

### MEAN

La función `MEAN` retorna el promedio de una lista de números.

Argumentos:
- Número 1.
- Número 2.
- _Opcional_: Número 3, Número 4, ..., Número n.

```cobol
DISPLAY FUNCTION MEAN(10, 7, 8, 2, 9).
```

Resultado:

```
7.2
```

### MEDIAN

La función `MEDIAN` retorna la mediana de una lista de números.

Argumentos:
- Número 1.
- Número 2.
- _Opcional_: Número 3, Número 4, ..., Número n.

```cobol
DISPLAY FUNCTION MEDIAN(10, 7, 8, 2, 9).
```

Resultado:

```
8
```

### MIDRANGE

La función `MIDRANGE` retorna el promedio del valor máximo y el valor mínimo de una lista de números.

Argumentos:
- Número 1.
- Número 2.
- _Opcional_: Número 3, Número 4, ..., Número n.

```cobol
DISPLAY FUNCTION MIDRANGE(10, 7, 8, 2, 9).
```

Resultado:

```
6
```

### MIN

La función `MIN` retorna el valor mínimo de una lista de números.

Argumentos:
- Número 1.
- Número 2.
- _Opcional_: Número 3, Número 4, ..., Número n.

```cobol
DISPLAY FUNCTION MIN(1, 2, 6, 4, 5).
```

Resultado:

```
1
```

### RANGE

La función `RANGE` retorna la diferencia entre el valor máximo y el valor mínimo de una lista de números.

Argumentos:
- Número 1.
- Número 2.
- _Opcional_: Número 3, Número 4, ..., Número n.

```cobol
DISPLAY FUNCTION RANGE(10, 7, 8, 2, 9).
```

Resultado:

```
8
```

### STANDARD-DEVIATION

La función `STANDARD-DEVIATION` retorna la desviación estándar de una lista de números.

Argumentos:
- Número 1.
- Número 2.
- _Opcional_: Número 3, Número 4, ..., Número n.

```cobol
DISPLAY FUNCTION STANDARD-DEVIATION(10, 7, 8, 2, 9).
```

Resultado:

```
2.78567765543682386...
```

### SUM

La función `SUM` retorna la suma de una lista de números.

Argumentos:
- Número 1.
- Número 2.
- _Opcional_: Número 3, Número 4, ..., Número n.

```cobol
DISPLAY FUNCTION SUM(10, 7, 8, 2, 9).
```

Resultado:

```
36
```

### VARIANCE

La función `VARIANCE` retorna la varianza de una lista de números.

Argumentos:
- Número 1.
- Número 2.
- _Opcional_: Número 3, Número 4, ..., Número n.

```cobol
DISPLAY FUNCTION VARIANCE(10, 7, 8, 2, 9).
```

Resultado:

```
7.76
```

### Resumen de Funciones Estadísticas

```cobol
DISPLAY FUNCTION MAX(1, 2, 6, 4, 5).                  *> 6
DISPLAY FUNCTION MEAN(10, 7, 8, 2, 9).                *> 7.2
DISPLAY FUNCTION MEDIAN(10, 7, 8, 2, 9).              *> 8
DISPLAY FUNCTION MIDRANGE(10, 7, 8, 2, 9).            *> 6
DISPLAY FUNCTION MIN(1, 2, 6, 4, 5).                  *> 1
DISPLAY FUNCTION RANGE(10, 7, 8, 2, 9).               *> 8
DISPLAY FUNCTION STANDARD-DEVIATION(10, 7, 8, 2, 9).  *> 2.78567765543682386...
DISPLAY FUNCTION SUM(10, 7, 8, 2, 9).                 *> 36
DISPLAY FUNCTION VARIANCE(10, 7, 8, 2, 9).            *> 7.76
```

## Funciones Generales

### BOOLEAN-OF-INTEGER

BORRARLA

### INTEGER

La función `INTEGER` retorna el valor entero de un número.

Argumentos:
- Número.

```cobol
DISPLAY FUNCTION INTEGER(5.6).
```

Resultado:

```
5
```

### INTEGER-OF-BOOLEAN

BORRARLA

### INTEGER-PART

Es un sinónimo de `INTEGER`, retorna el valor entero de un número.

Argumentos:
- Número.

```cobol
DISPLAY FUNCTION INTEGER-PART(5.6).
```

Resultado:

```
5
```

### MODULE-ID

Esta función retorna el identificador del módulo actual.

```cobol
DISPLAY FUNCTION MODULE-ID.
```

### MODULE-SOURCE

Esta función retorna el nombre del archivo fuente del módulo actual.

```cobol
DISPLAY FUNCTION MODULE-SOURCE.
```



### MODULE-CALLER-ID

Esta función retorna el identificador del módulo que invocó al módulo actual.

```cobol
DISPLAY FUNCTION MODULE-CALLER-ID.
```

### MODULE-DATE

Esta función retorna la fecha de compilación del módulo actual.

```cobol
DISPLAY FUNCTION MODULE-DATE.
```

### MODULE-FORMATTED-DATE

Esta función retorna la fecha de compilación del módulo actual formateada.


```cobol
DISPLAY FUNCTION MODULE-FORMATTED-DATE.
```

Resultado:

```
feb. 13 2024 01:10:59
```

### MODULE-PATH

Esta función retorna la ruta del archivo fuente del módulo actual.

```cobol
DISPLAY FUNCTION MODULE-PATH.
```

### MODULE-TIME

Esta función retorna la hora de compilación del módulo actual.

```cobol
DISPLAY FUNCTION MODULE-TIME.
```

### LOCALE-COMPARE

La función `LOCALE-COMPARE` compara dos valores de cualquier tipo y retorna un caracter que indica si el primer valor es menor, igual o mayor que el segundo.

Argumentos:
- Valor 1.
- Valor 2.

```cobol
DISPLAY FUNCTION LOCALE-COMPARE(5,5).
DISPLAY FUNCTION LOCALE-COMPARE("Hola","hola").
DISPLAY FUNCTION LOCALE-COMPARE("a","t").
```

Resultado:

```
=
>
<
```

### NUMVAL

La función `NUMVAL` retorna el valor numérico de una cadena.

Argumentos:
- Cadena.

```cobol
DISPLAY FUNCTION NUMVAL("746").
```

Resultado:

```
000000746
```

### NUMVAL-C

La función `NUMVAL-C` retorna el valor numérico de una cadena, ignorando los valores de moneda, si los hay, y cualquier separador de agrupación (comas o puntos) para producir un valor numérico.

Argumentos:
- Cadena.

```cobol
DISPLAY FUNCTION NUMVAL-C("$746,454").
```

Resultado:

```
000746454
```

> Recordar que la coma es un separador de miles por defecto en Cobol, como el punto en español.


### NUMVAL-F

La función `NUMVAL-F` retorna el valor numérico de una cadena, la misma esta hecha especialmente para manejar valores expresados en notación científica. Lo que en COBOL se realiza con la letra E.

Argumentos:
- Cadena.

```cobol
DISPLAY FUNCTION NUMVAL-F("74645E+03").
```

Resultado:

```
074645000
```

### NUMERIC-DECIMAL-POINT

La función `NUMERIC-DECIMAL-POINT` retorna el carácter que se utiliza como separador decimal.

```cobol
DISPLAY FUNCTION NUMERIC-DECIMAL-POINT.
```

Por defecto, el resultado es el punto `.` (Aunque se puede cambiar con la directiva `DECIMAL-POINT IS COMMA`).

```
.
```

### NUMERIC-THOUSANDS-SEPARATOR

La función `NUMERIC-THOUSANDS-SEPARATOR` retorna el carácter que se utiliza como separador de miles.

```cobol
DISPLAY FUNCTION NUMERIC-THOUSANDS-SEPARATOR.
```

Por defecto, el resultado es "", es decir, no hay separador de miles (Aunque se puede cambiar con la directiva `THOUSANDS-SEPARATOR IS COMMA` o `THOUSANDS-SEPARATOR IS DOT`), lo que podemos ver cuando se imprime un número

### STANDARD-COMPARE

BORRAR

### Funciones TEST

Las funciones que comienzan con `TEST-` seguido del nombre de otra función intrínseca, son funciones que retornan un valor que indica si el valor que se le pasó como argumento es válido para la función.

Si el valor es válido, retorna el número 0, sino, retorna el valor de la posición en la que se encontró el error.

#### TEST-NUMVAL

La función `TEST-NUMVAL` retorna un valor numérico que indica si el valor que se le pasó como argumento es válido para la función [`NUMVAL`](#numval).

Si el valor es válido, retorna el número 0, sino, retorna el valor de la posición en la que se encontró el error.

Argumentos:
- Cadena.

```cobol
DISPLAY FUNCTION TEST-NUMVAL("746").
DISPLAY FUNCTION TEST-NUMVAL("34.3E+03").
DISPLAY FUNCTION TEST-NUMVAL(" ").
```

Resultado:

```
000000000
000000005
000000002

```

- El primer valor da 0, ya que el parametro dado es válido para la función `NUMVAL`
- El segundo valor da 5, ya que se encontró un error en la posición 5, que es la "E", ya que no se puede usar la notación científica en la función `NUMVAL`, solo en `NUMVAL-F`.
- El tercer valor da 2, lo que es el LENGTH de la cadena más 1 (`LENGTH(" ") + 1`). Esta condición se da cuando:
  - El argumento tiene longitud cero.
  - El argumento solo contiene espacios.  <-- Este es el caso de este ejemplo.
  - El argumento contiene caracteres validos pero esta incompleto.

#### TEST-NUMVAL-C

La función `TEST-NUMVAL-C` retorna un valor numérico que indica si el valor que se le pasó como argumento es válido para la función [`NUMVAL-C`](#numval-c).

Si el valor es válido, retorna el número 0, sino, retorna el valor de la posición en la que se encontró el error.

Argumentos:
- Cadena.

```cobol
DISPLAY FUNCTION TEST-NUMVAL-C("$746,454").
DISPLAY FUNCTION TEST-NUMVAL-C("34.3E+03").
DISPLAY FUNCTION TEST-NUMVAL-C(" ").
```

Resultado:

```
000000000
000000005
000000002
```

Esto funciona de la misma manera que [`TEST-NUMVAL`](#test-numval), pero para los valores que se le pasarían a `NUMVAL-C`.

#### TEST-NUMVAL-F

La función `TEST-NUMVAL-F` retorna un valor numérico que indica si el valor que se le pasó como argumento es válido para la función [`NUMVAL-F`](#numval-f).

Si el valor es válido, retorna el número 0, sino, retorna el valor de la posición en la que se encontró el error.

Argumentos:
- Cadena.

```cobol
DISPLAY FUNCTION TEST-NUMVAL-F("34.3E+03").
DISPLAY FUNCTION TEST-NUMVAL-F("$3434").
DISPLAY FUNCTION TEST-NUMVAL-F("   ").
```

Resultado:

```
000000000
000000001
000000004
```

Esto funciona de la misma manera que [`TEST-NUMVAL`](#test-numval), pero para los valores que se le pasarían a `NUMVAL-F`.

#### TEST-DAY-YYYYDDD

La función `TEST-DAY-YYYYDDD` retorna un valor numérico que indica si la fecha en formato Juliano que se le pasó como argumento es una fecha válida en el calendario gregoriano.

Argumentos:
- Fecha en formato `YYYYDDD`.

```cobol
DISPLAY FUNCTION TEST-DAY-YYYYDDD(2024110).
DISPLAY FUNCTION TEST-DAY-YYYYDDD(1200187).
DISPLAY FUNCTION TEST-DAY-YYYYDDD(2023366).
```

Resultado:

```
000000000
000000001
000000002
```

- El primer valor da 0, ya que el parametro dado es válido para la función `DAY-YYYYDDD`
- El segundo valor da 1, ya que se encontró un error en el año, ya que el valor es menor a 1601 o mayor a 9999.
- El tercer valor da 2, ya el día no es válido en el año dado. Si fuera un año bisiesto, el valor máximo sería 366, sino, 365.

#### TEST-DATE-YYYYMMDD

La función `TEST-DATE-YYYYMMDD` retorna un valor numérico que indica si la fecha en formato `YYYYMMDD` que se le pasó como argumento es una fecha válida en el calendario gregoriano.

Argumentos:
- Fecha en formato `YYYYMMDD`.

```cobol
DISPLAY FUNCTION TEST-DATE-YYYYMMDD(20240419).
DISPLAY FUNCTION TEST-DATE-YYYYMMDD(12000604).
DISPLAY FUNCTION TEST-DATE-YYYYMMDD(20241323).
DISPLAY FUNCTION TEST-DATE-YYYYMMDD(20240454).
```

Resultado:

```
000000000
000000001
000000002
000000003
```

- El primer valor da 0, ya que el parametro dado es válido para la función `DATE-YYYYMMDD`
- El segundo valor da 1, ya que se encontró un error en el año, ya que el valor es menor a 1601 o mayor a 9999.
- El tercer valor da 2, ya el mes no es válido en el año dado.
- El cuarto valor da 3, ya el día no es válido en el año y mes dado.

#### TEST-FORMATTED-DATETIME

La función `TEST-FORMATTED-DATETIME` retorna un valor numérico que indica si la fecha y hora en formato específico que se le pasó como argumento es una fecha y hora válida en el calendario gregoriano.

Argumentos:
- Formato de la fecha y hora. [Ver formatos](https://www.ibm.com/docs/en/cobol-zos/6.4?topic=sf-format-arguments-return-values-date-time-intrinsic-functions#INFFORM__date_and_time_format).
- Fecha y hora en el formato especificado.

```cobol
DISPLAY FUNCTION TEST-FORMATTED-DATETIME(
  "YYYY-MM-DDThh:mm:ss",
  "2024-04-19T05:14:27"
).

DISPLAY FUNCTION TEST-FORMATTED-DATETIME(
  "YYYY-MM-DDThh:mm:ss",
  "24-04-19T25:14:27"
).
```

Resultado:

```
000000000
000000003
```

Si el valor es válido, retorna el número 0.
Si el valor no es válido, retorna el número de la posición en la que se encontró el error en la cadena.


### WHEN-COMPILED

La función `WHEN-COMPILED` retorna la fecha y hora de compilación del módulo actual.

```cobol
DISPLAY FUNCTION WHEN-COMPILED.
```

### Resumen de Funciones Generales

```cobol
DISPLAY FUNCTION INTEGER(5.6).                               *> 5
DISPLAY FUNCTION INTEGER-PART(5.6).                          *> 5
DISPLAY FUNCTION MODULE-ID.                                   *> Nombre del módulo
DISPLAY FUNCTION MODULE-SOURCE.                               *> Nombre del archivo fuente
DISPLAY FUNCTION MODULE-CALLER-ID.                            *> Nombre del módulo que lo invocó
DISPLAY FUNCTION MODULE-DATE.                                 *> Fecha de compilación
DISPLAY FUNCTION MODULE-FORMATTED-DATE.                       *> Fecha de compilación formateada
DISPLAY FUNCTION MODULE-PATH.                                 *> Ruta del archivo fuente
DISPLAY FUNCTION MODULE-TIME.                                 *> Hora de compilación
DISPLAY FUNCTION LOCALE-COMPARE(5,5).                         *> =
DISPLAY FUNCTION LOCALE-COMPARE("Hola","hola").               *> >
DISPLAY FUNCTION LOCALE-COMPARE("a","t").                     *> <
DISPLAY FUNCTION NUMVAL("746").                               *> 000000746
DISPLAY FUNCTION NUMVAL-C("$746,454").                        *> 000746454
DISPLAY FUNCTION NUMVAL-F("74645E+03").                       *> 074645000
DISPLAY FUNCTION NUMERIC-DECIMAL-POINT.                       *> .
DISPLAY FUNCTION NUMERIC-THOUSANDS-SEPARATOR.                *> ""
DISPLAY FUNCTION STANDARD-COMPARE("Hola","hola").             *> >
DISPLAY FUNCTION TEST-NUMVAL("746").                          *> 000000000
DISPLAY FUNCTION TEST-NUMVAL-C("$746,454").                   *> 000000000
DISPLAY FUNCTION TEST-NUMVAL-F("74645E+03").                  *> 000000000
DISPLAY FUNCTION TEST-DAY-YYYYDDD(2024110).                   *> 000000000
DISPLAY FUNCTION TEST-DAY-YYYYDDD(1200187).                   *> 000000001
DISPLAY FUNCTION TEST-DAY-YYYYDDD(2023366).                   *> 000000002
DISPLAY FUNCTION TEST-DATE-YYYYMMDD(20240419).                *> 000000000
DISPLAY FUNCTION TEST-DATE-YYYYMMDD(12000604).                *> 000000001
DISPLAY FUNCTION TEST-DATE-YYYYMMDD(20241323).                *> 000000002
DISPLAY FUNCTION TEST-DATE-YYYYMMDD(20240454).                *> 000000003
DISPLAY FUNCTION TEST-FORMATTED-DATETIME(
  "YYYY-MM-DDThh:mm:ss",
  "2024-04-19T05:14:27"
).                                                            *> 000000000
DISPLAY FUNCTION TEST-FORMATTED-DATETIME(
  "YYYY-MM-DDThh:mm:ss",
  "24-04-19T25:14:27"
).                                                            *> 000000003
DISPLAY FUNCTION WHEN-COMPILED.                               *> Fecha y hora de compilación
```