# Operaciones en Cobol

Las operaciones en Cobol son muy similares a las de otros lenguajes de programación, pero con algunas diferencias. En este Cheat Sheet se describen las operaciones aritméticas, lógicas y de comparación.

1. [Operaciones Aritméticas](#operaciones-aritméticas)
   1. [Utilizando el verbo `COMPUTE`](#utilizando-el-verbo-compute)
      1. [Números Enteros](#números-enteros)
      2. [Números Decimales](#números-decimales)
   2. [Utilizando los verbos `ADD`, `SUBTRACT`, `MULTIPLY` y `DIVIDE`](#utilizando-los-verbos-add-subtract-multiply-y-divide)
      1. [Suma](#suma)
      2. [Resta](#resta)
      3. [Multiplicación](#multiplicación)
      4. [División](#división)
2. [Operaciones Condicionales](#operaciones-condicionales)
   1. [Operadores de Comparación](#operadores-de-comparación)
   2. [Operadores Lógicos](#operadores-lógicos)
   3. [Condicionales de Tipado y Clasificación](#condicionales-de-tipado-y-clasificación)
      - [Clases Condicionales](#clases-condicionales)

- [Datos del Cheat Sheet](#datos-del-cheat-sheet)

## Operaciones Aritméticas

Las operaciones aritméticas en Cobol se pueden realizar de dos maneras diferentes. La primera es utilizando el verbo `COMPUTE` y la segunda es utilizando los verbos `ADD`, `SUBTRACT`, `MULTIPLY` o `DIVIDE` dependiendo de qué operacíón se desea aplicar.

### Utilizando el verbo `COMPUTE`

La sintaxis del verbo `COMPUTE` es la más parecida a los lenguajes convencionales de la actualidad, ya que se utiliza el signo `=` para asignar el resultado de la operación a una variable.

Vamos a aprovechar esta simplicidad para entender cómo realizar las operaciones aritméticas correctamente con las variables cuyo PICTURE sea el correcto para cada situación.

#### Números Enteros

La suma con el verbo `COMPUTE` se realiza con el símbolo `+`. Por ejemplo, si queremos sumar dos números enteros `num1` y `num2` y guardar el resultado en la variable `res`, podemos hacerlo de la siguiente manera:

```cobol
COMPUTE res = num1 + num2
```

Pero para esto necesitamos tener creadas las variables `res`, `num1` y `num2` con el PICTURE correcto.


```cobol
01 num1 PIC 99 VALUE 25.
01 num2 PIC 99 VALUE 15.
01 res	PIC 999 VALUE ZERO.
```

En este caso, debemos tener en cuenta que la suma de dos **números enteros** puede dar como máximo resultado un número de tres dígitos, por lo que el PICTURE de la variable `res` debe ser `PIC 999` o `PIC 9(3)`.

Si imprimimos el contenido de la variables `res` con el comando `DISPLAY "Resultado: " res` obtendremos:

```
Resultado: 040
```

Estas operaciones se pueden realizar con los operadores `+`, `-`, `*` y `/` para sumar, restar, multiplicar y dividir respectivamente. Todas funcionan de la misma manera. Además, podemos realizar operaciones complejas con paréntesis para dar prioridad a las operaciones.

```cobol
COMPUTE res = (num1 + num2) / (3 * num3) - num4
```

#### Números Decimales

Más alla del verbo que utilicemos para realizar las operaciones, es importante tener en cuenta que las variables que vamos a utilizar deben tener el PICTURE correcto para el tipo de dato que vamos a utilizar o precisemos en nuestro programa.

```cobol
01 NUM1 PIC 99V99 VALUE 10.25.
01 NUM2 PIC 99V99 VALUE 65.20.
01 RES	PIC S99V99 VALUE ZERO.
```

Si hacemos una resta con el verbo `COMPUTE`:

```cobol
COMPUTE RES = NUM1 - NUM2
```

El resultado será:

```
-54.95
```

Ya que en la respuesta se incluye el signo de la variable y el punto decimal con dos decimales. Vemos que no es importante que el PICTURE de las variables `NUM1` y `NUM2` incluyan el signo si no vamos a utilizarlo.


### Utilizando los verbos `ADD`, `SUBTRACT`, `MULTIPLY` y `DIVIDE`

Estos verbos son específicos para cada operación. Los mismos tienen una sintaxis más orientada a lo coloquial y no a lo matemática e incluyen algunas funcionalidades específicas de guardado de resultados en variables.

#### Suma

La suma entre dos valores se realiza con el verbo `ADD` y la preposición `TO`. Por ejemplo, si queremos sumar dos números enteros `num1` y `num2`, podemos hacerlo de la siguiente manera:

```cobol
ADD num1 TO num2.
```

En este caso, el resultado de la suma se guardará en la variable `num2`. Si queremos guardar el resultado en otra variable, podemos hacerlo de la siguiente manera:

```cobol
ADD num1 TO num2 GIVING res.
```

Además, podemos realizar varias operaciones en una sola línea:

```cobol
ADD num1 TO num2, num3, num4.
```

En este caso, el resultado de la suma `num1 + num2` se guardará en `num2`, el resultado de la suma `num1 + num3` se guardará en `num3` y el resultado de la suma `num1 + num4` se guardará en `num4`.

También podemos guardar el mismo resultado de una operacion en varias variables:

```cobol
ADD num1 TO num2 GIVING res1, res2.
```

Aquí, el mismo resultado de la suma `num1 + num2` se guardará en `res1` y `res2`.

> Todo lo visto para la suma aplica para todos los verbos de operaciones aritméticas que veremos a continuación.

#### Resta

La resta entre dos valores se realiza con el verbo `SUBTRACT` y la preposición `FROM`. Por ejemplo, si queremos restar dos números enteros `num1` y `num2`, podemos hacerlo de la siguiente manera:

```cobol
SUBTRACT num1 FROM num2.
```

Al igual que el verbo de la suma, se le puede agregar la preposición `GIVING` y tener varias operaciones en una sola línea.

```cobol
SUBTRACT num1 FROM num2.
SUBTRACT num1 FROM num2 GIVING RES.
SUBTRACT num1 FROM num2, num3, num4.
SUBTRACT num1 FROM num2 GIVING res1, res2.
```

#### Multiplicación

La multiplicación entre dos valores se realiza con el verbo `MULTIPLY` y la preposición `BY`. Por ejemplo, si queremos multiplicar dos números enteros `num1` y `num2`, podemos hacerlo de la siguiente manera:

```cobol
MULTIPLY num1 BY num2.
```

Tengamos en cuenta en la multiplicación que el resultado puede ser un número de la sumatoria de dígitos que los números que estamos multiplicando, por lo que el PICTURE de la variable que va a guardar el resultado debe ser acorde a esto.

```cobol
01 num1 PIC 9(2).
01 num2 PIC 9(3).
01 res	PIC 9(5) VALUE ZERO.
```

Como vemos, el PICTURE de la variable `res` es `PIC 9(5)` ya que el resultado de la multiplicación de dos números, uno de **2 dígitos** y el otro de **3 dígitos** (2+3) puede ser de hasta **5 dígitos**.

#### División

Mientras que la división entre dos valores se realiza con el verbo `DIVIDE` y la misma preposición `BY`. Por ejemplo, si queremos dividir dos números enteros `num1` y `num2`, podemos hacerlo de la siguiente manera:

```cobol
DIVIDE num1 BY num2.
```

En la división hay que tener en cuenta dos cosas importantes:
- Al contrario que la multiplicación, el resultado puede ser un número de hasta la cantidad de dígitos del dividendo, por lo que el PICTURE de la variable que va a guardar el resultado debe ser acorde a esto.
- El resultado de la división puede tener decimales, si esto no se tiene en cuenta en el PICTURE de la variable que va a guardar el resultado, el resultado se va a truncar al guardarlo en la variable.

```cobol
01 num1 PIC 9(3).
01 num2 PIC 9(2).
01 res	PIC 9(3)V9(2) VALUE ZERO.
```

En este caso, el PICTURE de la variable `res` es `PIC 9(3)V9(2)` ya que el resultado de la división de dos números, uno de **3 dígitos** y el otro de **2 dígitos** (3/2) puede ser de hasta **3 dígitos** y le agregamos **2 decimales** para poder visualizar el resultado con más precisión.


## Operaciones Condicionales

### Operadores de Comparación

Los operadores de comparación en Cobol son similares a los de otros lenguajes de programación. Aunque existen dos maneras de escribir condiciones en el mismo: Con verbos o con operadores lógicos. A continuación se muestra cómo se haría cada uno de ellos con su respectiva sintaxis.

| Verbo            | Operador |
| ---------------- | -------- |
| EQUAL            | =        |
| GREATER          | >        |
| GREATER OR EQUAL | >=       |
| LESS OR EQUAL    | <=       |
| LESS             | <        |

Ejemplo:
    
```cobol 
IF num1 GREATER num2
    DISPLAY "num1 es mayor que num2"
END-IF
```

Este código sería lo mismo que el siguiente:

```cobol
IF num1 > num2
    DISPLAY "num1 es mayor que num2"
END-IF
```

También se puede utilizar la preposición `THAN` para ser más expresivo, al igual que podemos agregarle la preposición `IS` que tiene el _IF_.

```cobol
IF num1 IS LESS OR EQUAL THAN num2
    DISPLAY "num1 es mayor que num2"
END-IF
```

En inglés, la frase del condicional se traduciría a "Si num1 es menor o igual que num2, mostrar 'num1 es mayor que num2'". Esto es posible ya que Cobol es un lenguaje de programación de muy alto nivel y se puede escribir de manera muy expresiva.

### Operadores Lógicos

Los operadores lógicos en Cobol solamente se pueden utilizar con verbos. A continuación se muestra una tabla con los verbos y su respectiva traducción a operadores lógicos.

| Verbo | Descripción                  |
| ----- | ---------------------------- |
| AND   | Operador lógico '_y_'        |
| OR    | Operador lógico '_o_'        |
| NOT   | Operador lógico '_negación_' |

El operador lógico `AND` se utiliza para unir dos condiciones y que ambas sean verdaderas para que se ejecute el bloque de código.

El operador lógico `OR` se utiliza para unir dos condiciones y que al menos una de ellas sea verdadera para que se ejecute el bloque de código.

El operador lógico `NOT` se utiliza para negar una condición, es decir, si la condición es verdadera, se vuelve falsa y si la condición es falsa, se vuelve verdadera.

Ejemplo:

```cobol
IF num1 IS GREATER THAN num2 AND num1 NOT LESS num3
    DISPLAY "num1 es mayor que num2 y menor que num3"
END-IF
```

También podemos separa por bloques las condiciones para hacer más legible el código y separar de manera correcta las condiciones que se quieren evaluar.

```cobol
IF (num1 EQUAL num2 OR num1 EQUAL num3) AND (num1 NOT LESS num3)
    DISPLAY "num1 es mayor que num2 y menor que num3"
END-IF
```

### Condicionales de Tipado y Clasificación

En Cobol, las variables tienen un tipo de dato específico que se define con el PICTURE. Por lo tanto, se pueden realizar condicionales para verificar si una variable es de un tipo de dato específico.

Para saber si la variable `var1` es un número lo hacemos de la siguiente manera:

```cobol
IF var1 IS NUMERIC
```

Como otro ejemplo, podemos verificar si la variable `var2` es alfabética:

```cobol
IF var2 IS ALPHABETIC
```

Además de poder saber si es alfabética, también podemos saber si esta escrita toda en minúscula o toda en mayúscula:

```cobol
IF var2 IS ALPHABETIC-LOWER
IF var2 IS ALPHABETIC-UPPER
```

Así como utilizando los operadores anteriores podemos negar estas condiciones:

```cobol
IF var2 IS NOT ALPHABETIC-UPPER
```

#### Clases Condicionales

Además de las condiciones de tipado, podemos crear nuestras propias condiciones para saber si una variable cumple con ciertas características y hacer una comparación simple en nuestro programa. Para ello existen lo que son llamadas _clases_ en Cobol.

> No confundir las clases de Cobol con las clases de los lenguajes de programación orientados a objetos.

Las clases se definen en la sección `CONFIGURATION SECTION` dentro de `SPECIAL-NAMES`. A continuación se muestra un ejemplo de cómo se definen las clases y cómo se utilizan.

```cobol
CONFIGURATION SECTION.
SPECIAL-NAME.
    CLASS UPPER-CASE IS "A" THRU "Z".
```

Aquí estamos definiendo una clase llamada `IS-UPPER` que va a contener todas las letras del alfabeto en mayúscula. Esta clase retornará verdadero si la variable que estamos comparando contiene solamente letras en mayúscula entre la _A_ y la _Z_. 

Ahora, podemos utilizar esta clase para hacer una comparación en nuestro programa.

```cobol
IF var2 IS UPPER-CASE
```

Como esto podemos crear muchos tipos de clases, aquí algunos ejemplos:

```cobol
CLASS A-R IS "A" THRU "R", "a" THRU "r".
CLASS ESPACIO IS SPACE.
CLASS BINARIO IS "0" THRU "1".
CLASS NUMERICO IS "0" THRU "9".
CLASS ALFANUMERICO IS "A" THRU "Z", "a" THRU "z", "0" THRU "9".
```

<br>

### Datos del Cheat Sheet

\- Autor: Nicolás Villamonte <br>
\- Fecha: 30/01/2024 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
\- Herramientas y Versiones: Cobol, OpenCobolIde 4.7.6