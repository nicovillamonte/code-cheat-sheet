# Estructuras Condicionales en Cobol

Las estructuras condicionales son aquellas que nos permiten tomar decisiones en un programa. Estas estructuras se basan en la evaluación de una condición, si esta es verdadera se ejecuta un bloque de código, si es falsa se ejecuta otro bloque de código.

1. [IF](#if)
   1. [ELSE](#else)
   2. [ELSE IF](#else-if)
   3. [Tipos de Condiciones](#tipos-de-condiciones)
      1. [Condiciones de Relación o Comparación](#condiciones-de-relación-o-comparación)
      2. [Condiciones de Signo](#condiciones-de-signo)
      3. [Condiciones de Clase](#condiciones-de-clase)
         1. [Creación de Clases Personalizadas](#creación-de-clases-personalizadas)
      4. [Condiciones Compuestas o Complejas](#condiciones-compuestas-o-complejas)
      5. [Nombres de Condición o Condiciones Nombradas](#nombres-de-condición-o-condiciones-nombradas)
2. [EVALUATE](#evaluate)
   1. [Evaluate TRUE](#evaluate-true)

- [Datos del Cheat Sheet](#datos-del-cheat-sheet)

## IF

Al igual que en la mayoría de los lenguajes de programación en la actualidad, en Cobol se utiliza la palabra reservada `IF` para evaluar una condición. La sintaxis es la siguiente:

```cobol
IF {condicion}... [THEN]
    {instrucciones}...
[END-IF | .]
```

Cabe aclarar que para terminar la instrucción `IF` se puede utilizar la palabra reservada `END-IF` o el punto `.` en la última instrucción que se ejecutará si la condición es verdadera. 

Los dos siguientes códigos son equivalentes:
```cobol
IF NUM = 0 THEN
    DISPLAY "El numero es cero. Ingrese el nuevo num:"
    ACCEPT NUM
    DISPLAY "El nuevo número es: " NUM
END-IF
```

> Recordar que las tabulaciones son opcionales y no son las que definen el bloque de código, sino que se definen por el `.` o el `END-IF`.

```cobol
IF NUM = 0
    DISPLAY "El numero es cero. Ingrese el nuevo num:"
    ACCEPT NUM
    DISPLAY "El nuevo número es: " NUM.
```

> El `THEN` es opcional, por eso no se encuentra en el código mostrado, pero se recomienda su uso para hacer el código más legible.

### ELSE

En caso de que la condición no se cumpla, se puede utilizar la palabra reservada `ELSE` para ejecutar otro bloque de código. La sintaxis es la siguiente:

```cobol
IF {condicion}... THEN
    {instrucciones}...
ELSE
    {instrucciones}...
[END-IF | .]
```

El siguiente código muestra un ejemplo de uso de `ELSE`:
```cobol
IF NUM > 0 THEN
    DISPLAY "El numero es positivo."
ELSE
    DISPLAY "El numero es negativo."
END-IF
```

### ELSE IF

En COBOL no existe la palabra reservada `ELSE IF`, pero se puede simular su uso utilizando la palabra reservada `ELSE` seguida de un `IF`. La sintaxis es la siguiente:

```cobol
IF {condicion}... THEN
    {instrucciones}...
ELSE
    IF {condicion}... THEN
        {instrucciones}...
    ELSE
        {instrucciones}...
    END-IF
END-IF
```

Aquí es muy importante tener en cuenta que el `IF` que se encuentra dentro del `ELSE` es otro IF anidado y no un `ELSE IF`. Aunque se puede simular un `ELSE IF` de la siguiente manera:

```cobol
IF NUM > 0 THEN
    DISPLAY "El numero es positivo."
ELSE IF NUM = 0 THEN
    DISPLAY "El numero es cero."
ELSE
    DISPLAY "El numero es negativo.".
```


Fíjese que, aunque ésta es otra manera de escribir `IF` anidados, este condicional completo se termina solamente con un punto y no necesita de dos `END-IF` como debería ser el caso. Por lo que este es el comportamiento más parecido a un `ELSE IF` que se puede lograr en COBOL.

Si se quiere ejecutar una instrucción dentro de la primera condición pero fuera de la segunda anidada es posible hacerlo utilizando solamente `END-IF` y no `.` en el condicional que se encuentra dentro del otro. La siguiente es una manera de hacerlo:

```cobol
IF NUM > 0 THEN
    DISPLAY "El numero es positivo."
ELSE IF NUM = 0 THEN
    DISPLAY "El numero es cero."
ELSE
    DISPLAY "El numero es negativo."
END-IF
DISPLAY "Ingrese un nuevo número:"
ACCEPT NUM.
```

> Esta no es una buena práctica de programación, ya que puede llevar a confusión, pero es bueno entender cómo funciona.

Las dos últimas líneas de código parecen estar fuera del condicional, pero en realidad están dentro del `ELSE` del último `IF` anidado. No se recomienda hacer esto, ya que puede llevar a confusión, pero para entenderlo más sería exactamente lo mismo que escribirlo de la siguiente manera:

```cobol
IF NUM > 0 THEN
    DISPLAY "El numero es positivo."
ELSE 
    IF NUM = 0 THEN
        DISPLAY "El numero es cero."
    ELSE
        DISPLAY "El numero es negativo."
    END-IF
    DISPLAY "Ingrese un nuevo número:"
    ACCEPT NUM
END-IF
```

En este caso, se ve de manera más clara que las dos últimas líneas de código están dentro del `ELSE` del último `IF` anidado, es decir que no se van a ejecutar nunca si el _NUM_ es mayor a 0.


## Tipos de Condiciones

Existen diferentes tipos de condiciones en COBOL, las cuales se pueden utilizar en las estructuras condicionales. A continuación, se muestran los tipos de condiciones más comunes:

- Condiciones de **Relación** o **Comparación**
- Condiciones de **Signo**
- Condiciones de **Clase**
- Condiciones **Compuestas** o **Complejas**
- **Nombres de Condición** o Condiciones **Nombradas**

### Condiciones de Relación o Comparación

Las condiciones de relación o comparación son aquellas que se utilizan para comparar dos valores.

```cobol
IF NUM1 = NUM2 THEN
    DISPLAY "Los números son iguales."
END-IF
```

```cobol
IF NUM1 IS GREATER THAN NUM2 THEN
    DISPLAY "El primer número es mayor que el segundo."
ELSE
    DISPLAY "El primer número es menor que el segundo."
END-IF
```

Estas condiciones se encuentran descritas más a profundidad [aquí](./Operaciones.md/#operaciones-condicionales).

### Condiciones de Signo

Las condiciones de signo son aquellas que se utilizan para evaluar el signo de un número. Existen muchas predefinidas en COBOL, de las más comunes:

- `POSITIVE`
- `NEGATIVE`
- `ZERO`

```cobol
IF NUM IS POSITIVE THEN
    DISPLAY "El número es positivo.".
```

```cobol
IF NUM IS NEGATIVE THEN
    DISPLAY "El número es positivo.".
```

Esto se puede combinar con las [condiciones compuestas](#condiciones-compuestas-o-complejas) para hacer condiciones más complejas.

### Condiciones de Clase

Las condiciones de clase son aquellas que se utilizan para evaluar la clase de un número, como por ejemplo, evaluar el tipo de dato de una variable.

```cobol
IF NUM IS NUMERIC THEN
    DISPLAY "El número es numérico.".
```

Existen muchas clases ya definidas en COBOL, como:

- `NUMERIC`: Evalúa si el valor de la variable es numéricom sin importar su tipo de dato, puede ser una variable con PICTURE "X(3)", pero si en su valor tiene "345" entonces es numérico.
- `ALPHABETIC`: Evalúa si el valor de la variable es alfabético, es decir, si es una cadena solamente de caracteres alfabéticos.
- `ALPHANUMERIC`: Evalúa si el valor de la variable es alfanumérico, es decir, si es una cadena de caracteres alfabéticos y numéricos.
- `ALPHABETIC-LOWER`: Evalúa si el valor de la variable es alfabético todo en minúsculas.
- `ALPHABETIC-UPPER`: Evalúa si el valor de la variable es alfabético todo en mayúsculas.

#### Creación de Clases Personalizadas

En COBOL, es posible crear clases personalizadas con la condición que se necesite utilizar en el programa. Estas clases deben definirse en la sección `ENVIRONMENT DIVISION`, dentro de la sección `CONFIGURATION SECTION` y su parrafo `SPECIAL-NAMES`. La sintaxis es la siguiente:

```cobol
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SPECIAL-NAMES.
    CLASS {nombre-condicion} IS {condicion},...
```

Los puntos suspensivos `...` indican que se pueden definir más de una condición en la misma clase separandola, en este caso, por comas. Podemos ejemplificar esto haciendo una clase que evalúe si una variable contiene un valor hexadecimal:

```cobol
CLASS HEXADECIMAL IS "0" THRU "9", "A" THRU "F", "a" THRU "f".
```

En este caso los valores alfabéticos si se diferencian entre mayúsculas y minúsculas, por eso se definen los valores de la `A` a la `F` y de la `a` a la `f`.

Ahora podemos utilzar esta clase en una condición:

```cobol
IF NUM IS HEXADECIMAL THEN
    DISPLAY "El número es hexadecimal.".
```

### Condiciones Compuestas o Complejas

Las condiciones compuestas o complejas son aquellas que se utilizan para combinar dos o más condiciones en una sola. Estas condiciones se pueden combinar utilizando las palabras reservadas `AND`, `OR` y `NOT`.

```cobol
IF NUM1 > 0 AND NUM2 > 0 THEN
    DISPLAY "Ambos números son positivos.".
```

```cobol
IF NUM IS NOT NUMERIC OR NUM NOT >= 0 THEN
    DISPLAY "El número no es numérico o es cero.".
```

También se pueden utilizar paréntesis para hacer más legible el código y para definir el orden de evaluación de las condiciones.

### Nombres de Condición o Condiciones Nombradas

Las condiciones nombradas son aquellas que, en COBOL, se refieren a las variables definidas en el nivel 88. Son condiciones que se le dan a una variable al crearla y que van cambiando si son verdaderas o falsas según el valor de la variable a la que están asociadas.

```cobol
77 PREGUNTA PIC X(50) VALUE "¿Desea continuar? (S/N)".
77 RESPUESTA PIC A.
    88 RESPUESTA-POSITIVA VALUE "S", "s".
    88 RESPUESTA-NEGATIVA VALUE "N", "n".
77 EDAD PIC 9(3).
    88 MENOR-DE-18 VALUE 1 THRU 17.
    88 MAYOR-DE-18 VALUE 18 THRU 999.
```

En este caso, las variables `RESPUESTA-POSITIVA` y `RESPUESTA-NEGATIVA` son condiciones nombradas que se asocian a la variable `RESPUESTA`. Si el valor de `RESPUESTA` es `S` o `s`, entonces `RESPUESTA-POSITIVA` es verdadera, si no, es falsa. Lo mismo pasa con `RESPUESTA-NEGATIVA` si el valor de `RESPUESTA` es `N` o `n`.

```cobol
IF RESPUESTA-POSITIVA THEN
    DISPLAY "Continuando..."
ELSE
    DISPLAY "Saliendo..."
    STOP RUN
END-IF
```

En el caso de `EDAD`, las condiciones nombradas `MENOR-DE-18` y `MAYOR-DE-18` se asocian a la variable `EDAD`. Si el valor de `EDAD` es un número entre 1 y 17, entonces `MENOR-DE-18` es verdadera, si no, es falsa. Lo mismo pasa con `MAYOR-DE-18` si el valor de `EDAD` es un número mayor o igual a 18.

```cobol
DISPLAY "Ingrese su edad:"
ACCEPT EDAD

IF MENOR-DE-18 THEN
    DISPLAY "Usted es menor de edad, no puede continuar."
    STOP RUN.
```

## EVALUATE

El `EVALUATE` es una estructura condicional que se utiliza para evaluar una variable y ejecutar un bloque de código dependiendo del valor de la variable. Es lo más parecido a un `switch` en otros lenguajes de programación.

```cobol
EVALUATE {variable}
    WHEN {valor | rango}...
        {instrucciones}...
    WHEN {valor | rango}...
        {instrucciones}...
    WHEN OTHER
        {instrucciones}...
END-EVALUATE
```

El `EVALUATE` evalúa la variable y ejecuta el bloque de código que se encuentra debajo de la condición que se cumpla. Si ninguna condición se cumple, se ejecuta el bloque de código que se encuentra debajo de `WHEN OTHER`, el cual es opcional.

```cobol
EVALUATE EDAD
    WHEN 18
        DISPLAY "Usted acaba de cumplir la mayoría de edad."
    WHEN 19 THRU 75
        DISPLAY "Usted es mayor de edad."
    WHEN 75 THRU 999
        DISPLAY "Usted es anciano."
    WHEN OTHER
        DISPLAY "Usted es menor de edad."
```

### Evaluate TRUE

El `EVALUATE TRUE` sirve para evaluar las variables de nivel 88. La sintaxis es la siguiente:

```cobol
EVALUATE TRUE
    WHEN {condicion-nombrada}
        {instrucciones}...
    WHEN {condicion-nombrada}
        {instrucciones}...
    WHEN OTHER
        {instrucciones}...
END-EVALUATE
```

Por ejemplo, si se tiene el siguiente código:

```cobol
01 USUARIO.
    05 USUARIO-NOMBRE PIC X(50).
    05 USUARIO-EDAD PIC 9(3).
        88 USUARIO-MENOR-DE-18 VALUE 1 THRU 17.
        88 USUARIO-MAYOR-DE-18 VALUE 18 THRU 150.

EVALUATE TRUE
    WHEN USUARIO-MENOR-DE-18
        DISPLAY "Continuando..."
    WHEN USUARIO-MAYOR-DE-18
        DISPLAY "Saliendo..."
        STOP RUN
    WHEN OTHER
        DISPLAY "Usted probablemente no se encuentre vivo."
END-EVALUATE
```

el `EVALUATE TRUE` evaluará la variable `TRUE` y ejecutará el bloque de código que se encuentra debajo de la condición que se cumpla. Si ninguna condición se cumple, se ejecuta el bloque de código que se encuentra debajo de `WHEN OTHER`, el cual es opcional.

> Se ejecutará el primer bloque de código que se cumpla, por lo que si se cumple la condición `USUARIO-MENOR-DE-18`, se ejecutará ese bloque de código y no se evaluarán las demás condiciones.


<br>

### Datos del Cheat Sheet

\- Autor: Nicolás Villamonte <br>
\- Fecha: 03/02/2024 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
\- Herramientas y Versiones: Cobol, OpenCobolIde 4.7.6