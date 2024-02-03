# Ejecución Procedural en Cobol

La ejecución procedural es un concepto que yace de la programación procedimental, un paradigma de programación que se basa en la ejecución de instrucciones englobadas en un procedimiento y llamarlas cada vez que sea necesario. 

1. [Introducción a Perform](#introducción-a-perform)
2. [Secciones y Parrafos](#secciones-y-parrafos)
   1. [Secciones](#secciones)
   2. [Parrafos](#parrafos)
   3. [Llamado de Secciones y Parrafos](#llamado-de-secciones-y-parrafos)
3. [Ejemplo de Uso de PERFORM](#ejemplo-de-uso-de-perform)
   1. [Variables (Data Division)](#variables-data-division)
   2. [Programa (Procedure Division)](#programa-procedure-division)
   3. [Llamado de Secciones y Parrafos](#llamado-de-secciones-y-parrafos-1)
4. [THRU o THROUGH](#thru-o-through)
5. [Manejo del Flujo de Ejecución con PERFORM](#manejo-del-flujo-de-ejecución-con-perform)
   1. [TIMES](#times)
   2. [UNTIL, BEFORE y AFTER](#until-before-y-after)
      1. [UNTIL](#until)
      2. [BEFORE y AFTER](#before-y-after)
   3. [VARYING](#varying)
6. [Resumen](#resumen)

- [Datos del Cheat Sheet](#datos-del-cheat-sheet)


## Introducción a Perform

Cobol es un lenguaje muy antiguo, por lo que no se parece mucho a los lenguajes de programación modernos, pero se puede aplicar la ejecución procedural mediante la instrucción `PERFORM`, la cual nos permitirá modificar el flujo de ejecución del programa, realizar iteraciones, organizar mejor el código, encapsular funcionalidades, entre otras cosas.

Este comando tiene muchas formas de uso, las cuales vamos a ver poco a poco. Inicialmente, el comando PERFORM tiene la siguiente estructura básica:

```cobol
PERFORM nombre-de-la-sección
```

o

```cobol
PERFORM nombre-del-parrafo
```

## Secciones y Parrafos

Tanto las secciones como los parrafos pueden ser llamados por el comando PERFORM. Ambas mencionadas, dentro de la división `PROCEDURE DIVISION` son secciones y parrafos creados por el programador, por lo que podemos ponerles los nombres que queramos siempre y cuando respetemos las reglas de nombramiento de COBOL. Se utilizan para organizar el código y encapsular funcionalidades.

### Secciones

Las secciones son bloques de código que organizan el programa dentro de todas las divisiones. Se definen con el nombre de la sección seguida de la palabra `SECTION`.

```cobol
PROCEDURE DIVISION.
NOMBRE-DE-LA-SECCION SECTION.
...
```

El nombre de la sección debe ser único y no puede ser igual al nombre de un parrafo.

### Parrafos

A su vez, las secciones pueden contener parrafos, los cuales son bloques de código que encapsulan funcionalidades. Se definen solamente con el nombre del parrafo.

```cobol
PROCEDURE DIVISION.
NOMBRE-DE-LA-SECCION SECTION.
NOMBRE-DEL-PARRAFO.
    ...
```

### Llamado de Secciones y Parrafos

La instrucción `PERFORM` puede llamar tanto a secciones como a parrafos por su nombre. Si se llama a una sección, se ejecutará todo el código en orden hasta toparse con otra sección (donde termina la misma), incluyendo todos los parrafos que contenga. 

```cobol
PERFORM nombre-de-la-sección.
```

Si se llama a un parrafo, se ejecutará solamente el código contenido en el parrafo.

```cobol
PERFORM nombre-del-parrafo.
```

## Ejemplo de Uso de PERFORM

```cobol
...
DATA DIVISION.
WORKING-STORAGE SECTION.
    77 NOMBRE PIC A(20) VALUE SPACE.
    
PROCEDURE DIVISION.
SECTION-A SECTION.
MAIN-PROCEDURE.
    DISPLAY "Ingresa tu nombre: ".
    ACCEPT NOMBRE.
    PERFORM SALUDAR.
    PERFORM SECTION-B.
    STOP RUN.

SECTION-B SECTION.
DISPLAY "Estoy en la seccion B".
SALUDAR.
    DISPLAY "Hola " NOMBRE.
IMPRIMIROTRACOSA.
    DISPLAY "Termino la seccion B".
```

En este ejemplo, desglozaremos el programa en diferentes partes para entender su funcionamiento.

### Variables (Data Division)

```cobol
DATA DIVISION.
WORKING-STORAGE SECTION.
    77 NOMBRE PIC A(20) VALUE SPACE.
```

Se define una variable llamada `NOMBRE` de tipo alfabética con un tamaño de 20 caracteres y se le asigna el valor de espacios a cada uno de éstos caracteres. En esta variable se almacenará el nombre que el usuario ingrese.

### Programa (Procedure Division)

Aquí se define el programa principal, el cual tiene, en este caso, dos secciones diferentes:

Sección A:
```cobol
SECTION-A SECTION.
MAIN-PROCEDURE.
    DISPLAY "Ingresa tu nombre: ".
    ACCEPT NOMBRE.
    PERFORM SALUDAR.
    PERFORM SECTION-B.
    STOP RUN.
```

Sección B:
```cobol
SECTION-B SECTION.
DISPLAY "Estoy en la seccion B".
SALUDAR.
    DISPLAY "Hola " NOMBRE.
IMPRIMIROTRACOSA.
    DISPLAY "Termino la seccion B".
```

Como vemos, las secciones finalizan cuando comienza otra sección. En este caso, la sección A contiene el parrafo `MAIN-PROCEDURE` y la sección B contiene los parrafos `SALUDAR` e `IMPRIMIROTRACOSA`.

### Llamado de Secciones y Parrafos

En la sección A, se llama al parrafo `SALUDAR` y, luego, a la sección B. El parrafo `IMPRIMIROTRACOSA` da final al parrafo `SALUDAR`, por lo tanto, cuando se llama al mismo con `PERFORM`, se ejecutará solamente el código contenido en el parrafo (Imprimir "Hola " NOMBRE) y volverá a la sección A para continuar con el flujo de ejecución donde lo había dejado.

En pocas palabras, cuando se ejecuta el siguiente código:

```cobol
PERFORM SALUDAR.
```

Solamente se ejecutaría el código contenido en el parrafo `SALUDAR`:
```cobol
SALUDAR.
    DISPLAY "Hola " NOMBRE.
```

Y luego volvería a donde se llamó al parrafo `SALUDAR` para continuar con el flujo de ejecución. En este caso, seguiría con la siguiente línea de código:

```cobol
PERFORM SECTION-B.
``` 

Esta vez se está llamando a una sección completa, llamada SECTION-B, por lo que se ejecutará todo el código contenido en la sección B, sin importar la división de parrafos que contenga en su interior. En este caso, se ejecutaría el siguiente código:

```cobol
DISPLAY "Estoy en la seccion B".
SALUDAR.
    DISPLAY "Hola " NOMBRE.
IMPRIMIROTRACOSA.
    DISPLAY "Termino la seccion B".
```

Lo que realmente saldría en pantalla sería lo siguiente:

```cobol
Estoy en la seccion B
Hola [NOMBRE]
Termino la seccion B
```

## THRU o THROUGH

El comando `THRU` o `THROUGH` se utiliza para llamar a un conjunto de parrafos o secciones en orden. Por ejemplo, si tenemos el siguiente codigo.

```cobol
PERFORM PARRAFO-B THRU PARRAFO-D.

PARRAFO-A.
    DISPLAY "Estoy en el parrafo A".
PARRAFO-B.
    DISPLAY "Estoy en el parrafo B".
PARRAFO-C.
    DISPLAY "Estoy en el parrafo C".
PARRAFO-D.
    DISPLAY "Estoy en el parrafo D".
PARRAFO-E.
    DISPLAY "Estoy en el parrafo E".
```

Entonces el primer comando `PERFORM` ejecutará los parrafos B, C y D en orden, por lo que el resultado en pantalla sería el siguiente:

```cobol
Estoy en el parrafo B
Estoy en el parrafo C
Estoy en el parrafo D
```

## Manejo del Flujo de Ejecución con PERFORM

Las iteraciones en Cobol son comunmente manejadas con el comando `PERFORM`. A continuación, se presentan algunas de las formas de manejar el flujo de ejecución con `PERFORM`.

### TIMES

El comando `TIMES` se utiliza para ejecutar un conjunto de parrafos o secciones un número determinado de veces. Por ejemplo, si tenemos el siguiente código.

```cobol
MAIN-PROCEDURE.
    PERFORM SALUDAR 6 TIMES.
    STOP RUN.

SALUDAR.
    DISPLAY "HOLA".
```

Entonces se va a llamar al parrafo `SALUDAR` 6 veces y luego finalizará la ejecución del programa, por lo que el resultado en pantalla sería el siguiente:

```cobol
HOLA
HOLA
HOLA
HOLA
HOLA
HOLA
```

### UNTIL, BEFORE y AFTER

Los comandos `UNTIL`, `BEFORE` y `AFTER` se utilizan para ejecutar un conjunto de parrafos o secciones hasta que se cumpla una condición.

#### UNTIL

```cobol
PERFORM SALUDAR UNTIL NOMBRE = "FIN".
```

Siguiendo con el ejemplo en el que se ejecuta el parrafo `SALUDAR`, en este caso se volverá a ejecutar este parrafo constantemente hasta que la variable `NOMBRE` sea igual a "FIN". Esto lo podemos hacer con un párrafo `SALUDAR` como el siguiente:

```cobol
SALUDAR.
    DISPLAY "Escriba su nombre:".
    ACCEPT NOMBRE.
    DISPLAY "HOLA" NOMBRE.
```

Cuando el usuario ingrese "FIN" en vez de su nombre, el programa dejará de ejecutar el parrafo `SALUDAR` y continuará con el flujo de ejecución desde donde se comenzo a llamar al mismo.

#### BEFORE y AFTER

Los agregados `BEFORE` y `AFTER` se utilizan para ejecutar un conjunto de parrafos o secciones antes o después de que se cumpla una condición. Esto se hace con el prefijo `WITH TEST` para su correcto funcionamiento.

```cobol
PERFORM SALUDAR WITH TEST BEFORE UNTIL CONTADOR = 10.
PERFORM SALUDAR WITH TEST AFTER UNTIL CONTADOR = 10.
```

En el primer caso, se ejecutará el parrafo `SALUDAR` antes de que la variable `CONTADOR` sea igual a 10. En el segundo caso, se ejecutará el parrafo `SALUDAR` después de que la variable `CONTADOR` sea igual a 10.

En este caso concreto, el parrafo `SALUDAR` debería cambiar el valor de la variable `CONTADOR` para que el flujo de ejecución cambie.

```cobol
SALUDAR.
    DISPLAY "HOLA".
    ADD 1 TO CONTADOR.
```

### VARYING

El comando `VARYING` se utiliza para ejecutar un conjunto de parrafos o secciones un número determinado de veces, variando el valor de una variable en cada iteración y pudiendo configurar los pasos que se darán en cada iteración.

```cobol
PERFORM MOSTRAR-NUM VARYING NUM FROM 15 BY 2 UNTIL NUM > 30.
```

En este caso, se ejecutará el parrafo `MOSTRAR-NUM` 8 veces, variando el valor de la variable `NUM` desde 15 hasta 31, de 2 en 2. El parrafo `MOSTRAR-NUM` debería ser algo como lo siguiente:

```cobol
MOSTRAR-NUM.
    DISPLAY NUM.
```

Y la salida en pantalla sería la siguiente:

```cobol
15
17
19
21
23
25
27
29
```

## Resumen

```cobol
PERFORM nombre-de-la-sección. *> Llama a una sección completa.
PERFORM nombre-del-parrafo. *> Llama a un parrafo.

PERFORM PARRAFO-B THRU PARRAFO-D. *> Llama a un conjunto de parrafos en orden.

PERFORM SALUDAR 6 TIMES. *> Llama a un conjunto de parrafos un número determinado de veces.

PERFORM SALUDAR UNTIL NOMBRE = "FIN". *> Llama a un conjunto de parrafos hasta que se cumpla una condición.
PERFORM SALUDAR WITH TEST BEFORE UNTIL CONTADOR = 10. *> Llama a un conjunto de parrafos antes de que se cumpla una condición.
PERFORM SALUDAR WITH TEST AFTER UNTIL CONTADOR = 10. *> Llama a un conjunto de parrafos después de que se cumpla una condición.

PERFORM MOSTRAR-NUM VARYING NUM FROM 15 BY 2 UNTIL NUM > 30. *> Llama a un conjunto de parrafos un número determinado de veces, variando el valor de una variable en cada iteración y pudiendo configurar los pasos que se darán en cada iteración.
```

<br>

### Datos del Cheat Sheet

\- Autor: Nicolás Villamonte <br>
\- Fecha: 03/02/2024 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
\- Herramientas y Versiones: Cobol, OpenCobolIde 4.7.6