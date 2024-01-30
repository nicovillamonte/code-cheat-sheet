# Variables en Cobol

Las variables en Cobol se llaman "Elementary Item" y se asignan en memoria de forma estática, es decir, se asigna un espacio de memoria para cada variable y este espacio no cambia durante la ejecución del programa.

Las mismas se encontrarán dentro de la división de datos, en la sección de _WORKING-STORAGE_.

```cobol
DATA DIVISION.
WORKING-STORAGE SECTION.
    ...
```

1. [Declaración de variables](#declaración-de-variables-pictures)
    1. [Variables numéricas](#variables-numéricas)
        1. [Variables numéricas con decimales](#variables-numéricas-con-decimales)
        2. [Variables numéricas con signo](#variables-numéricas-con-signo)
    2. [Variables alfabéticas](#variables-alfabéticas)
    3. [Variables alfanuméricas](#variables-alfanuméricas)
    4. [Variables Constantes](#variables-constantes)
2. [Inicialización de variables](#inicialización-de-variables)
3. [Niveles de Ejecución de las variables](#niveles-de-ejecución-de-las-variables)
    1. [Variables compuestas](#variables-compuestas)
    2. [Alias de variables (Nivel 66)](#alias-de-variables-nivel-66)
    3. [Variables Condicionales (Nivel 88)](#variables-condicionales-nivel-88)
4. [Resumen](#resumen)

- [Datos del Cheat Sheet](#datos-del-cheat-sheet)

## Declaración de variables (Pictures)

El tipo de variable se asigna con el PIC (Picture). El cual se usa normalmente para indicar el tipo de variable y la cantidad de bytes que va a ocupar en memoria.

### Variables numéricas

Las variables numéricas se declaran con símbolos 9:

```cobol
01 NOMBRE-VARIABLE PIC 999.
```

Esta variable tendrá un tamaño de 3 bytes y podrá almacenar números entre 0 y 999.

Si queremos hacerlo de forma más sencilla, podemos declarar la cantidad de dígitos que queremos que tenga la variable:

```cobol
01 NOMBRE-VARIABLE PIC 9(3).
```

#### Variables numéricas con decimales

Para indicar en el PIC que una variable será numérica y tendrá decimales, se debe agregar una coma V en donde se quiera que esté la coma y la cantidad de decimales que se quieran tener:

```cobol
01 NOMBRE-VARIABLE PIC 99V99 VALUE 10.34.
```

Lo que también se puede escribir así:

```cobol
01 NOMBRE-VARIABLE PIC 9(2)V9(2) VALUE 10.34.
```

#### Variables numéricas con signo

Para indicar en el PIC que una variable será numérica y tendrá signo, se debe agregar una S al principio del PIC:

```cobol
01 NOMBRE-VARIABLE PIC S9(3) VALUE 837.
```

### Variables alfabéticas

Las variables alfabéticas solamente pueden obtener letras, no aceptan números ni símbolos. Se declaran con la letra A:

```cobol
01 NOMBRE-VARIABLE PIC AAA.
```

Esta variable tendrá un tamaño de 3 bytes y podrá almacenar cualquier caracter en formato string.

Si queremos hacerlo de forma más sencilla, podemos declarar la cantidad de caracteres que queremos que tenga la variable:

```cobol
01 NOMBRE-VARIABLE PIC A(3).
```

### Variables alfanuméricas

Las variables alfanuméricas se declaran con símbolos X:

```cobol
01 NOMBRE-VARIABLE PIC XXX.
```

Esta variable tendrá un tamaño de 3 bytes y podrá almacenar cualquier caracter en formato string.

Si queremos hacerlo de forma más sencilla, podemos declarar la cantidad de caracteres que queremos que tenga la variable:

```cobol
01 NOMBRE-VARIABLE PIC X(3).
```

### Variables Constantes

Las variables constantes se declaran con la palabra clave _CONSTANT_ y se le asigna un valor con la palabra clave _AS_:

```cobol
01 NOMBRE-VARIABLE CONSTANT AS 837.
```

> IMPORTANTE: Las variables constantes pueden dar problemas en versiones anteriores del lenguaje Cobol. Si se quiere evitar esto no utilizarlas y poner los valores constantes en el código directamente.

## Inicialización de variables

Para inicializar una variable, se debe agregar la cláusula _VALUE_ al final de la declaración de la variable, y asignarle el valor que queremos que tenga

```cobol
01 NOMBRE-VARIABLE PIC 9(3) VALUE 837.
```

Existen palabras claves que nos permiten inicializar variables de forma más sencilla:

```cobol
01 NOMBRE-VARIABLE PIC 9(3) VALUE ZERO.
01 VARIABLE-TEXTO  PIC 9(3) VALUE SPACES.
```

Sino, también podemos inicializar variables con valores de otras variables:

```cobol
01 NOMBRE-VARIABLE PIC 9(3) VALUE VARIABLE-OTRA.
```

A parte, podemos dejar las variables sin un valor inicial y darselos en el programa con la cláusula _MOVE_:

```cobol
01 NOMBRE-VARIABLE PIC 9(3).
...
MOVE 837 TO NOMBRE-VARIABLE.
```

## Niveles de Ejecución de las variables

Las variables se pueden declarar con diferentes niveles, que indican la jerarquía de las mismas. Los niveles de **propósito general** van desde el 01 hasta el 49. Además, existen los de **propósito especial** que son el 66, 77 y 88.

Los niveles más importantes que hay que recordar son los siguientes:

- **01 al 49**: Niveles de propósito general. Se utilizan para declarar variables que se van a utilizar en el programa.
  - **01**: Nivel de variable. Se puede utilizar para declarar las variables simples y se utiliza siempre para declarar variables compuestas (Grupos).
  - **02 al 49**: Nivel de variable. Se utiliza para declarar las subvariables en una variable compuesta.
- **66**: Nivel especial. Declaraciones de RENAMES. Se utiliza para darle un nombre alternativo a una variable, un alias.
- **77**: Nivel especial. Esta expresamente creado para variables individuales. Es mejor utilizar este que el 01 cuando se trata de este tipo de variables. No se puede subdividir.
- **88**: Nivel especial. Diseñado para crear posibles valores o rangos que se van a almacenar en una variable dependiendo de la condición.

Las variables tienen dos categorías escenciales:

- **Elementary Items** o **Variables simples**: Son variables que no se pueden dividir en partes más pequeñas. Solo se pueden usar para estas variables el nivel 01 o el 77.
- **Group Items** o **Variables compuestas**: Son variables que se pueden dividir en partes más pequeñas.

### Variables compuestas

Las variables compuestas se declaran con el nivel 01 y se pueden dividir en partes más pequeñas. Estas partes más pequeñas pueden ser variables simples o variables compuestas.

```cobol
01 NOMBRE-VARIABLE.
    05 NOMBRE-VARIABLE-1 PIC 9(3).
    05 NOMBRE-VARIABLE-2.
        10 NOMBRE-VARIABLE-2-1 PIC 9(3).
        10 NOMBRE-VARIABLE-2-2 PIC 9(3).
    05 NOMBRE-VARIABLE-3 PIC 9(3).
```

Como ejemplo podemos construir la variable compuesta que nos de la información de una persona:

```cobol
01 PERSONA.
    05 NOMBRE PIC X(20).
    05 APELLIDO PIC X(20).
    05 EDAD PIC 9(3).
    05 DIRECCION.
        10 CALLE PIC X(20).
        10 NUMERO PIC 9(4).
        10 PISO PIC 9(3).
        10 DEPARTAMENTO PIC X(3).
    05 TELEFONO PIC 9(10).
```

### Alias de variables (Nivel 66)

Los alias de variables se declaran con el nivel 66 y se utilizan para darle un nombre alternativo a una variable. Se puede realizar utilizando la palabra clave _RENAMES_ para asignarle la misma ubicación de memoria.

```cobol
01 DETALLES-EMPLEADO.
   05 NOMBRE            PIC X(30).
   05 FECHA-NACIMIENTO.
       10 ANIO          PIC 9(4).
       10 MES           PIC 9(2).
       10 DIA           PIC 9(2).
   05 DIRECCION         PIC X(50).
   66 FECHA-COMPLETA    RENAMES FECHA-NACIMIENTO.
```

En este caso, la variable FECHA-COMPLETA va a tener la misma ubicación de memoria que FECHA-NACIMIENTO, por lo que si se modifica una, se modifica la otra.

### Variables Condicionales (Nivel 88)

Son variables que se utilizan para crear posibles valores o rangos que se van a almacenar en una variable dependiendo de la condición.

```cobol
77 EDAD PIC 9(3).
    88 MENOR VALUE 0 THRU 17.
    88 ADULTO VALUE 18 THRU 64.
    88 MAYOR VALUE 65 THRU 150.
```

Esto se puede utilizar para hacer validaciones de datos:

```cobol
IF MENOR
    DISPLAY "Es menor de edad"
END-IF
```

Tambi

# Resumen

```cobol
   *> Variables numéricas.
    77 VARIABLE-NUMERICA PIC 9(5) VALUE 12345.
    77 NOMBRE-VARIABLE PICTURE 9(2)V9(2) VALUE 10.34
    77 NOMBRE-VARIABLE PICTRUE IS S9(3) VALUE 837.
   
   *> Variables alfabéticas y alfanuméricas.
    77 VARIABLE-ALFABETICA PIC A(10) VALUE "Hola Mundo".
    77 VARIABLE-ALFANUMERICA PIC X(12) VALUE "Hola Mundo2!".

   *> Variables constantes.
    77 VARIABLE-CONSTANTE CONSTANT AS 837.

   *> Variables compuestas.
    01 VARIABLE-COMPUESTA.
        05 VARIABLE-COMPUESTA-1 PIC 9(3).
        05 VARIABLE-COMPUESTA-2.
            10 VARIABLE-COMPUESTA-2-1 PIC 9(3).
            10 VARIABLE-COMPUESTA-2-2 PIC 9(3).
        05 VARIABLE-COMPUESTA-3 PIC 9(3).
    
   *> Nivel 88. Variables Condicionales.
    77 VARIABLE-CONDICIONAL PIC 9(3).
        88 MENOR VALUE 0 THRU 17.
        88 ADULTO VALUE 18 THRU 64.
        88 MAYOR VALUE 65 THRU 150.

   *> Nivel 66. Alias de variables.
    77 VARIABLE-ALIAS PIC 9(3).
    66 VARIABLE-ALIAS-2 RENAMES VARIABLE-ALIAS.
```

<br>

### Datos del Cheat Sheet

\- Autor: Nicolás Villamonte <br>
\- Fecha: 29/01/2024 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
\- Herramientas y Versiones: Cobol, OpenCobolIde 4.7.6