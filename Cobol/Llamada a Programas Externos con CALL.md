# Llamadas a Programas Externos con CALL en COBOL

A la hora de hacer proyectos muy grandes en COBOL, es común que se necesite dividir el programa en varios programas más pequeños, además de poder reutilizar código que se haya escrito para otros programas en nuevos programas. Al realizar esta modularización, el programa se veuelve más fácil tanto de entender como de mantener a lo largo del tiempo.

Por eso, las llamadas a otros programas en COBOL son una herramienta muy útil que permite subir de nivel en la calidad de los programas que se escriban.

1. [Programa Principal vs Programa Subordinado](#programa-principal-vs-programa-subordinado)
2. [Llamada a un Programa Subordinado](#llamada-a-un-programa-subordinado)
    1. [Parámetros](#parámetros)
        - [Parámetros por Referencia](#parámetros-por-referencia)
        - [Parámetros por Valor](#parámetros-por-valor)
        - [Parámetros Cominados](#parámetros-combinados)
    2. [Retorno de Valores](#retorno-de-valores)
3. [Manejo de Errores](#manejo-de-errores)

- [Datos del Cheat Sheet](#datos-del-cheat-sheet)


## Programa Principal vs Programa Subordinado

En todos los lenguajes suele existir un punto en en el que el programa comienza a ejecutarse. En COBOL, este punto es el **programa principal**. El programa principal es el que se ejecuta cuando se inicia el mismo. A partir de este programa, se pueden llamar a otros programas que se denominan **programas subordinados** o **subprogramas**.

Para poder llamar a un programa subordinado desde el programa principal, hay que tener muchas cosas en cuenta para que la llamada se realice de manera correcta.

- El programa principal finaliza su ejecución con la clausula `STOP RUN`.
- El programa subordinado finaliza su ejecución con la clausula `EXIT PROGRAM`.
- Existen dos tipos de variables en este contexto ya que no se utilizan las mismas variables en el programa principal que en el programa subordinado.
  - **Actuales**: Son las variables que se utilizan en el programa principal y que se pasan al programa subordinado.
  - **Formales**: Son las variables que se utilizan en el programa subordinado, que se reciben del programa principal.
- Las variables actuales y formales deben tener el mismo PICTURE y el mismo tamaño en memoria.
- El orden en el que los parámetros se pasan al programa subordinado es importante.
- Se pueden pasar parámetros por referencia o por valor.

## Llamada a un Programa Subordinado

Para llamar a un programa subordinado desde el programa principal, se utiliza la clausula `CALL`. La sintaxis de la clausula `CALL` es la siguiente:

```cobol
CALL nombre-del-programa-subordinado.
```

En este caso, estaríamos llamando a un programa subordindo cuyo nombre es `nombre-del-programa-subordinado.dll` o`nombre-del-programa-subordinado.cbl`. Si el archivo `.dll` no existe, entonces se busca el archivo `.cbl` para crear el primero y luego ejecutarlo.

Por ejemplo, en este programa subordinado llamado `hola-mundo.cbl`:

```cobol
MAIN-PROCEDURE.
    DISPLAY "Hola Mundo"
    EXIT PROGRAM.
```

Se puede llamar desde el programa principal de la siguiente manera:

```cobol
MAIN-PROCEDURE.
    CALL "hola-mundo".
    STOP RUN.
```

Este código imprimirá en paranalla el mensaje `Hola Mundo`, ya que el programa subordinado se ejecutará al llamarlo desde el programa principal.

> Se puede generar de manera manual el archivo `.dll` utilizando el IDE de COBOL.

### Parámetros

Si se quiere pasar parámetros al programa subordinado, se puede hacer de la siguiente manera:

```cobol
CALL "suma" USING parametro1 parametro2.
```

Los parametros pueden ser variables o valores literales del programa principal. En el programa subordinado, se reciben los parámetros de la siguiente manera:

```cobol
DATA DIVISION.
LINKAGE SECTION.
    01  num1 PIC 9(2).
    01  num2 PIC 9(2).

MAIN-PROCEDURE USING parametro1 parametro2.
    ADD X TO Y.
    EXIT PROGRAM.
```

En este caso, el programa subordinado recibe desde el programa principal los parámetros `parametro1` y `parametro2` y los almacena en las variables `num1` y `num2` respectivamente. Luego, se realiza la suma de ambas variables, la misma se guarda en la variable `num1`, la cual referencia a `parametro1` en el programa principal, por lo que si se quiere se puede imprimir el valor de `parametro1` y se verá que el valor ha cambiado.

Como se ve, los parametros de los programas deben declararse en la sección **LINKAGE SECTION** de la `DATA DIVISION` del programa subordinado.

#### Parámetros por Referencia

Los parametros se pueden pasar por referencia, esta es la manera por defecto en la que se pasan los mismos. Esto significa que si se modifica el valor de un parámetro en el programa subordinado, el valor del parámetro en el programa principal también se modificará, como pasaba en el [ejemplo anterior](#parámetros). Para ello se utiliza la clausula `BY REFERENCE`.

Una forma más expresiva de llamar a un programa subordinado con parámetros por referencia es la siguiente:

```cobol
CALL "suma" USING BY REFERENCE parametro1 parametro2.
```

También se puede abreviar de la siguiente manera:

```cobol
CALL "suma" USING REFERENCE parametro1 parametro2.
```

#### Parámetros por Valor

Los parametros se pueden pasar por valor, esto significa que si se modifica el valor de un parámetro en el programa subordinado, el valor del parámetro en el programa principal no se modificará, por lo que podemos trabajar con este valor sin miedo a romper el programa principal. Para ello se utiliza la clausula `BY CONTENT`.

```cobol
CALL "suma" USING BY CONTENT parametro1 parametro2.
```

También se puede abreviar de la siguiente manera:

```cobol
CALL "suma" USING CONTENT parametro1 parametro2.
```

En este caso, el programa anterior va a hacer la suma entre los dos parámetros, pero si se modifica el valor de `num1` en el programa subordinado, el valor de `parametro1` en el programa principal no se modificará. Por lo que nunca obtendremos el resultado de la suma en el programa principal.

En este caso podríamos hacer que solamente se imprima el resultado de la suma en el subprograma en caso de que no se necesite su resultado para siguientes operaciones en el programa principal.

#### Parámetros Combinados

Se pueden pasar parámetros por valor y por referencia al mismo tiempo, para hacer esto se debe especificar el tipo de paso de cada parámetro.

```cobol
CALL "suma" USING BY CONTENT parametro1 BY REFERENCE parametro2.
```

Si fueran más parametros, se podría especificar cada uno de ellos de la siguiente manera:

```cobol
CALL "suma" USING BY CONTENT parametro1 BY REFERENCE parametro2 BY CONTENT parametro3 BY REFERENCE parametro4.
```

o se podría abreviar de la siguiente manera:

```cobol
CALL "suma" USING BY CONTENT parametro1 parametro3 BY REFERENCE parametro2 parametro4.
```

En el última caso, se pasan los parámetros `parametro1` y `parametro3` por valor y los parámetros `parametro2` y `parametro4` por referencia.

### Retorno de Valores

Se puede retornar un valor desde el programa subordinado al programa principal utilizando la clausula `RETURNING`. Para ello, se debe especificar el valor que se quiere retornar en el programa subordinado y el valor que se quiere recibir en el programa principal.

__Programa Principal__
```cobol
CALL "suma" USING BY REFERENCE parametro1 parametro2 RETURNING resultado.
```

o

```cobol
CALL "suma" USING BY REFERENCE parametro1 parametro2 GIVING resultado.
```

Siendo `resultado` una variable que se encuentra en el programa principal.

__Programa Subordinado__
```cobol
DATA DIVISION.
LINKAGE SECTION.
    01  num1 PIC 9(2).
    01  num2 PIC 9(2).
WORKING-STORAGE SECTION.
    01  resultado PIC 9(3).

PROCEDURE DIVISION USING parametro1 parametro2 RETURNING resultado.
    ADD num1 TO num2 GIVING resultado.
    EXIT PROGRAM.
```

Vemos que en el programa subordinado se declara una variable `resultado` en la sección `WORKING-STORAGE SECTION`, la cual se utiliza con la clausula `RETURNING` para retornar el valor de la misma cuando este subprograma finalice su ejecución. La misma no se encuentra en la sección `LINKAGE SECTION` ya que no se recibe desde el programa principal.

## Manejo de Errores

Siempre que se llame a un programa externo pueden ocurrir errores, por lo que es importante manejarlos de manera correcta. Para ello, se puede utilizar la clausula `ON EXCEPTION` para manejar los errores que ocurran en el programa subordinado.

```cobol
CALL "suma" USING BY REFERENCE parametro1 parametro2
    ON EXCEPTION
        DISPLAY "Error al llamar al programa suma"
END-CALL
```

En caso de que ocurra un error al llamar al programa `suma`, se imprimirá en pantalla el mensaje `Error al llamar al programa suma`, se pueden realizar cuantas acciones se deseen en caso de que ocurra un error y también puede reemplazarse el `END-CALL` por un `.` para finalizar el bloque de código dentro del `CALL`.


<br>

### Datos del Cheat Sheet

\- Autor: Nicolás Villamonte <br>
\- Fecha: 05/02/2024 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
\- Herramientas y Versiones: Cobol, OpenCobolIde 4.7.6