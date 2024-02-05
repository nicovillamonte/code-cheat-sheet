# Manejo de Archivos en COBOL

1. [Archivo Lógico y Físico](#archivo-lógico-y-físico)
2. [Tipos de Archivos](#tipos-de-archivos)
   1. [Archivos Secuenciales](#archivos-secuenciales)
   2. [Archivos Indexados](#archivos-indexados)
      - [Modos de Acceso](#modos-de-acceso)
3. [Apertura y Cierre de Archivos](#apertura-y-cierre-de-archivos)
   1. [Apertura de Archivos](#apertura-de-archivos)
       - [Modos de Apertura](#modos-de-apertura)
    2. [Cierre de Archivos](#cierre-de-archivos)
4. [Lectura y Escritura de Archivos](#lectura-y-escritura-de-archivos)
   1. [Escritura de Archivos](#escritura-de-archivos)
   2. [Lectura de Archivos](#lectura-de-archivos)
       1. [Lectura Secuencial](#lectura-secuencial)
          - [Terminar la Lectura Secuencial](#terminar-la-lectura-secuencial)
       2. [Lectura Directa](#lectura-directa)
           - [Manejo de Errores en la Lectura Directa](#manejo-de-errores-en-la-lectura-directa)

- [Datos del Cheat Sheet](#datos-del-cheat-sheet)

# Archivo Lógico y Físico

En COBOL, un **archivo lógico** es aquel que se define en el programa con la estructura de datos que manejará el mismo y se utiliza para leer o escribir datos. Un **archivo físico** es aquel que se encuentra en el sistema de archivos del sistema operativo. Ambos deben ser definidos en el programa de manera que se conecten para que el programa pueda leer o escribir datos en el mismo.

Para ello, se debe definir el archivo físico en la sección de `FILE-CONTROL` dentro de la `INPUT-OUTPUT SECTION` de la `ENVIRONMENT DIVISION` y el archivo lógico en la sección de `FILE SECTION` de la `DATA DIVISION` del programa.

_Archivo físico:_
```cobol
ENVIRONMENT DIVISION.
   INPUT-OUTPUT SECTION.
   FILE-CONTROL.
       SELECT file-name ASSIGN TO "file-name.dat".
```

> Este es un ejemplo de como se enlaza un archivo físico con un archivo lógico. El archivo físico se llama `file-name.dat` y el archivo lógico se llama `file-name`, el cual vamos a definir su estructura en la sección de `FILE SECTION` de la `DATA DIVISION` del programa.

_Archivo lógico:_
```cobol
DATA DIVISION.
   FILE SECTION.
   FD   file-name.
        01 USUARIO.
            05 USUARIO-ID PIC 9(5).
            05 USUARIO-NOMBRE PIC A(15).
            05 USUARIO-APELLIDO PIC A(25).
            05 USUARIO-EDAD PIC 9(3).
            05 USUARIO-TELEFONO.
                10 USUARIO-TELEFONO-CELULAR PIC 9(10).
                10 USUARIO-TELEFONO-FIJO PIC 9(8).
```

Se utiliza la cláusula `FD` seguido del nombre que se le asignó anteriormente al archivo lógico. Dentro de la cláusula `FD` se define la estructura del archivo lógico, en este caso, se define un registro compuesto llamado `USUARIO`. En este archivo se van a manejar los campos creados dentro de `USUARIO` por cada registro.

# Tipos de archivos

Existen varias formas de manejar archivos en COBOL, entre ellos se encuentran los archivos secuenciales, los archivos indexados y los archivos de acceso directo.

## Archivos Secuenciales

Los archivos secuenciales son aquellos que se leen o escriben de forma secuencial, es decir, se leen o escriben de principio a fin. Por lo que se debe recorrer el archivo completo para llegar a un registro en específico.

En COBOL, los archivos secuenciales se manejan con el uso de la cláusula `ORGANIZATION IS SEQUENTIAL` en la definición del archivo.

```cobol
SELECT file-name ASSIGN TO "file-name.dat"
ORGANIZATION IS SEQUENTIAL.
```

Este tipo de archivos suele ser legible por humanos, ya que se pueden abrir y leer con un editor de texto. Sin embargo, no son eficientes para manejar grandes cantidades de datos.

## Archivos Indexados

Los archivos indexados son aquellos en los que se puede llegar a un registro en específico a través de un índice. Es decir, cada registro debe tener una clave primaria única que lo represente para poder acceder a él.

En COBOL, los archivos indexados se manejan con la cláusula `ORGANIZATION IS INDEXED` en la definición del archivo, seguido de la clave primaria del archivo y el modo en el que se va a acceder a los datos.

```cobol
SELECT file-name ASSIGN TO "file-name.dat"
ORGANIZATION IS INDEXED
RECORD KEY IS USUARIO-ID
ACCESS MODE IS DYNAMIC.
```

### Modos de acceso

Los modos de acceso son la forma en la que se va a acceder a los datos del archivo. En COBOL, se pueden utilizar los siguientes modos de acceso:

- **DYNAMIC**: Permite leer, escribir, modificar y borrar registros de forma aleatoria. Es decir, se puede acceder directamente a un registro a través de su clave primaria sin necesidad de recorrer el archivo completo.
- **SEQUENTIAL**: Permite leer y escribir registros de forma secuencial. Es decir, se debe recorrer el archivo completo para llegar a un registro en específico.
- **RANDOM**: Permite utilizar tanto el acceso directo a registros como el acceso secuencial cuando se necesite.


# Apertura y Cierre de Archivos

Una vez que se han definido los archivos lógicos y físicos, se deben abrir para poder leer o escribir datos en ellos. Y es muy importante acordarse de cerrarlos una vez que el programa ya los haya terminado de utilizar.

## Apertura de Archivos

Para abrir un archivo en COBOL, se utiliza la cláusula `OPEN` seguido del modo en el que se va a abrir el archivo y el nombre del archivo lógico que se quiere abrir.

```cobol
OPEN {mode} {file-name}.
```

Donde `{mode}` es el modo en el que se va a abrir el archivo y `{file-name}` es el nombre del archivo lógico que se quiere abrir.

### Modos de apertura

En COBOL, se pueden utilizar los siguientes modos de apertura:

- **INPUT**: Abre el archivo para lectura solamente si el archivo existe, sino arroja un error.
- **OUTPUT**: Crea un nuevo archivo para escritura solamente, si el archivo existe lo sobreescribe.
- **I-O**: Abre el archivo para lectura y escritura, si el archivo no existe arroja un error.
- **EXTEND**: Abre el archivo para escritura solamente, si el archivo no existe lo crea y si existe agregará registros al final del archivo.

Es importante saber que para evitar que el I-O y el INPUT arrojen un error cuando los archivos no existen, se puede utilizar la cláusula `OPTIONAL` a la hora de conectar el archivo lógico con el archivo físico en la `ENVIRONMENT DIVISION`.

```cobol
SELECT OPTIONAL file-name ASSIGN TO "file-name.dat".
```

## Cierre de Archivos

Para cerrar un archivo en COBOL, se utiliza la cláusula `CLOSE` seguido del nombre del archivo lógico que se quiere cerrar.

```cobol
CLOSE {file-name}.
```

Recordar que es muy importante cerrar los archivos una vez que ya no se van a utilizar, ya que si no se cierran, se pueden perder los datos que se hayan escrito en ellos o cosas peores.

# Lectura y Escritura de Archivos

Una vez que se han definido y abierto los archivos, se pueden leer o escribir datos en ellos. Para ello, se utilizan las cláusulas `READ` y `WRITE` respectivamente.

## Escritura de Archivos

Para escribir datos en un archivo en COBOL, se utiliza la cláusula `WRITE` seguido del registro que se quiere escribir. Es importante recordar que el archivo debe estar abierto en modo de escritura para poder escribir datos en él.

```cobol
WRITE USUARIO.
```

En este caso, utilizamos la variable compuesta `USUARIO` que definimos anteriormente en la sección de `FILE SECTION` de la `DATA DIVISION` para escribir un registro en el archivo. Todo lo que se encuentre dentro de `USUARIO` se escribirá en el archivo campo por campo.

## Lectura de Archivos

Existen dos maneras de leer datos de un archivo en COBOL, la primera es leer el siguiente registro del archivo (lectura secuencal) y la segunda es leer un registro en específico a través de su clave primaria (lectura directa o aleatoria). Lo que dependerá del modo en el que se haya abierto el archivo y como se haya definido el mismo al conectarlo con el archivo físico.

### Lectura Secuencial

Para leer el siguiente registro de un archivo en COBOL, se utiliza la cláusula `READ` seguido del registro en el que se quiere almacenar el registro leído. Es importante recordar que el archivo debe estar abierto en modo de lectura para poder leer datos de él.

```cobol
READ nombre-archivo.
```

El resultado de la lectura del registro se almacenará en la variable que se le haya asignado al archivo lógico y se podrá utilizar para mostrar los datos leídos o para hacer operaciones con ellos.

Otra forma de realizarlo, un poco más expresivo, es la siguiente:

```cobol
READ nombre-archivo NEXT RECORD.
```

Esto no es obligatorio, pero es una forma de hacer más claro el código. De heco, esta es la manera más clara en la que se puede leer un archivo secuencial:

```cobol
READ nombre-archivo NEXT RECORD INTO USUARIO.
```

La palabra `NEXT` es la que nos dice con mayor claridad que los archivos se van a leer de manera secuencial, ya que para leerlo de forma directa esta palabra no clave debe ser utilizada.

> Estas son tres formas exactamente iguales de leer un archivo secuencial, cambiando solamente la expresividad del código.

#### Terminar la Lectura Secuencial

Al leer secuencialmente estamos leyendo registro a registro hasta que el archivo llegue a su fin. Para saber si ya se llegó al final del archivo, se puede utilizar la cláusula `AT END` seguido de las instrucciones que se quieran realizar en caso de que ya no haya más registros que leer.

```cobol
READ nombre-archivo NEXT RECORD
AT END
    DISPLAY "No hay más registros que leer"
    CLOSE nombre-archivo.
    STOP RUN.
```

Además, se puede utilizar la cláusula `NOT AT END` para realizar instrucciones en caso de que aún haya registros que leer.

```cobol
PERFORM LEER-REGISTRO.
    READ nombre-archivo NEXT RECORD
    AT END
        DISPLAY "No hay más registros que leer"
        CLOSE nombre-archivo
        STOP RUN
    NOT AT END
        DISPLAY "Registro leído: " USUARIO
        PERFORM LEER-REGISTRO
    END-READ
```

Los bloques READ se pueden cerrar con la cláusula `END-READ` o con un punto (`.`) en la última instrucción para indicar que ya no se van a realizar más lecturas de registros.


### Lectura Directa

Para leer un registro en específico de un archivo en COBOL, antes de utilizar la cláusula `READ` se debe actualizar la variable que hayamos elegido como clave primaria.

```cobol
MOVE 0 TO USUARIO-ID.
READ nombre-archivo.
```

Ahora todos los demás campos de `USUARIO` contendrán los datos del registro que tenga como clave primaria `USUARIO-ID` el valor `0`.

Otra forma de realizarlo, un poco más expresivo, es la siguiente:

```cobol
MOVE 0 TO USUARIO-ID.

READ nombre-archivo 
RECORD INTO USUARIO 
KEY IS USUARIO-ID.
```

Al igual que en la lectura secuencial, esto no es obligatorio, pero es una forma de hacer más claro el código para que se entienda que se está realizando una lectura directa y de donde se está obteniendo la clave primaria y a donde se está almacenando el registro leído.

#### Manejo de Errores en la Lectura Directa

Si no se encuentra un registro con esa clave primaria, se arrojará un error, el cual se puede manejar con la cláusula `INVALID KEY` seguido de las instrucciones que se quieran realizar en caso de que no se encuentre el registro.

```cobol
LEER-REGISTRO.
    READ nombre-archivo
    INVALID KEY
        DISPLAY "No se encontró el registro con la clave primaria 0"
        DISPLAY "Elija otra clave: "
        ACCEPT USUARIO-ID
        PERFORM LEER-REGISTRO.
```

Además, se puede utilizar la cláusula `NOT INVALID KEY` para realizar instrucciones en caso de que se encuentre el registro.

```cobol
DISPLAY "Ingrese el ID del usuario: ".
ACCEPT USUARIO-ID.

LEER-REGISTRO.
    READ nombre-archivo
    INVALID KEY
        DISPLAY "No se encontró el registro con la clave primaria 0"
        DISPLAY "Elija otra clave: "
        ACCEPT USUARIO-ID
        PERFORM LEER-REGISTRO
    NOT INVALID KEY
        DISPLAY "Usuario encontrado: " USUARIO.
    END-READ.
```


<br>

### Datos del Cheat Sheet

\- Autor: Nicolás Villamonte <br>
\- Fecha: 05/02/2024 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
\- Herramientas y Versiones: Cobol, OpenCobolIde 4.7.6