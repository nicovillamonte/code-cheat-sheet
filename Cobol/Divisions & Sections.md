# Divisiones y Secciones en Cobol

Cobol, estructuralmente, se compone de varias divisiones o `DIVISIONS`. Cada una de estas divisiones tiene un propósito específico y se utiliza para organizar el código de manera lógica.

1. [Tipos de Divisiones](#tipos-de-divisiones)
2. [Identification Division](#identification-division)
   1. [PROGRAM-ID](#program-id)
   2. [AUTHOR](#author)
   3. [DATE-WRITTEN](#date-written)
   4. [DATE-COMPILED](#date-compiled)
3. [Environment Division](#environment-division)
   1. [Configuration Section](#configuration-section)
   2. [Input-Output Section](#input-output-section)
4. [Data Division](#data-division)
   1. [File Section](#file-section)
   2. [Working-Storage Section](#working-storage-section)
   3. [Local-Storage Section](#local-storage-section)
   4. [Linkage Section](#linkage-section)
   5. [Communication Section](#communication-section)
   6. [Report Section](#report-section)
   7. [Screen Section](#screen-section)
5. [Procedure Division](#procedure-division)

- [Datos del Cheat Sheet](#datos-del-cheat-sheet)

## Tipos de Divisiones

Las principales 4 divisiones de las que se compone un programa en Cobol son:

- **Identification Division** o División de Identificación
- **Environment Division** o División de Entorno
- **Data Division** o División de Datos
- **Procedure Division** o División de Procedimientos

## Identification Division

La `Identification Division` es la primera división de un programa Cobol. Esta división se utiliza para identificar el programa y proporcionar información sobre el mismo.

En versiones anteriores de Cobol, en esta división se incluía el nombre del programa, el autor, la fecha de creación, la fecha de modificación, etc. Sin embargo, en la actualidad, la mayoría de estos detalles se incluyen en comentarios en lugar de en la `Identification Division`.

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.
AUTHOR. YOUR-NAME.
DATE-WRITTEN. 01/01/2021.
DATE-COMPILED. 01/01/2021.
INSTALLATION. YOUR-ORGANIZATION.
```

### PROGRAM-ID

La cláusula `PROGRAM-ID` se utiliza para especificar el nombre del programa. Este nombre se utiliza para identificar el programa en el sistema.

### AUTHOR

La cláusula `AUTHOR` se utiliza para especificar el nombre del autor del programa.

### DATE-WRITTEN

La cláusula `DATE-WRITTEN` se utiliza para especificar la fecha en que se escribió el programa.

### DATE-COMPILED

La cláusula `DATE-COMPILED` se utiliza para especificar la fecha en que se compiló el programa.

## Environment Division

En la `Environment Division` se especifican los recursos que el programa necesita para ejecutarse. Esto incluye archivos, dispositivos de entrada/salida, etc.

Dentro de esta division se encuentran dos secciones principales: `Configuration Section` y `Input-Output Section`. 

- **Configuration Section**: Identifica la computadora utilizada para compilar programas.
- **Input-Output Section**: Identifica los archivos y los recursos de entrada y salida utilizados por el programa.

```cobol
ENVIRONMENT DIVISION.
```

### Configuration Section

La `Configuration Section` se utiliza para identificar la computadora utilizada para compilar programas. Esta sección se compone de varias secciones, como `Source Computer`, `Object Computer`, `Special-Names`, etc.

```cobol
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
    SOURCE-COMPUTER. IBM-370.
    OBJECT-COMPUTER. IBM-370.
    SPECIAL-NAMES.
        DECIMAL-POINT IS COMMA.
        CURSOR IS UNDERLINE.
        ...
```

* `SOURCE-COMPUTER`: Se utiliza para especificar el tipo de computadora en la que se compila el programa.
* `OBJECT-COMPUTER`: Se utiliza para especificar el tipo de computadora en la que se ejecuta el programa.
* `SPECIAL-NAMES`: Se utiliza para especificar nombres especiales, como el punto decimal, el cursor, etc. Se cambian constantes del lenguaje.

### Input-Output Section

La `Input-Output Section` se utiliza para identificar los archivos y los recursos de entrada y salida utilizados por el programa. Esta sección se compone de varias secciones, como `File Control`, `I-O Control`, `I-O-Descriptor`, etc.

```cobol
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
    FILE-CONTROL.
        SELECT EMPLOYEE-FILE ASSIGN TO 'EMPLOYEE.DAT'.
        SELECT PAYROLL-FILE ASSIGN TO 'PAYROLL.DAT'.
        ...
    I-O-CONTROL.
        ORGANIZATION IS LINE SEQUENTIAL.
        ACCESS MODE IS SEQUENTIAL.
        ...
    I-O-DESCRIPTOR.
        FILE EMPLOYEE-FILE
            RECORDING MODE IS F.
            ...
        FILE PAYROLL-FILE
            RECORDING MODE IS F.
            ...
```

* `FILE-CONTROL`: Se utiliza para especificar los archivos físicos utilizados por el programa.
* `I-O-CONTROL`: Se utiliza para especificar los recursos de entrada y salida utilizados por el programa.
* `I-O-DESCRIPTOR`: Se utiliza para especificar la estructura de los archivos utilizados por el programa.


## Data Division

La `Data Division` se utiliza para definir las variables y estructuras de datos utilizadas por el programa. Esta división se compone de varias secciones, como `File Section`, `Working-Storage Section`, `Local-Storage Section`, etc.

- **File Section**: Se utiliza para definir archivos y registros.
- **Working-Storage Section**: Se utiliza para definir variables globales.
- **Local-Storage Section**: Se utiliza para definir variables locales.
- **Linkage Section**: Se utiliza para definir variables que se pasan entre programas.
- **Report Section**: Se utiliza para definir informes.
- **Screen Section**: Se utiliza para definir pantallas.

```cobol
DATA DIVISION.
FILE SECTION.
WORKING-STORAGE SECTION.
LOCAL-STORAGE SECTION.
LINKAGE SECTION.
REPORT SECTION.
SCREEN SECTION.
```

### File Section

La `File Section` se utiliza para definir archivos lógicos (Tomados de los físicos) y registros utilizados por el programa. Esta sección se compone de varias secciones, como `File Description`, `Record Description`, etc.

```cobol
DATA DIVISION.
FILE SECTION.
    FD EMPLOYEE-FILE.
        01 EMPLOYEE-RECORD.
            05 EMPLOYEE-ID PIC 9(5).
            05 EMPLOYEE-NAME PIC X(20).
            05 EMPLOYEE-SALARY PIC 9(5)V99.
            ...
```

En este caso se define un archivo lógico llamado `EMPLOYEE-FILE` con un registro llamado `EMPLOYEE-RECORD` que contiene tres campos: `EMPLOYEE-ID`, `EMPLOYEE-NAME` y `EMPLOYEE-SALARY`.

### Working-Storage Section

La `Working-Storage Section` se utiliza para describir datos que no forman parte de archivos de datos, pero que son desarrollados y procesados por un programa o método. Estos pueden ser datos temporales, variables de control, constantes, etc.

```cobol
DATA DIVISION.
WORKING-STORAGE SECTION.
    01 EMPLOYEE-ID PIC 9(5).
    01 EMPLOYEE-NAME PIC X(20).
    01 EMPLOYEE-SALARY PIC 9(5)V99.
    ...
```

Las variables en `WORKING-STORAGE` se asignan cuando el programa comienza y se liberan cuando el programa termina. Mantienen sus valores durante toda la ejecución del programa, lo que significa que no se reinician ni se reasignan con cada llamada a un procedimiento o método.

Los datos que tienen una cláusula `VALUE` se inicializan con el valor especificado en esta cláusula al inicio del programa. Si un valor cambia durante la ejecución, ese nuevo valor se mantiene hasta que el programa termina o hasta que el valor sea modificado nuevamente.

### Local-Storage Section


Según la documentación oficial de Cobol, la `Local-Storage Section` define el almacenamiento que se asigna y libera a las variables en una base por invocación. Está diseñado para datos que son específicos a una invocación particular de un programa o método.

```cobol
DATA DIVISION.
LOCAL-STORAGE SECTION.
    01 EMPLOYEE-ID PIC 9(5).
    01 EMPLOYEE-NAME PIC X(20).
    01 EMPLOYEE-SALARY PIC 9(5)V99.
    ...
```

Las variables en `LOCAL-STORAGE` se realocan en cada invocación. Esto significa que cada vez que se invoca el programa o método que contiene estas variables, se asigna un nuevo espacio de almacenamiento para ellas.

En cada invocación, los datos que tienen una cláusula `VALUE` se inicializan con el valor especificado en esa cláusula. Esto es crucial en el caso de programas anidados o métodos, donde los datos se reasignan y reinicializan en cada invocación del programa o método contenedor.

### Linkage Section

La `Linkage Section` se utiliza en COBOL para definir variables que se pasan entre diferentes programas. Es comúnmente usada cuando un programa COBOL llama a otro programa COBOL o es llamado por otro programa, y necesita compartir datos entre ellos. Es decir, se utiliza para la **comunicación entre programas**.

```cobol
DATA DIVISION.
LINKAGE SECTION.
    01 EMPLOYEE-ID PIC 9(5).
    01 EMPLOYEE-NAME PIC X(20).
    01 EMPLOYEE-SALARY PIC 9(5)V99.
    ...
```

Las variables definidas en la `Linkage Section` son usadas para recibir datos de otro programa o para enviar datos a otro programa.

Estas variables no son automáticamente inicializadas; sus valores son establecidos por el programa que hace la llamada o por parámetros pasados durante la llamada.

### Communication Section

La `Communication Section` se utiliza en programas que necesitan **comunicarse con sistemas externos o dispositivos**, como parte de un sistema de procesamiento de transacciones o en la programación de sistemas de comunicación.

```cobol
DATA DIVISION.
COMMUNICATION SECTION.
    01 EMPLOYEE-ID PIC 9(5).
    01 EMPLOYEE-NAME PIC X(20).
    01 EMPLOYEE-SALARY PIC 9(5)V99.
    ...
```

Se emplea para definir áreas de memoria para la recepción y transmisión de datos entre el programa COBOL y un sistema o dispositivo externo.

Es menos común en aplicaciones modernas de COBOL, pero sigue siendo relevante en ciertos sistemas legados y aplicaciones de procesamiento de transacciones.

### Report Section

La `Report Section` se utiliza para definir la estructura y el formato de los informes que tu programa generará. Es útil para crear informes impresos o archivos de salida con un formato específico.

### Screen Section

La `Screen Section` se utiliza para definir la interfaz de usuario en programas COBOL que interactúan con una pantalla o terminal. Esta sección es importante para diseñar la disposición y el control de los elementos en una pantalla de usuario.


## Procedure Division

La `Procedure Division` es la división principal de un programa Cobol. Esta división se utiliza para escribir el código del programa, donde la lógica de negocio se va a encontrar. 

```cobol
PROCEDURE DIVISION.
```

Esta sección contiene en su interior secciones definidas por el usuario, párrafos, oraciones, declaraciones, cláusulas y verbos.

```cobol
PROCEDURE DIVISION.
MAIN.
    DISPLAY 'Hello, World!'.

QUEST.
    DISPLAY 'What is your name?'.
    ACCEPT NAME.
    IF NAME IS ALPHABETIC
        DISPLAY 'Hello, ' NAME
        PERFORM FINISH
    PERFORM QUEST.
    
FINISH.
    STOP RUN.
```

<br>

### Datos del Cheat Sheet

\- Autor: Nicolás Villamonte <br>
\- Fecha: 01/02/2024 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
\- Herramientas y Versiones: Cobol, OpenCobolIde 4.7.6