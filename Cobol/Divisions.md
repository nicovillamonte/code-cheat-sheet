> !! INCOMPLETO

# Divisiones en Cobol

Cobol, estructuralmente, se compone de varias divisiones o `DIVISIONS`. Cada una de estas divisiones tiene un propósito específico y se utiliza para organizar el código de manera lógica.

1. 

## Divisions

Las principales 4 divisiones de las que se compone un programa en Cobol son:

- **Identification Division** o División de Identificación
- **Environment Division** o División de Entorno
- **Data Division** o División de Datos
- **Procedure Division** o División de Procedimientos

### Identification Division

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

### Environment Division

En la `Environment Division` se especifican los recursos que el programa necesita para ejecutarse. Esto incluye archivos, dispositivos de entrada/salida, etc.

Dentro de esta division se encuentran dos secciones principales: `Configuration Section` y `Input-Output Section`. 

- **Configuration Section**: Identifica la computadora utilizada para compilar programas.
- **Input-Output Section**: Identifica los archivos y los recursos de entrada y salida utilizados por el programa.

```cobol
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
```

### Data Division

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
    FD ...
WORKING-STORAGE SECTION.
LOCAL-STORAGE SECTION.
LINKAGE SECTION.
REPORT SECTION.
SCREEN SECTION.
```




LINKS PARA SEGUIR

https://www.programacionfacil.org/cursos/cobol/capitulo_2_identification_division.html

https://www.geeksforgeeks.org/cobol-divisions/