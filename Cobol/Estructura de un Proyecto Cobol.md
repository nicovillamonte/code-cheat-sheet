# Estructura de un Proyecto Cobol

La estructura de un proyecto siempre dependerá de la organización y de las necesidades de la misma, pero a continuación se muestra una estructura básica de un proyecto Cobol que se recomienda utilizar para mantener un orden en el mismo.

## Estructura de Archivos

La estructura de archivos de un proyecto Cobol puede variar dependiendo de la organización, pero a continuación se muestra una estructura básica que se recomienda utilizar para mantener un orden en el proyecto.

```
Proyecto/
├── src/
│   ├── main.cbl
│   ├── productos/
│   │   ├── agregarProducto.cbl
│   │   ├── actualizarProducto.cbl
│   │   └── listarProductos.cbl
│   ├── ventas/
│   │   ├── procesarVenta.cbl
│   │   ├── devolucionVenta.cbl
│   │   └── reporteVentas.cbl
│   └── utilidades/
│       ├── manejoArchivos.cbl
│       └── calculosGenerales.cbl
├── data/
│   ├── inventario.dat
│   └── ventas.dat
├── lib/
│   └── (bibliotecas externas, si se utilizan)
├── build/
│   └── compile_and_link_script.sh
└── docs/
    └── (documentación del proyecto)
```

Como vemos, la estructura de archivos de un proyecto Cobol se divide en las siguientes carpetas:

- **src**: Contiene los programas Cobol que forman parte del proyecto.
  - **main.cbl**: Programa principal del proyecto.
  - **modulos**: Carpetas que contienen programas que realizan tareas específicas.
- **data**: Contiene los archivos de datos que se utilizan en el proyecto.
- **lib**: Contiene las bibliotecas externas que se utilizan en el proyecto si es que se utilizan. Depende del sistema operativo la extensión de los archivos de estas bibliotecas van a variar, por ejemplo en Windows la extensión es `.dll`.
- **build**: Contiene los scripts de compilación y enlazado del proyecto. Cuando no se utiliza un IDE, es común tener un script que compile y enlace todos los programas del proyecto.
- **docs**: Contiene la documentación del proyecto. Generalmente archivos `.md` o `.pdf` que describen el funcionamiento del proyecto.


<br>

### Datos del Cheat Sheet

\- Autor: Nicolás Villamonte <br>
\- Fecha: 05/02/2024 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
\- Herramientas y Versiones: Cobol, OpenCobolIde 4.7.6