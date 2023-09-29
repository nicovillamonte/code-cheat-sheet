# Estructura de un proyecto Angular

En este Cheat Sheet vamos a ver una opción muy viable para estructurar un proyecto profesional de frontend con el framework Angular.

- [Estructura](#estructura)
  - [Core](#core)
  - [Data](#data)
  - [Shared](#shared)
  - [Layout](#layout)
  - [Modules](#modules)
  - [Documentation](#documentation)
- [Fuente](#fuente)


- [Datos del cheat sheet](#datos-del-cheat-sheet)

> Al momento de escribirse este Cheat Sheet, Angular se encuentra en su versión 16. Por lo que debe estar ateneto a posibles cambios en futuras versiones.

## Estructura

La estructura de un proyecto Angular idealmente se organizaría de la siguiente manera:

```
├───app
│   ├───core
│   ├───data
│   │   ├───dto
│   │   ├───mocks
│   │   ├───models
│   │   └───services
│   ├───documentation
│   ├───layout
│   ├───modules
│   ├───shared
│   │   ├───components
│   │   └───modules
│   └───styles
└───assets
```

### Core

Este módulo contiene clases utilizadas por el módulo principal de la aplicación app.module. Los recursos que siempre se cargan en nuestra aplicación, como guards de rutas, interceptores HTTP y servicios de nivel de aplicación.

```
├───app
│   ├───core
│   │   ├───interceptors
│   │   ├───guards
│   │   └───...
```

### Data

El módulo de datos contiene los tipos (modelos / entidades) y servicios de los datos consumidos por la aplicación.

```
├───app
│   ├───core
│   ├───data
│   │   ├───dto
│   │   ├───mocks
│   │   ├───models
│   │   └───services
```

### Shared

> Este módulo contiene recursos que se utilizan en más de un módulo cargado dinámicamente. Al cargar siempre con la aplicación, los componentes compartidos están listos cuando un módulo los solicita.

Dentro de este directorio, podemos hacer otra clasificacion entre modulos y componentes, ya que se pueden desarrollar componentes separados para compartir como un modulo completo que se exporte para su uso en otros modulos de la aplicacion.

```
├───app
│   ├───shared
│   │   ├───components
│   │   └───modules
```

### Layout

> Este directorio contiene componentes que actúan como un diseño o son partes de un diseño como el header, footer, skeleton, etc.

Es posible que te encuentres con el dilema de si poner el header o el footer dentro de esta carpeta o dentro de la carpeta shared. Ninguno de los casos es incorrecto. Sin embargo, en este esquema que se esta proponiendo es mejor que se desarrollen dentro del directorio Layout ya que este directorio se enfoca específicamente en contener componentes que están relacionados con la estructura general y diseño de la aplicación. 

### Modules

El directorio de módulos contiene una colección de módulos que son independientes entre sí. Esto permite que Angular cargue solo el módulo que requiere para mostrar la solicitud, lo que ahorra ancho de banda y acelera toda la aplicación.


### Documentation

Este directorio se utilizara para almacenar la documentacion del proyecto, todos deberán ser archivos markdown (.md) como el que está leyendo actualmente.

Aunque generalmente la documentación de un proyecto se incluye en el repositorio en el que el código se encuentra subido, esta carpeta puede utilizarse para documentación más técnica que pueda servirles a los desarrolladores a la hora de trabajar en el proyecto. Además, aquí se pueden dejar anotadas las pautas del proyecto para una mejor organización y código más limpio y legible.


## Fuente
[Estructura base para cualquier proyecto de Angular](https://baguilar6174.medium.com/estructura-base-para-cualquier-proyecto-de-angular-6a035a27bfcf)

### Datos del cheat sheet
\- Autor: Nicolás Villamonte <br>
\- Fecha: 20/07/2023 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
\- Herramientas y Versiones: Angular V16