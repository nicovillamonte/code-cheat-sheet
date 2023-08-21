# Creación de un Proyecto NestJS

La creación de un proyecto en el framework de NodeJS, NestJS, es muy sencilla. Sin embargo la misma contiene muchas configuraciones que tenemos que ser capaces de entender para crear un proyecto más eficiente a la hora de trabajar sobre el código. En este documento se explicará como crear un proyecto NestJS y las configuraciones que se pueden realizar para mejorar el mismo. Además, se explicarán algunos tips, configuraciones y de más que pueden ser de gran utilidad.

## Indice

- [Pre-requisitos](#pre-requisites)
1. Instalación de NestJS
2. [Resumen](#resumen)

<h3 id="pre-requisites">Pre-requisitos</h3>

Como pre-requisito para poder crear un proyecto NestJS, es necesario tener instalado NodeJS y NPM. Para ello, se puede descargar desde su [página oficial](https://nodejs.org/es/) la versión LTS (Long Term Support) que es la recomendada para la mayoría de los usuarios.

Una vez instalado NodeJS, se puede verificar que se haya instalado correctamente ejecutando el siguiente comando en la terminal:

```bash
node -v
```
<p align="center">
  <img src="https://github.com/nicovillamonte/code-cheat-sheet/assets/64659720/da08dbc2-b428-4f01-ac19-cb9228b82967">
</p>

## Instalación de NestJS CLI

El primer paso para la creación de un proyecto en NestJS es instalar el CLI (Command Line Interface) de NestJS. Para ello, se debe ejecutar el siguiente comando en la terminal:

```bash
npm i -g @nestjs/cli
```

Esto se debe realizar por única vez en el ordenador que se va a utilizar NestJS ya que, con ese comando, se estaría instalando de forma global el CLI de NestJS. Una vez instalado, se puede verificar que se haya instalado correctamente ejecutando el siguiente comando en la terminal:

```bash
nest -v
```
<p align="center">
  <img src="https://github.com/nicovillamonte/code-cheat-sheet/assets/64659720/63f8ce95-dcf0-4497-aa8e-de9e88ae7c5b">
</p>

## Creación del proyecto

El comando por defecto para crear un proyecto en NestJS es el siguiente:

```bash
nest new <nombre-proyecto>
```

Donde _`<nombre-proyecto>`_ es el nombre que se le quiere dar al proyecto. Por ejemplo:

```bash
nest new mi-proyecto
```

Este comando creará una carpeta con el nombre del proyecto y dentro de la misma se encontrarán todos los archivos y carpetas necesarios para el funcionamiento del proyecto.

### Opciones del comando

En la [documentación oficial del comando `nest new` de NestJS](https://docs.nestjs.com/cli/usages) se pueden encontrar todas las opciones que se pueden utilizar al momento de crear un proyecto. Sin embargo, a continuación se explicarán las más importantes.

- **Opción `--skip-git`** o `-g`: Esta opción permite que no se cree un repositorio Git dentro del proyecto. Esto es útil cuando se va a crear un repositorio Git desde un servicio externo como GitHub, GitLab, Bitbucket, etc. y no se quiere que se cree un repositorio Git local.
- **Opción `--skip-install`** o `-s`: Esta opción permite que no se instalen las dependencias del proyecto. Esto es útil cuando se quiere instalar las dependencias manualmente o cuando se quiere instalar las dependencias en un momento posterior al de crear el proyecto.
- **Opción `--package-manager [package-manager]`** o `-p [package-manager]`: Esta opción permite que se especifique el gestor de paquetes que se quiere utilizar para instalar las dependencias del proyecto. Por defecto, se pregunta cuál se quiere utilizar a la hora de ejecutar el comando. Se pueden utilizar `npm`, `yarn` o `pnpm`.
- **Opción `--language [language]`** o `-l [language]`: Esta opción permite que se especifique el lenguaje que se quiere utilizar para el proyecto. Por defecto, se utiliza `TS` (TypeScript). Sin embargo, se puede utilizar `JS` (JavaScript).
- **Opción `--strict`**: Esta opción permite que se cree el proyecto con la configuración de TypeScript en modo estricto. Esto es útil cuando se quiere tener una configuración de TypeScript más estricta para evitar errores en el código aplicando mejores prácticas al mismo.


## Estructura del proyecto

La estructura de un proyecto NestJS creado con el comando `nest new` es la siguiente:

```bash
mi-proyecto
├── node_modules
├── src
│   ├── app.controller.spec.ts
│   ├── app.controller.ts
│   ├── app.module.ts
│   ├── app.service.ts
│   └── main.ts
├── test
│   ├── app.e2e-spec.ts
│   └── jest-e2e.json
├── .eslintrc.js
├── .gitignore
├── .prettierrc
├── nest-cli.json
├── package-lock.json
├── package.json
├── README.md
├── tsconfig.build.json
└── tsconfig.json
```

En un inicio, el proyecto creado con el comando `nest new` tiene una estructura muy simple. Sin embargo, a medida que se va desarrollando el proyecto, se van creando más carpetas y archivos para organizar el código de una mejor manera.

- En la carpeta **`src`** suele estar el código fuente del proyecto.
- El uso de la carpeta **`test`** es algo extraño en los proyectos JavaScript o TypeScript. Esto se debe a que los tests suelen ser archivos que se encuentran en la misma carpeta que el archivo que se está testeando. Sin embargo, el directorio `test` se suele utilizar para almacenar los tests de integración o end-to-end. De todas formas, se puede configurar el framework de testing elegido para que los tests se encuentren en ésta carpeta, separados del código fuente. Podemos tener algunos tests en la carpeta `test` y otros tests en la misma carpeta que el código fuente para ser más organizados en el código si es lo que se necesita para el proyecto.
- El archivo **`.eslintrc.js`** es el archivo de configuración de ESLint. ESLint es una herramienta de linting que permite analizar el código fuente para encontrar errores de sintaxis, errores de estilo, errores de lógica, etc. y así poder corregirlos antes de ejecutar el código. Esto es muy útil para evitar errores en tiempo de ejecución y para mantener un código limpio y ordenado.
- El archivo **`.prettierrc`** es el archivo de configuración de Prettier. Prettier es una herramienta que permite formatear el código fuente de una manera más legible y ordenada. Debe estar en armonía con el archivo de configuración de ESLint para que no haya conflictos entre ambos.
- El archivo **`.gitignore`** es el archivo que se utiliza para indicarle a Git qué archivos y carpetas no se deben subir al repositorio Git.
- El archivo **`nest-cli.json`** es el archivo de configuración del CLI de NestJS. En este archivo se pueden configurar algunas opciones del CLI de NestJS.
- Los archivos **`tsconfig.json`** y **`tsconfig.build.json`** son los archivos de configuración de TypeScript.
- Los archivos **`package.json`** y **`package-lock.json`** son los archivos de configuración de npm. En el archivo `package.json` se encuentran las dependencias del proyecto y en el archivo `package-lock.json` se encuentran las versiones de las dependencias del proyecto.
- El archivo **`README.md`** es el archivo que se utiliza para mostrar información sobre el proyecto en el repositorio Git. En este archivo se suele colocar información sobre el proyecto, cómo instalarlo, cómo ejecutarlo, cómo contribuir al proyecto, etc.
- La carpeta **`node_modules`** es la carpeta que se crea al instalar las dependencias del proyecto. En esta carpeta se encuentran todas las dependencias del proyecto.

## Herramientas útiles

### Swagger

Es recomendable, si se esta desarrollando un api con NestJS, utilizar Swagger para documentar y probar la misma. Para ello, se deben seguir los pasos de la [documentación oficial de NestJS para añadir y configurar Swagger](https://docs.nestjs.com/openapi/introduction) en el proyecto.

Tanto la instalación como configuración de Swagger es muy sencilla y rápida, por lo que se recomienda realizarla antes de comenzar a desarrollar una api con este framework.

Luego, durante el desarrollo, hay que tener en cuenta cómo se debe utilizar esta herramienta en el código, por lo que se recomienda leer las siguientes secciones de la documentación otorgada anteriormente. Sin embargo, podemos decir que algunos de los decoradores que se suelen usar más frecuentemente con esta herramienta son `@ApiTags()`, `@ApiHeader()`, `@ApiResponse()`, `@ApiProperty()`, [entre otros](https://docs.nestjs.com/openapi/decorators).


## Configuración del Proyecto

Antes de empezar a desarrollar el proyecto, es recomendable configurar algunas cosas para que el proyecto sea más eficiente y para que se pueda trabajar de una manera más cómoda tanto en equipos individuales como en equipos de trabajo.

### Configuración del editor de código

Es importante configurar el editor de código para que se adapte a las necesidades de cada uno. Pues algunas de las configuraciones por default con las que viene nuestro proyecto no son del todo cómodas para todos los desarrolladores. Una de las más comunes suele ser el tamaño de las tabulaciones. Por defecto, el tamaño de las tabulaciones suele ser de 4 espacios. Sin embargo, hay desarrolladores que prefieren que el tamaño de las tabulaciones sea de 2 espacios por su mayor prolijidad, legibilidad y mayor espacio en la pantalla para el código. Sin mencionar que el Prettier por defecto suele formatear el código con tabulaciones de 2 espacios al guardar el archivo, lo que nos arrojará un error de ESLint si no configuramos el editor de código para que tenga el mismo tamaño de tabulaciones que el Prettier o viceversa.

En este caso, se va a utilizar VS Code como editor de código. Tenemos dos formas para configurar VS Code en nuestro proyecto, la primera consta de presionar `Ctrl + Shift + P` para abrir el menú de comandos. Luego, se debe buscar la opción `Preferences: Open Settings (JSON)` y presionar `Enter` para abrir el archivo de configuración de VS Code. En este archivo se deben agregar las configuraciones que nosotros queramos en formato JSON. Por ejemplo, si queremos que el tamaño de las tabulaciones sea de 2 espacios, debemos agregar la siguiente configuración en el archivo de configuración de VS Code:

```js
{
  "editor.tabSize": 2
}
```

La otra manera de configurar VS Code es presionar `Ctrl + Shift + P` para abrir el menú de comandos. Luego, se debe buscar la opción `Preferences: Open Workspace Settings` (Esta vez no en formato JSON) y se nos abrirá un menú más visual que contiene la configuración que puede ser modificada de manera más sencilla. En este caso, se debe buscar la opción `Editor: Tab Size` y modificar el valor a 2.

![image](https://github.com/nicovillamonte/code-cheat-sheet/assets/64659720/45989d53-df3d-403f-8579-d62a78351a93)

Ambas formas son válidas. Yo, en lo personal, recomiendo que se acostumbren a realizarlo con el **JSON**, ya que una vez que tienes el JSON ideal para ti, puedes copiarlo y pegarlo en cualquier proyecto que tengas sin tener que configurar todo de nuevo. Además, es más fácil de compartir con otros desarrolladores (omitiendo este archivo en el `.gitignore`) para que todos puedan tener la misma configuración.

### Configuración de Scripts

Los Scripts de ejecución son comandos que se pueden ejecutar desde la terminal para realizar tareas específicas. Un tipico ejemplo podría ser el comando `npm start` con el que se suelen ejecutar los proyectos, como también el comando `npm test` para realizar los tests. Estos Scripts son totalmente configurables y se pueden crear nuevos Scripts para realizar tareas específicas.

En un proyecto de NestJS se creán muchos Scripts por defecto. La idea es modificarlos o agregar según la necesidad del desarrollador.

En mi caso suelo modificar solo dos de estos Scripts para el desarrollo de mis proyectos.

```js
"scripts": {
  ...
  "start": "nest start --watch",
  "test": "jest --detectOpenHandles --verbose --passWithNoTests",
},
```

El primero es el `npm start`, al que siempre le agrego el `--watch` para que el proyecto se ejecute en modo watch y se reinicie automáticamente cada vez que se detecte un cambio en el código fuente. Esto es muy útil para no tener que estar reiniciando el proyecto cada vez que se realiza un cambio. Aunque ya existe el Script `npm run start:dev` que hace lo mismo, yo prefiero utilizar el `npm start` para que sea más fácil de recordar y escribir, además de que se utilice de la misma manera por todos los miembros de un equipo de trabajo.

Por otro lado, al `npm test` le agrego varias opciones para tener una mejor experiencia de desarrollo. La primera es el `--detectOpenHandles` que nos permite detectar si hay algún proceso que no se ha cerrado correctamente. Esto es muy útil para detectar errores en el código que no se han manejado correctamente. La segunda es el `--verbose` que nos permite ver más información sobre los tests que se están ejecutando. Y la tercera es el `--passWithNoTests` que nos permite que los tests pasen aunque no haya tests escritos. Esto es muy útil para cuando estamos desarrollando y no hemos escrito ningún test aún, ya que nos permite ejecutar el comando `npm test` sin que nos arroje un error.

### Configuración de ESLint

ESLint es una herramienta de linting que permite analizar el código fuente para encontrar errores de sintaxis, errores de estilo, errores de lógica, etc. y así poder corregirlos antes de ejecutar el código. Esto es muy útil para evitar errores en tiempo de ejecución y para mantener un código limpio y ordenado. Sin embargo, a veces la configuración por defecto no se ajusta a nuestras necesidades de desarrollo y puede llegar a un punto de ser muy molesto a la hora de programar. Por eso es que es necesario saber cómo configurar ESLint para que se adapte a nuestras necesidades.

Para ello, debemos abrir el archivo **`.eslintrc.js`** que se encuentra en la raíz del proyecto. En él vamos a ver que hay muchos campos a configurar. 

```js
module.exports = {
  parser: '@typescript-eslint/parser',
  parserOptions: {
    project: 'tsconfig.json',
    tsconfigRootDir: __dirname,
    sourceType: 'module',
  },
  plugins: ['@typescript-eslint/eslint-plugin'],
  extends: [
    'plugin:@typescript-eslint/recommended',
    'plugin:prettier/recommended',
  ],
  root: true,
  env: {
    node: true,
    jest: true,
  },
  ignorePatterns: ['.eslintrc.js'],
  rules: {
    '@typescript-eslint/interface-name-prefix': 'off',
    '@typescript-eslint/explicit-function-return-type': 'off',
    '@typescript-eslint/explicit-module-boundary-types': 'off',
    '@typescript-eslint/no-explicit-any': 'off',
  },
};
```

Lo más importante es entender que todo se basa en el parser y en reglas. En este caso se está utilizando el parser de **TypeScript**, `@typescript-eslint/parser`. Mientras que el mundo de las reglas es un poco más amplio.

Podemos ver a simple vista que el ultimo parámetro del JSON es `rules`, el cual es un objeto que contiene todas las reglas que se van a aplicar en el proyecto y en el que nos centraremos la mayor parte del tiempo configurando en un proyecto. Sin embargo, las reglas no se quedan solo en ese objeto, sino que también se pueden configurar en los campos `extends` y `plugins`, que definen conjuntos de reglas predefinidas para el proyecto.

#### Reglas

Las reglas son las que nos permiten definir el comportamiento de ESLint en nuestro proyecto. Por ejemplo, si queremos que ESLint nos arroje un error cuando no se utilice alguna variable en nuestro código, podemos configurar la siguiente regla en el objeto `rules`:

```js
rules: {
  '@typescript-eslint/no-unused-vars': 'error'
}
```

Estos solo es un ejemplo, ya que justo esa configuración viene dada por defecto en los proyectos de NestJS. Sin embargo, es un claro ejemplo de cómo se configuran las reglas. En este caso, la regla se llama `@typescript-eslint/no-unused-vars` y el valor que se le asigna es `error`, lo que significa que si no se utiliza alguna variable en nuestro código, ESLint nos arrojará un error. Si lo que queremos es que no nos arroje un error, sino que solo nos de un aviso, podemos cambiar el valor de `error` por `warn`. De esta manera, si no se utiliza alguna variable en nuestro código, ESLint nos arrojará un aviso en lugar de un error.

Una posible configuración de reglas para un proyecto de NestJS simple podría ser la siguiente:

```js
rules: {
  '@typescript-eslint/interface-name-prefix': 'off',
  '@typescript-eslint/explicit-function-return-type': 'off',
  '@typescript-eslint/explicit-module-boundary-types': 'off',
  '@typescript-eslint/no-explicit-any': 'off',

  '@typescript-eslint/no-unused-vars': 'warn',  // Advierte sobre variables no utilizadas en lugar de marcarlas como errores
  '@typescript-eslint/no-empty-function': 'warn',  // Advierte sobre funciones vacías en lugar de marcarlas como errores

  'prettier/prettier': ['error', { endOfLine: 'auto' }], // Evita el error: Delete `␍`eslintprettier/prettier error
},
```

Claramente hay cientos de reglas diferentes para poder modificar y hacer su proyecto más o menos estricto, los invito a que investiguen y puedan crear su propio archivo de configuración de ESLint que, luego, puedan reutilizar en todos sus proyectos. 

### Configuración del Framework de Testing

En este caso, vamos a dar el ejemplo con Jest, ya que es el framework de testing que viene por defecto en los proyectos de NestJS. Sin embargo, la configuración es muy similar para otros frameworks de testing.

La misma se encuentra en el `package.json`, en el campo `jest`. En él podemos configurar muchas cosas, pero nos vamos a centrar en las más importantes.

```js
"jest": {
  "moduleFileExtensions": [
    "js",
    "json",
    "ts"
  ],
  "rootDir": "src",
  "testRegex": ".*\\.spec\\.ts$",
  "transform": {
    "^.+\\.(t|j)s$": "ts-jest"
  },
  "collectCoverageFrom": [
    "**/*.(t|j)s"
  ],
  "coverageDirectory": "../coverage",
  "testEnvironment": "node"
}
```

Esta configuración podríamos dejarla tal cual si se van a realizar los tests de la manera en que se recomienda en la documentación de NestJS. Sin embargo, si queremos ubicar los archivos de tests en la carpeta `test` y no en la carpeta `src`, debemos modificarlo en varios aspectos, no solo para que tome los archivos de tests de la carpeta `test`, sino también para que tome los archivos de código de la carpeta `src` para realizar bien tareas como calcular la cobertura de los tests. Una posible configuración para esto sería la siguiente:

```js
"jest": {
  "moduleFileExtensions": [
    "js",
    "json",
    "ts"
  ],
  "rootDir": ".",
  "testMatch": [
    "<rootDir>/test/**/*.spec.ts",
    "<rootDir>/src/**/*.spec.ts"
  ],
  "transform": {
    "^.+\\.(t|j)s$": "ts-jest"
  },
  "collectCoverageFrom": [
    "src/**/*.{ts,js}",
    "!src/**/*.module.ts",
    "!src/main.ts"
  ],
  "coverageDirectory": "./coverage",
  "testEnvironment": "node",
  "moduleNameMapper": {
    "^src/(.*)$": "<rootDir>/src/$1"
  }
}
```

Con esta configuración de Jest podremos tener tests tanto en la carpeta `test` como en la carpeta `src` y que Jest los tome a todos. Además, podremos calcular la cobertura de los tests de manera correcta con el comando `npm run test:cov`, sin que molesten archivos cuyo testeo se ve innecesario en la mayoría de los casos como los son `main.ts` y todos los archivos `.module` que lo único que realizan es configurar los diferentes módulos del proyecto.

### Configuración de Prettier

Prettier es una herramienta que permite formatear el código fuente de una manera más legible y ordenada. Debe estar en armonía con el archivo de configuración de ESLint para que no haya conflictos entre ambos.

Su archivo en formato json, `.prettierrc`, es en donde se configurarán los parámetros necesarios del formateador.

Según la documentación oficial de Prettier, su configuración básica es la siguiente:

``` js
{
  "trailingComma": "es5",
  "tabWidth": 4,
  "semi": false,
  "singleQuote": true
}
```

Claramente modificable, es5 por ejemplo ya es una versión vieja, por lo tanto, yo suelo utilizar la siguiente configuración para mis proyectos de NestJS:

``` js
{
  "trailingComma": "all",
  "tabWidth": 2,
  "semi": false,
  "singleQuote": true,
}
```

## A tener en cuenta

Se pudo ver en este cheat sheet que la creación de un proyecto en NestJS puede ser muy sencilla. Sin embargo, la configuración inicial del mismo termina siendo todo un arte, ya que es una parte muy importante y marcará la diferencia durante el desarrollo del mismo, por lo tanto, es importante que se le dedique tiempo a la configuración inicial del proyecto teniendo una visión a futuro tanto del proyecto como del equipo que lo va a desarrollar para que el mismo sea lo más eficiente posible.

# Resumen
<h1 id="resumen">Resumen</h1>

Pasos a seguir para crear un proyecto rápido y eficiente en NestJS (Modificable según las necesidades y el criterio de cada uno) suponiendo que estamos utilizando VS Code como editor de código y Jest como framework de testing:

1. Ejecutar el comando `nest new <nombre-proyecto> -p npm`
   1. Entrar en la carpeta del proyecto creado con el comando `cd <nombre-proyecto>`
   2. Abrir el editor de código, en este caso VS Code, con el comando `code .`
2. Activar las extensiones a utilizar en el editor de código para el espacio de trabajo del proyecto.
3. En VS Code, presionar `Ctrl + Shift + P` y abrir la opción `Prefences: Open Workspace Settings (JSON)` para pegar el siguiente JSON:
```js
{
  "editor.tabSize": 2
}
```
1. Modificar las reglas en el archivo `.eslintrc.js` para que queden de la siguiente manera:
```js
rules: {
  '@typescript-eslint/interface-name-prefix': 'off',
  '@typescript-eslint/explicit-function-return-type': 'off',
  '@typescript-eslint/explicit-module-boundary-types': 'off',
  '@typescript-eslint/no-explicit-any': 'off',

  '@typescript-eslint/no-unused-vars': 'warn',  // Advierte sobre variables no utilizadas en lugar de marcarlas como errores
  '@typescript-eslint/no-empty-function': 'warn',  // Advierte sobre funciones vacías en lugar de marcarlas como errores

  'prettier/prettier': ['error', { endOfLine: 'auto' }], // Evita el error: Delete `␍`eslintprettier/prettier error
},
```
5. Cambibar la configuración del Prettier si es necesario.
```js
{
  "trailingComma": "all",
  "tabWidth": 2,
  "semi": false,
  "singleQuote": true,
}
```
6. Cambiar los comandos del archivo `package.json` a gusto para una mejor experiencia de desarrollo, en este caso podremos cambiar los siguientes:
```js
"start": "nest start --watch",
"test" : "jest --detectOpenHandles --verbose --passWithNoTests",
```
7. Borrar los archivos innecesarios que se hayan creado si es que los hay.
8. En caso de utilizar Jest como framework de testing, se puede cambiar su configuración a gusto en el archivo `package.json` en la sección `jest`.
   1. En caso de querer realizar los tests en los archivos del código como viene por defecto dejarlo como esta.
   2. En caso de querer ubicar los tests en el directorio `test`, fuera de los archivos del código, se debe cambiar la configuración de la siguiente manera:
    ```js
    "jest": {
      "moduleFileExtensions": [
        "js",
        "json",
        "ts"
      ],
      "rootDir": ".",
      "testMatch": [
        "<rootDir>/test/**/*.spec.ts",
        "<rootDir>/src/**/*.spec.ts"
      ],
      "transform": {
        "^.+\\.(t|j)s$": "ts-jest"
      },
      "collectCoverageFrom": [
        "src/**/*.{ts,js}",
        "!src/**/*.module.ts",
        "!src/main.ts"
      ],
      "coverageDirectory": "./coverage",
      "testEnvironment": "node",
      "moduleNameMapper": {
        "^src/(.*)$": "<rootDir>/src/$1"
      }
    }
    ```
    Esta configuración permite tener los tests en los directorios tanto como `test` y `src` y que Jest los reconozca y los ejecute, incluyendo la cobertura de los mismos con el comando `npm run test:cov`.
9. Instalar Swagger y adaptarlo al proyecto si es necesario.

# Siguiente paso

- Generación de archivos basados en un esquema en NestJS (Proximamente)

<br>

### Datos del cheat sheet
\- Autor: Nicolás Villamonte <br>
\- Fecha: 18/08/2023 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
