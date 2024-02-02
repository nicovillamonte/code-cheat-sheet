# Microservicios en NodeJS con ExpressJS

En este Cheat Sheet se explicará cómo crear microservicios en NodeJS paso a paso desde cero utilizando el FrameWork ExpressJS.

1. [Requerimientos](#requerimientos)
2. [Descripción del Ejemplo](#descripción-del-ejemplo)
3. [Pasos a seguir para la creación de los Microservicios](#pasos-a-seguir-para-la-creación-de-los-microservicios)

- [Datos del Cheat Sheet](#datos-del-cheat-sheet)

## Requerimientos

Para poder seguir este Cheat Sheet es necesario tener instalado **NodeJS** junto con algún manejador de paquetes como NPM. Si no los tienes instalados, puedes descargarlos desde [aquí](https://nodejs.org/en).

Además, es necesario contar con algun editor de código como **Visual Studio Code** para facilitar el proceso de desarrollo. Puedes descargarlo desde [aquí](https://code.visualstudio.com/).

Tener conocimientos en:

- NodeJS y NPM
- JavaScript
- ExpressJS (No se va a explicar cómo realizar una API REST con ExpressJS, solo se va a aprender a configurar microservicios en este framework)

## Descripción del Ejemplo

Vamos a crear, para esta ocasión, tres microservicios sin ningún tipo de contenido, solo para mostrar cómo se configuran y cómo se pueden consumir. Los microservicios serán sencillos, sin referir a nada en específico:

- microservicio1
- microservicio2
- microservicio3

Se utilizará un GateWay para poder consumir los microservicios. El GateWay será un cuarto microservicio que se encargará de redirigir las peticiones a los microservicios correspondientes.

## Pasos a seguir para la creación de los Microservicios

1. En un directorio específico para el proyecto en conjunto, crear un directorio para cada microservicio. Por ejemplo, para el microservicio1:

    ```bash
    mkdir microservicio1 && cd microservicio1
    ```

2. Ingresar al directorio de cada microservicio e inicializar un proyecto de NodeJS con NPM utilizando el siguiente comando:

    ```bash
    npm init -y
    ```

3. Instalar las dependencias necesarias para cada microservicio, en este caso se da un ejemplo de las dependencias base que se necesitarían, pero cada microservicio puede tener sus propias dependencias y dependerá de lo que se necesite en cada uno y del proyecto en general:

    ```bash
    npm i dotenv express cors
    npm i nodemon -D
    ```

    > **Nota:** Los puntos 2 y 3 pueden realizarse en un solo comando de manera rápida en caso de que se quiera hacer todo de una vez. En este caso se podría utilizar: `cd <microservicio> && npm init -y && npm i dotenv express cors && npm i nodemon -D && cd ..`, cambiando _\<microservicio\>_ por el nombre del microservicio correspondiente.

4. Crear los archivos `index.js` con el contenido que sea necesario para cada microservicio. En este caso vamos a crear un archivo `index.js` con el siguiente contenido simple de ejemplo para el microservicio1:

    ```javascript
    import express from 'express';

    const app = express();

    app.get('/', (_, res) => {
        res.send('Hello from microservicio1');
    });

    app.listen(8001, () => {
        console.log('Microservicio1 running on port http://localhost:8001');
    });
    ```

    > Se debe configurar cada microservicio en un puerto diferente. En este caso se configuró el microservicio1 en el puerto 8001, el microservicio2 en el puerto 8002 y el microservicio3 en el puerto 8003.

5. Configurar el proyecto. 
    - Agregar el script para correr el proyecto con `nodemon` en el archivo `package.json`:
    ```json
    "scripts": {
        "start": "nodemon index.js"
    }
    ```
    - Agregar el tipo de módulo en el archivo `package.json` para poder utilizar `import` y `export` en vez de `require` y `module.exports`:
    ```json
    {
        "name": "microservicio1",
        "version": "1.0.0",
        "type": "module"  // <------ Agregar esta línea
        ...
    }
    ```

6. Crear el directorio `gateway` para el Gateway y realizar los pasos 2 y 3 para inicializar el proyecto y agregar las dependencias necesarias, pero esta vez se instalará `express-http-proxy` para poder redirigir las peticiones a los microservicios correspondientes:

    ```bash
    mkdir gateway && cd gateway
    npm init -y
    npm i express cors express-http-proxy
    npm i nodemon -D
    ```

7. Configurar el `package.json` como en los anteriores microservicios y crear el archivo `index.js` del `Gateway` con el siguiente contenido simple de ejemplo para el GateWay:

    ```javascript
    import express from 'express';
    import cors from 'cors';
    import proxy from 'express-http-proxy';

    const app = express();

    app.use(cors());
    app.use(express.json());

    app.use('/microservicio1', proxy('http://localhost:8001'));
    app.use('/microservicio2', proxy('http://localhost:8002'));
    app.use('/microservicio3', proxy('http://localhost:8003'));

    app.use('/', (req, res) => {
        res.send(`
        <h1>Gateway</h1>
        <p>Microservicios:</p>
        <ul>
            <li><a href="/microservicio1">microservicio1</a></li>
            <li><a href="/microservicio2">microservicio2</a></li>
            <li><a href="/microservicio3">microservicio3</a></li>
        </ul>
        `)
    });

    app.listen(8000, () => {
        console.log('Gateway running on port http://localhost:8000');
    });
    ```

    > El puerto en el que se configure el Gateway es el puerto al que se le van a enviar las peticiones para que este las redirija a los microservicios correspondientes.

    > Se puede configurar el nombre de las rutas que se desee, no deben ser específicamente los nombres de los microservicios,.

8. Correr todos los microservicios y el gateway por separado con el comando `npm start` en cada uno de ellos.

9. Probar los microservicios y el gateway en el navegador _(Ingresando al puerto del gateway, en este caso http://localhost/8000)_ o con alguna herramienta como Postman para ver que funcionen correctamente.

10. Comenzar el desarrollo de cada microservicio por separado, agregando las rutas y lógica necesaria para cada uno.

<br>

### Datos del Cheat Sheet

\- Autor: Nicolás Villamonte <br>
\- Fecha: 02/02/2024 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
\- Herramientas y Versiones: Node 18.14.0, npm 9.4.1, Express 4.18.2, Express HTTP Proxy 2.0.0, Nodemon 3.0.3.