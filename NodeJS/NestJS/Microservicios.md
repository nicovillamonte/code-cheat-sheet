# Microservicios en NodeJS con NestJS

En este Cheat Sheet se explicará cómo crear microservicios en NodeJS paso a paso desde cero utilizando el FrameWork **NestJS**.

1. [Requerimientos](#requerimientos)
2. [A tener en cuenta](#a-tener-en-cuenta)
3. [Descripción del Ejemplo](#descripción-del-ejemplo)
4. [Pasos a seguir para la creación de los Microservicios](#pasos-a-seguir-para-la-creación-de-los-microservicios)
   1. [Forma no Convencional de configurar el Gateway (NestJS)](#forma-no-convencional-de-configurar-el-gateway-nestjs)
   2. [Forma Convencional de configurar el Gateway (ExpressJS)](#forma-convencional-de-configurar-el-gateway-expressjs)
5. [Correr el Proyecto](#correr-el-proyecto)

- [Datos del Cheat Sheet](#datos-del-cheat-sheet)

## Requerimientos

Para poder seguir este Cheat Sheet es necesario tener instaladas las siguientes herramientas:

- **NodeJS** junto con algún manejador de paquetes como NPM. Si no los tienes instalados, puedes descargarlos desde [aquí](https://nodejs.org/en).
- **NestJS**. Si no lo tienes instalado, puedes instalarlo con el siguiente comando:
    ```bash
    npm i -g @nestjs/cli
    ```
- Algun editor de código como **Visual Studio Code** para facilitar el proceso de desarrollo. Puedes descargarlo desde [aquí](https://code.visualstudio.com/).

Además es necesario contar con conocimientos básicos en las siguientes tecnologías:

- NodeJS y NPM
- NestJS (No se va a explicar cómo realizar una API REST con NestJS, solo se va a aprender a configurar microservicios en este framework)
- TypeScript

## A tener en cuenta

NestJS se destaca de los demás frameworks dentro del área de los microservicios por la implementación de métodos más complejos para la aplicación de los mismos. Por ejemplo, para la creación de microservcios a través de diferentes protocolos de transporte como TCP, Redis, NATS, MQTT, etc. 

Sin embargo, en este Cheat Sheet se va a explicar cómo crear microservicios a través de HTTP, algo que no se encuentra tan claro en la documentación oficial ya que esto se podía hacer con ExpressJS mediante Proxy y NestJS se encuentra basado en ExpressJS, por lo que se dá por hecho que se puede hacer de la misma manera, como se explica en el Cheat Sheet de [Microservicios en NodeJS con ExpressJS](../ExpressJS/Microservicios.md).

De todas maneras, en este Cheat Sheet vamos a hacerlo de una manera más profesional que se adapte más a un proyecto en este framework, una solución más escalable y mantenible.

## Descripción del Ejemplo

Vamos a crear, para esta ocasión, tres microservicios sin ningún tipo de contenido, solo para mostrar cómo se configuran y cómo se pueden consumir. Los microservicios serán sencillos, sin referir a nada en específico:

- microservicio1
- microservicio2
- microservicio3

Se utilizará un **GateWay** para poder consumir los microservicios. El GateWay será un _cuarto microservicio_ que se encargará de redirigir las peticiones a los microservicios correspondientes.

## Pasos a seguir para la creación de los Microservicios

1. Crear los proyectos de NestJS por separado utilizando el comando `nest new <nombre-proyecto>`. Por ejemplo, para el microservicio1:
    ```bash
    nest new microservicio1
    ```
    En este ejemplo terminaremos teniendo una estructura de directorios como la siguiente:
    ```bash
    ├── microservicio1
    │   ├── src
    │   │   ├── main.ts
    │   │   ├── app.controller.ts
    │   │   ├── app.service.ts
    │   │   └── app.module.ts
    │   └── test
    ├── microservicio2
    │   ├── src
    │   │   ├── main.ts
    │   │   ├── app.controller.ts
    │   │   ├── app.service.ts
    │   │   └── app.module.ts
    │   └── test
    ├── microservicio3
    │   ├── src
    │   │   ├── main.ts
    │   │   ├── app.controller.ts
    │   │   ├── app.service.ts
    │   │   └── app.module.ts
    │   └── test
    └── gateway
        ├── src
        │   ├── main.ts
        │   ├── app.controller.ts
        │   ├── app.service.ts
        │   └── app.module.ts
        └── test
    ```
2. Configurar cada microservicio en un puerto diferente. En este caso se configuraría el microservicio1 en el puerto 8001, el microservicio2 en el puerto 8002 y el microservicio3 en el puerto 8003 a modo de ejemplo. Para hacer esto, se debe modificar el archivo `main.ts` de cada microservicio. 

    Por ejemplo, para el microservicio1:
    ```typescript
    import { NestFactory } from '@nestjs/core';
    import { AppModule } from './app.module';

    async function bootstrap() {
      const app = await NestFactory.create(AppModule);
      await app.listen(8001); // Modificar esta línea
    }
    bootstrap();
    ```

3. Para diferenciar cada uno de los microservicios, se puede cambiar el mensaje que se muestra en la ruta raíz de cada uno. Para hacer esto, se debe modificar el archivo `app.service.ts` de cada microservicio. 

    Por ejemplo, para el microservicio1:
    ```typescript
    import { Injectable } from '@nestjs/common';

    @Injectable()
    export class AppService {
        getHello(): string {
            return 'Hello from microservicio1!'; // Modificar esta línea
        }
    }
    ```

### Forma no Convencional de configurar el Gateway (NestJS)

La siguiente no es la mejor manera de configurar el Gateway y se explica por qué en esta sección, si desea ir directamente a la forma convencional de configurar el Gateway, puede hacer click [aquí](#forma-convencional-de-configurar-el-gateway-expressjs).

4. Ir al `gateway` y realizar los siguientes pasos:
   1. Instalar la dependencia `http-proxy-middleware` para poder redirigir las peticiones a los microservicios correspondientes:
        ```bash
        npm i http-proxy-middleware
        ```
   2. Crear el archivo `/src/config/proxy.configuration.ts` y agregar el siguiente contenido:
        ```typescript
        const proxyConfigurations = [
            {
                name: 'microservicio1',
                route: '/api/microservicio1',
                target: 'http://localhost:8001/',
            },
            {
                name: 'microservicio2',
                route: '/api/microservicio2',
                target: 'http://localhost:8002/',
            },
            {
                name: 'microservicio3',
                route: '/api/microservicio3',
                target: 'http://localhost:8003/',
            },
        ];

        export default proxyConfigurations;
        ```
        En este archivo es en donde se configurarán las rutas a los microservicios correspondientes en caso de que se agreguen, eliminen o modifiquen. En una aplicación real, es recomendable reemplazar muchos de los valores por variables de entorno para que sea más escalable y mantenible.
    
    3. Modificar el `bootstrap` del proyecto en el archivo `main.ts` de la siguiente manera:
        ```typescript
        import { NestFactory } from '@nestjs/core';
        import { AppModule } from './app.module';
        import { createProxyMiddleware } from 'http-proxy-middleware';
        import { Request, Response } from 'express';
        import proxyConfigurations from './config/proxy.configuration';

        async function bootstrap() {
            // Se crea la aplicación de NestJS y se configura el prefijo global.
            const app = await NestFactory.create(AppModule);
            const globalPrefix = 'api';
            app.setGlobalPrefix(globalPrefix);

            // Aquí se configura automaticamente la página de inicio del Gateway para que muestre los microservicios disponibles.
            app.use(`/${globalPrefix}`, (req: Request, res: Response, next) => {
                if (req.originalUrl === '/api') {
                res.send(`
                <h1>Gateway</h1>
                <p>Microservicios:</p>
                <ul>
                    ${proxyConfigurations
                        .map(({ route, name }) => `<li><a href="${route}">${name}</a></li>`)
                        .join('')}
                </ul>
                `);
                } else {
                next();
                }
            });

            // Aquí se configuran los proxy a las rutas de los microservicios correspondientes.
            proxyConfigurations.forEach(({ route, target }) => {
                app.use(
                route,
                createProxyMiddleware({
                    target: target,
                    pathRewrite: {
                    [`^${route}`]: '/',
                    },
                    secure: false,
                    changeOrigin: true,
                }),
                );
            });

            // Aquí se configura el puerto en el que correrá el Gateway y se corre el servidor.
            const port = 8000;
            await app.listen(port, () => {
                console.log('Listening at http://localhost:' + port + '/' + globalPrefix);
            });
        }
        ```

        _[Opcional]_ Si se quiere, se puede agregar esta porción de código debajo de la configuración del prefijo para que cuando se ingrese a la url raíz del Gateway, se redirija a la página con el prefijo configurado:

        ```typescript
        app.use((req: Request, res: Response, next) => {
            if (req.originalUrl === '/') {
                res.redirect(globalPrefix);
            } else {
                next();
            }
        });
        ```

Esto ya debería funcionar a la perfección. Para probarlo se debe correr cada uno de los microservicios y el Gateway por separado. Luego, se debe ingresar a la url del Gateway `http://localhost:8000/api` y se va a ver el menú en el que se puede elegir a qué microservicio se quiere redirigir la petición.

Pero ¿Por qué decimos que esta no es la manera convencional de realizarlo?

NestJS se creo con un concepto modularizado de una aplicación, en la que iniciamos con un módulo de aplicación principal que se encuentra conectado a un controlador y un servicio.

```typescript
import { Module } from '@nestjs/common';
import { AppController } from './app.controller';
import { AppService } from './app.service';

@Module({
  imports: [],
  controllers: [AppController],
  providers: [AppService],
})
export class AppModule {}
```

Ahora, con nuestra implementación actual, podemos ver como, si borramos el controlador y el servicio, el proyecto sigue funcionando en su totalidad.

```typescript
import { Module } from '@nestjs/common';

@Module({
  imports: [],
  controllers: [],
  providers: [],
})
export class AppModule {}
```

Esto le quita el sentido a la modularización de la aplicación y, por lo tanto, le quita sentido a utilizar NestJS para programar el Gateway de nuestra aplicación.

Si fuiste lo suficientemente observador, te habrás dado cuenta de que nuestro GateWay actualmente esta configurado como se configura un proyecto en ExpressJS sin utilizar NestJS. Por lo tanto, es una recomendación personal desarrollar el GateWay, que no es más que una aplicación sencilla que redirige las peticiones, con ExpressJS u otro framework que se adapte mejor a este tipo de aplicaciones que NestJS.

### Forma Convencional de configurar el Gateway (ExpressJS)

La forma convencional de configurar el Gateway es muy parecida a la que se explica en el Cheat Sheet de Microservicios en NodeJS con ExpressJS. Por lo que se recomienda leer los pasos de ese [Cheat Sheet desde el **Punto 6**](../ExpressJS/Microservicios.md/#pasos-a-seguir-para-la-creación-de-los-microservicios) para entender cómo vamos a modificar ese mismo gateway para que sea más automático, escalable y mantenible.

4. Creación y desarrollo del `Gateway`.
   1. Crear el directorio `gateway` para el Gateway y realizar los pasos 2 y 3 para inicializar el proyecto y agregar las dependencias necesarias, pero esta vez se instalará `express-http-proxy` para poder redirigir las peticiones a los microservicios correspondientes:

        ```bash
        mkdir gateway && cd gateway
        npm init -y
        npm i express cors express-http-proxy
        npm i nodemon -D
        ```
        
        Y configurar su package.json:
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
   2. Crear el archivo `/src/config/proxy.configuration.ts` y agregar el siguiente contenido:
        ```typescript
        const proxyConfigurations = [
            {
                name: 'microservicio1',
                route: '/microservicio1',
                target: 'http://localhost:8001/',
            },
            {
                name: 'microservicio2',
                route: '/microservicio2',
                target: 'http://localhost:8002/',
            },
            {
                name: 'microservicio3',
                route: '/microservicio3',
                target: 'http://localhost:8003/',
            },
        ];

        export default proxyConfigurations;
        ```
        En este archivo es en donde se configurarán las rutas a los microservicios correspondientes en caso de que se agreguen, eliminen o modifiquen. En una aplicación real, es recomendable reemplazar muchos de los valores por variables de entorno para que sea más escalable y mantenible.
    3. Crear el archivo `index.js` del `Gateway` con el siguiente contenido simple de ejemplo para el GateWay:
        ```javascript
        import express from 'express';
        import cors from 'cors';
        import proxy from 'express-http-proxy';
        import proxyConfigurations from './config/proxy.configuration.js';

        const app = express();

        app.use(cors());
        app.use(express.json());

        let globalPrefix = '/api';

        proxyConfigurations.forEach((proxyConfiguration) => {
            app.use(globalPrefix + proxyConfiguration.route, proxy(proxyConfiguration.target));
        });

        app.use('/', (req, res) => {
            res.send(`
            <h1>Gateway</h1>
            <p>Microservicios:</p>
            <ul>
                ${
                    proxyConfigurations.map((proxyConfiguration) => {
                        return `<li><a href="${globalPrefix}${proxyConfiguration.route}">${proxyConfiguration.name}</a></li>`;
                    }).join('')
                }
            </ul>
            `)
        });

        let port = 8000;
        app.listen(port, () => {
            console.log(`Gateway running on port http://localhost:${port}`);
        });
        ```

        En este archivo se configuran los proxy a las rutas de los microservicios correspondientes. Se utiliza `express-http-proxy` para redirigir las peticiones a los microservicios correspondientes. Se configura el puerto en el que correrá el Gateway y se corre el servidor.

        Vemos una gran diferencia en este Gateway, ya que una vez que lo tenemos configurado, si queremos agregar, eliminar o modificar los microservicios, solo debemos modificar al archivo `proxy.configuration.js` y el código que desarrollamos se encargará de todo lo demás.


## Correr el Proyecto

Para correr el proyecto, se debe correr cada uno de los microservicios y el Gateway por separado con el comando `npm start` o `npm run start:dev` en cada uno de ellos.

Luego, se debe ingresar a la url del Gateway `http://localhost:8000/api` y se va a ver el menú en el que se puede elegir a qué microservicio se quiere redirigir la petición.

Veremos que si ingresamos a `http://localhost:8000/api/microservicio1`, se verá el mismo contenido que si ingresamos a `http://localhost:8001` directamente y lo mismo para los demás microservicios.

Si se quiere, se puede dar de baja alguno de los microservicios para ver que el Gateway redirige correctamente a los microservicios que siguen en pie y que si se ingresa a un microservicio que no está corriendo, se va a mostrar un error.


<br>

### Datos del Cheat Sheet

\- Autor: Nicolás Villamonte <br>
\- Fecha: 07/02/2024 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
\- Herramientas y Versiones: Node 20.11.0, npm 9.4.1, Nest 10.3.1, Express 4.18.2, Express HTTP Proxy 2.0.0, Nodemon 3.0.3.