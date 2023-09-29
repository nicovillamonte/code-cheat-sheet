# Routing - Angular

Angular es un SPA (Single Page Application), por lo que toda la aplicación es una sola página que se va modificando dinámicamente. Para poder navegar entre las distintas páginas de la aplicación, Angular utiliza un sistema de rutas integrado en el framework, el cuál vamos a aprender a configurar y utilizar en esta sección.

1. [Introducción](#introducción)
2. [Conceptos Básicos](#conceptos-básicos)
   - [Módulo Router](#módulo-router)
   - [Configuración de rutas](#configuración-de-rutas)
   - [Router Outlet](#router-outlet)
   - [Redirección de rutas](#redirección-de-rutas)
   - [Rutas no existentes (404)](#rutas-no-existentes-404)
   - [Control de Navegación con UI](#control-de-navegación-con-ui)
     - [Data Binding](#data-binding)
   - [Identificación de la ruta activa](#identificación-de-la-ruta-activa)
3. [Conceptos Intermedios](#conceptos-intermedios)
   - [Rutas Anidadas](#rutas-anidadas)
   - [Rutas dinámicas (Paramétros)](#rutas-dinámicas-paramétros)
   - [Rutas dinámicas con data binding](#rutas-dinámicas-con-data-binding)
   - [Router en el Modelo](#router-en-el-modelo)
     - [Navegación](#navegación)
     - [Redirección](#redirección)
     - [Obtener la ruta actual](#obtener-la-ruta-actual)
     - [Obtener el estado de la ruta actual](#obtener-el-estado-de-la-ruta-actual)
4. [Conceptos Avanzados](#conceptos-avanzados)

- [Datos del Cheat Sheet](#datos-del-cheat-sheet)

> Al momento de escribirse este Cheat Sheet, Angular se encuentra en su versión 16. Por lo que debe estar ateneto a posibles cambios en futuras versiones.

# Introducción

En principio, cuando creamos la aplicación con el CLI de Angular debemos indicar que queremos utilizar el routing de la aplicación. Para ello, debemos agregar el flag `--routing` al comando `ng new`:

```bash
ng new my-app --routing
```

Si se quiere agregar ruteo a una aplicación ya existente, se puede agregar el módulo de ruteo con el comando:

```bash
ng generate module app-routing --flat --module=app
```

O, también, se puede importar directamente el módulo `RouterModule` en el módulo principal de la aplicación:

```typescript
import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';

const routes: Routes = [];

@NgModule({
  ...
  imports: [RouterModule.forRoot(routes)],
  ...
})
export class AppModule { }
```

Esta es la forma manual en la que agregamos el módulo de ruteo a nuestra aplicación.

> *IMPORTANTE*: En este Cheat Sheet vamos a estar utilizando la versión en la que se agrega el módulo de ruteo al crear la aplicación con el CLI de Angular. Lo que va a crear un módulo a parte que se importa directamente en el módulo principal de la aplicación.

# Conceptos básicos

## Módulo Router

Para utilizar rutas en Angular nos viene declarado el módulo `AppRoutingModule` en el módulo principal de la aplicación. Este módulo se encuentra en el archivo `app-routing.module.ts`.

App Module
```typescript
import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';


@NgModule({
  declarations: [
    AppComponent
  ],
  imports: [
    BrowserModule,
    AppRoutingModule
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule { }
```

Si ingresamos al archivo en el que se encuentra declarado el módulo `AppRoutingModule`, podemos ver que tenemos una constante `routes` que es un arreglo de objetos de tipo `Route`. Cada uno de estos objetos representa una ruta de la aplicación y es aquí donde se van a tener que configurar.

```typescript
import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';

const routes: Routes = []; // Configuración de rutas

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
```

## Configuración de rutas

Para configurar las rutas de la aplicación, debemos agregar objetos de tipo `Route` al arreglo `routes` del módulo `AppRoutingModule`. Cada uno de estos objetos representa una ruta de la aplicación y tiene la siguiente estructura:

```typescript
{
  path: 'ruta',
  component: Componente
}
```

Esta sería la configuración más básica de una ruta. Y podemos tener como ejemplo la siguiente configuración:

```typescript
const routes: Routes = [
  {
    path: 'home',
    component: HomeComponent
  },
  {
    path: 'about',
    component: AboutComponent
  }
];
```

Con esta configuración cuando la ruta de la aplicación sea `{url}/home`, se va a cargar el componente `HomeComponent` y cuando la ruta sea `{url}/about`, se va a cargar el componente `AboutComponent`. Componentes que deben existir en la aplicación y ser importados en el módulo de ruteo.

## Router Outlet

Para utilizar esta configuración, debemos agregar el componente `router-outlet` en el HTML del componente principal de la aplicación. Este componente es el que se va a encargar de cargar los componentes que correspondan según la ruta de la aplicación.

```html
<router-outlet></router-outlet>
```

Además, podemos agregar HTML antes o después del componente `router-outlet` para que se muestre en todas las rutas de la aplicación.

```html
<router-outlet></router-outlet>
<footer>Este es el footer de la app</footer>
```

Con ésto, no importa en que ruta estemos, el footer se va a mostrar siempre por debajo del contenido de la ruta. Esto sería lo mismo que hacer el siguiente código en la ruta `/home` por ejemplo:

```html
<app-home></app-home>
<footer>Este es el footer de la app</footer>
```

Y para la ruta `/about` cambiaría al siguiente:

```html
<app-about></app-about>
<footer>Este es el footer de la app</footer>
```

Es decir, el componente `router-outlet` es la posición del HTML en la que se va a mostrar el debido componente según la ruta.

## Redirección de rutas

En caso de que queramos redirigir a otra ruta cuando el usuario ingrese a otra ruta en particular, podemos utilizar la propiedad `redirectTo` en la configuración de la ruta. Por ejemplo, si queremos que cuando el usuario ingrese a la ruta `/` sea redirigido a la ruta `/home` automáticamente, podemos hacer lo siguiente:

```typescript
const routes: Routes = [
  {
    path: '',
    redirectTo: '/home',
    pathMatch: 'full'
  },
  {
    path: 'home',
    component: HomeComponent
  },
  {
    path: 'about',
    component: AboutComponent
  }
];
```

En este caso, cuando el usuario entre a la url de la página, en este caso `http://localhost:4200`, va a ser redirigido a la ruta `http://localhost:4200/home` automáticamente. El atributo `pathMatch` es necesario para indicarle a Angular que la ruta debe coincidir exactamente con el path que le pasamos como argumento. Se puede ver en la [documentación de Angular](https://angular.io/api/router/Route#pathMatch) para entender mejor el funcionamiento de este atributo.


## Rutas no existentes (404)

En caso de que el usuario ingrese a una ruta que no existe en la aplicación, podemos configurar un componente por defecto que suele relacionarse a un **error 404**. Para ello, debemos agregar una ruta con el path `**` al final del arreglo de rutas. Por ejemplo:

```typescript
const routes: Routes = [
  {
    path: '',
    redirectTo: '/home',
    pathMatch: 'full'
  },
  {
    path: 'home',
    component: HomeComponent
  },
  {
    path: 'about',
    component: AboutComponent
  },
  {
    path: '**',
    component: NotFoundComponent
  }
];
```

Como ejemplo, si intentamos ingresar a la ruta `http://localhost:4200/profile` vamos a ver renderizado el componente `NotFoundComponent`, sin redirecciones de la ruta, indicando al usuario que la ruta a la que quiere ingresar no existe.

## Control de Navegación con UI

Para poder navegar entre las distintas rutas de la aplicación utilizando componentes de la vista (botones, links, etc), debemos utilizar la directiva `routerLink` que nos provee Angular. La misma se utiliza como atributo de una etiqueta HTML y recibe como argumento la ruta a la que queremos navegar.

```html
<nav>
  <ul>
    <li>
      <a routerLink="/home">Home</a>
    </li>
    <li>
      <a routerLink="/about">About</a>
    </li>
  </ul>
</nav>
```

Veremos que estamos reemplazando la propiedad `href` por la directiva `routerLink`. Esto se debe a que el href redirige a la url que le pasamos como argumento, mientras que la directiva `routerLink` utiliza el módulo de ruteo de Angular para navegar entre las distintas rutas de la aplicación. 

> **IMPORTANTE**: Si se utiliza `href`, la aplicación se va a recargar en cada navegación, lo que hará perder a Angular su propiedad de **SPA** (Single Page Application), lo que puede desencadenar problemas de performance, ya que cada vez que se navega cargaría nuevamente todo el proyecto Angular en vez de solamente renderizar la nueva página.

### Data Binding

La directiva `routerLink` también nos permite utilizar data binding para poder navegar a rutas dinámicamente. Por ejemplo, si tenemos un arreglo de rutas en el componente:

```typescript
export class AppComponent {
  routes = ['home', 'about'];
}
```

Podemos utilizar un `*ngFor` para mostrar un link por cada ruta del arreglo:

```html
<nav>
  <ul>
    <li *ngFor="let route of routes">
      <a [routerLink]="[route]">{{ route }}</a>
    </li>
  </ul>
</nav>
```

Más adelante vamos a volver al data binding de esta directiva cuando necesitemos pasar parámetros a las rutas. Pero por ahora logramos ver que podemos navegar entre las distintas rutas de la aplicación utilizando la directiva `routerLink` y que podemos utilizar data binding para hacerlo dinámicamente y majorar la calidad del código.

## Identificación de la ruta activa

Cuando tenemos un menú de navegación, usualmente debemos tener un estilo diferente para el link de la ruta en la que nos encontramos. Para esto, Angular nos provee la directiva `routerLinkActive` que nos permite agregar una clase CSS a un elemento HTML cuando la ruta que se está mostrando es la que le pasamos como argumento.

Siguiendo con el ejemplo anterior, pero sin utilizar data binding para simplificar el ejemplo, el código quedría de la siguiente manera:

```html
<nav>
  <ul>
    <li>
      <a routerLink="/home" routerLinkActive="active">Home</a>
    </li>
    <li>
      <a routerLink="/about" routerLinkActive="active">About</a>
    </li>
  </ul>
</nav>
```

Cuando la ruta actual sea `/home`, se le va a agregar la clase `active` al elemento HTML que coincida con la url en la directiva `routerLink`. En este caso, el link de Home va a tener la clase `active` y el de About no.

El ejemplo se puede simplificar aún más si utilizamos data binding para el atributo `routerLink`:

```html
<nav>
  <ul>
    <li *ngFor="let route of routes">
      <a [routerLink]="[route]" routerLinkActive="active">{{ route }}</a>
    </li>
  </ul>
</nav>
```

# Conceptos Intermedios

## Rutas Anidadas

Podemos tener rutas anidadas en nuestra aplicación. Esto quiere decir que podemos tener rutas que dependan de otras rutas. Por ejemplo, si tenemos una ruta `/about` y queremos que tenga una subruta `/about/contact`, podemos hacer lo siguiente:

```typescript
const routes: Routes = [
  ...
  {
    path: 'about',
    component: AboutComponent,
    children: [
      {
        path: 'contact',
        component: ContactComponent
      }
    ]
  },
  ...
];
```

Con el argumento `children` (De tipo `Routes`) podemos agregar un arreglo de rutas que van a ser rutas hijas de la ruta padre. En este caso, cuando la ruta sea `/about/contact`, se va a cargar el componente `ContactComponent` dentro del componente `AboutComponent`. Es decir, el componente `AboutComponent` va a ser el componente padre del componente `ContactComponent`.

Esto quiere decir que en el HTML del componente `AboutComponent` debemos agregar el componente `router-outlet` para que se muestre el componente `ContactComponent` cuando la ruta sea `/about/contact`:

```html
<h1>About</h1>
<router-outlet></router-outlet>
```

## Rutas dinámicas (Paramétros)

Las rutas dinámicas son aquellas rutas que pueden recibir parámetros. Por ejemplo, si tenemos una ruta `/user` y queremos que reciba un parámetro `id` para mostrar el perfil de un usuario en particular, podemos configurar las mismas de la siguiente manera:

```typescript
const routes: Routes = [
  ...
  {
    path: 'user/:id',
    component: UserComponent
  },
  ...
];
```

Cuando se ingresan los dos puntos en un path, se está indicando que ese path va a recibir un parámetro. En este caso, el path va a recibir un parámetro `id` que va a ser utilizado por el componente `UserComponent` para mostrar el perfil del usuario con ese `id`.

En este ejemplo, podemos tener diferentes rutas como las siguientes:

- `/user/1`
- `/user/8743b52063cd84097a65d1633f5c74f5`
- `/user/eduardkofhner`

Estos son ejemplos de formatos en los que se puede recibir el argumento para este caso, la desición queda en el desarrollador y va a variar por cada situación a la que se deba enfrentar.

Para poder acceder al parámetro `id` desde el componente, debemos importar el módulo `ActivatedRoute` e inyectarlo en dicho componente.

```typescript
constructor(private route: ActivatedRoute) { }
```

o se puede inyectar mediante la nueva funcionalidad de Angular desde la version 14:

```typescript
private route = inject(ActivatedRoute)
```

Ahora, con el módulo inyectado, podemos acceder al parámetro `id` de la ruta de la siguiente manera:

```typescript
id = this.route.snapshot.params.id;
```

o de la siguiente manera:

```typescript
this.route.paramMap.subscribe(params => {
  var id = params.get('id');
  console.log(id);
});
```

Por lo tanto, el código del componente `UserComponent` podría quedar aproximadamente de la siguiente manera:

```typescript
import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';

@Component({
  ...
})
export class UserComponent implements OnInit {

  private route = inject(ActivatedRoute)

  ngOnInit(): void {
    let id = this.route.snapshot.params.id;

    console.log(id);
  }
}
```

### Rutas dinámicas con data binding

Volvemos, como prometimos, a visitar al data binding de la directiva `routerLink`. En este caso, podemos utilizar data binding para pasarle el parámetro a la ruta dinámica. Por ejemplo, si tenemos un arreglo de usuarios en el componente:

```typescript
export class AppComponent {
  users = [
    {
      id: 1,
      name: 'Eduard'
    },
    {
      id: 2,
      name: 'Juan'
    },
    {
      id: 3,
      name: 'Pedro'
    }
  ];
}
```

Podemos utilizar un `*ngFor` para mostrar un link por cada usuario del arreglo:

```html
<nav>
  <ul>
    <li *ngFor="let user of users">
      <a [routerLink]="['/user', user.id]">{{ user.name }}</a>
    </li>
  </ul>
</nav>
```

De esta manera, cuando el usuario haga click en el link del usuario, va a ser redirigido a la ruta `/user/{id}` donde `id` es el id del usuario que se le pasó como argumento a la ruta.

Los parámetros pueden ser opcionales, por lo que podemos llamar a dichas rutas con el `routerLink` de la siguiente manera:

```html
<a [routerLink]="['/user', user.id, { user.active : 'active' }]">{{ user.name }}</a>
```

De esta manera, si el usuario que se esta pidiendo tiene la propiedad `active` en `true`, se va a agregar el parámetro `active` a la ruta. Por lo que la ruta podría quedar como `/user/1/active` o `/user/1` dependiendo de la propiedad `active` del usuario.

## Router en el Modelo

El router también puede ser utilizado en el modelo de la aplicación. Para ello, debemos importar el módulo `Router` e inyectarlo en el componente. El mismo tiene varios métodos que pueden utilizarse, vamos a abordar de manera resumida los más importantes.

### Navegación

La navegación puede ser útil cuando se quiere realizar una acción antes de navegar a otra ruta o para cuando la navegación hacia otra ruta sea condicionada. Para navegar a otra ruta, podemos utilizar el método `navigate` del módulo `Router`. Por ejemplo:

```typescript
this.router.navigate(['/home']);
```

Por ser el primer ejemplo vamos a ver cómo quedaría el modelo del componente completo con la navegación a la ruta `/home`:

```typescript
import { Component } from '@angular/core';
import { Router } from '@angular/router';

@Component({
  ...
})
export class AppComponent {

  private router = inject(Router)
  
  navigateToHome() {
    // Se pueden realizar acciones o validaciones antes de navegar
    this.router.navigate(['/home']);
  }
}
```

El método `navigateToHome` podría ser asignado a un botón en el HTML del componente para que cuando el usuario haga click sea redirigido a la ruta `/home`, pudiendo realizar cualquier acción previamente.

Otro ejemplo es la redirección en caso de que algo fallé, si no falla queremos permanecer en la misma página, mientras que si falla queremos enviar al usuario a una página de error:

```typescript
import { Component } from '@angular/core';
import { Router } from '@angular/router';

@Component({
  ...
})
export class AppComponent {

  private router = inject(Router)
  
  makeSomething() {
    try {
      // Hacer algo
    } catch (error) {
      this.router.navigate(['/error']);
    }
  }
}
```

Esto es a lo que nos referíamos cuando decíamos que la navegación estaba condicionada, no podemos asignarle la directiva `routerLink` a el botón de este ejemplo porque redirigiría siempre, en este caso queremos que redirija solo si existe un error. Por eso se programa esta funcionalidad desde el modelo del componente.

### Redirección

La redirección es similar a la navegación, pero en este caso se utiliza el método `navigateByUrl` del módulo `Router`. Por ejemplo:

```typescript
this.router.navigateByUrl('/home');
```

La diferencia entre este método y el anterior es que este recibe como argumento la url completa de la ruta a la que queremos redirigir, mientras que el anterior recibe un arreglo de strings que representan la ruta. Por lo que el método anterior es más flexible y se puede utilizar para rutas dinámicas, mientras que este es más simple y se utiliza para rutas estáticas.

### Obtener la ruta actual

Para obtener la ruta actual, podemos utilizar el método `url` del módulo `Router`. Por ejemplo:

```typescript
let currentUrl = this.router.url;
```

Este método nos va a devolver un string con la ruta actual de la aplicación. Por ejemplo, si la ruta actual es `/home`, el método nos va a devolver el string `/home`.

### Obtener el estado de la ruta actual

Así como hacíamos con la directiva `routerLinkActive` para identificar la ruta activa, podemos utilizar el método `isActive` del módulo `Router` para obtener el estado de la ruta actual. Por ejemplo:

```typescript
let isActive = this.router.isActive('/home', true);
```

Lo que devolverá un booleano indicando si la ruta actual es la que le pasamos como argumento. En este caso, si la ruta actual es `/home`, el método nos va a devolver `true`.


# Conceptos avanzados

Los conceptos avanzados llegan a ser conceptos un poco más complejos de entender a la primera, pero que son muy útiles para ciertas situaciones. Por lo que es recomendable entenderlos y saber que existen para poder utilizarlos cuando sea necesario.

La documentación del tema de rutas en Angular es excesivamente extensa, mientras que la idea de estos Cheat Sheets es que sean lo más resumidos posible y vayan al grano totalmente. Por lo tanto no vamos a ver en éste los conceptos mencionados. Sin embargo, dejaremos una serie de links a la documentación oficial de Angular para que puedan leerlos y entenderlos si así lo desean:

- [Rutas con Módulos y Lazy Loading](https://angular.io/guide/lazy-loading-ngmodules)
- [Guards en Rutas](https://angular.io/guide/router#preventing-unauthorized-access)
- [Interfaz `Navigation`](https://angular.io/api/router/Navigation)
- [Método router.getCurrentNavigation](https://angular.io/api/router/Router#getCurrentNavigation)
- [Parametro `data` del objeto Route](https://angular.io/api/router/Route#data)
- [Y mucho más...](https://angular.io/guide/routing-overview)


Proximamente podrían llegar a existir otros Cheat Sheets que aborden estos temas de manera más profunda.

Navegar detalladamente por la documentación de Angular puede ser de gran ayuda para entender en su totalidad el funcionamiento de las rutas en dicho framework así como cualquier otro tema, por lo que es muy recomendable hacerlo.

<br>

### Datos del Cheat Sheet

\- Autor: Nicolás Villamonte <br>
\- Fecha: 28/09/2023 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
\- Herramientas y Versiones: Angular V16