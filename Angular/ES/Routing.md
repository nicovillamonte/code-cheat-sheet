# Routing - Angular

Angular es un SPA (Single Page Application), por lo que toda la aplicación es una sola página que se va modificando dinámicamente. Para poder navegar entre las distintas páginas de la aplicación, Angular utiliza un sistema de rutas integrado en el framework, el cuál vamos a aprender a configurar y utilizar en esta sección.

1. [Introducción](#introducción)
2. [Módulo Router](#módulo-router)
3. [Configuración de rutas](#configuración-de-rutas)
4. [Router Outlet](#router-outlet)
5. [Control de Navegación con UI](#control-de-navegación-con-ui)


## Introducción

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