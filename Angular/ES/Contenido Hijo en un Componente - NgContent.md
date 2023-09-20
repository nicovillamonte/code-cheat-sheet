# Contenido Hijo en Componentes Angular (`ng-content`)

El contenido hijo suele ser llamado de diferentes maneras en los diferentes frameworks, puede que lo conozcas como `slots` en Vue o `children` en React, pero en Angular se utiliza la directiva `ng-content` para hacer referencia a ello.

1. [Ng-content como contenido hijo](#ng-content-como-contenido-hijo)
2. [Ng-content con selectores](#ng-content-con-selectores)
    1. [Valores opcionales](#valores-opcionales)
    2. [Uso múltiple de un selector](#uso-múltiple-de-un-selector)
3. [Más información](#mas-informacion)

- [Datos del cheat sheet](#cheat-sheet-data)


## Ng-content como contenido hijo <a id="ng-content-como-contenido-hijo"></a>

Los componentes en Angular pueden tener contenido hijo, es decir, que se puede pasar contenido dentro de la etiqueta de un componente, por ejemplo, si tenemos el componente `app-card`, al igual que con muchas otras etiquetas HTML, vamos a poder pasarle contenido dentro de la misma de la siguiente manera:

```html
<app-card>
  <p>Contenido</p>
</app-card>
```

Para poder mostrar el contenido que se pasa dentro de la etiqueta del componente, se debe utilizar la directiva `ng-content` en el template del componente, de la siguiente manera:

```html
<div class="card">
  <div class="card-body">
    <ng-content></ng-content>
  </div>
</div>
```

En donde pongamos la directiva `ng-content` se va a mostrar el contenido que se pasa dentro de la etiqueta del componente. Por lo tanto, realizar lo anterior es lo mismo que hacer lo siguiente:

```html
<div class="card">
  <div class="card-body">
    <p>Contenido</p>
  </div>
</div>
```

Esto nos provee más flexibilidad a la hora de crear componentes reutilizables, ya que podemos pasarle contenido dentro de la etiqueta del componente y mostrarlo en el template del mismo de la manera que lo necesitemos.


## Ng-content con selectores <a id="ng-content-con-selectores"></a>

Otra forma de utilizar la directiva `ng-content` es pasándole un selector, de esta manera, podemos tener más de un `ng-content` en el template del componente y mostrar el contenido que se pasa dentro de la etiqueta del componente en el lugar que queramos, por ejemplo, podemos tener un componente `app-card` con la siguiente estructura:

```html
<div class="card">
  <div class="card-header">
    <ng-content select="[header]"></ng-content>
  </div>
  <div class="card-body">
    <ng-content select="[body]"></ng-content>
  </div>
  <ng-content></ng-content>
</div>
```

Para luego llamarlo de la siguiente manera:

```html
<app-card>
  <p body>Body</p>
  <p header>Header</p>
  <p>Contenido</p>
</app-card>
```

De esta manera, el contenido que se pasa dentro de la etiqueta del componente se va a mostrar en el lugar que le indiquemos con el selector que le pasamos a la directiva `ng-content`. En este caso, el elemento con el selector `[header]` se va a mostrar al inicio de la card, el elemento con el selector `[body]` se va a mostrar segundo, y todo lo demás que no pertenezca a ninguno de los selectores se va a mostrar al final de la card.

### Valores opcionales <a id="valores-opcionales"></a>

Los selectores de `ng-content` son opcionales, es decir, que no es necesario que se pase contenido para todos los selectores que se tengan, de hecho, puede no pasarse ninguno, siguiendo con el ejemplo anterior:

```html
<app-card>
  <p header>Header</p>
  <p>Contenido</p>
</app-card>
```

No llamamos al selector `[body]`, por lo tanto, el contenido no mostrará nada en el lugar en el que se encuentra el `ng-content` con dicho selector.

### Uso múltiple de un selector <a id="uso-múltiple-de-un-selector"></a>

Vimos que el orden era un factor poco importante a la hora de utilizar un componente con selectores, pero ¿qué pasaría si mezclaramos el orden de varios selectores?

```html
<app-card>
  <p header>Header</p>
  <p body>Body</p>
  <p>Contenido</p>
  <p header>Header2</p>
</app-card>
```

En este caso también lo ordenaría, aunque haya más de un elemento utilizando el mismo selector. Entonces el orden quedaría así:

```
Header
Header2
Body
Contenido
```

Lo mismo en caso de que tengamos varios elementos sin etiquetas, todos se van a asignar a la posición en la que se encuentre el `ng-content` sin selector.

```html
<app-card>
  <p>Contenido</p>
  <p header>Header</p>
  <p body>Body</p>
  <p>Contenido2</p>
</app-card>
```


## Más información <a id="mas-informacion"></a>

- [Angular - Proyección de contenido](https://angular.io/guide/content-projection)


<br>

## Datos del cheat sheet <a id="cheat-sheet-data"></a>

\- Autor: Nicolás Villamonte <br>
\- Fecha: 20/09/2023 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
