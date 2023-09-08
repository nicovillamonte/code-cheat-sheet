# Data Binding - Angular

El data binding es la forma en la que Angular conecta los datos de la aplicación (modelo) con la vista. En este Cheat Sheet se mostrarán todas sus formas de uso con ejemplos y explicaciones.

En el momento en el que se esta escribiendo ésto, Angular esta en su versión 16.

### Tabla de contenidos

1. [Tipos de Data Binding](#introduccion)
2. [Interpolation](#interpolation)
3. [Property Binding](#property-binding)
   1. [Property Binding vs Interpolation](#property-binding-vs-interpolation)
4. [Event Binding](#event-binding)
5. [Two-Way Binding](#two-way-binding)
6. [Más información](#mas-informacion)

- [Datos del cheat sheet](#cheat-sheet-data)

<h2 id="introduccion">Tipos de Data Binding</h2>

Existen 4 formas de hacer data binding en Angular:

- Interpolation
- Property Binding
- Event Binding
- Two-Way Binding

<h2 id="interpolation">Interpolation</h2>

**Modelo → Vista**

La interpolación es la forma más simple de hacer data binding. Se utiliza para mostrar datos en la vista. Se utiliza la sintaxis de doble llave `{{ }}` para indicar que se va a utilizar interpolación.

```html
<h1>{{ title }}</h1>
```

En el ejemplo anterior, se esta utilizando interpolación para mostrar el valor de la variable `title` en la vista. La variable `title` debe estar definida en el componente de la vista.

```typescript
export class AppComponent {
  title = 'My App';
}
```

La interpolación consiste en una sola dirección, desde el **modelo** hacia la **vista**. Si se cambia el valor de la variable `title` en el componente, el valor en la vista también cambiará.

También puede ser utilizada para mostrar el resultado de una expresión.

```html
<h1>{{ title + ' ' + subtitle }}</h1>
```

O de una función.

```html
<h1>{{ getTitle() }}</h1>
```

Otro ejemplo de dónde se puede utilizar la interpolación es en el valor de un atributo de una etiqueta del HTML.

```html
<img src="{{ imageUrl }}" alt="Image">
```

Vemos que su uso es muy simple y que es muy útil para mostrar datos en la vista. Cualquier cosa que se pueda hacer en una expresión de JavaScript, se puede hacer en una interpolación, con la importancia de que debe devolver un valor para mostrarlo en la vista.


<h2 id="property-binding">Property Binding</h2>

**Modelo → Vista**

El property binding es la forma de enlazar una propiedad de un elemento del HTML con una variable del componente. Se utiliza la sintaxis de corchetes `[]` para indicar que se va a utilizar property binding.

```html
<img [src]="imageUrl" alt="Image">
```

En el ejemplo anterior, se esta utilizando property binding para enlazar la propiedad `src` de la etiqueta `img` con la variable `imageUrl` del componente. La variable `imageUrl` debe estar definida en el componente de la vista.

Notese que la variable `imageUrl` se muestra como un string, pero con las extensiones necesarias de Angular en nuestro editor de código deberíamos verlo como una variable de TypeScript, es decir que lo único que cambiaría serían los colores.

Este sería el código en TypeScript para el ejemplo anterior.

```typescript
export class AppComponent {
  imageUrl = 'https://angular.io/assets/images/logos/angular/angular.svg';
}
```

Al igual que el data binding por interpolación, el property binding consiste en una sola dirección, desde el **modelo** hacia la **vista**.

<h3 id="property-binding-vs-interpolation">Property Binding vs Interpolation</h3>

La diferencia entre el property binding y la interpolación es que el property binding se utiliza para enlazar una propiedad de un elemento del HTML con una variable del componente, mientras que la interpolación se utiliza para mostrar datos en la vista.

Es verdad que se puede realizar el siguiente caso de las dos formas:

```html
<button [disabled]="isDisabled">Haz clic en mí</button>
```
o

```html
<button disabled={{ isDisabled }}>Haz clic en mí</button>
```

Sin embargo, la primera es la forma correcta de hacerlo, por varias razones, entre ellas un tema de **Performance**, ya que con interpolación, Angular debe actualizar el atributo HTML para luego reflejar el cambio en el DOM, lo que generaría un re-renderizado absurdo de todo el elemento. Con property binding, Angular modifica directamente la propiedad del elemento en el DOM.

Si se quiere saber más sobre la diferencia entre estos dos ejemplos, es recomendable que lean el artículo[_Binding a propiedad vs interpolación de strings, en Angular_](https://desarrolloweb.com/articulos/binding-propiedad-vs-interpolacion-strings.html)


<h2 id="event-binding">Event Binding</h2>

**Modelo ← Vista**

El event binding es la forma de enlazar un evento o propiedad de un elemento del HTML con una función o variable del componente. Se utiliza la sintaxis de paréntesis `()` para indicar que se va a utilizar event binding.

```html
<button (click)="sayHello()">Haz clic en mí</button>
```

En el ejemplo anterior, se esta utilizando event binding para enlazar el evento `click` de la etiqueta `button` con la función `sayHello()` del componente. La función `sayHello()` debe estar definida en el componente de la vista.

Veremos como la mayoría de los eventos en Angular se escriben de esta manera, a diferencia de HTML puro que utilizaba el prefijo `on` para indicar que se trataba de un evento, por ejemplo `onclick`, `onmouseover`, etc...

Este sería el código en TypeScript para el ejemplo anterior.

```typescript
export class AppComponent {
  sayHello() {
    console.log('Hola mundo!');
  }
}
```

Al igual que el data binding por interpolación y por property binding, el event binding consiste en una sola dirección, pero esta vez desde la **vista** hacia el **modelo**. Es decir, el usuario cambiará el valor de los elementos de la vista y el modelo se actualizará según ese cambio.


<h2 id="two-way-binding">Two-Way Binding</h2>

**Modelo ⟷ Vista**


El **two-way binding** es la forma de enlazar una propiedad de un elemento del HTML con una variable del componente, y a su vez, enlazar esa variable del componente con la misma propiedad del elemento del HTML. Se utiliza la sintaxis de corchetes y paréntesis `[()]` (Llamada _banana in a box_) para indicar que se va a utilizar two-way binding.

```html
<input [(ngModel)]="name">
```

Podemos ver que este ejemplo tiene una particularidad, y es que no se esta utilizando una atributo conocido de HTML, sino que se esta utilizando `ngModel`. Esto es porque `ngModel` es una **directiva de atributo** de Angular que permite hacer two-way binding en elementos del HTML. La misma debe ser importada en el módulo princiapl de la aplicación mediante el FormsModule, sino les arrojará un error.

```typescript
import { FormsModule } from '@angular/forms';

@NgModule({
  imports: [
    FormsModule
  ]
})
```

Lo que sucede en el ejemplo anteriore es básicamente que si el usuario cambia el valor del input, entonces también se cambiará en la variable `name` del componente, y si se cambia el valor de la variable `name` mediante código en el componente, entonces también se cambiará el valor en el input automáticamente.


Vamos a tomar como ejemplo el siguiente HTML:

```html
<input [(ngModel)]="name">
<button (click)="changeName()">Cambiar nombre</button>
```

Con este código TypeScript en el componente:

```typescript
export class AppComponent {
  name = '';

  changeName() {
    console.log(`Nombre actual: ${this.name}`)
    this.name = '';
  }
}
```

Si ejecutamos ese código, podemos ver que si escribimos algo en el Input, al hacer click en el botón, el valor del input se reseteará porque se le asignó un string vacío y se imprimirá en consola el valor que haya sido escrito en el input antes de cambiarlo. Esto se debe a que el two-way binding se encarga de actualizar el valor del input cuando cambia la variable `name` en el componente y viceversa.

El two-way binding es muy útil, sin embargo hay que utilizarlo cuando realmente sea necesario, ya que tiene un costo elevado a comparación con los otros tipos de data binding. Además de que debemos saber evitar funcionalidad que no queremos en nuestro código.


<h2 id="mas-informacion">Más información</h2>

- [Documentación de Angular - Binding Syntax](https://angular.io/guide/binding-syntax)
- [Documentación de Angular - Two Way Binding](https://angular.io/guide/two-way-binding)
- Cheat Sheet de directivas (_proximamente..._)

<br>

<h3 id="cheat-sheet-data">Datos del cheat sheet</h3>

\- Autor: Nicolás Villamonte <br>
\- Fecha: 08/09/2023 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>