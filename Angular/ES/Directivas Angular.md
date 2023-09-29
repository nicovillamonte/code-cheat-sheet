# Directivas - Angular

Las directivas son clases que añaden comportamiento a elementos del DOM. Se utilizan para manipular el DOM, añadir o eliminar elementos, mostrar u ocultar elementos, etc.

Existen 3 tipos de directivas:

- Componentes
- Estructurales
- Atributos

En este Cheat Sheet no se van a ver todas las directivas porque son muchas, sino que se van a ver las principales, más utilizadas y más importantes de Angular.

### Tabla de contenidos

1. [Componentes](#componentes)
2. [Directivas Estructurales](#directivas-estructurales)
   1. [Directiva *ngIf](#directiva-ngif)
   2. [Directiva *ngFor](#directiva-ngfor)
   3. [Directiva *ngSwitch](#directiva-ngswitch)
3. [Directivas Atributo](#directivas-atributo)
   1. [Directiva ngModel](#directiva-ngmodel)
   2. [Directiva ngClass](#directiva-ngclass)
   3. [Directiva ngStyle](#directiva-ngstyle)
4. [Más información](#más-información)
   
- [Datos del Cheat Sheet](#datos-del-cheat-sheet)

> Al momento de escribirse este Cheat Sheet, Angular se encuentra en su versión 16. Por lo que debe estar ateneto a posibles cambios en futuras versiones.

## Componentes

Esto pocas personas lo saben, pero los componentes son directivas en Angular. A diferencia de las demás, tienen la peculariedad de que siempre tienen una plantilla asociada a ellos. Se definen por el decorador `@Component` y se utilizan como etiquetas en el HTML.


```typescript
@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent {
  title = 'angular-app';
}
```

> El decorador `@Component` es un decorador `@Directive` extendido con características propias de los templates.


## Directivas Estructurales

Las directivas estructurales son aquellas que manipulan el DOM añadiendo o eliminando elementos. Se utilizan para mostrar u ocultar elementos, repetir elementos, etc.

> Las directivas de este tipo comienzan siempre con un asterisco `*` y se utilizan como atributos de etiquetas del HTML.


Existen varios tipos de directivas estructurales, entre ellas las más utilizadas son:

- `*ngIf`
- `*ngFor`
- `*ngSwitch`

### Directiva *ngIf

La directiva `*ngIf` se utiliza para mostrar u ocultar elementos del DOM. Recibe como argumento una expresión que debe ser evaluada a un valor booleano. Si la expresión es `true`, el elemento se mostrará, si es `false`, el elemento se ocultará.

```html
<p *ngIf="true">Este párrafo se mostrará</p>
```

No se suele utilizar de esta manera, sino que se utiliza una variable del componente que se evalúa a un valor booleano.

Modelo del componente:

```typescript
export class AppComponent {
  showParagraph = true;

  switchParagraph() {
    this.showParagraph = !this.showParagraph;
  }
}
```
Vista del componente:

```html
<p *ngIf="showParagraph">Este párrafo se mostrará</p>

<button (click)="switchParagraph()">Mostrar/Ocultar</button>
```

Con este ejemplo, si presionamos el botón, el valor de la variable `showParagraph` cambiará y el párrafo se mostrará u ocultará según el valor de la variable.

### Directiva *ngFor

Cuando necesitamos mostrar una lista de elementos, en vez de repetir código HTML, podemos utilizar la directiva `*ngFor`. Esta directiva recibe como argumento un arreglo de elementos que se repetirán en el DOM y utilizarán para mostrar la información del mismo.

Entonces pasaríamos de realizar algo como lo siguiente:

```html
<ul>
  <li>{{ items[0] }}</li>
  <li>{{ items[1] }}</li>
  <li>{{ items[2] }}</li>
  <li>{{ items[3] }}</li>
  <li>{{ items[4] }}</li>
</ul>
```
A algo como lo siguiente:

```html
<ul>
  <li *ngFor="let item of items">{{ item }}</li>
</ul>
```

> La directiva `*ngFor` también puede recibir un segundo argumento opcional que es el índice del elemento actual.

```html
<ul>
  <li *ngFor="let item of items, let i = index">{{ i }} - {{ item }}</li>
</ul>
```


### Directiva *ngSwitch

La directiva `*ngSwitch` se utiliza para mostrar un elemento u otro según el valor de una variable. Se utiliza en conjunto con las directivas `*ngSwitchCase` y `*ngSwitchDefault`.

```html
<div [ngSwitch]="color">
  <p *ngSwitchCase="'red'">El color es rojo</p>
  <p *ngSwitchCase="'blue'">El color es azul</p>
  <p *ngSwitchCase="'green'">El color es verde</p>
  <p *ngSwitchDefault>El color es desconocido</p>
</div>
```

En este caso, si la variable color contiene alguno de los valores de los casos, se mostrará el elemento correspondiente. Si no, se mostrará el elemento por defecto marcado por el atributo `*ngSwitchDefault`.


## Directivas Atributo

Las directivas atributo son aquellas que manipulan el DOM  mediante los atributos de los elementos HTML. Se utilizan para añadir clases, estilos, eventos, etc. 

> Las directivas atributo se suelen utilizar mediante [data binding](https://github.com/nicovillamonte/code-cheat-sheet/blob/main/Angular/ES/Data%20Binding.md), por lo que es necesario tener este concepto claro.

Existen varias directivas de este tipo, entre ellas las más utilizadas son:

- `ngModel`
- `ngClass`
- `ngStyle`

### Directiva `ngModel`

La directiva `ngModel` se utiliza para enlazar el valor de un elemento del DOM con una variable del componente. Se utiliza principalmente en los elementos de formulario.

```html
<input type="text" [(ngModel)]="name">
```

Para entender esta dorectiva, es recomendable leer el [Cheat Sheet sobre Data Binding](https://github.com/nicovillamonte/code-cheat-sheet/blob/main/Angular/ES/Data%20Binding.md).

### Directiva `ngClass`

La directiva `ngClass` se utiliza para añadir o eliminar clases de un elemento del DOM. Recibe como argumento un objeto cuyas claves son las clases que se añadirán o eliminarán y cuyos valores son expresiones que deben ser evaluadas a un valor booleano. Si la expresión es `true`, la clase se añadirá, si es `false`, la clase se eliminará.

```html
<div [ngClass]="{ 'red': isRed, 'blue': isBlue }"></div>
```

En este ejemplo, si la variable `isRed` es `true`, se añadirá la clase `red` al elemento, si es `false`, se eliminará. Lo mismo sucede con la clase `blue` y la variable `isBlue`.

También se puede utilizar de la siguiente manera:

```html
<div [ngClass]="isAvailable ? 'active-class' : 'deactivate-class'"></div>
```

### Directiva `ngStyle`

La directiva `ngStyle` se utiliza para añadir o eliminar estilos de un elemento del DOM inline. Recibe como argumento un objeto cuyas claves son los estilos que se añadirán o eliminarán y cuyos valores son expresiones que deben ser evaluadas.

```html
<div [ngStyle]="{ 
    'color': textColor,
    'font-size': fontSize + 'px',
  }"></div>
```

En este ejemplo, la variable `textColor` debe contener un color válido, y la variable `fontSize` debe contener un número. Lo que hará que se pueda cambiar el color y el tamaño de la fuente del elemento dinámicamente.

> **IMPORTANTE**: _No es recomendable utilizar el `ngStyle` por varias razones, pero la más importante es que según la misma documentación de Angular, `ngStyle` ya no proporciona un valor significativo y podría eliminarse en el futuro._


## Más información

- [Documentación de Angular](https://angular.io/docs)
  - [Introduction to components and templates](https://angular.io/guide/architecture-components)
  - [Structural directives](https://angular.io/guide/structural-directives)
  - [Attribute directives](https://angular.io/guide/attribute-directives)
- [Cheat Sheet de Data Binding](https://github.com/nicovillamonte/code-cheat-sheet/blob/main/Angular/ES/Data%20Binding.md)

<br>

### Datos del Cheat Sheet

\- Autor: Nicolás Villamonte <br>
\- Fecha: 09/09/2023 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
\- Herramientas y Versiones: Angular V16