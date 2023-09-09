# Directivas - Angular

Las directivas son clases que añaden comportamiento a elementos del DOM. Se utilizan para manipular el DOM, añadir o eliminar elementos, mostrar u ocultar elementos, etc.

Existen 3 tipos de directivas:

- Componentes
- Estructurales
- Atributos

### Tabla de contenidos

1. [Componentes](#componentes)
2. [Directivas Estructurales](#estructurales)
3. [Directivas Atributo](#atributos)
4. [Más información](#mas-informacion)
   
- [Datos del cheat sheet](#cheat-sheet-data)


<h2 id="componentes">Componentes</h2>

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


<h2 id="estructurales">Directivas Estructurales</h2>

Las directivas estructurales son aquellas que manipulan el DOM añadiendo o eliminando elementos. Se utilizan para mostrar u ocultar elementos, repetir elementos, etc.

> Las directivas de este tipo comienzan siempre con un asterisco `*` y se utilizan como atributos de etiquetas del HTML.


Existen varios tipos de directivas estructurales, entre ellas las más utilizadas son:

- `*ngIf`
- `*ngFor`
- `*ngSwitch`

<h3 id="ngif">Directiva *ngIf</h3>

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

<h3 id="ngfor">Directiva *ngFor</h3>

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


<h3 id="ngswitch">Directiva *ngSwitch</h3>
