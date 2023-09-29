# Propiedades @Input y @Output - Manejo de datos entre componentes en Angular

Las propiedades `@Input` y `@Output` son decoradores que nos permiten pasar datos entre componentes, de padre a hijo y de hijo a padre respectivamente. Es necesario comprender el concepto de _data binding_ en su totalidad para poder entender el funcionamiento de estas propiedades, ya que las mismas se encargan de crear atriutos personalizados para las etiquetas de componentes que pueden ser utilizados utilizando _data binding_.

1. [Propiedad @Input](#propiedad-input)
   1. [Introducción](#introducción-a-input)
   2. [Data Binding](#data-binding-en-input)
   3. [Ejemplo didáctico](#ejemplo-didáctico-de-input)
      1. [Pasar valor fijo del padre al hijo](#pasar-valor-fijo-del-padre-al-hijo)
      2. [Enlazarlo a una variable del padre](#enlazarlo-a-una-variable-del-padre)
   4. [Problema](#problema)
2. [Propiedad @Output](#propiedad-output)
   1. [Introducción](#introducción-a-output)
   2. [EventEmitter](#eventemitter)
   3. [Ejemplo didáctico](#ejemplo-didáctico-de-output)
3. [Usando @Input y @Output juntos](#usando-input-y-output-juntos)
   1. [Introducción](#introducción-de-uso-doble)
   2. [Ejemplo didáctico](#ejemplo-didáctico-de-uso-doble)
4. [Más información](#más-información)


- [Datos del Cheat Sheet](#datos-del-cheat-sheet)

> Al momento de escribirse este Cheat Sheet, Angular se encuentra en su versión 16. Por lo que debe estar ateneto a posibles cambios en futuras versiones.

## Propiedad @Input

### Introducción a @Input

La propiedad @Input es un decorador que marca un campo de la clase como propiedad de entrada. Tiene dos maneras de escribirse y es importante entender ambas para evitar confusiones:

```ts
export class HijoComponent {
    @Input() propiedad: tipo;
}
```

En esta primera posibilidad de escritura, el decorador no recibe ningún parámetro, por lo que el nombre de la propiedad será el mismo que el nombre del atributo que se le asigne en el componente padre. Por ejemplo, si tenemos un componente hijo llamado `HijoComponent` y un componente padre llamado `PadreComponent`, y en el componente padre tenemos la siguiente etiqueta:

```html
<hijo-component></hijo-component>
```

Entonces, en el componente hijo, la propiedad `propiedad` será `undefined`. Para asignarle un valor, debemos hacerlo de la siguiente manera:

```html
<hijo-component propiedad=valor></hijo-component>
```

Antes de complejizar el ejemplo, vamos a pasar a ver la segunda forma en la que podemos escribir el decorador `@Input`. El único cambio que vamos a ver en la misma es que el decorador recibe un string como argumento, esto nos permite utilizar el nombre de la variable que queramos en el componente en el que estamos trabajando y asignarle un nombre diferente a la propiedad de la etiqueta del componente.

```ts
export class HijoComponent {
    @Input('alias') propiedad: tipo;
}
```

En este caso, no se llamaría al atributo `propiedad` en el componente padre, sino que se llamaría `alias`. Siguiendo con el mismo ejemplo:

```html
<hijo-component alias=valor></hijo-component>
```

### Data Binding en @Input

Como mencionamos anteriormente, la propiedad `@Input` nos permite utilizar _data binding_ para enlazar una variable a la propiedad del componente hijo que se sincronice con la vista en vez de ser un valor fijo como vimos hasta recién. Para ello, debemos utilizar la sintaxis de _data binding_ de Angular, que consiste en utilizar corchetes `[]`, es decir que utilizaremos [property binding](https://github.com/nicovillamonte/code-cheat-sheet/blob/main/Angular/ES/Data%20Binding.md#property-binding). Por ejemplo, si tenemos una variable `valor` en el componente padre, podemos asignarle el valor de la siguiente manera:

TS del componente padre:

```ts
export class PadreComponent {
    valor: tipo;
}
```

HTML del componente padre:

```html
<hijo-component [propiedad]="valor"></hijo-component>
```

Lo que hará que cuando la variable `valor` cambie, la propiedad `propiedad` del componente hijo también lo haga.

### Ejemplo didáctico de @Input

Vamos a realizar un ejemplo en el que vamos a ir escalando en funcionalidad poco a poco. Inicialmente vamos a constar de un componente hijo llamado `HijoComponent` y un componente padre llamado `PadreComponent`. 

#### Pasar valor fijo del padre al hijo

Lo que queremos realizar inicialmente es mostrar en una etiqueta `h1`, dentro del componente hijo, el valor que se le pase como argumento al mismo desde el componente padre. Por ejemplo, la típica frase "Hola mundo". Para ello, vamos a utilizar la primera forma de escribir el decorador `@Input` que vimos anteriormente.

TS del componente hijo, en el que se encuentra definida la propiedad `titulo`:

```ts
export class HijoComponent {
    @Input() titulo: string = 'Titulo default';
}
```

HTML del componente hijo, en el que se muestra la propiedad `titulo` en la vista utilizando la [interpolación de Angular](https://github.com/nicovillamonte/code-cheat-sheet/blob/main/Angular/ES/Data%20Binding.md#interpolation):

```html
<h1>{{ titulo }}</h1>
```

HTML del componente padre, en el que se llama al componente hijo y se le pasa como argumento la propiedad `titulo` y su respectivo valor:


```html
<hijo-component titulo="Hola Mundo"></hijo-component>
```

Si ejecutamos este simple código, vamos a ver como se ve el "Hola Mundo" en el navegador sin ningún problema. Ya hemos logrado pasar datos desde el componente padre al componente hijo utilizando la propiedad `@Input`. Ahora, vamos a agregarle un poco más de funcionalidad.

#### Enlazarlo a una variable del padre

En este caso, vamos a agregar una etiqueta _input_ en el hijo para que el usuario pueda ingresar un valor y que el mismo se muestre en la etiqueta `h1` del componente hijo. Como bonús adicional, vamos a utilizar la segunda forma de escribir el decorador `@Input` que vimos anteriormente.

TS del componente hijo, en el que se encuentra definida la propiedad `titulo`:

```ts
export class HijoComponent {
    @Input('title') titulo: string = 'Titulo default';
}
```

HTML del componente hijo, que sigue totalmente igual:

```html
<h1>{{ titulo }}</h1>
```

TS del componente padre, en el que se encuentra definida la propiedad `inputValue`:

```ts
export class PadreComponent {
    inputValue: string = 'Hola Mundo';
}
```

HTML del componente padre, en el que pasan dos cosas interesantes. La primera es que le vamos a agregar el input requerido, mientras que la segunda es que vamos a utilizar la sintaxis de _data binding_ para enlazar la propiedad `titulo` del componente hijo con la propiedad `inputValue` del componente padre:

```html
<input type="text" [(ngModel)]="inputValue">
<hijo-component [title]="inputValue"></hijo-component>
```

Si ejecutamos este código, vamos a ver como el valor que ingresemos en el input se va a mostrar en la etiqueta `h1` del componente hijo. Esto quiere decir que la propiedad que creamos mediante el decorador `@Input` se enlazó correctamente con el estado del componente padre.

### Problema

Hemos logrado conectar los componentes padre e hijo en una sola dirección actualmente. La dirección `padre -> hijo` sugiere que pudimos modificar una propiedad del componentes hijo desde el comopnente padre, pero el componente hijo no puede modificar el valor de la propiedad del componente padre. Entonces, ¿qué pasaría si el componente hijo tuviera un botón que modificase el valor de la porpiedad `titulo`? ¿Cambiaría en el padre?

Vamos a verlo modificando el mismo ejemplo:

TS del componente hijo, se le agrega la funcion que cambiá el valor de la propiedad `titulo` mediante un botón:

```ts
export class HijoComponent {
    @Input('title') titulo: string = 'Titulo default';

    cambiarTitulo() {
        this.titulo = 'Nuevo titulo';
    }
}
```

HTML del componente hijo, se le agrega un botón que llama a la función `cambiarTitulo()`:

```html
<h1>{{ titulo }}</h1>
<button (click)="cambiarTitulo()">Cambiar titulo</button>
```

Si ejecutamos este código, vamos a ver como el valor de la propiedad `titulo` cambia en el título con etiqueta `h1` que se encuentra en el componente hijo, pero no en el _input_ del componente padre. Esto se debe a que el _data binding_ que realizamos es de una sola dirección, por lo que el componente hijo puede modificar el valor de la propiedad `titulo` del componente padre, pero no al revés.

¿Entonces con utilizar el [binding bidireccional](https://github.com/nicovillamonte/code-cheat-sheet/blob/main/Angular/ES/Data%20Binding.md#two-way-binding) se solucionaría el problema? No, ya que no dimos a entender al componente hijo que puede modificar un valor y hacerlo llegar al componente padre de ninguna manera, por lo que el hecho de solo intentarlo nos arrojaría un error.

## Propiedad @Output

### Introducción a @Output

La propiedad `@Output` es un decorador que marca un campo de la clase como propiedad de salida. Al igual que el decorador `@Input`, se puede escribir con o sin argumento, lo que funciona de la misma manera en ambos casos.

```ts
export class HijoComponent {
    @Output() propiedad: tipo;
}
```

```ts
export class HijoComponent {
    @Output('alias') propiedad: tipo;
}
```

La diferencia entre la propiedad `@Input` y la propiedad `@Output` es que la primera se utiliza para pasar datos desde el componente padre al componente hijo, mientras que la segunda se utiliza para pasar datos desde el componente hijo al componente padre. Para lo último, se utiliza la sintaxis de [_event binding_](https://github.com/nicovillamonte/code-cheat-sheet/blob/main/Angular/ES/Data%20Binding.md#event-binding) de Angular, que consiste en utilizar paréntesis `()`. Por ejemplo, si tenemos una función `funcion` en el componente padre, podemos asignarle el valor de la siguiente manera:

TS del componente padre:

```ts
export class PadreComponent {
    funcion() {
        // Código
    }
}
```

HTML del componente padre:

```html
<hijo-component (propiedad)="funcion()"></hijo-component>
```

Hasta acá todo bien, pero ¿qué se hace con esa función?, ¿cuándo se ejecuta?

### EventEmitter

En Angular, los eventos se representan mediante la clase `EventEmitter`, que es una clase genérica que nos permite emitir eventos de cualquier tipo. En el decorador `@Output`, se utiliza siempre un valor de tipo `EventEmitter` para poder emitir eventos, ya que estamos queriendo pasar datos desde el componente hijo al componente padre cuando los mismos cambien, no tiene sentido hacerlo de otra manera.

En el EventEmitter, se debe especificar el tipo de dato que se va a emitir, por lo que se debe utilizar la clase `EventEmitter` con el tipo de dato que se va a emitir como parámetro. El componente hijo quedaría, como ejemplo genérico, de la siguiente manera:

TS del componente hijo:

```ts
export class HijoComponent {
    @Output() propiedad: EventEmitter<tipo> = new EventEmitter<tipo>();

    emitirEvento() {
        this.propiedad.emit(valor);
    }
}
```

HTML del componente hijo:

```html
<button (click)="emitirEvento()">Botón</button>
```

El objeto EventEmitter tiene un método llamado `emit()` que recibe como parámetro el valor que se va a emitir. Debemos llamar a este método cada vez que queramos que el valor de la propiedad "salga" del componente hijo y llegue al componente padre. En el ejemplo anterior, se llama al método `emit()` cuando se hace click en el botón, por lo que cada vez que se haga click en el botón, se va a emitir el valor que se le pase como parámetro y el componente padre va a ejecutar la función que tenga ligada a la propiedad del componente hijo.

### Ejemplo didáctico de @Output

Vamos a realizar un nuevo ejemplo en el que utilicemos un decorador `@Output` para emitir un evento al componente padre cada vez que se superen los 10 caracteres en un _input_ del componente hijo, para que así el padre pueda mostrar un mensaje de error en consola.

TS del componente hijo, donde esta el `@Output` y la función que emite el evento:

```ts
export class HijoComponent {
    @Output() superoMaximo: EventEmitter<void> = new EventEmitter<void>();

    inputValue = '';

    emitirEvento() {
        if (this.inputValue.length > 10) {
            this.superoMaximo.emit();
        }
    }
}
```

HTML del componente hijo, se le agrega un _input_ que llama a la función `emitirEvento()` por cada vez que se modifica el valor del mismo:

```html
<input type="text" [(ngModel)]="inputValue" (input)="emitirEvento()">
```

TS del componente padre, donde se encuentra la función que se ejecuta cuando se emite el evento:

```ts
export class PadreComponent {
    funcion() {
        console.log('Se superó el máximo de caracteres');
    }
}
```

HTML del componente padre, donde se llama al componente hijo y se le asigna la función `funcion()` a la propiedad `superoMaximo` con el _event binding_:

```html
<hijo-component (superoMaximo)="funcion()"></hijo-component>
```

¿Que hace esto? En sencillas palabras, cada vez que el usuario escriba y haya más de 10 caracteres en el _input_ del componente hijo, se va a emitir un evento que estará enlazado a la función `funcion()` del componente padre y la ejecutará, en este caso solo mostrará un mensaje en consola.


## Usando @Input y @Output juntos

### Introducción de uso doble

Hasta ahora, vimos como utilizar los decoradores `@Input` y `@Output` por separado, pero ¿qué pasaría si los utilizamos juntos? ¿Podríamos pasar el mismo dato desde el componente padre al componente hijo y viceversa? La respuesta es sí, y tiene mucho que ver con el _binding bidireccional_ que generalmente utilizamos con la directiva de Angular `ngModel`.

Sabemos que para que el componente padre pueda envíarle datos al componente hijo mediante atributos de la etiqueta de componente, se debe utilizar el decorador `@Input`, y que para que el componente hijo pueda enviarle datos al componente padre mediante eventos, se debe utilizar el decorador `@Output`. Entonces, por deducción, podríamos utilizar un mismo dato que pueda ser modificado por ambos componentes.

Pero esto tiene una manera muy específica de realizarse, y es que inicialmente debemos darle un nombre a la "entrada" del atributo, por ejemplo:

```ts
export class HijoComponent {
    @Input() propiedad: tipo;
}
```

Y luego, debemos darle el mismo nombre a la "salida" del evento, con el agregado de la palabra `Change`:

```ts
export class HijoComponent {
    @Input() propiedad: tipo;
    @Output() propiedadChange: EventEmitter<tipo> = new EventEmitter<tipo>();
}
```

En este instante, lo más importante es el nombre de la propiedad, no tanto el de la variable, es decir que eso mismo se podría realizar de la siguiente manera y funcionaría correctamente:

```ts
export class HijoComponent {
    @Input('propiedad') variable: tipo;
    @Output('propiedadChange') otraVariable: EventEmitter<tipo> = new EventEmitter<tipo>();
}
```

Angular entiende que ambas propiedades, una de entrada y otra de salida, están relacionadas por el nombre, por lo que activamos, de esta manera, la posibilidad de utilizar el _binding bidireccional_ en la propiedad `propiedad` del componente hijo cuando se lo llama en la vista del componente padre:

```html
<hijo-component [(propiedad)]="variable"></hijo-component>
```

### Ejemplo didáctico de uso doble

Siguiendo con el ejemplo del _input_ del componente padre que maneja el texto del componente hijo que vimos cuando explicamos el decorador `@Input`, vamos a agregarle la posibilidad de que el componente hijo pueda modificar el valor del _input_ del componente padre mediante un botón.

TS del componente hijo, donde se encuentra la propiedad `inputValue` y la función que emite el evento:

```ts
export class HijoComponent {
    @Input() text: string = '';
    @Output() textChange: EventEmitter<string> = new EventEmitter<string>();

    resetText() {
        this.text = 'Texto reseteado';
        this.textEvent.emit(this.text);
    }
}
```

HTML del componente hijo, donde se le agrega un botón que llama a la función `resetText()`:

```html
<p>{{ text }}</p>
<button (click)="resetText()">Resetear texto</button>
```

TS del componente padre, donde tenemos el valor del _input_ que se va a envíar al componente hijo:

```ts
export class PadreComponent {
    inputValue: string = 'Hola Mundo';
}
```

A partir de ahora vamos a poder utilizar el _binding_ en dos formas diferentes cuando en la vista del componente padre llamemos al componente hijo. La primera es la que ya conocemos, que es la de _property binding_, que consta de una sola dirección, del padre al hijo:

```html
<input type="text" [(ngModel)]="inputValue" />
<hijo-component [text]="inputValue"></hijo-component>
```

Esto no es nada nuevo, de hecho lo podemos realizar sin necesidad del `@Output`. Si corremos el ejemplo de esta manera veremos que cuando presionamos el botón se cambiará el valor del texto pero no del _input_ del componente padre. De todas maneras, lo que nos compete ahora mismo es poder utilizar ambos en conjunto, y para ello vamos a utilizar el _binding bidireccional_ con la propiedad creada:

```html
<input type="text" [(ngModel)]="inputValue" />
<hijo-component [(text)]="inputValue"></hijo-component>
```

Ahora, si presionamos el botón, vemos que el componente hijo tiene la capacidad de modificar el valor del _input_ del componente padre mediante el evento que emite. Es decir, ahora se puede modificar el valor del padre al hijo y del hijo al padre (El último mediante eventos).


## Más información

- [Documentación de Angular - Inputs Outputs](https://angular.io/guide/inputs-outputs)
- [Otros Cheat Sheets de Angular](https://github.com/nicovillamonte/code-cheat-sheet/tree/main/Angular/ES)

<br>

### Datos del Cheat Sheet

\- Autor: Nicolás Villamonte <br>
\- Fecha: 16/09/2023 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
\- Herramientas y Versiones: Angular V16