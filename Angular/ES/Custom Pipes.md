# Custom Pipes - Angular

Los pipes son una forma de transformar datos antes de mostrarlos en la vista. Angular provee varios pipes por defecto, pero también es posible crear pipes personalizados, que es lo que vamos a estar abordando en este Cheat Sheet.

Se esta utilizando a la hora de escribir ésto, Angular 16.

### Tabla de contenidos

1. [Creación de un Custom Pipe](#creando-un-pipe)
   1. [Archivo para el pipe](#archivo-para-el-pipe)
   2. [Decorador @Pipe](#decorador-pipe)
   3. [Interfaz PipeTransform](#interfaz-pipetransform)
   4. [Método transform](#metodo-transform)
2. [Utilizando el custom pipe](#usando-el-pipe)
3. [Argumentos en los Custom Pipes](#argumentos-en-los-custom-pipes)
4. [Más información](#mas-informacion)


- [Datos del cheat sheet](#cheat-sheet-data)


<h2 id="creando-un-pipe">Creación de un Custom Pipe</h2>

La creación de un Custom Pipe es muy sencilla y útiles en muchos casos. Por ejemplo, si se quiere agregar una comillas a un texto para que sea como una cita, se puede realizar de la ciguiente manera:

```html
<p>{{ '"' + text + '"' }}</p>
```

Sin embargo, realizar esto por cada texto que queremos citar en toda una página puede ser muy tedioso y el código no se vería muy limpio. Entonces otra manera que podemos aplicar para resolver este problema es creando un método en el componente que se encargue de ésto.

```typescript
export class AppComponent {
  text = 'Lorem ipsum dolor sit amet, consectetur adipiscing elit.';

  quote(text: string): string {
    return '"' + text + '"';
  }
}
```

Entonces el HTML quedaría de la siguiente manera:

```html
<p>{{ quote(text) }}</p>
```

Aunque esto parece una muy buena opción, no es la mejor. Si se quiere utilizar el mismo método en otro componente, se tendría que copiar y pegar el método duplicando código constantemente. Además, si se quiere cambiar la forma en la que se cita el texto, se tendría que cambiar en todos los componentes en los que se esta utilizando el método.

El código HTML puede parecer limpio en esta instancia, sin embargo, una buena práctica sería utilizar la menor cantidad de lógica posible en el HTML. Por lo que la mejor opción sería crear un Custom Pipe, que lo que hace es encapsular la lógica en un solo lugar y se puede utilizar en cualquier componente de la aplicación en Angular.

Este sería el ejemplo anterior utilizando un Custom Pipe:

```html
<p>{{ text | quote }}</p>
```

También podríamos decidir qué tipo de comillas utilizar para citar el texto desde la misma llamada al Pipe:

```html
<p>{{ text | quote:"'" }}</p>
```

<h3 id="archivo-para-el-pipe">Archivo para el pipe</h3>

¿Qué es realmente el Pipe y en dónde vamos a crearlo? Un Pipe, internamente, es una clase que implementa una interfaz que se encarga de convertir el código que escribamos a un Pipe que Angular pueda entender. Por lo que vamos a crear un archivo para el Pipe. Para seguir buenas prácticas en la estructura de un proyecto de Angular, el archivo debería ser nombrado con el nombre del Pipe seguido de un `.pipe` y con la extensión `.ts`. Por ejemplo, si el Pipe se llama `quote`, el archivo debería ser `quote.pipe.ts`.

Los archivos de Pipes se pueden almacenar en un directorio llamado `pipes` dentro de la carpeta `shared` de la aplicación si se van a compartir en toda la aplicación. Es muy común también ubicarlos en el directorio `core` del proyecto. La diferencia entre estas dos opciones es que los pipes en el directorio core generalmente son fundamentales para la lógica de negocio de la aplicación y se cargan una sola vez cuando se inicia la aplicación, mientras que los pipes en el directorio shared son más propensos a ser reutilizables en diferentes partes de la aplicación pero no son esenciales para su funcionamiento básico.

<h3 id="decorador-pipe">Decorador @Pipe</h3>

El decorador @Pipe es el que se encarga de convertir la clase en un Pipe. Este decorador se importa desde `@angular/core`. El mismo recibe un objeto como parámetro con una propiedad llamada `name` que es el nombre que se le va a dar al Pipe. Este nombre es el que se va a utilizar en la vista para invocar al Pipe. Por ejemplo, para que podamos utilizar el pipe de la manera en la que vimos en el ejemplo se le debe asignar el valor `quote` al atributo `name` del objeto que se le pasa al decorador.

Este decorador tiene otros dos argumentos, vamos a verlos todos y sus funciones en la siguiente tabla:

| Argumento | Descripción | Características |
| --- | --- | --- |
| name | Nombre del Pipe. Este es el nombre que se va a utilizar en la vista para invocar al Pipe. | Es obligatorio. Debe ser lowerCamelCase. |
| pure | Si es `true`, el Pipe es puro y se ejecuta solo cuando el valor de sus argumentos cambian. Si es `false`, se invoca en cada ciclo de detección de cambios, incluso si los argumentos no han cambiado. | Es opcional. Por defecto es `true`. |
| standalone | A partir de la v16, los pipes también pueden ser standalone, lo que significa que no necesitan ser declarados en un módulo para ser utilizados en la aplicación. | Es opcional. Por defecto es `false`. |

Generalmente los argumentos `pure` y `standalone` no se suelen cambiar al menos que sea necesario, por lo que el decorador quedaría de la siguiente manera:

```ts
import { Pipe } from '@angular/core';

@Pipe({
  name: 'quote'
})
...
```

<h3 id="interfaz-pipetransform">Interfaz PipeTransform</h3>

Para crear el Pipe dijimos que debíamos implementar una interfaz a la clase del mismo. Esa interfaz se llama `PipeTransform` y se importa desde `@angular/core`. Según la documentación oficial de Angular, esta es la estructura de dicha interfaz:

```ts
interface PipeTransform {
  transform(value: any, ...args: any[]): any
}
```

<h3 id="metodo-transform">Método transform</h3>

La interfaz `PipeTransform` tiene un método llamado `transform` que recibe dos parámetros. El primero es el valor que se va a transformar y el segundo es un array de argumentos que se pueden pasar al Pipe. El método `transform` debe devolver el valor transformado.

Cuando se invoca al Pipe, Angular se encarga de llamar al método `transform` y pasarle el valor que se quiere transformar como primer parámetro. Si se pasan argumentos al Pipe, estos se pasan como segundo parámetro. Lo que devuelva este método es lo que se va a mostrar en la vista como transformación del valor inicial.

Por lo tanto, volviendo a nuestro ejemplo de como crear un Pipe, el código quedaría de la siguiente manera:

```ts
import { Pipe, PipeTransform } from '@angular/core';

@Pipe({
  name: 'quote'
})
export class QuotePipe implements PipeTransform {
  transform(value: string): string {
    return '"' + value + '"';
  }
}
```

<h2 id="usando-el-pipe">Utilizando el custom pipe</h2>

Para poder utilizar nuestro Custom Pipe en la aplicación de Angular, debemos declararlo en el módulo en el que lo vamos a utilizar. Por ejemplo, si lo vamos a utilizar en el módulo principal de la aplicación, el archivo `app.module.ts` debería quedar de la siguiente manera:

```ts
import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { AppComponent } from './app.component';

import { QuotePipe } from './pipes/quote.pipe'; // Se debe importar

@NgModule({
  declarations: [
    AppComponent,
    QuotePipe       // Acá se decalara el Pipe
  ],
  imports: [
    BrowserModule
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule { }
```

Una vez declarado el Pipe en el módulo, ya podemos utilizarlo en cualquier componente de la aplicación. Por ejemplo, si queremos utilizarlo en el componente `app.component.html`, el código quedaría de la siguiente manera:

```html
<p>{{ text | quote }}</p>
```

<h2 id="argumentos-en-los-custom-pipes">Argumentos en los Custom Pipes</h2>

Además de esta funcionalidad, los Custom Pipes pueden recibir argumentos. Por ejemplo, si queremos que el Pipe `quote` reciba como argumento el tipo de comillas que se van a utilizar para citar el texto, el código del Pipe quedaría de la siguiente manera:

```ts
import { Pipe, PipeTransform } from '@angular/core';

@Pipe({
  name: 'quote'
})
export class QuotePipe implements PipeTransform {
  transform(value: string, quoteChar: string = '"'): string {
    return quoteChar + value + quoteChar;
  }
}
```

En este momento podemos utilizar el Pipe de la misma manera que lo utilizabamos antes ya que el argumento es opcional y tiene un valor por defecto. Sin embargo, si queremos utilizar otro tipo de comillas, podemos pasarlas como argumento al Pipe, lo que se realiza escribiendo dos puntos (`:`) y el valor del argumento. Por ejemplo, si queremos utilizar comillas simples, el código quedaría de la siguiente manera:

```html
<p>{{ text | quote:"'" }}</p>
```

Si se quieren utilizar múltiples argumentos, se deben separar por dos puntos (`:`). Por ejemplo, si queremos que el Pipe reciba como argumento el tipo de comillas y la posibilidad de poner todo en mayuscula, el código del Pipe quedaría de la siguiente manera:

```ts
import { Pipe, PipeTransform } from '@angular/core';

@Pipe({
  name: 'quote',
})
export class QuotePipe implements PipeTransform {
  transform(
    value: string,
    quoteChar: string = '"',
    upperCase: boolean = false
  ): string {
    return quoteChar + (upperCase ? value.toUpperCase() : value) + quoteChar;
  }
}
```

Y se utilizaría, en caso de querer cambiar todo el texto a mayúscula, de la siguiente manera:

```html
<p>{{ text | quote:"'":true }}</p>
```

Si solamente se quiere cambiar el texto a mayúscula con el segundo argumento y no modificar el valor default del primero (comillas dobles), se puede utilizar el valor `undefined` para el primer argumento. Por ejemplo:

```html
<p>{{ text | quote:undefined:true }}</p>
```

Vemos que si el valor de la variable `text` es: 

```
Hola como estas
```

Entonces lo que se verá en la vista con el ejemplo donde se aplica el pipe como `quote:"'":true` es:

```
'HOLA COMO ESTAS'
```

<h2 id="mas-informacion">Más información</h2>

- Documentación de [Pipes en Angular](https://angular.io/guide/pipes)
- Documentación de [Custom Pipes en Angular](https://angular.io/guide/pipes-custom-data-trans)
- Referencia a la [interfaz PipeTransform](https://angular.io/api/core/PipeTransform)
- Referencia al [decorador @Pipe](https://angular.io/api/core/Pipe)
- [Cheat Sheet de Data Binding en Angular](https://github.com/nicovillamonte/code-cheat-sheet/blob/main/Angular/ES/Data%20Binding.md)

<br>

<h3 id="cheat-sheet-data">Datos del cheat sheet</h3>

\- Autor: Nicolás Villamonte <br>
\- Fecha: 13/09/2023 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
