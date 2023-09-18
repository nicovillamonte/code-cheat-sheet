# Pseudo-clases y Pseudo-elementos - CSS

Las pseudo-clases y pseudo-elementos son selectores utilizados para darle estilos a elementos partiendo de un estado o posición específica. Son muy útiles y muy utilizados. Estos dos tipos de selectores no son sinónimos, aunque se parezcan en su sintaxis. La diferencia, en resumidas palabras, es que las pseudo-clases se utilizan para darle estilos a elementos partiendo de un estado específico, mientras que los pseudo-elementos se utilizan para darle estilos a partes específicas de un elemento.

1. [Pseudo-clases](#pseudo-clases)
   1. [Pseudo-clases más utilizadas](#pseudo-clases-utilizadas)
   2. [Ejemplo de pseudo-clases](#ejemplo-pseudo-clases)
   3. [Más Pseudo-clases](#mas-pseudo-clases)
2. [Pseudo-elementos](#pseudo-elementos)
   1. [Pseudo-elementos más utilizados](#pseudo-elementos-utilizados)
   2. [Ejemplo de pseudo-elementos](#ejemplo-pseudo-elementos)
   3. [Más Pseudo-elementos](#mas-pseudo-elementos)
3. [Más información](#mas-info)

- [Datos del cheat sheet](#cheat-sheet-data)

## Pseudo-clases <span id="pseudo-clases"></span>

Las pseudo-clases se utilizan para darle estilos a elementos partiendo de un estado específico que puedan tener los mismos. Son representados por dos puntos `:` seguidos del nombre de la pseudo-clase. Por ejemplo, si queremos que al pasar el cursor por encima de un elemento, los estilos del mismo cambien, entonces se debe utilizar una pseudoclase en el css del elemento.

```css
/* Ejemplo de pseudo-clase */
a:hover {
    color: red;
}
```

En este ejemplo, cuando el usuario pase el cursor por encima de un elemento `a`, el color del texto del mismo cambiará a rojo. Cuando no este el cursor por encima del elemento, el color del texto volverá a ser el que tenía antes.


### Pseudo-clases más utilizadas <span id="pseudo-clases-utilizadas"></span>

Los posibles valores **más utilizados** que puede tomar una pseudo-clase son:

- `:hover`: Se aplica cuando el mouse se coloca sobre el elemento.
- `:disabled`: Se aplica cuando el elemento está deshabilitado.
- `:enabled`: Se aplica cuando el elemento está habilitado.
- `:focus`: Se aplica cuando el elemento está en foco, es decir, cuando el usuario está interactuando con el elemento, como por ejemplo escribiendo en un input.
- `:active`: Se aplica cuando el elemento está activo, es decir, cuando el usuario está haciendo click sobre el elemento.
- `:not()`: Se aplica cuando el elemento no cumple con la condición que se le pasa como parámetro (Solo permite CSS).
- `:checked`: Se aplica cuando el elemento está seleccionado, como por ejemplo un checkbox.

### Ejemplo de pseudo-clases <span id="ejemplo-pseudo-clases"></span>

Veremos a continuación algunos ejemplos de lo que se puede realizar con las pseudoclases anteriormente mencionadas:

Con el `:hover` podemos cambiar el color de un elemento y el tipo de cursor cuando el cursor se coloca por encima del elemento que seleccionemos:

```css
/* Ejemplo de pseudo-clase */
.elemento:hover {
    color: red;
    cursor: pointer;
}
```

Con el `:disabled` podemos cambiar el color del fondo de un elemento cuando el mismo está deshabilitado:

```css
.elemento:disabled {
    background-color: gray;
    cursor: not-allowed;
}
```

Esto sucede cuando el elemento tiene el atributo `disabled`:

```html
<input class="elemento" disabled>
```

Con el `:enabled` podemos cambiar el color del fondo de un elemento cuando el mismo está habilitado, es decir, cuando el elemento no tiene el atributo `disabled`:

```css
.elemento:enabled {
    background-color: gray;
}
```

Con el `:focus` podemos cambiar los estilos de un elemento cuando el mismo está en foco, es decir, cuando el usuario está interactuando con el elemento:

```css
.elemento:focus {
    border: solid 1px red;
}
```

Con el `:active` podemos cambiar los estilos de un elemento cuando el mismo está activo, es decir, cuando el usuario está haciendo click sobre el elemento:

```css
button:active {
    background-color: green;
}
```

Con el `:not()` podemos cambiar los estilos de un elemento cuando el mismo no cumple con la condición que se le pasa como parámetro. Por ejemplo, si queremos que todos los elementos `a` que no tengan la clase `active` tengan un color de texto rojo, podemos hacer lo siguiente:

```css
a:not(.active) {
    color: red;
}
```

Con el `:checked` podemos cambiar los estilos de un elemento cuando el mismo está seleccionado, como por ejemplo un checkbox:

```css
input[type="checkbox"]:checked {
    background-color: green;
}
```


### Más Pseudo-clases <span id="mas-pseudo-clases"></span>

Estas son todas las pseudoclases existentes hasta el día de la fecha en la que se escribió este Cheat Sheet:

- [`:active`](https://developer.mozilla.org/es/docs/Web/CSS/:active)
- [`:checked`](https://developer.mozilla.org/es/docs/Web/CSS/:checked)
- [`:default`](https://developer.mozilla.org/es/docs/Web/CSS/:default)
- [`:dir()`](https://developer.mozilla.org/es/docs/Web/CSS/:dir)
- [`:disabled`](https://developer.mozilla.org/es/docs/Web/CSS/:disabled)
- [`:empty`](https://developer.mozilla.org/es/docs/Web/CSS/:empty)
- [`:enabled`](https://developer.mozilla.org/es/docs/Web/CSS/:enabled)
- [`:first`](https://developer.mozilla.org/es/docs/Web/CSS/:first)
- [`:first-child`](https://developer.mozilla.org/es/docs/Web/CSS/:first-child)
- [`:first-of-type`](https://developer.mozilla.org/es/docs/Web/CSS/:first-of-type)
- [`:fullscreen`](https://developer.mozilla.org/es/docs/Web/CSS/:fullscreen)
- [`:focus`](https://developer.mozilla.org/es/docs/Web/CSS/:focus)
- [`:hover`](https://developer.mozilla.org/es/docs/Web/CSS/:hover)
- [`:indeterminate`](https://developer.mozilla.org/es/docs/Web/CSS/:indeterminate)
- [`:in-range`](https://developer.mozilla.org/es/docs/Web/CSS/:in-range)
- [`:invalid`](https://developer.mozilla.org/es/docs/Web/CSS/:invalid)
- [`:lang()`](https://developer.mozilla.org/es/docs/Web/CSS/:lang)
- [`:last-child`](https://developer.mozilla.org/es/docs/Web/CSS/:last-child)
- [`:last-of-type`](https://developer.mozilla.org/es/docs/Web/CSS/:last-of-type)
- [`:left`](https://developer.mozilla.org/es/docs/Web/CSS/:left)
- [`:link`](https://developer.mozilla.org/es/docs/Web/CSS/:link)
- [`:not()`](https://developer.mozilla.org/es/docs/Web/CSS/:not)
- [`:nth-child()`](https://developer.mozilla.org/es/docs/Web/CSS/:nth-child)
- [`:nth-last-child()`](https://developer.mozilla.org/es/docs/Web/CSS/:nth-last-child)
- [`:nth-last-of-type()`](https://developer.mozilla.org/es/docs/Web/CSS/:nth-last-of-type)
- [`:nth-of-type()`](https://developer.mozilla.org/es/docs/Web/CSS/:nth-of-type)
- [`:only-child`](https://developer.mozilla.org/es/docs/Web/CSS/:only-child)
- [`:only-of-type`](https://developer.mozilla.org/es/docs/Web/CSS/:only-of-type)
- [`:optional`](https://developer.mozilla.org/es/docs/Web/CSS/:optional)
- [`:out-of-range`](https://developer.mozilla.org/es/docs/Web/CSS/:out-of-range)
- [`:read-only`](https://developer.mozilla.org/es/docs/Web/CSS/:read-only)
- [`:read-write`](https://developer.mozilla.org/es/docs/Web/CSS/:read-write)
- [`:required`](https://developer.mozilla.org/es/docs/Web/CSS/:required)
- [`:right`](https://developer.mozilla.org/es/docs/Web/CSS/:right)
- [`:root`](https://developer.mozilla.org/es/docs/Web/CSS/:root)
- [`:scope`](https://developer.mozilla.org/en-US/docs/Web/CSS/:scope)
- [`:target`](https://developer.mozilla.org/es/docs/Web/CSS/:target)
- [`:valid`](https://developer.mozilla.org/es/docs/Web/CSS/:valid)
- [`:visited`](https://developer.mozilla.org/es/docs/Web/CSS/:visited)

Hay que tener en cuenta que existen muchas más pseudo-clases si se escarba bien en la [documentiación de pseudo-clases de css](https://developer.mozilla.org/es/docs/Web/CSS/Pseudo-classes)


## Pseudo-elementos <span id="pseudo-elementos"></span>

Los pseudo-elementos se utilizan para darle estilos a partes específicas de un elemento. Son representados por dos puntos `::` seguidos del nombre del pseudo-elemento. Por ejemplo, si queremos que la primer palabra del texto de un elemento tenga un color rojo, entonces se debe utilizar un pseudo-elemento en el css del elemento.

```css
/* Ejemplo de pseudo-elemento */
.elemento::first-letter {
    color: red;
}
```

En este ejemplo, la primer letra del texto del elemento tendrá un color rojo. En el siguiente ejemplo de HTML, la letra _L_ de la primer palabra _Lorem_ se verá afectada por el estilo anterior:

```html
<p class="elemento">Lorem ipsum dolor sit amet consectetur adipisicing elit. Quisquam, voluptatum.</p>
```

> Los pseudo-elementos pueden utilizarse con solamente un `:` en vez de `::`, pero no es lo recomendable porque la idea es que se utilicen con dos puntos para diferenciarlos de las pseudo-clases.

### Pseudo-elementos más utilizados <span id="pseudo-elementos-utilizados"></span>

Los posibles valores **más utilizados** que puede tomar un pseudo-elemento son:

- `::after`: Se aplica al final del elemento.
- `::before`: Se aplica al principio del elemento.
- `::first-letter`: Se aplica a la primer letra del elemento.
- `::first-line`: Se aplica a la primer línea del elemento.
- `::selection`: Se aplica a la parte del elemento que el usuario selecciona.
- `::placeholder`: Se aplica al placeholder de un input.

### Ejemplo de pseudo-elementos <span id="ejemplo-pseudo-elementos"></span>

Veremos a continuación algunos ejemplos de lo que se puede realizar con los pseudo-elementos anteriormente mencionados:

Con el `::after` podemos agregar contenido al final de un elemento:

```css
.elemento::after {
    content: " - Fin del elemento";
}
```

Con el `::before` podemos agregar contenido al principio de un elemento:

```css
.elemento::before {
    content: "Inicio del elemento - ";
}
```

> Estos dos pseudo-elementos son muy utilizados con su contenido vacío para agregar estilos a elementos sin tener que agregar un elemento HTML extra.

Con el `::first-letter` podemos cambiar los estilos de la primer letra de un elemento:

```css
.elemento::first-letter {
    color: red;
}
```

Con el `::first-line` podemos cambiar los estilos de la primer línea de un elemento:

```css
.elemento::first-line {
    color: red;
}
```

Con el `::selection` podemos cambiar los estilos de la parte del elemento que el usuario selecciona, en este caso estaríamos cambiando el color default que suele ser azul y blanco del seleccionado en la web:

```css
.elemento::selection {
    background-color: red;
    color: white;
}
```

Con el `::placeholder` podemos cambiar los estilos del placeholder de un input:

```css
input::placeholder {
    color: red;
}
```

### Más Pseudo-elementos <span id="mas-pseudo-elementos"></span>

Según la documentación de css, los siguientes son todos los pseudo-elementos existentes hasta el día de la fecha en la que se escribió este Cheat Sheet:

- [::after](https://developer.mozilla.org/es/docs/Web/CSS/::after)
- [::before](https://developer.mozilla.org/es/docs/Web/CSS/::before)
- [::first-letter](https://developer.mozilla.org/es/docs/Web/CSS/::first-letter)
- [::first-line](https://developer.mozilla.org/es/docs/Web/CSS/::first-line)
- [::selection](https://developer.mozilla.org/es/docs/Web/CSS/::selection)
- [::backdrop](https://developer.mozilla.org/es/docs/Web/CSS/::backdrop)
- [::placeholder](https://developer.mozilla.org/es/docs/Web/CSS/::placeholder) (Experimental)
- [::marker](https://developer.mozilla.org/es/docs/Web/CSS/::marker) (Experimental)
- [::spelling-error](https://developer.mozilla.org/es/docs/Web/CSS/::spelling-error) (Experimental)
- [::grammar-error](https://developer.mozilla.org/en-US/docs/Web/CSS/::grammar-error) (Experimental)


Sin embargo, al igual que en las pseudo-clases, hay que tener en cuenta que existen algunos más. Por ejemplo, el pseudo-elemento `::webkit-scrollbar` y sus derivados se utilizan para darle estilos a los scrollbars de los elementos. Para poder encontrarlos hay que escarbar un poco más a fondo en la [documentación de pseudo-elementos de css](https://developer.mozilla.org/en-US/docs/Web/CSS/Pseudo-elements).


## Más información <span id="mas-info"></span>

- [Documentación oficial de Pseudo-clases](https://developer.mozilla.org/es/docs/Web/CSS/Pseudo-classes)
- [Documentación oficial de Pseudo-elementos](https://developer.mozilla.org/es/docs/Web/CSS/Pseudo-elements)

<br>

<h3 id="cheat-sheet-data">Datos del cheat sheet</h3>

\- Autor: Nicolás Villamonte <br>
\- Fecha: 17/09/2023 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>