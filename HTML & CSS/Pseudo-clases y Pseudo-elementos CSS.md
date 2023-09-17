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
3. [Ejemplos](#ejemplos)
4. [Más información](#mas-info)

- [Datos del cheat sheet](#cheat-sheet-info)

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

```html
```

### Más Pseudo-clases

TODO: ENLISTAR TODAS LAS PSEUDOCLASES EXISTENTES Y DEJAR EL LINK PARA VER MÁS INFO DE CADA UNA

https://developer.mozilla.org/es/docs/Web/CSS/Pseudo-classes



## Pseudo-elementos <span id="pseudo-elementos"></span>

https://developer.mozilla.org/es/docs/Web/CSS/Pseudo-elements

## Ejemplos <span id="ejemplos"></span>

## Más información <span id="mas-info"></span>