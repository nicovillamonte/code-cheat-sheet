# Emmet Abreviations

Las abreviaturas Emmet en HTML y CSS son una forma de escribir código HTML y CSS de manera más rápida y eficiente. Esto se logra gracias a que solamente se deben escribir sentencias con una combinación de abreviaturas y caracteres especiales, para luego expandir el código automáticamente a su forma completa.

<p align="center">
    <img src="https://github.com/nicovillamonte/code-cheat-sheet/assets/64659720/a36aafed-f81d-4cc2-b33d-0dfe2fd452f2" alt="emmet demostration" />
</p>

En este Cheat Sheet vamos a ver algunas abreviaturas importantes a tener en cuenta para incrementar la velocidad de programación y reducir la cantidad de errores al escribir código HTML únicamente, proximamente exisitirá otro Cheat Sheet para CSS.

[**HTML**](#html)

1. [Estructura básica](#estructura-básica)
2. [Etiquetas](#etiquetas)
3. [Atributos](#atributos)
    1. [Sin valor](#sin-valor)
    2. [Con valor](#con-valor)
    3. [Múltiples atributos](#múltiples-atributos)
4. [Clases](#clases)
    1. [Múltiples clases](#múltiples-clases)
5. [ID](#id)
6. [Texto dentro de la etiqueta](#texto-dentro-de-la-etiqueta)
7. [Contenido de la etiqueta](#contenido-de-la-etiqueta)
    1. [Múltiples etiquetas anidadas](#múltiples-etiquetas-anidadas)
    2. [Múltiples etiquetas hermanas](#múltiples-etiquetas-hermanas)
8. [Multiplicación de etiquetas](#multiplicación-de-etiquetas)
9. [Agrupamiento](#agrupamiento)
    1. [Multiplicación de grupos](#multiplicación-de-grupos)
10. [Numeración de items](#numeración-de-items)
    1. [Generador de Lorem Ipsum](#generador-de-lorem-ipsum)
    2. [Generador de Lorem Ipsum con cantidad de palabras](#generador-de-lorem-ipsum-con-cantidad-de-palabras)
11. [Ejemplos](#ejemplos)
    1.  [Formulario](#formulario)
    2.  [Lista](#lista)
    3.  [Tabla](#tabla)
12. [Más Información](#más-información)

- [Datos del Cheat Sheet](#datos-del-cheat-sheet)


# HTML

## Estructura básica

Para generar la estructura básica de un documento HTML, podemos utilizar la abreviatura `!` y luego presionar la tecla TAB para autocompletar el código:

**Abreviación**:

```
!
```

**Salida**:

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Document</title>
</head>
<body>
    
</body>
</html>
```

## Etiquetas

Para generar una etiqueta HTML, podemos escribir el nombre de la etiqueta y luego presionar la tecla TAB para autocompletar el código:

**Abreviación**:

```
div
```

**Salida**:

```html
<div></div>
```

## Atributos

### Sin valor

Para generar un atributo HTML, podemos escribir el nombre del atributo entre corchetes luego de la etiqueta en la que se va a aplicar:

**Abreviación**:

```html
div[title]
```

**Salida**:

```html
<div title=""></div>
```

Veremos que al autocompletarse nos dejará el foco de escritura dentro de las comillas del atributo, para que podamos escribir el valor del mismo.

### Con valor

Para generar un atributo HTML con un valor, podemos escribir el nombre del atributo entre corchetes luego de la etiqueta en la que se va a aplicar, y luego escribir el valor del atributo entre comillas:

**Abreviación**:

```html
div[title="Título"]
```

**Salida**:

```html
<div title="Título"></div>
```

### Múltiples atributos

Para generar múltiples atributos, los mismos deben ser escritos en el mismo corchete y separados por espacios:

**Abreviación**:

```html
div[title="Título" id="id"]
```

**Salida**:

```html
<div title="Título" id="id"></div>
```

## Clases

Para generar una clase en una etiqueta HTML, podemos escribir el nombre de la clase luego de la etiqueta y anteponer un punto:

**Abreviación**:

```html
div.nombre-clase
```

**Salida**:

```html
<div class="nombre-clase"></div>
```

Las etiquetas div tienen la peculiaridad de que si no se especifica el nombre de la etiqueta, se asume que es una de ellas. Por lo que la abreviación anterior es equivalente a la siguiente:

```html
.nombre-clase
```

### Múltiples clases

Para generar múltiples clases, las mismas deben ser separadas por puntos luego de la etiqueta:

**Abreviación**:

```html
div.nombre-clase1.nombre-clase2
```

**Salida**:

```html
<div class="nombre-clase1 nombre-clase2"></div>
```

## ID

Para generar un ID en una etiqueta HTML, podemos escribir el nombre del ID luego de la etiqueta y anteponer un numeral:

**Abreviación**:

```html
div#nombre-id
```

**Salida**:

```html
<div id="nombre-id"></div>
```

A diferencia de las clases, no se pueden generar múltiples IDs en una misma etiqueta.

Al igual que las clases, si no se especifica el nombre de la etiqueta, se asume que es un div:

```html
#nombre-id
```

## Texto dentro de la etiqueta

Para generar texto dentro de una etiqueta HTML, podemos escribir el texto encerrado entre llaves luego de la etiqueta:

**Abreviación**:

```html
div{Texto}
```

**Salida**:

```html
<div>Texto</div>
```

## Contenido de la etiqueta

Para generar otras etiquetas dentro de una etiqueta HTML, podemos escribir el contenido luego de la etiqueta y anteponer un signo mayor:

**Abreviación**:

```html
div>p
```

**Salida**:

```html
<div>
    <p></p>
</div>
```

### Múltiples etiquetas anidadas

Para generar múltiples etiquetas anidadas dentro de una etiqueta HTML, las mismas deben ser separadas por signos mayores:

**Abreviación**:

```html
div>p>span
```

**Salida**:

```html
<div>
    <p>
        <span></span>
    </p>
</div>
```

### Múltiples etiquetas hermanas

Para generar múltiples etiquetas hermanas dentro de una etiqueta HTML, las mismas deben ser separadas por signos de la operación suma:

**Abreviación**:

```html
div>p+span
```

**Salida**:

```html
<div>
    <p></p>
    <span></span>
</div>
```

## Multiplicación de etiquetas

Para generar múltiples etiquetas anidadas o hermanas, podemos escribir el número de veces que se va a repetir la etiqueta con un asterisco y luego presionar la tecla TAB para autocompletar el código:

**Abreviación**:

```html
div>p*3
```

**Salida**:

```html
<div>
    <p></p>
    <p></p>
    <p></p>
</div>
```

## Agrupamiento

Los parentesis permiten agrupar abreviaciones para que se expandan juntas:

**Abreviación**:

```html
header>(span>p*2)+footer
```

**Salida**:

```html
<header>
    <span>
        <p></p>
        <p></p>
    </span>
    <footer></footer>
</header>
```

Si hubiesemos hecho los mismo sin agrupamient, es decir, que la abreviación emmet fuera `header>span>p*2+footer` Entonces el `+` aplicaría a `p` y `footer` y no a `span` y `footer` como queremos. Lo que quedaría de la siguiente manera:

```html
<header>
    <span>
        <p></p>
        <p></p>
        <footer></footer>
    </span>
</header>
```

### Multiplicación de grupos

También podemos multiplicar el grupo de etiquetas:

**Abreviación**:

```html
(div+span)*2
```

**Salida**:

```html
<div></div>
<span></span>
<div></div>
<span></span>
```

## Numeración de items

El símbolo $ nos permite numerar los items de una abreviación. Por ejemplo, si queremos hacer varios items con diferentes clases, podemos hacer lo siguiente:

**Abreviación**:

```html
ul>li.item$*3
```

**Salida**:

```html
<ul>
    <li class="item1"></li>
    <li class="item2"></li>
    <li class="item3"></li>
</ul>
```

> Se pueden utilizar varios `$` juntos, lo que nos permite generar números de más de un dígito. Por ejemplo, si queremos generar 10 items, podemos hacer `li.item$$*10` y obtendremos `item01`, `item02`, `item03`, etc.


Existen muchos otros modificadores para esta abrieviatura, lo que se puede ver en la [documentación oficial de la numeración de items en Emmet](https://docs.emmet.io/abbreviations/syntax/#item-numbering).

## Generador de Lorem Ipsum

Con la abreviación Lorem se puede generar texto de Lorem Ipsum. Vamos a demostrarlo con una etiqueta `p`:

**Abreviación**:

```html
p>lorem
```

**Salida**:

```html
<p>Lorem ipsum dolor sit amet consectetur, adipisicing elit. Culpa dicta quaerat, consequuntur, soluta, eaque unde nam doloremque distinctio aliquid dolorum adipisci eum quidem aut cum repellat! Ratione adipisci repudiandae quibusdam.</p>
```

### Generador de Lorem Ipsum con cantidad de palabras

Se puede añadir un número luego de la abreviación `lorem` para indicar la cantidad de palabras que se quieren generar:

**Abreviación**:

```html
p>lorem10
```

**Salida**:

```html
<p>Lorem ipsum dolor sit amet consectetur adipisicing elit. Consectetur, obcaecati!</p>
```


# Ejemplos

## Formulario

**Abreviación**:

```html
form>input[type="text" name="nombre"]+input[type="text" name="apellido"]+input[type="email" name="email"]+input[type="password" name="password"]+input[type="submit" value="Enviar"]
```

**Salida**:

```html
<form action="">
    <input type="text" name="nombre" /><input
    type="text"
    name="apellido"
    /><input type="email" name="email" /><input
    type="password"
    name="password"
    /><input type="submit" value="Enviar" />
</form>
```

## Lista

**Abreviación**:

```html
ul>li.item$*3
```

**Salida**:

```html
<ul>
    <li class="item1"></li>
    <li class="item2"></li>
    <li class="item3"></li>
</ul>
```

## Tabla

**Abreviación**:

```html
table>thead>tr>th*3+tbody>tr*3>td{item$}*3
```

**Salida**:

```html
<table>
    <thead>
        <tr>
            <th></th>
            <th></th>
            <th></th>
            <tbody>
                <tr>
                    <td>item1</td>
                    <td>item2</td>
                    <td>item3</td>
                </tr>
                <tr>
                    <td>item1</td>
                    <td>item2</td>
                    <td>item3</td>
                </tr>
                <tr>
                    <td>item1</td>
                    <td>item2</td>
                    <td>item3</td>
                </tr>
            </tbody>
        </tr>
    </thead>
</table>
```

## Más Información

- [Documentación oficial de Emmet](https://docs.emmet.io/)


<br>

### Datos del Cheat Sheet

\- Autor: Nicolás Villamonte <br>
\- Fecha: 05/09/2023 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
\- Herramientas y Versiones: HTML5, CSS3, Visual Studio Code 1.82.2, Emmet 2.4.6