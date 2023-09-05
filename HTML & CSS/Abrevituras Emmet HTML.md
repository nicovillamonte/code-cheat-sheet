# Emmet Abreviations

Las abreviaturas Emmet en HTML y CSS son una forma de escribir código HTML y CSS de manera más rápida y eficiente. Esto se logra gracias a que solamente se deben escribir sentencias con una combinación de abreviaturas y caracteres especiales, para luego expandir el código automáticamente a su forma completa.

En este Cheat Sheet vamos a ver algunas abreviaturas importantes a tener en cuenta para incrementar la velocidad de programación y reducir la cantidad de errores al escribir código HTML únicamente, proximamente exisitirá otro Cheat Sheet para CSS.

[**HTML**](#html)

1. [Estructura básica](#estructura-basica)
2. [Etiquetas](#etiquetas)
3. [Atributos](#atributos)
    1. [Sin valor](#atributos-sin-valor)
    2. [Con valor](#atributos-con-valor)
    3. [Múltiples atributos](#atributos-multiples)
4. [Clases](#clases)
    1. [Múltiples clases](#clases-multiples)
5. [ID](#ids)
6. [Texto dentro de la etiqueta](#text-content)
7. [Contenido de la etiqueta](#tag-children)
    1. [Múltiples etiquetas anidadas](#tag-children-anid)
    2. [Múltiples etiquetas hermanas](#tag-children-add)
8. [Multiplicación de etiquetas](#tag-multiplication)
9. [Agrupamiento](#groups)
    1. [Multiplicación de grupos](#groups-multiplication)
10. [Numeración de items](#numeration)
    1. [Generador de Lorem Ipsum](#lorem-ipsum)
    2. [Generador de Lorem Ipsum con cantidad de palabras](#lorem-ipsum-words)
11. [Ejemplos](#examples)
    1.  [Formulario](#example-formulario)
    2.  [Lista](#example-list)
    3.  [Tabla](#example-table)
12. [Más Información](#mas-info)

- [Datos del cheat sheet](#cheat-sheet-data)


<h1 id="html">HTML</h1>

<h2 id="estructura-basica">Estructura básica</h2>

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

<h2 id="etiquetas">Etiquetas</h2>

Para generar una etiqueta HTML, podemos escribir el nombre de la etiqueta y luego presionar la tecla TAB para autocompletar el código:

**Abreviación**:

```
div
```

**Salida**:

```html
<div></div>
```

<h2 id="atributos">Atributos</h2>

<h3 id="atributos-sin-valor">Sin valor</h3>

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

<h3 id="atributos-con-valor">Con valor</h3>

Para generar un atributo HTML con un valor, podemos escribir el nombre del atributo entre corchetes luego de la etiqueta en la que se va a aplicar, y luego escribir el valor del atributo entre comillas:

**Abreviación**:

```html
div[title="Título"]
```

**Salida**:

```html
<div title="Título"></div>
```

<h3 id="atributos-multiples">Múltiples atributos</h3>

Para generar múltiples atributos, los mismos deben ser escritos en el mismo corchete y separados por espacios:

**Abreviación**:

```html
div[title="Título" id="id"]
```

**Salida**:

```html
<div title="Título" id="id"></div>
```

<h2 id="clases">Clases</h2>

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

<h3 id="clases-multiples">Múltiples clases</h3>

Para generar múltiples clases, las mismas deben ser separadas por puntos luego de la etiqueta:

**Abreviación**:

```html
div.nombre-clase1.nombre-clase2
```

**Salida**:

```html
<div class="nombre-clase1 nombre-clase2"></div>
```

<h2 id="ids">ID</h2>

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

<h2 id="text-content">Texto dentro de la etiqueta</h2>

Para generar texto dentro de una etiqueta HTML, podemos escribir el texto encerrado entre llaves luego de la etiqueta:

**Abreviación**:

```html
div{Texto}
```

**Salida**:

```html
<div>Texto</div>
```

<h2 id="tag-children">Contenido de la etiqueta</h2>

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

<h3 id="tag-children-anid">Múltiples etiquetas anidadas</h3>

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

<h3 id="tag-children-add">Múltiples etiquetas hermanas</h3>

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

<h2 id="tag-multiplication">Multiplicación de etiquetas</h2>

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

<h2 id="groups">Agrupamiento</h2>

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

<h3 id="groups-multiplication">Multiplicación de grupos</h3>

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

<h2 id="numeration">Numeración de items</h2>

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

<h2 id="lorem-ipsum">Generador de Lorem Ipsum</h2>

Con la abreviación Lorem se puede generar texto de Lorem Ipsum. Vamos a demostrarlo con una etiqueta `p`:

**Abreviación**:

```html
p>lorem
```

**Salida**:

```html
<p>Lorem ipsum dolor sit amet consectetur, adipisicing elit. Culpa dicta quaerat, consequuntur, soluta, eaque unde nam doloremque distinctio aliquid dolorum adipisci eum quidem aut cum repellat! Ratione adipisci repudiandae quibusdam.</p>
```

<h3 id="lorem-ipsum-words">Generador de Lorem Ipsum con cantidad de palabras</h3>

Se puede añadir un número luego de la abreviación `lorem` para indicar la cantidad de palabras que se quieren generar:

**Abreviación**:

```html
p>lorem10
```

**Salida**:

```html
<p>Lorem ipsum dolor sit amet consectetur adipisicing elit. Consectetur, obcaecati!</p>
```


<h1 id="examples">Ejemplos</h1>

<h2 id="example-formulario">Formulario</h2>

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

<h2 id="example-list">Lista</h2>

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

<h2 id="example-table">Tabla</h2>

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

<h2 id="mas-info">Más Información</h2>

- [Documentación oficial de Emmet](https://docs.emmet.io/)


<br>

<h3 id="cheat-sheet-data">Datos del cheat sheet</h3>

\- Autor: Nicolás Villamonte <br>
\- Fecha: 05/09/2023 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>