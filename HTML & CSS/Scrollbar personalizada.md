# Personalización de la barra de Scroll con CSS

Existen pseudoelementos en css que nos permitirán personalizar la barra de scroll de nuestro sitio web o de alguna sección en específico de nuestro sitio web para que se vea más atractivo y no tan simple como la barra de scroll por defecto.

1. [Barra de scroll de Mac OS](#barra-de-scroll-de-mac-os)
2. [Pseudo elementos de la barra de scroll](#pseudo-elementos-de-la-barra-de-scroll)
3. [Utilidad](#utilidad)
4. [Contras](#contras)

- [Datos del Cheat Sheet](#datos-del-cheat-sheet)


## Barra de scroll de Mac OS

Para personalizar la barra de scroll como la que se muestra en los dispositivos Apple en todo nuestro sitio, recomiendo insertar el siguiente css en los estilos generales del proyecto.

```css
::-webkit-scrollbar {
  background-color: transparent;
  width: 8px;
}

::-webkit-scrollbar-track {
  background-color: transparent;
}

::-webkit-scrollbar-thumb {
  background-color: #babac0;
  border-radius: 16px;
}

::-webkit-scrollbar-button {
  display:none;
}
```

Lo que nos dará como resultado una barra como la siguiente:

<p align="center">
  <img src='https://github.com/nicovillamonte/code-cheat-sheet/assets/64659720/a5aeeac2-d094-4377-946a-8c92ab758bc6' align="center" />
</p>

En vez de la original que se ve de ésta manera:

<p align="center">
  <img src='https://github.com/nicovillamonte/code-cheat-sheet/assets/64659720/4b8cad7e-6dcf-4952-acce-d76c81e700fa' align="center" />
</p>


## Pseudo Elementos de la barra de Scroll

- `::-webkit-scrollbar`: Es el pseudoelemento que contiene a todos los demás elementos de la barra de scroll. Con el podemos, por ejemplo, delimitar las dimensiones de la barra, en especial el ancho. O incluso podemos hacer que el scrollbar no sea visible pero sí funcional con un `display: none;`.
- `::-webkit-scrollbar-button`: Es el pseudoelemento que contiene a los botones de la barra de scroll.
- `::-webkit-scrollbar-track`: Es el pseudoelemento que contiene a la barra de scroll en sí. **Importante**: _No es el elemento de desplazamiento arrastrable que se mueve cuando scrolleamos, sino todo el camino que recorre esta última_.
- `::-webkit-scrollbar-track-piece`: Es la parte del track no cubierta por el elemento de desplazamiento arrastrable.
- `::-webkit-scrollbar-thumb`: Se refiere al elemento de desplazamiento arrastrable que se mueve cuando scrolleamos.
- `::-webkit-scrollbar-corner`: Cuando las barras del _eje x_ y el _eje y_ se encuentran, se crea una esquina se forma un cuadrado. Este pseudoelemento se refiere a esa sección.

Estos son los pseudoelementos más importantes, pero no son todos. Para ver la lista completa de pseudoelementos de la barra de scroll, puedes visitar [este enlace](https://developer.mozilla.org/en-US/docs/Web/CSS/::-webkit-scrollbar).

Además se pueden utilizar los pseudoelementos `:horizontal` y `:vertical` para darle un estilo diferente a cada barra de scroll individualmente.

## Utilidad

Esto es algo esplendido visualmente, teniendo en cuenta que la barra de scroll que viene por defecto muchas veces no es del todo linda y podría romper con la estética del sitio. 

Además de que se puede personalizar la misma para que sea un elemento más de la página con su misma temática. Con la imaginación todo es posible.

## Contras

Esto no lo soportan en su totalidad todos los navegadores, de hecho que comience con -webkit quiere decir en la mayoría de los casos que solo lo soporta para navegadores basados en webkit, como lo son Chrome y Safari. Muchos usuarios no van a ver estos cambios y, en algunos casos, hasta puede llegar a romper otros estilos de la página. Así que si se desea utilizar es recomendable hacerlo con precaución y hacer pruebas en diferentes navegadores.

<br>

### Datos del Cheat Sheet

\- Autor: Nicolás Villamonte <br>
\- Fecha: 30/08/2023 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
\- Herramientas y Versiones: HTML5, CSS3, Visual Studio Code 1.82.2, Emmet 2.4.6