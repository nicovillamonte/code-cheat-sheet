# Centrado de elementos de una barra de navegación

Al desarrollar, en frontend, una barra de navegación o NavBar, es común que se nos presente el problema de que no se centren correctamente sus elementos a pesar de utilizar las técnicas de centrado con flex o grid. En este artículo se muestra como hacer una barra de navegación desde el principio y como centrar sus elementos correctamente con flex.

## Características de la barra de navegación

Existen miles de formas de hacer una barra de navegación, puede estar constantemente fija en la página a pesar del scroll, puede quedarse en la misma posición exacta y desaparecer cuando se hace scroll, puede tener menus, logos, interacciones de todo tipo, diferentes tamnaños, colores, etc. Sin embargo aquí nos vamos a centrar en realizar una barra de navegación específica para poder centrarnos más en el problema de centrado que se nos presenta en la mayoría de los casos. Esto aplica para cualquier otra cosa que se quiera centrar, no solamente para una barra de navegación.

La barra de navegación que vamos a realizar va a tener las siguientes características:

- Va a estar fija en la parte superior de la página.
- Va a tener una marca a la izquierda.
- Va a tener un menu de navegación en el medio.
- Va a tener un menú de cuenta a la derecha.
- No nos vamos a centrar en tener un diseño precioso, sino uno útil para el ejemplo.
- Vamos a utilizar puro flex.

## Estructura HTML

Para poder realizar la barra de navegación, vamos a utilizar la siguiente estructura HTML:

```html
<header>
        <div id="left-section">
            <h1>Brand</h1>
        </div>

        <nav>
            <ul>
                <li class="current"><a href="#">Home</a></li>
                <li><a href="#">About</a></li>
                <li><a href="#">Services</a></li>
                <li><a href="#">Contact Us</a></li>
            </ul>
        </nav>

        <div id="right-section">
            <ul>
                <li><a href="#">Login</a></li>
                <li><a href="#">Register</a></li>
            </ul>
        </div>
</header>
```

Como se puede ver, tenemos un header que contiene 3 elementos, un div con id left-section, un nav y un div con id right-section.

## Primer intento y su problema

Generalmente, para separar los elemento con flex hacemos que el contenedor alinee sus componente con la propiedad space-between. Si tenemos estos tres componentes, lo que va a suceder es que el primer elemento se va a alinear a la izquierda, el segundo al centro y el tercero a la derecha.

```css
header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  width: 100%;

  background-color: gainsboro;
  box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2);

  box-sizing: border-box;
  padding: 1rem;
}
```

> Nota: _El parametro box-sizing se utiliza para que el padding no afecte el tamaño del header, lo que haría que el header se desbordara de la pantalla. En este caso el padding se realiza para adentro del contenedor y no para afuera._

Con esto y los estilos de los elementos internos, obtenemos algo como lo siguiente:

---------------FOTO DE LA BARRA DE NAVEGACIÓN CON FLEX SIN 1

Como vemos, parece ya estar listo y bien centrado, pero vamos a jugar con los textos de la izquierda y la derecha para ver que sucede con el elemento centrado.

Vamos a hacer que el texto de la izquierda sea más largo, por ejemplo, en vez de decir `Brand` vamos a poner `Bussiness Brand` con el editor de elementos del navegador.

---------------FOTO DE LA BARRA DE NAVEGACIÓN CON FLEX SIN 1 y titulo largo

Claramente, el texto que supuestamente estaba bien centrado, ahora se movio para la izquierda, de hecho, esta centrado con respecto a los textos de la izquierda y derecha, pero no con respecto al header. Esto es porque el texto de la izquierda es más largo y por lo tanto el elemento se mueve hacia la izquierda.

Este es un error común y vamos a ver como solucionarlo.

## Solución

Para solucionar este problema, deberíamos hacer crecer los componentes de los extremos todo lo que puedan, para que el elemento del medio quede centrado. Para esto, vamos a utilizar las propiedades `flex-grow` y `flex-basis`. 

La propiedad `flex-grow` indica cuanto puede crecer un elemento con respecto a los demás. Por ejemplo, si tenemos 3 elementos con `flex-grow: 1`, cada uno va a ocupar un tercio del espacio disponible. Si tenemos 2 elementos con `flex-grow: 1` y uno con `flex-grow: 2`, el elemento con `flex-grow: 2` va a ocupar el doble de espacio que los otros dos.

Mientras que la propiedad `flex-basis` indica cuanto espacio debería ocupar un elemento. Por ejemplo, si tenemos 3 elementos con `flex-basis: 0`, cada uno va a ocupar un tercio del espacio disponible. Si tenemos 2 elementos con `flex-basis: 0` y uno con `flex-basis: 100px`, el elemento con `flex-basis: 100px` va a ocupar 100px y los otros dos van a ocupar el espacio restante.

```css
#left-section {
  display: flex;

  flex-grow: 1;
  flex-basis: 0;
}

#right-section {
  display: flex;

  flex-grow: 1;
  flex-basis: 0;
  justify-content: flex-end;
}
```

> Nota: _En la sección de la izquierda se esta justificando el contenido hacia el final, porque al ocupar todo lo que sea posible ocupar, por defecto el contenido se muestra al incio del contenedor, lo que lo pegaría al menú de navegación del centro._

## Flex 1

Esta operación se puede realizar de manera más sencilla simplemente con `flex: 1`. Esto es lo mismo que poner `flex-grow: 1`, `flex-shrink: 1` y `flex-basis: 0` en un solo parametro.

```css
#left-section {
  display: flex;
  flex: 1;
}

#right-section {
  display: flex;
  flex: 1;
  justify-content: flex-end;
}
```

Ahora podemos ver como el elemento del medio se centra correctamente sin importar el tamaño de los elementos de los extremos:

---------------FOTO DE LA BARRA DE NAVEGACIÓN CON FLEX 1 y titulo corto

Mientras que si alargamos el texto como hicimos anteriormente se vería de la siguiente manera:

---------------FOTO DE LA BARRA DE NAVEGACIÓN CON FLEX 1 y titulo largo


## Código final

Para ver como quedó el código final y modificarlo para realizar pruebas pueden acceder en el [repositorio del ejemplo de barra de navegación para HTML y CSS](https://github.com/nicovillamonte/eg-navbar-CSS).



### Datos del cheat sheet

\- Autor: Nicolás Villamonte <br>
\- Fecha: 22/08/2023 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>