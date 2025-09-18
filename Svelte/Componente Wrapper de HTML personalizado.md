# Componente Wrapper de HTML personalizado en Svelte

> IMPORTANTE: Leer todo el artículo antes de implementar cualquier código, algunos ejemplos estan mal hechos a propósito para explicar un concepto. Si se quiere implementar el código correctamente, seguir el [ejemplo final](#ejemplo-final).

1. [Introducción](#introducción)
2. [Componente personalizado](#componente-personalizado)
   - [Obtener props adicionales](#obtener-props-adicionales)
   - [Tipado correcto de las props](#tipado-correcto-de-las-props)
     - [Tipado de `children`](#tipado-de-children)
     - [Tipado de `rest`](#tipado-de-rest)
   - [Ejemplo final](#ejemplo-final)

- [Datos del Cheat Sheet](#datos-del-cheat-sheet)

## Introducción

Imaginemos que queremos crear un componente en Svelte que actúe como un contenedor (wrapper) para cualquier elemento HTML ya existente, permitiéndonos pasar atributos y contenido dinámicamente.

```svelte
<MiBoton tipo="primario" onClick={handleClick}>
  Haz clic aquí
</MiBoton>
```

## Componente personalizado

Para lograr esto, podemos comenzar creando un componente llamado `MiBoton.svelte` de esta manera:

```svelte
<script lang="ts">
  const { tipo, children } = $props();
</script>

<button class={`btn-${tipo}`}>
  {@render children()}
</button>
```

Esto funcionaría para el parametro `tipo` y el contenido dentro del botón, pero no permitiría pasar otros atributos como `onclick`, `id`, `style` y demás que podríamos querer añadir al botón.

### Obtener props adicionales

No queremos tener que definir cada uno de estos atributos manualmente, ya que esto haría que el componente sea menos flexible y más difícil de mantener. Por eso es que en la desestructuración de las props, Typescript nos permite usar el spread operator (`...rest`) para capturar todos los atributos adicionales que se pasen al componente.

```svelte
<script lang="ts">
  const { tipo = "primario", children, ...rest } = $props();
</script>

<button class={`btn-${tipo}`} onclick={onclick} {...rest}>
  {@render children()}
</button>
```

Sin embargo, esto no es del todo correcto, ya que el tipado de cada una de las props no se encuentra definido, por lo tanto Typescript infiere todos estos tipos como `any`.

### Tipado correcto de las props

Vamos a definir una interfaz para tipar correctamente las props del componente:

```svelte
<script lang="ts">
  interface Props {
    tipo: "primario" | "secundario";
    children: ...; // Cómo tipar children?
  }

  const { tipo = "primario", children, ...rest }: Props = $props(); // Y cómo tipar rest?
</script>

<button class={`btn-${tipo}`} {...rest}>
  {@render children()}
</button>
```

Acá nos encontramos con dos problemas:
1. No sabemos cómo tipar `children` en la interfaz `Props`.
2. No sabemos cómo tipar `rest` en la desestructuración de las props.

#### Tipado de `children`

Para el primer caso, children por defecto en Svelte 5 permite importar el tipo `Snippet` desde `svelte` y usarlo para tipar children:

```ts
import type { Snippet } from 'svelte';
... 
children: Snippet;
```

#### Tipado de `rest`

Para el segundo caso, debemos realizar una intersección de tipos entre los atributos de las props que definimos en la interfaz `Props` y los atributos HTML del elemento que estamos envolviendo (en este caso un botón).

Svelte nos permite obtener los atributos HTML de un elemento específico importando el tipo `HTMLAttributes` desde `svelte/elements` y usándolo con el nombre del elemento que queremos (en este caso `HTMLButtonElement`).

```ts
import type { HTMLButtonAttributes } from 'svelte/elements'
import type { Snippet } from 'svelte';

interface Props {
  tipo: "primario" | "secundario";
  children: Snippet;
}

const { tipo = "primario", children, ...rest }: Props & HTMLButtonAttributes = $props();
```

Una manera mas elegante de realizarlo es extendiendo la interfaz `Props` con los atributos HTML del elemento que queremos envolver:

```ts
import type { HTMLButtonAttributes } from 'svelte/elements'
import type { Snippet } from 'svelte';

interface Props extends HTMLButtonAttributes {
  tipo: "primario" | "secundario";
  children: Snippet;
}

const { tipo = "primario", children, ...rest }: Props = $props();
```

De esta manera, `rest` tendrá el tipado correcto de todos los atributos HTML que se le pueden pasar a un botón, y `children` estará correctamente tipado como un `Snippet`.

## Ejemplo final

Finalmente, el componente `MiBoton.svelte` quedaría de la siguiente manera:

```svelte
<script lang="ts">
  import type { HTMLButtonAttributes } from 'svelte/elements'
  import type { Snippet } from 'svelte';

  interface Props extends HTMLButtonAttributes {
    tipo: "primario" | "secundario";
    children: Snippet;
  }

  const { tipo = "primario", children, ...rest }: Props = $props();
</script>

<button class={`btn-${tipo}`} {...rest}>
  {@render children()}
</button>
```

<br>

### Datos del Cheat Sheet

\- Autor: Nicolás Villamonte <br>
\- Fecha: 17/09/2025 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
\- Herramientas y Versiones: Svelte 5, Nodejs v22.14.0, Typescript 5.0.0